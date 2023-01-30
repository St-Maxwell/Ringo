module ringo_scf_rhf
    use machina_basic, only: f8
    use machina_assert
    use ringo_scf
    use ringo_scf_params
    use ringo_mole_detail
    use ringo_cint
    use ringo_math
    !>
    use ringo_env
    use ringo_log_utils
    implicit none
    private
    public :: rhf_t, construct_rhf

    type, extends(scf_t) :: rhf_t
        type(mole_t), pointer :: mol => null()
        !> (alpha) Fock matrix
        real(kind=f8), dimension(:, :), allocatable :: F
        !> (alpha) molecular coefficients matrix
        real(kind=f8), dimension(:, :), allocatable :: C
        !> (alpha) density matrix
        real(kind=f8), dimension(:, :), allocatable :: P
        !> overlap matrix
        real(kind=f8), dimension(:, :), allocatable :: S
        !> transform matrix (which orthogonalize S)
        real(kind=f8), dimension(:, :), allocatable :: X
        !> MO energies
        real(kind=f8), dimension(:), allocatable :: eigv
        !> the number of basis functions
        integer :: nbas = 0
        !> the number of occupied MOs
        integer :: nocc = 0
        !> the number of virtual MOs
        integer :: nvir = 0
    contains
        !> inherit from objective_function_t
        !> must be overridden
        procedure :: get_value
        procedure :: residual
        procedure :: get_vector
        procedure :: put_vector
        !> inherit from scf_t
        !> must be overridden
        procedure :: energy
        procedure :: obtain_guess
        !> rhf methods
        procedure :: fock_to_mo
        procedure :: mo_to_density
        procedure :: build_fock_own_dm, build_fock_any_dm
        generic :: build_fock => build_fock_own_dm, build_fock_any_dm
        procedure :: build_hcore
        procedure :: build_veff
    end type

contains

    subroutine construct_rhf(this, mole, params)
        class(scf_t), allocatable, intent(out) :: this
        type(mole_t), intent(in), target :: mole
        type(scf_params_t), intent(in) :: params

        allocate (rhf_t :: this)
        select type (this)
        type is (rhf_t)
            this%mol => mole
            this%params = params
            this%nbas = mole%get_nbas()
            this%nocc = mole%nalpha
            this%nvir = this%nbas - this%nocc
            allocate (this%F(this%nbas, this%nbas))
            allocate (this%C(this%nbas, this%nbas))
            allocate (this%P(this%nbas, this%nbas))
            !allocate(this%S(this%nbas,this%nbas))
            allocate (this%X(this%nbas, this%nbas))
            allocate (this%eigv(this%nbas))

            call mole%get_int1e("ovlp", this%S)
            call lowdin_orthogonaliztion(this%S, this%X)

            call this%cec%set("Nuclear repulsion", mole%get_nuc_repulsion_energy())
        end select

    end subroutine construct_rhf

    function get_value(this) result(v)
        class(rhf_t), intent(inout) :: this
        real(kind=f8) :: v

        v = this%energy()

    end function get_value

    subroutine residual(this, res)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:), allocatable, intent(out) :: res
        !> locals
        real(kind=f8), dimension(:), allocatable, target :: vtmp ! temp vector
        real(kind=f8), dimension(:, :), allocatable :: mtmp ! temp matrix
        real(kind=f8), dimension(:, :), pointer :: SPF

        ! res = 2 * X^T*(SPF-FPS)*X
        !     = 2 * (P'F'-F'P')
        allocate (vtmp(size(this%F)))
        allocate (mtmp, mold=this%F)

        SPF(1:this%nbas, 1:this%nbas) => vtmp

        call matmul_gemm(this%S, this%P, mtmp)
        call matmul_gemm(mtmp, this%F, SPF)
        SPF = SPF - transpose(SPF)
        call matmul_gemm(this%X, SPF, mtmp, AT='T')
        call matmul_gemm(mtmp, this%X, SPF)
        SPF = 2*SPF

        call move_alloc(vtmp, res)

    end subroutine residual

    subroutine get_vector(this, vec)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:), allocatable, intent(out) :: vec
        real(kind=f8), dimension(:), pointer :: ptr

        ! equivalent to vec = reshape(this%F, [this%nbas*this%nbas])
        ptr(1:this%nbas*this%nbas) => array_view(this%F)
        vec = ptr

    end subroutine get_vector

    subroutine put_vector(this, vec)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:), intent(in) :: vec

        !> set new Fock matrix
        this%F = reshape(vec, [this%nbas, this%nbas])
        !this%F = mat_view(vec, this%nbas, this%nbas)

        !> Fock to MO
        call this%fock_to_mo()

        !> MO to density
        call this%mo_to_density()

        !> build Fock
        call this%build_fock()

    end subroutine put_vector

    function energy(this) result(e)
        class(rhf_t), intent(inout) :: this
        real(kind=f8) :: e

        call this%build_fock()
        e = this%ec%total_energy()

    end function energy

    subroutine obtain_guess(this, guess)
        class(rhf_t), intent(inout) :: this
        integer, intent(in) :: guess

        select case (guess)
        case (scf_guess%hcore)
            call guess_core_hamiltonian(this, this%C)
        case default
            call assert(.false., "Guess not implemented yet")
        end select

        call this%mo_to_density()
        call this%build_fock()

    end subroutine obtain_guess

    subroutine fock_to_mo(this)
        class(rhf_t), intent(inout) :: this
        !> locals
        real(kind=f8), dimension(:, :), allocatable :: Fprime, Cprime

        allocate (Fprime, mold=this%F)
        allocate (Cprime, mold=this%C)
        call unitary_transform(this%F, this%X, Fprime)

        call solve_eigen(Fprime, this%eigv, Cprime)
        call matmul_gemm(this%X, Cprime, this%C)

    end subroutine fock_to_mo

    subroutine mo_to_density(this)
        class(rhf_t), intent(inout) :: this

        call matmul_gemm(this%C(:, :this%nocc), this%C(:, :this%nocc), this%P, BT='T')

    end subroutine mo_to_density

    subroutine build_fock_own_dm(this)
        class(rhf_t), intent(inout) :: this

        call this%build_fock(this%P)

    end subroutine build_fock_own_dm

    subroutine build_fock_any_dm(this, dm)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:, :), intent(in) :: dm
        !> locals
        real(kind=f8), dimension(:, :), allocatable :: h, Veff

        call this%build_hcore(h)
        call this%build_veff(dm, Veff)
        this%F = h + Veff

    end subroutine build_fock_any_dm

    subroutine build_hcore(this, h)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:, :), allocatable, intent(out) :: h
        real(kind=f8), dimension(:, :), allocatable :: tmp

        call this%mol%get_int1e("kin", tmp)
        h = tmp
        call this%mol%get_int1e("nuc", tmp)
        h = h + tmp

        !> set energy component contribution
        call this%ec%set("One-electron", 2*sum(this%P*h))

    end subroutine build_hcore

    subroutine build_veff(this, dm, Veff)
        class(rhf_t), intent(inout) :: this
        real(kind=f8), dimension(:, :), intent(in) :: dm
        real(kind=f8), dimension(:, :), allocatable, intent(out) :: Veff
        real(kind=f8), dimension(:, :), allocatable :: J, K

        allocate (J(this%nbas, this%nbas))
        allocate (K(this%nbas, this%nbas))
        call calc_J(J, dm, this%mol%atm, this%mol%bas, this%mol%env)
        call calc_K(K, dm, this%mol%atm, this%mol%bas, this%mol%env)

        Veff = 2*J - K

        !> set energy component contribution
        call this%ec%set("Coulomb", sum(2*this%P*J))
        call this%ec%set("Exchange", -sum(this%P*K))

    end subroutine build_veff

!>==============================================================================

    subroutine guess_core_hamiltonian(this, C)
        type(rhf_t), intent(inout) :: this
        !> guess MO coefficients
        real(kind=f8), dimension(:, :), allocatable, intent(out) :: C
        !> locals
        real(kind=f8), dimension(:, :), allocatable :: h, mtmp, Cprime
        real(kind=f8), dimension(:), allocatable :: vtmp

        !> build core hamiltonian
        call this%mol%get_int1e("kin", mtmp)
        h = mtmp
        call this%mol%get_int1e("nuc", mtmp)
        h = h + mtmp

        allocate (vtmp(this%nbas))
        allocate (C(this%nbas, this%nbas))
        allocate (Cprime(this%nbas, this%nbas))
        call unitary_transform(h, this%X, mtmp)
        call solve_eigen(mtmp, vtmp, Cprime)
        call matmul_gemm(this%X, Cprime, C)

    end subroutine guess_core_hamiltonian

end module ringo_scf_rhf
