module ringo_mole_detail
    use machina_basic, only: i4, f8
    use machina_string, only: to_string
    use machina_core
    use machina_vla
    use machina_map
    use machina_set
    use machina_error
    use machina_assert
    use ringo_env
    use ringo_phys_const, only: bohr_to_angstrom
    use ringo_mole_atom
    use ringo_mole_basis
    use ringo_elements
    use ringo_cint
    use ringo_log_utils
    implicit none
    private
    public :: mole_t, construct_mole

    type :: mole_t
        private
        !> the total charge of the molecule
        integer :: charge = 0
        !> spin multiplicity of the molecule
        integer :: mult = 0
        !> the internal format of atoms in molecule
        type(atom_t), dimension(:), allocatable :: atoms
        !> the internal format of basis set
        type(basis_set_t), dimension(:), allocatable :: basis
        !> the number of electrons
        integer :: nele = 0
        integer :: nalpha = 0
        integer :: nbeta = 0
        !> libcint arrays
        integer(kind=i4), dimension(:), allocatable :: atm
        integer(kind=i4), dimension(:), allocatable :: bas
        real(kind=f8), dimension(:), allocatable :: env
        !> if a mole_t instance is ready to be used
        logical :: built = .false.
    contains
        procedure :: set_basis_set_string
        procedure :: set_basis_set_map
        generic :: set_basis_set => set_basis_set_string, set_basis_set_map
        procedure :: print_geometry
        procedure :: build
        procedure :: get_int1e
    end type

contains

    subroutine construct_mole(this, atoms, charge, mult, error)
        type(mole_t), intent(out) :: this
        character(len=*), intent(in) :: atoms
        integer, intent(in) :: charge
        integer, intent(in) :: mult
        type(error_t), intent(out) :: error

        this%charge = charge
        this%mult = mult

        call format_atoms(atoms, this%atoms, error)
        if (.has.error) return

        this%nele = sum(this%atoms%n) - this%charge
        if (mod(this%nele + this%mult - 1, 2) /= 0) then
            call raise_error(error, "Electron number "//to_string(this%nele)// &
                                  & " and multiplicity "//to_string(this%mult)// &
                                  & " are not consistent")
            return
        end if
        this%nalpha = (this%nele + this%mult - 1)/2
        this%nbeta = this%nele - this%nalpha
        if (this%nbeta < 0) then
            call raise_error(error, "Electron number "//to_string(this%nele)// &
                                  & " and multiplicity "//to_string(this%mult)// &
                                  & " are not consistent")
            return
        end if

    end subroutine construct_mole

    subroutine set_basis_set_string(this, basis, error)
        class(mole_t), intent(inout) :: this
        character(len=*), intent(in) :: basis
        type(error_t), intent(out) :: error
        type(vla_char) :: unique_atom
        type(map_t) :: bas

        block
            type(vla_char) :: atoms
            integer :: i
            do i = 1, size(this%atoms)
                call atoms%push_back(this%atoms(i)%s)
            end do
            call unique(atoms, unique_atom)
        end block

        block
            type(vla_char_const_iterator) :: it
            it = unique_atom%const_iterator()
            do while (it%has_next())
                call bas%insert(it%get_next(), basis)
            end do
        end block

        call print_header("Loading Basis Set", std_out)
        call format_basis_set(bas, this%basis, error)
        write (std_out, "(A)")

    end subroutine set_basis_set_string

    subroutine set_basis_set_map(this, basis, error)
        class(mole_t), intent(inout) :: this
        type(map_t), intent(in) :: basis
        type(error_t), intent(out) :: error

        call print_header("Loading Basis Set", std_out)
        call format_basis_set(basis, this%basis, error)
        write (std_out, "(A)")

    end subroutine set_basis_set_map

    subroutine build(this, error)
        class(mole_t), intent(inout) :: this
        type(error_t), intent(out) :: error

        call make_env(this, error)

        this%built = .true.

    end subroutine build

    subroutine make_env(this, error)
        class(mole_t), intent(inout) :: this
        type(error_t), intent(out) :: error
        type(vla_int) :: atm, bas
        type(vla_real) :: env
        type(map_t), target :: bas_dict
        integer :: offset
        integer :: i, j

        offset = PTR_ENV_START
        call env%push_back([(0._f8, i=1, PTR_ENV_START)])

        do i = 1, size(this%atoms)
            call make_atm_env(this%atoms(i), offset, atm, env)
        end do

        iterate_shell: block
            type(vla_int), allocatable :: bas0
            do i = 1, size(this%basis)
                call make_bas_env(this%basis(i)%shells, offset, bas0, env)
                call bas_dict%insert(this%basis(i)%a(:), bas0)
            end do
        end block iterate_shell

        assign_bas_for_atoms: block
            type(vla_int), pointer :: b
            do i = 1, size(this%atoms)
                if (bas_dict%has_key(this%atoms(i)%s)) then
                    call bas_dict%get_vla_int(this%atoms(i)%s, b, error)
                    if (.has.error) return
                    do j = 1, size(b), BAS_SLOTS
                        b%ptr_at((j - 1) + ATOM_OF) = i - 1 ! 0-based
                    end do
                    call bas%push_back(b)
                end if
            end do
        end block assign_bas_for_atoms

        call copy_to_array(atm, this%atm)
        call copy_to_array(bas, this%bas)
        call copy_to_array(env, this%env)

    end subroutine make_env

    subroutine make_atm_env(atom, offset, atm, env)
        type(atom_t), intent(in) :: atom
        integer, intent(inout) :: offset
        type(vla_int), intent(inout) :: atm
        type(vla_real), intent(inout) :: env
        integer(kind=i4), dimension(ATM_SLOTS) :: atm_

        atm_ = 0
        atm_(CHARGE_OF) = atom%n
        atm_(PTR_COORD) = offset
        atm_(NUC_MOD_OF) = 1

        offset = offset + 3
        call atm%push_back(atm_)
        call env%push_back(atom%xyz)

    end subroutine make_atm_env

    subroutine make_bas_env(basis, offset, bas0, env)
        type(basis_set_shell), dimension(:), intent(in) :: basis
        integer, intent(inout) :: offset
        type(vla_int), allocatable, intent(out) :: bas0
        type(vla_real), intent(inout) :: env
        integer(kind=i4), dimension(BAS_SLOTS) :: bas_
        real(kind=f8), dimension(:), allocatable :: coeff
        integer :: i, j

        allocate (bas0)
        do i = 1, size(basis)
            bas_ = 0
            ! bas_(ATOM_OF) = 0 *intent to do so*
            bas_(ANG_OF) = basis(i)%l
            bas_(NPRIM_OF) = size(basis(i)%e)
            bas_(NCTR_OF) = 1 ! just fix it

            coeff = basis(i)%c
            do j = 1, size(basis(i)%e)
                coeff(j) = coeff(j)*gto_norm(basis(i)%l, basis(i)%e(j))
            end do
            call normalize_contracted_ao(basis(i)%l, basis(i)%e, coeff)

            call env%push_back(basis(i)%e)
            bas_(PTR_EXP) = offset
            offset = offset + size(basis(i)%e)
            call env%push_back(coeff)
            bas_(PTR_COEFF) = offset
            offset = offset + size(basis(i)%e)
            call bas0%push_back(bas_)
        end do

    contains

        subroutine normalize_contracted_ao(l, expn, coeff)
            integer, intent(in) :: l
            real(kind=f8), dimension(:), intent(in) :: expn
            real(kind=f8), dimension(:), intent(inout) :: coeff
            real(kind=f8), dimension(:, :), allocatable :: ee
            real(kind=f8) :: s1
            integer :: i, j

            allocate (ee(size(expn), size(expn)))

            do i = 1, size(expn)
                do j = 1, i
                    ee(i, j) = gaussian_int(l*2 + 2, expn(i) + expn(j))
                    ee(j, i) = ee(i, j)
                end do
            end do

            s1 = 1/sqrt(dot_product(coeff, matmul(ee, coeff)))
            coeff = coeff*s1

        end subroutine normalize_contracted_ao

    end subroutine make_bas_env

    subroutine get_int1e(this, int, mat)
        class(mole_t), intent(in) :: this
        character(len=*), intent(in) :: int
        real(kind=f8), dimension(:, :), allocatable, intent(out) :: mat

        allocate (mat(nao(this%bas), nao(this%bas)))
        select case (trim(int))
        case ("ovlp")
            call calc_ovlp_int(mat, this%atm, this%bas, this%env)
        case ("kin")
            call calc_kin_int(mat, this%atm, this%bas, this%env)
        case ("nuc")
            call calc_nuc_int(mat, this%atm, this%bas, this%env)
        case default
            call assert(.false., "Unknown integral requested") ! always failed
        end select

    end subroutine get_int1e

    subroutine print_geometry(this, unit)
        class(mole_t), intent(in) :: this
        integer, intent(in) :: unit
        integer :: i

        call assert(this%built, "the instance is not built")

        call print_header("Geometry", unit)
        write (unit, "(/,'Charge = ',g0,', Spin multiplicity = ',g0)") this%charge, this%mult
        write (unit, "('Cartesian coordinates (in Angstroms):')")
        write (unit, "(/,'     Atom           X                 Y                 Z')")
        write (unit, "('    ------  ----------------  ----------------  ----------------')")
        do i = 1, size(this%atoms)
            write (unit, "(A8,1X,3F18.10)") this%atoms(i)%s, this%atoms(i)%xyz*bohr_to_angstrom
        end do
        write (unit, "(/,'The number of pure spherical functions = ',g0)") nao(this%bas)
        write (unit, "('The number of shells = ',g0)") nshell(this%bas)
        write (unit, "(A)")

    end subroutine print_geometry

end module ringo_mole_detail
