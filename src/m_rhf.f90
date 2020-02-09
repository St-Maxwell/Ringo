module mod_rhf
    use mod_const
    use mod_mol
    use mod_basis
    use mod_libcint
    use mod_SCF
    use mod_algorithm
    implicit none
    
    type, extends(SCF) :: RHF
        integer :: numAlpha
        real(kind=r8), dimension(:,:), allocatable :: S ! overlap matrix
        real(kind=r8), dimension(:,:), allocatable :: X ! transform matrix
        real(kind=r8), dimension(:,:), allocatable :: T ! kinetic matrix
        real(kind=r8), dimension(:,:), allocatable :: V ! electron-nucleus attraction
        real(kind=r8), dimension(:,:), allocatable :: H ! core-Hamiltonian matrix
        real(kind=r8), dimension(:,:), allocatable :: C ! MO coefficient matrix
        real(kind=r8), dimension(:,:), allocatable :: P ! density matrix
        real(kind=r8), dimension(:,:), allocatable :: F ! Fock matrix
        real(kind=r8), dimension(:), allocatable :: orbEnergy
        type(LibcintArray) :: cint
    contains
        procedure :: init => initialize_rhf
        procedure :: calcOvlp => calc_overlap_matrix
        procedure :: calcKin => calc_kinetic_matrix
        procedure :: calcNuAttrc => calc_nucl_attrc_matrix
        procedure :: orthOrbital => orthogonalize_orbital
        procedure :: initGuess => initial_guess_rhf
        procedure :: buildFock => build_Fock_matrix_rhf
        procedure :: diagFock => diagonolize_Fock_rhf
        procedure :: calcDensity => calc_density_matrix_rhf
        procedure :: elecEnergy => electron_energy_rhf
        procedure :: totalEnergy => total_energy_rhf
        procedure :: getDensity => get_density_rhf
        final :: destroy_rhf
    end type RHF

contains

    subroutine initialize_rhf(this, mole, basis)
        class(RHF) :: this
        class(Molecule), intent(in) :: mole
        class(BasisSet), intent(in) :: basis
        !===================================================
        integer :: istat

        this%numAlpha = mole%numElectron / 2
        this%ec%nuRepulsion = mole%nuRepuls()
        call this%cint%init(mole, basis)
        allocate(this%S(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%X(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%T(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%V(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%H(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%C(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%P(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%F(basis%numBasis, basis%numBasis), stat=istat)
        allocate(this%orbEnergy(basis%numBasis), stat=istat)

        call this%calcOvlp()
        call this%calcKin()
        call this%calcNuAttrc()

        call this%orthOrbital()

    end subroutine initialize_rhf

    subroutine orthogonalize_orbital(this)
        class(RHF) :: this
        real(kind=r8), dimension(:,:), allocatable :: eigen_vec
        real(kind=r8), dimension(:), allocatable :: eigen_val

        allocate(eigen_vec, mold=this%S)
        allocate(eigen_val(size(this%S, dim=1)))

        call solve_eigen(this%S, eigen_val, eigen_vec)
        this%X =  matmul(matmul(eigen_vec,diag(1/sqrt(eigen_val))),transpose(eigen_vec))

    end subroutine orthogonalize_orbital

    subroutine initial_guess_rhf(this)
        class(RHF) :: this

        this%P = 0._r8
        this%H = this%T + this%V

    end subroutine initial_guess_rhf

    subroutine build_Fock_matrix_rhf(this)
        class(RHF) :: this
        !===================================================
        real(kind=r8), dimension(:,:,:,:), allocatable :: buf2e
        real(kind=r8), dimension(:,:), allocatable :: Jm
        real(kind=r8), dimension(:,:), allocatable :: Km
        integer, dimension(4) :: shls
        integer, external :: CINTcgto_spheric
        integer :: i, j, k, l, di, dj, dk, dl
        integer :: ioff, joff, koff, loff
        integer :: x, y

        allocate(Jm, mold=this%P)
        allocate(Km, mold=this%P)
        
        Jm = 0._r8; Km = 0._r8

        ioff = 0
        do i = 1, this%cint%numShell
            di = CINTcgto_spheric(i-1, this%cint%bas)
            joff = 0
            do j = 1, this%cint%numShell
                dj = CINTcgto_spheric(j-1, this%cint%bas)
                koff = 0
                do k = 1, this%cint%numShell
                    dk = CINTcgto_spheric(k-1, this%cint%bas)
                    loff = 0
                    do l = 1, this%cint%numShell
                        dl = CINTcgto_spheric(l-1, this%cint%bas)

                        shls = [i-1, j-1, k-1, l-1]
                        allocate(buf2e(di,dj,dk,dl))

                        call cint2e_sph(buf2e, shls, this%cint%atm, this%cint%numAtom, &
                                        this%cint%bas, this%cint%numShell, this%cint%env, 0_8)
                        
                        do y = loff+1, loff+dl
                            do x = joff+1, joff+dj
                                Km(x,y) = Km(x,y) + sum(this%P(ioff+1:ioff+di,koff+1:koff+dk) * buf2e(:,x-joff,:,y-loff))
                            end do
                            do x = koff+1, koff+dk
                                Jm(x,y) = Jm(x,y) + sum(this%P(ioff+1:ioff+di,joff+1:joff+dj) * buf2e(:,:,x-koff,y-loff))
                            end do
                        end do

                        deallocate(buf2e)

                        loff = loff + dl
                    end do
                    koff = koff + dk
                end do
                joff = joff + dj
            end do
            ioff = ioff + di
        end do

        this%F = this%H + 2 * Jm - Km

    end subroutine build_Fock_matrix_rhf

    subroutine diagonolize_Fock_rhf(this)
        class(RHF) :: this
        real(kind=r8), dimension(:,:), allocatable :: FPrime
        real(kind=r8), dimension(:,:), allocatable :: CPrime

        allocate(FPrime, mold=this%F)
        allocate(CPrime, mold=this%S)

        FPrime = matmul(transpose(this%X), matmul(this%F,this%X)) 

        call solve_eigen(FPrime, this%orbEnergy, CPrime)

        this%C = matmul(this%X, CPrime)

    end subroutine diagonolize_Fock_rhf

    subroutine calc_density_matrix_rhf(this)
        class(RHF) :: this

        this%P = matmul(this%C(:,:this%numAlpha), transpose(this%C(:,:this%numAlpha)))

    end subroutine calc_density_matrix_rhf

    function total_energy_rhf(this)
        class(RHF) :: this
        real(kind=r8) :: total_energy_rhf

        total_energy_rhf = this%elecEnergy() + this%ec%nuRepulsion

    end function total_energy_rhf

    function electron_energy_rhf(this) result(E_ele)
        class(RHF) :: this
        real(kind=r8) :: E_ele

        E_ele = sum(this%P * (this%H + this%F))

    end function electron_energy_rhf

    pure function get_density_rhf(this) result(P)
        class(RHF), intent(in) :: this
        real(kind=r8), dimension(:,:), allocatable :: P

        allocate(P, source=this%P)

    end function get_density_rhf

    subroutine calc_overlap_matrix(this)
        class(RHF) :: this
        !===================================================
        real(kind=r8), dimension(:,:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer, external :: CINTcgto_spheric
        integer :: i, j, di, dj, x, y

        do i = 1, this%cint%numShell
            shls(1) = i - 1
            di = CINTcgto_spheric(i-1, this%cint%bas)
            do j = 1, this%cint%numShell
                shls(2) = j - 1
                dj = CINTcgto_spheric(j-1, this%cint%bas)
                allocate(buf1e(di,dj))

                x = this%cint%shellIndex(i); y = this%cint%shellIndex(j)
                call cint1e_ovlp_sph(buf1e, shls, this%cint%atm, this%cint%numAtom, &
                                     this%cint%bas, this%cint%numShell, this%cint%env)
                this%S(x:,y:) = buf1e(:,:)

                deallocate(buf1e)
            end do
        end do

    end subroutine calc_overlap_matrix

    subroutine calc_kinetic_matrix(this)
        class(RHF) :: this
        !===================================================
        real(kind=r8), dimension(:,:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer, external :: CINTcgto_spheric
        integer :: i, j, di, dj, x, y

        do i = 1, this%cint%numShell
            shls(1) = i - 1
            di = CINTcgto_spheric(i-1, this%cint%bas)
            do j = 1, this%cint%numShell
                shls(2) = j - 1
                dj = CINTcgto_spheric(j-1, this%cint%bas)
                allocate(buf1e(di,dj))

                x = this%cint%shellIndex(i); y = this%cint%shellIndex(j)
                call cint1e_kin_sph(buf1e, shls, this%cint%atm, this%cint%numAtom, &
                                    this%cint%bas, this%cint%numShell, this%cint%env)
                this%T(x:,y:) = buf1e(:,:)

                deallocate(buf1e)
            end do
        end do
        
    end subroutine calc_kinetic_matrix

    subroutine calc_nucl_attrc_matrix(this)
        class(RHF) :: this
        !===================================================
        real(kind=r8), dimension(:,:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer, external :: CINTcgto_spheric
        integer :: i, j, di, dj, x, y

        do i = 1, this%cint%numShell
            shls(1) = i - 1
            di = CINTcgto_spheric(i-1, this%cint%bas)
            do j = 1, this%cint%numShell
                shls(2) = j - 1
                dj = CINTcgto_spheric(j-1, this%cint%bas)
                allocate(buf1e(di,dj))

                x = this%cint%shellIndex(i); y = this%cint%shellIndex(j)
                call cint1e_nuc_sph(buf1e, shls, this%cint%atm, this%cint%numAtom, &
                                    this%cint%bas, this%cint%numShell, this%cint%env)
                this%V(x:,y:) = buf1e(:,:)

                deallocate(buf1e)
            end do
        end do
        
    end subroutine calc_nucl_attrc_matrix

    subroutine destroy_rhf(this)
        type(RHF) :: this

        call this%cint%destroy()
        deallocate(this%S)
        deallocate(this%X)
        deallocate(this%T)
        deallocate(this%V)
        deallocate(this%C)
        deallocate(this%P)
        deallocate(this%F)
        deallocate(this%orbEnergy)

    end subroutine destroy_rhf

end module mod_rhf