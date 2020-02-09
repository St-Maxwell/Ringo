module mod_libcint
    use mod_const
    use mod_mol
    use mod_basis
    implicit none
    
    type LibcintArray
        integer, dimension(:,:), allocatable :: atm
        integer, dimension(:,:), allocatable :: bas
        real(kind=r8), dimension(:), allocatable :: env
        integer :: numShell
        integer :: numAtom
        integer, dimension(:), allocatable :: shellIndex
    contains
        procedure :: init => initialize_cintarray
        procedure :: destroy => destroy_cintarray
    end type LibcintArray

    integer, parameter :: CHARGE_OF  = 1
    integer, parameter :: PTR_COORD  = 2
    integer, parameter :: NUC_MOD_OF = 3
    integer, parameter :: PTR_ZETA   = 4
    integer, parameter :: ATM_SLOTS  = 6
    integer, parameter :: ATOM_OF    = 1
    integer, parameter :: ANG_OF     = 2
    integer, parameter :: NPRIM_OF   = 3
    integer, parameter :: NCTR_OF    = 4
    integer, parameter :: KAPPA_OF   = 5
    integer, parameter :: PTR_EXP    = 6
    integer, parameter :: PTR_COEFF  = 7
    integer, parameter :: BAS_SLOTS  = 8
    integer, parameter :: PTR_ENV_START = 20

contains
    
    subroutine initialize_cintarray(this, mole, basis)
        class(LibcintArray), intent(inout) :: this
        class(Molecule), intent(in) :: mole
        class(BasisSet), intent(in) :: basis
        !===================================================
        real(kind=r8), dimension(:), allocatable :: tmp_coeff
        real(kind=r8), dimension(:), allocatable :: tmp_expnt
        integer, dimension(:), allocatable :: tmp_angl
        real(kind=r8), dimension(:), allocatable :: tmp_coord
        integer :: off, prim_off
        integer :: iatom, ishl, iprim, ioff
        integer :: di, dj
        real(kind=r8), dimension(:,:), allocatable :: buf1e
        integer, dimension(2) :: shls
        real(kind=r8), external :: CINTgto_norm
        integer, external :: CINTcgto_spheric
        integer :: istat

        this%numShell = basis%numShell
        this%numAtom = mole%numAtom
        this%shellIndex = basis%shellIndex
        
        allocate(this%atm(ATM_SLOTS, mole%NumAtom), stat=istat)
        allocate(this%bas(BAS_SLOTS, basis%numShell), stat=istat)
        allocate(this%env(PTR_ENV_START + mole%NumAtom*3 + basis%numPrimitive*2), stat=istat)

        call pack_basis_gto(basis, tmp_expnt, tmp_coeff, tmp_angl)
        call pack_coord(mole, tmp_coord)

        off = PTR_ENV_START
        do iatom = 1, mole%NumAtom
            this%atm(CHARGE_OF,iatom) = mole%nuclearCharge(iatom)
            this%atm(PTR_COORD,iatom) = off
            this%atm(NUC_MOD_OF,iatom) = 1
            this%env(off+1:) = mole%geom(:,iatom)
            off = off + 3
        end do

        prim_off = 1
        do ishl = 1, basis%numShell
            this%bas(ATOM_OF,ishl) = basis%atomIndex(ishl) - 1 ! 0-based
            this%bas(ANG_OF,ishl) = basis%angl(ishl)
            this%bas(NPRIM_OF,ishl) = basis%cntrOrder(ishl)
            this%bas(NCTR_OF,ishl) = 1
  
            this%bas(PTR_EXP,ishl) = off
            ioff = 0
            do iprim = prim_off, prim_off + basis%cntrOrder(ishl)-1
                this%env(off+1+ioff) = tmp_expnt(iprim)
                ioff = ioff + 1
            end do
  
            off = off + basis%cntrOrder(ishl)
  
            this%bas(PTR_COEFF,ishl) = off
            ioff = 0
            do iprim = prim_off, prim_off + basis%cntrOrder(ishl)-1
                this%env(off+1+ioff) = tmp_coeff(iprim) * CINTgto_norm(tmp_angl(ishl), tmp_expnt(iprim))
                ioff = ioff + 1
            end do
  
            off = off + basis%cntrOrder(ishl)
  
            prim_off = prim_off + basis%cntrOrder(ishl)
        end do

        ! normalize 
        do ishl = 1, basis%numShell
            shls(1) = ishl - 1
            shls(2) = ishl - 1
            di = CINTcgto_spheric(ishl-1, this%bas)
            dj = CINTcgto_spheric(ishl-1, this%bas)
            allocate(buf1e(di,dj))
            call cint1e_ovlp_sph(buf1e,shls,this%atm,mole%numAtom,this%bas,basis%numShell,this%env)
            do iprim = 1, basis%cntrOrder(ishl)
                this%env(this%bas(PTR_COEFF,ishl)+iprim) = this%env(this%bas(PTR_COEFF,ishl)+iprim) / sqrt(buf1e(1,1))
            end do
            deallocate(buf1e)
        end do

    end subroutine initialize_cintarray

    subroutine destroy_cintarray(this)
        class(LibcintArray) :: this

        deallocate(this%atm)
        deallocate(this%bas)
        deallocate(this%env)
        deallocate(this%shellIndex)

    end subroutine destroy_cintarray

end module mod_libcint