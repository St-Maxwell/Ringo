module mod_mol
    use mod_const
    use mod_io
    implicit none
    
    type Molecule
        integer :: numAtom
        character(len=2), dimension(:), allocatable :: atomSymbol
        real(kind=r8), dimension(:,:), allocatable :: geom
        integer, dimension(:), allocatable :: nuclearCharge
        integer :: charge
        integer :: multiplicity
        integer :: numElectron
    contains
        procedure :: init => init_from_input
        procedure :: destroy => destroy_molecule
        procedure :: nuRepuls => nuclear_repulsion
    end type Molecule

contains
    
    subroutine init_from_input(this, filename)
        class(Molecule), intent(inout) :: this
        character(len=*), intent(in) :: filename
        !===================================================
        integer :: stat

        call load_input_file(filename, stat)

        call read_geometry(this%numAtom, &
                           this%atomSymbol, &
                           this%geom, &
                           this%charge, &
                           this%multiplicity)
        
        call set_nuclear_charge(this)
        this%numElectron = sum(this%nuclearCharge) - this%charge
        
        call close_input_file(stat)

    end subroutine init_from_input

    subroutine set_nuclear_charge(mole)
        class(Molecule), intent(inout) :: mole
        !===================================================
        integer :: i, j
        ! gfortran (>=9) intrinsic procedure
        ! mole%nu_chrg = findloc(element_list, mole%atom)
        
        allocate(mole%nuclearCharge(mole%numAtom))
        do i = 1, mole%numAtom
            do j = lbound(element_list, dim=1), ubound(element_list, dim=1)
                if (mole%atomSymbol(i) == element_list(j)) exit
            end do
            if (j > num_supported_elements) stop "unsupported atom!" !mole%atom(i)
            mole%nuclearCharge(i) = j
        end do

    end subroutine set_nuclear_charge

    subroutine pack_coord(mole, coord)
        class(Molecule), intent(in) :: mole
        real(kind=r8), dimension(:), allocatable :: coord
        !===================================================
        integer :: i, j

        allocate(coord(mole%numAtom*3))
    
        j = 1
        do i = 1, mole%numAtom
            coord(j:j+2) = mole%geom(:,i)
            j = j + 3
        end do

    end subroutine pack_coord

    function nuclear_repulsion(this) result(E_nuc)
        class(Molecule), intent(in) :: this
        real(kind=r8) :: E_nuc
        !===================================================
        real(kind=r8) :: distance
        integer :: i, j

        E_nuc = 0._r8
        do i = 1, this%numAtom-1
            do j = i+1, this%numAtom
                distance = norm2(this%geom(:,i) - this%geom(:,j))
                E_nuc = E_nuc + this%nuclearCharge(i) * this%nuclearCharge(j) / distance
            end do
        end do

    end function nuclear_repulsion

    subroutine destroy_molecule(this)
        class(Molecule) :: this

        deallocate(this%atomSymbol)
        deallocate(this%geom)
        deallocate(this%nuclearCharge)

    end subroutine destroy_molecule

end module mod_mol