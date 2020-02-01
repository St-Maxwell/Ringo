module mod_mol
    use iso_fortran_env, only: r8 => real64
    use mod_io
    implicit none
    
    type Molecule
        integer :: num_atom
        character(len=2), dimension(:), allocatable :: atom
        real(kind=r8), dimension(:,:), allocatable :: geom
        integer :: charge
        integer :: multiplicity
    contains
        procedure :: init => init_from_input
        procedure :: destroy => destroy_molecule
    end type Molecule

contains
    
    subroutine init_from_input(this, filename)
        class(Molecule), intent(inout) :: this
        character(len=*), intent(in) :: filename
        !===================================================
        integer :: stat

        call load_input_file(filename, stat)

        call read_geometry(this%num_atom, &
                           this%atom, &
                           this%geom, &
                           this%charge, &
                           this%multiplicity)
        
        call close_input_file(stat)

    end subroutine init_from_input

    subroutine destroy_molecule(this)
        class(Molecule) :: this

        deallocate(this%atom)
        deallocate(this%geom)

    end subroutine destroy_molecule

end module mod_mol