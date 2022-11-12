module ringo_mole_detail
    use machina_basic, only: f8
    use machina_error
    use ringo_mole_atom
    implicit none

    type :: mole
        private
        !> the total charge of the molecule
        integer :: charge = 0
        !> spin multiplicity of the molecule
        integer :: spin = 0
        !> the internal format of atoms in molecule
        type(atom_t), dimension(:), allocatable :: atoms
        !> the number of electrons
        integer :: nele
    end type

contains

    subroutine construct_mole(this, atoms, charge, spin, basis, error)
        type(mole), intent(out) :: this
        character(len=*), intent(in) :: atoms
        integer, intent(in) :: charge
        integer, intent(in) :: spin
        character(len=*), intent(in) :: basis
        type(error_t), intent(out) :: error
        ! local

        this%charge = charge
        this%spin = spin

        call format_atoms(atoms, this%atoms, error)
        if (.has.error) return

    end subroutine construct_mole

end module ringo_mole_detail
