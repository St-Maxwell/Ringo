module mod_scf
    use mod_const
    use mod_io
    use mod_mol
    use mod_basis
    implicit none
    
    integer :: SCF_E_CONVERGENCE = 6
    integer :: SCF_D_CONVERGENCE = 6
    integer :: SCF_MAX_ITER = 128

    type EnergyComponent
        real(kind=r8) :: nuRepulsion
    end type EnergyComponent

    type, abstract :: SCF
        type(EnergyComponent) :: ec
    contains
        procedure(initialize), deferred :: init
        procedure(initial_guess), deferred :: initGuess
        procedure(build_Fock_matrix), deferred :: buildFock
        procedure(diagonolize_Fock), deferred :: diagFock
        procedure(calc_density_matrix), deferred :: calcDensity 
        procedure(total_energy), deferred :: totalEnergy
        procedure(get_density), deferred :: getDensity
    end type SCF

    abstract interface
        subroutine initialize(this, mole, basis)
            import :: SCF, Molecule, BasisSet
            class(SCF) :: this
            class(Molecule), intent(in) :: mole
            class(BasisSet), intent(in) :: basis
        end subroutine initialize

        subroutine initial_guess(this)
            import :: SCF
            class(SCF) :: this
        end subroutine initial_guess

        subroutine diagonolize_Fock(this)
            import :: SCF
            class(SCF) :: this
        end subroutine diagonolize_Fock

        subroutine calc_density_matrix(this)
            import :: SCF
            class(SCF) :: this
        end subroutine calc_density_matrix

        subroutine build_Fock_matrix(this)
            import :: SCF
            class(SCF) :: this
        end subroutine build_Fock_matrix

        function total_energy(this)
            import :: SCF, r8
            class(SCF) :: this
            real(kind=r8) :: total_energy
        end function total_energy

        pure function get_density(this)
            import :: SCF, r8
            class(SCF), intent(in) :: this
            real(kind=r8), dimension(:,:), allocatable :: get_density
        end function get_density
    end interface

contains
    

end module mod_scf