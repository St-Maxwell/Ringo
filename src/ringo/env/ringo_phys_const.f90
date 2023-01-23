module ringo_phys_const
    use machina_basic, only: f8
    implicit none
    private

    real(kind=f8), parameter, public :: PI = 3.14159265358979323846_f8

    ! unit conversion
    real(kind=f8), parameter, public :: bohr_to_angstrom = 0.529177210903_f8

end module ringo_phys_const
