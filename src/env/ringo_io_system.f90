module ringo_io_system
    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    private
    public :: std_out, std_err
    public :: redirect_std_output

    !> standard output unit of ringo
    !> can be redirected to a file unit
    integer, protected :: std_out = output_unit
    !> standard error unit of ringo
    integer, protected :: std_err = error_unit

contains

    subroutine redirect_std_output(unit)
        integer, intent(in) :: unit

        std_out = unit

    end subroutine redirect_std_output

end module ringo_io_system
