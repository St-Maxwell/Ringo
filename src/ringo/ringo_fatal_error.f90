module ringo_fatal_error
    use iso_fortran_env, only: error_unit
    implicit none
    private
    public :: ringo_crash

contains

    subroutine ringo_crash(msg)
        character(len=*), intent(in) :: msg

        write (error_unit, "('[RINGO FATAL ERROR] ',A)") msg
        error stop

    end subroutine ringo_crash

end module ringo_fatal_error
