module ringo_fatal_error
    use ringo_io_system, only: std_err
    implicit none
    private
    public :: ringo_crash

contains

    subroutine ringo_crash(msg)
        character(len=*), intent(in) :: msg

        write (std_err, "('[RINGO FATAL ERROR] ',A)") msg
        error stop

    end subroutine ringo_crash

end module ringo_fatal_error
