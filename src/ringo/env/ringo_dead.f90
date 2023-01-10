module ringo_dead
    use ringo_env
    use machina_date_time, only: get_date_time
    use machina_timer, only: format_time
    implicit none
    private
    public :: ringo_terminate, ringo_crash

contains

    subroutine ringo_terminate()

        !> clean up
        call ringo_clean_up()

        !> print leave message

        !> stop timing
        call ringo_clock%stop()
        write (std_out, "(/,'Total Run Time: ',A)") &
            format_time(ringo_clock%get_elapsed_time())

        write (std_out, "('Ringo Normal Termination on: ',A)") get_date_time()

        !> close output file
        call close_std_output()

    end subroutine ringo_terminate

    subroutine ringo_crash(msg)
        character(len=*), intent(in) :: msg

        write (std_err, "('[RINGO FATAL ERROR] ',A)") msg
        write (std_err, "(/,'Ringo Error Termination on: ',A)") get_date_time()

        call ringo_clean_up()
        error stop

    end subroutine ringo_crash

    subroutine ringo_clean_up()

    end subroutine ringo_clean_up

end module ringo_dead
