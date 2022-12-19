program main
    use ringo_cli
    use ringo_born
    use ringo_dead
    use machina_error
    implicit none
    type(cli_t) :: args
    type(error_t) :: error

    call parse_command_line_args(args, error)
    if (.has.error) then
        call ringo_crash(error%msg)
    end if

    call ringo_launch(args%input, args%output, args%num_thread, error)
    if (.has.error) then
        call ringo_crash(error%msg)
    end if

    call ringo_terminate()

end program main
