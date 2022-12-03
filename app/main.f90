program main
    use ringo_cli
    use ringo_fatal_error
    use machina_error
    implicit none
    type(cli_t) :: args
    type(error_t) :: error

    call parse_command_line_args(args, error)
    if (.has.error) then
        call ringo_crash(error%msg)
    end if

    print*, args%input
    print*, args%output
    print*, args%num_thread
 
end program main
