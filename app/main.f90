program main
    use ringo_cli
    use ringo_parse_input
    use ringo_born
    use ringo_dead
    use machina_error
    implicit none
    type(cli_t) :: args
    type(config_t) :: config
    type(error_t) :: error

    !> parse command line arguments
    call parse_command_line_args(args, error)
    if (.has.error) call ringo_crash(error%msg)

    !> start up
    call ringo_launch(args%input, args%output, args%num_thread, error)
    if (.has.error) call ringo_crash(error%msg)

    !> parse input file
    call parse_input(config, error)
    if (.has.error) call ringo_crash(error%msg)

    write(std_out,*) config%basis
    write(std_out,*) config%method
    write(std_out,*) config%jobtype
    write(std_out,*) config%verbose

    !> normal terminate
    call ringo_terminate()

end program main
