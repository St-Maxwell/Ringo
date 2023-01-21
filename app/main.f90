program main
    use ringo_cli
    use ringo_config
    use ringo_driver
    use ringo_mole_detail
    use ringo_parse_input
    use ringo_env
    use ringo_born
    use ringo_dead
    use machina_error
    implicit none
    type(cli_t) :: args
    type(driver_t) :: driver
    type(config_t) :: config
    type(mole_t) :: mole
    type(error_t) :: error

    !> parse command line arguments
    call parse_command_line_args(args, error)
    if (.has.error) call ringo_crash(error%msg)

    !> start up
    call ringo_launch(args%input, args%output, args%num_thread, error)
    if (.has.error) call ringo_crash(error%msg)

    !> parse input file
    call parse_input(config, mole, error)
    if (.has.error) call ringo_crash(error%msg)

    call construct_jobs_driver(driver, config)
    call driver%run_jobs(config, mole, error)
    if (.has.error) call ringo_crash(error%msg)

    !> normal terminate
    call ringo_terminate()

end program main
