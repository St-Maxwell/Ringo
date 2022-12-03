module ringo_cli
    use iso_fortran_env, only: output_unit
    use machina_string
    use machina_error
    use ringo_env
    implicit none
    private
    public :: cli_t, parse_command_line_args

    type :: cli_t
        character(len=:), allocatable :: input
        character(len=:), allocatable :: output
        integer :: num_thread = 1
    end type

contains

    !> parse command line arguments
    subroutine parse_command_line_args(args, error)
        type(cli_t), intent(out) :: args
        type(error_t), intent(out) :: error
        integer :: nargs
        character(len=:), allocatable :: arg
        integer :: i

        logical :: catch_inp

        nargs = command_argument_count()
        catch_inp = .false.

        i = 1
        do while (i <= nargs)
            call get_arg(i, arg)
            i = i + 1
            select case (arg)
            case ('-h', '--help')
                call print_help_text()
                stop
            case ('-v', '--version')
                write(output_unit,"(g0)") ringo_version
                stop
            case default
                args%input = arg
                catch_inp = .true.
                exit
            end select
        end do

        if (.not. catch_inp) then
            call raise_error(error, "Error: Input file not specified")
            return
        end if

        call parse_run_args(args, i, error)

    end subroutine parse_command_line_args

    subroutine parse_run_args(config, start, error)
        type(cli_t), intent(inout) :: config
        integer, intent(in) :: start
        type(error_t), intent(out) :: error

        integer :: nargs
        character(len=:), allocatable :: arg
        integer :: i

        logical :: catch_out
        logical :: expect_out

        nargs = command_argument_count()
        catch_out = .false.
        expect_out = .true.

        i = start
        do while (i <= nargs)
            call get_arg(i, arg)
            i = i + 1
            select case (arg)
            case ('-nt', '--nthread')
                if (i > nargs) then
                    call raise_error(error, "Error: The option of -nt is missed")
                    return
                else
                    call get_arg(i, arg)
                    i = i + 1
                end if
                call string_to_int(arg, config%num_thread, error)
                if (.has.error) return
                expect_out = .false.
            case default
                if (expect_out) then
                    config%output = arg
                    catch_out = .true.
                    expect_out = .false.
                else
                    call raise_error(error, "Error: Unknow argument """//arg//"""")
                    return
                end if
            end select
        end do

        if (.not. catch_out) config%output = config%input//".out"

    end subroutine parse_run_args

    subroutine get_arg(n, arg)
        integer, intent(in) :: n
        character(len=:), allocatable, intent(out) :: arg
        integer :: length

        call get_command_argument(n, length=length)

        if (length > 0) allocate (character(len=length) :: arg)

        call get_command_argument(n, value=arg)

    end subroutine get_arg

    subroutine print_help_text()
        character(len=*), parameter :: help = &
        "Ringo - Ringo Is Not Gaussian/Orca"//LF//LF// &
        "Usage: ringo INPUTFILE [OUTPUTFILE] [OPTIONS]"//LF//LF// &
        "       ringo [-h|--help]|[-v|--version]"//LF//LF// &
        "Options:"//LF// &
        "  INPUTFILE      Input file name."//LF//LF// &
        "  INPUTFILE      Output file name. If not given, the output filename"//LF// &
        "                 defaults to INPUTFILE with "".out"" extension."//LF//LF// &
        "  -nt,--nthread  Number of threads to use. Default value is 1."//LF//LF// &
        "  -h,--help      Show help text and exit."//LF//LF// &
        "  -v,--version   Show version information and exit."//LF

        write (output_unit, "(g0)") help

    end subroutine print_help_text

end module ringo_cli
