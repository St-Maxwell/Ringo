module ringo_env
    use iso_fortran_env, only: output_unit, error_unit
    use ringo_input_sections, only: section_t
    use machina_string, only: LF, read_whole_file
    use machina_timer, only: timer_t
    use machina_error
    implicit none
    private
    public :: ringo_version, ringo_welcome
    public :: std_out, std_err
    public :: load_input_file, raw_input_file, sections
    public :: open_std_output, close_std_output
    public :: set_num_threads, num_threads
    public :: ringo_clock

    !> version information of ringo
    character(len=*), parameter :: ringo_version = &
        "Version:     0.0.2, alpha"//LF// &
        "Program:     ringo"//LF// &
        "Description: Ringo Is Not Gaussian/Orca"//LF// &
        "Home Page:   https://github.com/St-Maxwell/Ringo"//LF// &
        "License:     MIT"//LF

    !> ringo welcome message
    character(len=*), parameter :: ringo_welcome = &
        "    -----------------------------------------------------------------------"//LF// &
        "                   Ringo: Ringo Is Not Gaussian/Orca"//LF// &
        "                         Version: 0.0.2, alpha"//LF//LF// &
        "                                 Author"//LF// &
        "                  St Maxwell (st_maxwell@outlook.com)"//LF//LF// &
        "                  https://github.com/St-Maxwell/Ringo"//LF//LF// &
        "    -----------------------------------------------------------------------"//LF

    !> standard output unit of ringo
    !> can be redirected to a file unit
    integer, protected :: std_out = output_unit
    !> standard error unit of ringo
    integer, protected :: std_err = error_unit

    !> buffer of input file
    character(len=:), allocatable, protected :: raw_input_file
    !> sections in input file
    type(section_t), dimension(:), allocatable :: sections

    !> number of threads for OpenMP parallel
    integer, protected :: num_threads = 1

    !> global clock
    type(timer_t) :: ringo_clock

contains

    subroutine load_input_file(file, error)
        !> input file name
        character(len=*), intent(in) :: file
        type(error_t), intent(out) :: error

        call read_whole_file(file, raw_input_file, error)

    end subroutine load_input_file

    subroutine open_std_output(file, error)
        !> output file name
        character(len=*), intent(in) :: file
        type(error_t), intent(out) :: error
        ! local
        integer :: u, istat

        open (newunit=u, file=file, action="write", status="replace", iostat=istat)
        if (istat /= 0) then
            call raise_error(error, "Could not create output file """//trim(file)//"""")
            return
        end if

        std_out = u
        std_err = u

    end subroutine open_std_output

    subroutine close_std_output()
        logical :: opened

        inquire (unit=std_out, opened=opened)
        if (opened) close (std_out)

    end subroutine close_std_output

    subroutine set_num_threads(num)
        integer, intent(in) :: num

        num_threads = num

    end subroutine set_num_threads

end module ringo_env
