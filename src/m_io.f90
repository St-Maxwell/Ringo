module mod_io
    use mod_const
    use iso_fortran_env, only: STD_OUT => output_unit, &
                               STD_ERR => error_unit
    implicit none

    integer, protected :: OUTPUT_FILE_UNIT
    integer, protected :: INPUT_FILE_UNIT
    integer, protected :: BASIS_FILE_UNIT
    integer, parameter :: MAX_LINE_WIDTH = 80
    character(len=60), protected :: input_file
    character(len=60), protected :: output_file

contains

    subroutine get_cmd_input()
        integer :: num_args
        integer :: i

        num_args = command_argument_count()
        if (num_args == 0) then
            call error_terminate(STD_ERR, "Input file not specified.")
        else if (num_args == 1) then
            call get_command_argument(1, input_file)
            i = index(input_file, ".inp", back=.true.)
            output_file = input_file(:i-1) // ".out"
        else
            call get_command_argument(1, input_file)
            call get_command_argument(2, output_file)
        end if

    end subroutine get_cmd_input

    subroutine open_output_file()
        integer :: stat

        open(newunit=OUTPUT_FILE_UNIT, file=output_file, &
             action='write',iostat=stat)

        if (stat /= 0) then
            call error_terminate(STD_ERR, "Can not open output file.")
        end if

    end subroutine open_output_file
    
    subroutine load_input_file(filename, stat)
        ! load input file
        ! stat = 0 : open file successfully
        ! stat = 1 : open file failed
        ! stat = 2 : didn't find the file
        character(len=*), intent(in) :: filename
        integer, intent(out) :: stat
        !===================================================
        logical :: alive

        inquire(file=filename, exist=alive)
        if (.not. alive) then
            stat = 2
            return
        end if

        open(newunit=INPUT_FILE_UNIT, file=filename, &
             action='read', status='old', iostat=stat)
        if (stat /= 0) stat = 1

    end subroutine load_input_file

    subroutine read_geometry(num_atom, atom, geom, charge, spin)
        integer :: num_atom
        character(len=2), dimension(:), allocatable :: atom
        real(kind=r8), dimension(:,:), allocatable :: geom
        integer :: charge
        integer :: spin
        !===================================================
        character(len=MAX_LINE_WIDTH) :: line
        integer :: i
        integer :: stat

        call locate_keyword("geometry", INPUT_FILE_UNIT, stat)
        if (stat /= 0) then
            call error_terminate(OUTPUT_FILE_UNIT, "no geometry block")
        end if

        read(INPUT_FILE_UNIT, "(A)", iostat=stat) line
        read(INPUT_FILE_UNIT, "(A)", iostat=stat) line
        num_atom = 0
        do while (.true.)
            read(INPUT_FILE_UNIT, "(A)", iostat=stat) line
            if (index(line, '}') /= 0) exit
            num_atom = num_atom + 1
        end do

        allocate(atom(num_atom))
        allocate(geom(3,num_atom))

        call backspace_lines(num_atom+2, INPUT_FILE_UNIT)
        read(INPUT_FILE_UNIT, *) charge, spin
        do i = 1, num_atom
            read(INPUT_FILE_UNIT, *) atom(i), geom(:,i)
        end do

    end subroutine read_geometry

    subroutine basis_input(basis)
        character(len=20) :: basis
        !===================================================
        character(len=20) :: line
        integer :: stat

        call locate_keyword("basis", INPUT_FILE_UNIT, stat)
        read(INPUT_FILE_UNIT, "(A)", iostat=stat) line
        line = return_config(line, '=')
        basis = adjustl(line)

    end subroutine basis_input

    subroutine read_job()
        character(len=40) :: line
        integer :: stat

        call load_input_file(input_file, stat)
        if (stat /= 0) then
            call error_terminate(STD_OUT, "Open input file failed")
        end if

        call locate_keyword("method", INPUT_FILE_UNIT, stat)
        if (stat /= 0) then
            call error_terminate(OUTPUT_FILE_UNIT, "Method not defined.")
        end if

        read(INPUT_FILE_UNIT, "(A)") line
        line = return_config(line, '=')
        
        select case(lower(trim(line)))
        case("rhf")
            SCFJob = RHFJob
        case("uhf")
            SCFJob = UHFJob
        case("rohf")
            SCFJob = ROHFJob
        end select

        call close_input_file(stat)

    end subroutine read_job

    subroutine locate_keyword(keyword, unit, stat)
        character(len=*) :: keyword
        integer, intent(in) :: unit
        integer :: stat
        !===================================================
        character(len=MAX_LINE_WIDTH) :: line

        rewind(unit)
        do while (.true.)
            read(unit, "(A)", iostat=stat) line
            if (stat /= 0) exit ! END_OF_FILE
            if (index(line, keyword) /=0) then
                backspace(unit) ! found keyword
                exit
            end if
        end do

    end subroutine locate_keyword

    subroutine close_input_file(stat)
        integer :: stat
        
        close(INPUT_FILE_UNIT, iostat=stat)

    end subroutine close_input_file

    subroutine close_output_file()
        close(OUTPUT_FILE_UNIT)
    end subroutine close_output_file

    subroutine backspace_lines(n, unit)
        integer, intent(in) :: n
        integer, intent(in) :: unit
        !===================================================
        integer :: i

        do i = 1, n
            backspace(unit)
        end do

    end subroutine backspace_lines

    subroutine load_basis_set(basis_name, stat)
        ! load input file
        ! stat = 0 : open file successfully
        ! stat = 1 : open file failed
        ! stat = 2 : didn't find the file
        character(len=*), intent(in) :: basis_name
        integer, intent(out) :: stat
        !===================================================
        character(len=:), allocatable :: basis_path
        character(len=:), allocatable :: filename
        logical :: alive
    
        basis_path = "./basis/"
        filename = basis_path // lower(trim(basis_name)) // ".gbs"

        inquire(file=filename, exist=alive)
        if (.not. alive) then
            stat = 2
            return
        end if
    
        open(newunit=BASIS_FILE_UNIT, file=filename, &
             action='read', status='old', iostat=stat)
        if (stat /= 0) stat = 1
    
    end subroutine load_basis_set

    subroutine close_basis_file(stat)
        integer :: stat
        
        close(BASIS_FILE_UNIT, iostat=stat)

    end subroutine close_basis_file

    pure function return_config(instr, delimiter) result(config)
        character(len=*), intent(in) :: instr
        character(len=1), intent(in) :: delimiter
        character(len=:), allocatable :: config
        
        integer :: idx
        
        idx = scan(instr, delimiter)
        config = adjustl(instr(idx+1:))
    
    end function return_config

    function lower(string)
        character(len=*) :: string
        character(len=:), allocatable :: lower
        !===================================================
        integer :: ascii
        integer :: i

        lower = string
        do i = 1, len(lower)
            ascii = ichar(lower(i:i))
            if (ascii >= 65 .and. ascii<= 90) ascii = ascii + 32
            lower(i:i) = char(ascii)
        end do

    end function lower

    subroutine welcome()
    
        write(OUTPUT_FILE_UNIT,*) "                                        "
        write(OUTPUT_FILE_UNIT,*) " ====================================== "
        write(OUTPUT_FILE_UNIT,*) "   ______ _____ __   _  ______  _____   "
        write(OUTPUT_FILE_UNIT,*) "  |_____/   |   | \  | |  ____ |     |  "
        write(OUTPUT_FILE_UNIT,*) "  |    \_ __|__ |  \_| |_____| |_____|  "
        write(OUTPUT_FILE_UNIT,*) "                                        "
        write(OUTPUT_FILE_UNIT,*) "       Ringo Is Not Gaussian/Orca       "
        write(OUTPUT_FILE_UNIT,*) "                                        "
        write(OUTPUT_FILE_UNIT,*) "        Programmed by St Maxwell        "
        write(OUTPUT_FILE_UNIT,*) "         st_maxwell@outlook.com         "
        write(OUTPUT_FILE_UNIT,*) " ====================================== "
        write(OUTPUT_FILE_UNIT,*) "                                        "

    end subroutine welcome

    subroutine normal_terminate()
        
        !call execution_time()
        call close_output_file()
        stop

    end subroutine normal_terminate

    subroutine error_terminate(unit, msg)
        integer, intent(in) :: unit
        character(len=*) :: msg

        write(unit, "(A,A)") "Error Terminate: ", trim(msg)
        stop

    end subroutine error_terminate

end module mod_io