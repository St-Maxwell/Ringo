module ringo_mole_atom
    use machina_basic, only: f8
    use machina_string
    use machina_vra
    use machina_error
    use machina_assert
    use ringo_elements
    implicit none
    private
    public :: atom_t, format_atoms

    !> the internal format of the atoms in mole type
    !> do not use it for other purpose
    type :: atom_t
        !> atomic number
        integer :: n
        !> symbol of the atom
        character(len=:), allocatable :: s
        !> nuclear charge of the atom
        real(kind=f8) :: c
        !> weight of the atom, in atomic unit
        real(kind=f8) :: w
        !> cartesian coordinate of the atom
        real(kind=f8), dimension(3) :: xyz
    end type

contains

    !> convert the raw string to the internal format of atoms
    !> e.g. parse
    !> "H 0.0 0.0 0.0
    !>  H 0.0 0.0 0.7"
    !> to an array of type(atom) with size 2
    !> if parsing failed, raise a value_error
    subroutine format_atoms(str, atoms, error)
        character(len=*), intent(in) :: str
        type(atom_t), dimension(:), allocatable, intent(out) :: atoms
        type(error_t), intent(out) :: error
        ! local
        type(vra_char) :: lines
        integer :: atomic_number
        real(kind=f8), dimension(3) :: xyz
        integer :: i

        ! split str by line
        call tokenize(str, lines, sep=LF)
        ! check if empty line exists
        do i = 1, size(lines)
            if (len(trim(lines%at(i))) == 0) then
                ! raise an error
                call set_error(error, value_error(), "Empty line in coordinate")
                error%msg = empty_line_error_msg(lines, i, error%msg)
                return
            end if
        end do

        ! parse each line to get atom info
        allocate (atoms(0))
        do i = 1, size(lines)
            append_atom: block
                type(atom_t) :: newatom
                call parse_line(lines%at(i), atomic_number, xyz, error)
                if (.has.error) return
                call construct_atom(newatom, atomic_number, xyz)
                atoms = [atoms, newatom]
            end block append_atom
        end do

    contains

        function empty_line_error_msg(lines, pos, msg) result(s)
            type(vra_char), intent(in) :: lines
            integer, intent(in) :: pos
            character(len=*), intent(in) :: msg
            character(len=:), allocatable :: s

            if (pos == 1) then
                s = msg//LF// &
                & "  | "//"     <--- Empty line"//LF// &
                & "  | "//lines%at(2)
            else if (pos == size(lines)) then
                s = msg//LF// &
                & "  | "//lines%at(pos - 1)//LF// &
                & "  | "//"     <--- Empty line"
            else
                s = msg//LF// &
                & "  | "//lines%at(pos - 1)//LF// &
                & "  | "//"     <--- Empty line"//LF// &
                & "  | "//lines%at(pos + 1)
            end if

        end function empty_line_error_msg

    end subroutine format_atoms

    subroutine construct_atom(atm, atomic_number, xyz)
        type(atom_t), intent(out) :: atm
        integer, intent(in) :: atomic_number
        real(kind=f8), dimension(3), intent(in) :: xyz

        call debug_assert(atomic_number >= 0 .and. atomic_number <= num_supported_elements, &
                          msg="Unsupported atom")

        atm%s = element_list(atomic_number)
        atm%n = atomic_number
        atm%xyz = xyz
        atm%c = atomic_number
        atm%w = element_weight(atomic_number)

    end subroutine construct_atom

    subroutine parse_line(input, atomic_num, xyz, error)
        character(len=*), intent(in) :: input
        integer, intent(out) :: atomic_num
        real(kind=f8), dimension(3), intent(out) :: xyz
        type(error_t), intent(out) :: error

        integer :: count
        integer :: pos
        character(len=:), allocatable :: buf
        integer :: istat
        character(len=*), parameter :: validchar = ".+-"//digits//letters

        enum, bind(c)
        !! space: [ \t]
        !! string: [a-z] | [A-Z] | [0-9] | [.+-]
        !! invalid: others
            enumerator :: invalid, space, string
        end enum

        type :: token_type
            integer :: first, last, kind
        end type token_type

        type(token_type) :: token
        character(len=:), allocatable :: msg

        count = 0
        pos = 0
        do while (pos < len(input))
            call get_token(input, pos, token)
            select case (token%kind)
            case (string)
                count = count + 1
                select case (count)
                case (1)
                    buf = input(token%first:token%last)
                    atomic_num = symbol_to_number(buf)
                    if (atomic_num == -1) then
                        call set_error(error, value_error(), "Unknown atomic symbol")
                        error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                        return
                    end if
                case (2)
                    buf = input(token%first:token%last)
                    read (buf, *, iostat=istat) xyz(1)
                    if (istat /= 0) then
                        call set_error(error, value_error(), "Invalid number in coordinate")
                        error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                        return
                    end if
                case (3)
                    buf = input(token%first:token%last)
                    read (buf, *, iostat=istat) xyz(2)
                    if (istat /= 0) then
                        call set_error(error, value_error(), "Invalid number in coordinate")
                        error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                        return
                    end if
                case (4)
                    buf = input(token%first:token%last)
                    read (buf, *, iostat=istat) xyz(3)
                    if (istat /= 0) then
                        call set_error(error, value_error(), "Invalid number in coordinate")
                        error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                        return
                    end if
                case default
                    call set_error(error, value_error(), "Unexpected characters in coordinate")
                    error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                    return
                end select
            case (space)
                continue
            case default
                call set_error(error, value_error(), "Unexpected characters in coordinate")
                error%msg = invalid_line_error_msg(input, token%first, token%last, error%msg)
                return
            end select
        end do

        if (count /= 4) then
            call set_error(error, value_error(), "Invalid coordinate")
            error%msg = invalid_line_error_msg(input, len(input), len(input), error%msg)
        end if

    contains

        subroutine get_token(input, pos, token)
            !> Input string to tokenize
            character(len=*), intent(in) :: input
            !> Offset in input string, will be advanced
            integer, intent(inout) :: pos
            !> Returned token from the next position
            type(token_type), intent(out) :: token

            pos = pos + 1
            select case (input(pos:pos))
            case ('.', '+', '-', '0':'9', 'A':'Z', 'a':'z')
                token%first = pos
                do while (pos <= len(input))
                    if (scan(input(pos:pos), validchar) == 0) then
                        pos = pos - 1
                        token%last = pos
                        exit
                    else
                        pos = pos + 1
                        if (pos > len(input)) then
                            pos = pos - 1
                            token%last = pos
                            exit
                        end if
                    end if
                end do
                token%kind = string
            case (' ', TAB)
                token = token_type(pos, pos, space)
            case default
                token = token_type(pos, pos, invalid)
            end select

        end subroutine get_token

        function invalid_line_error_msg(input, first, last, msg) result(s)
            !> Input string to parse
            character(len=*), intent(in) :: input
            !> Offset in the input
            integer, intent(in) :: first, last
            !> Error message
            character(len=*), intent(in) :: msg
            character(len=:), allocatable :: s

            s = msg//LF// &
            & "  | "//input//LF// &
            & "  |"//repeat("-", first)//repeat("^", last - first + 1)

        end function invalid_line_error_msg

    end subroutine parse_line

end module ringo_mole_atom
