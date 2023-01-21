module ringo_input_sections
    use machina_string
    use machina_vla, only: vla_char, size
    use machina_error
    implicit none
    private
    public :: section_t, parse_input_sections, get_section

    !> $name
    !>   content
    !> $end
    type :: section_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: content
    end type

contains

    subroutine parse_input_sections(source, sections, error)
        !> raw string of input file
        character(len=*), intent(in) :: source
        !> parsed sections
        type(section_t), dimension(:), allocatable, intent(out) :: sections
        type(error_t), intent(out) :: error

        type(section_t) :: section
        type(vla_char) :: lines
        character(len=:), allocatable :: buf, name
        logical :: in_section ! if a $IDENTIFIER has been matched
        integer :: i, j

        sections = [section_t ::]

        call tokenize(source, lines, sep=LF)

        in_section = .false.
        do i = 1, size(lines)
            scan_lines: block
                character(len=:), allocatable :: line
                integer :: istat

                line = trim(adjustl(to_lower(lines%at(i))))

                ! empty line, do nothing
                if (len(line) == 0) cycle

                ! meet '$'
                if (line(1:1) == '$') then
                    call get_identifier(lines%at(i), name, error)
                    if (.has.error) return

                    name = to_lower(name)
                    if (name == "end") then
                        if (in_section) then
                            section%content = buf(2:) ! remove the first LF character
                            ! check if is duplicate
                            do j = 1, size(sections)
                                if (sections(j)%name == section%name) then
                                    call raise_error(error, "Found duplicate section $"//section%name)
                                    return
                                end if
                            end do
                            sections = [sections, section]
                            in_section = .false.
                        else
                            call raise_error(error, "Unexpected $end in"//LF// &
                                                  & "  | "//lines%at(i)//"     <---")
                            return
                        end if
                    else
                        section%name = name
                        buf = ""
                        in_section = .true.
                    end if
                    cycle
                end if

                ! other case
                if (in_section) then
                    buf = buf//LF//lines%at(i)
                else
                    call raise_error(error, "Invalid line in"//LF// &
                                          & "  | "//lines%at(i)//"     <---")
                    return
                end if
            end block scan_lines
        end do

        if (in_section) call raise_error(error, "Unclosed section $"//section%name)

    contains

        subroutine get_identifier(line, string, error)
            character(len=*), intent(in) :: line
            character(len=:), allocatable, intent(out) :: string
            type(error_t), intent(out) :: error

            logical :: id_ok
            integer :: first, last ! index for identifier
            integer :: pos

            pos = 0
            do
                pos = pos + 1
                if (pos > len(line)) then
                    ! this is not expected to happen
                    call raise_error(error, "Invalid line in "//LF//line)
                    return
                end if
                select case (line(pos:pos))
                case (' ')
                    cycle
                case ('$')
                    exit ! found the first '$'
                case default
                    call raise_error(error, "Unexpected character in")
                    error%msg = invalid_line_error_msg(line, pos, error%msg)
                    return
                end select
            end do

            if (pos + 1 > len(line)) then
                call raise_error(error, "Identifier is not found")
                error%msg = invalid_line_error_msg(line, pos, error%msg)
                return
            end if

            pos = pos + 1
            first = pos
            if (.not. is_alpha(line(pos:pos))) then
                call raise_error(error, "Invalid identifier")
                error%msg = invalid_line_error_msg(line, pos, error%msg)
                return
            end if

            id_ok = .false.
            do
                pos = pos + 1
                if (pos > len(line)) then
                    if (.not. id_ok) then
                        last = pos - 1
                        id_ok = .true.
                    end if
                    exit
                end if

                select case (line(pos:pos))
                case (' ')
                    if (.not. id_ok) then
                        last = pos - 1
                        id_ok = .true.
                    end if
                case ('0':'9', 'A':'Z', '_', 'a':'z')
                    if (id_ok) then
                        call raise_error(error, "Unexpected character in")
                        error%msg = invalid_line_error_msg(line, pos, error%msg)
                        return
                    end if
                case default
                    if (id_ok) then
                        call raise_error(error, "Unexpected character in")
                    else
                        call raise_error(error, "Invalid identifier")
                    end if
                    error%msg = invalid_line_error_msg(line, pos, error%msg)
                    return
                end select
            end do

            string = line(first:last)

        end subroutine get_identifier

        function invalid_line_error_msg(input, pos, msg) result(s)
            !> Input string to parse
            character(len=*), intent(in) :: input
            !> Offset in the input
            integer, intent(in) :: pos
            !> Error message
            character(len=*), intent(in) :: msg
            character(len=:), allocatable :: s

            s = msg//LF// &
            & "  | "//input//LF// &
            & "  |"//repeat("-", pos)//"^"

        end function invalid_line_error_msg

    end subroutine parse_input_sections

    function get_section(sections, name) result(ptr)
        type(section_t), dimension(:), intent(in), target :: sections
        character(len=*), intent(in) :: name
        type(section_t), pointer :: ptr
        integer :: i

        ptr => null()
        do i = 1, size(sections)
            if (sections(i)%name == name) then
                ptr => sections(i)
                return
            end if
        end do

    end function get_section

end module ringo_input_sections
