module machina_string_utils
    use machina_basic
    use machina_string_ascii
    use machina_error
    implicit none

contains

    !> replace all old character in string by new
    subroutine replace(string, old, new)
        character(len=*), intent(inout) :: string
        character(len=1), intent(in) :: old
        character(len=1), intent(in) :: new
        integer :: i

        do i = 1, len(string)
            if (string(i:i) == old) string(i:i) = new
        end do

    end subroutine replace

    !> split the string based on seperator characters
    !> the default seperator is " "
    subroutine tokenize(string, tokens, sep)
        use machina_vla, only: vla_char
        character(len=*), intent(in) :: string
        type(vla_char), intent(out) :: tokens
        character(len=*), intent(in), optional :: sep
        character(len=:), allocatable :: sep_
        integer :: pos, lastpos

        sep_ = optval(sep, " ")
        lastpos = find_first_not_of(string, sep_)
        pos = find_first_of(string, sep_, lastpos)
        do while (pos <= len(string) .or. lastpos <= len(string))
            call tokens%push_back(string(lastpos:pos - 1))
            lastpos = find_first_not_of(string, sep_, pos)
            pos = find_first_of(string, sep_, lastpos)
        end do

    end subroutine tokenize

    pure function find_first_not_of(string, char, pos) result(idx)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: char
        integer, intent(in), optional :: pos
        integer :: idx

        do idx = optval(pos, 1), len(string)
            if (index(char, string(idx:idx)) == 0) return
        end do
        idx = len(string) + 1

    end function find_first_not_of

    pure function find_first_of(string, char, pos) result(idx)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: char
        integer, intent(in), optional :: pos
        integer :: idx

        do idx = optval(pos, 1), len(string)
            if (index(char, string(idx:idx)) /= 0) return
        end do
        idx = len(string) + 1

    end function find_first_of

    !> Convert character variable to lower case
    pure function to_lower(string) result(lower_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: lower_string
        integer :: i

        do i = 1, len(string)
            lower_string(i:i) = char_to_lower(string(i:i))
        end do

    end function to_lower

    !> Convert character variable to upper case
    pure function to_upper(string) result(upper_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: upper_string
        integer :: i

        do i = 1, len(string)
            upper_string(i:i) = char_to_upper(string(i:i))
        end do

    end function to_upper

    !> Converts character sequence to title case
    pure function to_title(string) result(title_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: title_string
        integer :: i
        logical :: capitalize_switch

        capitalize_switch = .true.
        do i = 1, len(string)
            if (is_alphanum(string(i:i))) then
                if (capitalize_switch) then
                    title_string(i:i) = char_to_upper(string(i:i))
                    capitalize_switch = .false.
                else
                    title_string(i:i) = char_to_lower(string(i:i))
                end if
            else
                title_string(i:i) = string(i:i)
                capitalize_switch = .true.
            end if
        end do

    end function to_title

    !> Converts character sequence to sentence case
    pure function to_sentence(string) result(sentence_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: sentence_string
        integer :: i, n

        n = len(string)
        do i = 1, len(string)
            if (is_alphanum(string(i:i))) then
                sentence_string(i:i) = char_to_upper(string(i:i))
                n = i
                exit
            else
                sentence_string(i:i) = string(i:i)
            end if
        end do

        do i = n + 1, len(string)
            sentence_string(i:i) = char_to_lower(string(i:i))
        end do

    end function to_sentence

    !> Reverse the character order in the input character variable
    pure function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n

        n = len(string)
        do i = 1, n
            reverse_string(n - i + 1:n - i + 1) = string(i:i)
        end do

    end function reverse

    pure function is_all_alpha(string)
        character(len=*), intent(in) :: string
        logical :: is_all_alpha
        integer :: i

        is_all_alpha = .false.
        do i = 1, len(string)
            if (.not. is_alpha(string(i:i))) return
        end do
        is_all_alpha = .true.

    end function is_all_alpha

    pure function is_all_digit(string)
        character(len=*), intent(in) :: string
        logical :: is_all_digit
        integer :: i

        is_all_digit = .false.
        do i = 1, len(string)
            if (.not. is_digit(string(i:i))) return
        end do
        is_all_digit = .true.

    end function is_all_digit

    subroutine read_whole_file(file, string, error)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: string
        type(error_t), intent(out) :: error
        character(len=:), allocatable :: line
        integer :: u, length, istat

        open (newunit=u, file=file, action="read", status="old", iostat=istat)

        if (istat /= 0) then
            call raise_error(error, "Unable to open the file """//trim(file)//"""")
            return
        end if

        string = ""
        do while (istat == 0)
            call getline(u, line, istat)
            if (istat == 0) string = string//line//LF
        end do

        if (is_iostat_end(istat)) istat = 0
        if (istat /= 0) then
            call raise_error(error, "Could not read the file """//trim(file)//"""")
            close (u)
            return
        end if

        close (u)

    end subroutine read_whole_file

    subroutine getline(unit, line, iostat)
        !> Formatted IO unit
        integer, intent(in) :: unit
        !> Line to read
        character(len=:), allocatable, intent(out) :: line
        !> Status of operation
        integer, intent(out) :: iostat

        integer, parameter :: bufsize = 4096
        character(len=bufsize) :: buffer
        integer :: chunk, stat
        logical :: opened

        if (unit /= -1) then
            inquire (unit=unit, opened=opened)
        else
            opened = .false.
        end if

        if (opened) then
            open (unit=unit, pad="yes", iostat=iostat)
        else
            iostat = 1
        end if

        line = ""
        do while (iostat == 0)
            read (unit, '(a)', advance='no', iostat=iostat, size=chunk) buffer
            if (iostat > 0) exit
            line = line//buffer(:chunk)
        end do
        if (is_iostat_eor(iostat)) iostat = 0

    end subroutine getline

end module machina_string_utils
