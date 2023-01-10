module ringo_parse_section
    use machina_basic, only: f8
    use machina_string, only: LF, string_to_int, string_to_real, string_to_bool
    use machina_vla
    use machina_error
    implicit none
    private
    public :: get_key_scalar_value, get_key_array_value

    interface get_key_scalar_value
        module procedure :: get_key_scalar_value_char
        module procedure :: get_key_scalar_value_int
        module procedure :: get_key_scalar_value_real
        module procedure :: get_key_scalar_value_bool
    end interface

    interface get_key_array_value
        module procedure :: get_key_array_value_char
        module procedure :: get_key_array_value_int
        module procedure :: get_key_array_value_real
        module procedure :: get_key_array_value_bool
    end interface

    type :: token_t
        integer :: first
        integer :: last
        integer :: kind
    end type

    enum, bind(c)
        enumerator :: string, comma, equal, exclamation, space
    end enum

contains

    subroutine split_tokens(line, tokens)
        character(len=*), intent(in) :: line
        type(token_t), dimension(:), allocatable, intent(out) :: tokens

        type(token_t) :: token
        integer :: pos

        tokens = [token_t ::]
        pos = 0
        do while (pos < len(line))
            call get_token(line, pos, token)
            select case (token%kind)
            case (space)
                cycle
            case (exclamation)
                ! the rest of line is comment
                ! no effect token any more
                exit
            case default
                tokens = [tokens, token]
            end select
        end do

    contains

        subroutine get_token(input, pos, token)
            character(len=*), intent(in) :: input
            integer, intent(inout) :: pos
            type(token_t), intent(out) :: token

            pos = pos + 1
            select case (line(pos:pos))
            case (' ')
                token = token_t(pos, pos, space)
            case ('=')
                token = token_t(pos, pos, equal)
            case (',')
                token = token_t(pos, pos, comma)
            case ('!')
                token = token_t(pos, pos, exclamation)
            case default
                token%first = pos
                do while (pos <= len(input))
                    if (index(" =,!", input(pos:pos)) /= 0) then
                        pos = pos - 1
                        token%last = pos
                        exit
                    else
                        pos = pos + 1
                    end if
                end do
                if (pos == len(input) + 1) token%last = len(input)
                token%kind = string
            end select

        end subroutine get_token

    end subroutine split_tokens

    subroutine get_key_scalar_value_char(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        character(len=:), allocatable, intent(out) :: val
        type(error_t), intent(out) :: error

        type(token_t), dimension(:), allocatable :: tokens
        logical :: meet_eq
        integer :: i

        call split_tokens(line, tokens)
        if (size(tokens) <= 1) then
            call raise_error(error, "Invalid line, no key-value"//LF//"  | "//line)
            return
        end if

        meet_eq = .false.
        do i = 1, size(tokens)
            select case (i)
            case (1)
                if (tokens(1)%kind /= string) then
                    call raise_error(error, "Invalid character")
                    error%msg = invalid_line_error_msg(line, tokens(1)%first, error%msg)
                    return
                end if
            case (2)
                if (tokens(2)%kind == equal) then
                    if (size(tokens) < 3) then
                        call raise_error(error, "Invalid character")
                        error%msg = invalid_line_error_msg(line, tokens(2)%first, error%msg)
                        return
                    end if
                    meet_eq = .true.
                else if (tokens(2)%kind == comma) then
                    call raise_error(error, "Invalid character")
                    error%msg = invalid_line_error_msg(line, tokens(2)%first, error%msg)
                    return
                end if
            case (3)
                if (.not. meet_eq .or. tokens(3)%kind /= string) then
                    call raise_error(error, "Invalid character")
                    error%msg = invalid_line_error_msg(line, tokens(3)%first, error%msg)
                    return
                end if
            case default
                call raise_error(error, "Invalid character")
                error%msg = invalid_line_error_msg(line, tokens(i)%first, error%msg)
                return
            end select
        end do

        key = line(tokens(1)%first:tokens(1)%last)
        if (meet_eq) then
            val = line(tokens(3)%first:tokens(3)%last)
        else
            val = line(tokens(2)%first:tokens(2)%last)
        end if

    end subroutine get_key_scalar_value_char

    subroutine get_key_scalar_value_int(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        integer, intent(out) :: val
        type(error_t), intent(out) :: error
        character(len=:), allocatable :: val_string

        call get_key_scalar_value(line, key, val_string, error)
        if (.has.error) return

        call string_to_int(val_string, val, error)
        if (.has.error) error%msg = error%msg//LF// &
                                    & "  | "//line

    end subroutine get_key_scalar_value_int

    subroutine get_key_scalar_value_real(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        real(kind=f8), intent(out) :: val
        type(error_t), intent(out) :: error
        character(len=:), allocatable :: val_string

        call get_key_scalar_value(line, key, val_string, error)
        if (.has.error) return

        call string_to_real(val_string, val, error)
        if (.has.error) error%msg = error%msg//LF// &
                                    & "  | "//line

    end subroutine get_key_scalar_value_real

    subroutine get_key_scalar_value_bool(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        logical, intent(out) :: val
        type(error_t), intent(out) :: error
        character(len=:), allocatable :: val_string

        call get_key_scalar_value(line, key, val_string, error)
        if (.has.error) return

        call string_to_bool(val_string, val, error)
        if (.has.error) error%msg = error%msg//LF// &
                                    & "  | "//line

    end subroutine get_key_scalar_value_bool

    subroutine get_key_array_value_char(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        type(vla_char), intent(out) :: val
        type(error_t), intent(out) :: error

        type(token_t), dimension(:), allocatable :: tokens
        logical :: meet_eq, meet_comma
        integer :: i, expect_kind

        call split_tokens(line, tokens)
        if (size(tokens) <= 1) then
            call raise_error(error, "Invalid line, no key-value"//LF//"  | "//line)
            return
        end if

        ! check the key
        if (tokens(1)%kind /= string) then
            call raise_error(error, "Invalid character")
            error%msg = invalid_line_error_msg(line, tokens(1)%first, error%msg)
            return
        end if
        key = line(tokens(1)%first:tokens(1)%last)

        ! check if there is an equal sign
        meet_eq = tokens(2)%kind == equal
        ! check the separator of array
        if (meet_eq) then
            if (size(tokens) > 3) then
                meet_comma = tokens(4)%kind == comma
            end if
        else
            if (size(tokens) > 2) then
                meet_comma = tokens(3)%kind == comma
            end if
        end if

        expect_kind = string
        do i = merge(3, 2, meet_eq), size(tokens)
            if (tokens(i)%kind == string .and. expect_kind == string) then
                call val%push_back(line(tokens(i)%first:tokens(i)%last))
                if (meet_comma) expect_kind = comma
            else if (tokens(i)%kind == comma .and. expect_kind == comma) then
                expect_kind = string
            else
                call raise_error(error, "Invalid character")
                error%msg = invalid_line_error_msg(line, tokens(i)%first, error%msg)
                return
            end if
        end do

        if (meet_comma .and. tokens(size(tokens))%kind == comma) then
            call raise_error(error, "Invalid character")
            error%msg = invalid_line_error_msg(line, tokens(size(tokens))%first, error%msg)
            return
        end if

    end subroutine get_key_array_value_char

    subroutine get_key_array_value_int(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        type(vla_int), intent(out) :: val
        type(error_t), intent(out) :: error
        type(vla_char) :: val_string
        integer :: v
        integer :: i

        call get_key_array_value(line, key, val_string, error)
        if (.has.error) return

        do i = 1, size(val_string)
            call string_to_int(val_string%at(i), v, error)
            if (.has.error) then
                error%msg = error%msg//LF//"  | "//line
                return
            end if
            call val%push_back(v)
        end do

    end subroutine get_key_array_value_int

    subroutine get_key_array_value_real(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        type(vla_real), intent(out) :: val
        type(error_t), intent(out) :: error
        type(vla_char) :: val_string
        real(kind=f8) :: v
        integer :: i

        call get_key_array_value(line, key, val_string, error)
        if (.has.error) return

        do i = 1, size(val_string)
            call string_to_real(val_string%at(i), v, error)
            if (.has.error) then
                error%msg = error%msg//LF//"  | "//line
                return
            end if
            call val%push_back(v)
        end do

    end subroutine get_key_array_value_real

    subroutine get_key_array_value_bool(line, key, val, error)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: key
        type(vla_bool), intent(out) :: val
        type(error_t), intent(out) :: error
        type(vla_char) :: val_string
        logical :: v
        integer :: i

        call get_key_array_value(line, key, val_string, error)
        if (.has.error) return

        do i = 1, size(val_string)
            call string_to_bool(val_string%at(i), v, error)
            if (.has.error) then
                error%msg = error%msg//LF//"  | "//line
                return
            end if
            call val%push_back(v)
        end do

    end subroutine get_key_array_value_bool

    function invalid_line_error_msg(input, first, msg) result(s)
        !> Input string to parse
        character(len=*), intent(in) :: input
        !> Offset in the input
        integer, intent(in) :: first
        !> Error message
        character(len=*), intent(in) :: msg
        character(len=:), allocatable :: s

        s = msg//LF// &
        & "  | "//input//LF// &
        & "  |"//repeat("-", first)//'^'

    end function invalid_line_error_msg

end module ringo_parse_section
