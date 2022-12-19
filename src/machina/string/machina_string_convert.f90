module machina_string_convert
    use iso_fortran_env
    use ryu, only: real32_to_string => f2shortest, &
                   real64_to_string => d2shortest, &
                   real64_to_string_fixed => d2fixed
    use machina_error
    implicit none
    private
    public :: string_to_int, string_to_real
    public :: to_string

    interface string_to_int
        module procedure :: string_to_int8
        module procedure :: string_to_int8_with_error
        module procedure :: string_to_int16
        module procedure :: string_to_int16_with_error
        module procedure :: string_to_int32
        module procedure :: string_to_int32_with_error
        module procedure :: string_to_int64
        module procedure :: string_to_int64_with_error
    end interface

    interface string_to_real
        module procedure :: string_to_real32
        module procedure :: string_to_real32_with_error
        module procedure :: string_to_real64
        module procedure :: string_to_real64_with_error
        module procedure :: string_to_real128
        module procedure :: string_to_real128_with_error
    end interface

    interface to_string
        module procedure :: int32_to_string
        module procedure :: int64_to_string
        module procedure :: real32_to_string
        module procedure :: real64_to_string
        module procedure :: real64_to_string_fixed
    end interface

contains

    subroutine string_to_int8(string, v)
        character(len=*), intent(in) :: string
        integer(kind=int8), intent(out) :: v

        read (string, *) v

    end subroutine string_to_int8

    subroutine string_to_int8_with_error(string, v, error)
        character(len=*), intent(in) :: string
        integer(kind=int8), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to integer")

    end subroutine string_to_int8_with_error

    subroutine string_to_int16(string, v)
        character(len=*), intent(in) :: string
        integer(kind=int16), intent(out) :: v

        read (string, *) v

    end subroutine string_to_int16

    subroutine string_to_int16_with_error(string, v, error)
        character(len=*), intent(in) :: string
        integer(kind=int16), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to integer")

    end subroutine string_to_int16_with_error

    subroutine string_to_int32(string, v)
        character(len=*), intent(in) :: string
        integer(kind=int32), intent(out) :: v

        read (string, *) v

    end subroutine string_to_int32

    subroutine string_to_int32_with_error(string, v, error)
        character(len=*), intent(in) :: string
        integer(kind=int32), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to integer")

    end subroutine string_to_int32_with_error

    subroutine string_to_int64(string, v)
        character(len=*), intent(in) :: string
        integer(kind=int64), intent(out) :: v

        read (string, *) v

    end subroutine string_to_int64

    subroutine string_to_int64_with_error(string, v, error)
        character(len=*), intent(in) :: string
        integer(kind=int64), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to integer")

    end subroutine string_to_int64_with_error

    subroutine string_to_real32(string, v)
        character(len=*), intent(in) :: string
        real(kind=real32), intent(out) :: v

        read (string, *) v

    end subroutine string_to_real32

    subroutine string_to_real32_with_error(string, v, error)
        character(len=*), intent(in) :: string
        real(kind=real32), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to real")

    end subroutine string_to_real32_with_error

    subroutine string_to_real64(string, v)
        character(len=*), intent(in) :: string
        real(kind=real64), intent(out) :: v

        read (string, *) v

    end subroutine string_to_real64

    subroutine string_to_real64_with_error(string, v, error)
        character(len=*), intent(in) :: string
        real(kind=real64), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to real")

    end subroutine string_to_real64_with_error

    subroutine string_to_real128(string, v)
        character(len=*), intent(in) :: string
        real(kind=real128), intent(out) :: v

        read (string, *) v

    end subroutine string_to_real128

    subroutine string_to_real128_with_error(string, v, error)
        character(len=*), intent(in) :: string
        real(kind=real128), intent(out) :: v
        type(error_t), intent(out) :: error
        integer :: istat

        read (string, *, iostat=istat) v
        if (istat /= 0) call raise_error(error, "Can not convert """//string//""" to real")

    end subroutine string_to_real128_with_error

    !> int32 to string
    pure function int32_to_string(int) result(str)
        integer(kind=int32), value :: int
        character(len=:), allocatable :: str
        character(len=range(int) + 2) :: buffer
        character(len=1), dimension(0:*), parameter :: digits = &
            ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
        integer :: n
        logical :: has_sign
        integer :: pos

        has_sign = int < 0
        if (has_sign) int = -int

        pos = len(buffer)
        do
            if (int < 10) then
                buffer(pos:pos) = digits(int)
                exit
            end if

            n = mod(int, 10)
            buffer(pos:pos) = digits(n)
            pos = pos - 1
            int = int/10
        end do

        if (has_sign) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        str = buffer(pos:)

    end function int32_to_string

    !> int64 to string
    pure function int64_to_string(int) result(str)
        integer(kind=int64), value :: int
        character(len=:), allocatable :: str
        character(len=range(int) + 2) :: buffer
        character(len=1), dimension(0:*), parameter :: digits = &
            ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
        integer :: n
        logical :: has_sign
        integer :: pos

        has_sign = int < 0
        if (has_sign) int = -int

        pos = len(buffer)
        do
            if (int < 10) then
                buffer(pos:pos) = digits(int)
                exit
            end if

            n = mod(int, 10)
            buffer(pos:pos) = digits(n)
            pos = pos - 1
            int = int/10
        end do

        if (has_sign) then
            pos = pos - 1
            buffer(pos:pos) = '-'
        end if

        str = buffer(pos:)

    end function int64_to_string

end module machina_string_convert
