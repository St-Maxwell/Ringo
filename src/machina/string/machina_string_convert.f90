module machina_string_convert
    use iso_fortran_env
    use machina_error
    implicit none
    private
    public :: string_to_int, string_to_real

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

end module machina_string_convert
