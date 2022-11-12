module machina_kinds
    use iso_fortran_env
    implicit none
    private
    public :: i4, i8
    public :: f4, f8, f16
    public :: c4, c8, c16

    !> kind specifier of 4 byte integer
    integer, parameter :: i4 = int32
    !> kind specifier of 8 byte integer
    integer, parameter :: i8 = int64
    !> kind specifier of 4 byte real
    integer, parameter :: f4 = real32
    !> kind specifier of 8 byte real
    integer, parameter :: f8 = real64
    !> kind specifier of 16 byte real
    integer, parameter :: f16 = real128
    !> kind specifier of 4 byte complex
    integer, parameter :: c4 = real32
    !> kind specifier of 8 byte complex
    integer, parameter :: c8 = real64
    !> kind specifier of 16 byte complex
    integer, parameter :: c16 = real128

end module machina_kinds
