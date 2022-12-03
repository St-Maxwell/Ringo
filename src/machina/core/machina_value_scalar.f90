module machina_value_scalar
    use machina_basic, only: i4, f8
    use machina_value_base
    implicit none
    private
    public :: int_value, real_value, cmplx_value, bool_value, char_value
    public :: construct_scalar
    public :: cast_to_int, cast_to_real, cast_to_cmplx, cast_to_bool, cast_to_char

    type, extends(machina_value) :: int_value
        integer(kind=i4) :: raw
    end type

    type, extends(machina_value) :: real_value
        real(kind=f8) :: raw
    end type

    type, extends(machina_value) :: cmplx_value
        complex(kind=f8) :: raw
    end type

    type, extends(machina_value) :: bool_value
        logical :: raw
    end type

    type, extends(machina_value) :: char_value
        character(len=:), allocatable :: raw
    end type

    interface construct_scalar
        module procedure :: construct_int_scalar
        module procedure :: construct_real_scalar
        module procedure :: construct_cmplx_scalar
        module procedure :: construct_bool_scalar
        module procedure :: construct_char_scalar
    end interface

contains

    subroutine construct_int_scalar(val, v)
        class(machina_value), allocatable, intent(out) :: val
        integer(kind=i4), intent(in) :: v

        allocate (int_value :: val)
        select type (val)
        type is (int_value)
            val%raw = v
        end select

    end subroutine construct_int_scalar

    subroutine construct_real_scalar(val, v)
        class(machina_value), allocatable, intent(out) :: val
        real(kind=f8), intent(in) :: v

        allocate (real_value :: val)
        select type (val)
        type is (real_value)
            val%raw = v
        end select

    end subroutine construct_real_scalar

    subroutine construct_cmplx_scalar(val, v)
        class(machina_value), allocatable, intent(out) :: val
        complex(kind=f8), intent(in) :: v

        allocate (cmplx_value :: val)
        select type (val)
        type is (cmplx_value)
            val%raw = v
        end select

    end subroutine construct_cmplx_scalar

    subroutine construct_bool_scalar(val, v)
        class(machina_value), allocatable, intent(out) :: val
        logical, intent(in) :: v

        allocate (bool_value :: val)
        select type (val)
        type is (bool_value)
            val%raw = v
        end select

    end subroutine construct_bool_scalar

    subroutine construct_char_scalar(val, v)
        class(machina_value), allocatable, intent(out) :: val
        character(len=*), intent(in) :: v

        allocate (char_value :: val)
        select type (val)
        type is (char_value)
            val%raw = v
        end select

    end subroutine construct_char_scalar

    function cast_to_int(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        integer(kind=i4), pointer :: v

        v => null()
        select type (ptr)
        type is (int_value)
            v => ptr%raw
        end select

    end function cast_to_int

    function cast_to_real(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        real(kind=f8), pointer :: v

        v => null()
        select type (ptr)
        type is (real_value)
            v => ptr%raw
        end select

    end function cast_to_real

    function cast_to_cmplx(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        complex(kind=f8), pointer :: v

        v => null()
        select type (ptr)
        type is (cmplx_value)
            v => ptr%raw
        end select

    end function cast_to_cmplx

    function cast_to_bool(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        logical, pointer :: v

        v => null()
        select type (ptr)
        type is (bool_value)
            v => ptr%raw
        end select

    end function cast_to_bool

    function cast_to_char(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        character(len=:), pointer :: v

        v => null()
        select type (ptr)
        type is (char_value)
            v => ptr%raw
        end select

    end function cast_to_char

end module machina_value_scalar
