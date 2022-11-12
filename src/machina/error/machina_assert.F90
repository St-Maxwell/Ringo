module machina_assert
    use iso_fortran_env
    implicit none
    private
    public :: debug
    public :: assert, debug_assert
    public :: assert_eq, assert_neq

#ifdef DEBUG
    logical, parameter :: debug = .true.
#else
    logical, parameter :: debug = .false.
#endif

    interface debug_assert_eq
        module procedure :: debug_assert_eq_int8
        module procedure :: debug_assert_eq_int16
        module procedure :: debug_assert_eq_int32
        module procedure :: debug_assert_eq_int64
        module procedure :: debug_assert_eq_real32
        module procedure :: debug_assert_eq_real64
        module procedure :: debug_assert_eq_real128
        module procedure :: debug_assert_eq_cmplx32
        module procedure :: debug_assert_eq_cmplx64
        module procedure :: debug_assert_eq_cmplx128
        module procedure :: debug_assert_eq_character
    end interface

    interface debug_assert_neq
        module procedure :: debug_assert_neq_int8
        module procedure :: debug_assert_neq_int16
        module procedure :: debug_assert_neq_int32
        module procedure :: debug_assert_neq_int64
        module procedure :: debug_assert_neq_real32
        module procedure :: debug_assert_neq_real64
        module procedure :: debug_assert_neq_real128
        module procedure :: debug_assert_neq_cmplx32
        module procedure :: debug_assert_neq_cmplx64
        module procedure :: debug_assert_neq_cmplx128
        module procedure :: debug_assert_neq_character
    end interface

    interface assert_eq
        module procedure :: assert_eq_int8
        module procedure :: assert_eq_int16
        module procedure :: assert_eq_int32
        module procedure :: assert_eq_int64
        module procedure :: assert_eq_real32
        module procedure :: assert_eq_real64
        module procedure :: assert_eq_real128
        module procedure :: assert_eq_cmplx32
        module procedure :: assert_eq_cmplx64
        module procedure :: assert_eq_cmplx128
        module procedure :: assert_eq_character
    end interface

    interface assert_neq
        module procedure :: assert_neq_int8
        module procedure :: assert_neq_int16
        module procedure :: assert_neq_int32
        module procedure :: assert_neq_int64
        module procedure :: assert_neq_real32
        module procedure :: assert_neq_real64
        module procedure :: assert_neq_real128
        module procedure :: assert_neq_cmplx32
        module procedure :: assert_neq_cmplx64
        module procedure :: assert_neq_cmplx128
        module procedure :: assert_neq_character
    end interface

contains

    subroutine debug_assert(debug_assertion, msg)
        logical, intent(in) :: debug_assertion
        character(len=*), intent(in), optional :: msg

        if (.not. debug_assertion) then
            write (error_unit, "('ASSERT FAILED')")
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert

    subroutine assert(debug_assertion, msg)
        logical, intent(in) :: debug_assertion
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert(debug_assertion, msg)

    end subroutine assert

    subroutine debug_assert_eq_int8(left, right, msg)
        integer(kind=int8), intent(in) :: left
        integer(kind=int8), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_int8

    subroutine debug_assert_eq_int16(left, right, msg)
        integer(kind=int16), intent(in) :: left
        integer(kind=int16), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_int16

    subroutine debug_assert_eq_int32(left, right, msg)
        integer(kind=int32), intent(in) :: left
        integer(kind=int32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_int32

    subroutine debug_assert_eq_int64(left, right, msg)
        integer(kind=int64), intent(in) :: left
        integer(kind=int64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_int64

    subroutine debug_assert_eq_real32(left, right, msg)
        real(kind=real32), intent(in) :: left
        real(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_real32

    subroutine debug_assert_eq_real64(left, right, msg)
        real(kind=real64), intent(in) :: left
        real(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_real64

    subroutine debug_assert_eq_real128(left, right, msg)
        real(kind=real128), intent(in) :: left
        real(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_real128

    subroutine debug_assert_eq_cmplx32(left, right, msg)
        complex(kind=real32), intent(in) :: left
        complex(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_cmplx32

    subroutine debug_assert_eq_cmplx64(left, right, msg)
        complex(kind=real64), intent(in) :: left
        complex(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_cmplx64

    subroutine debug_assert_eq_cmplx128(left, right, msg)
        complex(kind=real128), intent(in) :: left
        complex(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_cmplx128

    subroutine debug_assert_eq_character(left, right, msg)
        character(len=*), intent(in) :: left
        character(len=*), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left /= right) then
            write (error_unit, "('ASSERT_EQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_eq_character

    subroutine debug_assert_neq_int8(left, right, msg)
        integer(kind=int8), intent(in) :: left
        integer(kind=int8), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_int8

    subroutine debug_assert_neq_int16(left, right, msg)
        integer(kind=int16), intent(in) :: left
        integer(kind=int16), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_int16

    subroutine debug_assert_neq_int32(left, right, msg)
        integer(kind=int32), intent(in) :: left
        integer(kind=int32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_int32

    subroutine debug_assert_neq_int64(left, right, msg)
        integer(kind=int64), intent(in) :: left
        integer(kind=int64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_int64

    subroutine debug_assert_neq_real32(left, right, msg)
        real(kind=real32), intent(in) :: left
        real(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_real32

    subroutine debug_assert_neq_real64(left, right, msg)
        real(kind=real64), intent(in) :: left
        real(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_real64

    subroutine debug_assert_neq_real128(left, right, msg)
        real(kind=real128), intent(in) :: left
        real(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_real128

    subroutine debug_assert_neq_cmplx32(left, right, msg)
        complex(kind=real32), intent(in) :: left
        complex(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_cmplx32

    subroutine debug_assert_neq_cmplx64(left, right, msg)
        complex(kind=real64), intent(in) :: left
        complex(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_cmplx64

    subroutine debug_assert_neq_cmplx128(left, right, msg)
        complex(kind=real128), intent(in) :: left
        complex(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_cmplx128

    subroutine debug_assert_neq_character(left, right, msg)
        character(len=*), intent(in) :: left
        character(len=*), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (left == right) then
            write (error_unit, "('ASSERT_NEQ FAILED')")
            write (error_unit, "('LEFT = ',g0)") left
            write (error_unit, "('RIGHT = ',g0)") right
            if (present(msg)) write (error_unit, "('MSG: ',A)") msg
            error stop
        end if

    end subroutine debug_assert_neq_character

    subroutine assert_eq_int8(left, right, msg)
        integer(kind=int8), intent(in) :: left
        integer(kind=int8), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_int8

    subroutine assert_eq_int16(left, right, msg)
        integer(kind=int16), intent(in) :: left
        integer(kind=int16), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_int16

    subroutine assert_eq_int32(left, right, msg)
        integer(kind=int32), intent(in) :: left
        integer(kind=int32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_int32

    subroutine assert_eq_int64(left, right, msg)
        integer(kind=int64), intent(in) :: left
        integer(kind=int64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_int64

    subroutine assert_eq_real32(left, right, msg)
        real(kind=real32), intent(in) :: left
        real(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_real32

    subroutine assert_eq_real64(left, right, msg)
        real(kind=real64), intent(in) :: left
        real(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_real64

    subroutine assert_eq_real128(left, right, msg)
        real(kind=real128), intent(in) :: left
        real(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_real128

    subroutine assert_eq_cmplx32(left, right, msg)
        complex(kind=real32), intent(in) :: left
        complex(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_cmplx32

    subroutine assert_eq_cmplx64(left, right, msg)
        complex(kind=real64), intent(in) :: left
        complex(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_cmplx64

    subroutine assert_eq_cmplx128(left, right, msg)
        complex(kind=real128), intent(in) :: left
        complex(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_cmplx128

    subroutine assert_eq_character(left, right, msg)
        character(len=*), intent(in) :: left
        character(len=*), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_eq(left, right, msg)

    end subroutine assert_eq_character

    subroutine assert_neq_int8(left, right, msg)
        integer(kind=int8), intent(in) :: left
        integer(kind=int8), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_int8

    subroutine assert_neq_int16(left, right, msg)
        integer(kind=int16), intent(in) :: left
        integer(kind=int16), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_int16

    subroutine assert_neq_int32(left, right, msg)
        integer(kind=int32), intent(in) :: left
        integer(kind=int32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_int32

    subroutine assert_neq_int64(left, right, msg)
        integer(kind=int64), intent(in) :: left
        integer(kind=int64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_int64

    subroutine assert_neq_real32(left, right, msg)
        real(kind=real32), intent(in) :: left
        real(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_real32

    subroutine assert_neq_real64(left, right, msg)
        real(kind=real64), intent(in) :: left
        real(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_real64

    subroutine assert_neq_real128(left, right, msg)
        real(kind=real128), intent(in) :: left
        real(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_real128

    subroutine assert_neq_cmplx32(left, right, msg)
        complex(kind=real32), intent(in) :: left
        complex(kind=real32), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_cmplx32

    subroutine assert_neq_cmplx64(left, right, msg)
        complex(kind=real64), intent(in) :: left
        complex(kind=real64), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_cmplx64

    subroutine assert_neq_cmplx128(left, right, msg)
        complex(kind=real128), intent(in) :: left
        complex(kind=real128), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_cmplx128

    subroutine assert_neq_character(left, right, msg)
        character(len=*), intent(in) :: left
        character(len=*), intent(in) :: right
        character(len=*), intent(in), optional :: msg

        if (debug) call debug_assert_neq(left, right, msg)

    end subroutine assert_neq_character

end module machina_assert
