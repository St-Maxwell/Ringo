module machina_optval
    use machina_kinds
    implicit none
    private
    public :: optval

    !> Fallback value for optional arguments
    interface optval
        module procedure optval_f4
        module procedure optval_f8
        module procedure optval_i4
        module procedure optval_i8
        module procedure optval_c4
        module procedure optval_c8
        module procedure optval_bool
        module procedure optval_char
    end interface optval

contains

    pure elemental function optval_f4(x, default) result(y)
        real(f4), intent(in), optional :: x
        real(f4), intent(in) :: default
        real(f4) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_f4

    pure elemental function optval_f8(x, default) result(y)
        real(f8), intent(in), optional :: x
        real(f8), intent(in) :: default
        real(f8) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_f8

    pure elemental function optval_i4(x, default) result(y)
        integer(i4), intent(in), optional :: x
        integer(i4), intent(in) :: default
        integer(i4) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_i4

    pure elemental function optval_i8(x, default) result(y)
        integer(i8), intent(in), optional :: x
        integer(i8), intent(in) :: default
        integer(i8) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_i8

    pure elemental function optval_c4(x, default) result(y)
        complex(c4), intent(in), optional :: x
        complex(c4), intent(in) :: default
        complex(c4) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_c4

    pure elemental function optval_c8(x, default) result(y)
        complex(c8), intent(in), optional :: x
        complex(c8), intent(in) :: default
        complex(c8) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_c8

    pure elemental function optval_bool(x, default) result(y)
        logical, intent(in), optional :: x
        logical, intent(in) :: default
        logical :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_bool

    ! Cannot be made elemental
    pure function optval_char(x, default) result(y)
        character(len=*), intent(in), optional :: x
        character(len=*), intent(in) :: default
        character(len=:), allocatable :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if

    end function optval_char

end module machina_optval
