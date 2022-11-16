module machina_error
    implicit none
    private
    public :: error_t, raise_error, operator(.has.)

    !> handles run time errors
    type :: error_t
        !> error message
        character(len=:), allocatable :: msg
        !> error code (optional)
        !> user specified
        integer, allocatable :: code
    end type

    interface operator(.has.)
        module procedure :: has_error
    end interface

contains

    !> raise an error
    subroutine raise_error(error, msg, code)
        type(error_t), intent(out) :: error
        character(len=*), intent(in) :: msg
        integer, intent(in), optional :: code

        error%msg = msg
        if (present(code)) error%code = code

    end subroutine raise_error

    !> check if an error is recorded
    pure function has_error(error)
        type(error_t), intent(in) :: error
        logical :: has_error

        has_error = allocated(error%msg)

    end function has_error

end module machina_error
