module machina_error_detail
    use machina_error_kind
    use iso_fortran_env, only: error_unit
    implicit none

    !> error_t is the type that handles run time errors
    !>
    !> Examples:
    !>
    !> ```fortran
    !> block
    !>     type(error_t) :: error
    !>     call sub(..., error)
    !>     write (*, *) .has. error
    !>     select type (k => error%kind)
    !>     type is (value_error)
    !>         write (*, "(A)") "catch the error"
    !>         write (*, "(A)") error%msg
    !>     type is (open_file_error)
    !>         write (*, "(A)") "catch the error"
    !>         write (*, "(A)") error%msg
    !>     class default
    !>         write (*, "(A)") "will go here"
    !>     end select
    !>     call clear_error(error)
    !> end block
    !> ! if error is not properly dealt, the finalizer of error_t
    !> ! will cause an error stop
    !> ```
    !>
    type :: error_t
        class(error_kind_base), allocatable :: kind
        character(len=:), allocatable :: msg
    contains
        final :: raise_exception
    end type

    interface operator(.has.)
        module procedure :: has_error
    end interface

contains

    !> set the error status
    !> not that when entering this subroutine,
    !> the finalizer of error_t will be triggered
    !> so you have to clear the previous error
    subroutine set_error(error, kind, msg)
        type(error_t), intent(out) :: error
        class(error_kind_base), intent(in) :: kind
        character(len=*), intent(in) :: msg

        error%kind = kind
        error%msg = msg

    end subroutine set_error

    !> check if an error is recorded
    pure function has_error(error)
        type(error_t), intent(in) :: error
        logical :: has_error

        if (allocated(error%kind)) then
            select type (k => error%kind)
            class is (ok_base)
                has_error = .false.
            class default
                has_error = .true.
            end select
        else
            has_error = .false.
        end if

    end function has_error

    !> reset the status of error_t
    !> to prevent from the abort of the code
    subroutine clear_error(error)
        type(error_t), intent(inout) :: error

        if (allocated(error%kind)) deallocate (error%kind)
        if (allocated(error%msg)) deallocate (error%msg)

    end subroutine clear_error

    !> if an error is reported but not solved,
    !> the error information is printed and program panics
    subroutine raise_exception(error)
        type(error_t), intent(inout) :: error

        if (.has.error) then
            write (error_unit, "('[UNSOLVED FATAL ERROR] ',A)") error%msg
            error stop
        end if

    end subroutine raise_exception

end module machina_error_detail
