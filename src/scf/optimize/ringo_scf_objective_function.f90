module ringo_scf_objective_function
    use machina_basic, only: f8
    implicit none
    private
    public :: objective_function_t

    type, abstract :: objective_function_t
    contains
        procedure(get_value), deferred :: get_value
        !procedure(gradient), deferred :: gradient
        procedure(residual), deferred :: residual
        procedure(get_vector), deferred :: get_vector
        procedure(put_vector), deferred :: put_vector
    end type

    abstract interface
        function get_value(this) result(v)
            import :: objective_function_t, f8
            class(objective_function_t), intent(inout) :: this
            real(kind=f8) :: v
        end function get_value

        !subroutine gradient(this, grad)
        !    import :: objective_function_t, f8
        !    class(objective_function_t), intent(inout) :: this
        !    real(kind=f8), dimension(:), allocatable, intent(out) :: grad
        !end subroutine gradient

        subroutine residual(this, res)
            import :: objective_function_t, f8
            class(objective_function_t), intent(inout) :: this
            real(kind=f8), dimension(:), allocatable, intent(out) :: res
        end subroutine residual

        subroutine get_vector(this, vec)
            import :: objective_function_t, f8
            class(objective_function_t), intent(inout) :: this
            real(kind=f8), dimension(:), allocatable, intent(out) :: vec
        end subroutine get_vector

        subroutine put_vector(this, vec)
            import :: objective_function_t, f8
            class(objective_function_t), intent(inout) :: this
            real(kind=f8), dimension(:), intent(in) :: vec
        end subroutine put_vector
    end interface

end module ringo_scf_objective_function
