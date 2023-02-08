module ringo_scf_algorithm
    use machina_basic, only: f8
    use ringo_scf_objective_function
    implicit none
    private
    public :: scf_optimizer_t

    type, abstract :: scf_optimizer_t
        class(objective_function_t), pointer :: problem => null()
        real(kind=f8) :: error
        real(kind=f8) :: tol
    contains
        procedure(next_step), deferred :: next_step
        procedure(name), deferred :: name
        procedure(error_description), deferred :: error_description
    end type

    abstract interface
        !> do next step, return convergence
        function next_step(this) result(r)
            import :: scf_optimizer_t
            class(scf_optimizer_t), intent(inout) :: this
            logical :: r
        end function next_step

        function name(this) result(s)
            import :: scf_optimizer_t
            class(scf_optimizer_t), intent(in) :: this
            character(len=:), allocatable :: s
        end function name

        function error_description(this) result(s)
            import :: scf_optimizer_t
            class(scf_optimizer_t), intent(in) :: this
            character(len=:), allocatable :: s
        end function error_description
    end interface

end module ringo_scf_algorithm
