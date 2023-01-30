module ringo_scf_roothaan
    use machina_basic, only: f8
    use ringo_scf_algorithm
    use ringo_scf_objective_function
    implicit none
    private
    public :: roothaan_t

    type, extends(scf_optimizer_t) :: roothaan_t
        logical :: first_step = .true.
    contains
        procedure :: next_step
        procedure :: name
    end type

contains

    function next_step(this) result(r)
        class(roothaan_t), intent(inout) :: this
        logical :: r
        !> locals
        real(kind=f8), dimension(:), allocatable :: res, vec

        !> get residual
        call this%problem%residual(res)

        this%error = norm2(res)/sqrt(real(size(res), f8))

        if (this%error < this%tol .and. (.not. this%first_step)) then
            r = .true.
            return
        end if

        call this%problem%get_vector(vec)
        call this%problem%put_vector(vec)

        this%first_step = .false.
        r = .false.

    end function next_step

    function name(this) result(s)
        class(roothaan_t), intent(in) :: this
        character(len=:), allocatable :: s

        s = "Roothaan diagonalization"

    end function name

end module ringo_scf_roothaan
