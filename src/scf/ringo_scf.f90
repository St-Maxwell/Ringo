module ringo_scf
    use machina_basic, only: f8
    use machina_map
    use machina_error
    use ringo_scf_objective_function
    use ringo_scf_algorithm
    use ringo_scf_energy_component
    use ringo_scf_params
    use ringo_env
    use ringo_log_utils
    implicit none
    private
    public :: scf_t

    type, extends(objective_function_t), abstract :: scf_t
        type(energy_component_t) :: cec !> constant energy components
        type(energy_component_t) :: ec !> energy components (depend on wf)
        logical :: converged = .false. !> scf convergence
        class(scf_optimizer_t), allocatable :: optimizer
        type(scf_params_t) :: params
    contains
        procedure :: kernal
        procedure :: total_energy
        procedure(energy), deferred :: energy
        procedure(obtain_guess), deferred :: obtain_guess
    end type

    abstract interface
        function energy(this) result(e)
            import :: f8, scf_t
            class(scf_t), intent(inout) :: this
            real(kind=f8) :: e
        end function energy

        subroutine obtain_guess(this, guess)
            import :: scf_t
            class(scf_t), intent(inout) :: this
            integer, intent(in) :: guess
        end subroutine obtain_guess
    end interface

contains

    function total_energy(this) result(e)
        class(scf_t), intent(inout) :: this
        real(kind=f8) :: e

        e = this%cec%total_energy() + this%energy()

    end function total_energy

    subroutine kernal(this, error)
        class(scf_t), intent(inout) :: this
        type(error_t), intent(out) :: error
        !> locals
        logical :: converged
        integer :: iter

        call show_params(this%params, std_out)

        call this%obtain_guess(this%params%guess)

        call print_header("SCF Iterations", std_out)
        write (std_out, "(/,10X,'Total Energy',7X,A,/)") this%optimizer%error_description()
        do iter = 1, this%params%max_iter
            if (this%optimizer%next_step()) exit
            write (std_out, "(I5,2F17.8)") iter, this%total_energy(), this%optimizer%error
        end do

        converged = iter <= this%params%max_iter

        if (converged) then
            write (std_out, "(I5,2F17.8,' <-- converged')") iter + 1, this%total_energy(), this%optimizer%error
            write (std_out, "(A)")

            call print_header("Converged Energies", std_out)
            write (std_out, "(A)")
            call this%cec%print(std_out)
            call this%ec%print(std_out)
            write (std_out, "(' Total energy = ',F16.10,' a.u.')") this%total_energy()
            write (std_out, "(A)")
        else
            write (std_out, "(/,'SCF failed to converge',/)")
        end if

    end subroutine kernal

end module ringo_scf
