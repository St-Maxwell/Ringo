module ringo_driver
    use ringo_config
    use ringo_env
    use ringo_mole_detail
    use ringo_dead
    use machina_vla
    use machina_error
    implicit none
    private
    public :: driver_t, construct_jobs_driver

    !> jobtypes
    integer, parameter :: scf_job = 1
    integer, parameter :: post_scf_job = 2
    integer, parameter :: analysis_job = 3

    type :: driver_t
        type(vla_int) :: jobs
    contains
        procedure :: run_jobs
    end type

contains

    subroutine construct_jobs_driver(this, config)
        type(driver_t), intent(out) :: this
        type(config_t), intent(in) :: config

        !> anyway, the first job is SCF
        call this%jobs%push_back(scf_job)

        select case (config%jobtype)
        case ("sp")
            select case (config%method)
            case ("hf")
            case default
                call ringo_crash("The "//config%method//" method is currently not implemented")
            end select
            call this%jobs%push_back(analysis_job)
        case default
            call ringo_crash("The "//config%jobtype//" job is currently not implemented")
        end select

    end subroutine construct_jobs_driver

    subroutine run_jobs(this, config, mole, error)
        class(driver_t), intent(in) :: this
        type(config_t), intent(in) :: config
        type(mole_t), intent(inout) :: mole
        type(error_t), intent(out) :: error

        !> build mole
        call mole%set_basis_set(config%basis, error)
        if (.has. error) return
        call mole%build(error)
        if (.has. error) return
        call mole%print_geometry(std_out)

        jobs: block
            type(vla_int_const_iterator) :: it
            it = this%jobs%const_iterator()
            do while (it%has_next())
                !> perform each jobs
                write(std_out,"('run job ',g0)") it%get_next()
            end do
        end block jobs

    end subroutine run_jobs

end module ringo_driver
