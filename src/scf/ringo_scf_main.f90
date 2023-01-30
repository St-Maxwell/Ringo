module ringo_scf_main
    use ringo_scf
    use ringo_scf_rhf
    use ringo_scf_params
    use ringo_scf_select_algorithm
    use ringo_mole_detail
    use machina_error
    use machina_assert
    implicit none

contains

    subroutine construct_scf(scf, mole, error)
        class(scf_t), allocatable, intent(out) :: scf
        type(mole_t), intent(in) :: mole
        type(error_t), intent(out) :: error
        type(scf_params_t) :: params

        call set_scf_default_params(params)
        call parse_scf_sections(params, mole, error)
        if (.has.error) return

        select case (params%orb)
        case (scf_orbital%rhf)
            call construct_rhf(scf, mole, params)
        case default
            call assert(.false., "Orbital not implemented yet")
        end select

        call select_algorithm(scf%optimizer, scf, scf%params)

    end subroutine construct_scf

    subroutine run_scf(scf, mole, error)
        class(scf_t), allocatable, intent(out) :: scf
        type(mole_t), intent(in) :: mole
        type(error_t), intent(out) :: error

        call construct_scf(scf, mole, error)
        if (.has.error) return

        call scf%kernal(error)
        if (.has.error) return

    end subroutine run_scf

end module ringo_scf_main
