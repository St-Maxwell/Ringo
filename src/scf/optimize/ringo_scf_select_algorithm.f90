module ringo_scf_select_algorithm
    use machina_basic, only: f8
    use ringo_scf_algorithm
    use ringo_scf_roothaan
    use ringo_scf
    use ringo_scf_params
    use machina_assert
    implicit none
    private
    public :: select_algorithm

contains

    subroutine select_algorithm(algo, scf, params)
        class(scf_optimizer_t), allocatable, intent(out) :: algo
        class(scf_t), intent(in), target :: scf
        type(scf_params_t), intent(in) :: params

        select case (params%algo)
        case (scf_algorithm%roothaan)
            allocate (roothaan_t :: algo)
            algo%problem => scf
            algo%tol = 10._f8**(-params%tol)
        case (scf_algorithm%diis)
            call assert(.false., "DIIS not implemented yet")
        end select

    end subroutine select_algorithm

end module ringo_scf_select_algorithm
