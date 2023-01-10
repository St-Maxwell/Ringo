module ringo_config
    implicit none
    private
    public :: config_t, set_default_config

    type :: config_t
        character(len=:), allocatable :: jobtype
        character(len=:), allocatable :: method
        character(len=:), allocatable :: basis
        integer :: verbose
    end type

contains

    subroutine set_default_config(config)
        type(config_t), intent(out) :: config

        config%basis = "sto-3g"
        config%jobtype = "sp"
        config%method = "hf"
        config%verbose = 0

    end subroutine set_default_config

end module ringo_config
