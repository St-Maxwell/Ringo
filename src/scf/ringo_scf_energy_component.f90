module ringo_scf_energy_component
    use machina_basic, only: f8
    use machina_map
    use machina_core, only: machina_value, cast_to_real
    implicit none
    private
    public :: energy_component_t

    type :: energy_component_t
        private
        type(map_t) :: c
    contains
        procedure :: set
        procedure :: total_energy
        procedure :: print
    end type

contains

    subroutine set(this, comp, v)
        class(energy_component_t), intent(inout) :: this
        character(len=*), intent(in) :: comp
        real(kind=f8), intent(in) :: v

        call this%c%insert(comp, v)

    end subroutine set

    function total_energy(this) result(e)
        class(energy_component_t), intent(in) :: this
        real(kind=f8) :: e
        type(map_iterator) :: it
        character(len=:), allocatable :: key
        class(machina_value), pointer :: v

        e = 0
        it = this%c%iterator()
        do while (it%has_next())
            call it%next_pair(key, v)
            e = e + cast_to_real(v)
        end do

    end function total_energy

    subroutine print(this, unit)
        class(energy_component_t), intent(in) :: this
        integer, intent(in) :: unit
        type(map_iterator) :: it
        character(len=:), allocatable :: key
        class(machina_value), pointer :: v

        it = this%c%iterator()
        do while (it%has_next())
            call it%next_pair(key, v)
            write (unit, "(1X,A,' energy = ',F16.10,' a.u.')") key, cast_to_real(v)
        end do

    end subroutine print

end module ringo_scf_energy_component
