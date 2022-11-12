module machina_vra_char
    implicit none
    private
    public :: vra_char, size

    integer, parameter :: initial_size = 16

    type :: vra_node
        character(len=:), allocatable :: val
    end type

    type :: vra_char
        private
        type(vra_node), dimension(:), allocatable :: lst
        integer :: sz = 0
    contains
        procedure :: push_back
        procedure :: pop
        procedure :: at
    end type

    interface size
        module procedure :: vra_size
    end interface

contains

    subroutine push_back(this, char)
        class(vra_char), intent(inout) :: this
        character(len=*), intent(in) :: char
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        this%lst(this%sz)%val = char

    end subroutine push_back

    subroutine pop(this, char)
        class(vra_char), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: char

        if (this%sz > 0) then
            call move_alloc(this%lst(this%sz)%val, char)
            this%sz = this%sz - 1
        end if

    end subroutine pop

    function at(this, idx) result(char)
        class(vra_char), intent(in) :: this
        integer, intent(in) :: idx
        character(len=:), allocatable :: char

        if (idx <= this%sz .and. idx > 0) then
            char = this%lst(idx)%val
        end if

    end function at

    pure function vra_size(this) result(sz)
        class(vra_char), intent(in) :: this
        integer :: sz

        sz = this%sz

    end function vra_size

    subroutine resize(list, new_size)
        type(vra_node), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        type(vra_node), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            ! move list to tmp
            allocate (tmp(new_size))
            do i = 1, size(list)
                call move_alloc(from=list(i)%val, to=tmp(i)%val)
            end do
            deallocate (list)

            ! reallocate
            call move_alloc(from=tmp, to=list)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

end module machina_vra_char
