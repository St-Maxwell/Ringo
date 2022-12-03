
module machina_vla_char
    use machina_value_base
    implicit none
    private
    public :: vla_char, vla_char_iterator, vla_char_const_iterator, size
    public :: cast_to_vla_char

    integer, parameter :: initial_size = 16

    type :: vla_node
        character(len=:), allocatable :: val
    end type

    type, extends(machina_value) :: vla_char
        private
        integer :: sz = 0
        type(vla_node), dimension(:), allocatable :: lst
    contains
        procedure :: push_back_v, push_back_arr
        generic :: push_back => push_back_v, push_back_arr
        procedure :: pop
        procedure :: shift
        procedure :: at
        procedure :: iterator, const_iterator
        procedure :: destroy
    end type

    type :: vla_char_iterator
        private
        type(vla_node), dimension(:), pointer :: ptr => null()
        integer :: cur = 1
        logical :: reverse = .false.
    contains
        procedure :: has_next
        procedure :: get_next
    end type

    type :: vla_char_const_iterator
        private
        type(vla_node), dimension(:), pointer :: ptr => null()
        integer :: cur = 1
        logical :: reverse = .false.
    contains
        procedure :: has_next => has_next_const
        procedure :: get_next => get_next_const
    end type

    interface size
        module procedure :: vla_size
    end interface

contains

    subroutine push_back_v(this, v)
        class(vla_char), intent(inout) :: this
        character(len=*), intent(in) :: v
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        this%lst(this%sz)%val = v

    end subroutine push_back_v

    subroutine push_back_arr(this, arr)
        class(vla_char), intent(inout) :: this
        character(len=*), dimension(:), intent(in) :: arr
        integer :: sz, i

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz + size(arr) >= sz) then
            call resize(this%lst, sz + sz/2 + size(arr))
        end if

        do i = 1, size(arr)
            this%lst(i + this%sz)%val = trim(arr(i))
        end do
        this%sz = this%sz + size(arr)

    end subroutine push_back_arr

    subroutine pop(this, v)
        class(vla_char), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: v

        call move_alloc(this%lst(this%sz)%val, v)
        this%sz = this%sz - 1

    end subroutine pop

    subroutine shift(this, v)
        class(vla_char), intent(inout) :: this
        character(len=:), allocatable, intent(out) :: v
        integer :: i

        call move_alloc(this%lst(1)%val, v)
        do i = 1, this%sz - 1
            call move_alloc(this%lst(i + 1)%val, this%lst(i)%val)
        end do
        this%sz = this%sz - 1

    end subroutine shift

    pure function at(this, idx) result(v)
        class(vla_char), intent(in) :: this
        integer, intent(in) :: idx
        character(len=:), allocatable :: v

        v = this%lst(idx)%val

    end function at

    pure function vla_size(this) result(sz)
        type(vla_char), intent(in) :: this
        integer :: sz

        sz = this%sz

    end function vla_size

    function iterator(this, reverse) result(it)
        class(vla_char), intent(in), target :: this
        logical, intent(in), optional :: reverse
        type(vla_char_iterator) :: it

        if (present(reverse)) it%reverse = reverse
        it%ptr => this%lst(1:this%sz)
        if (it%reverse) it%cur = this%sz

    end function iterator

    pure function has_next(this) result(r)
        class(vla_char_iterator), intent(in) :: this
        logical :: r

        if (this%reverse) then
            r = this%cur >= 1
        else
            r = this%cur <= size(this%ptr)
        end if

    end function has_next

    function get_next(this) result(ptr)
        class(vla_char_iterator), intent(inout), target :: this
        character(len=:), pointer :: ptr

        ptr => this%ptr(this%cur)%val
        if (this%reverse) then
            this%cur = this%cur - 1
        else
            this%cur = this%cur + 1
        end if

    end function get_next

    function const_iterator(this, reverse) result(it)
        class(vla_char), intent(in), target :: this
        logical, intent(in), optional :: reverse
        type(vla_char_const_iterator) :: it

        if (present(reverse)) it%reverse = reverse
        it%ptr => this%lst(1:this%sz)
        if (it%reverse) it%cur = this%sz

    end function const_iterator

    pure function has_next_const(this) result(r)
        class(vla_char_const_iterator), intent(in) :: this
        logical :: r

        if (this%reverse) then
            r = this%cur >= 1
        else
            r = this%cur <= size(this%ptr)
        end if

    end function has_next_const

    function get_next_const(this) result(v)
        class(vla_char_const_iterator), intent(inout) :: this
        character(len=:), allocatable :: v

        v = this%ptr(this%cur)%val
        if (this%reverse) then
            this%cur = this%cur - 1
        else
            this%cur = this%cur + 1
        end if

    end function get_next_const

    subroutine destroy(this)
        class(vla_char), intent(inout) :: this

        if (allocated(this%lst)) deallocate (this%lst)
        this%sz = 0

    end subroutine destroy

    subroutine resize(list, new_size)
        type(vla_node), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        type(vla_node), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            call move_alloc(from=list, to=tmp)
            allocate (list(new_size))
            do i = 1, min(new_size, size(tmp))
                list(i)%val = tmp(i)%val
            end do

            do i = new_size + 1, size(tmp)
                if (allocated(tmp(i)%val)) then
                    deallocate (tmp(i)%val)
                end if
            end do

            deallocate (tmp)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

    function cast_to_vla_char(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        type(vla_char), pointer :: v

        v => null()
        select type (ptr)
        type is (vla_char)
            v => ptr
        end select

    end function cast_to_vla_char

end module machina_vla_char
