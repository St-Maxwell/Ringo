module machina_vla_int
    use machina_basic, only: i4
    use machina_value_base
    implicit none
    private
    public :: vla_int, vla_int_iterator, vla_int_const_iterator
    public :: size, copy_to_array
    public :: cast_to_vla_int

    integer, parameter :: initial_size = 16

    type, extends(machina_value) :: vla_int
        private
        integer :: sz = 0
        integer(kind=i4), dimension(:), allocatable :: lst
    contains
        procedure :: push_back_v, push_back_arr, push_back_vla
        generic :: push_back => push_back_v, push_back_arr, push_back_vla
        procedure :: pop
        procedure :: shift
        procedure :: at
        procedure :: ptr_at
        procedure :: iterator, const_iterator
        procedure :: destroy
    end type

    type :: vla_int_iterator
        private
        integer(kind=i4), dimension(:), pointer :: ptr => null()
        integer :: cur = 1
        logical :: reverse = .false.
    contains
        procedure :: has_next
        procedure :: get_next
    end type

    type :: vla_int_const_iterator
        private
        integer(kind=i4), dimension(:), pointer :: ptr => null()
        integer :: cur = 1
        logical :: reverse = .false.
    contains
        procedure :: has_next => has_next_const
        procedure :: get_next => get_next_const
    end type

    interface size
        module procedure :: vla_size
    end interface

    interface copy_to_array
        module procedure :: copy_to_array_int
    end interface

contains

    subroutine push_back_v(this, v)
        class(vla_int), intent(inout) :: this
        integer(kind=i4), intent(in) :: v
        integer :: sz

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz >= sz) then
            call resize(this%lst, sz + sz/2 + 1)
        end if

        this%sz = this%sz + 1
        this%lst(this%sz) = v

    end subroutine push_back_v

    subroutine push_back_arr(this, arr)
        class(vla_int), intent(inout) :: this
        integer(kind=i4), dimension(:), intent(in) :: arr
        integer :: sz, i

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz + size(arr) >= sz) then
            call resize(this%lst, sz + sz/2 + size(arr))
        end if

        do i = 1, size(arr)
            this%lst(i + this%sz) = arr(i)
        end do
        this%sz = this%sz + size(arr)

    end subroutine push_back_arr

    subroutine push_back_vla(this, vla)
        class(vla_int), intent(inout) :: this
        type(vla_int), intent(in) :: vla
        integer :: sz, i

        if (.not. allocated(this%lst)) then
            call resize(this%lst, initial_size)
        end if

        sz = size(this%lst)
        if (this%sz + size(vla) >= sz) then
            call resize(this%lst, sz + sz/2 + size(vla))
        end if

        do i = 1, size(vla)
            this%lst(i + this%sz) = vla%ptr_at(i)
        end do
        this%sz = this%sz + size(vla)

    end subroutine push_back_vla

    subroutine pop(this, v)
        class(vla_int), intent(inout) :: this
        integer(kind=i4), intent(out) :: v

        v = this%lst(this%sz)
        this%sz = this%sz - 1

    end subroutine pop

    subroutine shift(this, v)
        class(vla_int), intent(inout) :: this
        integer(kind=i4), intent(out) :: v
        integer :: i

        v = this%lst(1)
        do i = 1, this%sz - 1
            this%lst(i) = this%lst(i + 1)
        end do
        this%sz = this%sz - 1

    end subroutine shift

    pure function at(this, idx) result(v)
        class(vla_int), intent(in) :: this
        integer, intent(in) :: idx
        integer(kind=i4) :: v

        v = this%lst(idx)

    end function at

    function ptr_at(this, idx) result(v)
        class(vla_int), intent(in), target :: this
        integer, intent(in) :: idx
        integer(kind=i4), pointer :: v

        v => this%lst(idx)

    end function ptr_at

    pure function vla_size(this) result(sz)
        type(vla_int), intent(in) :: this
        integer :: sz

        sz = this%sz

    end function vla_size

    subroutine copy_to_array_int(this, array)
        type(vla_int), intent(in) :: this
        integer(kind=i4), dimension(:), allocatable, intent(out) :: array
        integer :: i

        allocate (array(size(this)))
        do i = 1, size(this)
            array(i) = this%ptr_at(i)
        end do

    end subroutine copy_to_array_int

    function iterator(this, reverse) result(it)
        class(vla_int), intent(in), target :: this
        logical, intent(in), optional :: reverse
        type(vla_int_iterator) :: it

        if (present(reverse)) it%reverse = reverse
        it%ptr => this%lst(1:this%sz)
        if (it%reverse) it%cur = this%sz

    end function iterator

    pure function has_next(this) result(r)
        class(vla_int_iterator), intent(in) :: this
        logical :: r

        if (this%reverse) then
            r = this%cur >= 1
        else
            r = this%cur <= size(this%ptr)
        end if

    end function has_next

    function get_next(this) result(ptr)
        class(vla_int_iterator), intent(inout), target :: this
        integer(kind=i4), pointer :: ptr

        ptr => this%ptr(this%cur)
        if (this%reverse) then
            this%cur = this%cur - 1
        else
            this%cur = this%cur + 1
        end if

    end function get_next

    function const_iterator(this, reverse) result(it)
        class(vla_int), intent(in), target :: this
        logical, intent(in), optional :: reverse
        type(vla_int_const_iterator) :: it

        if (present(reverse)) it%reverse = reverse
        it%ptr => this%lst(1:this%sz)
        if (it%reverse) it%cur = this%sz

    end function const_iterator

    pure function has_next_const(this) result(r)
        class(vla_int_const_iterator), intent(in) :: this
        logical :: r

        if (this%reverse) then
            r = this%cur >= 1
        else
            r = this%cur <= size(this%ptr)
        end if

    end function has_next_const

    function get_next_const(this) result(v)
        class(vla_int_const_iterator), intent(inout) :: this
        integer(kind=i4) :: v

        v = this%ptr(this%cur)
        if (this%reverse) then
            this%cur = this%cur - 1
        else
            this%cur = this%cur + 1
        end if

    end function get_next_const

    subroutine destroy(this)
        class(vla_int), intent(inout) :: this

        if (allocated(this%lst)) deallocate (this%lst)
        this%sz = 0

    end subroutine destroy

    subroutine resize(list, new_size)
        integer(kind=i4), dimension(:), allocatable, intent(inout) :: list
        integer, intent(in) :: new_size
        integer(kind=i4), dimension(:), allocatable :: tmp
        integer :: i

        if (allocated(list)) then
            call move_alloc(from=list, to=tmp)
            allocate (list(new_size))
            do i = 1, min(new_size, size(tmp))
                list(i) = tmp(i)
            end do
            deallocate (tmp)
        else
            allocate (list(new_size))
        end if

    end subroutine resize

    function cast_to_vla_int(ptr) result(v)
        class(machina_value), intent(in), target :: ptr
        type(vla_int), pointer :: v

        v => null()
        select type (ptr)
        type is (vla_int)
            v => ptr
        end select

    end function cast_to_vla_int

end module machina_vla_int
