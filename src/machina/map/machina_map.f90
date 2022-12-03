module machina_map
    use machina_basic, only: i4, f8
    use machina_core
    use machina_vla
    use machina_error
    implicit none
    private
    public :: map_t, map_iterator

    type :: map_t
        private
        type(machina_tree_node), allocatable :: root
    contains
        procedure :: insert_int
        procedure :: insert_real
        procedure :: insert_cmplx
        procedure :: insert_bool
        procedure :: insert_char
        procedure :: insert_vla_int
        procedure :: insert_vla_real
        procedure :: insert_vla_cmplx
        procedure :: insert_vla_bool
        procedure :: insert_vla_char
        generic :: insert => insert_int, insert_real, insert_cmplx, insert_bool, insert_char, &
            insert_vla_int, insert_vla_real, insert_vla_cmplx, insert_vla_bool, insert_vla_char
        procedure :: get_int
        procedure :: get_real
        procedure :: get_cmplx
        procedure :: get_bool
        procedure :: get_char
        procedure :: get_vla_int
        procedure :: get_vla_real
        procedure :: get_vla_cmplx
        procedure :: get_vla_bool
        procedure :: get_vla_char
        procedure :: remove
        procedure :: has_key
        procedure :: iterator => construct_map_iterator
    end type

    !> auxiliary type for representing the stack of tree_node
    !> which is used for iteration of map
    type :: node_ptr
        type(machina_tree_node), pointer :: ptr => null()
    end type

    type :: map_iterator
        private
        type(node_ptr), dimension(:), allocatable :: stack
    contains
        procedure :: has_next => iterator_has_next
        procedure :: next_pair => iterator_next_pair
    end type

contains

    subroutine insert_int(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(in) :: val
        class(machina_value), allocatable :: tmpv

        call construct_scalar(tmpv, val)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_int

    subroutine insert_real(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        real(kind=f8), intent(in) :: val
        class(machina_value), allocatable :: tmpv

        call construct_scalar(tmpv, val)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_real

    subroutine insert_cmplx(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        complex(kind=f8), intent(in) :: val
        class(machina_value), allocatable :: tmpv

        call construct_scalar(tmpv, val)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_cmplx

    subroutine insert_bool(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        logical, intent(in) :: val
        class(machina_value), allocatable :: tmpv

        call construct_scalar(tmpv, val)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_bool

    subroutine insert_char(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: val
        class(machina_value), allocatable :: tmpv

        call construct_scalar(tmpv, val)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_char

    subroutine insert_vla_int(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(vla_int), allocatable, intent(inout) :: val
        class(machina_value), allocatable :: tmpv

        call move_alloc(val, tmpv)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_vla_int

    subroutine insert_vla_real(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(vla_real), allocatable, intent(inout) :: val
        class(machina_value), allocatable :: tmpv

        call move_alloc(val, tmpv)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_vla_real

    subroutine insert_vla_cmplx(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(vla_cmplx), allocatable, intent(inout) :: val
        class(machina_value), allocatable :: tmpv

        call move_alloc(val, tmpv)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_vla_cmplx

    subroutine insert_vla_bool(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(vla_bool), allocatable, intent(inout) :: val
        class(machina_value), allocatable :: tmpv

        call move_alloc(val, tmpv)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_vla_bool

    subroutine insert_vla_char(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(vla_char), allocatable, intent(inout) :: val
        class(machina_value), allocatable :: tmpv

        call move_alloc(val, tmpv)
        call insert_node(this%root, trim(adjustl(key)), tmpv)

    end subroutine insert_vla_char

    !> delete a key-value pair
    !> according the input key
    subroutine remove(this, key)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key

        call delete_node(this%root, trim(adjustl(key)))

    end subroutine remove

    !> check the existence of the inquired key-value pair
    function has_key(this, key) result(has)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        logical :: has

        has = associated(find_node(this%root, trim(adjustl(key))))

    end function has_key

    !> try to get an int32 value
    subroutine get_int(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(out) :: val
        type(error_t), intent(out) :: error
        integer(kind=i4), intent(in), optional :: default
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val = cast_to_int(ptr)
            return
        end if

        if (present(default)) then
            val = default
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_int

    !> try to get a real64 value
    subroutine get_real(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=f8), intent(out) :: val
        type(error_t), intent(out) :: error
        real(kind=f8), intent(in), optional :: default
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val = cast_to_real(ptr)
            return
        end if

        if (present(default)) then
            val = default
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_real

    !> try to get a real64 complex value
    subroutine get_cmplx(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        complex(kind=f8), intent(out) :: val
        type(error_t), intent(out) :: error
        complex(kind=f8), intent(in), optional :: default
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val = cast_to_cmplx(ptr)
            return
        end if

        if (present(default)) then
            val = default
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_cmplx

    !> try to get a logical value
    subroutine get_bool(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(out) :: val
        type(error_t), intent(out) :: error
        logical, intent(in), optional :: default
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val = cast_to_bool(ptr)
            return
        end if

        if (present(default)) then
            val = default
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_bool

    !> try to get a character value
    subroutine get_char(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=:), allocatable, intent(out) :: val
        type(error_t), intent(out) :: error
        character(len=*), intent(in), optional :: default
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val = cast_to_char(ptr)
            return
        end if

        if (present(default)) then
            val = default
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_char

    !> try to get a vla_int value
    subroutine get_vla_int(this, key, val, error)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(vla_int), pointer, intent(out) :: val
        type(error_t), intent(out) :: error
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val => cast_to_vla_int(ptr)
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_vla_int

    !> try to get a vla_real value
    subroutine get_vla_real(this, key, val, error)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(vla_real), pointer, intent(out) :: val
        type(error_t), intent(out) :: error
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val => cast_to_vla_real(ptr)
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_vla_real

    !> try to get a vla_cmplx value
    subroutine get_vla_cmplx(this, key, val, error)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(vla_cmplx), pointer, intent(out) :: val
        type(error_t), intent(out) :: error
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val => cast_to_vla_cmplx(ptr)
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_vla_cmplx

    !> try to get a vla_bool value
    subroutine get_vla_bool(this, key, val, error)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(vla_bool), pointer, intent(out) :: val
        type(error_t), intent(out) :: error
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val => cast_to_vla_bool(ptr)
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_vla_bool

    !> try to get a vla_char value
    subroutine get_vla_char(this, key, val, error)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        type(vla_char), pointer, intent(out) :: val
        type(error_t), intent(out) :: error
        class(machina_value), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            val => cast_to_vla_char(ptr)
            return
        end if

        call raise_error(error, not_found_error(trim(adjustl(key))))

    end subroutine get_vla_char

!> iterator method

    function construct_map_iterator(this) result(it)
        class(map_t), intent(in), target :: this
        type(map_iterator) :: it
        type(machina_tree_node), pointer :: p

        p => this%root
        do
            if (.not. associated(p)) exit
            call push_back(it%stack, p)
            p => p%left
        end do

    end function construct_map_iterator

    subroutine push_back(stack, ptr)
        type(node_ptr), dimension(:), allocatable, intent(inout) :: stack
        type(machina_tree_node), pointer, intent(in) :: ptr

        if (allocated(stack)) then
            stack = [stack, node_ptr(ptr)]
        else
            stack = [node_ptr(ptr)]
        end if

    end subroutine push_back

    function pop(stack) result(ptr)
        type(node_ptr), dimension(:), allocatable, intent(inout) :: stack
        type(machina_tree_node), pointer :: ptr
        type(node_ptr), dimension(:), allocatable :: tmp

        ptr => stack(size(stack))%ptr
        call move_alloc(stack, tmp)
        stack = tmp(:size(stack) - 1)

    end function pop

    pure function iterator_has_next(this) result(r)
        class(map_iterator), intent(in) :: this
        logical :: r

        r = size(this%stack) > 0

    end function iterator_has_next

    subroutine iterator_next_pair(this, key, value)
        class(map_iterator), intent(inout), target :: this
        character(len=:), allocatable, intent(out) :: key
        class(machina_value), pointer, intent(out) :: value
        type(machina_tree_node), pointer :: p, cur

        p => pop(this%stack)
        cur => p%right
        do
            if (.not. associated(cur)) exit
            call push_back(this%stack, cur)
            cur => cur%left
        end do

        key = p%key
        value => p%val

    end subroutine iterator_next_pair

end module machina_map
