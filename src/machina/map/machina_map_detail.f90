module machina_map_detail
    use machina_basic
    use machina_error
    implicit none
    private
    public :: map_t, map_iterator

    !> node of a binary tree
    type :: tree_node
        character(len=:), allocatable :: key
        class(*), allocatable :: val
        type(tree_node), allocatable :: left
        type(tree_node), allocatable :: right
        integer :: height = 0
    end type

    !> a sorted map based on AVL tree
    type :: map_t
        private
        type(tree_node), allocatable :: root
    contains
        procedure :: insert
        procedure :: remove
        procedure :: has_key
        procedure :: ptr_at
        procedure :: get_i4
        procedure :: get_i8
        procedure :: get_f4
        procedure :: get_f8
        procedure :: get_f16
        procedure :: get_c8
        procedure :: get_c16
        procedure :: get_char
        procedure :: get_bool
        procedure :: iterator => construct_map_iterator
    end type

    !> auxiliary type for representing the stack of tree_node
    !> which is used for iteration of map
    type :: node_ptr
        type(tree_node), pointer :: ptr => null()
    end type

    type :: map_iterator
        private
        type(node_ptr), dimension(:), allocatable :: stack
    contains
        procedure :: has_next => iterator_has_next
        procedure :: next_pair => iterator_next_pair
    end type

contains

    !> append a new key-value pair
    !> if the key already exists in the map
    !> nothing will happen
    subroutine insert(this, key, val)
        class(map_t), intent(inout) :: this
        character(len=*), intent(in) :: key
        class(*), intent(in) :: val

        call insert_node(this%root, trim(adjustl(key)), val)

    end subroutine insert

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

    !> get the raw pointer of value
    !> via its key
    !> *should be used with caution*
    function ptr_at(this, key) result(ptr)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))

    end function ptr_at

    pure recursive function tree_height(node) result(h)
        type(tree_node), allocatable, intent(in) :: node
        integer :: h
        integer :: rh, lh

        if (.not. allocated(node)) then
            h = -1; return
        end if

        if (allocated(node%right)) then
            rh = node%right%height + 1
        else
            rh = 0
        end if

        if (allocated(node%left)) then
            lh = node%left%height + 1
        else
            lh = 0
        end if

        h = max(lh, rh)

    end function tree_height

    pure function tree_balance_factor(node) result(bf)
        type(tree_node), allocatable, intent(in) :: node
        integer :: bf
        integer :: rh, lh

        if (.not. allocated(node)) then
            bf = 0; return
        end if

        if (allocated(node%right)) then
            rh = node%right%height
        else
            rh = -1
        end if

        if (allocated(node%left)) then
            lh = node%left%height
        else
            lh = -1
        end if

        bf = lh - rh

    end function tree_balance_factor

    subroutine rotate_right(root)
        type(tree_node), allocatable, intent(inout) :: root
        type(tree_node), allocatable :: left

        call move_alloc(root%left, left)
        call move_alloc(left%right, root%left)
        call move_alloc(root, left%right)

        left%right%height = tree_height(left%right)
        left%height = tree_height(left)

        call move_alloc(left, root)

    end subroutine rotate_right

    subroutine rotate_left(root)
        type(tree_node), allocatable, intent(inout) :: root
        type(tree_node), allocatable :: right

        call move_alloc(root%right, right)
        call move_alloc(right%left, root%right)
        call move_alloc(root, right%left)

        right%left%height = tree_height(right%left)
        right%height = tree_height(right)

        call move_alloc(right, root)

    end subroutine rotate_left

    subroutine rebalance(root)
        type(tree_node), allocatable, intent(inout) :: root
        integer :: factor

        factor = tree_balance_factor(root)
        if (factor > 1 .and. tree_balance_factor(root%left) > 0) then
            call rotate_right(root)
        else if (factor > 1 .and. tree_balance_factor(root%left) <= 0) then
            call rotate_left(root%left)
            call rotate_right(root)
        else if (factor < -1 .and. tree_balance_factor(root%right) <= 0) then
            call rotate_left(root)
        else if (factor < -1 .and. tree_balance_factor(root%right) > 0) then
            call rotate_right(root%right)
            call rotate_left(root)
        end if

    end subroutine rebalance

    recursive subroutine insert_node(root, key, val)
        type(tree_node), allocatable, intent(inout) :: root
        character(len=*), intent(in) :: key
        class(*), intent(in) :: val
        type(tree_node), allocatable :: new_node

        if (allocated(root)) then
            ! do nothing when pass an existing key
            if (key < root%key) then
                call insert_node(root%left, key, val)
            else if (key > root%key) then
                call insert_node(root%right, key, val)
            end if
            root%height = tree_height(root)
            call rebalance(root)
        else
            allocate (new_node)
            allocate (new_node%key, source=key)
            allocate (new_node%val, source=val)
            call move_alloc(new_node, root)
        end if

    end subroutine insert_node

    recursive subroutine delete_node(root, key)
        type(tree_node), allocatable, intent(inout) :: root
        character(len=*), intent(in) :: key
        type(tree_node), allocatable :: tmp

        if (.not. allocated(root)) return

        if (key < root%key) then
            call delete_node(root%left, key)
        else if (key > root%key) then
            call delete_node(root%right, key)
        else
            if (allocated(root%left) .and. allocated(root%left)) then
                call move_right_min_node(root, root%right)
                call delete_node(root%right, root%key)
            else
                if (allocated(root%left) .or. allocated(root%left)) then
                    if (allocated(root%left)) then
                        call move_alloc(root%left, tmp)
                        call move_alloc(tmp, root)
                    end if
                    if (allocated(root%right)) then
                        call move_alloc(root%right, tmp)
                        call move_alloc(tmp, root)
                    end if
                else
                    deallocate (root)
                    return
                end if
            end if
        end if
        root%height = tree_height(root)
        call rebalance(root)

    contains

        recursive subroutine move_right_min_node(to, from)
            type(tree_node), allocatable, intent(inout) :: to
            type(tree_node), allocatable, intent(inout) :: from

            if (allocated(from%left)) then
                call move_right_min_node(to, from%left)
            else
                to%key = from%key
                call move_alloc(from%val, to%val)
            end if

        end subroutine move_right_min_node

    end subroutine delete_node

    recursive function find_node(root, key) result(ptr)
        type(tree_node), allocatable, intent(in), target :: root
        character(len=*), intent(in) :: key
        class(*), pointer :: ptr

        if (allocated(root)) then
            if (key < root%key) then
                ptr => find_node(root%left, key)
            else if (key > root%key) then
                ptr => find_node(root%right, key)
            else
                ptr => root%val
            end if
        else
            ptr => null()
        end if

    end function find_node

!===========================================================

    !> try to get an int32 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_i4(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(kind=i4), intent(inout) :: val
        type(error_t), intent(out) :: error
        integer(kind=i4), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (integer(kind=i4))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_i4

    !> try to get an int64 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_i8(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        integer(kind=i8), intent(inout) :: val
        type(error_t), intent(out) :: error
        integer(kind=i8), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (integer(kind=i8))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_i8

    !> try to get a real32 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_f4(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=f4), intent(inout) :: val
        type(error_t), intent(out) :: error
        real(kind=f4), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (real(kind=f4))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_f4

    !> try to get a real64 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_f8(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=f8), intent(inout) :: val
        type(error_t), intent(out) :: error
        real(kind=f8), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (real(kind=f8))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_f8

    !> try to get a real128 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_f16(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        real(kind=f16), intent(inout) :: val
        type(error_t), intent(out) :: error
        real(kind=f16), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (real(kind=f16))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_f16

    !> try to get a complex64 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_c8(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        complex(kind=c8), intent(inout) :: val
        type(error_t), intent(out) :: error
        complex(kind=c8), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (complex(kind=c8))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_c8

    !> try to get a complex128 value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_c16(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        complex(kind=c16), intent(inout) :: val
        type(error_t), intent(out) :: error
        complex(kind=c16), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (complex(kind=c16))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_c16

    !> try to get a character value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_char(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        character(len=*), intent(inout) :: val
        type(error_t), intent(out) :: error
        character(len=*), intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (character(len=*))
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_char

    !> try to get a logical value
    !> if the key does not exist,
    !> an out_of_range_error will be thrown
    subroutine get_bool(this, key, val, error, default)
        class(map_t), intent(in) :: this
        character(len=*), intent(in) :: key
        logical, intent(inout) :: val
        type(error_t), intent(out) :: error
        logical, intent(in), optional :: default
        class(*), pointer :: ptr

        ptr => find_node(this%root, trim(adjustl(key)))
        if (associated(ptr)) then
            select type (ptr)
            type is (logical)
                val = ptr
                return
            end select
        end if

        if (present(default)) then
            val = default
            return
        end if

        call set_error(error, out_of_range_error(), trim(adjustl(key))//" not found.")

    end subroutine get_bool

    function construct_map_iterator(this) result(it)
        class(map_t), intent(in), target :: this
        type(map_iterator) :: it
        type(tree_node), pointer :: p

        p => this%root
        do
            if (.not. associated(p)) exit
            call push_back(it%stack, p)
            p => p%left
        end do

    end function construct_map_iterator

!> iterator method

    subroutine push_back(stack, ptr)
        type(node_ptr), dimension(:), allocatable, intent(inout) :: stack
        type(tree_node), pointer, intent(in) :: ptr
        
        if (allocated(stack)) then
            stack = [stack, node_ptr(ptr)]
        else
            stack = [node_ptr(ptr)]
        end if

    end subroutine push_back

    function pop(stack) result(ptr)
        type(node_ptr), dimension(:), allocatable, intent(inout) :: stack
        type(tree_node), pointer:: ptr
        type(node_ptr), dimension(:), allocatable :: tmp

        ptr => stack(size(stack))%ptr
        call move_alloc(stack, tmp)
        stack = tmp(:size(stack)-1)

    end function pop

    pure function iterator_has_next(this) result(r)
        class(map_iterator), intent(in) :: this
        logical :: r

        r = size(this%stack) > 0

    end function iterator_has_next

    subroutine iterator_next_pair(this, key, value)
        class(map_iterator), intent(inout), target :: this
        character(len=:), allocatable :: key
        class(*), allocatable :: value
        type(tree_node), pointer :: p, cur

        p => pop(this%stack)
        cur => p%right
        do
            if (.not. associated(cur)) exit
            call push_back(this%stack, cur)
            cur => cur%left
        end do

        key = p%key
        value = p%val

    end subroutine iterator_next_pair

end module machina_map_detail
