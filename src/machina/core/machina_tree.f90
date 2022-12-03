module machina_tree
    use machina_value_base
    implicit none
    private
    public :: machina_tree_node
    public :: insert_node, delete_node, find_node

    !> AVL tree implementation
    type :: machina_tree_node
        character(len=:), allocatable :: key
        class(machina_value), allocatable :: val
        type(machina_tree_node), allocatable :: left
        type(machina_tree_node), allocatable :: right
        integer :: height = 0
    end type

contains

    pure recursive function tree_height(node) result(h)
        type(machina_tree_node), allocatable, intent(in) :: node
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
        type(machina_tree_node), allocatable, intent(in) :: node
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
        type(machina_tree_node), allocatable, intent(inout) :: root
        type(machina_tree_node), allocatable :: left

        call move_alloc(root%left, left)
        call move_alloc(left%right, root%left)
        call move_alloc(root, left%right)

        left%right%height = tree_height(left%right)
        left%height = tree_height(left)

        call move_alloc(left, root)

    end subroutine rotate_right

    subroutine rotate_left(root)
        type(machina_tree_node), allocatable, intent(inout) :: root
        type(machina_tree_node), allocatable :: right

        call move_alloc(root%right, right)
        call move_alloc(right%left, root%right)
        call move_alloc(root, right%left)

        right%left%height = tree_height(right%left)
        right%height = tree_height(right)

        call move_alloc(right, root)

    end subroutine rotate_left

    subroutine rebalance(root)
        type(machina_tree_node), allocatable, intent(inout) :: root
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
        type(machina_tree_node), allocatable, intent(inout) :: root
        character(len=*), intent(in) :: key
        class(machina_value), allocatable, intent(inout) :: val
        type(machina_tree_node), allocatable :: new_node

        if (allocated(root)) then
            if (key < root%key) then
                call insert_node(root%left, key, val)
            else if (key > root%key) then
                call insert_node(root%right, key, val)
            else
                deallocate (root%val)
                call move_alloc(val, root%val)
            end if
            root%height = tree_height(root)
            call rebalance(root)
        else
            allocate (new_node)
            allocate (new_node%key, source=key)
            call move_alloc(val, new_node%val)
            call move_alloc(new_node, root)
        end if

    end subroutine insert_node

    recursive subroutine delete_node(root, key)
        type(machina_tree_node), allocatable, intent(inout) :: root
        character(len=*), intent(in) :: key
        type(machina_tree_node), allocatable :: tmp

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
            type(machina_tree_node), allocatable, intent(inout) :: to
            type(machina_tree_node), allocatable, intent(inout) :: from

            if (allocated(from%left)) then
                call move_right_min_node(to, from%left)
            else
                to%key = from%key
                call move_alloc(from%val, to%val)
            end if

        end subroutine move_right_min_node

    end subroutine delete_node

    recursive function find_node(root, key) result(ptr)
        type(machina_tree_node), allocatable, intent(in), target :: root
        character(len=*), intent(in) :: key
        class(machina_value), pointer :: ptr

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

end module machina_tree
