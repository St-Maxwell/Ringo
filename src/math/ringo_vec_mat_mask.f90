module ringo_arr_mat_mask
    use machina_basic, only: f8
    use machina_assert
    implicit none
    private
    public :: array_view, mat_view

contains

    function array_view(mat) result(ptr)
        real(kind=f8), dimension(:, :), contiguous, intent(in), target :: mat
        real(kind=f8), dimension(:), pointer :: ptr

        ptr(1:size(mat)) => mat(:, :)

    end function array_view

    function mat_view(arr, m, n) result(ptr)
        real(kind=f8), dimension(:), contiguous, intent(in), target :: arr
        integer, intent(in) :: m, n
        real(kind=f8), dimension(:, :), pointer :: ptr

        call assert(m*n == size(arr), "Size not matched")

        ptr(1:m, 1:n) => arr(:)

    end function mat_view

end module ringo_arr_mat_mask
