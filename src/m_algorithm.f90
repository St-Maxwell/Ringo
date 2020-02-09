module mod_algorithm
    use mod_const
    implicit none

    interface RMS
        module procedure :: RMS_mat
    end interface
    
contains
    
    subroutine solve_eigen(mat, eigen_value, eigen_vec)
        real(kind=r8), dimension(:,:), intent(in) :: mat
        real(kind=r8), dimension(:), intent(inout) :: eigen_value
        real(kind=r8), dimension(:,:), intent(inout) :: eigen_vec
        real(kind=r8), dimension(:), allocatable :: work_arr, iwork_arr
        integer :: work_size, liwork_size
        integer :: isize, istat
    
        isize = size(mat, dim=1)
        eigen_vec = mat
    
        work_size = 2 * isize**2 + 6 * isize + 1
        liwork_size = 5 * isize + 3
        allocate(work_arr(work_size), stat=istat)
        allocate(iwork_arr(liwork_size), stat=istat)
    
        call dsyevd('V', 'U', isize, eigen_vec, isize, eigen_value, &
                    work_arr, work_size, iwork_arr, liwork_size, istat)
    
    end subroutine

    pure function diag(vec) result(mat)
        real(kind=r8), dimension(:), intent(in) :: vec
        real(kind=r8), dimension(:,:), allocatable :: mat
        !===================================================
        integer :: i, j

        allocate(mat(size(vec),size(vec)))
        
        do i = 1, size(vec)
            do j = 1, size(vec)
                if (i == j) then
                    mat(i,j) = vec(i)
                else
                    mat(i,j) = 0._r8
                end if
            end do
        end do

    end function diag

    function RMS_mat(mat1, mat2) result(RMSD)
        real(kind=r8), dimension(:,:), intent(in) :: mat1
        real(kind=r8), dimension(:,:), intent(in) :: mat2
        real(kind=r8) :: RMSD

        RMSD = sqrt( sum((mat1 - mat2)**2) ) / size(mat1, dim=1)

    end function RMS_mat

end module mod_algorithm