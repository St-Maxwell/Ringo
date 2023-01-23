module ringo_log_utils
    use machina_basic, only: f8
    implicit none
    private
    public :: print_header, print_bar, print_mat

    interface print_mat
        module procedure :: print_mat_real
    end interface

contains

    subroutine print_header(header, unit)
        character(len=*), intent(in) :: header
        integer, intent(in) :: unit

        write (unit, "(A)") " ==> "//trim(header)//" <=="

    end subroutine print_header

    subroutine print_bar(unit)
        integer, intent(in) :: unit

        write (unit, "(A)") repeat('-', 80)

    end subroutine print_bar

    subroutine print_mat_real(mat, header, unit)
        real(kind=f8), dimension(:, :), intent(in) :: mat
        character(len=*), intent(in) :: header
        integer, intent(in) :: unit
        integer :: nchunk, nremain
        integer :: ichunk, icol, irow, i

        nchunk = size(mat, dim=2)/5
        nremain = mod(size(mat, dim=2), 5)

        write (unit, "(A)") " ## "//trim(header)//" ##"

        icol = 1
        do ichunk = 1, nchunk
            write (unit, "(I17,4I20)") [(i, i=icol, icol + 4)]
            do irow = 1, size(mat, dim=1)
                write (unit, "(I4,5F20.12)") irow, mat(irow, icol:icol + 4)
            end do
            write (unit, "(A)")
            icol = icol + 5
        end do

        if (nremain == 0) return

        write (unit, "(I17,*(I20))") [(i, i=icol, icol + nremain - 1)]
        do irow = 1, size(mat, dim=1)
            write (unit, "(I4,*(F20.12))") irow, mat(irow, icol:icol + nremain - 1)
        end do
        write (unit, "(A)")

    end subroutine print_mat_real

end module ringo_log_utils
