module ringo_cint_int1e
    use machina_basic, only: i4, f8
    use machina_assert
    use ringo_cint_const
    use ringo_cint_interface
    use ringo_cint_utils
    implicit none
    private
    public :: calc_ovlp_int, calc_kin_int, calc_nuc_int

contains

    !> calculate overlap integrals of spherical basis functions
    !> <mu|nu>
    subroutine calc_ovlp_int(mat, atm, bas, env)
        real(kind=f8), dimension(:, :), intent(inout) :: mat
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer :: natm, nshls
        integer :: ishl, jshl, di, dj, info, x, y

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        allocate (buf1e(buffer_size(bas)))

        do jshl = 1, nshls
            shls(2) = jshl - 1
            dj = nbas_per_shell(bas, jshl)
            y = shell_slice(bas, jshl)

            do ishl = jshl, nshls
                shls(1) = ishl - 1
                di = nbas_per_shell(bas, ishl)
                x = shell_slice(bas, ishl)

                info = cint1e_ovlp_sph(buf1e, shls, atm, natm, bas, nshls, env)

                call array_to_mat(buf1e, mat(x:x + di - 1, y:y + dj - 1), di*dj)

                if (ishl /= jshl) call array_to_mat_trans(buf1e, mat(y:y + dj - 1, x:x + di - 1), di*dj)
            end do
        end do

    end subroutine calc_ovlp_int

    !> calculate kinetic integrals of spherical basis functions
    !> <mu|T|mu>
    subroutine calc_kin_int(mat, atm, bas, env)
        real(kind=f8), dimension(:, :), intent(inout) :: mat
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer :: natm, nshls
        integer :: ishl, jshl, di, dj, info, x, y

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        allocate (buf1e(buffer_size(bas)))

        do jshl = 1, nshls
            shls(2) = jshl - 1
            dj = nbas_per_shell(bas, jshl)
            y = shell_slice(bas, jshl)

            do ishl = jshl, nshls
                shls(1) = ishl - 1
                di = nbas_per_shell(bas, ishl)
                x = shell_slice(bas, ishl)

                info = cint1e_kin_sph(buf1e, shls, atm, natm, bas, nshls, env)

                call array_to_mat(buf1e, mat(x:x + di - 1, y:y + dj - 1), di*dj)

                if (ishl /= jshl) call array_to_mat_trans(buf1e, mat(y:y + dj - 1, x:x + di - 1), di*dj)
            end do
        end do

    end subroutine calc_kin_int

    !> calculate nuclear attraction integrals of spherical basis functions
    !> <mu|Vnuc|nu>
    subroutine calc_nuc_int(mat, atm, bas, env)
        real(kind=f8), dimension(:, :), intent(inout) :: mat
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable :: buf1e
        integer, dimension(2) :: shls
        integer :: natm, nshls
        integer :: ishl, jshl, di, dj, info, x, y

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        allocate (buf1e(buffer_size(bas)))

        do jshl = 1, nshls
            shls(2) = jshl - 1
            dj = nbas_per_shell(bas, jshl)
            y = shell_slice(bas, jshl)

            do ishl = jshl, nshls
                shls(1) = ishl - 1
                di = nbas_per_shell(bas, ishl)
                x = shell_slice(bas, ishl)

                info = cint1e_nuc_sph(buf1e, shls, atm, natm, bas, nshls, env)

                call array_to_mat(buf1e, mat(x:x + di - 1, y:y + dj - 1), di*dj)

                if (ishl /= jshl) call array_to_mat_trans(buf1e, mat(y:y + dj - 1, x:x + di - 1), di*dj)
            end do
        end do

    end subroutine calc_nuc_int

    !> Fortran column-major order
    subroutine array_to_mat(arr, mat, len)
        real(kind=f8), dimension(:), intent(in) :: arr
        real(kind=f8), dimension(:, :), intent(inout) :: mat
        integer, intent(in) :: len
        integer :: i, slot

        call assert(size(mat) == len, "The sizes are not consistent")

        slot = size(mat, dim=1)
        do i = 1, size(mat, dim=2)
            mat(:, i) = arr((i - 1)*slot + 1:i*slot)
        end do

    end subroutine array_to_mat

    !> C row-major order, the transpose of Fortran column-major order
    subroutine array_to_mat_trans(arr, mat, len)
        real(kind=f8), dimension(:), intent(in) :: arr
        real(kind=f8), dimension(:, :), intent(inout) :: mat
        integer, intent(in) :: len
        integer :: i, slot

        call assert(size(mat) == len, "The sizes are not consistent")

        slot = size(mat, dim=2)
        do i = 1, size(mat, dim=1)
            mat(i, :) = arr((i - 1)*slot + 1:i*slot)
        end do

    end subroutine array_to_mat_trans

    pure function buffer_size(bas) result(n)
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer :: n
        integer :: maxl

        maxl = maxval(bas(ANG_OF :: BAS_SLOTS), dim=1)
        n = (2*maxl + 1)*(2*maxl + 1)

    end function buffer_size

end module ringo_cint_int1e
