module ringo_cint_int2e
    use iso_c_binding, only: c_ptr
    use machina_basic, only: i4, f8
    use ringo_cint_const
    use ringo_cint_interface
    use ringo_cint_utils
    implicit none
    private
    public :: calc_JK, calc_J, calc_K

contains

    !> build J (coulomb) and J (exchange) matrix simultaneously
    !> however, currently the results are wrong
    subroutine calc_JK(J, K, dm, atm, bas, env)
        real(kind=f8), dimension(:, :), intent(inout) :: J
        real(kind=f8), dimension(:, :), intent(inout) :: K
        real(kind=f8), dimension(:, :), intent(in) :: dm
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable, target :: buf2e
        real(kind=f8), dimension(:, :, :, :), pointer :: eri
        real(kind=f8) :: tpq, tpr, tqr, I
        type(c_ptr) :: opt
        integer, dimension(4) :: shls
        integer, dimension(:, :), allocatable :: pairs
        integer :: natm, nshls, info
        integer :: ipr, jpr, p, q, r, s
        integer :: dp, dq, dr, ds
        integer :: x, y, z, w, xx, yy, zz, ww, ix, iy, iz, iw

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        call shell_pairs(pairs, nshls)
        allocate (buf2e(buffer_size(bas)))

        J = 0; K = 0
        do ipr = 1, size(pairs, dim=2)
            p = pairs(1, ipr); q = pairs(2, ipr)
            shls(1) = p - 1; shls(2) = q - 1
            dp = nbas_per_shell(bas, p)
            dq = nbas_per_shell(bas, q)
            x = shell_slice(bas, p); xx = x + dp - 1
            y = shell_slice(bas, q); yy = y + dq - 1
            do jpr = ipr, size(pairs, dim=2)
                r = pairs(1, jpr); s = pairs(2, jpr)
                shls(3) = r - 1; shls(4) = s - 1
                dr = nbas_per_shell(bas, r)
                ds = nbas_per_shell(bas, s)
                z = shell_slice(bas, r); zz = z + dr - 1
                w = shell_slice(bas, s); ww = w + ds - 1

                ! unique shell quartets (pq|rs)
                info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)

                eri(1:dp, 1:dq, 1:dr, 1:ds) => buf2e(:dp*dq*dr*ds)

                do ix = 1, dp
                    do iy = 1, dq
                        tpq = 0
                        do iz = 1, dr
                            tpr = 0; tqr = 0
                            do iw = 1, ds
                                I = eri(ix, iy, iz, iw)
                                tpq = tpq + dm(z + iz - 1, w + iw - 1)*I
                                tpr = tpr + dm(y + iy - 1, w + iw - 1)*I
                                tqr = tqr + dm(x + ix - 1, w + iw - 1)*I
                                J(z + iz - 1, w + iw - 1) = J(z + iz - 1, w + iw - 1) + dm(x + ix - 1, y + iy - 1)*I
                                K(x + ix - 1, w + iw - 1) = K(x + ix - 1, w + iw - 1) + dm(y + iy - 1, z + iz - 1)*I
                                K(y + iy - 1, w + iw - 1) = K(y + iy - 1, w + iw - 1) + dm(x + ix - 1, z + iz - 1)*I
                            end do
                            K(x + ix - 1, z + iz - 1) = K(x + ix - 1, z + iz - 1) + tpr
                            K(y + iy - 1, z + iz - 1) = K(y + iy - 1, z + iz - 1) + tqr
                        end do
                        J(x + ix - 1, y + iy - 1) = J(x + ix - 1, y + iy - 1) + tpq
                    end do
                end do

            end do
        end do

        call CINTdel_optimizer(opt)

    end subroutine calc_JK

    subroutine calc_J(J, dm, atm, bas, env)
        use ringo_env
        use ringo_log_utils
        real(kind=f8), dimension(:, :), intent(inout) :: J
        real(kind=f8), dimension(:, :), intent(in) :: dm
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable, target :: buf2e
        real(kind=f8), dimension(:, :, :, :), pointer :: eri
        type(c_ptr) :: opt
        integer, dimension(4) :: shls
        integer, dimension(:, :), allocatable :: pairs
        real(kind=f8) :: fact
        integer :: natm, nshls, info
        integer :: ipr, jpr, p, q, r, s
        integer :: dp, dq, dr, ds
        integer :: x, y, z, w, xx, yy, zz, ww, ix, iy, iz, iw

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        call shell_pairs(pairs, nshls)
        allocate (buf2e(buffer_size(bas)))

        J = 0
        do ipr = 1, size(pairs, dim=2)
            p = pairs(1, ipr); q = pairs(2, ipr)
            shls(1) = p - 1; shls(2) = q - 1
            dp = nbas_per_shell(bas, p)
            dq = nbas_per_shell(bas, q)
            x = shell_slice(bas, p); xx = x + dp - 1
            y = shell_slice(bas, q); yy = y + dq - 1
            do jpr = 1, size(pairs, dim=2)
                r = pairs(1, jpr); s = pairs(2, jpr)
                shls(3) = r - 1; shls(4) = s - 1
                dr = nbas_per_shell(bas, r)
                ds = nbas_per_shell(bas, s)
                z = shell_slice(bas, r); zz = z + dr - 1
                w = shell_slice(bas, s); ww = w + ds - 1

                ! unique shell quartets (pq|rs)
                info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)

                eri(1:dp, 1:dq, 1:dr, 1:ds) => buf2e(:dp*dq*dr*ds)
                fact = merge(1._f8, 2._f8, r == s)

                !> J_MN = (MN|PQ)*D_PQ
                do iy = 1, dq
                    do ix = 1, dp
                        J(x + ix - 1, y + iy - 1) = J(x + ix - 1, y + iy - 1) &
                                                & + fact*sum(eri(ix, iy, :, :)*dm(z:zz, w:ww))
                    end do
                end do

            end do
            if (p /= q) then
                J(y:yy, x:xx) = transpose(J(x:xx, y:yy))
            end if
        end do

        call CINTdel_optimizer(opt)

    end subroutine calc_J

    subroutine calc_K(K, dm, atm, bas, env)
        real(kind=f8), dimension(:, :), intent(inout) :: K
        real(kind=f8), dimension(:, :), intent(in) :: dm
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:), allocatable, target :: buf2e
        real(kind=f8), dimension(:, :, :, :), pointer :: eri
        type(c_ptr) :: opt
        integer, dimension(4) :: shls
        integer, dimension(:, :), allocatable :: pairs
        integer :: natm, nshls, info
        integer :: ipr, jpr, p, q, r, s
        integer :: dp, dq, dr, ds
        integer :: x, y, z, w, xx, yy, zz, ww, ix, iy, iz, iw

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        call shell_pairs(pairs, nshls)
        allocate (buf2e(buffer_size(bas)))

        K = 0
        do ipr = 1, size(pairs, dim=2)
            p = pairs(1, ipr); q = pairs(2, ipr)
            shls(1) = p - 1; shls(3) = q - 1
            dp = nbas_per_shell(bas, p)
            dq = nbas_per_shell(bas, q)
            x = shell_slice(bas, p); xx = x + dp - 1
            y = shell_slice(bas, q); yy = y + dq - 1
            do jpr = 1, size(pairs, dim=2)
                r = pairs(1, jpr); s = pairs(2, jpr)
                shls(2) = r - 1; shls(4) = s - 1
                dr = nbas_per_shell(bas, r)
                ds = nbas_per_shell(bas, s)
                z = shell_slice(bas, r); zz = z + dr - 1
                w = shell_slice(bas, s); ww = w + ds - 1

                ! shell quartets (pr|qs)
                info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)

                eri(1:dp, 1:dr, 1:dq, 1:ds) => buf2e(:dp*dr*dq*ds)

                !> K_MN = (MP|NQ)*D_PQ
                do iy = 1, dq
                    do ix = 1, dp
                        K(x + ix - 1, y + iy - 1) = K(x + ix - 1, y + iy - 1) &
                                                & + sum(eri(ix, :, iy, :)*dm(z:zz, w:ww))
                    end do
                end do

                if (r /= s) then
                    shls(4) = r - 1; shls(2) = s - 1

                    ! shell quartets (ps|qr)
                    info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)

                    eri(1:dp, 1:ds, 1:dq, 1:dr) => buf2e(:dp*dr*dq*ds)

                    !> K_MN = (MQ|NP)*D_QP
                    do iy = 1, dq
                        do ix = 1, dp
                            K(x + ix - 1, y + iy - 1) = K(x + ix - 1, y + iy - 1) &
                                                    & + sum(eri(ix, :, iy, :)*dm(w:ww, z:zz))
                        end do
                    end do
                end if

            end do
            if (p /= q) then
                K(y:yy, x:xx) = transpose(K(x:xx, y:yy))
            end if
        end do

        call CINTdel_optimizer(opt)

    end subroutine calc_K

    pure function buffer_size(bas) result(n)
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer :: n
        integer :: maxl

        maxl = maxval(bas(ANG_OF :: BAS_SLOTS), dim=1)
        n = (2*maxl + 1)**4

    end function buffer_size

    subroutine shell_pairs(pairs, nshls)
        integer, dimension(:, :), allocatable, intent(out) :: pairs
        integer, intent(in) :: nshls
        integer :: i, j, k

        allocate (pairs(2, nshls*(nshls + 1)/2))

        k = 1
        do i = 1, nshls
            do j = i, nshls
                pairs(1, k) = i
                pairs(2, k) = j
                k = k + 1
            end do
        end do

    end subroutine shell_pairs

end module ringo_cint_int2e
