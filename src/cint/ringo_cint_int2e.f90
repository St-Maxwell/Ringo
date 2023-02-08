module ringo_cint_int2e
    use iso_c_binding, only: c_ptr
    use machina_basic, only: i4, f8
    use ringo_cint_const
    use ringo_cint_interface
    use ringo_cint_utils
    implicit none
    private
    public :: calc_J, calc_K

contains

    subroutine calc_J(J, dm, atm, bas, env)
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
        integer, dimension(:), allocatable :: shl_dim, shl_slices
        integer :: natm, nshls, info
        real(kind=f8), dimension(6) :: factors
        integer :: ipr, jpr
        integer :: M, N, P, Q
        integer :: dimM, dimN, dimP, dimQ
        integer :: iM0, iN0, iP0, iQ0
        integer :: iM, iN, iP, iQ
        real(kind=f8) :: J_MN, eri_value
        real(kind=f8) :: fac2_D_MN
        integer :: i

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        call shell_pairs(pairs, nshls)
        allocate (shl_dim(nshls))
        allocate (shl_slices(nshls))
        do i = 1, nshls
            shl_dim(i) = nbas_per_shell(bas, i)
            shl_slices(i) = shell_slice(bas, i)
        end do
        allocate (buf2e(buffer_size(bas)))

        J = 0
        do ipr = 1, size(pairs, dim=2)
            M = pairs(1, ipr); N = pairs(2, ipr)
            shls(1) = M - 1; shls(2) = N - 1
            dimM = shl_dim(M); iM0 = shl_slices(M)
            dimN = shl_dim(N); iN0 = shl_slices(N)

            do jpr = 1, ipr
                P = pairs(1, jpr); Q = pairs(2, jpr)
                shls(3) = P - 1; shls(4) = Q - 1
                dimP = shl_dim(P); iP0 = shl_slices(P)
                dimQ = shl_dim(Q); iQ0 = shl_slices(Q)

                ! unique shell quartets (pq|rs)
                info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)
                eri(1:dimM, 1:dimN, 1:dimP, 1:dimQ) => buf2e(:dimM*dimN*dimP*dimQ)
                call unique_factor(M, N, P, Q, factors)

                do iN = 0, dimN - 1
                do iM = 0, dimM - 1
                    fac2_D_MN = factors(2)*dm(iM0 + iM, iN0 + iN)
                    J_MN = 0
                    do iQ = 0, dimQ - 1
                    do iP = 0, dimP - 1
                        eri_value = eri(iM + 1, iN + 1, iP + 1, iQ + 1)
                        J_MN = J_MN + eri_value*dm(iP0 + iP, iQ0 + iQ)
                        J(iP0 + iP, iQ0 + iQ) = J(iP0 + iP, iQ0 + iQ) + eri_value*fac2_D_MN
                    end do
                    end do
                    J(iM0 + iM, iN0 + iN) = J(iM0 + iM, iN0 + iN) + factors(1)*J_MN
                end do
                end do

            end do
        end do

        !> symmetrize
        J = 0.5_f8*(J + transpose(J))

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
        integer, dimension(:), allocatable :: shl_dim, shl_slices
        integer :: natm, nshls, info
        real(kind=f8), dimension(6) :: factors
        integer :: ipr, jpr
        integer :: M, N, P, Q
        integer :: dimM, dimN, dimP, dimQ
        integer :: iM0, iN0, iP0, iQ0
        integer :: iM, iN, iP, iQ
        real(kind=f8) :: K_MP, K_NP, eri_value
        real(kind=f8) :: fac5_D_NP, fac6_D_MP
        integer :: i

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        call shell_pairs(pairs, nshls)
        allocate (shl_dim(nshls))
        allocate (shl_slices(nshls))
        do i = 1, nshls
            shl_dim(i) = nbas_per_shell(bas, i)
            shl_slices(i) = shell_slice(bas, i)
        end do
        allocate (buf2e(buffer_size(bas)))

        K = 0
        do ipr = 1, size(pairs, dim=2)
            M = pairs(1, ipr); N = pairs(2, ipr)
            shls(1) = M - 1; shls(2) = N - 1
            dimM = shl_dim(M); iM0 = shl_slices(M)
            dimN = shl_dim(N); iN0 = shl_slices(N)

            do jpr = 1, ipr
                P = pairs(1, jpr); Q = pairs(2, jpr)
                shls(3) = P - 1; shls(4) = Q - 1
                dimP = shl_dim(P); iP0 = shl_slices(P)
                dimQ = shl_dim(Q); iQ0 = shl_slices(Q)

                ! unique shell quartets (pq|rs)
                info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)
                eri(1:dimM, 1:dimN, 1:dimP, 1:dimQ) => buf2e(:dimM*dimN*dimP*dimQ)
                call unique_factor(M, N, P, Q, factors)

                do iM = 0, dimM - 1
                do iN = 0, dimN - 1
                do iP = 0, dimP - 1
                    fac5_D_NP = factors(5)*dm(iN0 + iN, iP0 + iP)
                    fac6_D_MP = factors(6)*dm(iM0 + iM, iP0 + iP)
                    K_MP = 0; K_NP = 0
                    do iQ = 0, dimQ - 1
                        eri_value = eri(iM + 1, iN + 1, iP + 1, iQ + 1)
                        K_MP = K_MP + eri_value*dm(iN0 + iN, iQ0 + iQ)
                        K_NP = K_NP + eri_value*dm(iM0 + iM, iQ0 + iQ)

                        K(iM0 + iM, iQ0 + iQ) = K(iM0 + iM, iQ0 + iQ) + eri_value*fac5_D_NP
                        K(iN0 + iN, iQ0 + iQ) = K(iN0 + iN, iQ0 + iQ) + eri_value*fac6_D_MP
                    end do
                    K(iM0 + iM, iP0 + iP) = K(iM0 + iM, iP0 + iP) + factors(3)*K_MP
                    K(iN0 + iN, iP0 + iP) = K(iN0 + iN, iP0 + iP) + factors(4)*K_NP
                end do
                end do
                end do

            end do
        end do

        !> symmetrize
        K = 0.5_f8*(K + transpose(K))

        call CINTdel_optimizer(opt)

    end subroutine calc_K

    pure subroutine unique_factor(M, N, P, Q, factors)
        integer, intent(in) :: M, N, P, Q
        real(kind=f8), dimension(6), intent(out) :: factors
        integer :: flag1, flag2, flag3, flag4, flag5, flag6, flag7

        flag1 = merge(0, 1, M == N)
        flag2 = merge(0, 1, P == Q)
        flag3 = merge(0, 1, (M == P) .and. (N == Q))
        flag4 = merge(1, 0, (flag1 == 1) .and. (flag2 == 1))
        flag5 = merge(1, 0, (flag1 == 1) .and. (flag3 == 1))
        flag6 = merge(1, 0, (flag2 == 1) .and. (flag3 == 1))
        flag7 = merge(1, 0, (flag4 == 1) .and. (flag3 == 1))
        factors(1) = 1._f8 + flag1 + flag2 + flag4 ! for J_MN
        factors(2) = flag3 + flag5 + flag6 + flag7 ! for J_PQ
        factors(3) = 1._f8 + flag3 ! for K_MP
        factors(4) = flag1 + flag5 ! for K_NP
        factors(5) = flag2 + flag6 ! for K_MQ
        factors(6) = flag4 + flag7 ! for K_NQ

    end subroutine unique_factor

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
            do j = 1, i
                pairs(1, k) = i
                pairs(2, k) = j
                k = k + 1
            end do
        end do

    end subroutine shell_pairs

end module ringo_cint_int2e
