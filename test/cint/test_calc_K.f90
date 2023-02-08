module test_calc_K
    use iso_fortran_env
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use machina_basic, only: i4, f8
    use ringo_cint_int2e, only: calc_K
    use ringo_log_utils, only: print_mat
    implicit none
    private
    public :: collect_calc_K

    integer(kind=i4), dimension(*), parameter :: ref_atm = [ &
 &  7, 20, 1, 0, 0, 0, 1, 23, 1, 0, &
 &  0, 0, 1, 26, 1, 0, 0, 0, 1, 29, &
 &  1, 0, 0, 0]

    integer(kind=i4), dimension(*), parameter :: ref_bas = [ &
 &  0, 0, 3, 1, 0, 38, 41, 0, 0, 0, &
 &  3, 1, 0, 44, 47, 0, 0, 1, 3, 1, &
 &  0, 50, 53, 0, 1, 0, 3, 1, 0, 32, &
 & 35, 0, 2, 0, 3, 1, 0, 32, 35, 0, &
 &  3, 0, 3, 1, 0, 32, 35, 0]

    real(kind=f8), dimension(*), parameter :: ref_env = [ &
 &  0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, &
 &  0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, &
 &  0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, &
 &  0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, 0.000000000000000_f8, &
 &  0.000000000000000_f8, 0.000000000000000_f8, -0.111855536686840_f8, 0.000000000000000_f8, 1.781650873421343_f8, &
 &  0.518053335298014_f8, 1.542954917893595_f8, -0.890825436710671_f8, 0.518053335298014_f8, -1.542954917893595_f8, &
 & -0.890825436710671_f8, 0.518053335298014_f8, 3.425250914000000_f8, 0.623913729800000_f8, 0.168855404000000_f8, &
 &  0.981706728215802_f8, 0.949464007914664_f8, 0.295906459686615_f8, 99.106168960000005_f8, 18.052312390000001_f8, &
 &  4.885660238000000_f8, 12.247233292760354_f8, 11.844990843614378_f8, 3.691566269212947_f8, 3.780455879000000_f8, &
 &  0.878496644900000_f8, 0.285714374400000_f8, -0.684748058383098_f8, 0.915905846477845_f8, 0.691247805352388_f8, &
 &  3.780455879000000_f8, 0.878496644900000_f8, 0.285714374400000_f8, 2.397761929253094_f8, 1.507777045121834_f8, &
 &  0.238857216028863_f8]

    real(kind=f8), dimension(8, 8), parameter :: ref_dm = reshape([ &
 &  1.071861156481741_f8, -0.342182083394711_f8, 0.000000000000000_f8, -0.000000000033624_f8, -0.029256307020913_f8, &
 &  0.048733145447144_f8, 0.048733145414215_f8, 0.048733145414216_f8, -0.342182083394711_f8, 1.617567075618316_f8, &
 & -0.000000000000002_f8, 0.000000000264296_f8, 0.189693445665099_f8, -0.225249310160231_f8, -0.225249309974030_f8, &
 & -0.225249309974037_f8, 0.000000000000000_f8, -0.000000000000002_f8, 1.267064750352007_f8, 0.000000000000000_f8, &
 & -0.000000000000001_f8, 0.000000000000002_f8, -0.229472279824177_f8, 0.229472279824175_f8, -0.000000000033624_f8, &
 &  0.000000000264296_f8, 0.000000000000000_f8, 1.267064750340126_f8, 0.000000000099067_f8, -0.264971765150370_f8, &
 &  0.132485882443363_f8, 0.132485882443365_f8, -0.029256307020913_f8, 0.189693445665099_f8, -0.000000000000001_f8, &
 &  0.000000000099067_f8, 1.061687304049258_f8, -0.088621903969432_f8, -0.088621903878939_f8, -0.088621903878940_f8, &
 &  0.048733145447144_f8, -0.225249310160231_f8, 0.000000000000002_f8, -0.264971765150370_f8, -0.088621903969432_f8, &
 &  0.090503810023010_f8, 0.007386467796443_f8, 0.007386467796444_f8, 0.048733145414215_f8, -0.225249309974030_f8, &
 & -0.229472279824177_f8, 0.132485882443363_f8, -0.088621903878939_f8, 0.007386467796443_f8, 0.090503809931047_f8, &
 &  0.007386467754057_f8, 0.048733145414216_f8, -0.225249309974037_f8, 0.229472279824175_f8, 0.132485882443365_f8, &
 & -0.088621903878940_f8, 0.007386467796444_f8, 0.007386467754057_f8, 0.090503809931048_f8], [8, 8])

contains

    subroutine collect_calc_K(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("compare_K", compare_K) &
                    ]

    end subroutine collect_calc_K

    subroutine compare_K(error)
        type(error_type), allocatable, intent(out) :: error
        real(kind=f8), dimension(8, 8) :: ref_K
        real(kind=f8), dimension(8, 8) :: ref_K_master
        real(kind=f8), dimension(8, 8) :: K
        real(kind=f8), parameter :: eps = epsilon(1._f8)*2

        call ref_calc_K(ref_K, ref_dm, ref_atm, ref_bas, ref_env)
        call calc_K(K, ref_dm, ref_atm, ref_bas, ref_env)

        call print_mat(ref_K, "ref_K", output_unit)
        call print_mat(K, "K", output_unit)

        call check(error, sum(abs(K - ref_K))/size(K) < eps)

        if (allocated(error)) return

    end subroutine compare_K

    subroutine ref_calc_K(K, dm, atm, bas, env)
        use iso_c_binding, only: c_ptr
        use ringo_cint_const
        use ringo_cint_interface
        use ringo_cint_utils
        real(kind=f8), dimension(:, :), intent(inout) :: K
        real(kind=f8), dimension(:, :), intent(in) :: dm
        integer(kind=i4), dimension(:), intent(in) :: atm
        integer(kind=i4), dimension(:), intent(in) :: bas
        real(kind=f8), dimension(:), intent(in) :: env
        !> locals
        real(kind=f8), dimension(:, :, :, :), allocatable :: buf2e
        type(c_ptr) :: opt
        integer, dimension(4) :: shls
        integer, dimension(:, :), allocatable :: pairs
        integer, dimension(:), allocatable :: shl_dim, shl_slices
        integer :: natm, nshls, info
        integer :: M, N, P, Q
        integer :: dimM, dimN, dimP, dimQ
        integer :: iM0, iN0, iP0, iQ0
        integer :: iM, iN, iP, iQ
        integer :: i

        natm = size(atm)/ATM_SLOTS; nshls = nshell(bas)
        call cint2e_sph_optimizer(opt, atm, natm, bas, nshls, env)
        allocate (shl_dim(nshls))
        allocate (shl_slices(nshls))
        do i = 1, nshls
            shl_dim(i) = nbas_per_shell(bas, i)
            shl_slices(i) = shell_slice(bas, i)
        end do

        K = 0
        do M = 1, nshls
            shls(1) = M - 1
            dimM = shl_dim(M); iM0 = shl_slices(M)
            do N = 1, nshls
                shls(2) = N - 1
                dimN = shl_dim(N); iN0 = shl_slices(N)
                do P = 1, nshls
                    shls(3) = P - 1
                    dimP = shl_dim(P); iP0 = shl_slices(P)
                    do Q = 1, nshls
                        shls(4) = Q - 1
                        dimQ = shl_dim(Q); iQ0 = shl_slices(Q)

                        allocate (buf2e(dimM, dimN, dimP, dimQ))
                        info = cint2e_sph(buf2e, shls, atm, natm, bas, nshls, env, opt)

                        do iM = 0, dimM - 1
                        do iP = 0, dimP - 1
                            K(iM0 + iM, iP0 + iP) = K(iM0 + iM, iP0 + iP) +&
                            & sum(buf2e(iM + 1, :, iP + 1, :)*dm(iN0:iN0 + dimN - 1, iQ0:iQ0 + dimQ - 1))
                        end do
                        end do

                        deallocate (buf2e)
                    end do
                end do
            end do
        end do

        call CINTdel_optimizer(opt)

    end subroutine ref_calc_K

end module test_calc_K
