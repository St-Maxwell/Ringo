module ringo_math_linalg
    use machina_basic, only: f8, optval
    use machina_assert
    use ringo_lapack_interface
    implicit none
    private
    public :: matmul_gemm, solve_eigen, lowdin_orthogonaliztion, unitary_transform

    interface unitary_transform
        module procedure :: unitary_transform_diag
        module procedure :: unitary_transform_mat
    end interface

contains

    !> C = A * B
    subroutine matmul_gemm(A, B, C, AT, BT)
        real(kind=f8), dimension(:, :), intent(in) :: A
        real(kind=f8), dimension(:, :), intent(in) :: B
        real(kind=f8), dimension(:, :), intent(inout) :: C
        character(len=1), intent(in), optional :: AT
        character(len=1), intent(in), optional :: BT

        !! locals
        character(len=1) :: transa, transb
        integer :: M, N, K, LDA, LDB, LDC

        transa = optval(AT, 'N')
        transb = optval(BT, 'N')

        if (transa == 'N') then
            K = size(A, dim=2)
        else
            K = size(A, dim=1)
        end if

        M = size(C, dim=1)
        N = size(C, dim=2)
        LDA = max(1, size(A, dim=1))
        LDB = max(1, size(B, dim=1))
        LDC = max(1, size(C, dim=1))

        call dgemm(transa, transb, M, N, K, 1._f8, A, LDA, B, LDB, 0._f8, C, LDC)

    end subroutine matmul_gemm

    subroutine solve_eigen(mat, eigenval, eigenvec)
        real(kind=f8), dimension(:, :), intent(in) :: mat
        real(kind=f8), dimension(:), intent(out) :: eigenval
        real(kind=f8), dimension(:, :), intent(out) :: eigenvec

        !! locals
        integer, parameter :: nb = 64
        real(kind=f8), dimension(:), allocatable :: work
        real(kind=f8), dimension(1) :: dummy
        integer :: N, LDA, lwork, info

        LDA = size(mat, dim=1)
        N = size(mat, dim=2)
        eigenvec = mat

        lwork = -1
        call dsyev('V', 'U', N, eigenvec, LDA, eigenval, dummy, lwork, info)

        lwork = max((nb + 2)*n, nint(dummy(1)))
        allocate (work(lwork))

        call dsyev('V', 'U', N, eigenvec, LDA, eigenval, work, lwork, info)

        call debug_assert(info == 0, msg="dsyev did not successfully exit.")

    end subroutine solve_eigen

    !> C = X * C', X = S^(-1/2)
    !> or C' = S^(1/2) * C
    !> X^T * S * X = I
    !> X = S^(-1/2) = U * s^(-1/2) * U^T
    subroutine lowdin_orthogonaliztion(S, X)
        real(kind=f8), dimension(:, :), intent(in) :: S
        real(kind=f8), dimension(:, :), intent(out) :: X

        !! locals
        real(kind=f8), dimension(:, :), allocatable :: U, tmp
        real(kind=f8), dimension(:), allocatable :: s_eig
        !! s_eig = U * S * U^T
        integer :: i

        allocate (s_eig(size(S, dim=1)))
        allocate (U, mold=S)
        allocate (tmp, mold=S)

        call solve_eigen(S, s_eig, U)

        !! tmp = U * s^(-1/2)
        do i = 1, size(U, dim=2)
            tmp(:, i) = U(:, i)/sqrt(s_eig(i))
        end do

        !! S^(-1/2) = tmp * U^T
        call matmul_gemm(tmp, U, X, BT='T')

    end subroutine lowdin_orthogonaliztion

    !> B = U^T * A * U
    subroutine unitary_transform_diag(A, U, B)
        real(kind=f8), dimension(:), intent(in) :: A
        real(kind=f8), dimension(:, :), intent(in) :: U
        real(kind=f8), dimension(:, :), intent(out) :: B

        !! locals
        real(kind=f8), dimension(:, :), allocatable :: tmp
        integer :: i

        allocate (tmp, mold=B)

        !! tmp = A * U
        do i = 1, size(A)
            tmp(i, :) = U(i, :)*A(i)
        end do

        !! B = U^T * tmp = U^T * A * U
        call matmul_gemm(U, tmp, B, AT='T')

    end subroutine unitary_transform_diag

    !> B = U^T * A * U
    subroutine unitary_transform_mat(A, U, B)
        real(kind=f8), dimension(:, :), intent(in) :: A
        real(kind=f8), dimension(:, :), intent(in) :: U
        real(kind=f8), dimension(:, :), intent(out) :: B

        !! locals
        real(kind=f8), dimension(:, :), allocatable :: tmp

        allocate (tmp, mold=B)

        !! tmp = U^T * A
        call matmul_gemm(U, A, tmp, AT='T')

        !! B = tmp * U = U^T * A * U
        call matmul_gemm(tmp, U, B)

    end subroutine unitary_transform_mat

end module ringo_math_linalg
