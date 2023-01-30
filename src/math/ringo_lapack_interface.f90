module ringo_lapack_interface
    use machina_basic, only: f8
    implicit none
    private
    public :: dgemm, dsyev

    interface
    !! adopted from https://github.com/numericalalgorithmsgroup/LAPACK_Examples
        subroutine dgemm(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc)
            import :: f8
            real(kind=f8), intent(in) :: alpha, beta
            integer, intent(in) :: k, lda, ldb, ldc, m, n
            character(len=1), intent(in) :: transa, transb
            real(kind=f8), intent(in) :: a(lda, *), b(ldb, *)
            real(kind=f8), intent(inout) :: c(ldc, *)
        end subroutine

        subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
            import :: f8
            integer, intent(out) :: info
            integer, intent(in) :: lda, lwork, n
            character(len=1), intent(in) :: jobz, uplo
            real(kind=f8), intent(inout) :: a(lda, *)
            real(kind=f8), intent(out) :: w(n), work(max(1, lwork))
            intrinsic :: max
        end subroutine

    end interface

end module ringo_lapack_interface
