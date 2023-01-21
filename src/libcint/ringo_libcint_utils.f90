module ringo_libcint_utils
    use machina_basic, only: f8
    implicit none

contains

    pure function gaussian_int(n, alpha) result(r)
        integer, intent(in) :: n
        real(kind=f8), intent(in) :: alpha
        real(kind=f8) :: r
        real(kind=f8) :: n1

        n1 = (n + 1)*0.5_f8
        r = gamma(n1)/(2*alpha**n1)

    end function gaussian_int

    pure function gto_norm(n, a) result(r)
        integer, intent(in) :: n
        real(kind=f8), intent(in) :: a
        real(kind=f8) :: r

        r = 1/sqrt(gaussian_int(n*2 + 2, 2*a))

    end function gto_norm

end module ringo_libcint_utils
