module ringo_cint_utils
    use machina_basic, only: i4, f8
    use ringo_cint_const
    implicit none
    private
    public :: nshell, nao, nbas_per_shell, shell_slice
    public :: gaussian_int, gto_norm

contains

    pure function nao(bas) result(n)
        !> libcint bas vector
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer :: n
        integer, dimension(:), allocatable :: l

        l = bas(ANG_OF :: BAS_SLOTS)
        n = sum(l*2 + 1)

    end function nao

    pure function nshell(bas) result(n)
        !> libcint bas vector
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer :: n

        n = size(bas)/BAS_SLOTS

    end function nshell

    pure function nbas_per_shell(bas, idx) result(n)
        !> libcint bas vector
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer, intent(in) :: idx ! 1-based
        integer :: n

        n = bas(BAS_SLOTS*(idx - 1) + ANG_OF)*2 + 1

    end function nbas_per_shell

    pure function shell_slice(bas, idx) result(n)
        !> libcint bas vector
        integer(kind=i4), dimension(:), intent(in) :: bas
        integer, intent(in) :: idx ! 1-based
        integer :: n
        integer :: i

        n = 1
        do i = 1, idx - 1
            n = n + nbas_per_shell(bas, i)
        end do

    end function shell_slice

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

end module ringo_cint_utils
