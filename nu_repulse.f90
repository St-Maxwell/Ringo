subroutine N_N_Reluse(coord,nu_charge,e_nn,n)
    implicit none
    integer :: n,i,j
    real(kind=8) :: coord(n,3),nu_charge(n)
    real(kind=8) :: e_nn=0.000000D0
    real(kind=8),external :: Distance
    !-----------------------------------------------------
    do i=1,n
        do j=i,n
            e_nn=e_nn+nu_charge(i)*nu_charge(j)/Distance(coord(i,:),coord(j,:))
        end do
    end do
    return
end subroutine