subroutine N_N_Reluse(coord,nu_charge,e_nn,n)
    implicit none
    integer :: n,i,j
    real(kind=8) :: coord(n,3),nu_charge(n)
    real(kind=8) :: e_nn
    real(kind=8),external :: Distance
    !-----------------------------------------------------
    e_nn=0.0D0
    do i=1,n-1
        do j=i+1,n
            e_nn=e_nn+nu_charge(i)*nu_charge(j)/Distance(coord(i,:),coord(j,:))
        end do
    end do
    return
end subroutine