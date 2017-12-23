!*********************************************************
! 重叠积分&动能积分 (A|B)&(A|-1/2delta^2|B)
!*********************************************************
subroutine Overlap_Kinetic_integral(coord,basis_a,basis_d,overlap,kinetic,n)
    implicit none
    integer :: n,i,j,p,q
	real(kind=8) :: coord(n,3),basis_a(n,3),basis_d(n,3)
	real(kind=8) :: overlap(n,n),kinetic(n,n)
    real(kind=8) :: rab,temp1,temp2
    real(kind=8),external :: Distance,overlap_gto,kinetic_gto
	!-----------------------------------------------------
    do i=1,n
        do j=i,n
            temp1=0.000000D0
            temp2=0.000000D0
            rab=Distance(coord(i,:),coord(j,:))
            do p=1,3
                do q=1,3
                    temp1=temp1+overlap_gto(rab,basis_a(i,p),basis_a(j,q),basis_d(i,p),basis_d(j,q))
                    temp2=temp2+kinetic_gto(rab,basis_a(i,p),basis_a(j,q),basis_d(i,p),basis_d(j,q))
                end do
            end do
            overlap(i,j)=temp1
            overlap(j,i)=temp1
            kinetic(i,j)=temp2
            kinetic(j,i)=temp2
        end do
    end do
	return
end subroutine

function overlap_gto(rab,a,b,La,Lb)
    implicit none
    real(kind=8) :: rab,a,b,La,Lb
    real(kind=8) :: overlap_gto
    overlap_gto = La*Lb*(4.0D0*a*b)**0.75D0/(a+b)**1.5D0*exp(-a*b/(a+b)*rab**2.000000D0)
    return
end function

function kinetic_gto(rab,a,b,La,Lb)
    implicit none
    real(kind=8) :: rab,a,b,La,Lb
    real(kind=8) :: kinetic_gto
    kinetic_gto = La*Lb*(2.828427125D0*(a*b)**1.750000D0/(a+b)**2.500000D0) & 
                  & *(3.000000D0-2.000000D0*a*b/(a+b)*rab**2.000000D0)*exp(-a*b/(a+b)*rab**2.000000D0)
    return
end function

!*********************************************************
! 核库仑积分 (A|-ZC/r1C|B)
!*********************************************************
subroutine Nu_attracive_integral(coord,nu_charge,basis_a,basis_d,nu_attrac,n)
    implicit none
    integer :: n,i,j,p,q,c
	real(kind=8) :: coord(n,3),basis_a(n,3),basis_d(n,3),nu_charge(n)
	real(kind=8) :: nu_attrac(n,n)
    real(kind=8) :: rab,rp(3),rpc,temp
    real(kind=8),external :: Distance,nu_attrac_gto
    !-----------------------------------------------------
    do i=1,n
        do j=i,n
            temp=0.000000D0
            rab=Distance(coord(i,:),coord(j,:))
            do c=1,n
                do p=1,3
                    do q=1,3
                        rp=(basis_a(i,p)*coord(i,:)+basis_a(j,q)*coord(j,:))/(basis_a(i,p)+basis_a(j,q))
                        rpc=Distance(rp,coord(c,:))
                        temp=temp+nu_attrac_gto(nu_charge(c),rab,rpc,basis_a(i,p),basis_a(j,q),basis_d(i,p),basis_d(j,q))
                    end do
                end do
            end do
            nu_attrac(i,j)=temp
            nu_attrac(j,i)=temp
        end do
    end do
    return
end subroutine

function nu_attrac_gto(nu_charge,rab,rpc,a,b,La,Lb)
    implicit none
    real(kind=8) :: nu_charge,rab,rpc,a,b,La,Lb
	real(kind=8),external :: BoysF
    real(kind=8) :: nu_attrac_gto
    real(kind=8),parameter :: pi=3.1415926535897932385D+00
    !-----------------------------------------------------
    nu_attrac_gto = La*Lb*-2.0D0**2.500000D0/dsqrt(pi)*nu_charge*(a*b)**0.750000D0/(a+b) &
                    & *exp(-a*b/(a+b)*rab**2.000000D0)*BoysF((a+b)*rpc**2.000000D0)
	return
end function

!*********************************************************
! 双电子积分 (ij|kl)
!*********************************************************
subroutine E_E_Repulse(coord,basis_a,basis_d,two_e,n)
    implicit none
    integer :: n,i,j,k,l,p,q,r,s
    real(kind=8) :: coord(n,3),basis_a(n,3),basis_d(n,3)
    real(kind=8) :: two_e(n,n,n,n)
    real(kind=8) :: rab,rcd,rp(3),rq(3),rpq,temp
    real(kind=8),external :: Distance,two_e_gto
    !-----------------------------------------------------
    do i=1,n
        do j=1,i
            do k=1,n
                do l=1,k
                    temp=0.000000D0
                    rab=Distance(coord(i,:),coord(j,:))
                    rcd=Distance(coord(k,:),coord(l,:))
                    do p=1,3
                        do q=1,3
                            do r=1,3
                                do s=1,3
                                    rp=(basis_a(i,p)*coord(i,:)+basis_a(j,q)*coord(j,:))/(basis_a(i,p)+basis_a(j,q))
                                    rq=(basis_a(k,r)*coord(k,:)+basis_a(l,s)*coord(l,:))/(basis_a(k,r)+basis_a(l,s))
                                    rpq=Distance(rp,rq)
                                    temp=temp+basis_d(i,p)*basis_d(j,q)*basis_d(k,r)*basis_d(l,s) &
                                         & *two_e_gto(rab,rcd,rpq,basis_a(i,p),basis_a(j,q),basis_a(k,r),basis_a(l,s))
                                end do
                            end do
                        end do
                    end do
                    two_e(i,j,k,l)=temp
                    two_e(j,i,k,l)=temp
                    two_e(i,j,l,k)=temp
                    two_e(j,i,l,k)=temp
                end do
            end do
        end do
    end do
	return
end subroutine

function two_e_gto(rab,rcd,rpq,a,b,c,d)
    implicit none
    real(kind=8) :: rab,rcd,rpq,a,b,c,d,two_e_gto
    real(kind=8),external :: BoysF
    real(kind=8),parameter :: pi=3.1415926535897932385D+00
    !-----------------------------------------------------
    two_e_gto = 16.000000D0/dsqrt(pi)*(a*b*c*d)**0.750000D0/((a+b)*(c+d)*dsqrt(a+b+c+d)) &
                & * exp(-a*b/(a+b)*rab*rab-c*d/(c+d)*rcd*rcd)*BoysF((a+b)*(c+d)/(a+b+c+d)*rpq*rpq)
    return
end function

!=============================
function Distance(ra,rb)
    implicit none
    real(kind=8) :: Distance
    real(kind=8) :: ra(3),rb(3)
    !-------------------------------
    Distance=dsqrt((ra(1)-rb(1))**2.000000D0+(ra(2)-rb(2))**2.000000D0+(ra(3)-rb(3))**2.000000D0)
    return
end function
function BoysF(x)
    implicit none
    real(kind = 8) :: BoysF
    real(kind = 8) :: x    
    real(kind=8),parameter :: pi=3.1415926535897932385D+00
    ! --------------------------------------------------------
    if (x<1.0D-8) then
        BoysF=1.0D0
    else
        BoysF=dsqrt(pi/(4.0D0*x))*derf(dsqrt(x))
    end if 
    return
end function