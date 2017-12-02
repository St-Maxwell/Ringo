!*********************************************************
! Ringo: a HeH+ RHF/STO-3G calculation program
! Ringo Is Not Gaussian/Orca
! Programmed by St Maxwell 
!*********************************************************
module diamat
    contains
    !*****************************************************
    ! 求解实对称矩阵的特征矩阵和特征值
    ! mat————待求解矩阵
    ! eigvecmat————特征矩阵
    ! eigvalarr————特征值
    !*****************************************************
    subroutine diagsymat(mat,eigvecmat,eigvalarr,istat)
        implicit none
        integer istat,isize,i
        real(kind=8) :: mat(:,:),eigvecmat(:,:),eigvalarr(:)
        real(kind=8),allocatable :: temp(:,:)
        real(kind=8),allocatable :: lworkvec(:)
        !-------------------------------------------------
        isize=size(mat,1)
        allocate(temp(isize,isize))
        allocate(lworkvec(3*isize-1))
        temp=mat
        call DSYEV('V','U',isize,mat,isize,eigvalarr,lworkvec,3*isize-1,istat)
        eigvecmat=mat
        mat=temp
    end subroutine
end module
program rhf_sto3g
    use lapack95 ! 本程序用于对角化矩阵部分依赖MKL库的LAPACK包，编译时需链接
    use diamat
    implicit none
    real(kind=8),external :: overlap,kinetic,nu_attrac,two_elec
    real(kind=8) :: contra_coeff(3,2)=reshape((/ 0.4446345422D0,0.5353281423D0,0.1543289673D0, &
                                               & 0.4446345422D0,0.5353281423D0,0.1543289673D0 /),(/3,2/))
    real(kind=8) :: orbi_expo(3,2)=reshape((/ 0.48084429D0,1.776691148D0,9.753934616D0, &
                                            & 0.1688554040D0,0.6239137298D0,0.3425250914D1 /),(/3,2/))
    real(kind=8) :: S_overlap(2,2)=0D0
    real(kind=8) :: T_kinetic(2,2)=0D0
    real(kind=8) :: V_nuattrac1(2,2)=0D0
    real(kind=8) :: V_nuattrac2(2,2)=0D0
    real(kind=8) :: H_core(2,2)=0D0
    real(kind=8) :: two_e(2,2,2,2)=0D0
    real(kind=8) :: Fock(2,2)=0D0
    real(kind=8) :: Fock_dia(2,2)=0D0
    real(kind=8) :: unitary(2,2)=0D0
    real(kind=8) :: eigvalarr(2)=0D0
    real(kind=8) :: s_overlap12(2,2)=0D0
    real(kind=8) :: X_canonical(2,2)=0D0
    real(kind=8) :: C_coeff(2,2)=0D0
    real(kind=8) :: C_coeff_new(2,2)=0D0
    real(kind=8) :: energy(2,2)=0D0
    real(kind=8) :: P_rho(2,2)=0D0
    real(kind=8) :: G_twoe(2,2)=0D0
    real(kind=8) :: energy_e=0D0
    real(kind=8) :: energy_eold=0D0
    real(kind=8) :: nu_repuls=0D0
    real(kind=8) :: energy_total=0D0
    real(kind=8) :: Mulliken_charge(2)=0D0
    real(kind=8) :: nu_charge(2)=(/2.0D0,1.0D0/) ! 1号是He，2号是H
    real(kind=8) :: R_mo=1.4632D0
    real(kind=8) :: R_atom(2)
    real :: time_begin,time_end
    integer :: i,j,k,l,p,q,x,y,steps
    integer :: istat=0
    !-----------------------------------------------------
    call cpu_time(time_begin)
    R_atom(1)=1.007276D0/(1.007276D0+4.001506D0)*R_mo
    R_atom(2)=-4.001506D0/(1.007276D0+4.001506D0)*R_mo
    nu_repuls=nu_charge(1)*nu_charge(2)/R_mo
    !--------------Calculate matrix elements--------------
    do i=1,2
        do j=1,2
            do p=1,3
                do q=1,3
                    S_overlap(i,j)=S_overlap(i,j)+contra_coeff(p,i)*contra_coeff(q,j)*overlap(orbi_expo(p,i),orbi_expo(q,j),R_atom(i),R_atom(j))
                    T_kinetic(i,j)=T_kinetic(i,j)+contra_coeff(p,i)*contra_coeff(q,j)*kinetic(orbi_expo(p,i),orbi_expo(q,j),R_atom(i),R_atom(j))
                    V_nuattrac1(i,j)=V_nuattrac1(i,j)+contra_coeff(p,i)*contra_coeff(q,j)* &
                        & nu_attrac(nu_charge(1),orbi_expo(p,i),orbi_expo(q,j),R_atom(i),R_atom(j),R_atom(1))
                    V_nuattrac2(i,j)=V_nuattrac2(i,j)+contra_coeff(p,i)* &
                        & contra_coeff(q,j)*nu_attrac(nu_charge(2),orbi_expo(p,i),orbi_expo(q,j),R_atom(i),R_atom(j),R_atom(2))
                end do
            end do
            do k=1,2
                do l=1,2
                    do p=1,3
                        do q=1,3
                            do x=1,3
                                do y=1,3
                                    two_e(i,j,k,l)=two_e(i,j,k,l)+contra_coeff(p,i)*contra_coeff(q,j)*contra_coeff(x,k)*contra_coeff(y,l)* &
                                        & two_elec(orbi_expo(p,i),orbi_expo(q,j),orbi_expo(x,k),orbi_expo(y,l),R_atom(i),R_atom(j),R_atom(k),R_atom(l))
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
    !-----------对角化重叠矩阵，并得到密度矩阵的初猜------------
    H_core=T_kinetic+V_nuattrac1+V_nuattrac2
    call diagsymat(S_overlap,unitary,eigvalarr,istat)
    forall(i=1:2) s_overlap12(i,i)=1.0D0/sqrt(eigvalarr(i))
    X_canonical=matmul(unitary,s_overlap12)
    Fock=H_core 
    Fock_dia=matmul(transpose(X_canonical),matmul(Fock,X_canonical))
    call diagsymat(Fock_dia,C_coeff_new,eigvalarr,istat)
    forall(i=1:2) energy(i,i)=eigvalarr(i)
    C_coeff=matmul(X_canonical,C_coeff_new)
    do i=1,2
        do j=1,2
            P_rho(i,j)=P_rho(i,j)+2.0D0*C_coeff(i,1)*C_coeff(j,1)
        end do
    end do
    do i=1,2
        do j=1,2
            do k=1,2
                do l=1,2
                    G_twoe(i,j)=G_twoe(i,j)+P_rho(k,l)*(two_e(i,j,k,l)-0.5D0*two_e(i,l,k,j))
                end do
            end do
        end do
    end do
    Fock=H_core+G_twoe
    do i=1,2
        do j=1,2
            energy_e=energy_e+0.5D0*P_rho(j,i)*(H_core(i,j)+Fock(i,j))
        end do
    end do    
    !--------------------SCF Procedure--------------------
    do while(.true.)
        Fock_dia=matmul(transpose(X_canonical),matmul(Fock,X_canonical))
        call diagsymat(Fock_dia,C_coeff_new,eigvalarr,istat)
        forall(i=1:2) energy(i,i)=eigvalarr(i)
        C_coeff=matmul(X_canonical,C_coeff_new)
        P_rho=0.0D0
        do i=1,2
            do j=1,2
                P_rho(i,j)=P_rho(i,j)+2.0D0*C_coeff(i,1)*C_coeff(j,1)
            end do
        end do
        energy_eold=energy_e
        energy_e=0D0
        do i=1,2
            do j=1,2
                energy_e=energy_e+0.5D0*P_rho(j,i)*(H_core(i,j)+Fock(i,j))
            end do
        end do
        if (abs(energy_e-energy_eold)<1.0D-6) exit ! 收敛标准delta<1.0D-6
        G_twoe=0.0D0
        do i=1,2
            do j=1,2
                do k=1,2
                    do l=1,2
                        G_twoe(i,j)=G_twoe(i,j)+P_rho(k,l)*(two_e(i,j,k,l)-0.5D0*two_e(i,l,k,j))
                    end do
                end do
            end do
        end do
        Fock=H_core+G_twoe
    end do
    !----------------------SCF Done-----------------------
    energy_total=energy_e+nu_repuls
    do i=1,2
        Mulliken_charge(1)=Mulliken_charge(1)-P_rho(1,i)*S_overlap(i,1)
        Mulliken_charge(2)=Mulliken_charge(2)-P_rho(2,i)*S_overlap(i,2)
    end do
    Mulliken_charge(1)=Mulliken_charge(1)+nu_charge(1)
    Mulliken_charge(2)=Mulliken_charge(2)+nu_charge(2)
    call cpu_time(time_end)
    !-----------------------Output------------------------
    write(*,"(' Ringo: a HeH+ RHF/STO-3G calculation program')")
    write(*,"(' Ringo Is Not Gaussian/Orca')")
    write(*,"(' Programmed by St Maxwell')")
    write(*,"(' ***********************************************',/)")
    write(*,"(' Calulation Done Successfully!',/)")
    write(*,"(' Distance = ',F6.4,' a.u.',/)") R_mo
    write(*,"(' ----- Restricted Hartree Fock Energy -----')") 
    write(*,"(' Electronic energy = ',F10.6,' a.u.')") energy_e
    write(*,"(' Nuclear repulsion energy = ',F9.6,' a.u.')") nu_repuls
    write(*,"(' Total energy = ',F10.6,' a.u.',/)") energy_total
    write(*,"(' ----- Population Analysis -----')") 
    if (energy(1,1)<energy(2,2)) then
        write(*,"(' Occupied orbital eigenvalue = ',F10.6,' a.u.')") energy(1,1)
        write(*,"(' Virtual  orbital eigenvalue = ',F10.6,' a.u.',/)") energy(2,2)
        write(*,"(' ----- Molecular Orbitals Coefficients -----')")
        write(*,"('                        phi1      phi2 ')") 
        write(*,"(' Occupied orbital --  ',F8.5,'  ',F8.5)") (C_coeff(i,1),i=1,2)
        write(*,"(' Virtual  orbital --  ',F8.5,'  ',F8.5,/)") (C_coeff(i,2),i=1,2)
    else
        write(*,"(' Occupied orbital eigenvalue = ',F10.6,' a.u.')") energy(2,2)
        write(*,"(' Virtual orbital eigenvalue = ',F10.6,' a.u.',/)") energy(1,1)
        write(*,"(' ----- Molecular Orbitals Coefficients -----')")
        write(*,"('                        phi1       phi2 ')") 
        write(*,"(' Occupied orbital --  ',F8.5,'  ',F8.5)") (C_coeff(i,2),i=1,2)
        write(*,"(' Virtual  orbital --  ',F8.5,'  ',F8.5,/)") (C_coeff(i,1),i=1,2)
    end if
    write(*,"(' ----- Mulliken Charges -----')")
    write(*,"(' He  ',F8.6)") Mulliken_charge(1)
    write(*,"(' H   ',F8.6,/)") Mulliken_charge(2)
    write(*,"(' Job cpu time: ',F6.4,' seconds.')") time_end-time_begin
    read(*,*)
end program
!*********************************************************
! 重叠积分
! alpha,beta————Gaussian函数的指数系数
! R_A,R_B————Gaussian函数的中心
!*********************************************************
function overlap(alpha,beta,R_A,R_B)
    implicit none
    real(kind=8) :: alpha,beta,R_A,R_B
    real(kind=8) :: overlap
    !-----------------------------------------------------
    overlap=(4.0D0*alpha*beta)**0.75D0/(alpha+beta)**1.5D0*exp(-alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B))
end function
!*********************************************************
! 动能积分
! alpha,beta————Gaussian函数的指数系数
! R_A,R_B————Gaussian函数的中心
!********************************************************* 
function kinetic(alpha,beta,R_A,R_B)
    implicit none
    real(kind=8) :: alpha,beta,R_A,R_B
    real(kind=8) :: kinetic
    !-----------------------------------------------------
    kinetic=(2.828427125D0*(alpha*beta)**1.75D0/(alpha+beta)**2.5D0)*(3.0D0-2.0D0*alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B)) &
           & *exp(-alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B))
end function
!*********************************************************
! 核库仑积分
! charge————核电荷
! alpha,beta————Gaussian函数的指数系数
! R_A,R_B————Gaussian函数（电子）的中心
! R_C————核坐标
!*********************************************************
function nu_attrac(charge,alpha,beta,R_A,R_B,R_C)
    implicit none
    real(kind=8) :: alpha,beta,R_A,R_B,R_C,R_P
    real(kind=8) :: nu_attrac
    real(kind=8) :: charge
    real(kind=8) :: t
    real(kind=8),parameter :: pi=3.1415926535897932385D+00
    !-----------------------------------------------------
    R_P=(alpha*R_A+beta*R_B)/(alpha+beta)
    t=(alpha+beta)*(R_P-R_C)*(R_P-R_C)
    if (t<1.0D-06) then
        nu_attrac=-charge*5.656854249D00/sqrt(pi)*(alpha*beta)**0.75D0/(alpha+beta) ! 参考Ideas of Quantum Chemistry APPENDIX P
    else
        nu_attrac=-(4.0D0*alpha*beta)**0.75D0/(alpha+beta)*charge*exp(-alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B))*erf(sqrt(t))/sqrt(t)
    end if
end function
!*********************************************************
! 双电子积分
! alpha,beta,gama,delta————Gaussian函数的指数系数
! R_A,R_B,R_C,R_D————Gaussian函数的中心
! R_P————alpha,beta对应的Gaussian函数的等效中心
! R_Q————gama,delta对应的Gaussian函数的等效中心
!*********************************************************
function two_elec(alpha,beta,gama,delta,R_A,R_B,R_C,R_D)
    implicit none
    real(kind=8) :: alpha,beta,gama,delta,R_A,R_B,R_C,R_D,R_P,R_Q ! gamma是内建函数，写成gama以避免冲突
    real(kind=8) :: two_elec
    real(kind=8) :: t
    real(kind=8),parameter :: pi=3.1415926535897932385D+00
    !-----------------------------------------------------
    R_P=(alpha*R_A+beta*R_B)/(alpha+beta)
    R_Q=(gama*R_C+delta*R_D)/(gama+delta)
    t=(alpha+beta)*(gama+delta)/(alpha+beta+gama+delta)*(R_P-R_Q)*(R_P-R_Q)
    if (t<1.0D-06) then
        two_elec=16.0D0/sqrt(pi)*(alpha*beta*gama*delta)**0.75D0/((alpha+beta)*(gama+delta)*sqrt(alpha+beta+gama+delta))* &
            & exp(-alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B)-gama*delta/(gama+delta)*(R_C-R_D)*(R_C-R_D)) ! 参考Ideas of Quantum Chemistry APPENDIX P
    else
        two_elec=8.0D0*(alpha*beta*gama*delta)**0.75D0/((alpha+beta)*(gama+delta)*sqrt(alpha+beta+gama+delta))* &
            & exp(-alpha*beta/(alpha+beta)*(R_A-R_B)*(R_A-R_B)-gama*delta/(gama+delta)*(R_C-R_D)*(R_C-R_D))*erf(sqrt(t))/sqrt(t)
    end if
end function