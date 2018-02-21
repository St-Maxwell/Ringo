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
    integer,parameter :: n_basis=2 ! 基函数数目，对于H3+体系应改为3
    integer,parameter :: n_atom=2 ! 原子数目，对于H3+体系应改为3
    real(kind=8) :: S_overlap(n_basis,n_basis)=0D0
    real(kind=8) :: T_kinetic(n_basis,n_basis)=0D0
    real(kind=8) :: Nu_nuattrac(n_basis,n_basis)=0D0
    real(kind=8) :: H_core(n_basis,n_basis)=0D0
    real(kind=8) :: two_e(n_basis,n_basis,n_basis,n_basis)=0D0
    real(kind=8) :: two_e_psi(n_basis,n_basis,n_basis,n_basis)=0D0
    real(kind=8) :: Fock(n_basis,n_basis)=0D0
    real(kind=8) :: Fock_dia(n_basis,n_basis)=0D0
    real(kind=8) :: unitary(n_basis,n_basis)=0D0
    real(kind=8) :: eigvalarr(n_basis)=0D0
    real(kind=8) :: s_overlap12(n_basis,n_basis)=0D0
    real(kind=8) :: X_canonical(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff_new(n_basis,n_basis)=0D0
    real(kind=8) :: energy(n_basis,n_basis)=0D0
    real(kind=8) :: P_rho(n_basis,n_basis)=0D0
    real(kind=8) :: G_twoe(n_basis,n_basis)=0D0
    real(kind=8) :: energy_e=0D0
    real(kind=8) :: energy_eold=0D0
    real(kind=8) :: nu_repuls=0D0
    real(kind=8) :: energy_hf=0D0
    real(kind=8) :: e_corr=0D0
    real(kind=8) :: energy_ci=0D0
    real(kind=8) :: Mulliken_charge(n_atom)=0D0
    real(kind=8) :: rho_orb(n_basis,n_basis)=0D0
    real :: time_begin,time_end
    integer :: i,j,k,l,p,q,x,y,steps
    integer :: istat=0
    !-----------------------------------------------------
    real(kind=8) :: coord(n_atom,3)
    real(kind=8) :: nu_charge(n_atom)
    real(kind=8) :: basis_a(n_atom,3)
    real(kind=8) :: basis_d(n_atom,3)
    coord(1,:)=(/0.0,0.0,0.0/)
    coord(2,:)=(/0.0,0.0,1.0/)
    !-----------------------------------------------------
    coord=coord*1.8897260D0
    nu_charge=(/1.0D0,1.0D0/)
    basis_a(1,:)=(/0.1688554040D0,0.6239137298D0,0.3425250914D1/)
    basis_a(2,:)=(/0.1688554040D0,0.6239137298D0,0.3425250914D1/)
    basis_d(1,:)=(/0.4446345422D0,0.5353281423D0,0.1543289673D0/)
    basis_d(2,:)=(/0.4446345422D0,0.5353281423D0,0.1543289673D0/)
    !-----------------------------------------------------
    call cpu_time(time_begin)
    !--------------Calculate matrix elements--------------
    call N_N_Reluse(coord,nu_charge,nu_repuls,n_atom)
    call Overlap_Kinetic_integral(coord,basis_a,basis_d,S_overlap,T_kinetic,n_basis)
    call Nu_attracive_integral(coord,nu_charge,basis_a,basis_d,Nu_nuattrac,n_basis)
    call E_E_Repulse(coord,basis_a,basis_d,two_e,n_basis)
    !-----------对角化重叠矩阵，并得到密度矩阵的初猜------------
    H_core=T_kinetic+Nu_nuattrac
    call diagsymat(S_overlap,unitary,eigvalarr,istat)
    forall(i=1:n_basis) s_overlap12(i,i)=1.0D0/dsqrt(eigvalarr(i))
    X_canonical=matmul(unitary,s_overlap12)
    Fock=H_core 
    Fock_dia=matmul(transpose(X_canonical),matmul(Fock,X_canonical))
    call diagsymat(Fock_dia,C_coeff_new,eigvalarr,istat)
    forall(i=1:n_basis) energy(i,i)=eigvalarr(i)
    C_coeff=matmul(X_canonical,C_coeff_new)
    do i=1,n_basis
        do j=1,n_basis
            P_rho(i,j)=P_rho(i,j)+2.0D0*C_coeff(i,n_basis/2)*C_coeff(j,n_basis/2)
        end do
    end do
    do i=1,n_basis
        do j=1,n_basis
            do k=1,n_basis
                do l=1,n_basis
                    G_twoe(i,j)=G_twoe(i,j)+P_rho(k,l)*(two_e(i,j,k,l)-0.5D0*two_e(i,l,k,j))
                end do
            end do
        end do
    end do
    Fock=H_core+G_twoe
    do i=1,n_basis
        do j=1,n_basis
            energy_e=energy_e+0.5D0*P_rho(j,i)*(H_core(i,j)+Fock(i,j))
        end do
    end do        
    !--------------------SCF Procedure--------------------
    do while(.true.)
        Fock_dia=matmul(transpose(X_canonical),matmul(Fock,X_canonical))
        call diagsymat(Fock_dia,C_coeff_new,eigvalarr,istat)
        forall(i=1:n_basis) energy(i,i)=eigvalarr(i)
        C_coeff=matmul(X_canonical,C_coeff_new)
        P_rho=0.0D0
        do i=1,n_basis
            do j=1,n_basis
                P_rho(i,j)=P_rho(i,j)+2.0D0*C_coeff(i,n_basis/2)*C_coeff(j,n_basis/2)
            end do
        end do
        energy_eold=energy_e
        energy_e=0D0
        do i=1,n_basis
            do j=1,n_basis
                energy_e=energy_e+0.5D0*P_rho(j,i)*(H_core(i,j)+Fock(i,j))
            end do
        end do
        if (abs(energy_e-energy_eold)<1.0D-6) exit ! 收敛标准delta<1.0D-6
        G_twoe=0.0D0
        do i=1,n_basis
            do j=1,n_basis
                do k=1,n_basis
                    do l=1,n_basis
                        G_twoe(i,j)=G_twoe(i,j)+P_rho(k,l)*(two_e(i,j,k,l)-0.5D0*two_e(i,l,k,j))
                    end do
                end do
            end do
        end do
        Fock=H_core+G_twoe
    end do

    !---------------------- J & K ------------------------

    call adjust_position(energy,C_coeff)
    call two_e_P(two_e_psi,two_e,C_coeff)
    call calc_e_corr(energy,two_e_psi,e_corr)

    !----------------------SCF Done-----------------------
    energy_hf=energy_e+nu_repuls
    energy_ci=energy_hf+e_corr
    rho_orb=matmul(P_rho,S_overlap)
    forall(i=1:n_atom) Mulliken_charge(i)=nu_charge(i)-rho_orb(i,i)
    call cpu_time(time_end)
    !-----------------------Output------------------------
    write(*,"(' Ringo: a RHF/STO-3G calculation program')")
    write(*,"(' Ringo Is Not Gaussian/Orca')")
    write(*,"(' Programmed by St Maxwell')")
    write(*,"(' ***********************************************',/)")
    write(*,"(' Calculation Done Successfully!',/)")
    write(*,"(' ----- Restricted Hartree Fock Energy -----')") 
    write(*,"(' Electronic energy = ',F10.6,' a.u.')") energy_e
    write(*,"(' Nuclear repulsion energy = ',F9.6,' a.u.')") nu_repuls
    write(*,"(' HF energy = ',F10.6,' a.u.')") energy_hf
    write(*,"(' Correlation energy = ',F10.6,' a.u.')") e_corr
    write(*,"(' CI energy = ',F10.6,' a.u.',/)") energy_ci
    !---------------对于H3+体系请将两行注释之间注释------------------
    write(*,"(' ----- Population Analysis -----')") 
    write(*,"(' Occupied orbital eigenvalue = ',F10.6,' a.u.')") energy(1,1)
    write(*,"(' Virtual  orbital eigenvalue = ',F10.6,' a.u.',/)") energy(2,2)
    write(*,"(' ----- Molecular Orbitals Coefficients -----')")
    write(*,"('                        phi1      phi2 ')") 
    write(*,"(' Occupied orbital --  ',F8.5,'  ',F8.5)") (C_coeff(i,1),i=1,2)
    write(*,"(' Virtual  orbital --  ',F8.5,'  ',F8.5,/)") (C_coeff(i,2),i=1,2)
    write(*,"(' ----- Mulliken Charges -----')")
    write(*,"(' H  ',F8.6)") Mulliken_charge(1)
    write(*,"(' H   ',F8.6,/)") Mulliken_charge(2)
    !----------------------------------------------------------
    write(*,"(' Job cpu time: ',F6.4,' seconds.')") time_end-time_begin
    read(*,*)
end program

subroutine adjust_position(energy,C_coeff)
    implicit none
    real(kind=8) :: energy(2,2)
    real(kind=8) :: C_coeff(2,2)
    real(kind=8) :: temp1
    real(kind=8) :: temp2(2)

    if (energy(1,1)>energy(2,2)) then
        temp1 = energy(1,1)
        energy(1,1) = energy(2,2)
        energy(2,2) = temp1
        temp2(:) = C_coeff(1,:)
        C_coeff(1,:) = C_coeff(2,:)
        C_coeff(2,:) = temp2(:)
    end if

end subroutine

subroutine two_e_P(two_e_psi,two_e,C_coeff)
    implicit none
    real(kind=8) :: two_e_psi(2,2,2,2)
    real(kind=8) :: two_e(2,2,2,2)
    real(kind=8) :: C_coeff(2,2)
    real(kind=8) :: temp
    integer :: i,j,k,l,m,n,r,s

    do i=1,2
    do j=1,2
    do k=1,2
    do l=1,2
        temp=0.0D0
        do m=1,2
        do n=1,2
        do r=1,2
        do s=1,2
            temp = temp + C_coeff(m,i)*C_coeff(n,j)*C_coeff(r,k) &
                                   *C_coeff(s,l) * two_e(m,n,r,s)
        end do
        end do
        end do
        end do
        two_e_psi(i,j,k,l) = temp
    end do
    end do
    end do
    end do

end subroutine
    
subroutine calc_e_corr(energy,two_e_psi,e_corr)
    implicit none
    real(kind=8) :: energy(2,2)
    real(kind=8) :: two_e_psi(2,2,2,2)
    real(kind=8) :: e_corr
    real(kind=8) :: D
    
    D = energy(2,2) - energy(1,1) + 0.5D0 * two_e_psi(1,1,1,1) + 0.5D0 * two_e_psi(2,2,2,2) &
        - 2.0D0 * two_e_psi(1,1,2,2) + two_e_psi(1,2,2,1)
    e_corr = D - sqrt( D*D + two_e_psi(1,2,2,1)*two_e_psi(1,2,2,1) )
    
end subroutine