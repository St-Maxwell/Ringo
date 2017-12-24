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
    integer,parameter :: n_basis=4 
    integer,parameter :: n_atom=4 
    integer,parameter :: n_alpha=2
    integer,parameter :: n_beta=1
    real(kind=8) :: S_overlap(n_basis,n_basis)=0D0
    real(kind=8) :: T_kinetic(n_basis,n_basis)=0D0
    real(kind=8) :: Nu_nuattrac(n_basis,n_basis)=0D0
    real(kind=8) :: H_core(n_basis,n_basis)=0D0
    real(kind=8) :: two_e(n_basis,n_basis,n_basis,n_basis)=0D0
    real(kind=8) :: Fock_a(n_basis,n_basis)=0D0
    real(kind=8) :: Fock_dia_a(n_basis,n_basis)=0D0
    real(kind=8) :: Fock_b(n_basis,n_basis)=0D0
    real(kind=8) :: Fock_dia_b(n_basis,n_basis)=0D0
    real(kind=8) :: unitary(n_basis,n_basis)=0D0
    real(kind=8) :: eigvalarr(n_basis)=0D0
    real(kind=8) :: s_overlap12(n_basis,n_basis)=0D0
    real(kind=8) :: X_canonical(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff_a(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff_new_a(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff_b(n_basis,n_basis)=0D0
    real(kind=8) :: C_coeff_new_b(n_basis,n_basis)=0D0
    real(kind=8) :: energy_a(n_basis,n_basis)=0D0
    real(kind=8) :: energy_b(n_basis,n_basis)=0D0
    real(kind=8) :: P_rho_a(n_basis,n_basis)=0D0
    real(kind=8) :: G_twoe_a(n_basis,n_basis)=0D0
    real(kind=8) :: P_rho_b(n_basis,n_basis)=0D0
    real(kind=8) :: G_twoe_b(n_basis,n_basis)=0D0
    real(kind=8) :: P_rho_total(n_basis,n_basis)=0D0
    real(kind=8) :: energy_e=0D0
    real(kind=8) :: energy_eold=0D0
    real(kind=8) :: nu_repuls=0D0
    real(kind=8) :: energy_total=0D0
    real(kind=8) :: Mulliken_charge(n_atom)=0D0
    real(kind=8) :: rho_orb(n_basis,n_basis)=0D0
    real(kind=8) :: time_begin,time_end
    integer :: i,j,k,l,p,q,x,y,steps
    integer :: istat=0
    !-----------------------------------------------------
    real(kind=8) :: coord(n_atom,3)
    real(kind=8) :: nu_charge(n_atom)
    real(kind=8) :: basis_a(n_atom,3)
    real(kind=8) :: basis_d(n_atom,3)
    coord(1,:)=(/-0.76174300,-0.41374000,0.00011000/)
    coord(2,:)=(/-0.75958600,0.41496200,0.00010800/)
    coord(3,:)=(/0.13659900,-0.00138500,-0.00037400/)
    coord(4,:)=(/1.38473000,0.00016200,0.00015600/)
    coord=coord*1.8897260D0
    nu_charge=1.0D0
    do i=1,n_basis
        basis_a(i,:)=(/0.1688554040D0,0.6239137298D0,0.3425250914D1/)
        basis_d(i,:)=(/0.4446345422D0,0.5353281423D0,0.1543289673D0/)
    end do
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
    
    Fock_a=H_core
    Fock_b=H_core
    Fock_dia_a=matmul(transpose(X_canonical),matmul(Fock_a,X_canonical))
    Fock_dia_b=matmul(transpose(X_canonical),matmul(Fock_b,X_canonical))
    
    call diagsymat(Fock_dia_a,C_coeff_new_a,eigvalarr,istat)
    forall(i=1:n_basis) energy_a(i,i)=eigvalarr(i)
    call diagsymat(Fock_dia_b,C_coeff_new_b,eigvalarr,istat)
    forall(i=1:n_basis) energy_b(i,i)=eigvalarr(i)
    
    C_coeff_a=matmul(X_canonical,C_coeff_new_a)
    C_coeff_b=matmul(X_canonical,C_coeff_new_b)
    
    do i=1,n_basis
        do j=1,n_basis
            do k=1,n_alpha
                P_rho_a(i,j)=P_rho_a(i,j)+C_coeff_a(i,k)*C_coeff_a(j,k)
            end do
        end do
    end do
    do i=1,n_basis
        do j=1,n_basis
            do k=1,n_beta
                P_rho_b(i,j)=P_rho_b(i,j)+C_coeff_b(i,k)*C_coeff_b(j,k)
            end do
        end do
    end do
    P_rho_total=P_rho_a+P_rho_b
    do i=1,n_basis
        do j=1,n_basis
            do k=1,n_basis
                do l=1,n_basis
                    G_twoe_a(i,j)=G_twoe_a(i,j)+P_rho_total(k,l)*two_e(i,j,k,l)-P_rho_a(k,l)*two_e(i,l,k,j)
                end do
            end do
        end do
    end do
    do i=1,n_basis
        do j=1,n_basis
            do k=1,n_basis
                do l=1,n_basis
                    G_twoe_b(i,j)=G_twoe_b(i,j)+P_rho_total(k,l)*two_e(i,j,k,l)-P_rho_b(k,l)*two_e(i,l,k,j)
                end do
            end do
        end do
    end do
    Fock_a=H_core+G_twoe_a
    Fock_b=H_core+G_twoe_b
    
    do i=1,n_basis
        do j=1,n_basis
            energy_e=energy_e+0.5D0*(P_rho_total(j,i)*H_core(i,j)+P_rho_a(j,i)*Fock_a(i,j)+P_rho_b(j,i)*Fock_b(i,j))
        end do
    end do

    !--------------------SCF Procedure--------------------
    do while(.true.)
        Fock_dia_a=matmul(transpose(X_canonical),matmul(Fock_a,X_canonical))
        Fock_dia_b=matmul(transpose(X_canonical),matmul(Fock_b,X_canonical))
        
        call diagsymat(Fock_dia_a,C_coeff_new_a,eigvalarr,istat)
        forall(i=1:n_basis) energy_a(i,i)=eigvalarr(i)
        call diagsymat(Fock_dia_b,C_coeff_new_b,eigvalarr,istat)
        forall(i=1:n_basis) energy_b(i,i)=eigvalarr(i)
        
        C_coeff_a=matmul(X_canonical,C_coeff_new_a)
        C_coeff_b=matmul(X_canonical,C_coeff_new_b)
        
        P_rho_a=0.0D0
        P_rho_b=0.0D0
        do i=1,n_basis
            do j=1,n_basis
                do k=1,n_alpha
                    P_rho_a(i,j)=P_rho_a(i,j)+C_coeff_a(i,k)*C_coeff_a(j,k)
                end do
            end do
        end do
        do i=1,n_basis
            do j=1,n_basis
                do k=1,n_beta
                    P_rho_b(i,j)=P_rho_b(i,j)+C_coeff_b(i,k)*C_coeff_b(j,k)
                end do
            end do
        end do
            
        energy_eold=energy_e
        energy_e=0D0
        do i=1,n_basis
            do j=1,n_basis
                energy_e=energy_e+0.5D0*(P_rho_total(j,i)*H_core(i,j)+P_rho_a(j,i)*Fock_a(i,j)+P_rho_b(j,i)*Fock_b  (i,j))
            end do
        end do

        if (abs(energy_e-energy_eold)<1.0D-6) exit ! 收敛标准delta<1.0D-6
        
        G_twoe_a=0.0D0
        G_twoe_b=0.0D0
        P_rho_total=P_rho_a+P_rho_b
        do i=1,n_basis
            do j=1,n_basis
                do k=1,n_basis
                    do l=1,n_basis
                        G_twoe_a(i,j)=G_twoe_a(i,j)+P_rho_total(k,l)*two_e(i,j,k,l)-P_rho_a(k,l)*two_e(i,l,k,j)
                    end do
                end do
            end do
        end do
        do i=1,n_basis
            do j=1,n_basis
                do k=1,n_basis
                    do l=1,n_basis
                        G_twoe_b(i,j)=G_twoe_b(i,j)+P_rho_total(k,l)*two_e(i,j,k,l)-P_rho_b(k,l)*two_e(i,l,k,j)
                    end do
                end do
            end do
        end do
        Fock_a=H_core+G_twoe_a
        Fock_b=H_core+G_twoe_b
    end do
    !----------------------SCF Done-----------------------
    energy_total=energy_e+nu_repuls
    !rho_orb=matmul(P_rho,S_overlap)
    !forall(i=1:n_atom) Mulliken_charge(i)=nu_charge(i)-rho_orb(i,i)
    call cpu_time(time_end)
    !-----------------------Output------------------------
    write(*,"(' Ringo: a UHF/STO-3G calculation program')")
    write(*,"(' Ringo Is Not Gaussian/Orca')")
    write(*,"(' Programmed by St Maxwell')")
    write(*,"(' ***********************************************',/)")
    write(*,"(' Calculation Done Successfully!',/)")
    write(*,"(' ----- Restricted Hartree Fock Energy -----')") 
    write(*,"(' Electronic energy = ',F10.6,' a.u.')") energy_e
    write(*,"(' Nuclear repulsion energy = ',F9.6,' a.u.')") nu_repuls
    write(*,"(' Total energy = ',F10.6,' a.u.',/)") energy_total
    !---------------对于H3+体系请将两行注释之间注释------------------
    !    write(*,"(' ----- Population Analysis -----')") 
    !if (energy(1,1)<energy(2,2)) then
    !    write(*,"(' Occupied orbital eigenvalue = ',F10.6,' a.u.')") energy(1,1)
    !    write(*,"(' Virtual  orbital eigenvalue = ',F10.6,' a.u.',/)") energy(2,2)
    !    write(*,"(' ----- Molecular Orbitals Coefficients -----')")
    !    write(*,"('                        phi1      phi2 ')") 
    !    write(*,"(' Occupied orbital --  ',F8.5,'  ',F8.5)") (C_coeff(i,1),i=1,2)
    !    write(*,"(' Virtual  orbital --  ',F8.5,'  ',F8.5,/)") (C_coeff(i,2),i=1,2)
    !else
    !    write(*,"(' Occupied orbital eigenvalue = ',F10.6,' a.u.')") energy(2,2)
    !    write(*,"(' Virtual orbital eigenvalue = ',F10.6,' a.u.',/)") energy(1,1)
    !    write(*,"(' ----- Molecular Orbitals Coefficients -----')")
    !    write(*,"('                        phi1       phi2 ')") 
    !    write(*,"(' Occupied orbital --  ',F8.5,'  ',F8.5)") (C_coeff(i,2),i=1,2)
    !    write(*,"(' Virtual  orbital --  ',F8.5,'  ',F8.5,/)") (C_coeff(i,1),i=1,2)
    !end if
    !write(*,"(' ----- Mulliken Charges -----')")
    !write(*,"(' He  ',F8.6)") Mulliken_charge(1)
    !write(*,"(' H   ',F8.6,/)") Mulliken_charge(2)
    !----------------------------------------------------------
    write(*,"(' Job cpu time: ',F6.4,' seconds.')") time_end-time_begin
    read(*,*)
end program