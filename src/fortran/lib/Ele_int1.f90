module m_int
  implicit none
  private
  public :: Bas_Normal, Overlap_Kinetic_GE, N_E_Attractive_GE, &
            E_E_Repulsion_sp

  !-----------------------------------------------------------------------------
  ! A Electronic Integral Library written by @Warm_Cloud
  ! https://github.com/zhangyingfeng1993/Ele_Int1
  ! https://github.com/zhangyingfeng1993/Ele_Int2
  !
  ! |GTF> = BNa*(x-Ra(1))^La(1)*(y-Ra(2))*La(2)*(z-Ra(3))^La(3)*exp(-a*r^2)
  ! Cartesian type Gaussian functions
  ! BNa: Normalization factor of GTF
  ! Ra(3): x, y, z coordinates of GTF
  ! La(3): lx, ly, lz power of GTF !!-----------  lx, ly, lz <= 1  -----------!!
  ! a: exponent of GTF
  !-----------------------------------------------------------------------------
  
  interface
    subroutine Overlap_Kinetic_GE(a, b, La, Lb, Ra, Rb, S, Ki, YN)
    !---------------------------------------------------------------------------
    ! Calculate Overlap and Kinetic of GTFs
    ! <a|b> = S * BNa * BNb
    ! <a|-1/2 ∇^2|b> = Ki * BNa * BNb
    !
    ! >> Input
    ! a, b: exponent of |a> and |b>
    ! La(3), Lb(3): lx, ly, lz of |a> and |b>
    ! Ra(3), Rb(3): position of |a> and |b>
    ! YN = "Y", calculate Overlap and Kinetic
    ! YN = "N", calculate Overlap only
    ! >> Output
    ! S: Overlap without normalization factor
    ! Ki: Kinetic without normalization factor
    !---------------------------------------------------------------------------
      real(kind=8) :: a
      real(kind=8) :: b
      integer, dimension(3) :: La
      integer, dimension(3) :: Lb
      real(kind=8), dimension(3) :: Ra
      real(kind=8), dimension(3) :: Rb
      real(kind=8) :: S
      real(kind=8) :: Ki
      character(len=1) :: YN
    end subroutine

    subroutine Bas_Normal(a, l, m, n, BN)
    !---------------------------------------------------------------------------
    ! Calculate Normalization Factor
    !
    ! >> Input
    ! a: exponent of GTF
    ! l, m, n: lx, ly, lz of GTF
    ! >> Output
    ! BN: Normalization factor
    !---------------------------------------------------------------------------
      real(kind=8) :: a
      integer :: l
      integer :: m
      integer :: n
      real(kind=8) :: BN
    end subroutine

    subroutine N_E_Attractive_GE(a, b, La, Lb, Ra, Rb, Rn, Natm, Nuc_Char, S)
    !---------------------------------------------------------------------------
    ! Calculate Nucleus-Electron Attractive
    ! Σ_A <a|-Z_A/r_iA|b> = S * BNa * BNb
    !
    ! >> Input
    ! a, b: exponent of |a> and |b>
    ! La(3), Lb(3): lx, ly, lz of |a> and |b>
    ! Ra(3), Rb(3): position of |a> and |b>
    ! Natm: number of atoms
    ! Rn(3,Natm): position of atoms
    ! Nuc_Char(natom): nuclear charge of atoms
    ! >> Output
    ! S: Nucleus-Electron Attractive without normalization factor
    !---------------------------------------------------------------------------
      real(kind=8) :: a
      real(kind=8) :: b
      integer, dimension(3) :: La
      integer, dimension(3) :: Lb
      real(kind=8), dimension(3) :: Ra
      real(kind=8), dimension(3) :: Rb
      real(kind=8), dimension(3,Natm) :: Rn
      integer :: Natm
      integer, dimension(Natm) :: Nuc_Char
      real(kind=8) :: S
    end subroutine

    subroutine E_E_Repulsion_sp(a, b, c, d, Ra, Rb, Rc, Rd, La, Lb, Lc, Ld, S)
    !---------------------------------------------------------------------------
    ! Calculate Electron-Electron Repulsion
    ! <ab|-1/r12|cd> = S
    !
    ! >> Input
    ! a, b, c, d: exponent of |a>, |b>, |c> and |d>
    ! La(3), Lb(3), Lc(3), Ld(3): lx, ly, lz of |a>, |b>, |c> and |d>
    ! Ra(3), Rb(3), Rc(3), Rd(3): position of |a>, |b>, |c> and |d>
    ! >> Output
    ! S: Electron-Electron Repulsion with normalization factor
    !---------------------------------------------------------------------------
      real(kind=8) :: a
      real(kind=8) :: b
      real(kind=8) :: c
      real(kind=8) :: d
      real(kind=8), dimension(3) :: Ra
      real(kind=8), dimension(3) :: Rb
      real(kind=8), dimension(3) :: Rc
      real(kind=8), dimension(3) :: Rd
      integer, dimension(3) :: La
      integer, dimension(3) :: Lb
      integer, dimension(3) :: Lc
      integer, dimension(3) :: Ld
      real(kind=8) :: S
    end subroutine
  end interface

end module

submodule (m_int) m_ele_int1
contains
!======================================================================= 
subroutine Overlap_Kinetic_GE(a,b,La,Lb,Ra,Rb,S,Ki,YN)
    ! YN = "Y",calculate Overlap and Kinetic.
    ! YN = "N",calculate Overlap only.
    
    implicit none
    integer i,i1,j1,L_max
    integer La(3),Lb(3)
    integer(kind=8) Ca,Cb
    real(kind=8),parameter::Pi = 3.141592653589793d0
    real(kind=8) a,b,K,AB,S,Ki,Ix,Iy,Iz,KIx,KIy,KIz
    real(kind=8) I2x,I2y,I2z,I_2x,I_2y,I_2z,IT
    real(kind=8) fx(0:La(1)+Lb(1)+2),fy(0:La(2)+Lb(2)+2),fz(0:La(3)+Lb(3)+2)
    real(kind=8) Ra(3),Rb(3),Rp(3),PA(3),PB(3),Temp
    character(len=1) YN
    
    call GTO_GE(a,b,Ra,Rb,K,AB,Rp,PA,PB)
    L_max = sum(La(:)) + sum(Lb(:))
    
    if( L_max == 0 ) then
        S = K*(Pi/(a+b))**1.5
        if( YN /= "Y" ) return
        Ki = a*b/(a+b)*(3.0d0-2*a*b/(a+b)*AB)*S
        return
    end if       
    !------------------------------<0|0>--------------------------------
    do i = 0,La(1)+Lb(1)
       if( mod(i,2) == 1 ) cycle
       fx(i) = 0.0d0
       do i1 = 0,La(1)
          do j1 = 0,Lb(1)
             if ( i1+j1 == i ) then
                Ca = Comb(La(1),i1) 
                Cb = Comb(Lb(1),j1)
                fx(i) = fx(i) + Ca*Cb*PA(1)**(La(1)-i1)*PB(1)**(Lb(1)-j1)
             end if
          end do
       end do
    end do
    Ix = 0.0d0
    do i = 0,(La(1)+Lb(1))/2
        IT = DobF(2*i-1)
        Ix = Ix + fx(i*2)*IT/(2.0d0*(a+b))**i
    end do

    
    do i = 0,La(2)+Lb(2)
       if( mod(i,2) == 1 ) cycle
       fy(i) = 0.0d0
       do i1 = 0,La(2)
          do j1 = 0,Lb(2)
             if ( i1+j1 == i ) then
                Ca = Comb(La(2),i1) 
                Cb = Comb(Lb(2),j1)
                fy(i) = fy(i) + Ca*Cb*PA(2)**(La(2)-i1)*PB(2)**(Lb(2)-j1)
             end if
          end do
       end do
    end do    
    Iy = 0.0d0
    do i = 0,(La(2)+Lb(2))/2
        IT = DobF(2*i-1)
        Iy = Iy + fy(i*2)*IT/(2.0d0*(a+b))**i
    end do

        
    do i = 0,La(3)+Lb(3)
       if( mod(i,2) == 1 ) cycle
       fz(i) = 0.0d0
       do i1 = 0,La(3)
          do j1 = 0,Lb(3)
             if ( i1+j1 == i ) then
                Ca = Comb(La(3),i1) 
                Cb = Comb(Lb(3),j1)
                fz(i) = fz(i) + Ca*Cb*PA(3)**(La(3)-i1)*PB(3)**(Lb(3)-j1)
             end if
          end do
       end do
    end do
    Iz = 0.0d0
    do i = 0,(La(3)+Lb(3))/2
        IT = DobF(2*i-1)
        Iz = Iz + fz(i*2)*IT/(2.0d0*(a+b))**i
    end do
    
    Temp = (Pi/(a+b))**(1.5d0)*K
    S = Temp*Ix*Iy*Iz 
    if( YN /= "Y" ) return
    !------------------------------<0|+2>-------------------------------
    do i = 0,La(1)+Lb(1)+2
       if( mod(i,2) == 1 ) cycle
       fx(i) = 0.0d0
       do i1 = 0,La(1)
          do j1 = 0,Lb(1)+2
             if ( i1+j1 == i ) then
                Ca = Comb(La(1),i1) 
                Cb = Comb(Lb(1)+2,j1)
                fx(i) = fx(i) + Ca*Cb*PA(1)**(La(1)-i1)*PB(1)**(Lb(1)+2-j1)
             end if
          end do
       end do
    end do
    I2x = 0.0d0
    do i = 0,(La(1)+Lb(1))/2+1
        IT = DobF(2*i-1)
        I2x = I2x + fx(i*2)*IT/(2.0d0*(a+b))**i
    end do

    
    do i = 0,La(2)+Lb(2)+2
       if( mod(i,2) == 1 ) cycle
       fy(i) = 0.0d0
       do i1 = 0,La(2)
          do j1 = 0,Lb(2)+2
             if ( i1+j1 == i ) then
                Ca = Comb(La(2),i1) 
                Cb = Comb(Lb(2)+2,j1)
                fy(i) = fy(i) + Ca*Cb*PA(2)**(La(2)-i1)*PB(2)**(Lb(2)+2-j1)
             end if
          end do
       end do
    end do    
    I2y = 0.0d0
    do i = 0,(La(2)+Lb(2))/2+1
        IT = DobF(2*i-1)
        I2y = I2y + fy(i*2)*IT/(2.0d0*(a+b))**i
    end do

        
    do i = 0,La(3)+Lb(3)+2
       if( mod(i,2) == 1 ) cycle
       fz(i) = 0.0d0
       do i1 = 0,La(3)
          do j1 = 0,Lb(3)+2
             if ( i1+j1 == i ) then
                Ca = Comb(La(3),i1) 
                Cb = Comb(Lb(3)+2,j1)
                fz(i) = fz(i) + Ca*Cb*PA(3)**(La(3)-i1)*PB(3)**(Lb(3)+2-j1)
             end if
          end do
       end do
    end do
    I2z = 0.0d0
    do i = 0,(La(3)+Lb(3))/2+1
        IT = DobF(2*i-1)
        I2z = I2z + fz(i*2)*IT/(2.0d0*(a+b))**i
    end do
    !------------------------------<0|-2>-------------------------------
    if ( Lb(1) < 2 ) then
       I_2x = 0.0d0
    else   
    do i = 0,La(1)+Lb(1)-2
       if( mod(i,2) == 1 ) cycle
       fx(i) = 0.0d0
       do i1 = 0,La(1)
          do j1 = 0,Lb(1)-2
             if ( i1+j1 == i ) then
                Ca = Comb(La(1),i1) 
                Cb = Comb(Lb(1)-2,j1)
                fx(i) = fx(i) + Ca*Cb*PA(1)**(La(1)-i1)*PB(1)**(Lb(1)-2-j1)
             end if
          end do
       end do
    end do
    I_2x = 0.0d0
    do i = 0,(La(1)+Lb(1))/2-1
        IT = DobF(2*i-1)
        I_2x = I_2x + fx(i*2)*IT/(2.0d0*(a+b))**i
    end do
    end if

    if ( Lb(2) < 2 ) then
       I_2y = 0.0d0
    else     
    do i = 0,La(2)+Lb(2)-2
       if( mod(i,2) == 1 ) cycle
       fy(i) = 0.0d0
       do i1 = 0,La(2)
          do j1 = 0,Lb(2)-2
             if ( i1+j1 == i ) then
                Ca = Comb(La(2),i1) 
                Cb = Comb(Lb(2)-2,j1)
                fy(i) = fy(i) + Ca*Cb*PA(2)**(La(2)-i1)*PB(2)**(Lb(2)-2-j1)
             end if
          end do
       end do
    end do    
    I_2y = 0.0d0
    do i = 0,(La(2)+Lb(2))/2-1
        IT = DobF(2*i-1)
        I_2y = I_2y + fy(i*2)*IT/(2.0d0*(a+b))**i
    end do
    end if

    if ( Lb(3) < 2 ) then
       I_2z = 0.0d0
    else         
    do i = 0,La(3)+Lb(3)-2
       if( mod(i,2) == 1 ) cycle
       fz(i) = 0.0d0
       do i1 = 0,La(3)
          do j1 = 0,Lb(3)-2
             if ( i1+j1 == i ) then
                Ca = Comb(La(3),i1) 
                Cb = Comb(Lb(3)-2,j1)
                fz(i) = fz(i) + Ca*Cb*PA(3)**(La(3)-i1)*PB(3)**(Lb(3)-2-j1)
             end if
          end do
       end do
    end do
    I_2z = 0.0d0
    do i = 0,(La(3)+Lb(3))/2-1
        IT = DobF(2*i-1)
        I_2z = I_2z + fz(i*2)*IT/(2.0d0*(a+b))**i
    end do
    end if
    !-------------------------------------------------------------------
    I2x = Temp*I2x*Iy*Iz
    I2y = Temp*Ix*I2y*Iz
    I2z = Temp*Ix*Iy*I2z
    
    I_2x = Temp*I_2x*Iy*Iz
    I_2y = Temp*Ix*I_2y*Iz
    I_2z = Temp*Ix*Iy*I_2z

    KIx = -0.5d0*Lb(1)*(Lb(1)-1)*I_2x + b*(2*Lb(1)+1)*S - 2.0d0*b**2*I2x
    KIy = -0.5d0*Lb(2)*(Lb(2)-1)*I_2y + b*(2*Lb(2)+1)*S - 2.0d0*b**2*I2y
    KIz = -0.5d0*Lb(3)*(Lb(3)-1)*I_2z + b*(2*Lb(3)+1)*S - 2.0d0*b**2*I2z
    !-------------------------------------------------------------------
   
    Ki = KIx + KIy + KIz

end subroutine Overlap_Kinetic_GE
!=======================================================================
subroutine N_E_Attractive_GE(a,b,La,Lb,Ra,Rb,Rn,Natm,Nuc_Char,S)

    implicit none
    integer La(3),Lb(3),ii,i,j,k,Natm,Lmax
    integer L_max,L1(3),L2(3),Lx,Ly,Lz,Nuc_Char(Natm)
    real(kind=8),parameter::Pi = 3.141592653589793d0
    real(kind=8) ,allocatable :: Zero(:),F(:)
    real(kind=8) a,b,p,AB,T,S
    real(kind=8) K0,Rp(3),PA(3),PB(3),PC(3),R1(3),R2(3)
    real(kind=8) RPC,w,Gx(0:4),Gy(0:4),Gz(0:4)
    real(kind=8) Rn(3,Natm),Ra(3),Rb(3),Rc(3)
    real(kind=8) Temp1
    
    call GTO_GE(a,b,Ra,Rb,K0,AB,Rp,PA,PB)
    
    L_max = sum(La(:)) + sum(Lb(:))
    Lmax = max( sum(La(:)) , sum(Lb(:)) )
    allocate(Zero(0:L_Max),F(0:L_Max))
    p = a + b
    
    S = 0.0d0    
    !============================== S ==================================
    if( L_max == 0 ) then
    
        do ii = 1,Natm  
            Rc = Rn(:,ii)
            PC = - Rc + Rp
            RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
            w = RPC*p
            call Boys_Gen(0,w,F(0))
            S = S + Nuc_Char(ii)*F(0)*K0*2.0d0*Pi/p            
        end do
        
        return
        
    end if
    !============================ S,P,D ================================
    if( L_max <= 4 .and. Lmax < 3 ) then
    
        Lx = La(1) + Lb(1) ; Ly = La(2) + Lb(2) ; Lz = La(3) + Lb(3) 
        T = 1.0d0/(a+b)
        !---------------------------------------------------------------
        if( Lx == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
        
                call G_Func(la(2),lb(2),PA(2),PB(2),PC(2),T,Gy(0:Ly))      
                call G_Func(la(3),lb(3),PA(3),PB(3),PC(3),T,Gz(0:Lz))
        
                
                Temp1 = 0.0d0
                do j = 0,Ly
                   do k = 0,Lz
                       Temp1 = Temp1 + Gy(j)*Gz(k)*F(j+k)
                   end do
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !---------------------------------------------------------------
        if( Ly == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
               
                call G_Func(la(1),lb(1),PA(1),PB(1),PC(1),T,Gx(0:Lx))
                call G_Func(la(3),lb(3),PA(3),PB(3),PC(3),T,Gz(0:Lz))        
                
                Temp1 = 0.0d0
                do j = 0,Lx
                   do k = 0,Lz
                       Temp1 = Temp1 + Gx(j)*Gz(k)*F(j+k)
                   end do
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !---------------------------------------------------------------
        if( Lz == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
                
                call G_Func(la(1),lb(1),PA(1),PB(1),PC(1),T,Gx(0:Lx))
                call G_Func(la(2),lb(2),PA(2),PB(2),PC(2),T,Gy(0:Ly))            
                
                Temp1 = 0.0d0
                do j = 0,Lx
                   do k = 0,Ly
                       Temp1 = Temp1 + Gx(j)*Gy(k)*F(j+k)
                   end do
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !---------------------------------------------------------------
        if( Lx + Ly == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
             
                call G_Func(la(3),lb(3),PA(3),PB(3),PC(3),T,Gz(0:Lz))        
                
                Temp1 = 0.0d0
                do k = 0,Lz
                    Temp1 = Temp1 + Gz(k)*F(k)
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !---------------------------------------------------------------
        if( Ly + Lz == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
               
                call G_Func(la(1),lb(1),PA(1),PB(1),PC(1),T,Gx(0:Lx))  
                
                Temp1 = 0.0d0
                do j = 0,Lx
                    Temp1 = Temp1 + Gx(j)*F(j)
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !---------------------------------------------------------------
        if( Lz + Lx == 0 ) then 
            S = 0.0d0
            do ii = 1,Natm
    
                Rc = Rn(:,ii)
                PC = - Rc + Rp
                RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
                w = RPC*(a+b)
        
                do i = 0,L_max
                    call Boys_Gen(i,w,F(i))
                end do
                
                call G_Func(la(2),lb(2),PA(2),PB(2),PC(2),T,Gy(0:Ly))            
                
                Temp1 = 0.0d0
                do k = 0,Ly
                    Temp1 = Temp1 + Gy(k)*F(k)
                end do
                
                S = S + Temp1*Nuc_Char(ii)
        
            end do
            S = S*K0*2.0d0*Pi/(a+b)
            return
        end if
        !------------------------- General -----------------------------
        S = 0.0d0
        do ii = 1,Natm
    
            Rc = Rn(:,ii)
            PC = - Rc + Rp
            RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
            w = RPC*(a+b)
        
            do i = 0,L_max
                call Boys_Gen(i,w,F(i))
            end do
        
            call G_Func(la(1),lb(1),PA(1),PB(1),PC(1),T,Gx(0:Lx))
            call G_Func(la(2),lb(2),PA(2),PB(2),PC(2),T,Gy(0:Ly))      
            call G_Func(la(3),lb(3),PA(3),PB(3),PC(3),T,Gz(0:Lz))
        
            !-----------------------------------------------------------
            Temp1 = 0.0d0
            do i = 0,Lx
               do j = 0,Ly
                  do k = 0,Lz
                     Temp1 = Temp1 + Gx(i)*Gy(j)*Gz(k)*F(i+j+k)
                  end do
               end do
            end do
            !-----------------------------------------------------------
            S = S + Temp1*Nuc_Char(ii)
        
        end do
    
        S = S*K0*2.0d0*Pi/(a+b)
        
        return
        
    end if
    !============================ General ==============================
    l1 = La ; l2 = Lb
    R1 = Ra ; R2 = Rb 
    
    if( La(1) < Lb(1) ) then
        l1(1) = Lb(1)
        l2(1) = La(1)
        R1(1) = Rb(1)
        R2(1) = Ra(1)
    end if
    if( La(2) < Lb(2) ) then
        l1(2) = Lb(2)
        l2(2) = La(2)
        R1(2) = Rb(2)
        R2(2) = Ra(2)
    end if
    if( La(3) < Lb(3) ) then
        l1(3) = Lb(3)
        l2(3) = La(3)
        R1(3) = Rb(3)
        R2(3) = Ra(3)
    end if
    
    do ii = 1,Natm
    
        Rc = Rn(:,ii)
        PC = - Rc + Rp
        RPC = (Rp(1)-Rc(1))**2+(Rp(2)-Rc(2))**2+(Rp(3)-Rc(3))**2
        w = RPC*p
        
        do i = 0,L_max
            call Boys_Gen(i,w,F(i))
            Zero(i) = F(i)
        end do
        
        !---------------------------------------------------------------
        call Nuclei_rec(L1(1),L2(1),L_max,p,R1(1),Rp(1),R2(1),Rc(1),Zero,.true.)
        call Nuclei_rec(L1(2),L2(2),L_max,p,R1(2),Rp(2),R2(2),Rc(2),Zero,.true.)
        call Nuclei_rec(L1(3),L2(3),L_max,p,R1(3),Rp(3),R2(3),Rc(3),Zero,.false.)
        Temp1 = Zero(0)       
        !---------------------------------------------------------------
        S = S + Temp1*Nuc_Char(ii)

    end do

    S = S*K0*2.0d0*Pi/p

end subroutine N_E_Attractive_GE
!=======================================================================
subroutine Nuclei_rec(La,Lb,M0,p,Ra,Rp,Rb,Rc,Zero,logc)
    !  DOI : 10.1063/1.450106
    implicit none
    integer La,Lb,i,j,m,M0
    real(kind=8) Temp1(0:La+Lb+1,0:M0),Zero(0:M0)
    real(kind=8) Ra,Rp,Rb,Rc,p,Temp2(0:La+Lb+1,0:Lb)
    logical logc
    
    if ( M0 == 0 ) return
    if ( La + Lb == 0 ) return
    
    Temp1(0,M0) = Zero(M0)
    Temp1(1,M0) = (Rp-Ra)*Zero(M0)
    
    !===================================================================
    if ( M0 == 1 )then
          
        Temp1(1,0) = (Rp-Ra)*Zero(0) - (Rp-Rc)*Zero(1)  
                     
        Zero(0:1) = Temp1(1,0:1)                    
        return             
        
    end if
    !-------------------------------------------------------------------
    if( La + Lb == 1 ) then
          
        do m = M0 - 1 ,0,-1       
            Temp1(0,m) = Zero(m)
            Temp1(1,m) = (Rp-Ra)*Zero(m) - (Rp-Rc)*Zero(m+1) 
        end do       
        
        if( Lb == 0 ) then
        
            if( .not. logc ) then
                Zero(0) = Temp1(1,0) 
                return
            end if
            
            do m = 0,M0    
                Zero(m) = Temp1(1,m)       
            end do  
            return
              
        end if !because La > Lb.
        
    end if
    !============================ General ==============================
    do i = 1,La+Lb
        Temp1(i+1,M0) = i/(2*p)*Temp1(i-1,M0) + (Rp-Ra)*Temp1(i,M0)
    end do  
            
    do m = M0 - 1 ,0,-1   
    
        Temp1(0,m) = Zero(m)
        Temp1(1,m) = (Rp-Ra)*Zero(m) - (Rp-Rc)*Zero(m+1)  
        
        do i = 1,La+Lb
            Temp1(i+1,m) = i/(2*p)*Temp1(i-1,m) + (Rp-Ra)*Temp1(i,m) &
                         - (Rp-Rc)*Temp1(i,m+1) - i/(2*p)*Temp1(i-1,m+1)
        end do        
    end do  
    !===================================================================
    if( Lb == 0 ) then
        if( logc ) then
            Zero(0:M0) = Temp1(La,0:M0)       
            return 
        else
            Zero(0) = Temp1(La,0) 
            return
        end if 
    end if
    !-------------------------------------------------------------------
    if( Lb == 1 ) then
        if( logc ) then
            do m = 0,M0  
                
                do i = 0,La + 2
                    Temp2(i,0) = Temp1(i,m)
                end do
                
                Temp2(La+1,1) = Temp2(La+2,0) + (Ra-Rb)*Temp2(La+1,0)
                Zero(m) = Temp2(La+1,0) + (Ra-Rb)*Temp2(La,0)
                     
            end do  
            return 
        else           
            do i = 0,La + 2
                Temp2(i,0) = Temp1(i,0)
            end do
                
            Temp2(La+1,1) = Temp2(La+2,0) + (Ra-Rb)*Temp2(La+1,0)
            Zero(0) = Temp2(La+1,0) + (Ra-Rb)*Temp2(La,0)
            return
        end if  
    end if
    !===================================================================
    if( logc ) then
    
        do m = 0,M0
    
            do i = 0,La + Lb + 1
                Temp2(i,0) = Temp1(i,m)
            end do
    
            do j = 1,Lb
                do i = La + Lb,La,-1 
                    Temp2(i,j) = Temp2(i+1,j-1) + (Ra-Rb)*Temp2(i,j-1)
                end do
            end do
    
            Zero(m) = Temp2(La,Lb)
        
        end do
        
    else
    
        do i = 0,La + Lb + 1
            Temp2(i,0) = Temp1(i,0)
        end do
    
        do j = 1,Lb
            do i = La + Lb,La,-1 
                Temp2(i,j) = Temp2(i+1,j-1) + (Ra-Rb)*Temp2(i,j-1)
            end do
        end do
    
        Zero(0) = Temp2(La,Lb)
        
    end if
    
end subroutine Nuclei_rec
!=======================================================================
subroutine G_Func(l1,l2,a1,b1,c,T,G)

    implicit none
    integer l1,l2,L
    real(kind=8) a,b,a1,b1,c,T,G(0:l1+l2)
    L = l1 + l2
    
    if( l1 < l2 ) then
        a = b1
        b = a1
    else
        a = a1
        b = b1
    end if
    
    select case(L)
    
        case(0)
            G(0) = 1.0d0
        case(1)
            G(0) = a
            G(1) = -c
        case(2)
            if(l1 == l2) then
                G(0) = a*b + T*0.5
                G(1) = -(a+b)*c - T*0.5d0
                G(2) = c*c
            else
                G(0) = a*a + T*0.5
                G(1) = -2*a*c - T*0.5d0
                G(2) = c*c
            end if
        case(3)
            G(0) = a*a*b + (2*a + b)*T*0.5
            G(1) = -(a*a + 2*a*b)*c - (2*a + b)*T*0.5d0 - 1.5d0*T*c
            G(2) = (2*a+b)*c*c + 1.5d0*T*c
            G(3) = -c*c*c
        case(4)
            G(0) = a*a*b*b + (a*a + 4*a*b + b*b)*T*0.5d0 + 0.75d0*T*T
            G(1) = -2*(a*a*b + a*b*b)*c - (a*a + 4*a*b + b*b)*T*0.5d0 - 3*(a + b)*c*T - 1.5d0*T*T
            G(2) = (a*a + 4*a*b + b*b)*c*c + 3*c*c*T + 3*(a+b)*c*T + 0.75d0*T*T
            G(3) = -2*(a+b)*c*c*c - 3*c*c*T
            G(4) = c**4
    end select
    
    return
    
end subroutine G_Func
!=======================================================================
subroutine Boys_Gen(m,w,F)

    implicit none
    integer m,i
    real(kind=8) w,F,t
    real(kind=8) F_T(0:m)
    
    if( w < 1.0d-10 ) then
       F = 1.0d0/(2*m+1)
       return
    end if
    
    t = dsqrt(w)
    F = 0.88622692545275801d0*derf(t)/t
    if( m == 0 ) return
    
    F_T(0) = F
    do i = 1,m
       F_T(i) = 0.5d0/w*( (2*i-1)*F_T(i-1) - dexp(-w) )
    end do
    
    F = F_T(m)
          
end subroutine Boys_Gen
!=======================================================================
subroutine GTO_GE(a,b,Ra,Rb,K,AB,Rp,PA,PB) 

    implicit none
    integer i
    real(kind=8) a,b,K
    real(kind=8) Ra(3),Rb(3),Rp(3),PA(3),PB(3)
    real(kind=8) AB,cosi(3)
    
    AB = (Ra(1)-Rb(1))**2+(Ra(2)-Rb(2))**2+(Ra(3)-Rb(3))**2
    
    if( AB < 1.0d-12 ) then
       K = 1.0d0
       PA = 0.0d0
       PB = 0.0d0
       Rp = Ra
       return
    end if
    
    K = dexp(-a*b/(a+b)*AB)
    
    do i = 1,3
         Rp(i) = (Ra(i)*a + Rb(i)*b)/(a+b) 
         cosi(i) = (Rb(i)-Ra(i))
         PA(i) = (b/(a+b))*cosi(i)
         PB(i) = -(a/(a+b))*cosi(i)
    end do   
    
end subroutine GTO_GE
!=======================================================================
subroutine Bas_Normal(a,l,m,n,BN)
    
    implicit none
    integer l,m,n,lmn
    real(kind=8) L2,M2,N2
    real(kind=8),parameter::Pi = 3.141592653589793d0
    real(kind=8) a,BN
    
    lmn = l + m + n
    
    if( lmn == 0 ) then
        BN = 0.7127054703549902d0*a**0.75
        return
    end if
    
    if( lmn == 1) then
        BN = 1.4254109407099803d0*a**1.25
        return
    end if
    
    if( lmn == 2) then
        if( l == 2 .or. m == 2 .or. n == 2 ) then
            BN = 1.64592278064948966d0*a**1.75
            return
        else
            BN = 2.85082188141996064d0*a**1.75
            return
        end if
    end if
    
    if( lmn > 2 ) then
    
        L2 = DobF(2*l-1)
        M2 = DobF(2*m-1)
        N2 = DobF(2*n-1)
    
        BN = (2.0d0*a/Pi)**(0.75)*dsqrt((4.0d0*a)**(l+m+n)/(real(L2*M2*N2,8)))
        return
        
    end if
    
end subroutine Bas_Normal
!=======================================================================
real(kind=8) function DobF(n)

    implicit none
    integer n, i
    real(kind=8) iTemp

    iTemp = 1.d0
    if (mod(n,2)==0) then
        do i = 1, n/2
            iTemp = iTemp*2.d0*real(i,8)
        end do
    else
        do i = 1, (n+1)/2
            iTemp = iTemp*(2.d0*real(i,8)-1.d0)
        end do
    end if
    DobF = iTemp
    
end function DobF
!=======================================================================
integer(kind=8) function Comb(m,n)

    implicit none
    integer m, n, i
    integer(kind=8) isum

    isum = 1
    do i = m - n + 1, m
        isum = isum*i
    end do
    Comb = isum/Fact(n)

end function Comb
!=======================================================================
integer(kind=8) function Fact(n)

    implicit none
    integer n, i
    integer(kind=8) isum

    isum = 1
    do i = 2, n
        isum = isum*i
    end do
    fact = isum

end function Fact
!=======================================================================
end submodule