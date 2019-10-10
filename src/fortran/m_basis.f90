module m_basis
  use m_const
  implicit none

  type :: BasisSet

  end type

  type :: BasisFuncion
    integer :: iatom ! locate at which atom
    real(kind=r8), dimension(3) :: coord ! coordinates
    integer, dimension(3) :: l ! angular momentum
    integer :: ishell ! belong to which shell
    integer :: n_gf ! order of contraction
    type(GaussFunction), dimension(:), allocatable :: gf ! gauss function
    real(kind=r8), dimension(:), allocatable :: cf ! contraction factor
  end type

  type :: BasisShell
    integer :: am ! angular momentum number = lx + ly + lz
    character(len=2) :: amc ! symbol of shell: s, p, sp, d,...
  end type

  type :: GaussFunction
    ! gtf = N * x^i * y^j * z^k * exp( -zeta * r^2 )
    real(kind=r8), dimension(3) :: coord ! coordinates
    integer, dimension(3) :: l ! angular momentum
    real(kind=r8) :: zeta
    real(kind=r8) :: Norm ! N: normalization factor
  end type

end module
