module m_geometry
  use m_const
  use m_tools
  implicit none

  type :: Molecule
    integer :: order = 1 
    integer :: natom ! number of atoms
    real(kind=r8), dimension(:), allocatable :: atom_chg ! nuclear charges
    character(len=2), dimension(:), allocatable :: atom_syb
    ! coordinates of atom (3,natom)
    real(kind=r8), dimension(:,:), allocatable :: atom_coord
    integer :: charge
    integer :: spin ! spin multiplicity
  end type






end module