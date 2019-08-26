module m_definition
  use m_const
  use m_tools
  implicit none
  private
  public :: Molecule, BasisSet, BasisFunction

  type :: Molecule
    integer :: num_atoms
    integer, dimension(:), allocatable :: atom_charges
    real(kind=r8), dimension(:,:), allocatable :: geoms
    integer :: charge
    integer :: spin
    character(len=20) :: basis_set
    character(len=5) :: point_group

    contains
      procedure :: read_charge_spin => Molecule_read_charge_spin
      procedure :: read_geometry => Molecule_read_geometry
  end type

  type :: BasisSet
  ! gtf_type
  ! 'C' = Cartesian
  ! 'S' = Sphere
    character(len=1) :: gtf_type
    type(BasisFunction), dimension(:), allocatable :: basis
    ! 0 -> s, 1 -> p, -1 -> sp, 2 -> d,...
    integer, dimension(:), allocatable :: shell_type
    integer, dimension(:,:), allocatable :: basis_in_shell
  end type

  type :: BasisFunction
    character(len=2) :: bs_type ! basis function type: s, p, sp, d,...
    real(kind=r8), dimension(:,:), allocatable :: geoms
    integer :: lx, ly, lz
    real(kind=r8), dimension(:), allocatable :: exponent
    real(kind=r8), dimension(:), allocatable :: coefficient
  end type

  contains

  subroutine Molecule_read_charge_spin(this, iounit)
    class(Molecule), intent(out) :: this
    integer, intent(in) :: iounit
    integer :: iflag
    integer :: tmp_chrg, tmp_spin

    call locate_label(iounit, "geometry", iflag)
    if (iflag /= 0) call terminate(output_file_unit, "Geometry Block Not Found")

    read(input_file_unit, *)

    read(iounit, *) tmp_chrg, tmp_spin
    if(tmp_spin <= 0) call terminate(output_file_unit, "Unreasonable Spin!")
    this%charge = tmp_chrg
    this%spin = tmp_spin

  end subroutine

  subroutine Molecule_read_geometry(this, iounit)
    class(Molecule), intent(out) :: this
    integer, intent(in) :: iounit
    !---------------------------------------------------------------------------
    character(len=80) :: char_tmp
    character(len=2) :: element_tmp
    integer :: num_ele
    integer :: iflag
    integer :: iter
    
    call locate_label(iounit, "geometry", iflag)
    if (iflag /= 0) call terminate(output_file_unit, "Geometry Block Not Found")
    
    iter = 0
    do while (.true.)
      read(iounit, '(A80)', iostat=iflag) char_tmp
      if (iflag /= 0) call terminate(output_file_unit, "Unpaired Geometry Block")
      if (index(adjustl(char_tmp), "}") == 1) exit
      iter = iter + 1
    end do

    allocate(this%atom_charges(iter-1))
    allocate(this%geoms(3,iter-1))

    call locate_label(iounit, "geometry", iflag)
    read(iounit, *)
    read(iounit, *)

    do iter = 1, size(this%atom_charges)
      read(iounit, *) element_tmp, this%geoms(:,iter)
      this%atom_charges(iter) = findloc(element_list, element_tmp, dim=1)
    end do

    !---------------------------------------------------------------------------
    ! check whether charge and spin can match
    num_ele = sum(this%atom_charges) - this%charge
    if (mod(num_ele, 2) == 0 .and. mod(this%spin, 2) == 0) &
      call terminate(output_file_unit, "charge and spin can not match")
    if (mod(num_ele, 2) /= 0 .and. mod(this%spin, 2) /= 0) &
      call terminate(output_file_unit, "charge and spin can not match")

  end subroutine

  subroutine locate_element_basis(iounit, element, iflag)
    integer, intent(in) :: iounit
    character(len=2), intent(in) :: element
    integer, intent(out) :: iflag
    character(len=20) :: char_tmp

    rewind(iounit)
    do while (.true.)
      read(iounit, '(A20)', iostat=iflag) char_tmp
      if (iflag /= 0) return
      if (index(adjustl(char_tmp), element) == 1 .and. &
          str_word_count(char_tmp) == 2) exit
    end do
    backspace(iounit)

  end subroutine

  subroutine read_basis_set(this, basis_name)
    type(BasisSet), intent(out) :: this
    character(len=10), intent(in) :: basis_name
    character(len=:), allocatable :: basis_file
    logical :: file_exist

    basis_file = "/basis/" // trim(basis_name) // ".gbs"

    inquire(file=basis_file, exist=file_exist)
    if (.not. file_exist) call terminate(output_file_unit, "Basis Set Not Found ")

    open(unit=basis_set_file_unit, file=basis_file, status='old', action='read')
    

  end subroutine

















end module