module m_parser
  use m_const
  use m_definition
  use m_tools
  implicit none
  private
  

  contains

  subroutine read_input_file(file_name, mol, basis)
    character(len=*), intent(in) :: file_name
    type(Molecule), intent(out) :: mol
    type(BasisSet), intent(out) :: basis
    !---------------------------------------------------------------------------
    character(len=80) :: char_tmp
    character(len=:), allocatable :: basis_name
    logical :: file_exist
    integer :: iflag

    ! check if input file exist and open it
    inquire(file=file_name, exist=file_exist)
    if (.not. file_exist) call terminate(output_file_unit, "Input File Not Found")
    open(unit=input_file_unit, file=file_name, status='old', action='read')

    ! read molecule geometry, basis set, job type and options
    call mol%read_charge_spin(input_file_unit)
    call mol%read_geometry(input_file_unit)

    call locate_label(input_file_unit, "basis", iflag)
    if (iflag /= 0) call terminate(output_file_unit, "Basis Set Block Not Found")
    read(input_file_unit, '(A80)') char_tmp
    basis_name = s_get_word(2, char_tmp)


  end subroutine












end module