module m_io
  use m_const
  use m_tools
  use m_geometry
  implicit none
  private
  public :: read_input_file

  character(len=80) :: input_file
  character(len=80) :: output_file

  contains

  subroutine get_input_output_name()
    integer :: num_args
    integer :: istat

    num_args = command_argument_count()
    if (num_args == 1) then ! did not specify name of output file

      call get_command_argument(1, input_file, status=istat)
      output_file = input_file(:index(input_file,'.',back=.true.)-1) // '.out'

    else if (num_args == 2) then ! specify name of output file

      call get_command_argument(1, input_file, status=istat)
      call get_command_argument(2, output_file, status=istat)

    end if

    open(unit=output_file_unit, file=trim(output_file), status='replace', &
         action='write', iostat=istat)

  end subroutine

  subroutine load_input_file()
    logical :: alive
    integer :: istat

    call get_input_output_name()

    inquire(file=trim(input_file), exist=alive)
    if (.not. alive) call terminate(output_file_unit, trim(input_file)//' does not exist.')

    open(unit=input_file_unit, file=trim(output_file), status='old', &
         action='read', iostat=istat)

  end subroutine

  subroutine read_input_file(job_type, basis_set, mol)
    character(len=20) :: job_type
    character(len=20) :: basis_set
    type(Molecule) :: mol
    integer :: i

    call load_input_file()
    
    read(input_file_unit,*) job_type
    read(input_file_unit,*) basis_set
    read(input_file_unit,*) mol%charge, mol%spin

    read(input_file_unit,*) mol%natom
    allocate(mol%atom_coord(3,mol%natom))
    allocate(mol%atom_chg(mol%natom), mol%atom_syb(mol%natom))

    do i = 1, mol%natom
      read(input_file_unit,*) mol%atom_syb, mol%atom_coord(:,i)
    end do

  end subroutine

end module