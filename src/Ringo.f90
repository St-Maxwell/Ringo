program main
    use mod_const
    use mod_io
    use mod_mol
    use mod_basis
    use mod_scf
    use mod_scf_main
    implicit none
    type(Molecule) :: the_mol
    type(BasisSet) :: the_basis
    !class(SCF), pointer :: the_scf

    call get_cmd_input()
    call read_job()
    call open_output_file()
    call welcome()

    call the_mol%init(input_file)
    call the_basis%init(the_mol, input_file)

    call scf_loop(the_mol, the_basis)

    call normal_terminate()

end program main