program main
    use mod_io
    use mod_mol
    use mod_basis
    use iso_fortran_env, only: r8 => real64
    implicit none
    integer :: stdunit
    type(Molecule) :: mole
    type(BasisSet) :: basis
    character(len=25), dimension(3) :: filename  = ["Hydrogen_Fluoride.inp", &
                                                    "water.inp            ", &
                                                    "Argon.inp            "]
    character(len=20), dimension(3) :: basisname = ["6-31G   " , &
                                                    "def2-SVP", &
                                                    "cc-pVTZ "]
    integer :: i, j, k

    open(newunit=stdunit, file="test.txt", status="new")

    do k = 1, 3

        write(stdunit,*) trim(filename(k)), ' / ', trim(basisname(k))
        call mole%init( trim(filename(k)) )
        call basis%init(mole, trim(basisname(k)))
        write(stdunit, "(A,I4)") "Number of Basis Function:", basis%num_basis
        write(stdunit, "(A,I4)") "Number of Shell:", basis%num_shell
        write(stdunit, "(A,I4)") "Number of Primitive Gaussian:", basis%num_gto
        write(stdunit, "(A,I4)") "Maximum Angular Momentum:", basis%max_angl
        write(stdunit, "(A,*(I4))") "Number of Basis Functions in Shell:", basis%n_bf_in_shl
        write(stdunit, "(A,*(I4))") "Shell's Indecies:", basis%shl_idx
        
        do i = 1, basis%num_basis
            write(stdunit,*) "======================================================="
            write(stdunit, "(A,I4)") "Contraction Order:", basis%basis(i)%ctr_order
            write(stdunit, "(A,3F11.6)") "Centering at:", basis%basis(i)%center
            write(stdunit, "(A,I4)") "Angular Momentum:", basis%basis(i)%angl
            write(stdunit,*) "   Exponent       Coefficient  "
            do j = 1, basis%basis(i)%ctr_order
                write(stdunit, "(2ES15.6)") basis%basis(i)%gto(j)%alpha, basis%basis(i)%ctr_coeff(j)
            end do
        end do
        write(stdunit,*) 
        write(stdunit,*) 
        
        call mole%destroy()
        call basis%destroy()
        
    end do

    close(stdunit)

end program main