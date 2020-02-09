module mod_scf_main
    use mod_const
    use mod_io
    use mod_mol
    use mod_basis
    use mod_SCF
    use mod_RHF
    implicit none

    interface SCF
        module procedure :: scf_constructor
    end interface

contains
    
    function scf_constructor(mole, basis) result(new_scf)
        class(Molecule), intent(in) :: mole
        class(BasisSet), intent(in) :: basis
        class(SCF), pointer :: new_scf

        select case (SCFJob)
        case(SCFJobType)
            call error_terminate(OUTPUT_FILE_UNIT, "HF type not specified.")
        case(RHFJob)
            allocate(RHF :: new_scf)
        end select

        call new_scf%init(mole, basis)

    end function scf_constructor

    subroutine scf_loop(mole, basis)
        class(Molecule), intent(in) :: mole
        class(BasisSet), intent(in) :: basis
        !===================================================
        class(SCF), pointer :: the_scf
        real(kind=r8), dimension(:,:), allocatable :: P_new, P_old
        real(kind=r8) :: E_new, E_old
        real(kind=r8) :: delta_E, RMS_D
        logical :: conv
        integer :: iter

        the_scf => SCF(mole, basis)

        write(OUTPUT_FILE_UNIT, "(A)") "SCF Start"

        ! initial guess for density matrix
        call the_scf%initGuess()
        write(OUTPUT_FILE_UNIT, "(A)") "SCF Initial Gues"

        ! initial energy
        call the_scf%buildFock()
        call the_scf%diagFock()
        call the_scf%calcDensity()

        E_old = the_scf%totalEnergy()
        P_old = the_scf%getDensity()
        write(OUTPUT_FILE_UNIT, "(A, F15.6)") "SCF Initial Energy: ", E_old

        write(OUTPUT_FILE_UNIT, "(A)") "=============================================="
        write(OUTPUT_FILE_UNIT, "(A)") " Cycle      Energy (a.u.)      Delta E (a.u.) "

        conv = .false.
        do iter = 1, SCF_MAX_ITER

            call the_scf%buildFock()
            call the_scf%diagFock()
            call the_scf%calcDensity()
    
            E_new = the_scf%totalEnergy()
            P_new = the_scf%getDensity()

            delta_E = abs(E_new - E_old)
            RMS_D = RMS(P_new, P_old)

            write(OUTPUT_FILE_UNIT, "(A,I3,A,F15.6,A,F11.7)") "  ", iter, "      ", E_new, "      ", delta_E

            conv = delta_E < 10.**(-SCF_E_CONVERGENCE) .and. &
                   RMS_D < 10.**(-SCF_D_CONVERGENCE)
            if (conv) then
                write(OUTPUT_FILE_UNIT,"(A)") "SCF Converged!"
                exit
            end if

            E_old = E_new; P_old = P_new

        end do

        write(OUTPUT_FILE_UNIT, "(A)") "=============================================="
        
        if (.not. conv) then
            call error_terminate(OUTPUT_FILE_UNIT, "SCF Failed to Converge.")
        end if

        write(OUTPUT_FILE_UNIT, "(A,F15.6)") "SCF Done:", the_scf%totalEnergy()

        deallocate(the_scf)

    end subroutine scf_loop

end module mod_scf_main