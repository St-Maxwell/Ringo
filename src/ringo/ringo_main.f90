module ringo_main
    implicit none
    
contains

    !> the true starting point of ringo
    subroutine ringo()
        
    end subroutine ringo










    subroutine print_help()
        use iso_fortran_env, only: output_unit
        integer :: i
        character(len=80), dimension(*), parameter :: manual = &
        [character(len=80) :: &
        "Ringo                                                          ", &
        "by St Maxwell                                                  ", &
        "Ringo IS Not Gaussian/Orca                                     ", &
        "                                                               ", &
        "USAGE:                                                         ", &
        "    ringo [options] INFILE [OUTFILE]                           ", &
        "                                                               ", &
        "OPTIONS:                                                       ", &
        "    -nt <num>  Set the number of threads for OpenMP parallel   ", &
        "    -h         Print help information                          ", &
        "                                                               ", &
        "INFILE is the input file of Ringo                              ", &
        "The recommended filename extension is .inp                     ", &
        "                                                               ", &
        "OUTFILE is the output file of Ringo                            ", &
        "If not presented, it will be set INFILE.out                    ", &
        "                                                               "]

        write (output_unit, "(g0)") (trim(manual(i)), i=1, size(manual))

    end subroutine print_help

end module ringo_main