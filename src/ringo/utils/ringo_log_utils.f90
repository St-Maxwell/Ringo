module ringo_log_utils
    implicit none
    private
    public :: print_header, print_bar

contains

    subroutine print_header(header, unit)
        character(len=*), intent(in) :: header
        integer, intent(in) :: unit

        write (unit, "(A)") " ==> "//trim(header)//" <=="

    end subroutine print_header

    subroutine print_bar(unit)
        integer, intent(in) :: unit

        write (unit, "(A)") repeat('-', 80)

    end subroutine print_bar

end module ringo_log_utils
