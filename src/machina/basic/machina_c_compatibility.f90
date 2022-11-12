module machina_c_compatibility
    use iso_c_binding
    implicit none
    private
    public :: c_f_character

contains

    subroutine c_f_character(rhs, lhs)
        character(kind=c_char), intent(in) :: rhs(*)
        character(len=:), allocatable, intent(out) :: lhs

        integer :: ii

        do ii = 1, huge(ii) - 1
            if (rhs(ii) == c_null_char) then
                exit
            end if
        end do
        allocate (character(len=ii - 1) :: lhs)
        lhs = transfer(rhs(1:ii - 1), lhs)

    end subroutine c_f_character

end module machina_c_compatibility
