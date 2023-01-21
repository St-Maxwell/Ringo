module machina_set
    use machina_vla
    implicit none
    private
    public :: unique

    interface unique
        module procedure :: unique_int_vla
        module procedure :: unique_real_vla
        module procedure :: unique_cmplx_vla
        module procedure :: unique_char_vla
        module procedure :: unique_bool_vla
    end interface

contains

    subroutine unique_int_vla(array, set)
        type(vla_int), intent(in) :: array
        type(vla_int), intent(out) :: set
        logical :: all_unique
        integer :: i, j

        if (size(array) == 0) return

        call set%push_back(array%ptr_at(1))

        if (size(array) == 1) return

        do i = 2, size(array)
            all_unique = .true.
            do j = 1, size(set)
                if (array%ptr_at(i) == set%ptr_at(j)) then
                    all_unique = .false.
                    exit
                end if
            end do
            if (all_unique) call set%push_back(array%ptr_at(i))
        end do

    end subroutine unique_int_vla

    subroutine unique_real_vla(array, set)
        type(vla_real), intent(in) :: array
        type(vla_real), intent(out) :: set
        logical :: all_unique
        integer :: i, j

        if (size(array) == 0) return

        call set%push_back(array%ptr_at(1))

        if (size(array) == 1) return

        do i = 2, size(array)
            all_unique = .true.
            do j = 1, size(set)
                if (array%ptr_at(i) == set%ptr_at(j)) then
                    all_unique = .false.
                    exit
                end if
            end do
            if (all_unique) call set%push_back(array%ptr_at(i))
        end do

    end subroutine unique_real_vla

    subroutine unique_cmplx_vla(array, set)
        type(vla_cmplx), intent(in) :: array
        type(vla_cmplx), intent(out) :: set
        logical :: all_unique
        integer :: i, j

        if (size(array) == 0) return

        call set%push_back(array%ptr_at(1))

        if (size(array) == 1) return

        do i = 2, size(array)
            all_unique = .true.
            do j = 1, size(set)
                if (array%ptr_at(i) == set%ptr_at(j)) then
                    all_unique = .false.
                    exit
                end if
            end do
            if (all_unique) call set%push_back(array%ptr_at(i))
        end do

    end subroutine unique_cmplx_vla

    subroutine unique_char_vla(array, set)
        type(vla_char), intent(in) :: array
        type(vla_char), intent(out) :: set
        logical :: all_unique
        integer :: i, j

        if (size(array) == 0) return

        call set%push_back(array%ptr_at(1))

        if (size(array) == 1) return

        do i = 2, size(array)
            all_unique = .true.
            do j = 1, size(set)
                if (array%ptr_at(i) == set%ptr_at(j)) then
                    all_unique = .false.
                    exit
                end if
            end do
            if (all_unique) call set%push_back(array%ptr_at(i))
        end do

    end subroutine unique_char_vla

    subroutine unique_bool_vla(array, set)
        type(vla_bool), intent(in) :: array
        type(vla_bool), intent(out) :: set
        logical :: all_unique
        integer :: i, j

        if (size(array) == 0) return

        call set%push_back(array%ptr_at(1))

        if (size(array) == 1) return

        do i = 2, size(array)
            all_unique = .true.
            do j = 1, size(set)
                if (array%ptr_at(i) .eqv. set%ptr_at(j)) then
                    all_unique = .false.
                    exit
                end if
            end do
            if (all_unique) call set%push_back(array%ptr_at(i))
        end do

    end subroutine unique_bool_vla

end module machina_set
