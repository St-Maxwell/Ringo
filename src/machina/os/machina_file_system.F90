module machina_file_system
    use iso_c_binding
    use machina_c_compatibility
    use machina_error
    implicit none
    private
    public :: get_current_directory


    interface
        function getcwd(buf, bufsize) result(path) &
#ifndef _WIN32
            bind(C, name="getcwd")
#else
            bind(C, name="_getcwd")
#endif
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd
    end interface

contains

    subroutine get_current_directory(path, error)
        character(len=:), allocatable, intent(out) :: path
        type(error_t), intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer(c_int), parameter :: buffersize = 1000_c_int
        type(c_ptr) :: tmp

        allocate(cpath(buffersize))

        tmp = getcwd(cpath, buffersize)
        if (c_associated(tmp)) then
            call c_f_character(cpath, path)
        else
            call set_error(error, os_error(), "Failed to retrieve current directory")
        end if

    end subroutine get_current_directory

end module machina_file_system
