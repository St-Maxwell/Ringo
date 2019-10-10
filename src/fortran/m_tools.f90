module m_tools
  implicit none

contains
  subroutine locate_label(iounit, label, iflag)
    integer, intent(in) :: iounit
    character(len=*), intent(in) :: label
    integer, intent(out) :: iflag
    character(len=80) :: char_tmp

    rewind (iounit)
    do while (.true.)
      read (iounit, '(A80)', iostat=iflag) char_tmp
      if (iflag /= 0) return
      if (index(adjustl(char_tmp), label) == 1) exit
    end do
    backspace (iounit)

  end subroutine

  subroutine terminate(iounit, error_msg)
    integer, intent(in) :: iounit
    character(len=*), intent(in) :: error_msg

    write (iounit, "(A)") "======================= ERROR ======================="
    write (iounit, "(A)") error_msg
    write (iounit, "(A)") "====================== JOB DIED ====================="

    ! call clock ! time stamp

    stop "Error Occurred! Check Output File for Details."

  end subroutine

end module
