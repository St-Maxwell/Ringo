module m_tools
  implicit none
  
  contains
  subroutine locate_label(iounit, label, iflag)
    integer, intent(in) :: iounit
    character(len=*), intent(in) :: label
    integer, intent(out) :: iflag
    character(len=80) :: char_tmp

    rewind(iounit)
    do while (.true.)
      read(iounit, '(A80)', iostat=iflag) char_tmp
      if (iflag /= 0) return
      if (index(adjustl(char_tmp), label) == 1) exit
    end do
    backspace(iounit)

  end subroutine

  function str_word_count(s)
    !*****************************************************************************80
    !  STR_WORD_COUNT counts the number of "words" in a string.
    !  https://github.com/stewmasterj/stringParseMods
    !  Licensing:
    !    This code is distributed under the GNU LGPL license.
    !  Modified:
    !    14 April 1999, 30 May 2012
    !  Author:
    !    John Burkardt
    !  Made into a function by Ross J. Stewart
    !  Parameters:
    !    Input, character ( len = * ) S, the string to be examined.
    !    Output, integer ( kind = 4 ) NWORD, the number of "words" in the string.
    !    Words are presumed to be separated by one or more blanks.
    logical :: blank
    integer :: i
    integer :: lens
    integer :: str_word_count
    character(len=*) :: s
    character(len=19) :: delimiters
    
    delimiters = " ,;:()[]{}'=*%" // char(09) // char(34)
    str_word_count = 0
    lens = len(s)
    
    if (lens <= 0) return
    
    blank = .true.
    
    do i = 1, lens
      if (index(delimiters, s(i:i)) /= 0) then
        blank = .true.
      else if (blank) then
        str_word_count = str_word_count + 1
        blank = .false.
      end if
    enddo
    
  end function

  function s_get_word(n, s, attr)
    ! Ross J. Stewart, 01 June 2012
    ! https://github.com/stewmasterj/stringParseMods
    logical :: blank, dquo, squo
    integer, intent(in) :: n
    integer :: lens, i, m
    integer :: s_word_count
    character, optional, intent(out) :: attr
    character(len=*), intent(in) :: s
    character(len=:), allocatable :: s_get_word
    character(len=18) :: delimiters
    character(len=5) :: obrk, cbrk
    
    obrk="([{'"//char(34)
    cbrk=")]}'"//char(34)
    delimiters=" ,:;=%"//char(09)//obrk//cbrk
    s_word_count = 0
    lens = len( trim(s) )
    s_get_word = ""
    
    if (lens <= 0) Return
    if (present(attr)) attr = " "
    
    blank = .true.
    dquo = .false.
    squo = .false.
    
    do i = 1, lens
      if ( index(delimiters,s(i:i)).ne.0 ) then
        blank = .true.
        if (n.eq.s_word_count) Return
      elseif ( blank ) then
        s_word_count = s_word_count + 1
        blank = .false.
      endif
      if (.not.blank.and.n.eq.s_word_count) then
        s_get_word=trim(s_get_word)//s(i:i)
      endif
    
      if (present(attr).and.index(obrk,s(i:i)).ne.0 ) then
        attr=s(i:i) !set attr to the open bracket type
        if (.not.dquo.and.attr.eq.char(34)) dquo = .true.
        if (.not.squo.and.attr.eq."'") squo = .true.
      elseif (present(attr).and.index(cbrk,s(i:i)).ne.0 ) then
        m=index(cbrk,s(i:i))
        if (attr.eq.obrk(m:m)) then ! closing bracket
           attr= " " 
        if (attr.eq.char(34)) dquo = .false.
        if (attr.eq."'") squo = .false.
        end if
      endif
    enddo
    
  end function

  subroutine terminate(iounit, error_msg)
    integer, intent(in) :: iounit
    character(len=*), intent(in) :: error_msg

    write(iounit,"(A)") "======================= ERROR ======================="
    write(iounit,"(A)") error_msg
    write(iounit,"(A)") "====================== JOB DIED ====================="

    ! call clock ! time stamp

    stop "Error Occurred! Check Output File for Details."

  end subroutine

end module