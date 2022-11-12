module machina_string_ascii
    implicit none

    character(len=1), parameter :: NUL = achar(int(z'00')) !! Null
    character(len=1), parameter :: SOH = achar(int(z'01')) !! Start of heading
    character(len=1), parameter :: STX = achar(int(z'02')) !! Start of text
    character(len=1), parameter :: ETX = achar(int(z'03')) !! End of text
    character(len=1), parameter :: EOT = achar(int(z'04')) !! End of transmission
    character(len=1), parameter :: ENQ = achar(int(z'05')) !! Enquiry
    character(len=1), parameter :: ACK = achar(int(z'06')) !! Acknowledge
    character(len=1), parameter :: BEL = achar(int(z'07')) !! Bell
    character(len=1), parameter :: BS = achar(int(z'08')) !! Backspace
    character(len=1), parameter :: TAB = achar(int(z'09')) !! Horizontal tab
    character(len=1), parameter :: LF = achar(int(z'0A')) !! NL line feed, new line
    character(len=1), parameter :: VT = achar(int(z'0B')) !! Vertical tab
    character(len=1), parameter :: FF = achar(int(z'0C')) !! NP form feed, new page
    character(len=1), parameter :: CR = achar(int(z'0D')) !! Carriage return
    character(len=1), parameter :: SO = achar(int(z'0E')) !! Shift out
    character(len=1), parameter :: SI = achar(int(z'0F')) !! Shift in
    character(len=1), parameter :: DLE = achar(int(z'10')) !! Data link escape
    character(len=1), parameter :: DC1 = achar(int(z'11')) !! Device control 1
    character(len=1), parameter :: DC2 = achar(int(z'12')) !! Device control 2
    character(len=1), parameter :: DC3 = achar(int(z'13')) !! Device control 3
    character(len=1), parameter :: DC4 = achar(int(z'14')) !! Device control 4
    character(len=1), parameter :: NAK = achar(int(z'15')) !! Negative acknowledge
    character(len=1), parameter :: SYN = achar(int(z'16')) !! Synchronous idle
    character(len=1), parameter :: ETB = achar(int(z'17')) !! End of transmission block
    character(len=1), parameter :: CAN = achar(int(z'18')) !! Cancel
    character(len=1), parameter :: EM = achar(int(z'19')) !! End of medium
    character(len=1), parameter :: SUB = achar(int(z'1A')) !! Substitute
    character(len=1), parameter :: ESC = achar(int(z'1B')) !! Escape
    character(len=1), parameter :: FS = achar(int(z'1C')) !! File separator
    character(len=1), parameter :: GS = achar(int(z'1D')) !! Group separator
    character(len=1), parameter :: RS = achar(int(z'1E')) !! Record separator
    character(len=1), parameter :: US = achar(int(z'1F')) !! Unit separator
    character(len=1), parameter :: DEL = achar(int(z'7F')) !! Delete

    ! Constant character sequences
    character(len=*), parameter :: fullhex_digits = "0123456789ABCDEFabcdef" !! 0 .. 9A .. Fa .. f
    character(len=*), parameter :: hex_digits = fullhex_digits(1:16) !! 0 .. 9A .. F
    character(len=*), parameter :: lowerhex_digits = "0123456789abcdef" !! 0 .. 9a .. f
    character(len=*), parameter :: digits = hex_digits(1:10) !! 0 .. 9
    character(len=*), parameter :: octal_digits = digits(1:8) !! 0 .. 7
    character(len=*), parameter :: letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" !! A .. Za .. z
    character(len=*), parameter :: uppercase = letters(1:26) !! A .. Z
    character(len=*), parameter :: lowercase = letters(27:) !! a .. z
    character(len=*), parameter :: whitespace = " "//TAB//VT//CR//LF//FF !! ASCII _whitespace

contains

    !> Checks whether `c` is an ASCII letter (A .. Z, a .. z).
    pure logical function is_alpha(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

    !> Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    pure logical function is_alphanum(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') &
                      .or. (c >= 'A' .and. c <= 'Z')
    end function

    !> Checks whether or not `c` is in the ASCII character set -
    !> i.e. in the range 0 .. 0x7F.
    pure logical function is_ascii(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_ascii = iachar(c) <= int(z'7F')
    end function

    !> Checks whether `c` is a control character.
    pure logical function is_control(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_control = ic < int(z'20') .or. ic == int(z'7F')
    end function

    !> Checks whether `c` is a digit (0 .. 9).
    pure logical function is_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_digit = ('0' <= c) .and. (c <= '9')
    end function

    !> Checks whether `c` is a digit in base 8 (0 .. 7).
    pure logical function is_octal_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_octal_digit = (c >= '0') .and. (c <= '7'); 
    end function

    !> Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    pure logical function is_hex_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_hex_digit = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f') &
                       .or. (c >= 'A' .and. c <= 'F')
    end function

    !> Checks whether or not `c` is a punctuation character. That includes
    !> all ASCII characters which are not control characters, letters,
    !> digits, or whitespace.
    pure logical function is_punctuation(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !       '~'                 '!'
        is_punctuation = (ic <= int(z'7E')) .and. (ic >= int(z'21')) .and. &
                         (.not. is_alphanum(c))
    end function

    !> Checks whether or not `c` is a printable character other than the
    !> space character.
    pure logical function is_graphical(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        !The character is graphical if it's between '!' and '~' in the ASCII table,
        !that is: printable but not a space
        is_graphical = (int(z'21') <= ic) .and. (ic <= int(z'7E'))
    end function
    !> Checks whether or not `c` is a printable character - including the
    !> space character.
    pure logical function is_printable(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        !The character is printable if it's between ' ' and '~' in the ASCII table
        is_printable = ic >= iachar(' ') .and. ic <= int(z'7E')
    end function

    !> Checks whether `c` is a lowercase ASCII letter (a .. z).
    pure logical function is_lower(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_lower = ic >= iachar('a') .and. ic <= iachar('z')
    end function

    !> Checks whether `c` is an uppercase ASCII letter (A .. Z).
    pure logical function is_upper(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_upper = (c >= 'A') .and. (c <= 'Z')
    end function

    !> Checks whether or not `c` is a whitespace character. That includes the
    !> space, tab, vertical tab, form feed, carriage return, and linefeed
    !> characters.
    pure logical function is_white(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB, LF, VT, FF, CR
        is_white = (c == ' ') .or. (ic >= int(z'09') .and. ic <= int(z'0D')); 
    end function

    !> Checks whether or not `c` is a blank character. That includes the
    !> only the space and tab characters
    pure logical function is_blank(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB
        is_blank = (c == ' ') .or. (ic == int(z'09')); 
    end function

    !> Returns the corresponding lowercase letter, if `c` is an uppercase
    !> ASCII character, otherwise `c` itself.
    pure function char_to_lower(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: k

        k = index(uppercase, c)

        if (k > 0) then
            t = lowercase(k:k)
        else
            t = c
        end if
    end function char_to_lower

    !> Returns the corresponding uppercase letter, if `c` is a lowercase
    !> ASCII character, otherwise `c` itself.
    pure function char_to_upper(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: k

        k = index(lowercase, c)

        if (k > 0) then
            t = uppercase(k:k)
        else
            t = c
        end if
    end function char_to_upper

end module machina_string_ascii
