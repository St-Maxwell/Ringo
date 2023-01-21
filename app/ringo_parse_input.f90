module ringo_parse_input
    use ringo_config
    use ringo_mole_detail
    use ringo_input_sections
    use ringo_parse_section
    use ringo_env
    use ringo_log_utils
    use machina_vla
    use machina_string
    use machina_error
    implicit none
    private
    public :: parse_input

contains

    subroutine parse_input(config, mole, error)
        type(config_t), intent(out) :: config
        type(mole_t), intent(out) :: mole
        type(error_t), intent(out) :: error

        !> print the whole input
        call print_header("Input File", std_out)
        call print_bar(std_out)
        write (std_out, "(A)") raw_input_file
        call print_bar(std_out)
        write (std_out, "(A)")

        !> parse
        call parse_input_sections(raw_input_file, sections, error)
        if (.has.error) return

        !> parse $set
        call parse_set_section(config, error)
        if (.has.error) return

        !> parse $molecule
        call parse_molecule_section(mole, error)
        if (.has.error) return

    end subroutine parse_input

    subroutine parse_set_section(config, error)
        type(config_t), intent(out) :: config
        type(error_t), intent(out) :: error
        type(section_t), pointer :: ptr
        type(vla_char) :: lines
        character(len=:), allocatable :: key, val
        integer :: int_val
        integer :: i

        call set_default_config(config)

        ptr => get_section(sections, "set")
        if (.not. associated(ptr)) then
            call raise_error(error, "$set section is not found")
            return
        end if

        call tokenize(ptr%content, lines, LF)
        do i = 1, size(lines)
            call get_key_scalar_value(lines%at(i), key, val, error)
            if (.has.error) return
            select case (key)
            case ("method")
                config%method = to_lower(val)
            case ("basis")
                config%basis = to_lower(val)
            case ("jobtype")
                config%jobtype = to_lower(val)
            case ("verbose")
                call string_to_int(val, int_val, error)
                if (.has.error) return
                config%verbose = int_val
            case default
                call raise_error(error, "Unknown key """//key//""" found in $set"// &
                                      & "  | "//lines%at(i))
                return
            end select
        end do

    end subroutine parse_set_section

    subroutine parse_molecule_section(mole, error)
        type(mole_t), intent(out) :: mole
        type(error_t), intent(out) :: error
        type(section_t), pointer :: ptr
        character(len=:), allocatable :: line1, lines
        type(vla_char) :: tokens
        integer :: charge, mult
        integer :: i

        ptr => get_section(sections, "molecule")
        if (.not. associated(ptr)) then
            call raise_error(error, "$molecule section is not found")
            return
        end if

        ! split the first line and the rest lines
        i = index(ptr%content, LF)
        if (i == 0) then
            call raise_error(error, "$molecule section is incomplete")
            return
        end if

        line1 = ptr%content(:i - 1)
        lines = ptr%content(i + 1:)

        ! parse the first line
        call tokenize(line1, tokens)
        if (size(tokens) /= 2) then
            call raise_error(error, "Syntax error in $molecule section"// &
                                  & "  | "//line1)
            return
        end if

        call string_to_int(tokens%at(1), charge, error)
        if (.has.error) return
        call string_to_int(tokens%at(2), mult, error)
        if (.has.error) return

        call construct_mole(mole, lines, charge, mult, error)

    end subroutine parse_molecule_section

end module ringo_parse_input
