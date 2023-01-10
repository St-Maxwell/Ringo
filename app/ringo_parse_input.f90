module ringo_parse_input
    use ringo_config
    use ringo_input_sections
    use ringo_parse_section
    use ringo_env
    use ringo_log_utils
    use machina_vla
    use machina_string
    use machina_error
    implicit none

contains

    subroutine parse_input(config, error)
        type(config_t), intent(out) :: config
        type(error_t), intent(out) :: error

        !> print the whole input
        call print_header("Input File", std_out)
        call print_bar(std_out)
        write (std_out, "(A)") raw_input_file
        call print_bar(std_out)

        !> parse
        call parse_input_sections(raw_input_file, sections, error)
        if (.has.error) return

        !> parse $set
        call parse_set_section(config, error)
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
                config%method = val
            case ("basis")
                config%basis = val
            case ("jobtype")
                config%jobtype = val
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

end module ringo_parse_input
