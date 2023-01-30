module ringo_scf_params
    use machina_basic, only: f8
    use machina_vla
    use machina_string
    use machina_error
    use ringo_mole_detail
    use ringo_env
    use ringo_input_sections
    use ringo_parse_section
    use ringo_log_utils
    implicit none
    private
    public :: scf_params_t
    public :: scf_guess, scf_algorithm, scf_orbital
    public :: set_scf_default_params, parse_scf_sections
    public :: show_params

    !> initial guess options
    type :: enum_guess
        integer :: hcore
        integer :: sad
        integer :: sap
    end type

    type(enum_guess), parameter :: scf_guess = enum_guess( &
                                   hcore=0, &
                                   sad=1, &
                                   sap=2 &
                                   )

    !> convergence algorithm options
    type :: enum_algo
        integer :: roothaan
        integer :: diis
    end type

    type(enum_algo), parameter :: scf_algorithm = enum_algo( &
                                  roothaan=0, &
                                  diis=1 &
                                  )

    !> orbital types
    type :: enum_orb
        integer :: rhf
        integer :: uhf
        integer :: rohf
    end type

    type(enum_orb), parameter :: scf_orbital = enum_orb( &
                                 rhf=0, &
                                 uhf=1, &
                                 rohf=2 &
                                 )

    !> options for scf
    type :: scf_params_t
        integer :: max_iter = 100
        integer :: verbose = 0
        integer :: tol = 6 !> 10^-tol
        integer :: guess = -1
        integer :: algo
        !> a remainder that will cause an error if orbital type is not specified
        integer :: orb
    end type

contains

    !> orbital type is not included
    subroutine set_scf_default_params(params)
        type(scf_params_t), intent(out) :: params

        params%guess = scf_guess%hcore
        params%algo = scf_algorithm%roothaan

    end subroutine set_scf_default_params

    !> parse the options in $scf section
    subroutine parse_scf_sections(params, mole, error)
        type(scf_params_t), intent(inout) :: params
        type(mole_t), intent(in) :: mole
        type(error_t), intent(out) :: error
        type(section_t), pointer :: ptr
        type(vla_char) :: lines
        character(len=:), allocatable :: key, val
        integer :: int_val
        logical :: bool_val
        integer :: i

        !> special cases
        logical :: unrestricted, set_unrestricted

        !> set default values
        unrestricted = .false.
        set_unrestricted = .false.

        ptr => get_section(sections, "scf")
        if (.not. associated(ptr)) goto 1000 !> no $scf in input file

        call tokenize(ptr%content, lines, LF)
        do i = 1, size(lines)
            call get_key_scalar_value(lines%at(i), key, val, error)
            if (.has.error) return
            select case (key)
            case ("convergence")
                call string_to_int(val, int_val, error)
                if (.has.error) return
                params%tol = int_val
            case ("guess")
                call string_to_guess(val, params%guess, error)
                if (.has.error) return
            case ("max_cyles")
                call string_to_int(val, int_val, error)
                if (.has.error) return
                if (int_val <= 0) then
                    call raise_error(error, "max_cyles should be a positive number")
                    return
                end if
                params%max_iter = int_val
            case ("unrestricted")
                call string_to_bool(val, bool_val, error)
                if (.has.error) return
                set_unrestricted = .true.
                unrestricted = bool_val
            case ("verbose")
                call string_to_int(val, int_val, error)
                if (.has.error) return
                params%verbose = int_val
            case default
                call raise_error(error, "Unknown key """//key//""" found in $scf"// &
                                      & "  | "//lines%at(i))
                return
            end select
        end do

1000    continue

        !> deal with special cases
        if (set_unrestricted) then
            if (unrestricted) then
                ! unrestricted true
                params%orb = scf_orbital%uhf
            else
                ! unrestricted false
                if (mole%mult == 1) then
                    params%orb = scf_orbital%rhf
                else
                    params%orb = scf_orbital%rohf
                end if
            end if
        else
            params%orb = merge(scf_orbital%rhf, scf_orbital%uhf, mole%mult == 1)
        end if

    end subroutine parse_scf_sections

    subroutine show_params(params, unit)
        type(scf_params_t), intent(in) :: params
        integer, intent(in) :: unit

        call print_header("SCF Job Paramters", unit)
        write (unit, "(/,' Guess : ',A)") guess_to_string(params%guess)
        write (unit, "(' Algorithm : ',A)") algorithm_to_string(params%algo)
        write (unit, "(' Max steps : ',g0)") params%max_iter
        write (unit, "(A)")

    end subroutine show_params

    subroutine string_to_guess(string, guess, error)
        character(len=*), intent(in) :: string
        integer, intent(out) :: guess
        type(error_t), intent(out) :: error

        select case (to_lower(string))
        case ("hcore")
            guess = scf_guess%hcore
        case ("sad")
            guess = scf_guess%sad
        case ("sap")
            guess = scf_guess%sap
        case default
            call raise_error(error, "Unknown guess option """//string//""" found in $scf")
        end select

    end subroutine string_to_guess

    function guess_to_string(guess) result(s)
        integer, intent(in) :: guess
        character(len=:), allocatable :: s

        select case (guess)
        case (scf_guess%hcore)
            s = "hcore"
        case (scf_guess%sad)
            s = "SAD"
        case (scf_guess%sap)
            s = "SAP"
        end select

    end function guess_to_string

    function algorithm_to_string(algo) result(s)
        integer, intent(in) :: algo
        character(len=:), allocatable :: s

        select case (algo)
        case (scf_algorithm%roothaan)
            s = "Roothaan diagonalization"
        case (scf_algorithm%diis)
            s = "DIIS"
        end select

    end function algorithm_to_string

end module ringo_scf_params
