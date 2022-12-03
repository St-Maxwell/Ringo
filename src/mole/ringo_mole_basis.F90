!> from https://stackoverflow.com/a/46342008
#ifdef __GFORTRAN__
# define STRINGIFY_START(X) "&
# define STRINGIFY_END(X) &X"
#else /* default stringification */
# define STRINGIFY_(X) #X
# define STRINGIFY_START(X) &
# define STRINGIFY_END(X) STRINGIFY_(X)
#endif

module ringo_mole_basis
    use machina_basic
    use machina_core
    use machina_string
    use machina_map
    use machina_vla
    use machina_error
    implicit none
    private
    public :: basis_set_t, format_basis_set

    character(len=*), parameter :: basis_dir = STRINGIFY_START(PROJECT_ROOT)
    STRINGIFY_END(PROJECT_ROOT) &
        //"/src/mole/basis/"

    !> the internal format of shell in basis set definition
    !> not used for calculation
    type :: basis_set_shell
        !> orbital angular momentum
        integer :: l
        !> contraction coefficients of GTOs
        real(kind=f8), dimension(:), allocatable :: c
        !> exponents of GTOs
        real(kind=f8), dimension(:), allocatable :: e
    end type

    !> the internal format of basis set definition
    !> not used for calculation
    type :: basis_set_t
        !> atom which is assigned the basis set
        character(len=:), allocatable :: a
        !> the definition of basis set
        type(basis_set_shell), dimension(:), allocatable :: shells
    end type

contains

    !> read basis set file, and convert to the internal format
    !> e.g. pass {"H":"3-21G","O":"6-31G*"}
    subroutine format_basis_set(bas_map, basis, error)
        type(map_t), intent(in) :: bas_map
        type(basis_set_t), dimension(:), allocatable, intent(out) :: basis
        type(error_t), intent(out) :: error

        iterate: block
            type(map_iterator) :: it
            character(len=:), allocatable :: atom
            class(machina_value), pointer :: vptr
            type(basis_set_t) :: newbasis
            type(basis_set_shell), dimension(:), allocatable :: shls
            logical :: next

            allocate (basis(0))
            it = bas_map%iterator()
            do
                next = it%has_next()
                if (.not. next) exit
                call it%next_pair(atom, vptr)

                select type (vptr)
                type is (char_value)
                    call load_basis(shls, basis_dir//basis_file_name(vptr%raw), &
                                    to_lower(atom), error)
                    if (.has.error) return
                    basis = [basis, basis_set_t(atom, shls)]
                class default
                    call raise_error(error, "Invalid basis set name")
                    return
                end select

            end do

        end block iterate

    end subroutine format_basis_set

    !> load required basis set from file
    subroutine load_basis(bas_shls, basis_file, atom, error)
        type(basis_set_shell), dimension(:), allocatable, intent(out) :: bas_shls
        character(len=*), intent(in) :: basis_file
        character(len=*), intent(in) :: atom
        type(error_t), intent(out) :: error
        ! local
        character(len=:), allocatable :: str

        call search_segment_from_file(str, basis_file, atom)
        call parse_basis(str, bas_shls, error)

    end subroutine load_basis

    subroutine search_segment_from_file(raw_str, basis_file, label)
        character(len=:), allocatable, intent(out) :: raw_str
        character(len=*), intent(in) :: basis_file
        character(len=*), intent(in) :: label
        !local
        type(string_buffer_t) :: strbuf
        character(len=120) :: linebuf
        integer :: unit, istat, n, i

        call construct_string_buffer(strbuf)
        open (newunit=unit, file=basis_file, action="read", status="old")

        do
            read (unit, "(A)", iostat=istat) linebuf
            if (istat /= 0) exit ! not found, return empty string
            if (linebuf(1:1) == '*') then
                read (unit, "(A)") linebuf
                if (linebuf(1:2) == to_lower(label)) then ! found label
                    read (unit, "(A)") linebuf ! skip *
                    do
                        read (unit, "(A)") linebuf
                        if (linebuf(1:1) == '*') exit ! finish
                        call strbuf%append(trim(linebuf)//LF)
                    end do
                    raw_str = strbuf%to_string()
                    close (unit)
                    return
                end if
            end if
        end do

        raw_str = ""
        close (unit)

    end subroutine search_segment_from_file

    !> parse the string
    !> "3   s
    !>    0.1873113696D+02       0.3349460434D-01
    !>    0.2825394365D+01       0.2347269535D+00
    !>    0.6401216923D+00       0.8137573261D+00
    !>  1   s
    !>    0.1612777588D+00       1.0000000"
    subroutine parse_basis(input, bas_shls, error)
        character(len=*), intent(in) :: input
        type(basis_set_shell), dimension(:), allocatable, intent(out) :: bas_shls
        type(error_t), intent(out) :: error
        !local
        type(vla_char) :: lines
        character(len=:), allocatable :: line
        integer :: num_contract
        character(len=1) :: symb
        real(kind=f8), dimension(:), allocatable :: expn, coeff
        integer :: l
        integer :: i, j, istat

        allocate (bas_shls(0))

        call tokenize(input, lines, LF)

        i = 0
        do
            i = i + 1
            if (i > size(lines)) exit
            line = lines%at(i)
            if (len(trim(line)) == 0) cycle
            read (line, *, iostat=istat) num_contract, symb

            if (istat /= 0) then
                call raise_error(error, &
                                 "Invalid basis set definition in"//LF//"  | "//lines%at(i))
                return
            end if

            if (scan(symb, "spdfghi") == 0) then
                call raise_error(error, &
                                 "Unsupported shell type"//LF//"  | "//lines%at(i))
                return
            end if
            l = symb_to_l(symb)

            allocate (expn(num_contract))
            allocate (coeff(num_contract))
            do j = 1, num_contract
                i = i + 1
                line = lines%at(i)
                read (line, *, iostat=istat) expn(j), coeff(j)

                if (istat /= 0) then
                    call raise_error(error, &
                                     "Invalid basis set definition in"//LF//"  | "//lines%at(j))
                    return
                end if
            end do

            bas_shls = [bas_shls, basis_set_shell(l, coeff, expn)]
            deallocate (expn)
            deallocate (coeff)
        end do

    end subroutine parse_basis

    pure function symb_to_l(symb) result(l)
        character(len=1), intent(in) :: symb
        integer :: l

        select case (symb)
        case ('s'); l = 0
        case ('p'); l = 1
        case ('d'); l = 2
        case ('f'); l = 3
        case ('g'); l = 4
        case ('h'); l = 5
        case ('i'); l = 6
        end select

    end function symb_to_l

    function basis_file_name(bas_name) result(name)
        character(len=*), intent(in) :: bas_name
        character(len=:), allocatable :: name

        select case (to_lower(bas_name))
        case ("sto-3g"); name = "sto-3g.dat"
        case ("3-21g"); name = "3-21g.dat"
        case ("6-31g*"); name = "6-31g_st_.dat"
        case ("6-31g**"); name = "6-31g_st__st_.dat"
        case ("def2-svp"); name = "def2-svp.dat"
        case ("def2-svpd"); name = "def2-svpd.dat"
        case ("def2-sv(p)"); name = "def2-sv(p).dat"
        case default
            name = ""
        end select

    end function basis_file_name

end module ringo_mole_basis
