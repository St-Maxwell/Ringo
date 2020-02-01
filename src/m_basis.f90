module mod_basis
    use mod_io
    use mod_mol
    implicit none

    type ShellEnum
        integer :: s 
        integer :: sp
        integer :: p 
        integer :: d 
        integer :: f 
        integer :: g 
        integer :: h 
        integer :: i 
    end type

    type BasisSet
        integer :: num_basis
        integer :: num_shell
        integer :: num_gto
        integer :: max_angl ! maximum angular momentum
        type(BasisFunction), dimension(:), allocatable :: basis ! Basis Functions
        integer, dimension(:), allocatable :: shl_idx ! subscript of shell in basis functions' dimension
        integer, dimension(:), allocatable :: n_bf_in_shl ! number of basis functions in shell
    contains
        procedure :: init => init_from_file
        procedure :: destroy => destroy_basis_set
    end type BasisSet
    
    type BasisFunction
        integer :: ctr_order ! order of contraction
        type(Gaussian), dimension(:), allocatable :: gto
        real(kind=r8), dimension(:), allocatable :: ctr_coeff ! contraction coefficients
        real(kind=r8), dimension(3) :: center
        integer :: angl
    contains
        final :: destroy_basis_function
    end type BasisFunction

    type Gaussian
    ! g = x^lx * y^ly * z^lz * exp(-alpha*(x^2 + y^2 + z^2))
       real(kind=r8) :: alpha
       integer :: lx, ly, lz
       integer :: l ! l = lx + ly + lz
    end type Gaussian

    type(ShellEnum), parameter, private :: cart_shl = ShellEnum(1,4,3,6,10,15,21,28)

contains
    
    subroutine init_from_file(this, mole, basis_name)
        class(BasisSet), intent(inout) :: this
        class(Molecule), intent(in) :: mole
        character(len=*), intent(in) :: basis_name
        !===================================================
        integer :: stat

        call load_basis_set(basis_name, stat)

        call determine_num_basis(mole, &
                                 this%num_basis, &
                                 this%num_shell, &
                                 this%num_gto, &
                                 this%max_angl)
        allocate(this%basis(this%num_basis))
        allocate(this%shl_idx(this%num_shell))
        allocate(this%n_bf_in_shl(this%num_shell))

        call read_gto_for_basis(this, mole)

        call close_basis_file(stat)

    end subroutine init_from_file

    subroutine find_element(element, stat)
        character(len=2), intent(in) :: element
        integer :: stat
        !===================================================
        character(len=MAX_LINE_WIDTH) :: line
        integer :: istart, iend

        rewind(BASIS_FILE_UNIT)
        do while (.true.)
            read(BASIS_FILE_UNIT, "(A)", iostat=stat) line
            if (stat /= 0) return ! element not found

            istart = index(line, trim(element))
            iend = index(line, '0', back=.true.)

            if (istart /= 0 .and. iend /=0) then
                if (len(trim(element)) == 2) istart = istart + 1
                if (line(istart+1:iend-1) == repeat(' ', iend-istart-1)) then ! found
                    backspace(BASIS_FILE_UNIT)
                    return
                end if
            end if

        end do
    end subroutine find_element

    function check_shell_def(line, shell_type, ctr_odr) result(res)
        character(len=*), intent(in) :: line
        character(len=2) :: shell_type
        integer :: ctr_odr
        logical :: res
        !===================================================
        real(kind=r8) :: tmp
        integer :: i

        i = scan(line, "SPDFGHI")

        if (i == 0 .or. i > 20) then
            res = .false.
        else
            res = .true.
            read(line, *) shell_type, ctr_odr, tmp
        end if

    end function check_shell_def

    subroutine determine_num_basis(mole, num_basis, num_shell, num_gto, max_l)
        ! determine the number of basis functions,
        !           the number of contracted shells,
        !           the number of primitive Gaussian functions,
        !           maximum angular momentum
        class(Molecule), intent(in) :: mole
        integer :: num_basis
        integer :: num_shell
        integer :: num_gto
        integer :: max_l
        !===================================================
        character(len=MAX_LINE_WIDTH) :: line
        character(len=2) :: shell_type
        integer :: ctr_odr, l
        integer :: i, stat

        num_basis = 0; num_shell = 0
        num_gto = 0; max_l = 0
        do i = 1, mole%num_atom

            call find_element(mole%atom(i), stat)
            if (stat /= 0) stop "element not found"
            read(BASIS_FILE_UNIT, "(A)") line

            do while (.true.)
                read(BASIS_FILE_UNIT, "(A)") line
                if (index(line, "****") /= 0) exit

                if (check_shell_def(line, shell_type, ctr_odr)) then
                    num_shell = num_shell + 1
                    select case (shell_type)
                    case('S ')
                        num_basis = num_basis + cart_shl%s
                        num_gto = num_gto + ctr_odr * cart_shl%s
                        l = 0
                    case('SP')
                        num_basis = num_basis + cart_shl%sp
                        num_gto = num_gto + ctr_odr * cart_shl%sp
                        l = 1
                    case('P ')
                        num_basis = num_basis + cart_shl%p
                        num_gto = num_gto + ctr_odr * cart_shl%p
                        l = 1
                    case('D ')
                        num_basis = num_basis + cart_shl%d
                        num_gto = num_gto + ctr_odr * cart_shl%d
                        l = 2
                    case('F ')
                        num_basis = num_basis + cart_shl%f
                        num_gto = num_gto + ctr_odr * cart_shl%f
                        l = 3
                    case('G ')
                        num_basis = num_basis + cart_shl%g
                        num_gto = num_gto + ctr_odr * cart_shl%g
                        l = 4
                    case('H ')
                        num_basis = num_basis + cart_shl%h
                        num_gto = num_gto + ctr_odr * cart_shl%h
                        l = 5
                    case('I ')
                        num_basis = num_basis + cart_shl%i
                        num_gto = num_gto + ctr_odr * cart_shl%i
                        l = 6
                    end select
                    if (l > max_l) max_l = l
                end if
            end do

        end do

    end subroutine determine_num_basis

    subroutine read_gto_for_basis(basis_set, mole)
        ! read basis functions
        !      contraction coefficients
        !      exponents of GTOs
        ! ...
        class(BasisSet), intent(inout) :: basis_set
        class(Molecule), intent(in) :: mole
        !===================================================
        real(kind=r8), dimension(50) :: expnt
        real(kind=r8), dimension(50,2) :: coeff
        character(len=MAX_LINE_WIDTH) :: line
        character(len=2) :: shell_type
        integer :: ctr_odr
        integer :: i, j, ibasis, ishell
        integer :: stat


        ishell = 0; ibasis = 1
        do i = 1, mole%num_atom

            call find_element(mole%atom(i), stat)
            if (stat /= 0) stop "element not find"
            read(BASIS_FILE_UNIT, "(A)") line

            do while (.true.)
                read(BASIS_FILE_UNIT, "(A)") line
                if (index(line, "****") /= 0) exit

                if (check_shell_def(line, shell_type, ctr_odr)) then

                    ishell = ishell + 1
                    basis_set%shl_idx(ishell) = ibasis
                    call read_gto(ctr_odr, shell_type, expnt, coeff)

                    select case (shell_type)
                    case('S ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%s

                        do j = ibasis, ibasis + cart_shl%s - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 0

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 0

                        end do

                        ibasis = ibasis + cart_shl%s
                    case('SP')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%sp

                        block
                        integer :: tmp
                        tmp = 1
                        do j = ibasis, ibasis + cart_shl%sp - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            if (tmp == 1) then
                                basis_set%basis(j)%angl = 0
                            else
                                basis_set%basis(j)%angl = 1
                            end if
                            
                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))

                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            
                            if (tmp == 1) then
                                basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                                basis_set%basis(j)%gto(:)%l = 0
                            else
                                basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,2)
                                basis_set%basis(j)%gto(:)%l = 1
                            end if

                            tmp = tmp + 1
                        end do
                        end block

                        ibasis = ibasis + cart_shl%sp
                    case('P ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%p

                        do j = ibasis, ibasis + cart_shl%p - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 1

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 1

                        end do

                        ibasis = ibasis + cart_shl%p
                    case('D ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%d

                        do j = ibasis, ibasis + cart_shl%d - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 2

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 2
                            
                        end do

                        ibasis = ibasis + cart_shl%d
                    case('F ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%f

                        do j = ibasis, ibasis + cart_shl%f - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 3

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 3

                        end do

                        ibasis = ibasis + cart_shl%f
                    case('G ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%g

                        do j = ibasis, ibasis + cart_shl%g - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 4

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 4

                        end do

                        ibasis = ibasis + cart_shl%g
                    case('H ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%h

                        do j = ibasis, ibasis + cart_shl%h - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 5

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 5

                        end do

                        ibasis = ibasis + cart_shl%h
                    case('I ')
                        basis_set%n_bf_in_shl(ishell) = cart_shl%i

                        do j = ibasis, ibasis + cart_shl%i - 1
                            basis_set%basis(j)%center(:) = mole%geom(:,i)
                            basis_set%basis(j)%ctr_order = ctr_odr
                            basis_set%basis(j)%angl = 6

                            allocate(basis_set%basis(j)%gto(ctr_odr))
                            allocate(basis_set%basis(j)%ctr_coeff(ctr_odr))
                            
                            basis_set%basis(j)%gto(:)%alpha = expnt(1:ctr_odr)
                            basis_set%basis(j)%ctr_coeff(:) = coeff(1:ctr_odr,1)
                            basis_set%basis(j)%gto(:)%l = 6

                        end do

                        ibasis = ibasis + cart_shl%i
                    end select

                end if
            end do

        end do

    end subroutine read_gto_for_basis

    subroutine read_gto(ctr_odr, shell_type, expnt, coeff)
        integer, intent(in) :: ctr_odr
        character(len=2), intent(in) :: shell_type
        real(kind=r8), dimension(:) :: expnt
        real(kind=r8), dimension(:,:) :: coeff
        !===================================================
        integer :: i

        if (shell_type == "SP") then
            do i = 1, ctr_odr
                read(BASIS_FILE_UNIT, *) expnt(i), coeff(i,1), coeff(i,2)
            end do
        else
            do i = 1, ctr_odr
                read(BASIS_FILE_UNIT, *) expnt(i), coeff(i,1)
            end do
        end if

    end subroutine read_gto

    subroutine destroy_basis_set(this)
        class(BasisSet) :: this

        deallocate(this%basis)
        deallocate(this%n_bf_in_shl)
        deallocate(this%shl_idx)

    end subroutine destroy_basis_set

    subroutine destroy_basis_function(this)
        type(BasisFunction) :: this

        deallocate(this%gto)
        deallocate(this%ctr_coeff)

    end subroutine destroy_basis_function

end module mod_basis