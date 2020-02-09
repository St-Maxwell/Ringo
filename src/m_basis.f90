module mod_basis
    use mod_const
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

    type Shell
        integer :: cntrOrder
        real(kind=r8), dimension(:), allocatable :: expnt
        real(kind=r8), dimension(:), allocatable :: coeff
    contains
        final :: destroy_shell
    end type Shell
    
    type BasisSet
        integer :: numBasis
        integer :: numShell
        integer :: numGTO
        integer :: numPrimitive
        integer :: maxAngl ! maximum angular momentum
        integer, dimension(:), allocatable :: angl
        integer, dimension(:), allocatable :: cntrOrder
        integer, dimension(:), allocatable :: numCntrBasis
        real(kind=r8), dimension(:,:), allocatable :: center
        type(Shell), dimension(:), allocatable :: shell
        integer, dimension(:), allocatable :: shellIndex
        integer, dimension(:), allocatable :: atomIndex
    contains
        procedure :: init => init_from_file
        procedure :: destroy => destroy_basis_set
    end type BasisSet
    

    !type(ShellEnum), parameter, private :: cart_shl = ShellEnum(1,4,3,6,10,15,21,28)
    type(ShellEnum), parameter, private :: sph_shl = ShellEnum(1,4,3,5,7,9,11,13)
    type(ShellEnum), parameter, private :: shl_angl = ShellEnum(0,-1,1,2,3,4,5,6)

contains
    
    subroutine init_from_file(this, mole, filename)
        class(BasisSet), intent(inout) :: this
        class(Molecule), intent(in) :: mole
        character(len=*), intent(in) :: filename
        !===================================================
        character(len=20) :: basis_name
        integer :: stat

        call load_input_file(filename, stat)
        call basis_input(basis_name)
        call close_input_file(stat)

        call load_basis_set(basis_name, stat)
        if (stat == 1) then
            call error_terminate(OUTPUT_FILE_UNIT, "Open basis file failed.")
        else if (stat == 2) then
            call error_terminate(OUTPUT_FILE_UNIT, "Basis set not exist.")
        end if

        call determine_basis_from_file(this, mole)

        allocate(this%angl(this%numShell))
        allocate(this%cntrOrder(this%numShell))
        allocate(this%numCntrBasis(this%numShell))
        allocate(this%center(3,this%numShell))
        allocate(this%shell(this%numShell))
        allocate(this%shellIndex(this%numShell))
        allocate(this%atomIndex(this%numShell))

        call read_basis_from_file(this, mole)

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

    subroutine determine_basis_from_file(basis_set, mole)
        ! determine the number of basis functions,
        !           the number of contracted shells,
        !           the number of primitive Gaussian functions,
        !           maximum angular momentum
        class(BasisSet), intent(inout) :: basis_set
        class(Molecule), intent(in) :: mole
        !===================================================
        character(len=MAX_LINE_WIDTH) :: line
        character(len=2) :: shell_type
        integer :: ctr_odr, l
        integer :: i, stat

        basis_set%numBasis = 0
        basis_set%numShell = 0
        basis_set%numGTO = 0
        basis_set%numPrimitive = 0
        basis_set%maxAngl = 0

        do i = 1, mole%NumAtom

            call find_element(mole%atomSymbol(i), stat)
            if (stat /= 0) stop "element not found"
            read(BASIS_FILE_UNIT, "(A)") line

            do while (.true.)
                read(BASIS_FILE_UNIT, "(A)") line
                if (index(line, "****") /= 0) exit

                if (check_shell_def(line, shell_type, ctr_odr)) then
                    basis_set%numShell = basis_set%numShell + 1
                    basis_set%numPrimitive = basis_set%numPrimitive + ctr_odr

                    select case (shell_type)
                    case('S ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%s
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%s
                        l = 0
                    case('SP')
                        basis_set%numShell = basis_set%numShell + 1 ! split up sp shell into s and p
                        basis_set%numBasis = basis_set%numBasis + sph_shl%sp
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%sp
                        basis_set%numPrimitive = basis_set%numPrimitive + ctr_odr
                        l = 1
                    case('P ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%p
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%p
                        l = 1
                    case('D ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%d
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%d
                        l = 2
                    case('F ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%f
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%f
                        l = 3
                    case('G ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%g
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%g
                        l = 4
                    case('H ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%h
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%h
                        l = 5
                    case('I ')
                        basis_set%numBasis = basis_set%numBasis + sph_shl%i
                        basis_set%numGTO = basis_set%numGTO + ctr_odr * sph_shl%i
                        l = 6
                    end select
                    if (l > basis_set%maxAngl) basis_set%maxAngl = l
                end if
            end do

        end do

    end subroutine determine_basis_from_file

    subroutine read_basis_from_file(basis_set, mole)
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
        integer :: ishell, iatom, iprim
        integer :: stat

        ishell = 0
        iprim = 1
        do iatom = 1, mole%NumAtom

            call find_element(mole%atomSymbol(iatom), stat)
            if (stat /= 0) stop "element not find"
            read(BASIS_FILE_UNIT, "(A)") line

            do while (.true.)
                read(BASIS_FILE_UNIT, "(A)") line
                if (index(line, "****") /= 0) exit

                if (check_shell_def(line, shell_type, ctr_odr)) then

                    ishell = ishell + 1

                    call read_gto(ctr_odr, shell_type, expnt, coeff)
                    basis_set%shellIndex(ishell) = iprim

                    select case (shell_type)
                    case('S ')
                        basis_set%angl(ishell) = shl_angl%s
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%s

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('SP')
                        ! deal with s
                        basis_set%angl(ishell) = shl_angl%s
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%s

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                        ishell = ishell + 1
                        basis_set%shellIndex(ishell) = iprim

                        ! deal with p
                        basis_set%angl(ishell) = shl_angl%p
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%p

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,2)

                    case('P ')
                        basis_set%angl(ishell) = shl_angl%p
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%p

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('D ')
                        basis_set%angl(ishell) = shl_angl%d
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%d

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('F ')
                        basis_set%angl(ishell) = shl_angl%f
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%f

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('G ')
                        basis_set%angl(ishell) = shl_angl%g
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%g

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('H ')
                        basis_set%angl(ishell) = shl_angl%h
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%h

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    case('I ')
                        basis_set%angl(ishell) = shl_angl%i
                        basis_set%cntrOrder(ishell) = ctr_odr
                        basis_set%numCntrBasis(ishell) = 1
                        basis_set%center(:,ishell) = mole%geom(:,iatom)
                        basis_set%atomIndex(ishell) = iatom
                        iprim = iprim + sph_shl%i

                        basis_set%shell(ishell)%cntrOrder = ctr_odr
                        allocate(basis_set%shell(ishell)%expnt(ctr_odr))
                        allocate(basis_set%shell(ishell)%coeff(ctr_odr))

                        basis_set%shell(ishell)%expnt(:) = expnt(1:ctr_odr)
                        basis_set%shell(ishell)%coeff(:) = coeff(1:ctr_odr,1)

                    end select

                end if
            end do

        end do

    end subroutine read_basis_from_file

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

    subroutine pack_basis_gto(basis_set, expnt, coeff, angl)
        class(BasisSet), intent(in) :: basis_set
        real(kind=r8), dimension(:), allocatable :: expnt
        real(kind=r8), dimension(:), allocatable :: coeff
        integer, dimension(:), allocatable :: angl
        !===================================================
        integer :: ishell, istart, iend
        
        allocate(expnt(sum(basis_set%cntrOrder)))
        allocate(coeff(sum(basis_set%cntrOrder)))
        allocate(angl(sum(basis_set%cntrOrder)))

        istart = 1
        do ishell = 1, basis_set%numShell
            iend = istart + basis_set%cntrOrder(ishell) - 1
            expnt(istart:iend) = basis_set%shell(ishell)%expnt(:)
            coeff(istart:iend) = basis_set%shell(ishell)%coeff(:)
            angl(istart:iend) = basis_set%angl(ishell)
            istart = istart + basis_set%cntrOrder(ishell)
        end do

    end subroutine pack_basis_gto

!    subroutine pack_basis_shell(basis_set, idx)
!        class(BasisSet), intent(in) :: basis_set
!        integer, dimension(:), allocatable :: idx
!        !===================================================
!        integer :: ishell, k
!
!        k = 1
!        do ishell = 1, basis_set%numShell
!            idx(ishell) = k
!            k = k + basis_set%cntrOrder(ishell)
!        end do
!
!    end subroutine pack_basis_shell

    subroutine destroy_basis_set(this)
        class(BasisSet) :: this

        deallocate(this%angl)
        deallocate(this%cntrOrder)
        deallocate(this%numCntrBasis)
        deallocate(this%center)
        deallocate(this%shell)
        deallocate(this%shellIndex)
        deallocate(this%atomIndex)

    end subroutine destroy_basis_set

    subroutine destroy_shell(this)
        type(Shell) :: this

        deallocate(this%expnt)
        deallocate(this%coeff)

    end subroutine destroy_shell

end module mod_basis