module ringo_elements
    use machina_basic
    use machina_string, only: to_upper
    implicit none

    integer, parameter :: num_supported_elements = 36

    character(len=2), parameter :: element_list(0:num_supported_elements) = &
    ['Bq',                                                                                      & ! 0
     'H ',                                                                                'He', & ! 1-2
     'Li','Be',                                                  'B ','C ','N ','O ','F ','Ne', & ! 3-10
     'Na','Mg',                                                  'Al','Si','P ','S ','Cl','Ar', & ! 11-18
     'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr']   ! 19-36

    character(len=2), parameter :: element_list_upper(0:num_supported_elements) = &
    ['BQ',                                                                                      & ! 0
     'H ',                                                                                'HE', & ! 1-2
     'LI','BE',                                                  'B ','C ','N ','O ','F ','NE', & ! 3-10
     'NA','MG',                                                  'AL','SI','P ','S ','CL','AR', & ! 11-18
     'K ','CA','SC','TI','V ','CR','MN','FE','CO','NI','CU','ZN','GA','GE','AS','SE','BR','KR']   ! 19-36

     real(kind=f8), parameter :: element_weight(0:num_supported_elements) = &
    [0._f8, & ! 0
     1.00794_f8,4.0026_f8, & ! 1-2
     6.941_f8,9.01218_f8,10.811_f8,12.0107_f8,14.0067_f8,15.9994_f8,18.9984_f8,20.1797_f8, & ! 3-10
     22.98977_f8,24.305_f8,26.98154_f8,28.0855_f8,30.97376_f8,32.065_f8,35.453_f8,39.948_f8, & ! 11-18
     39.0983_f8,40.078_f8,44.95591_f8,47.867_f8,50.9415_f8,51.9961_f8,54.93805_f8,55.845_f8, & ! 19-
     58.93319_f8,58.6934_f8,63.546_f8,65.38_f8,69.723_f8,72.64_f8,74.9216_f8,78.96_f8,79.904_f8,83.798_f8] ! -36

contains

    !> return the atomic number by giving atomic symbol
    !> if the passed atomic symbol is not found (not supported or invalid value)
    !> will return -1 instead
    function symbol_to_number(symbol) result(num)
        character(len=*), intent(in) :: symbol
        integer :: num

        num = findloc(element_list_upper, to_upper(symbol), dim=1) - 1

    end function symbol_to_number


    function number_to_symbol(num) result(symbol)
        integer, intent(in) :: num
        character(len=:), allocatable :: symbol
        
        if (num >= 0 .and. num <= num_supported_elements) then
            symbol = element_list(num)
        else
            symbol = ""
        end if

    end function number_to_symbol

end module ringo_elements
