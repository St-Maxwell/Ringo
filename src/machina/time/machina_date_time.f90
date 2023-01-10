module machina_date_time
    use machina_string, only: to_string
    implicit none
    private
    public :: get_date_time, get_time

contains

    !> get date and time
    !> format: yyyy-mm-dd hh:mm:ss
    function get_date_time() result(string)
        character(len=:), allocatable :: string
        character(len=8) :: date
        character(len=10) :: time

        call date_and_time(date=date, time=time)
        string = date(1:4)//'-'//date(5:6)//'-'//date(7:8)//' '// &
                 time(1:2)//':'//time(3:4)//':'//time(5:6)

    end function get_date_time

    !> get time
    !> format: hh:mm:ss.sss
    function get_time() result(string)
        character(len=:), allocatable :: string
        character(len=10) :: time

        call date_and_time(time=time)
        string = time(1:2)//':'//time(3:4)//':'//time(5:10)

    end function get_time

end module machina_date_time
