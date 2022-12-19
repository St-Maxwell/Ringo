module machina_timer
    use machina_basic, only: i8, f8
    use machina_string, only: to_string
    implicit none
    private
    public :: timer_t, format_time

    type :: timer_t
        private
        integer(kind=i8) :: tick = 0
        integer(kind=i8) :: tock = 0
        real(kind=f8) :: cpu_tick = 0
        real(kind=f8) :: cpu_tock = 0
        logical :: started = .false.
    contains
        procedure :: start
        procedure :: stop
        procedure :: get_cpu_time
        procedure :: get_elapsed_time
    end type

contains

    subroutine start(this)
        class(timer_t), intent(out) :: this

        call reset(this)
        this%started = .true.

        call cpu_time(this%cpu_tick)
        call system_clock(count=this%tick)

    end subroutine start

    subroutine stop(this)
        class(timer_t), intent(inout) :: this

        if (this%started) then
            call cpu_time(this%cpu_tock)
            call system_clock(count=this%tock)

            this%started = .false.
        end if

    end subroutine stop

    !> return the cpu time, in seconds
    function get_cpu_time(this) result(time)
        class(timer_t), intent(in) :: this
        real(kind=f8) :: time

        if (.not. this%started) then
            time = this%cpu_tock - this%cpu_tick
        else
            time = 0
        end if

    end function get_cpu_time

    !> return the elapsed time (wall clock time), in seconds
    function get_elapsed_time(this) result(time)
        class(timer_t), intent(in) :: this
        real(kind=f8) :: time
        integer(i8) :: count_rate, count_max, ticks

        if (.not. this%started) then
            call system_clock(count_rate=count_rate, count_max=count_max)

            ticks = this%tock - this%tick
            if (ticks > 0) then
                time = real(ticks, f8)/real(count_rate, f8)
            else
                time = real(ticks + count_max, f8)/real(count_rate, f8)
            end if
        else
            time = 0
        end if

    end function get_elapsed_time

    !> convert time (in seconds) to formatted string
    function format_time(time) result(string)
        real(kind=f8), intent(in) :: time
        character(len=:), allocatable :: string
        ! locals
        real(kind=f8) :: secs
        integer :: mins, hours, days

        secs = time
        days = int(secs/86400.0_f8)
        secs = secs - days*86400.0_f8
        hours = int(secs/3600.0_f8)
        secs = secs - hours*3600.0_f8
        mins = int(secs/60.0_f8)
        secs = time - mins*60.0_f8

        if (days >= 0) then
            string = to_string(days)//" days "
        end if
        if (hours >= 0) then
            string = string//to_string(hours)//" hours "
        end if
        if (mins >= 0) then
            string = string//to_string(mins)//" minutes "
        end if
        string = string//to_string(secs, 2)//" seconds"

    end function format_time

    subroutine reset(timer)
        type(timer_t), intent(out) :: timer

        timer%tick = 0
        timer%tock = 0
        timer%cpu_tick = 0
        timer%cpu_tock = 0
        timer%started = .false.

    end subroutine reset

end module machina_timer
