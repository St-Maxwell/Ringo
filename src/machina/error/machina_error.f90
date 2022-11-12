module machina_error
    use machina_error_kind
    use machina_error_detail
    implicit none
    private
    public :: ok_base, error_base
    public :: ok, logic_error, value_error, os_error
    public :: out_of_range_error, parse_failed_error
    public :: error_t, set_error, clear_error
    public :: operator(.has.)
end module machina_error
