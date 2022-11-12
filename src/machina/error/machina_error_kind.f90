module machina_error_kind
    implicit none
    public

    !> base type for representing execution status
    !> do not expose to outside
    type, abstract :: error_kind_base
    end type

    !> base type of success status
    type, extends(error_kind_base), abstract :: ok_base
    end type
    !> base type of error status
    type, extends(error_kind_base), abstract :: error_base
    end type

    !> success defined in machina
    type, extends(ok_base) :: ok
    end type
    !> errors defined in machina
    type, extends(error_base) :: logic_error
    end type
    type, extends(error_base) :: value_error
    end type
    type, extends(error_base) :: os_error
    end type
    type, extends(error_base) :: out_of_range_error
    end type
    type, extends(error_base) :: parse_failed_error
    end type

end module machina_error_kind
