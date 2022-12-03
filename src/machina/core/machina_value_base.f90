module machina_value_base
    implicit none
    private
    public :: machina_value

    !> the universal interface of data structure in machina
    type, abstract :: machina_value
    end type

end module machina_value_base
