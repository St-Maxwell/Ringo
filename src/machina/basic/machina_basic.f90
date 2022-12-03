module machina_basic
    use machina_kinds
    use machina_optval
    use machina_c_compatibility
    implicit none
    !> from machina_kinds
    public :: i4, i8
    public :: f4, f8, f16
    public :: c4, c8, c16
    !> from machina_optval
    public :: optval
    !> from machina_c_compatibility
    public :: c_f_character
end module machina_basic
