module ringo_cint_interface
    use machina_basic, only: i4, f8
    use iso_c_binding, only: c_ptr
    implicit none
    private
    public :: cint1e_ovlp_sph, cint1e_kin_sph, cint1e_nuc_sph
    public :: cint2e_sph, cint2e_sph_optimizer, CINTdel_optimizer

    interface
        integer(kind=i4) function cint1e_ovlp_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_ovlp_sph

        integer(kind=i4) function cint1e_kin_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_kin_sph

        integer(kind=i4) function cint1e_nuc_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_nuc_sph

        integer(kind=i4) function cint2e_sph(buf2e, shls, atm, natm, bas, nbas, env, opt) bind(C)
            import :: i4, f8, c_ptr
            real(kind=f8), intent(out) :: buf2e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
            type(c_ptr), value :: opt
        end function cint2e_sph

        subroutine cint2e_sph_optimizer(opt, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8, c_ptr
            type(c_ptr), intent(inout) :: opt(*)
            integer(kind=i4), intent(in) :: atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end subroutine cint2e_sph_optimizer

        subroutine CINTdel_optimizer(opt) bind(C, name="CINTdel_optimizer")
            import :: c_ptr
            type(c_ptr), intent(inout) :: opt(*)
        end subroutine CINTdel_optimizer
    end interface

end module ringo_cint_interface
