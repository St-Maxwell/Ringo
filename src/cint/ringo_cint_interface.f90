module ringo_cint_interface
    use machina_basic, only: i4, f8
    implicit none
    private
    public :: cint1e_ovlp_sph, cint1e_kin_sph, cint1e_nuc_sph

    interface
        function cint1e_ovlp_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_ovlp_sph

        function cint1e_kin_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_kin_sph

        function cint1e_nuc_sph(buf1e, shls, atm, natm, bas, nbas, env) bind(C)
            import :: i4, f8
            real(kind=f8), intent(out) :: buf1e(*)
            integer(kind=i4), intent(in) :: shls(*), atm(*), bas(*)
            integer(kind=i4), value :: natm, nbas
            real(kind=f8), intent(in) :: env(*)
        end function cint1e_nuc_sph
    end interface

end module ringo_cint_interface
