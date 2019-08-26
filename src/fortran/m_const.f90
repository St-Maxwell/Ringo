module m_const
  use iso_fortran_env, only: real64, real128
  implicit none

  ! real kind type
  integer, parameter :: r8 = real64
  integer, parameter :: r16 = real128

  ! constant
  real(kind=r8), parameter :: pi = 3.141592653589793
  ! 2018 CODATA recommended values
  ! https://physics.nist.gov/cuu/Constants/index.html
  real(kind=r8), parameter :: plank_constant = 6.62607015E-34
  real(kind=r8), parameter :: reduced_plank_constant = 1.054571817E-34
  real(kind=r8), parameter :: speed_of_light = 299792458.
  real(kind=r8), parameter :: electron_mass = 9.1093837015E-31
  real(kind=r8), parameter :: hartree = 4.3597447222071E-18
  real(kind=r8), parameter :: bohr_radius = 5.29177210903E-11
  real(kind=r8), parameter :: boltzmann_constant = 1.380649E-23

  ! I/O Unit
  integer, parameter :: basis_set_file_unit = 10
  integer, parameter :: input_file_unit = 11
  integer, parameter :: output_file_unit = 12

  ! elements
  integer, parameter :: num_supported_elements = 18
  character(len=2), parameter :: element_list(num_supported_elements) =  &
  ['H ',                                     'He', &
   'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
   'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar']



end module