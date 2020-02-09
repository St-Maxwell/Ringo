# recommended gfortran version >= 7
FC = gfortran-8

Ringo: m_const.o m_io.o m_algorithm.o m_mol.o \
m_basis.o m_libcint.o m_scf.o m_rhf.o m_scf_main.o Ringo.o
	${FC} -o Ringo *.o ./lib/libcint.a -lrefblas -llapack

m_const.o: m_const.f90
	${FC} -c m_const.f90

m_io.o: m_io.f90
	${FC} -c m_io.f90

m_algorithm.o: m_algorithm.f90
	${FC} -c m_algorithm.f90

m_mol.o: m_mol.f90
	${FC} -c m_mol.f90

m_basis.o: m_basis.f90
	${FC} -c m_basis.f90

m_libcint.o: m_libcint.f90
	${FC} -c m_libcint.f90

m_scf.o: m_scf.f90
	${FC} -c m_scf.f90

m_rhf.o: m_rhf.f90
	${FC} -c m_rhf.f90

m_scf_main.o: m_scf_main.f90
	${FC} -c m_scf_main.f90

Ringo.o: Ringo.f90
	${FC} -c Ringo.f90
