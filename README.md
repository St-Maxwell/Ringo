# Ringo

Ringo is a quantum chemistry program written in Fortran, designed for electronic structure calculations.

Note: In the development of Ringo, I have referred to the code and design of several software, including [PySCF](https://github.com/pyscf/pyscf), [Q-Chem](https://www.q-chem.com/), [Psi4](https://github.com/psi4/psi4), and [YATDFT](https://github.com/huanghua1994/YATDFT).

## Dependencies
* [Libcint](https://github.com/sunqm/libcint), for electron integrals
* LAPACK / BLAS provider, [OpenBLAS](https://www.openblas.net/) is recommended

## Build
* [fpm](https://github.com/fortran-lang/fpm), version 0.7.0 or newer

Edit `PROJECT_ROOT` with your actual path in the `fpm.toml`, then run
```
fpm buid
```
Make sure that libcint and OpenBLAS have been added in the `LIBRARY_PATH` environment variable. A detailed tutorial has been documented in the [Manifest reference](https://fpm.fortran-lang.org/en/spec/manifest.html#link-external-libraries) of fpm.

## Example
Run
```
fpm run -- ./example/test.inp
```
