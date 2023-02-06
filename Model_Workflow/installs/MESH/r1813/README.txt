This file contains information about options in the 'makefile' included
with this distribution.

To compile SA_MESH, run 'make' with one of the following targets:
Serial (sa_mesh):
  $ make gfortran
  $ make ifort
  $ mingw32-make mingw_static
Parallel (mpi_sa_mesh; requires OpenMPI compiler):
  $ make mpi_gcc
  $ make mpi_intel

If no target is specified, 'gfortran' is assumed.
  $ make

The makefile will automatically remove *.o and *.mod files if the
compile process is successful.

Recommended for Cygwin, Ubuntu (without Intel license)
To create 'sa_mesh':
  $ make
To create 'mpi_sa_mesh':
  $ make mpi_gcc

Recommended for Linux where the Intel license is available:
To create 'sa_mesh':
  $ make ifort
To create 'mpi_sa_mesh':
  $ make mpi_intel

Description of target to compiler:
  - gfortran: GNU compiler (gcc and gfortran).
  - ifort: Intel compiler (ifort).
  - mingw_static: MinGW MS-Windows/MS-DOS GNU distribution.
  - mpi_gcc*: GNU version of OpenMPI compiler (mpifort).
  - mpi_intel*: Intel version of OpenMPI compiler (mpifort).
*'mpi_gcc' and 'mpi_intel' are not interchangeable, even though in both
cases the compiler is named 'mpifort'. The code will also check for the
compiler named 'mpif90'.

To remove remaining *.o and *.mod files:
  $ make clean
*For MinGW, DIST=mingw must also be provided.
Examples:
  - To remove *.o and *.mod files in Cygwin/Linux environment:
      $ make clean
  - To remove *.o and *.mod files in MS-Windows/MS-DOS environment:
      $ mingw32-make clean DIST=mingw

To remove remaining *.o and *.mod files, and the compiled program:
  $ make veryclean
*For MinGW, DIST=mingw must also be provided.
*For 'mpi_gcc' and 'mpi_intel', MPI=ompi must also be provided.
Examples:
  - To remove *.o and *.mod files, and 'sa_mesh' in Cygwin/Linux
    environment:
      $ make veryclean
  - To remove *.o and *.mod files, and 'mpi_sa_mesh' in Cygwin/Linux
    environment:
      $ make veryclean MPI=ompi
  - To remove *.o and *.mod files, and 'sa_mesh_static' in
    MS-Windows/MS-DOS environment:
      $ mingw32-make veryclean DIST=mingw

The following options can be added to the compiler targets to enable
or add features.
  - symbols: Disable code optimization and add debugging symbols.
  - debug: Same as 'symbols' but also enabled extra debugging options
        and traps.
  - double: Enable double precision (only impacts code where default
        precision on types is used).
  - netcdf: Enable NetCDF file support (requires the NetCDF Fortran
        library, not included with this code -- often available as a
        system package or module on managed systems).
Examples:
  - To compile 'sa_mesh' using GNU compilers with strict debug options:
      $ make debug
  - To compile 'sa_mesh' using ifort compiler with strict debug options:
      $ make ifort debug
  - To compile 'mpi_sa_mesh' using GNU compilers with NetCDF support
    (requires the NetCDF Fortran library):
      $ make mpi_gcc netcdf
  - To compile 'sa_mesh' using GNU compilers with double precision:
      $ make double
  - To compile 'mpi_sa_mesh' using the Intel compiler with NetCDF
    support (requires the NetCDF Fortran library) and double precision:
      $ make mpi_intel double

The above targets are wrappers to configure variables in the makefile.
However, these variables can also be set directly without using any
target. The direct variables can be set as follows:
    DIST: Compiler family/distribution.
      - Blank/undefined to use GNU compilers (default).
      - 'intel' to use the Intel compiler.
      - 'mingw' to use MinGW GNU-based compiler; overrides 'rm' with
        'del' for MS-Windows/MS-DOS environment when cleaning up files.
    MPI: Parallel/serial compilation.
      - Blank/undefined to compile in serial (default).
      - 'ompi' to compile using OMPI compiler.
    LSS: Land surface scheme (LSS).
      - Blank/undefined to include default versions of CLASS+SVS
        (default).
    ROUTE: Routing scheme.
      - Blank/undefined to include default versions of
        WF_ROUTE+SA_RTE+RTE (default).
    SYMBOLS: Symbols.
      - Blank/undefined to compile with 'O2' optimization (default).
      - 'yes' to add debugging symbols.
    DEBUG: Debugging flags and options.
      - Blank/undefined to compile with options determined by 'SYMBOLS'
        (default).
      - 'yes' to include debugging symbols and extra debugging traps and
        options.
    DOUBLE: Single/double precision.
      - Blank/undefined to use single precision.
      - 'yes' to use double precision (only impacts code where default
        precision on types is used).
Examples:
  - To compile 'mpi_sa_mesh' using the Intel compiler (equivalent to
    'make mpi_intel'):
      $ make DIST=intel MPI=ompi
  - To compile 'sa_mesh' using GNU compilers with debug flags/options
    (equivalent to 'make debug'):
      $ make DEBUG=yes
