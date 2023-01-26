# ======================================================================
# Makefile for SA_MESH

# ======================================================================
# Make targets (defined below).
.PHONY: default gfortran ifort mingw_static mpi_gcc mpi_intel symbols debug netcdf all clean veryclean
default: all

# ======================================================================
# Options.
#   If the variable is not defined, an assumed value is assigned.
#   Options can be overwritten by user overrides or scripts.

# DIST: Compiler family/distribution.
#   - Blank/undefined to use GNU/GCC compiler (default).
#   - 'intel' to use Intel compiler.
#   - 'mingw' to use GNU/GCC compiler; overrides 'rm' with 'del' for MS-Windows/MS-DOS environment.

# MPI: Parallel/serial compilation.
#   - Blank/undefined to compile in serial (default).
#   - 'ompi' to compile using OMPI compiler.

# LSS: Land surface scheme (LSS).
#   - Blank/undefined to include default versions of CLASS+SVS (default).

# ROUTE: Routing scheme.
#   - Blank/undefined to include default versions of WF_ROUTE,SA_RTE+RTE (default).

# DEBUG: Debugging flags and options.
#   - Blank/undefined to compile with 'o2' optimization (default).
#   - 'yes' to include debug options and disable compiler optimization.

# ======================================================================
# File/object names.
include makefile.def

# ======================================================================
# Pre-configured targets: Compiler options.
ifeq ($(filter gfortran,$(MAKECMDGOALS)),gfortran)
DIST=
MPI=
else ifeq ($(filter ifort,$(MAKECMDGOALS)),ifort)
DIST=intel
MPI=
else ifeq ($(filter mingw_static,$(MAKECMDGOALS)),mingw_static)
DIST=mingw
MPI=
else ifeq ($(filter mpi_gcc,$(MAKECMDGOALS)),mpi_gcc)
DIST=
MPI=ompi
else ifeq ($(filter mpi_intel,$(MAKECMDGOALS)),mpi_intel)
DIST=intel
MPI=ompi
endif

# ======================================================================
# Pre-configured targets: Debug symbols.
ifeq ($(filter debug,$(MAKECMDGOALS)),debug)
SYMBOLS=yes
DEBUG=yes
else ifeq ($(filter symbols,$(MAKECMDGOALS)),symbols)
SYMBOLS=yes
endif

# ======================================================================
# Pre-configured targets: Double precision (where supported).
ifeq ($(filter double,$(MAKECMDGOALS)),double)
DOUBLE=yes
endif

# ======================================================================
# Pre-configured targets: netCDF library.
# This target will call 'nf-config' via the active shell.
# However, if the netCDF library is installed,
# 'nf-config' should be installed as well.
ifeq ($(filter netcdf,$(MAKECMDGOALS)),netcdf)
LIBNCO=$(shell nf-config --fflags) -DNETCDF
LIBNCL=$(shell nf-config --flibs)
endif

# ======================================================================
# Targets.
gfortran: all
ifort: all
mingw_static: all
mpi_gcc: all
mpi_intel: all
symbols: all
debug: all
double: all
netcdf: all

# ======================================================================
# Compiler check (if not the 'clean' or 'veryclean' targets).
# Minimum requirement.
# Intel 16+ (15+):
#   - Intel 15 introduces full Fortran 2003 support but is untested.
#   - Intel 14 will compile but may stall during run-time. This is
#       presumed to be the result of partial Fortran 2003 support.
# GNU/gcc 5+:
#   - GNU/gcc 4 does not implement the necessary Fortran 2003 features.
ifeq ($(filter clean veryclean, $(MAKECMDGOALS)), )
  ifeq ($(DIST),intel)
    ifeq ($(shell test $$(icc -dumpversion | cut -d '.' -f 1) -lt 16; echo $$?), 0)
      $(error The code requires Intel compiler version 16 or higher)
    endif
  else
    ifeq ($(shell test $$(gcc -dumpversion | cut -d '.' -f 1) -lt 5; echo $$?), 0)
      $(error The code requires GNU/gcc and GNU/gfortran version 5 or higher)
    endif
  endif
endif

# ======================================================================
# Compiler and options.
# 'FTN90PP' and 'FTN90PPOPT' required to compile 'ftn90' files.
ifeq ($(DIST),intel)
FC=ifort
CC=icc
GFLAG=-check bounds -fpe0
LFLAG=-c -g -traceback -fp-model source
CFLAG=
ifeq ($(shell test $$(icc -dumpversion | cut -d '.' -f 1) -lt 17; echo $$?), 0)
  CFLAG+= -no-multibyte-chars
endif
FTN90PP=-fpp -free
FTN90PPOPT=-Tf
else
FC=gfortran
CC=gcc
GFLAG=-fbounds-check -ffpe-trap=invalid,zero,overflow -Wconversion -Wsurprising -Wintrinsic-shadow -Wtarget-lifetime
ifeq ($(shell test $$(gcc -dumpversion | cut -d '.' -f 1) -gt 5; echo $$?), 0)
  GFLAG+= -Winteger-division
endif
LFLAG=-c -g -fbacktrace
CFLAG=
FTN90PP=-x f95 -cpp -ffree-form -ffree-line-length-none -fcray-pointer
FTN90PPOPT=
endif

# Override debugging options if 'DEBUG' not enabled.
ifndef DEBUG
GFLAG=
ifndef SYMBOLS
LFLAG=-c -O2
ifeq ($(DIST),intel)
LFLAG=-c -O2 -fp-model precise
endif
endif
CLEANUP=@$(MAKE) -s clean DIST=$(DIST)
endif

# Override compile options if 'DOUBLE' enabled.
ifdef DOUBLE
ifeq ($(DIST),intel)
LFLAG+=-r8
else
LFLAG+=-fdefault-double-8 -fdefault-real-8
endif
endif

# Output: sa_mesh.
OUT=sa_mesh

# If MPI is enabled, switch to OMPI compiler and rename output.
# Otherwise add MPI stub to 'OBJECTS'.
ifeq ($(MPI),ompi)
ifeq (,$(shell which mpifort))
FC=mpif90
else
FC=mpifort
endif
CC=mpicc
OUT=mpi_sa_mesh
else
OBJECTS:=	mpi_stub.o $(OBJECTS)
endif

# Override 'rm' with 'del' and add static option for MinGW.
ifeq ($(DIST),mingw)
BIN_DEL=del
LLINK=-static
FC=gfortran
CC=gcc
OUT=sa_mesh_static
else
BIN_DEL=rm
endif

# ======================================================================
# General rules.
%.o: %.f
	$(FC) $(LFLAG) $(GFLAG) $(LIBNCO) $<
%.o: %.F90
	$(FC) $(FTN90PP) $(LFLAG) $(GFLAG) $(INC_DIRS) $(DFLAG) $(LIBNCO) $(FTN90PPOPT) $<
%.o: %.f90
	$(FC) $(FTN90PP) $(LFLAG) $(GFLAG) $(LIBNCO) $<
%.o: %.for
	$(FC) $(LFLAG) $(GFLAG) $(LIBNCO) $<
%.o: %.c
	$(CC) $(LFLAG) $(CFLAG) $(INC_DIRS) $<

# ======================================================================
# Special rules.
EF_Module.o: EF_ParseUtilities.o
%.o: %.ftn90
	$(FC) $(FTN90PP) $(LFLAG) $(INC_DIRS) $(DFLAG) $(FTN90PPOPT)$<

# ======================================================================
# Files renamed for SVS.
runsvs_mod.o: runsvs_mod_sa_mesh.ftn90
	$(FC) $(FTN90PP) $(LFLAG) $(INC_DIRS) $(DFLAG) -o runsvs_mod.o $(FTN90PPOPT)$<

# ======================================================================
# Make target: all
# Deletes object and modules files unless 'DEBUG' has a value.
all: ${OBJECTS}
	$(FC) $(OBJECTS) -o $(OUT) $(LLINK) $(LIBNCL)
	$(CLEANUP)

# ======================================================================
# Make target: clean
# Remove object and module files.
clean:
	-$(BIN_DEL) *.mod *.o

# ======================================================================
# Make target: veryclean
# Remove object and module files, and output file.
veryclean: clean
	-$(BIN_DEL) $(OUT)
