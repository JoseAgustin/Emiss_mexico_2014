#
# This file is processed by automake (produces file Makefile.in)
# Cofiguration file for emiss_2014

include $(top_srcdir)/common.am

# Todos estos programas usan la interfaz Fortran de NetCDF
AM_FCFLAGS += $(NETCDF_FCFLAGS)
LDADD       = $(NETCDF_LIBS)

bin_PROGRAMS = Aanual
bin_PROGRAMS += Mannual
bin_PROGRAMS += Panual

# Additional files to be cleaned with 'make clean'
Aanual_SOURCES = check.F90 master_mod.F90 areaanual.F90
Mannual_SOURCES = check.F90 master_mod.F90 movil_anual.F90
Panual_SOURCES   = check.F90 master_mod.F90 f_anual.F90
