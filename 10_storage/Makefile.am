include $(top_srcdir)/common.am

# Todos estos programas usan la interfaz Fortran de NetCDF
AM_FCFLAGS += $(NETCDF_FCFLAGS)
LDADD       = $(NETCDF_LIBS)

bin_PROGRAMS   = racm2 cbm5 radm2 saprc
racm2_SOURCES  = g_2014_racm.f90
cbm5_SOURCES   = g_cbm5_2014.f90
radm2_SOURCES  = g_radm_2014.f90
saprc_SOURCES  = g_saprc_2014.f90
