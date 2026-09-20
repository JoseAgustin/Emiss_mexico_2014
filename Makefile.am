ACLOCAL_AMFLAGS = -I m4

SUBDIRS = 02_aemis 03_movilspatial 04_temis 05_semisM 06_temisM \
          07_puntual 08_spec 09_pm25spec

if HAVE_NETCDF
SUBDIRS += 10_storage
endif

# Script de ejecucion generado por configure
bin_SCRIPTS  = ejecuta.sh
CLEANFILES   = ejecuta.sh
EXTRA_DIST   = common.am ejecuta.sh.in autogen.sh README.md

# make run : ejecuta la cadena completa despues de compilar
run: all
	@$(abs_builddir)/ejecuta.sh
.PHONY: run
