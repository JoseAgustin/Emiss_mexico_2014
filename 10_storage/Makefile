#  
#	Lower level Makefile for 10_storage
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objec1 = g_radm_2014.o
objec2 = g_2014_racm.o
objec3 = g_saprc_2014.o
objec4 = g_cbm5_2014.o

RM_LIST =       *.mod *.o core .tmpfile *.exe

# Ejecuta
default:
	uname -a > .tmpfile
	grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Linux" ;               \
	( $(MAKE) all                               \
	"RM             = $(RM)"        "RM_LIST        = $(RM_LIST)"   \
	"LN             = $(LN)"        "MACH           = DEC"          \
	"MAKE           = $(MAKE)"      "CPP            = /lib/cpp"     \
	"CPPFLAGS       = -I. -C -traditional"  \
	"FC             = ifort"   "CFLAGS         = -I."       \
	"LDOPTIONS      = -fast -axAVX  -align commons  -lnetcdff -L$(NETCDF)/lib ”          \
	"FCFLAGS        = -fast -axAVX  -align commons -I$(NETCDF)/include ”
	"LOCAL_LIBRARIES= " ) ; \
	else \
	grep Darwin .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Darwin" ;              \
	( $(MAKE) all                          \
	"RM             = $(RM)"        "RM_LIST        = $(RM_LIST)"   \
	"LN             = $(LN)"        "MACH           = DEC"          \
	"MAKE           = $(MAKE)"      "CPP            = fpp"  \
	"CPPFLAGS       = -I. -C "      \
	"FC             = ifort"        "CFLAGS         = -I." \
	"LDOPTIONS      = -fast -axAVX  -align commons  -lnetcdff -L$(NETCDF)/lib  " \
	"FCFLAGS        = -fast -axAVX  -align commons -I$(NETCDF)/include"          \
	"LOCAL_LIBRARIES=  " ) ; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." \
	fi ; \
	fi ; \
	fi ; \

# Ejecuta
all :	radm2.exe racm2.exe saprc.exe cbm5.exe

radm2.exe: $(objec1)
	$(FC) -o $@ $(objec1) $(LDOPTIONS)

g_radm_2014.o: g_radm_2014.f90
	$(FC) -c $< $(FCFLAGS)
 
racm2.exe: $(objec2)
	$(FC) -o $@ $(objec2) $(LDOPTIONS)

g_2014_racm.o: g_2014_racm.f90
	$(FC) -c $< $(FCFLAGS)

saprc.exe: $(objec3)
	$(FC) -o $@ $(objec3) $(LDOPTIONS)

g_saprc_2014.o: g_saprc_2014.f90
	$(FC) -c $< $(FCFLAGS)

cbm5.exe: $(objec4)
	$(FC) -o $@ $(objec4) $(LDOPTIONS)

g_cbm5_2014.o: g_cbm5_2014.f90
	$(FC) -c $< $(FCFLAGS)

clean:
		$(RM) $(RM_LIST)
