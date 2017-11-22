#  
#	Lower level Makefile for 04_temis
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objects = atemporal.o

AR      =       ar ru
CD      =       cd
LN      =       ln -s
MAKE    =       make -i -f Makefile
RM      =       /bin/rm -f
RM_LIST =       *.mod *.o core .tmpfile *.exe  TA*2014.csv index.csv
INTEL_LIB =     /opt/intel/lib

# Ejecuta
default:
	uname -a > .tmpfile
	grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Linux" ;               \
	( $(MAKE) all							\
        "RM             = $(RM)"        "RM_LIST        = $(RM_LIST)"   \
        "LN             = $(LN)"        "MACH           = DEC"          \
        "MAKE           = $(MAKE)"      "CPP            = fpp"  \
        "CPPFLAGS       = -I. -C "      \
        "FC             = ifort"        "FCFLAGS        = -O3 -axAVX  -align commons" \
        "LDOPTIONS      = -O3 -axAVX  -align commons " \
        "CFLAGS         = -I."          \
        "LOCAL_LIBRARIES=  " ) ; \
	else \
	grep Darwin .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Darwin" ;              \
	( $(MAKE) all                          \
	"RM             = $(RM)"        "RM_LIST        = $(RM_LIST)"   \
	"LN             = $(LN)"        "MACH           = DEC"          \
	"MAKE           = $(MAKE)"      "CPP            = fpp"  \
	"CPPFLAGS       = -I. -C "      \
	"FC             = ifort"        "FCFLAGS        = -fast -axAVX  -align commons" \
	"LDOPTIONS      = -fast -axAVX  -align commons " \
	"CFLAGS         = -I."          \
	"LOCAL_LIBRARIES=  " ) ; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." \
	fi ; \
	fi ; \
	fi ; \

all :	Atemporal.exe

Atemporal.exe: $(objects)
	$(FC) -o $@ $(objects) $(LDOPTIONS)

atemporal.o: atemporal.f90
	$(FC) -c $< $(FCFLAGS)
 
clean:
		$(RM) $(RM_LIST)
