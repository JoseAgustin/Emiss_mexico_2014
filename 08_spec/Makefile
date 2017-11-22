#  
#	Lower level Makefile for 08_spec
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objec1 = agg_a.o
objec2 = agg_m.o
objec3 = agg_p.o
RM_LIST =  *.mod *.o core .tmpfile *.exe RA*.txt SAPRC9*txt CBM05*txt
RM	=	/bin/rm -f 
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
	"FC             = ifort"        "FCFLAGS        = -O2 -axAVX  -align commons " \
	"LDOPTIONS      = -O2 -axAVX  -align commons "  "CFLAGS         = -I."          \
	"LOCAL_LIBRARIES= " ) ; \
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

# Ejecuta
all :	spa.exe spm.exe spp.exe

spa.exe: $(objec1)
	$(FC) -o $@ $(objec1) $(LDOPTIONS)

agg_a.o: agg_a.f90
	$(FC) -c $< $(FCFLAGS)
 
spm.exe: $(objec2)
	$(FC) -o $@ $(objec2) $(LDOPTIONS)

agg_m.o: agg_m.f90
	$(FC) -c $< $(FCFLAGS)

spp.exe: $(objec3)
	$(FC) -o $@ $(objec3) $(LDOPTIONS)

agg_p.o: agg_p.f90
	$(FC) -c $< $(FCFLAGS)


clean:
		$(RM) $(RM_LIST)
