#  
#	Lower level Makefile for 09_pm25spec
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objec1 = pm25_speci_a.o
objec2 = pm25_speci_m.o
objec3 = pm25_speci_p.o
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
	"FC             = ifort"        "FCFLAGS        = -D$(MACH) -I. -convert big_endian -fast”\
	"LDOPTIONS      = -convert big_endian -fast”  "CFLAGS         = -I."          \
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
all :	spm25a.exe spm25m.exe spm25p.exe

spm25a.exe: $(objec1)
	$(FC) -o $@ $(objec1) $(LDOPTIONS)

pm25_speci_a.o: pm25_speci_a.f90
	$(FC) -c $< $(FCFLAGS)
 
spm25m.exe: $(objec2)
	$(FC) -o $@ $(objec2) $(LDOPTIONS)

pm25_speci_m.o: pm25_speci_m.f90
	$(FC) -c $< $(FCFLAGS)

spm25p.exe: $(objec3)
	$(FC) -o $@ $(objec3) $(LDOPTIONS)

pm25_speci_p.o: pm25_speci_p.f90
	$(FC) -c $< $(FCFLAGS)


clean:
		$(RM) $(RM_LIST)
