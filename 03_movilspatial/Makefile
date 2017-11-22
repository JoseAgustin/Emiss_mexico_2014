#  
#	Lower level Makefile for 03_movilspatial
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objec1 = suma_carretera.o
objec2 = suma_vialidades.o
objec3 = agrega.o
RM_LIST = *.mod *.o core .tmpfile *.exe salida.csv   salida2.csv  salida3.csv
RM	=	/bin/rm -f 
LN	=	ln -s
MAKE	=	make -i -f Makefile
# Ejecuta
default:
	uname -a > .tmpfile
	grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Linux" ;               \
	( $(MAKE) all                               \
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

# Ejecuta
all :	carr.exe vial.exe agrega.exe

carr.exe: $(objec1)
	$(FC) -o $@ $(objec1) $(LDOPTIONS)

suma_carretera.o: suma_carretera.f90
	$(FC) -c $< $(FCFLAGS)
 
vial.exe: $(objec2)
	$(FC) -o $@ $(objec2) $(LDOPTIONS)

suma_vialidades.o: suma_vialidades.f90
	$(FC) -c $< $(FCFLAGS)

agrega.exe: $(objec3)
	$(FC) -o $@ $(objec3) $(LDOPTIONS)

agrega.o: agrega.f90
	$(FC) -c $< $(FCFLAGS)


clean:
		$(RM) $(RM_LIST)
