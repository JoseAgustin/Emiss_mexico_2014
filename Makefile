#	Top-level Makefile for interp2wrf conversion program

#	Macros, these should be generic for all machines

.IGNORE:

AR	=	ar ru
CD	=	cd
LN	=	ln -s
MAKE	=	make -i -f Makefile
RM	=	/bin/rm -f
RM_LIST	=	*.mod *.o *.f core .tmpfile *.exe
INTEL_LIB =	/opt/intel/lib
#	Targets for supported architectures

default:
	uname -a > .tmpfile
		grep Linux .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Linux" ;		\
		( $(CD) src ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= DEC"		\
		"MAKE		= $(MAKE)"	"CPP		= /lib/cpp"	\
		"CPPFLAGS	= -I. -C -traditional"	\
		"FC		= ifort"	"FCFLAGS	= -D$(MACH) -I. -convert big_endian -pc32 -FR -tpp7 -xW"\
		"LDOPTIONS	= -convert big_endian -pc32 -tpp7 -xW"	"CFLAGS		= -I."		\
		"LOCAL_LIBRARIES= " ) ; \
	else \
		grep Darwin .tmpfile ; \
	if [ $$? = 0 ]; then echo "Compiling for Darwin" ;		\
		( $(CD) 02_aemis ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= DEC"		\
		"MAKE		= $(MAKE)"	"CPP		= fpp"	\
		"CPPFLAGS	= -I. -C "	\
		"FC		= ifort"	"FCFLAGS	= -fast -axAVX  -align commons"\
		"LDOPTIONS	= -fast -axAVX  -align commons "\
		"CFLAGS		= -I."		\
		"LOCAL_LIBRARIES=  " ) ; \
		( $(CD) 03_movilspatial ; $(MAKE) all				\
		"RM		= $(RM)" 	"RM_LIST	= $(RM_LIST)"	\
		"LN		= $(LN)" 	"MACH		= DEC"		\
		"MAKE		= $(MAKE)"	"CPP		= fpp"	\
		"CPPFLAGS	= -I. -C "	\
		"FC		= ifort"	"FCFLAGS	= -fast -FR -align commons"\
		"LDOPTIONS	=  -fast -align commons "\
		"CFLAGS		= -I."		\
		"LOCAL_LIBRARIES=  " ) ; \
	else echo "Do not know how to compile for the `cat .tmpfile` machine." \
		fi ; \
		fi ; \
	fi ; \
		( $(RM) interp_emis.exe ; $(LN) src/interp_emis.exe . ) ;

code:
	( $(CD) 02_aemis ; $(MAKE) code					\
	"MAKE			=	$(MAKE)"			\
	"CPPFLAGS		=	-I. -C -P -DDEC"		)

clean:
	( $(CD) 02_aemis   ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" );\
        ( $(CD) 03_movilspatial ; $(MAKE) clean "CD = $(CD)" "RM = $(RM)" "RM_LIST = $(RM_LIST)" )

	$(RM) $(RM_LIST)
