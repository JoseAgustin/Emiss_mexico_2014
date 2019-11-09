#	Top-level Makefile for Emiss_mexico conversion program

#	Macros, these should be generic for all machines

.IGNORE:
MAKE    =       make -i -f Makefile
LN      =    ln -s
CD      =       cd
RM	=	/bin/rm -f 
RM_LIST =	*.log
#	Targets for supported architectures

default:
	@echo " "
	@echo "Type one of the following:"
	@echo "make intel              for compiling with ifort"
	@echo "make pgi                for PGI fortran"
	@echo "make clean                to remove all .o files, the core file and the executable"
	@echo " "

intel:
	( $(CD) 02_aemis   ; $(MAKE) intel );\
	( $(CD) 03_movilspatial ; $(MAKE) -j 3 intel );\
	( $(CD) 04_temis   ; $(MAKE) intel );\
	( $(CD) 05_semisM  ; $(MAKE) intel );\
	( $(CD) 06_temisM  ; $(MAKE) intel );\
	( $(CD) 07_puntual ; $(MAKE) intel );\
	( $(CD) 08_spec    ; $(MAKE) -j 3 intel);\
	( $(CD) 09_pm25spec; $(MAKE) -j 3 intel);\
	( $(CD) 10_storage ; $(MAKE) -j 4 intel)

pgi:
	( $(CD) 02_aemis ; $(MAKE) pgi);\
	( $(CD) 03_movilspatial ; $(MAKE) pgi);\
	( $(CD) 04_temis   ; $(MAKE) pgi  );\
	( $(CD) 05_semisM  ; $(MAKE) pgi  );\
	( $(CD) 06_temisM  ; $(MAKE) pgi  );\
	( $(CD) 07_puntual ; $(MAKE) pgi  );\
	( $(CD) 08_spec    ; $(MAKE) -j 3 pgi );\
	( $(CD) 09_pm25spec; $(MAKE) -j 3 pgi );\
	( $(CD) 10_storage ; $(MAKE) -j 3 pgi )

clean:
	( $(CD) 02_aemis   ; $(MAKE) clean  );\
	( $(CD) 03_movilspatial ; $(MAKE) clean);\
	( $(CD) 04_temis   ; $(MAKE) clean  );\
	( $(CD) 05_semisM  ; $(MAKE) clean  );\
	( $(CD) 06_temisM  ; $(MAKE) clean  );\
	( $(CD) 07_puntual ; $(MAKE) clean  );\
	( $(CD) 08_spec    ; $(MAKE) clean  );\
	( $(CD) 09_pm25spec; $(MAKE) clean  );\
	( $(CD) 10_storage ; $(MAKE) clean  );\
	$(RM) $(RM_LIST)

