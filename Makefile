#	Top-level Makefile for Emiss_mexico conversion program

#	Macros, these should be generic for all machines

.IGNORE:
MAKE    =       make -i -f Makefile
CD      =       cd
RM	=	/bin/rm -f 
RM_LIST =	*.log
#	Targets for supported architectures

default:
	( $(CD) 02_aemis   ; $(MAKE)  );\
	( $(CD) 03_movilspatial ; $(MAKE) -j 3 );\
	( $(CD) 04_temis   ; $(MAKE) );\
	( $(CD) 05_semisM  ; $(MAKE) );\
	( $(CD) 06_temisM  ; $(MAKE) );\
	( $(CD) 07_puntual ; $(MAKE) );\
	( $(CD) 08_spec    ; $(MAKE) -j 3);\
	( $(CD) 09_pm25spec; $(MAKE) -j 3 );\
	( $(CD) 10_storage ; $(MAKE) -j 4 )

code:
	( $(CD) 02_aemis ; $(MAKE) code);\
	( $(CD) 03_movilspatial ; $(MAKE) code);\
	( $(CD) 04_temis   ; $(MAKE) code "FC= ifort" );\
	( $(CD) 05_semisM  ; $(MAKE) code  "FC= ifort");\
	( $(CD) 06_temisM  ; $(MAKE) code  );\
	( $(CD) 07_puntual ; $(MAKE) code  );\
	( $(CD) 08_spec    ; $(MAKE) code  );\
	( $(CD) 09_pm25spec; $(MAKE) code  );\
	( $(CD) 10_storage ; $(MAKE) code  )
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

