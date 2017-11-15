#  
#	Lower level Makefile for 02_aemis
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objec1 = suma_carretera.o
objec2 = suma_vialidades.o
objec3 = agrega.o


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
