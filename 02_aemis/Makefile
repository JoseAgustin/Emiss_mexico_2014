#  
#	Lower level Makefile for 02_aemis
#
#       Suffix rules and commands
#######################
FIX01 =
#######################


objects = area_espacial.o

# Ejecuta
all :	ASpatial.exe

ASpatial.exe: $(objects)
	$(FC) -o $@ $(objects) $(LDOPTIONS)

area_espacial.o: area_espacial.f90
	$(FC) -c $< $(FCFLAGS)
 
clean:
		$(RM) $(RM_LIST)
