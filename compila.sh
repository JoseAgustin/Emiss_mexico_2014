#!/bin/bash
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/17.
#
#  Proposito:
#         Compila los programas que se requieren para hacer la conversion
#
#  Modificaciones:
#         27/07/2017 Actualizacion para IE del 2014
#
export ProcessDir=$PWD
echo $ProcessDir
#
#  
cd  02_aemis/
    ifort -o ASpatial.exe -O3 area_espacial.f90 &
cd ../03_movilspatial/
    ifort -O2 -axAVX agrega.f90 -o agrega.exe &
    ifort -O2 -axAVX suma_carretera.f90 -o carr.exe &
    ifort -O2 -axAVX suma_vialidades.f90 -o vial.exe &
cd ../04_temis
   ifort -O3 -axAVX atemporal.f90 -o Atemporal.exe &
cd ../05_semisM
    ifort -O3 -axAVX -o MSpatial.exe movil_spatial.f90 &
cd ../06_temisM
    ifort -O3 movil_temp.f90 -o Mtemporal.exe &
cd ../07_puntual
    ifort -O3 -axAVX  t_puntal.f90 -o Puntual.exe &
cd ../08_spec
    ifort -O2 -axAVX  agg_a.f90 -o spa.exe
    ifort -O2 -axAVX agg_m.f90 -o spm.exe
    ifort -O2 -axAVX  agg_p.f90 -o spp.exe &
cd ../09_pm25spec
ifort -O2 -axAVX  pm25_speci_a.f90 -o spm25a.exe
ifort -O2 -axAVX  pm25_speci_m.f90 -o spm25m.exe
ifort -O2 -axAVX  pm25_speci_p.f90 -o spm25p.exe
cd ../10_storage/
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_2014_racm.f90 -o racm2.exe
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include  g_cbm5_2014.f90 -o cbm5.exe
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_radm_2014.f90 -o radm2.exe
ifort -O2 -axAVX -lnetcdff -L$NETCDF/lib -I$NETCDF/include g_saprc_2014.f90 -o saprc.exe
cd ..
