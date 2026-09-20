#!/bin/bash

#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/17.
#
#  Proposito:
#         Compila los programas que se requieren para hacer la conversion
#
#  Modificaciones:
#         27/07/2017 Actualizacion para IE del 2014
#         Reescrito para gfortran / macOS (ARM + Intel)
#

export ProcessDir=$PWD
echo $ProcessDir

# Flags comunes para gfortran en macOS
FC=gfortran
FFLAGS="-mcpu=native -DPGI"

#
cd 02_aemis/
    $FC -O3 $FFLAGS -o ASpatial.exe area_espacial.F90 &
    wait
    ./ASpatial.exe > ../ejecuta.log &

cd ../03_movilspatial/
    $FC -O2 $FFLAGS -o carr.exe   suma_carretera.F90 &
    $FC -O2 $FFLAGS -o vial.exe   suma_vialidades.F90 &
    $FC -O2 $FFLAGS -o agrega.exe agrega.f90 &
    wait
    ./carr.exe  > ../ejecuta.log &
    ./vial.exe >> ../ejecuta.log &
    wait
    ./agrega.exe >> ../ejecuta.log &

cd ../04_temis/
    $FC -O3 $FFLAGS -o Atemporal.exe atemporal.f90 &

cd ../05_semisM/
    $FC -O3 $FFLAGS -o MSpatial.exe movil_spatial.F90
    ./MSpatial.exe >> ../ejecuta.log &

cd ../06_temisM/
    $FC -O3 $FFLAGS -o Mtemporal.exe movil_temp.f90 &

cd ../07_puntual/
    $FC -O3 $FFLAGS -o Puntual.exe t_puntal.F90 &

cd ../08_spec/
    $FC -O2 $FFLAGS -o spa.exe agg_a.f90
    $FC -O2 $FFLAGS -o spm.exe agg_m.f90
    $FC -O2 $FFLAGS -o spp.exe agg_p.f90 &

cd ../09_pm25spec/
    $FC -O2 $FFLAGS -o spm25a.exe pm25_speci_a.F90 &
    $FC -O2 $FFLAGS -o spm25m.exe pm25_speci_m.F90 &
    $FC -O2 $FFLAGS -o spm25p.exe pm25_speci_p.F90 &

cd ../10_storage/
    $FC -O2 $FFLAGS -o racm2.exe g_2014_racm.f90  -lnetcdff -L$NETCDF/lib -I$NETCDF/include &
    $FC -O2 $FFLAGS -o cbm5.exe  g_cbm5_2014.f90  -lnetcdff -L$NETCDF/lib -I$NETCDF/include &
    $FC -O2 $FFLAGS -o radm2.exe g_radm_2014.f90  -lnetcdff -L$NETCDF/lib -I$NETCDF/include &
    $FC -O2 $FFLAGS -o saprc.exe g_saprc_2014.f90 -lnetcdff -L$NETCDF/lib -I$NETCDF/include

cd ..
echo "Done Compila"
