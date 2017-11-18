#!/bin/bash
#
# bsub -q q_hpc -oo salida_postwrf -n2 -R "span[hosts=1]" './sapr99.csh'
#  sapr99.csh
#
#
#  Creado por Jose Agustin Garcia Reynoso el 17/11/17.
#
#  Proposito:
#         Realiza la secuencia de pasos para generar diferentes fechas
#         del inventario de emisiones usando mecanismo SAPRC99.
#  Modificaciones:
#         17/11/2017 For SAPRC99 emissions
#
export ProcessDir=$PWD
echo $ProcessDir
#
#  Build the fecha.txt file

mes=5
dia=22
 
while [ $dia -le 22 ]
do
    echo $dia
    cd $ProcessDir/04_temis

    echo $PWD
    #
    if [ -e fecha.txt ]
    then
        rm fecha.txt
    fi
    ln -sf anio2014.csv.org  anio2014.csv
    #
cat << End_Of_File > fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
    #
    echo ' '
    echo '  Month = '$mes ' Day = '$dia
    #
    echo 'Fixed sources temporal distribution'
    cd ../07_puntual/
    ./Puntual.exe >& ../fsources.log &
    echo 'Mobile temporal distribution'
    cd ../06_temisM/
    ./Mtemporal.exe > ../Mobile.log &
    echo 'Area temporal distribution'
    cd ../04_temis/
    ./Atemporal.exe  >& ../area.log

    #
    #
    echo 'Speciation distribution PM2.5'
    #
    cd ../09_pm25spec
    ./spm25p.exe >> ../fsources.log &
    ./spm25m.exe >> ../Mobile.log &
    ./spm25a.exe >> ../area.log&
    #
    echo 'Speciation distribution VOCs'
    #
    cd ../08_spec
    echo '    SAPRC99 *****'
    ln -sf profile_saprc99.csv profile_mech.csv
    echo 'Mobile'
    ./spm.exe >> ../Mobile.log &
    echo 'Fixed sources'
    ./spp.exe >> ../fsources.log  &
    echo 'Area '
    ./spa.exe >> ../area.log
    #
    echo ' Generating NETCDF emissions file'

    cd ../10_storage
    ./saprc.exe > ../saprc.log
    (( dia++ ))
done
#ncrcat -O wrfchemi.d01.radm2.2016-04-1* wrfchemi_d01_2016-04-16_00:00:00
#mv wrfchemi_d01_2016-04-16_00:00:00 ../../DOMAIN/mecanismos/emisiones
echo 'DONE  guarda'
