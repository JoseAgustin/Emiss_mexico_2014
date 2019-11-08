#!/bin/bash -l 
#
#  genera_mozart.sh
#
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/12.
#
#  Proposito:
#         Realiza la secuencia de pasos para generar diferentes fechas
#         del inventario de emisiones.
#  Modificaciones:
#         14/08/2013 Actualizacion para IE del 2008
#         16/01/2018 Para 2018
#
#SBATCH -J emiss
#SBATCH -o emis%j.o
#SBATCH -n 4
#SBATCH --ntasks-per-node=24
#SBATCH -p operativo
# Load your modules here, for instance
module load compiladores/intel/17.4.196
module load herramientas/nco/4.6.7
/opt/intel/compilers_and_libraries_2017.4.196/linux/bin/ifortvars.sh intel64
export LD_LIBRARY_PATH=/opt/librerias/intel/nco/4.6.7/lib:/opt/librerias/intel/netcdf4_intel/lib:$LD_LIBRARY_PATH
export ProcessDir=$PWD
#echo $ProcessDir
#
# Borra archivos anteriores
rm  *.log 
cd ./10_storage
rm wrfchemi.d01.MOZART.2*
cd ..
#  Build the fecha.txt file
echo $PWD
let dian=0
while [ $dian -lt  3 ]
do
export mes=`date -d "$dian days" +%m`
export dia=`date -d "$dian days" +%d`
if [ $dian == 0  ]; then
   export outfile=wrfchemi_d01_2018-${mes}-${dia}_00:00:00
   echo $outfile
fi
#
cd $ProcessDir/04_temis
#
if [ -e fecha.txt ]
 then
rm fecha.txt
fi
ln -sf anio_2018.csv anio2014.csv
#
cat << End_Of_File > fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
#
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe >& ../puntual.log &
echo 'Movil Temporal distribution'
cd ../06_temisM/
./Mtemporal.exe > ../movil.log &
echo 'Area Temporal distribution'
cd ../04_temis/
./Atemporal.exe  > ../area.log
wait
#
#echo 'Biogenic'
#cd ../12_biogenic
#./Btemporal.exe > biog.log&
#
echo 'Speciation distribution PM2.5'
#
cd ../09_pm25spec
./spm25p.exe >> ../puntual.log &
./spm25m.exe >> ../movil.log &
./spm25a.exe >> ../area.log&
#
echo 'Speciation distribution VOCs'
#
cd ../08_spec
echo '   RADM2 *****'
ln -sf profile_mozart.csv profile_mech.csv
echo 'Movile'
./spm.exe >> ../movil.log &
echo 'Puntual'
./spp.exe >> ../puntual.log  &
echo 'Area '
./spa.exe >> ../area.log
wait
#
echo ' Guarda'
cd ../10_storage
./mozart.exe  > ../mozart.log
dian=$[$dian+1]
done 
echo $outfile
/opt/librerias/intel/nco/4.6.7/bin/ncrcat -O wrfchemi.d01.MOZART.20* $outfile
rm /LUSTRE/OPERATIVO/modelos/WRF-CHEM/interpola/wrfchem*
mv $outfile /LUSTRE/OPERATIVO/modelos/WRF-CHEM/interpola
echo "DONE  ******  Guarda MOZART mexico ******"
cd ..
#sbatch genera_mozart.sh
