#!/bin/bash
#
# bsub -q q_hpc -oo salida_postwrf -n2 -R "span[hosts=1]" './abril_2016.csh'
#  abril_2017.csh
#
#
#  Creado por Jose Agustin Garcia Reynoso el 26/07/17.
#
#  Proposito:
#         Realiza la secuencia de pasos para generar diferentes fechas
#         del inventario de emisiones.
#  Modificaciones:
#         14/08/2013 Actualizacion para IE del 2014
#         14/10/2017 para bash
#SBATCH -J emiss
#SBATCH -o emis_papila.o%j
#SBATCH -n 4
#SBATCH --ntasks-per-node=44
#SBATCH -p workq
ProcessDir=$PWD
echo $ProcessDir
#
#  Build the fecha.txt file
mes=1
dia1=27 
dia=27
((diaf=dia1+2))
if [ $dia1 -lt 10 ]; then
outfile=wrfchemi_d01_2015-0$mes-0${dia1}_00:00:00
else
outfile=wrfchemi_d01_2015-0$mes-${dia1}_00:00:00
fi
echo $outfile
# Inicia 
while [ $dia -le $diaf ] ;do
echo $dia
cd $ProcessDir/04_temis
echo $PWD
#
if [ -e fecha.txt ]; then
rm fecha.txt
fi
#    Aqui cambiar el año a modelar
#
ln -sf anio2015.csv  anio2014.csv
#
cat << End_Of_File > fecha.txt
$mes       ! month jan =1 to dec=12
$dia       ! day in the month (from 1 to 28,30 or 31)
End_Of_File
#
echo ' '
echo '  Mes ='$mes 'DIA '$dia
#
echo 'Area Temporal distribution'
./Atemporal.exe  > ../area.log &
echo 'Point Temporal distribution'
cd ../07_puntual/
./Puntual.exe >& ../puntual.log 
echo 'Movil Temporal distribution'
cd ../06_temisM/
./Mtemporal.exe > ../movil.log &
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
ln -sf profile_radm2.csv profile_mech.csv
echo 'Movile'
./spm.exe >> ../movil.log &
echo 'Puntual'
./spp.exe >> ../puntual.log  &
echo 'Area '
./spa.exe >> ../area.log
#
#echo '    CBM05 *****'
#ln -sf profile_cbm05.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log
#echo '    SAPRC99 *****'
#ln -sf profile_saprc99.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log
#
#echo '    RACM2 *****'
#ln -sf profile_racm2.csv profile_mech.csv
#echo 'Movile'
#./spm.exe >> ../movil.log &
#echo 'Puntual'
#./spp.exe >> ../puntual.log  &
#echo 'Area '
#./spa.exe >> ../area.log

echo ' Guarda'

cd ../10_storage
./radm2015.exe  > ../radm2.log
#./saprc.exe > ../saprc.log
#./cbm5.exe  > ../cbm5.log
#./racm2.exe > ../racm2.log
 let dia=dia+1
done
mv  wrfchemi.d01.RADM2.2015-0${mes}* /LUSTRE/ID/FQA/agustin/CIGOM/suma_radm/
cd /LUSTRE/ID/FQA/agustin/CIGOM/suma_radm
let diap=dia1-1
if [ -e wrfchemi.d01.RADM2.2015-0${mes}-${diap}_00:00:00 ];then
   rm wrfchemi.d01.RADM2.2015-0${mes}-${diap}_*
fi
file00=(`ls  wrfchemi.d01.RADM2*_00* `)
file12=(`ls  wrfchemi.d01.RADM2*_12* `)
for ((n=0;n<${#file00[*]}; n++)) ; do
ln -sf ${file00[$n]} wrfchemi_00z_d01_mx
ln -sf ${file12[$n]} wrfchemi_12z_d01_mx
./suma_chem.exe
mv wrfchemi_00z_d01 wrfchemi_${n}_00Z
mv wrfchemi_12z_d01 wrfchemi_$[n]_12Z
done
echo "Genera archivo concatenado"
ncrcat -O wrfchemi_*Z $outfile
mv $outfile /LUSTRE/ID/FQA/agustin/papila/wrfprd
echo "DONE  genera_2015"
exit 0
