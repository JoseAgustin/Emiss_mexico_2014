#!/bin/bash
#
#  Suma emisiones 
#
cd 02_aemis
ainv=(A*csv)
cd ..
num=${#ainv[@]}
num=$((num-1))
for (( i=0; i<=$num ;i++))
do
 echo "${ainv[$i]:1:4}"
 awk -F"," -f suma.awk 02_aemis/${ainv[$i]}
done

