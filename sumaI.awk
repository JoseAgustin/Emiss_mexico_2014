NR>3 { for (i=4;i<=NF;i++) a+=$i }
END{ printf a OFS; printf "\n"}
# awk -F"," -f sumaI.awk 02_aemis/INOx_2014.csv

