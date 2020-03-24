NR>2 { for (i=5;i<=NF;i++) a+=$i }
END{ printf a/1000000 OFS; printf "\n"}

