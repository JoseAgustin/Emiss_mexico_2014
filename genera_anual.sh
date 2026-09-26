#!/bin/bash
#
#  Programa para generar archivos netcdf anuales
#  requiere que se corra una vez 
set -euo pipefail

EXEEXT=".exe"
BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$BASE/ejec_anual.log"
: > "$LOG"          # el log se trunca una sola vez, al inicio

# ------------------------------------------------------------------
# Utilidades de tiempo
# ------------------------------------------------------------------
declare -a _TIMING=()   # acumula "etapa|segundos" para el resumen final

# Formatea segundos como  Xm Ys  o  Xs
fmt_elapsed() {
    local s=$1
    if (( s >= 60 )); then
        printf '%dm %02ds' $(( s / 60 )) $(( s % 60 ))
    else
        printf '%ds' "$s"
    fi
}

# Registra la duracion de una etapa en el array y en el log
record_stage() {               # record_stage <etiqueta> <segundos>
    local label="$1" elapsed="$2"
    _TIMING+=("${label}|${elapsed}")
    printf '    Tiempo etapa: %s\n' "$(fmt_elapsed "$elapsed")" >> "$LOG"
}

hace_namelist() {
    cat > namelist_emis.nml <<- End_Of_File
        !
        !   Definicion de variables para calculo del Inventario
        !
        &region_nml
        zona ="$dominio"
        /
        &fecha_nml
        idia=$dia
        month=$mes
        anio=$nyear
        periodo=$nfile
        /
        &verano_nml
        lsummer = .false.
        /
        &chem_nml
        mecha='$MECHA'
        model=$AQM_SELECT
        /
End_Of_File
}
run() {
    local dir="$1" prog="$2"
    local t0=$SECONDS
    printf '\n=== %s  %s/%s ===\n' "$(date '+%F %T')" "$dir" "$prog" >> "$LOG"
    ( cd "$BASE/$dir" && "./${prog}${EXEEXT}" >> "$LOG" 2>&1 )
    local elapsed=$(( SECONDS - t0 ))
    printf '    [%s/%s]  %s\n' "$dir" "$prog" "$(fmt_elapsed "$elapsed")" >> "$LOG"
    _TIMING+=("${dir}/${prog}|${elapsed}")
}
# 1) Se entra al directorio de trabajo
cd $BASE/11_anual
# 2) crea el namelist
 MECHA=radm2
 dominio=mexico
 dia='02'
 mes='01'
 nyear='2014'
 nfile=1
 AQM_SELECT=0

 hace_namelist 1 1
cd $BASE

# 3) ejecuta progama
# ------------------------------------------------------------------
# Inicio
# ------------------------------------------------------------------
T_TOTAL=$SECONDS
printf '=== Anual: inicio  %s ===\n' "$(date '+%F %T')" | tee -a "$LOG"

T_STAGE=$SECONDS

run 11_anual Aanual
record_stage "Etapa 1 Emisiones de area" $(( SECONDS - T_STAGE ))

T_STAGE=$SECONDS
run 11_anual Mannual
record_stage "Etapa 2 Emisiones de moviles" $(( SECONDS - T_STAGE ))

T_STAGE=$SECONDS
run 11_anual Panual
record_stage "Etapa 3 Emisiones de moviles" $(( SECONDS - T_STAGE ))

# ------------------------------------------------------------------
# Resumen de tiempos
# ------------------------------------------------------------------
T_WALL=$(( SECONDS - T_TOTAL ))
{
printf '\n'
printf '=%.0s' {1..55}; printf '\n'
printf '  RESUMEN DE TIEMPOS  (%s)\n' "$(date '+%F %T')"
printf '=%.0s' {1..55}; printf '\n'
printf '  %-42s %s\n' "Proceso / Etapa" "Tiempo"
printf -- '  %-42s %s\n' "-----------------------------------------" "-------"
for entry in "${_TIMING[@]}"; do
    label="${entry%%|*}"
    secs="${entry##*|}"
    printf '  %-42s %s\n' "$label" "$(fmt_elapsed "$secs")"
done
printf -- '  %-42s %s\n' "-----------------------------------------" "-------"
printf '  %-42s %s\n' "TOTAL (tiempo real)" "$(fmt_elapsed "$T_WALL")"
printf '=%.0s' {1..55}; printf '\n'
} | tee -a "$LOG"

printf '\nInventario de Emisiones: proceso terminado. Bitacora en %s\n' "$LOG"


