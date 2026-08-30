# Emiss_mexico_2014
Model system for convert annual emissions to WRF-chem ready emissions.

[![bash](https://img.shields.io/badge/bash-%E2%89%A54.0-blue?logo=gnu-bash)](#requisitos-del-sistema)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran%2090-orange.svg)]()
[![Institution](https://img.shields.io/badge/Institution-CCA%20UNAM-red.svg)](https://www.atmosfera.unam.mx/)
[![WRF-Chem](https://img.shields.io/badge/Model-WRF--Chem-lightblue.svg)](https://ruc.noaa.gov/wrf/wrf-chem/)

## Descripción

Inventario de emisiones para modelación de calidad del aire con **WRF-Chem** para la región **Nacional** (año base 2014).
Incluye emisiones de contaminantes criterio (CO, NOₓ, SO₂, PM₂.₅, PM₁₀, COV) organizadas en sectores:
fuentes móviles, fuentes de área y fuentes de punto.

## Estructura del repositorio

```
Emiss_mexico_2014/
├── 01_datos/        # Información para la distribución espacial en la malla correspondiente
├── 02_aemis/        # Distribución espacial de las emisiones de área
├── 03_movilspatial/ # Agrupa la malla de las emisiones por vialidades y carreteras
├── 04_temis/        # Distribución temporal de las emisiones de área (anual → horaria)
├── 05_semisM/       # Distribución espacial de emisiones de fuentes móviles
├── 06_temisM/       # Distribución temporal de las emisiones de fuentes móviles (anual → horaria)
├── 07_puntual/      # Distribución temporal de las emisiones de fuentes fijas (anual → horaria)
├── 08_spec/         # Especiación de COV según el mecanismo químico a usar
├── 09_pm25spec/     # Especiación de PM2.5 en especies inorgánicas y orgánicas
├── 10_storge/       # Lee salidas anteriores y genera archivo NetCDF para WRF-Chem
├── 12_cmaq/         # Generación del inventario para el modelo CMAQ (opcional)
├── README.md
└── .gitignore
```

## Requisitos

- Fortran 90/95 o superior
- Bibliotecas NetCDF (libnetcdf, libnetcdff)
- WRF-Chem (para usar las emisiones generadas)
- Python 3.x (para scripts de pre/post-procesamiento, opcional)

## Uso

1. Preparar los datos de entrada en `01_datos/`
2. Editar el mes y día en el archivo **`emis_2014.sh`**
3. Ejecutar los scripts en orden secuencial (directorios 02 al 09)
4. La salida final (archivo NetCDF para WRF-Chem) se guarda en **`10_storge/`**
5. Para generar el inventario para CMAQ, usar el directorio `12_cmaq/`

## Referencia

Si utiliza este código en su investigación, por favor cite:

> García-Reynoso, J.A. et al. Inventario de Emisiones **Nacional** (año base 2014) para modelación de calidad del aire.
> Centro de Ciencias de la Atmósfera, UNAM. https://github.com/JoseAgustin/Emiss_mexico_2014

## Autor

**José Agustín García Reynoso**  
Centro de Ciencias de la Atmósfera, UNAM  
📧 agustin@atmosfera.unam.mx  
🔗 https://github.com/JoseAgustin

## Licencia

Ver archivo [LICENSE](LICENSE) para detalles.
