# Emiss_mexico_2014

Sistema para convertir emisiones anuales a formato WRF-Chem (año base 2014).

[![bash](https://img.shields.io/badge/bash-%E2%89%A54.0-blue?logo=gnu-bash)](#construccion)
[![Language: Fortran](https://img.shields.io/badge/Language-Fortran%2090-orange.svg)]()
[![Institution](https://img.shields.io/badge/Institution-CCA%20UNAM-red.svg)](https://www.atmosfera.unam.mx/)
[![WRF-Chem](https://img.shields.io/badge/Model-WRF--Chem-lightblue.svg)](https://ruc.noaa.gov/wrf/wrf-chem/)

## Descripción

Inventario de emisiones para modelación de calidad del aire con **WRF-Chem** para la región
**Nacional** (año base 2014). Incluye emisiones de contaminantes criterio (CO, NOₓ, SO₂,
PM₂.₅, PM₁₀, COV) organizadas en sectores: fuentes móviles, fuentes de área y fuentes de punto.

El sistema de compilación está basado en **GNU Autotools**, que reemplaza al script `Compila`
original (J. A. García Reynoso, 26/07/2017).

---

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
├── 10_storage/      # Lee salidas anteriores y genera archivo NetCDF para WRF-Chem
├── 12_cmaq/         # Generación del inventario para el modelo CMAQ (opcional)
├── README.md
└── .gitignore
```

---

## Requisitos del sistema

- Fortran 90/95 o superior (`gfortran` ≥ 9, `ifort`, `ifx`, o `flang`)
- Bibliotecas **NetCDF-Fortran** (`libnetcdf`, `libnetcdff`)
- **GNU Autotools** (`autoconf`, `automake`) para la compilación
- WRF-Chem (para usar las emisiones generadas)
- Python 3.x (para scripts de pre/post-procesamiento, opcional)

---

## Construcción

### Compilación rápida

```bash
./autogen.sh          # solo la primera vez o si cambia configure.ac
./configure
make -j8
make install          # opcional
```

Con Intel y NetCDF (equivalente al script original):

```bash
export NETCDF=/opt/netcdf
./configure FC=ifort
make -j8
```

Con **gfortran en macOS** (Homebrew):

```bash
brew install gcc netcdf netcdf-fortran automake autoconf
./autogen.sh
./configure FC=gfortran-14 --with-netcdf=$(brew --prefix netcdf-fortran)
make -j8
```

### Opciones de `configure`

| Opción | Efecto |
|---|---|
| `FC=ifort` \| `FC=ifx` \| `FC=gfortran` | Elige compilador (se busca en ese orden) |
| `--with-netcdf=DIR` | Raíz de NetCDF-Fortran; por omisión usa `$NETCDF` o `nf-config` |
| `--enable-optimization=N` | Nivel `-ON`, por omisión 3 |
| `--disable-avx` | No usar banderas de arquitectura nativa (`-axAVX`, `-march=native`, `-mcpu=native`) |
| `--disable-varfmt` | Forzar `-DPGI` aunque el compilador admita `format(<n>...)` |
| `--enable-debug` | `-O0 -g` con verificaciones y traceback |
| `--disable-exe-suffix` | Ejecutables sin la extensión `.exe` |
| `FCFLAGS="..."` | Si se define, sustituye a las banderas automáticas |

Si no se encuentra NetCDF-Fortran, `configure` avisa y omite el directorio `10_storage`;
los demás programas se compilan igual.

### macOS y Apple Silicon

`configure` detecta la arquitectura (`AC_CANONICAL_HOST`) y elige la bandera que corresponde:

| Equipo | Bandera |
|---|---|
| Apple Silicon (arm64) | `-mcpu=native` (respaldo: `-mcpu=apple-m1`, `-mtune=native`) |
| x86_64 con Intel `ifort`/`ifx` | `-axAVX` |
| x86_64 con GNU u otros | `-march=native` (respaldo: `-mavx`) |

Cada bandera se prueba antes de usarla; si el compilador no la acepta, simplemente se omite.

### Preprocesador y `format(<n>...)`

Las fuentes `.f90` (minúscula) no se preprocesan por omisión. Como el código usa `#ifndef PGI`,
`configure` detecta la bandera adecuada (`-cpp` en GNU, `-fpp` en Intel, `-Mpreprocess` en NVIDIA)
y la aplica a todas las fuentes.

`gfortran`, NVIDIA/PGI y `flang` no admiten expresiones de formato variables tipo `format(I3,<n>(...))`,
extensión de Intel/Cray. En ese caso se compila con `-DPGI` y el código usa la rama de repetición fija.

> **Nota:** el descriptor `F` sin ancho también es extensión de Intel y `gfortran` lo rechaza.
> Se recomienda usar `F0.3` (u otro ancho explícito), aceptado por ambos compiladores:
>
> ```fortran
> 310 format(I9,",",I6,",",F0.3,",",F0.3,60(",",ES12.5))
> ```
>
> Con `F0.3` y el descriptor de repetición ilimitada `*(...)` de Fortran 2008, el `#ifdef` ya
> no es necesario porque ambas ramas quedan iguales y portables:
>
> ```fortran
> 300 format(I3,", g_per_year",*(",",A10))
> 310 format(I9,",",I6,",",F0.3,",",F0.3,*(",",ES12.5))
> ```

---

## Programas generados

| Directorio | Fuente | Ejecutable |
|---|---|---|
| `02_aemis` | `area_espacial.F90` | `ASpatial.exe` |
| `03_movilspatial` | `suma_carretera.F90` | `carr.exe` |
| `03_movilspatial` | `suma_vialidades.F90` | `vial.exe` |
| `03_movilspatial` | `agrega.f90` | `agrega.exe` |
| `04_temis` | `atemporal.f90` | `Atemporal.exe` |
| `05_semisM` | `movil_spatial.F90` | `MSpatial.exe` |
| `06_temisM` | `movil_temp.f90` | `Mtemporal.exe` |
| `07_puntual` | `t_puntal.F90` | `Puntual.exe` |
| `08_spec` | `agg_a.f90`, `agg_m.f90`, `agg_p.f90` | `spa.exe`, `spm.exe`, `spp.exe` |
| `09_pm25spec` | `pm25_speci_{a,m,p}.F90` | `spm25{a,m,p}.exe` |
| `10_storage` | `g_2014_racm.f90`, `g_cbm5_2014.f90`, `g_radm_2014.f90`, `g_saprc_2014.f90` | `racm2.exe`, `cbm5.exe`, `radm2.exe`, `saprc.exe` |

---

## Uso

1. Preparar los datos de entrada en `01_datos/`
2. Editar las variables `mes` y `dia` dentro de **`ejecuta.sh`** (generado por `configure`)
3. Compilar con `make -j8` (ver sección [Construcción](#construccion))
4. Ejecutar la cadena completa:

```bash
./ejecuta.sh      # o bien:  make run
```

### Orden de etapas y paralelismo

| Etapa | Directorio(s) | Descripción | Depende de |
|------:|--------------|-------------|------------|
| 1 | `02_aemis` | Distribución espacial de área | — |
| 2a | `04_temis` | Distribución temporal de área *(paralelo)* | — |
| 2b | `03_movilspatial` | Carreteras + vialidades *(paralelo)* | — |
| 3 | `03_movilspatial` | Agregación de móviles | 2b |
| 4 | `05_semisM` | Distribución espacial de móviles | 3 |
| 5 | `06_temisM` | Distribución temporal de móviles | 4 |
| 6 | `07_puntual` | Distribución temporal de fuentes fijas | — |
| 7 | `08_spec`, `09_pm25spec` | Especiación VOC + PM₂.₅ *(paralelo)* | 2a, 5, 6 |
| 8 | `10_storage` | Generación NetCDF para WRF-Chem | 7 |

### Log de tiempos (`ejecuta.log`)

Al finalizar, `ejecuta.log` incluye un resumen de tiempos por etapa y proceso:

```
=======================================================
  RESUMEN DE TIEMPOS  (2026-09-20 11:58:29)
=======================================================
  Proceso / Etapa                            Tiempo
  -----------------------------------------  -------
  02_aemis/ASpatial                            6s
  Etapa 1  (area espacial)                     6s
  Etapa 2a (carr+vial paralelo)                1s
  03_movilspatial/agrega                       0s
  Etapa 3  (agrega movil)                      0s
  05_semisM/MSpatial                           1s
  Etapa 4  (movil espacial)                    1s
  06_temisM/Mtemporal                          3s
  Etapa 5  (movil temporal)                    3s
  07_puntual/Puntual                          19s
  Etapa 6  (puntual temporal)                 19s
  Etapa 7  (especiacion VOC+PM2.5 paralelo)   20s
  10_storage/radm2                            57s
  Etapa 8  (NetCDF RADM2)                     57s
  -----------------------------------------  -------
  TOTAL (tiempo real)                        1m 47s
=======================================================
```

La salida final (archivo NetCDF para WRF-Chem) se guarda en **`10_storage/`**.
Para generar el inventario para CMAQ, usar el directorio `12_cmaq/`.

### Otros blancos de `make`

```bash
make clean        # borra objetos, módulos y ejecutables
make distclean    # además borra Makefiles y config.status
make dist         # tarball distribuible
make distcheck    # verifica que el tarball compila desde cero
make -C 08_spec   # compila un solo directorio
```

> La compilación en paralelo la maneja `make -jN`; los `&` y `wait` son internos de `ejecuta.sh`.

---

## Referencia

Si utiliza este código en su investigación, por favor cite:

> García-Reynoso, J.A. et al. Inventario de Emisiones **Nacional** (año base 2014) para
> modelación de calidad del aire.  
> Centro de Ciencias de la Atmósfera, UNAM.  
> https://github.com/JoseAgustin/Emiss_mexico_2014

---

## Autor

**José Agustín García Reynoso**  
Centro de Ciencias de la Atmósfera, UNAM  
📧 agustin@atmosfera.unam.mx  
🔗 https://github.com/JoseAgustin

## Licencia

Ver archivo [LICENSE](LICENSE) para detalles.
