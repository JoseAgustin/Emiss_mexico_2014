# Emiss_mexico_2014
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Language: Fortran](https://img.shields.io/badge/language-Fortran-blue.svg)](https://fortran-lang.org/)

Sistema de modelación para convertir emisiones anuales del **Inventario Nacional de Emisiones de México** a formato listo para **WRF-Chem**.

## Descripción

Este repositorio contiene un sistema de programas en Fortran para procesar y convertir 
las emisiones anuales del inventario nacional de México al formato de entrada requerido 
por el modelo de calidad del aire WRF-Chem.

Incluye emisiones de:
- **Fuentes móviles** (vehículos de carretera y fuera de carretera)
- **Fuentes de área** (doméstico, comercial, servicios)
- **Fuentes puntuales** (industria)
- **Fuentes biogénicas**

Los mecanismos químicos soportados incluyen RADM2 y MOZART.

## Estructura del repositorio

```
Emiss_mexico_2014/
├── 01_datos/       # Datos del inventario nacional de emisiones
├── doc/            # Documentación técnica detallada
├── README.md
└── .gitignore
```

## Requisitos

- Fortran 90/95 o superior
- Bibliotecas NetCDF (libnetcdf, libnetcdff)
- WRF-Chem v3.x o superior

## Uso

1. Preparar los archivos de emisiones en `01_datos/`
2. Revisar la documentación en `doc/`
3. Compilar los programas y ejecutar en orden
4. Las salidas son archivos NetCDF para WRF-Chem

## Autor

**José Agustín García Reynoso**  
Centro de Ciencias de la Atmósfera, UNAM  
📧 agustin@atmosfera.unam.mx  
🔗 https://www.atmosfera.unam.mx/ciencias-ambientales/fisicoquimica-atmosferica/jose-agustin-garcia-reynoso/

## Licencia

[MIT License](LICENSE)
