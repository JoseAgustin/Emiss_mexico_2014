#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
#  Sistema de construccion para la conversion del Inventario de Emisiones
#  Basado en el script Compila original (J. A. Garcia Reynoso, 26/07/2017)
#
AC_PREREQ([2.69])
AC_INIT([emisiones-ie], [2014.1], [agustin@atmosfera.unam.mx], [emisiones-ie])
AC_CONFIG_SRCDIR([02_aemis/area_espacial.F90])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
AC_CANONICAL_HOST
AM_INIT_AUTOMAKE([foreign 1.13 -Wall subdir-objects])
AM_MAINTAINER_MODE([enable])

# ---------------------------------------------------------------------
# Compilador Fortran
# ---------------------------------------------------------------------
dnl Si el usuario no fijo FCFLAGS, no dejamos el "-g -O2" que autoconf
dnl pone por omision: anularia a OPT_FCFLAGS al ir despues en la orden.
ja_user_FCFLAGS="$FCFLAGS"
AC_PROG_FC([ifx ifort gfortran nvfortran flang pgf90])
AS_IF([test -z "$ja_user_FCFLAGS"], [FCFLAGS=""])
AC_LANG([Fortran])
AC_FC_SRCEXT([f90])
AC_FC_PP_SRCEXT([F90])
AC_FC_FREEFORM

# ---------------------------------------------------------------------
# Sufijo .exe en los ejecutables (como en el script original)
#   --disable-exe-suffix  -> nombres sin extension
# ---------------------------------------------------------------------
AC_ARG_ENABLE([exe-suffix],
  [AS_HELP_STRING([--disable-exe-suffix],
     [generar ejecutables sin la extension .exe (por omision se usa .exe)])],
  [], [enable_exe_suffix=yes])
AS_IF([test "x$enable_exe_suffix" = xyes], [EXEEXT=.exe], [EXEEXT=""])
AC_SUBST([EXEEXT])

# ---------------------------------------------------------------------
# Banderas de optimizacion
#   --enable-optimization=LEVEL   (default 3)
#   --disable-avx                 (no usar -axAVX / -mavx)
#   --enable-debug                (-g -O0 y verificaciones)
# ---------------------------------------------------------------------
AC_ARG_ENABLE([optimization],
  [AS_HELP_STRING([--enable-optimization@<:@=NIVEL@:>@],
     [nivel de optimizacion 0..3 (por omision 3)])],
  [], [enable_optimization=3])
AC_ARG_ENABLE([avx],
  [AS_HELP_STRING([--disable-avx],
     [no usar banderas de arquitectura nativa (-axAVX / -mcpu=native)])],
  [], [enable_avx=yes])
AC_ARG_ENABLE([debug],
  [AS_HELP_STRING([--enable-debug], [compilar con depuracion y verificaciones])],
  [], [enable_debug=no])

OPT_FCFLAGS=""
OPT2_FCFLAGS=""
AVX_FCFLAGS=""
AS_IF([test "x$enable_debug" = xyes],
  [OPT_FCFLAGS="-g -O0"
   JA_FC_TRY_FLAG([-fcheck=all],  [OPT_FCFLAGS], [JA_FC_TRY_FLAG([-check all], [OPT_FCFLAGS])])
   JA_FC_TRY_FLAG([-fbacktrace],  [OPT_FCFLAGS], [JA_FC_TRY_FLAG([-traceback], [OPT_FCFLAGS])])
   OPT2_FCFLAGS="$OPT_FCFLAGS"],
  [AS_IF([test "x$enable_avx" = xyes], [JA_FC_ARCH_FLAG([AVX_FCFLAGS])])
   OPT_FCFLAGS="-O$enable_optimization$AVX_FCFLAGS"
   OPT2_FCFLAGS="-O2$AVX_FCFLAGS"])
AC_SUBST([OPT_FCFLAGS])
AC_SUBST([OPT2_FCFLAGS])

# Modulos .mod en el directorio de compilacion
JA_FC_MODULE_FLAG

# ---------------------------------------------------------------------
# Preprocesado y expresiones de formato variables <n>
#   Las fuentes .f90 (minuscula) tambien necesitan el preprocesador por
#   las directivas #ifndef PGI.  Si el compilador no admite  format(<n>...)
#   -- gfortran, NVIDIA/PGI, flang -- se compila con -DPGI.
# ---------------------------------------------------------------------
JA_FC_PP_FLAG
JA_FC_VARFMT

# ---------------------------------------------------------------------
# NetCDF-Fortran (requerido solo por 10_storage)
# ---------------------------------------------------------------------
JA_NETCDF_FORTRAN

# ---------------------------------------------------------------------
AC_CONFIG_FILES([Makefile
                 02_aemis/Makefile
                 03_movilspatial/Makefile
                 04_temis/Makefile
                 05_semisM/Makefile
                 06_temisM/Makefile
                 07_puntual/Makefile
                 08_spec/Makefile
                 09_pm25spec/Makefile
                 10_storage/Makefile])
AC_CONFIG_FILES([ejecuta.sh], [chmod +x ejecuta.sh])
AC_OUTPUT

AC_MSG_NOTICE([
  ----------------------------------------------------------
   $PACKAGE_NAME $PACKAGE_VERSION
   Compilador Fortran .... $FC
   Sistema ............... $host
   Banderas .............. $OPT_FCFLAGS $FC_PPFLAGS $FC_DEFS $FCFLAGS
   Formato variable <n> .. ${ja_varfmt}
   Sufijo ejecutables .... ${EXEEXT:-(ninguno)}
   NetCDF Fortran ........ $ja_netcdf_summary
   Prefijo instalacion ... $prefix
  ----------------------------------------------------------
])
