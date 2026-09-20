dnl ---------------------------------------------------------------------
dnl JA_FC_TRY_FLAG(FLAG, VARIABLE, [ACCION-SI-FALLA])
dnl   Agrega FLAG a VARIABLE si el compilador Fortran la acepta.
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_FC_TRY_FLAG],[
  AC_LANG_PUSH([Fortran])
  ja_save_FCFLAGS="$FCFLAGS"
  FCFLAGS="$FCFLAGS $1"
  AC_MSG_CHECKING([si $FC acepta $1])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],[])],
    [AC_MSG_RESULT([si]); ja_flag_ok=yes],
    [AC_MSG_RESULT([no]);  ja_flag_ok=no])
  FCFLAGS="$ja_save_FCFLAGS"
  AC_LANG_POP([Fortran])
  AS_IF([test "x$ja_flag_ok" = xyes], [$2="[$]$2 $1"], [$3])
])

dnl ---------------------------------------------------------------------
dnl JA_FC_MODULE_FLAG
dnl   Determina la bandera que indica donde escribir/leer los .mod
dnl   (-J en gfortran, -module en ifort/ifx, -moddir= en flang).
dnl   Define FC_MODOUT (AC_SUBST).
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_FC_MODULE_FLAG],[
  AC_MSG_CHECKING([bandera para el directorio de modulos Fortran])
  AC_LANG_PUSH([Fortran])
  FC_MODOUT=""
  mkdir -p conftest.dir
  for ja_flag in "-J" "-module " "-moddir=" "-M" "-qmoddir="; do
    ja_save_FCFLAGS="$FCFLAGS"
    FCFLAGS="$FCFLAGS ${ja_flag}conftest.dir"
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
      module conftest_mod
      integer :: i
      end module conftest_mod]])],
      [FC_MODOUT="$ja_flag"])
    FCFLAGS="$ja_save_FCFLAGS"
    test -n "$FC_MODOUT" && break
  done
  rm -rf conftest.dir
  AC_LANG_POP([Fortran])
  dnl El valor exportado ya apunta al directorio de compilacion actual
  AS_IF([test -n "$FC_MODOUT"],
    [FC_MODOUT="${FC_MODOUT}."; AC_MSG_RESULT([$FC_MODOUT])],
    [AC_MSG_RESULT([ninguna])])
  AC_SUBST([FC_MODOUT])
])

dnl ---------------------------------------------------------------------
dnl JA_NETCDF_FORTRAN
dnl   Busca la interfaz Fortran de NetCDF usando, en orden:
dnl     --with-netcdf=DIR, $NETCDF, nf-config, rutas del sistema.
dnl   Define NETCDF_FCFLAGS y NETCDF_LIBS (AC_SUBST) y el condicional
dnl   automake HAVE_NETCDF.
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_NETCDF_FORTRAN],[
  AC_ARG_WITH([netcdf],
    [AS_HELP_STRING([--with-netcdf=DIR],
       [ruta de instalacion de NetCDF-Fortran (por omision $NETCDF)])],
    [], [with_netcdf="$NETCDF"])
  AC_ARG_VAR([NETCDF], [directorio raiz de la instalacion de NetCDF])
  AC_ARG_VAR([NF_CONFIG], [ruta del programa nf-config])

  NETCDF_FCFLAGS=""
  NETCDF_LIBS=""
  ja_netcdf_summary="no encontrado"

  AS_IF([test "x$with_netcdf" != xno],[
    AS_IF([test -n "$with_netcdf" && test -x "$with_netcdf/bin/nf-config"],
      [NF_CONFIG="$with_netcdf/bin/nf-config"],
      [AC_PATH_PROG([NF_CONFIG], [nf-config], [no])])

    AS_IF([test "x$NF_CONFIG" != xno && test -n "$NF_CONFIG"],
      [NETCDF_FCFLAGS=`$NF_CONFIG --fflags 2>/dev/null`
       NETCDF_LIBS=`$NF_CONFIG --flibs 2>/dev/null`],
      [AS_IF([test -n "$with_netcdf"],
         [NETCDF_FCFLAGS="-I$with_netcdf/include"
          NETCDF_LIBS="-L$with_netcdf/lib -lnetcdff"],
         [NETCDF_LIBS="-lnetcdff"])])

    AC_MSG_CHECKING([si se puede enlazar con NetCDF-Fortran])
    AC_LANG_PUSH([Fortran])
    ja_save_FCFLAGS="$FCFLAGS"; ja_save_LIBS="$LIBS"
    FCFLAGS="$FCFLAGS $NETCDF_FCFLAGS"; LIBS="$NETCDF_LIBS $LIBS"
    AC_LINK_IFELSE([AC_LANG_SOURCE([[
      program conftest_nc
      use netcdf
      integer :: ierr
      ierr = nf90_noerr
      end program conftest_nc]])],
      [ja_have_netcdf=yes], [ja_have_netcdf=no])
    FCFLAGS="$ja_save_FCFLAGS"; LIBS="$ja_save_LIBS"
    AC_LANG_POP([Fortran])
    AC_MSG_RESULT([$ja_have_netcdf])
  ],[ja_have_netcdf=no])

  AS_IF([test "x$ja_have_netcdf" = xyes],
    [ja_netcdf_summary="$NETCDF_FCFLAGS $NETCDF_LIBS"],
    [NETCDF_FCFLAGS=""; NETCDF_LIBS=""
     AC_MSG_WARN([Sin NetCDF-Fortran: no se compilara el directorio 10_storage.
                  Use --with-netcdf=DIR o exporte NETCDF.])])

  AC_SUBST([NETCDF_FCFLAGS])
  AC_SUBST([NETCDF_LIBS])
  AM_CONDITIONAL([HAVE_NETCDF], [test "x$ja_have_netcdf" = xyes])
])

dnl ---------------------------------------------------------------------
dnl JA_FC_PP_FLAG
dnl   Bandera que activa el preprocesador de C tambien en fuentes .f90
dnl   (minuscula): -cpp en gfortran/flang, -fpp en Intel, -Mpreprocess en
dnl   NVIDIA/PGI.  Define FC_PPFLAGS (AC_SUBST).
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_FC_PP_FLAG],[
  AC_MSG_CHECKING([bandera de preprocesado para fuentes .f90])
  AC_LANG_PUSH([Fortran])
  FC_PPFLAGS=""
  for ja_flag in -cpp -fpp -Mpreprocess -eZ; do
    ja_save_FCFLAGS="$FCFLAGS"
    FCFLAGS="$FCFLAGS $ja_flag"
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
      program conftest_pp
@%:@ifndef JA_TEST
      print *, "ok"
@%:@endif
      end program conftest_pp]])],
      [FC_PPFLAGS="$ja_flag"])
    FCFLAGS="$ja_save_FCFLAGS"
    test -n "$FC_PPFLAGS" && break
  done
  AC_LANG_POP([Fortran])
  AS_IF([test -n "$FC_PPFLAGS"],
    [AC_MSG_RESULT([$FC_PPFLAGS])],
    [AC_MSG_RESULT([ninguna])
     AC_MSG_WARN([No se hallo bandera de preprocesado; las fuentes .f90 con
                  directivas del preprocesador podrian no compilar.])])
  AC_SUBST([FC_PPFLAGS])
])

dnl ---------------------------------------------------------------------
dnl JA_FC_VARFMT
dnl   Determina si el compilador admite expresiones de formato variables
dnl   del tipo  format(...,<n>(",",A10))  (extension de Intel/Cray).
dnl   gfortran, NVIDIA/PGI y flang NO las admiten: en ese caso se define
dnl   -DPGI para que el codigo use la rama con repeticion fija.
dnl   Define FC_DEFS (AC_SUBST).
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_FC_VARFMT],[
  AC_REQUIRE([JA_FC_PP_FLAG])
  AC_ARG_ENABLE([varfmt],
    [AS_HELP_STRING([--disable-varfmt],
       [forzar -DPGI: no usar expresiones de formato variables <n>])],
    [], [enable_varfmt=check])
  AC_MSG_CHECKING([si $FC admite expresiones de formato variables <n>])
  AS_IF([test "x$enable_varfmt" = xno],
    [ja_varfmt=no],
    [AC_LANG_PUSH([Fortran])
     AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
      program conftest_vfe
      integer :: n
      n = 3
      write(*,300) 1
 300  format(I3,<n>(",",A10))
      end program conftest_vfe]])],
      [ja_varfmt=yes], [ja_varfmt=no])
     AC_LANG_POP([Fortran])])
  AC_MSG_RESULT([$ja_varfmt])
  AS_IF([test "x$ja_varfmt" = xyes], [FC_DEFS=""], [FC_DEFS="-DPGI"])
  AC_SUBST([FC_DEFS])
])

dnl ---------------------------------------------------------------------
dnl JA_FC_ARCH_FLAG(VARIABLE)
dnl   Agrega a VARIABLE la bandera de arquitectura apropiada al equipo:
dnl     arm64 / Apple Silicon .... -mcpu=native  (o -mcpu=apple-m1)
dnl     x86_64 con Intel ......... -axAVX
dnl     x86_64 con GNU/otros ..... -march=native  o  -mavx
dnl ---------------------------------------------------------------------
AC_DEFUN([JA_FC_ARCH_FLAG],[
  AC_REQUIRE([AC_CANONICAL_HOST])
  case $host_cpu in
    aarch64|arm64|arm*)
      JA_FC_TRY_FLAG([-mcpu=native], [$1],
        [JA_FC_TRY_FLAG([-mcpu=apple-m1], [$1],
           [JA_FC_TRY_FLAG([-mtune=native], [$1])])])
      ;;
    *)
      JA_FC_TRY_FLAG([-axAVX], [$1],
        [JA_FC_TRY_FLAG([-march=native], [$1],
           [JA_FC_TRY_FLAG([-mavx], [$1])])])
      ;;
  esac
])
