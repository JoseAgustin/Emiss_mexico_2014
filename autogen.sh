#!/bin/sh
#  Genera configure y los Makefile.in a partir de configure.ac / Makefile.am
#
#  IMPORTANTE: usar SIEMPRE este script (o "autoreconf"), nunca "autoconf"
#  a secas. Las macros propias (JA_FC_TRY_FLAG, JA_FC_ARCH_FLAG, etc.) viven
#  en m4/ja_fortran.m4 y solo "aclocal" (que autoreconf invoca primero) las
#  recoge en aclocal.m4. Si se corre "autoconf" sin haber corrido antes
#  "aclocal" -o con un aclocal.m4 desactualizado- esas macros quedan sin
#  definir; autoconf las deja como texto literal en "configure" y al
#  ejecutarlo se obtienen errores de sintaxis de shell como:
#
#    ./configure: line NNNN: syntax error near unexpected token `-fcheck=all,'
#
set -e
mkdir -p build-aux m4
autoreconf --install --force --verbose
echo "Listo. Ahora: ./configure && make"
