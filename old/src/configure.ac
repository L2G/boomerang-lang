
AC_INIT(harmony, 0.01, alan.schmitt@polytechnique.org)


# LIFTED FROM GHC SOURCE -- JNF
#
# FP_COMPARE_VERSIONS(VERSION1, TEST, VERSION2, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ----------------------------------------------------------------------------------
# Compare dotted version numbers VERSION1 and VERSION2 lexicographically according
# to TEST (one of -eq, -ne, -lt, -le, -gt, or -ge).
AC_DEFUN([FP_COMPARE_VERSIONS],
[fp_version1=$1; fp_version2=$3
fp_save_IFS=$IFS; IFS='.'
while test x"$fp_version1" != x || test x"$fp_version2" != x
do

  set dummy $fp_version1; shift
  fp_num1=""
  test $[@%:@] = 0 || { fp_num1="[$]1"; shift; }
  test x"$fp_num1" = x && fp_num1="0"
  fp_version1="[$]*"

  set dummy $fp_version2; shift
  fp_num2=""
  test $[@%:@] = 0 || { fp_num2="[$]1"; shift; }
  test x"$fp_num2" = x && fp_num2="0"
  fp_version2="[$]*"

  test "$fp_num1" = "$fp_num2" || break;
done
IFS=$fp_save_IFS
AS_IF([test "$fp_num1" $2 "$fp_num2"], [$4], [$5])[]dnl
])# FP_COMPARE_VERSIONS


# locate ocamlc
# N.B. Don't use OCAMLC variable -- it is set in OCamlMakefiles.
AC_CHECK_PROG(OCAML_COMPILER,ocamlc,ocamlc,no)
if test "$OCAML_COMPILER" = no ; then
	AC_MSG_ERROR(Cannot find ocamlc.)
fi

# determine ocamlc version
OCAML_VERSION=`$OCAML_COMPILER -v | sed -n -e 's|.*version *\(.*\)$|\1|p' `
echo "ocaml version is $OCAML_VERSION"

# if OCAML_VERSION > 3.08, then set OCAML_GEQ_308 = 1
FP_COMPARE_VERSIONS([$OCAML_VERSION],[-ge],[3.08.0],[OCAML_GEQ_308=1])
if test "$OCAML_GEQ_308" = 1 ; then
        echo "ocaml version >= 3.08; using new uprintf source for ubase library."
else 
        echo "ocaml version < 3.08; using old uprintf source for ubase library."
fi

AC_ARG_ENABLE(tidy,
              AC_HELP_STRING([--disable-tidy],
                             [disable tidy support (default is tidy enabled)]),
              [case "${enableval}" in
               yes) use_tidy=true ;;
               no)  use_tidy=false ;;
               *)   AC_MSG_ERROR(bad value ${enableval} for --disable-tidy) ;;
              esac], 
              [use_tidy=true])
if test $use_tidy = true; then

##
## we don't use the tidy program do we? --jnf
##
#  AC_CHECK_PROG([TIDY], [tidy], [yes], [no], [$PATH])
#  if test $TIDY = yes; then
    AC_CHECK_LIB(tidy, tidyCreate, ,
                 AC_MSG_ERROR([couldn't find the tidy libraries.]),[])
    AC_CHECK_HEADERS([tidy.h tidyenum.h], [WITH_TIDY=1], 
                     AC_MSG_ERROR([tidy libraries found but not tidy.h or tidyenum.h]), [])
#  fi
fi
if test $WITH_TIDY; then
  echo "Tidy support will be compiled in"
else
  echo "Tidy support will not be compiled in"
fi

AC_SUBST(OCAML_VERSION, $OCAML_VERSION)
AC_SUBST(OCAML_GEQ_308, $OCAML_GEQ_308)
AC_SUBST(WITH_TIDY, $WITH_TIDY)
AC_CONFIG_FILES(config.mk)
AC_OUTPUT
