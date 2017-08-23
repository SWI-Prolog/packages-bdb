dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_SUBST(BDB_TARGETS)
AC_SUBST(BDB_PLTARGETS)

m4_include([../ac_swi_c.m4])

if test -z "$LIBDB"; then LIBDB=db; fi
if test -z "$CIFLAGS" -a ! -z "$CPPFLAGS"; then CIFLAGS="$CPPFLAGS"; fi

AC_ARG_WITH(db, [  --with-db=DIR           DIR holding DB installation],
	    [ case $withval in
		no|yes) echo 'Please specify dir holding DB'
			exit 1
			;;
		*)	CIFLAGS="-I$withval/include $CIFLAGS"
		        CPPFLAGS="-I$withval/include $CPPFLAGS"
		        LIBS="-L$withval/lib $LIBS"
			LDFLAGS="-L$withval/lib $LDFLAGS"
			if test -z "$LD_RUN_PATH"; then
			  LD_RUN_PATH="$withval/lib"
			else
			  LD_RUN_PATH="$withval/lib:$LD_RUN_PATH"
			fi
			;;
              esac
	    ])

AC_CHECK_HEADERS(db.h db3/db.h db4/db.h db5/db.h db6/db.h)
AC_MSG_CHECKING(db_create in -ldb)
LIBS="-l$LIBDB $LIBS"
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <db.h>]],
				[[db_create(0,0,0)]])],
	       [HAVEDB=yes
	        AC_MSG_RESULT(yes)
	       ],
	       [HAVEDB=no
	        AC_MSG_RESULT(no)
	       ])

AC_MSG_CHECKING(Whether environment is sane)
if test "$HAVEDB" = "yes"; then
   AC_MSG_RESULT(library ok);
else
   AC_MSG_RESULT(Cannot find db library.  Skipping);
   nodb=yes
fi

AC_MSG_CHECKING(Whether environment is sane)
if test "$ac_cv_header_db_h" = yes ; then
   AC_MSG_RESULT(header ok);
else
   AC_MSG_RESULT(Cannot find db.h.  Skipping);
   nodb=yes
fi

AC_MSG_CHECKING("set_rpc_server() in db_env")
AC_TRY_COMPILE(
[
#include <db.h>
],
[ DB_ENV *db_env;

  db_env->set_rpc_server(db_env, 0, "localhost", 0L, 0L, 0);
], AC_DEFINE(HAVE_SET_RPC_SERVER, 1,
	     [Define if db_env provides set_rpc_server])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

AC_MSG_CHECKING("set_server() in db_env")
AC_TRY_COMPILE(
[
#include <db.h>
],
[ DB_ENV *db_env;

  db_env->set_server(db_env, "localhost", 0L, 0L, 0);
], AC_DEFINE(HAVE_SET_SERVER, 1,
	     [Define if db_env provides set_server])
   AC_MSG_RESULT(yes),
   AC_MSG_RESULT(no))

if test "$nodb" = yes; then
  BDB_TARGETS=nolib
  cat << EOF
##################################################################
# ERROR: Could not find library db (-db).  Dropped library(bdb)
# Make sure you have the BerkeleyDB development library installed.
##################################################################
EOF
else
  BDB_TARGETS="bdb4pl.$SO"
  BDB_PLTARGETS=bdb.pl
fi

AC_OUTPUT(Makefile)




