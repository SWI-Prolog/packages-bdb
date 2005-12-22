################################################################
# Build the SWI-Prolog XML/SGML package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include ..\..\src\rules.mk
PKGDLL=db4pl

DBDIR=		C:\Program Files\Sleepycat Software\Berkeley DB 4.3.29
LIBDB=		libdb43
DBLIB=		"$(DBDIR)\lib\$(LIBDB).lib"
DBDLL=		"$(DBDIR)\bin\$(LIBDB).dll"
DBDEFS=		-DHAVE_SET_RPC_SERVER

INCLUDE=$(INCLUDE);$(DBDIR)\include

CFLAGS=$(CFLAGS) $(DBDEFS)

OBJ=		db4pl.obj atom.obj error.obj

all:		$(PKGDLL).dll

printenv::
		echo "%INCLUDE%"
		echo "%LIB%"

$(PKGDLL).dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(DBLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

idll::
		copy $(PKGDLL).dll "$(PLBASE)\bin"
		copy $(DBDLL) "$(PLBASE)\bin"
ilib::
		copy db.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(PLBASE)\bin\$(PKGDLL).dll"
		del "$(PLBASE)\bin\$(LIBDB).dll"
		del "$(PLBASE)\library\db.pl"
		$(MAKEINDEX)

html-install::
		copy doc\db4pl.html "$(PKGDOC)"

clean::
		DEL *.obj *~

distclean:	clean
		DEL $(PKGDLL).dll $(PKGDLL).lib $(PKGDLL).exp

