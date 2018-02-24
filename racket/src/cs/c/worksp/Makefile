
INCS = /I.. /I..\..\..\rktio /I$(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE) \
       /I..\..\..\worksp\librktio

RKTIO_LIB = ..\..\..\build\librktio.lib
WIN32_LIBS = WS2_32.lib Shell32.lib User32.lib RpCrt4.lib Ole32.lib

LIBS = $(RKTIO_LIB) \
       $(SCHEME_DIR)\$(MACHINE)\boot\$(MACHINE)\csv951mt.lib \
       $(WIN32_LIBS)

DEST = ..\..\..\build\raw_racketcs.exe

$(DEST): ..\main.c ..\boot.c $(RKTIO_LIB)
	cl /Fe$(DEST) /Ox /MT $(INCS) ..\main.c ..\boot.c $(LIBS)


# Useful for debugging:

RKTIO_DLL = ..\..\..\..\lib\librktio.dll

rktio-dll:
	cl /Fe$(RKTIO_DLL) /LD $(RKTIO_LIB) WS2_32.lib Shell32.lib User32.lib ..\..\..\rktio\rktio.def /link /machine:x64
