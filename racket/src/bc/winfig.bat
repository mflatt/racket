@echo off
setlocal

set SRCDIR=%~dp0

REM This script can be run directlly, but it is normally run when
REM `nmake` is used in the root of a checkout of the Git repository for
REM Racket, or by "../../build.zuo" after "../../winfig.bat" is used

set SLSP_SUFFIX=

:argloop
shift
set ARG=%0
if defined ARG (
  if "%ARG%"=="/nonatipkg" set SPLS_SUFFIX=-nonatipkg && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

copy /y "%SRCDIR%\buildmain.zuo" main.zuo > NUL
echo srcdir=%SRCDIR% > Makefile
echo CPPFLAGS=/DWIN32 >> Makefile
echo CFLAGS=/Ox /GS- >> Makefile
echo INSTALL_MISSING_PKGS=dist >> Makefile
echo INSTALL_SETUP_FLAGS=--no-user >> Makefile
echo SPLS_SUFFIX=%SPLS_SUFFIX% >> Makefile

cl.exe /nologo /Fe: winfig.exe "%SRCDIR%\..\worksp\winfig.c"
winfig.exe >> Makefile
