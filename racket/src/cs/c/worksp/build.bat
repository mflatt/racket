set COMPILED_SCM_DIR=../build/compiled/
setlocal

set RACKET=..\..\..\..\racket.exe
if "%1"=="" goto rktdone
set RACKET=%1
:rktdone

set SCHEME_DIR=..\..\..\build\ChezScheme
if "%2"=="" goto csdone
set SCHEME_DIR=%2
:csdone

cl rbuildmode.c
rbuildmode.exe
if errorlevel 1 (set MACHINE=ti3nt) else (set MACHINE=ta6nt)
if "%3"=="" goto machdone
set MACHINE=%3
:machdone

echo Racket is %RACKET%
echo Chez Scheme build directory is %SCHEME_DIR%
echo Machine type is %MACHINE%

if exist %SCHEME_DIR% goto csdirdone
git clone git@github.com:mflatt/ChezScheme %SCHEME_DIR%
if errorlevel 1 exit /B 1
:csdirdone

if exist %SCHEME_DIR%\zlib\Makefile goto cssubdone
pushd %SCHEME_DIR%
git submodule init
if errorlevel 1 exit /B 1
git submodule update
if errorlevel 1 exit /B 1
popd
:cssubdone

%RACKET% prep-cs.rkt --scheme %SCHEME_DIR% --machine %MACHINE%

pushd %SCHEME_DIR%
cd %MACHINE%
cd c
nmake Makefile.%MACHINE%
popd

FOR /F "tokens=* USEBACKQ" %%F IN (`%RACKET% relify.rkt ..\cs\c\worksp %RACKET%`) DO (
SET REL_RACKET=%%F
)
if errorlevel 1 exit /B 1

set SCHEME=%SCHEME_DIR%\%MACHINE%\bin\%MACHINE%\scheme.exe

FOR /F "tokens=* USEBACKQ" %%F IN (`%RACKET% relify.rkt ..\cs\c\worksp %SCHEME%`) DO (
SET REL_SCHEME=%%F
)
if errorlevel 1 exit /B 1

set COMPDIR=..\..\..\build\compiled

set CHAIN_RACKET=%REL_RACKET% -W info@compiler/cm -l- setup --chain ../setup-go.rkt ../build/compiled

pushd ..\..\..\expander
set EXPANDER_RKTL=..\build\compiled\expander.rktl
set EXPANDER_DEP=..\build\expander.d
if not exist %EXPANDER_DEP% echo # > %EXPANDER_DEP%
nmake %EXPANDER_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %EXPANDER_RKTL% %EXPANDER_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
popd

pushd ..\..\..\thread
set THREAD_RKTL=..\build\compiled\thread.rktl
set THREAD_DEP=..\build\compiled\thread.d
set EXP_THREAD_DEP=..\build\expander_thread.d
if not exist %THREAD_DEP% echo # > %THREAD_DEP%
if not exist %EXP_THREAD_DEP% echo # > %EXP_THREAD_DEP%
nmake %THREAD_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %THREAD_RKTL% %EXP_THREAD_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
popd

pushd ..\..\..\io
set IO_RKTL=..\build\compiled\io.rktl
set IO_DEP=..\build\compiled\io.d
set EXP_IO_DEP=..\build\expander_io.d
if not exist %IO_DEP% echo # > %IO_DEP%
if not exist %EXP_IO_DEP% echo # > %EXP_IO_DEP%
nmake %IO_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %IO_RKTL% %EXP_IO_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
popd

pushd ..\..\..\regexp
set REGEXP_RKTL=..\build\compiled\regexp.rktl
set REGEXP_DEP=..\build\compiled\regexp.d
set EXP_REGEXP_DEP=..\build\expander_regexp.d
if not exist %REGEXP_DEP% echo # > %REGEXP_DEP%
if not exist %EXP_REGEXP_DEP% echo # > %EXP_REGEXP_DEP%
nmake %REGEXP_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %REGEXP_RKTL% %EXP_REGEXP_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
popd

pushd ..\..\..\schemify

set SCHEMIFY_RKTL=..\build\compiled\schemify.rktl
set SCHEMIFY_DEP=..\build\compiled\schemify.d
set EXP_SCHEMIFY_DEP=..\build\expander_schemify.d
if not exist %SCHEMIFY_DEP% echo # > %SCHEMIFY_DEP%
if not exist %EXP_SCHEMIFY_DEP% echo # > %EXP_SCHEMIFY_DEP%

set KNOWN_RKTL=..\build\compiled\known.rktl
set KNOWN_DEP=..\build\compiled\known.d
set EXP_KNOWN_DEP=..\build\expander_known.d
if not exist %KNOWN_DEP% echo # > %KNOWN_DEP%
if not exist %EXP_KNOWN_DEP% echo # > %EXP_KNOWN_DEP%

nmake %SCHEMIFY_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %SCHEMIFY_RKTL% %EXP_SCHEMIFY_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
nmake %KNOWN_RKTL% BUILDDIR=..\build\ RACKET="%CHAIN_RACKET% %KNOWN_RKTL% %EXP_KNOWN_DEP%" DIRECT_DEP=""
if errorlevel 1 exit /B 1
popd

pushd ..\..
set CONVERT_RACKET=%CHAIN_RACKET% convert ..\build\convert.d
nmake ../build/racket.so RACKET="%REL_RACKET%" SCHEME="%REL_SCHEME%" BUILDDIR="../build/" CONVERT_RACKET="%CONVERT_RACKET%"
popd

pushd ..\..\..\worksp\
call rktio.bat
popd

set RAW_RACKETCS=..\..\..\build\raw_racketcs.exe
set RACKETCS=..\..\..\..\racketcs.exe

nmake %RAW_RACKETCS% SCHEME_DIR="%SCHEME_DIR%" MACHINE="%MACHINE%"
if errorlevel 1 exit /B 1

%RACKET% ..\embed-boot.rkt %RAW_RACKETCS% %RACKETCS% %SCHEME_DIR%\%MACHINE%\boot\%MACHINE% ..\..\..\build\racket.so
if errorlevel 1 exit /B 1
