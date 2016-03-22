@echo off
setlocal
rem figure out the folder name
set MONETDB=C:\Program Files\MonetDB\MonetDB5
rem extend the search path with our EXE and DLL folders
rem we depend on pthreadVC2.dll having been copied to the lib folder
set PATH=%MONETDB%\bin;%MONETDB%\lib;%MONETDB%\lib\MonetDB5;%PATH%
rem prepare the arguments to mserver5 to tell it where to put the dbfarm
if "%APPDATA%" == "" goto usevar
rem if the APPDATA variable does exist, put the database there
set MONETDBDIR=C:\Users\BR\Documents\GitHub\DCI\MonetDB\
set MONETDBFARM="--dbpath=C:\Users\BR\Documents\GitHub\DCI\MonetDB\test"
goto skipusevar
:usevar
rem if the APPDATA variable does not exist, put the database in the
rem installation folder (i.e. default location, so no command line argument)
set MONETDBDIR=%MONETDB%\var\MonetDB5
set MONETDBFARM=
:skipusevar
rem the SQL log directory used to be in %MONETDBDIR%, but we now
rem prefer it inside the dbfarm, so move it there
if not exist "%MONETDBDIR%\sql_logs" goto skipmove
for /d %%i in ("%MONETDBDIR%"\sql_logs\*) do move "%%i" "%MONETDBDIR%\test"\%%~ni\sql_logs
rmdir "%MONETDBDIR%\sql_logs"
:skipmove
rem start the real server
"%MONETDB%\bin\mserver5.exe" --set "prefix=%MONETDB%" --set "exec_prefix=%MONETDB%" %MONETDBFARM% %* --set mapi_port=50000
if ERRORLEVEL 1 pause
endlocal
