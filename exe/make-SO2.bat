@echo off
REM Note the path below must be modified to match locations on your machine
REM The first path referencing GnuWin32 should be changed to the correct needed-exes\bin directory
REM The second path for R should be double checked to make sure that it is pointing to the correct version

SET PATH=%PATH%;C:\Users\<username>\<pathway_to_CEDS>\CEDS\exe;C:\Program Files\R\<R_version>\bin\x64;


SET R_LIBS_USER=C:\Users\<username>\<pathway_to_R>\R\win-library\3.2

REM By default we will run "make all" however by changing CMD=all to CMD=clean it will clean
REM the data system instead which is useful to do for instance when you update the data system
IF [%1]==[] ( SET CMD=SO2-emissions ) ELSE ( set CMD=%1 )

cd ..\
make %CMD%

pause