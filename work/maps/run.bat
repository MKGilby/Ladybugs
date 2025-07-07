@echo off
echo This script copies maps to data.

call ..\setenv.bat

copy JSONMaps\*.json %WORKDATADIR%\maps /Y
copy pass\*.txt %WORKDATADIR%\maps /Y
