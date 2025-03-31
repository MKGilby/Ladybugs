@echo off
echo This script copies maps to data.

call ..\setenv.bat

copy JSONMaps\*.json %DATADIR% /Y
