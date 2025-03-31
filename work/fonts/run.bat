@echo off
echo This script copies fonts to data.

call ..\setenv.bat

copy data\npi69.mkr %DATADIR% /Y
