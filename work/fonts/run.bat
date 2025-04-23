@echo off
echo This script copies fonts to data.

call ..\setenv.bat

%TOOLSDIR%\fontbuild2 data\timerfont.png timerfont.png -charset "0123456789:"
for %%i in (*.png) do %TOOLSDIR%\pngout %%i %DATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

copy data\npi69.mkr %DATADIR% /Y

