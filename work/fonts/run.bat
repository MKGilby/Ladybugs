@echo off
echo This script copies fonts to data.

call ..\setenv.bat

%WORKTOOLSDIR%\fontbuild2 data\timerfont.png timerfont.png -charset "0123456789:"
%WORKTOOLSDIR%\fontbuild2 data\font.png font.png -charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrstuvwxyz!#22#27@.,-/=:;?" 
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

copy data\npi69.mkr %WORKDATADIR% /Y

