@echo off
echo This script prepares gfx.

call ..\setenv.bat

%TOOLSDIR%\BuildBugs
%TOOLSDIR%\BuildMushrooms
%TOOLSDIR%\mkconv2 convert.mc2
for %%i in (*.png) do %TOOLSDIR%\pngout %%i %DATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

