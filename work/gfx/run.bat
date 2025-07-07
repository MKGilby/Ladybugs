@echo off
echo This script prepares gfx.

call ..\setenv.bat

%WORKTOOLSDIR%\BuildGFX
%WORKTOOLSDIR%\mkconv2 convert.mc2
rem del mushroom.png
del bugs.png
del painters.png
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

