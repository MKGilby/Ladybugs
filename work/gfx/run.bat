@echo off
echo This script prepares gfx.

call ..\setenv.bat

%WORKTOOLSDIR%\BuildBugs
%WORKTOOLSDIR%\BuildMushrooms
%WORKTOOLSDIR%\mkconv2 convert.mc2
del mushroom.png
del bugs.png
for %%i in (*.png) do %WORKTOOLSDIR%\pngout %%i %WORKDATADIR%\%%i /y /kanMZ,fnTZ,anIM /f0
del *.png

