@echo off
call ..\work\setenv.bat
if not exist lib\ (mkdir lib) else (del /Q lib\*)
%FPCDIR%\fpc.exe -Twin64 -Px86_64 -l- -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\SDL2 -Fu. -FUlib -FE..\release\x64 -oLadybugs.exe Ladybugs.lpr
RMDIR "lib" /S /Q

