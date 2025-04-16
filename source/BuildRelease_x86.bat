@echo off
if not exist lib\ (mkdir lib) else (del /Q lib\*)
echo Compiling x86 release...
%FPCDIR%\fpc.exe -l- -Twin32 -Pi386 -MDelphi -Scghi -CX -O3 -XX -v0 -vm6058,5024 -Filib -Fuunits -Fuunits\SDL2 -Fu. -FUlib\ -FE..\release\x86 -oLadybugs.exe Ladybugs.lpr
RMDIR "lib" /S /Q

