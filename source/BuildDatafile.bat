@echo off
call ..\work\setenv.bat
if not exist ..\release\ (mkdir ..\release)
if not exist ..\release\x64 (mkdir ..\release\x64)
if not exist ..\release\x86 (mkdir ..\release\x86)
if exist ..\release\x64\Ladybugs.data del ..\release\x64\Ladybugs.data
%SOURCETOOLSDIR%\mad4 ..\release\x64\Ladybugs.data ..\data * -1 -r -n
copy /y ..\release\x64\Ladybugs.data ..\release\x86
