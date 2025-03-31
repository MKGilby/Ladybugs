@echo off
if exist ..\release\x64\Ladybugs.data del ..\release\x64\Ladybugs.data
..\tools\mad4 ..\release\x64\Ladybugs.data ..\data * -1 -r -n
copy /y ..\release\x64\Ladybugs.data ..\release\x86
