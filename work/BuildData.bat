rem This script prepares all data needed by the debug version of the game.
rem This is required before building a release version!

@echo off
if not exist ..\data\ (mkdir ..\data) else (del /Q ..\data\*)

cd gfx
call run.bat
cd ..
cd maps
call run.bat
cd ..
cd fonts
call run.bat
cd ..

