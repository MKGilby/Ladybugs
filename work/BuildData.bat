@echo off
rem This script prepares all data needed by the debug version of the game.
rem This is required before building a release version!

call setenv.bat
if not exist %SOURCEDATADIR%\ (mkdir %SOURCEDATADIR%) else (del /Q %SOURCEDATADIR%\*)

cd gfx
call run.bat
cd ..
cd maps
call run.bat
cd ..
cd fonts
call run.bat
cd ..

