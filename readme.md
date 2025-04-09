# Ladybugs
Remake of Logical (C) 1991 Rainbow Arts and Cat's Eye Chaos (C) 2003 JP Hamilton

This version written by Szabó "Gilby" Zsolt. This version copyright 2025 MKSZTSZ.

## Programming language
FreePascal (Lazarus 4.0.0rc3 with FPC 3.2.2 Windows x64 or x86 version or cross-compiler to Win32 or Win64)
[Lazarus homepage](https://www.lazarus-ide.org/)

## Source codes
SDL2 pascal headers (source\units\sdl2) is licensed under MPL or zlib license.
GitHub for SDL2 pascal headers: [PascalGameDevelopment/SDL2-for-Pascal](https://github.com/PascalGameDevelopment/SDL2-for-Pascal)

The rest of source code is licensed under GNU GPL v3 (or later).

## Tools
PNGOut tool is by Ken Silverman [His homepage](http://advsys.net/ken)

MKConv2, MAD4, BuildBugs and BuildMushrooms tools are made by me.

## Compiling
1. Go into tools\source and run BuildTools.bat
2. Go into work and run BuildData.bat
3. Go into source and run BuildRelease_x64.bat (or x86 as you wish.)
   You need Lazarus cross compiler libraries to be installed
   to compile x64 on x86 systems and vice-versa.

## Compiled binaries from current build with datafiles and DLLs
[x64](https://mksztsz.hu/tmpfiles/Ladybugs_0.0.0.7.zip "Download x64 version") or
[x86](https://mksztsz.hu/tmpfiles/Ladybugs_x86_0.0.0.7.zip "Download x86 version").

## What's new

### 2025.04.09 - Build 7
- Added moving bug counter. You can't launch more bugs from mushrooms when
  the limit of moving bugs is reached. (All pots have flowers in them.)
  You can set this limit in each map json file in the range of 4-16.
- Added flying bug sprites.  
- Collected all sprites into one spritesheet.
- Updated used units. 

### 2025.04.04 - Build 6
- You can click on bugs with the left mouse button to release them on paths 
  (or into another mushroom).

### 2025.04.04 - Build 5
- Added FPS display.
- You can rotate mushrooms with right mouse button. Bugs fall into empty slots. 
  Cannot launch them yet.

### 2025.04.03 - Build 4
- Ladybugs fall into mushrooms. Cannot do anything with them yet.
- Fixed a bug in BuildBugs: Animation coordinates didn't match with image.
- Added SDL_Init and SDL_Quit. (It worked without this but don't be lazy.)
- You can close window with X. (Only escape key worked before.)
  
### 2025.04.02 - Build 3
- First ladybug appears and moves in the top row.

### 2025.03.31 - Build 2
- Added a small font for debugging purposes.
- Press TAB to show map values.
- Fixed release part of code.
- Map entities sets map values accordingly.
- Since no sound and music yet, changed MediaManager to GFXManager. 
  (Don't need to include Bass and related units yet.)

### 2025.03.31 - Build 1
- Created first map in json format.
- Map is loaded in game.
- Paths and rotating mushrooms are displayed. Upper path need a junction 
  where mushrooms are below.

