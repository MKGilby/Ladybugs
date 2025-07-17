# Ladybugs
Remake of Logical (C) 1991 Rainbow Arts and Cat's Eye Chaos (C) 2003 JP Hamilton

This version written by Szabó "Gilby" Zsolt. This version copyright 2025 MKSZTSZ.

## Programming language
FreePascal (Lazarus 4.0.0 with FPC 3.2.2 Windows x64 or x86 version or cross-compiler to Win32 or Win64)
[Lazarus homepage](https://www.lazarus-ide.org/)

## Source codes
SDL2 pascal headers (source\units\sdl2) is licensed under MPL or zlib license.
GitHub for SDL2 pascal headers: [PascalGameDevelopment/SDL2-for-Pascal](https://github.com/PascalGameDevelopment/SDL2-for-Pascal)

The rest of source code is licensed under GNU GPL v3 (or later).

## Tools
PNGOut tool is by Ken Silverman [His homepage](http://advsys.net/ken)

MKConv2, MAD4, FontBuild2, BuildGFX tools are made by me.

## Graphics and fonts
Graphics is created by me and licensed under CC BY-NC 4.0

Main font are drawn by me based on font "So" by Apostrophic Laboratories.
(I found "So" here: [1001fonts](https://www.1001fonts.com/so-font.html))

## Compiling in windows environment
1. Go into folder "work" and edit setenv.bat, set FPCDIR to point to the folder
containing your fpc.exe
2. Go into folder "tools\source" and run BuildTools.bat
3. Go into folder "work" and run BuildData.bat
4. Go into folder "source" and run BuildRelease_x64.bat (or x86 as you wish.)
   You need Lazarus cross compiler libraries to be installed
   to compile x64 on x86 systems and vice-versa.
5. Download and extract the latest SDL2.dll into \release\x64 or x86
   (be aware of bitness!). The latest DLLs can be found on the [SDL releases page](https://github.com/libsdl-org/SDL/releases).
   Scroll down to the latest 2.xx version, click assets and download file.
   At the time of writing of this document the latest SDL2 version is 2.32.8.

## Compiled binaries from current build with datafiles and DLLs
[x64](https://mksztsz.hu/tmpfiles/Ladybugs_0.0.0.23.zip "Download x64 version") or
[x86](https://mksztsz.hu/tmpfiles/Ladybugs_x86_0.0.0.23.zip "Download x86 version").

## What's new

### 2025.07.17 - Build 23
- Added wait for a click before starting level. This gives you time to study
  the level an make a strategy.
- Level number and password are visible at the bottom of the level.
- You can complete levels. When all mushrooms are flipped the remaining
  bugs flies away. You can click then to proceed to the next level.

### 2025.07.15 - Build 22
- Added menu.
- Clicking "Play" starts level indicated in "Current level".
- Clicking "Enter password" let's you enter a password. If it matches any
  password from the selected password list, the "Current level" indicator
  refreshes, button changes to OK and "Enter password" changes to "Correct
  password!".
- Clicking "Exit" closes the game.
- Font expanded with underscore "_".

### 2025.07.07 - Build 21
- Added logo.
- Added font.
- Added password lists.
- Added Virtual Memory Unit (a glorious config file).
- Added first run choice of passwords. You can choose old passwords or new passwords.
  Old passwords are the ones are used in Logical and Cat's Eye Chaos. New passwords
  are generated for this game by Copilot.
- Mushrooms are put into a separated PNG, datafile size decreased.   

### 2025.07.02 - Build 20
- Increased bug moving speed.
- Increased mushroom rotating speed.
- Added bug timeout multiplier values to map files. This value specifies how
  many times a bug can move move along the upper row before you lose the map.
  This counter resets with every new bug.
- Added bug multiplier timeout. The path where the bugs appear gradually
  darkened from right to left. Once the whole path is dark you will lose
  the map.

### 2025.06.29 - Build 19
- Added the 3 remaining arrow tiles.
- Arrow color changed to black.
- Added color pattern lock. (That cross thing with four colors.)
- Added all original maps. Not all will work, one tile is not yet working.
  (Simple path down from the top row, introduced on map 58.)

### 2025.06.24 - Build 18
- Added arrow graphics.
- Added right arrow and map 9 and 10.

### 2025.06.23 - Build 17
- Added traffic light to the game. The bugs will only fly in the order of the lights.
- Fixed a bug with vertical blocker.

### 2025.06.16 - Build 16
- Added painter to game. It paints the passing bug to the color of the painter.
- Added a brush to the top of the painter graphics.

### 2025.06.11 - Build 15
- Teleports are working. You can specify teleport groups, bug will teleport
  beetween teleports belonging the same group. (Now all the teleports look
  the same, planning to add customization options.)
- If more than one teleport suitable to receive the bug one will be chosen randomly.
- Fixed an invalid typecast error when top row bug moved over a non-mushroom object.

### 2025.06.10 - Build 14
- Mushrooms are made bigger. They reach the edges of the tile.
- Moving reworked: If a bug reaches (but not yet crosses) the egde of the tile
  and the next tile is mushroom it instantly jumps into the mushroom slot or
  turns back if slot is occupied or mushroom is rotating.
- The same when jumping out of mushrooms: bug starts moving entirely out of the 
  mushroom tile. If the adjacent tile is another mushroom with free slot, 
  instantly jumps into it.
- Added teleport tile. Not working yet.  
  
### 2025.05.20 - Build 13
- Added color blocker. It only lets the matching color bug to pass,
  all the others are turned back.
- Map structure changed, path data is separated from tiles.  
- Added a few more maps. 

### 2025.04.29 - Build 12
- Fixed bug horizontal displacement. It now follows the curves of the road.
- Path made brighter and a bit more yellow. It is more pleasant now.
- Added next indicator. It shows the color of the next bug. 

### 2025.04.23 - Build 11
- Added timer font.
- Added timer entity. It shows the remaining time for the map.
- Added timer to the map. (It stops when reaching 0 but nothing will happen.)
- Increased bug moving speed.
- Increased mushroom rotating speed.
- Bugs flying off a little angled.  
- Bug entities are freed when flew off-screen (they were set to idle before).

### 2025.04.16 - Build 10
- Bugs start move from the mushroom exactly where they were.
- Bugs move all the way to the mushroom. If you start rotating it before they
  hop on the mushroom, they will turn back. 

### 2025.04.15 - Build 9
- Bugs fly away from completed mushrooms.
- Todo: Free bug entities when flew off-screen (now they are set to idle instead).

### 2025.04.14 - Build 8
- Added transitioning mushroom animation.
- When a mushroom is filled with the same colored bugs, it transitions to 
  light version, and bugs are removed. (They should fly away, but that part
  is not yet ready.)

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

