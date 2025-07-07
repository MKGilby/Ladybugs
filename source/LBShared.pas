{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBShared;

{$mode Delphi}

interface

uses
  GFXManagerUnit, LBMapEntities, LBBugs, LBBugTimer, LBVMU;

const
  DATAFILE='Ladybugs.data';
  VMUFILENAME='Ladybugs.cfg';
  WINDOWWIDTH=640;
  WINDOWHEIGHT=480;
  WINDOWCAPTION='Ladybugs V%s (%s)';
  BIGTILEMAPWIDTH=8;
  BIGTILEMAPHEIGHT=5;
  MAPWIDTH=BIGTILEMAPWIDTH*5;
  MAPHEIGHT=BIGTILEMAPHEIGHT*5+1;
  REALMAPTOP=32;

  BUGWALKINGSPEED=96;  // pixels per second
  BUGFLYINGSPEED=96;
  MAXTIMESLICE=1/128;

  // Don't change these. There are some things wired to the values of these constants.
  DIR_NONE=0;
  DIR_UP=1;
  DIR_RIGHT=2;
  DIR_DOWN=3;
  DIR_LEFT=4;

  // Map bit values.
  // You can change these if you wish.
  MAP_DIR_BIT_UP=1;
  MAP_DIR_BIT_RIGHT=2;
  MAP_DIR_BIT_DOWN=4;
  MAP_DIR_BIT_LEFT=8;
  MAP_DIR_BIT_ALL=MAP_DIR_BIT_UP or MAP_DIR_BIT_RIGHT or MAP_DIR_BIT_DOWN or MAP_DIR_BIT_LEFT;
  MAP_BIT_BLOCKER=16;

  // Color constants
  COLOR_RED=1;
  COLOR_YELLOW=2;
  COLOR_BLUE=3;
  COLOR_GREEN=4;
  COLOR_ANY=255;

  PATHIMAGEINDEX:array[1..15] of integer=(0,1,6,0,0,3,8,1,5,1,9,4,10,7,2);

  // Center positions of slots rotating around the mushroom.
  // Starting at the top position, going counter-clockwise.
  SLOTROTATEPOSITIONS:array[0..59,0..1] of integer=
    ((40, 13), (37, 13), (34, 14), (32, 14), (29, 15), (26, 17),
     (24, 18), (22, 20), (20, 22), (18, 24), (17, 26), (15, 29),
     (14, 32), (14, 34), (13, 37), (13, 40), (13, 43), (14, 46),
     (14, 48), (15, 51), (17, 54), (18, 56), (20, 58), (22, 60),
     (24, 62), (26, 63), (29, 65), (32, 66), (34, 66), (37, 67),
     (40, 67), (43, 67), (46, 66), (48, 66), (51, 65), (53, 63),
     (56, 62), (58, 60), (60, 58), (62, 56), (63, 54), (65, 51),
     (66, 48), (66, 46), (67, 43), (67, 40), (67, 37), (66, 34),
     (66, 32), (65, 29), (63, 27), (62, 24), (60, 22), (58, 20),
     (56, 18), (53, 17), (51, 15), (48, 14), (46, 14), (43, 13));


var
  MM:TGFXManager;
  Entities:TMapEntities;
  Bugs:TBugs;
  ShouldCreateNewBug:boolean;
  MaximumMovingBugs:integer;
  CurrentMovingBugs:integer;
  Paused:boolean;
  NextBugColor:integer;
  TrafficLight:TTrafficLight;
  PatternLock:TPatternLock;
  BugTimer:TBugTimer;
  VMU:TVMU;

procedure LoadAssets;
procedure FreeAssets;
function ValidColor(pColor:integer):boolean;

implementation

procedure LoadFont(pName:string;r,g,b:integer);
begin
  MM.Load('font.png',pName,MM_DONTKEEPIMAGE);
  MM.Fonts[pName].SetColorkey(0,0,0);
  MM.Fonts[pName].SetColor(r,g,b);
  MM.Fonts[pName].LetterSpace:=1;
  MM.Fonts[pName].SpaceSpace:=8;
end;

procedure LoadAssets;
begin
  MM:=TGFXManager.Create;
  MM.Load('sprites.png','Sprites',MM_DONTKEEPIMAGE);
  MM.Load('mushroom.png','Mushroom',MM_DONTKEEPIMAGE);
  MM.Load('grass.png','Grass1');
  MM.Load('grass.png','Grass2');
  MM.Images['Grass2'].Rotate(1);
  MM.Load('grass.png','Grass3');
  MM.Images['Grass3'].Rotate(2);
  MM.Load('grass.png','Grass4');
  MM.Images['Grass4'].Rotate(3);
  MM.Load('paths.png','Paths');
  MM.Load('next.png','Next');
  MM.Load('trafficlightbase.png','TrafficLightBase');
  MM.Load('lockbase.png','LockBase');
  LoadFont('White',255,255,255);
  LoadFont('Red',255,64,64);
  LoadFont('Yellow',255,255,64);
  LoadFont('Blue',64,64,255);
  MM.Load('timerfont.png','Timer',MM_DONTKEEPIMAGE);
  MM.Load('npi69.mkr','Small',MM_DONTKEEPIMAGE);
  MM.Fonts['Small'].SetColorKey(0,0,0);
  MM.Load('logo.png','Logo',MM_CREATETEXTUREONLY);
end;

procedure FreeAssets;
begin
  MM.Free;
end;

function ValidColor(pColor: integer): boolean;
var c:integer;
begin
  if Assigned(TrafficLight) then begin
    c:=TrafficLight.NextColor;
    if c=pColor then TrafficLight.Step;
    Result:=(c=pColor) or (c=COLOR_ANY);
  end else
    Result:=true;
end;

end.

