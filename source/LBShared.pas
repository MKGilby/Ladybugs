{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBShared;

{$mode Delphi}

interface

uses
  GFXManagerUnit, LBMapEntities, LBBugs;

const
  DATAFILE='Ladybugs.data';
  WINDOWWIDTH=640;
  WINDOWHEIGHT=480;
  WINDOWCAPTION='Ladybugs V%s (%s)';
  MAPWIDTH=8*5;
  MAPHEIGHT=5*5+1;

  BUGSPEED=48;  // pixels per second
  MAXTIMESLICE=1/128;

  DIR_NONE=0;
  DIR_UP=1;
  DIR_RIGHT=2;
  DIR_DOWN=3;
  DIR_LEFT=4;

  DIR_BIT_UP=1;
  DIR_BIT_RIGHT=2;
  DIR_BIT_DOWN=4;
  DIR_BIT_LEFT=8;
  DIR_BIT_ALL=DIR_BIT_UP or DIR_BIT_RIGHT or DIR_BIT_DOWN or DIR_BIT_LEFT;

  PATHIMAGEINDEX:array[1..15] of integer=(0,1,6,0,0,3,8,1,5,1,9,4,10,7,2);

  // Center positions of slots rotating around the mushroom.
  // Starting at the top position, going counter-clockwise.
  SLOTROTATEPOSITIONS:array[0..59,0..1] of integer=
    ((32,9), (30,9), (27,10), (25,10), (23,11), (20,12),
     (18,13), (17,15), (15,17), (13,18), (12,20), (11,23),
     (10,25), (10,27), (9,30), (9,32), (9,34), (10,37),
     (10,39), (11,41), (12,44), (13,46), (15,47), (17,49),
     (18,51), (20,52), (23,53), (25,54), (27,54), (30,55),
     (32,55), (34,55), (37,54), (39,54), (41,53), (43,52),
     (46,51), (47,49), (49,47), (51,46), (52,44), (53,41),
     (54,39), (54,37), (55,34), (55,32), (55,30), (54,27),
     (54,25), (53,23), (52,21), (51,18), (49,17), (47,15),
     (46,13), (43,12), (41,11), (39,10), (37,10), (34,9));


var
  MM:TGFXManager;
  Entities:TMapEntities;
  Bugs:TBugs;
  ShouldCreateNewBug:boolean;

procedure LoadAssets;
procedure FreeAssets;

implementation

procedure LoadAssets;
begin
  MM:=TGFXManager.Create;
  MM.Load('mushroom.png','Mushroom',MM_DONTKEEPIMAGE);
  MM.Load('bugs.png','Bugs',MM_DONTKEEPIMAGE);
  MM.Load('grass.png','Grass');
  MM.Load('paths.png','Paths');
  MM.Load('npi69.mkr','Small',MM_DONTKEEPIMAGE);
  MM.Fonts['Small'].SetColorKey(0,0,0);
end;

procedure FreeAssets;
begin
  MM.Free;
end;

end.

