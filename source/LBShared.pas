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

  BUGSPEED=32;  // pixels per second
  MAXTIMESLICE=1/128;

  DIR_UP=1;
  DIR_RIGHT=2;
  DIR_DOWN=3;
  DIR_LEFT=4;

  DIR_BIT_UP=1;
  DIR_BIT_RIGHT=2;
  DIR_BIT_DOWN=4;
  DIR_BIT_LEFT=8;

  PATHIMAGEINDEX:array[1..15] of integer=(0,1,6,0,0,3,8,1,5,1,9,4,10,7,2);

var
  MM:TGFXManager;
  Entities:TMapEntities;
  Bugs:TBugs;

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

