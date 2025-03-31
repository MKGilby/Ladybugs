{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBShared;

{$mode Delphi}

interface

uses
  MediaManagerUnit, LBMapEntities;

const
  WINDOWWIDTH=640;
  WINDOWHEIGHT=480;
  WINDOWCAPTION='Ladybugs V%s (%s)';
  MAPWIDTH=8*5;
  MAPHEIGHT=5*5+1;

  MAXTIMESLICE=1/128;

  DIR_BIT_UP=1;
  DIR_BIT_RIGHT=2;
  DIR_BIT_DOWN=4;
  DIR_BIT_LEFT=8;

  PATHIMAGEINDEX:array[1..15] of integer=(0,1,6,0,0,3,8,1,5,1,9,4,10,7,2);

var
  MM:TMediaManager;
  Entities:TMapEntities;

procedure LoadAssets;
procedure FreeAssets;

implementation

procedure LoadAssets;
begin
  MM:=TMediaManager.Create;
  MM.Load('mushroom.png','Mushroom');
  MM.Load('bugs.png','Bugs',MM_DONTKEEPIMAGE);
  MM.Load('grass.png','Grass');
  MM.Load('paths.png','Paths');
end;

procedure FreeAssets;
begin
  MM.Free;
end;

end.

