unit LBBugs;

{$mode Delphi}

interface

uses
  SysUtils, Animation2Unit, LBMap, fgl;

type

  { TBug }

  TBug=class
    constructor Create(iX,iY,iColor:integer;iMap:TMap);
    destructor Destroy; override;
    procedure Move(pElapsedTime:double);
    procedure Draw;
  private
    fX,fY:integer;
    fdX,fdY:double;
    fColor:integer;
    fMap:TMap;
    fDirection:integer;
    fAnimation:TAnimation;
    procedure SetAnimByDirection;
  end;

  { TBugs }

  TBugs=class(TFPGObjectList<TBug>)
    procedure CreateNewBug(pMap:TMap);
    procedure Move(pElapsedTime:double);
    procedure Draw;
  private
    procedure MoveEx(pElapsedTime:double);
  end;

implementation

uses LBShared;

const
  HorzDisplacement:array[0..15] of integer=(0,0,-1,-1,-1,-1,0,0,0,0,1,1,1,1,0,0);
  VertDisplacement:array[0..15] of integer=(0,0,0,0,-1,-1,-1,-1,0,0,0,0,1,1,1,1);

{ TBug }

constructor TBug.Create(iX,iY,iColor:integer; iMap:TMap);
begin
  fX:=iX;
  fY:=iY;
  fdX:=fX;
  fdY:=fY;
  fMap:=iMap;
  if iColor<1 then iColor:=1
  else if iColor>4 then iColor:=4;
  fColor:=iColor;
  fAnimation:=MM.Animations.ItemByName[Format('Bug%d',[iColor])].SpawnAnimation;
  fDirection:=DIR_LEFT;
  SetAnimByDirection;
end;

destructor TBug.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBug.Move(pElapsedTime:double);
var predir:integer;
begin
  predir:=fDirection;
  case fDirection of
    DIR_UP:fdY:=fdY-BUGSPEED*pElapsedTime;
    DIR_RIGHT:fdX:=fdX+BUGSPEED*pElapsedTime;
    DIR_DOWN:fdY:=fdY+BUGSPEED*pElapsedTime;
    DIR_LEFT:fdX:=fdX-BUGSPEED*pElapsedTime;
  end;
  fX:=trunc(fdX);
  fY:=trunc(fdY);
  if (fX mod 16)=0 then begin
    case fDirection of
      DIR_LEFT:begin
        // Somehow we should force bugs to move down from top line.
        if not ((fX>0) and (fMap.Tiles[fX-1,fY] and DIR_BIT_LEFT=0)) then begin
          if (fX<MAPWIDTH-1) and (fMap.Tiles[fX+1,fY] and DIR_BIT_RIGHT=0) then fDirection:=DIR_RIGHT
          else if (fY<MAPHEIGHT-1) and (fMap.Tiles[fX,fY+1] and DIR_BIT_DOWN=0) then fDirection:=DIR_DOWN
          else if (fY>1) and (fMap.Tiles[fX,fY-1] and DIR_BIT_UP=0) then fDirection:=DIR_UP;
        end;


      end;
    end;
  end;
  if predir<>fDirection then SetAnimByDirection;
end;

procedure TBug.Draw;
begin
  fAnimation.PutFrame(fX+HorzDisplacement[fY mod 16],fY+VertDisplacement[fX mod 16]+16);
end;

procedure TBug.SetAnimByDirection;
begin
  if fDirection=DIR_UP then fAnimation.Timer.CurrentFrameIndex:=0
  else if fDirection=DIR_RIGHT then fAnimation.Timer.CurrentFrameIndex:=1
  else if fDirection=DIR_DOWN then fAnimation.Timer.CurrentFrameIndex:=2
  else if fDirection=DIR_LEFT then fAnimation.Timer.CurrentFrameIndex:=3;
end;

{ TBugs }

procedure TBugs.CreateNewBug(pMap:TMap);
begin
  Add(TBug.Create((MAPWIDTH-1)*16,0,random(4)+1,pMap));
end;

procedure TBugs.Move(pElapsedTime:double);
begin
  while pElapsedTime>MAXTIMESLICE do begin
    MoveEx(MAXTIMESLICE);
    pElapsedTime:=pElapsedTime-MAXTIMESLICE;
  end;
  MoveEx(pElapsedTime);
end;

procedure TBugs.Draw;
var i:integer;
begin
  for i:=0 to Count-1 do
    Items[i].Draw;
end;

procedure TBugs.MoveEx(pElapsedTime:double);
var i:integer;
begin
  for i:=0 to Count-1 do
    Items[i].Move(pElapsedTime);
end;

end.

