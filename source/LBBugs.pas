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
    procedure Draw; overload;
    procedure Draw(pX,pY:integer); overload;
    procedure SetDirection(pDirection:integer);
  private
    fdX,fdY:double;
    fColor:integer;
    fMap:TMap;
    fDirection:integer;
    fAnimation:TAnimation;
    fMoving:boolean;
    procedure SetAnimByDirection;
  public
    X:integer;
    Y:integer;
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

uses LBShared, LBMapEntities;

const
  HorzDisplacement:array[0..15] of integer=(0,0,-1,-1,-1,-1,0,0,0,0,1,1,1,1,0,0);
  VertDisplacement:array[0..15] of integer=(0,0,-1,-1,-1,-1,0,0,0,0,1,1,1,1,0,0);

{ TBug }

constructor TBug.Create(iX,iY,iColor:integer; iMap:TMap);
begin
  X:=iX;
  Y:=iY;
  fdX:=X;
  fdY:=Y;
  fMap:=iMap;
  if iColor<1 then iColor:=1
  else if iColor>4 then iColor:=4;
  fColor:=iColor;
  fAnimation:=MM.Animations.ItemByName[Format('Bug%d',[iColor])].SpawnAnimation;
  fAnimation.LogData;
  fDirection:=DIR_LEFT;
  fMoving:=true;
  SetAnimByDirection;
end;

destructor TBug.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBug.Move(pElapsedTime:double);
var predir,px,py:integer;

  function CanMoveUp(px,py:integer):boolean; inline;
  begin
    Result:=(py>1) and (fMap.Tiles[px,py-1] and DIR_BIT_UP=0);
  end;

  function CanMoveRight(px,py:integer):boolean; inline;
  begin
    Result:=(px<MAPWIDTH-1) and (fMap.Tiles[px+1,py] and DIR_BIT_RIGHT=0);
  end;

  function CanMoveDown(px,py:integer):boolean; inline;
  begin
    Result:=(py<MAPHEIGHT-1) and (fMap.Tiles[px,py+1] and DIR_BIT_DOWN=0);
  end;

  function CanMoveLeft(px,py:integer):boolean; inline;
  begin
    Result:=(px>0) and (fMap.Tiles[px-1,py] and DIR_BIT_LEFT=0);
  end;

begin
  if not fMoving then exit;
  predir:=fDirection;
  case fDirection of
    DIR_UP:fdY:=fdY-BUGSPEED*pElapsedTime;
    DIR_RIGHT:fdX:=fdX+BUGSPEED*pElapsedTime;
    DIR_DOWN:fdY:=fdY+BUGSPEED*pElapsedTime;
    DIR_LEFT:fdX:=fdX-BUGSPEED*pElapsedTime;
  end;
  X:=trunc(fdX);
  Y:=trunc(fdY);
  px:=X div 16;
  py:=Y div 16;
  if (X mod 16)=0 then begin
    case fDirection of
      DIR_LEFT:begin
        if (py=0) and (fMap.Tiles[px,py+1] and DIR_BIT_DOWN=0) then begin
          fDirection:=DIR_DOWN;
          ShouldCreateNewBug:=true;
        end else
        if not (CanMoveLeft(px,py)) then begin
          if CanMoveDown(px,py) then fDirection:=DIR_DOWN
          else if CanMoveUp(px,py) then fDirection:=DIR_UP
          else if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
          else fDirection:=DIR_NONE;
        end;
      end;
      DIR_RIGHT:begin
        if (py=0) and (fMap.Tiles[px,py+1] and DIR_BIT_DOWN=0) then begin
          fDirection:=DIR_DOWN;
          ShouldCreateNewBug:=true;
        end else
        if not (CanMoveRight(px,py)) then begin
          if CanMoveDown(px,py) then fDirection:=DIR_DOWN
          else if CanMoveUp(px,py) then fDirection:=DIR_UP
          else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
          else fDirection:=DIR_NONE;
        end;
      end;
    end;
  end;
  if (Y mod 16)=0 then begin
    case fDirection of
      DIR_DOWN:begin
        if CanMoveDown(px,py) then begin
          if (Entities.EntityAt[px,py+1]) is TMushroom then begin
            Entities.AddBug(pX,pY+1,Self,DIR_UP);
            fMoving:=false;
          end;
        end else begin
          if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
          else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
          else if CanMoveUp(px,py) then fDirection:=DIR_UP
          else fDirection:=DIR_NONE;
        end;
      end;
      DIR_UP:begin
        if not (CanMoveUp(px,py)) then begin
          if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
          else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
          else if CanMoveDown(px,py) then fDirection:=DIR_DOWN
          else fDirection:=DIR_NONE;
        end;
      end;
    end;
  end;
  if predir<>fDirection then SetAnimByDirection;
end;

procedure TBug.Draw;
begin
  if fMoving then
    fAnimation.PutFrame(X+HorzDisplacement[Y mod 16],Y+VertDisplacement[X mod 16]+16);
end;

procedure TBug.Draw(pX,pY:integer);
begin
  fAnimation.PutFrame(pX,pY);
end;

procedure TBug.SetDirection(pDirection:integer);
begin
  fDirection:=pDirection;
  SetAnimByDirection;
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
  ShouldCreateNewBug:=false;
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

