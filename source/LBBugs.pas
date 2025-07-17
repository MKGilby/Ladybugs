{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBBugs;

{$mode Delphi}

interface

uses
  SysUtils, Animation2Unit, LBMap, fgl;

type

  TBugState=(bsIdle,bsMovingOnPath,bsFlying);

  { TBug }

  TBug=class
    constructor Create(iX,iY,iColor:integer;iMap:TMap);
    destructor Destroy; override;
    procedure Move(pElapsedTime:double);
    procedure Draw; overload;
    procedure Draw(pX,pY:integer); overload;
    procedure SetDirection(pDirection:integer);
    procedure StartMove(pX,pY:integer);
    procedure StartFly(pX,pY:integer);
  private
    fdX,fdY:double;
    fColor:integer;
    fMap:TMap;
    fDirection:integer;
    fAnimation:TAnimation;
    fState:TBugState;
    procedure SetAnimByDirection;
    procedure SetAnimByColor;
  public
    X:integer;
    Y:integer;
    property Color:integer read fColor;
    property State:TBugState read fState;
  end;

  { TBugs }

  TBugs=class(TFPGObjectList<TBug>)
    procedure CreateNewBug(pMap:TMap);
    procedure Move(pElapsedTime:double);
    procedure Draw;
    class function GetRandomBugColor:integer;
  private
    procedure MoveEx(pElapsedTime:double);
  end;

implementation

uses LBShared, LBMapEntities;

const
  HorzDisplacement:array[0..15] of integer=(0,0,1,1,1,1,0,0,0,0,-1,-1,-1,-1,0,0);
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
  fAnimation:=nil;
  SetAnimByColor;
//  fAnimation:=MM.Animations[Format('Bug%d',[fColor])].SpawnAnimation;
//  fAnimation.LogData;
  fDirection:=DIR_LEFT;
  fState:=bsMovingOnPath;
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
    Result:=(py>1) and (fMap.Tiles[px,py-1] and MAP_DIR_BIT_UP=0);
  end;

  function CanMoveRight(px,py:integer):boolean; inline;
  begin
    Result:=(px<MAPWIDTH-1) and (fMap.Tiles[px+1,py] and MAP_DIR_BIT_RIGHT=0);
  end;

  function CanMoveDown(px,py:integer):boolean; inline;
  begin
    Result:=(py<MAPHEIGHT-1) and (fMap.Tiles[px,py+1] and MAP_DIR_BIT_DOWN=0);
  end;

  function CanMoveLeft(px,py:integer):boolean; inline;
  begin
    Result:=(px>0) and (fMap.Tiles[px-1,py] and MAP_DIR_BIT_LEFT=0);
  end;

begin
  case fState of
    bsIdle: ; // No moving
    bsMovingOnPath:begin
      predir:=fDirection;
      case fDirection of
        DIR_UP:fdY:=fdY-BUGWALKINGSPEED*pElapsedTime;
        DIR_RIGHT:fdX:=fdX+BUGWALKINGSPEED*pElapsedTime;
        DIR_DOWN:fdY:=fdY+BUGWALKINGSPEED*pElapsedTime;
        DIR_LEFT:fdX:=fdX-BUGWALKINGSPEED*pElapsedTime;
      end;
      X:=trunc(fdX);
      Y:=trunc(fdY);
      px:=X div 16;
      py:=Y div 16;
      // When standing on whole block horizontally
      if (X mod 16)=0 then begin
        case predir of
          DIR_LEFT:begin
            // If moving in the top row and can hop onto a mushroom
            if (py=0) and (fMap.Tiles[px,py+1] and MAP_DIR_BIT_DOWN=0) and
                (Entities.EntityAt[px,py+1] is TMushroom) and
                TMushRoom(Entities.EntityAt[px,py+1]).AddBug(Self,DIR_UP) then begin
              // If the bug did not start flying out, it will be idling on the mushroom
              if fState<>bsFlying then fState:=bsIdle;
              // We should create a new bug to the upper row
              ShouldCreateNewBug:=true;
              // Bug faces upwards
              fDirection:=DIR_UP;
              // Reset bug timer
              BugTimer.Reset;
            end else
            if not CanMoveLeft(px,py) then begin  // Can't move left any more
              if CanMoveDown(px,py) then fDirection:=DIR_DOWN
              else if CanMoveUp(px,py) then fDirection:=DIR_UP
              else if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
              else fDirection:=DIR_NONE;  // This shouldn't be happening ever
            end else begin  // Can move to the left, check special blocks
              // Mushroom
              if (px mod 5=0) and (Entities.EntityAt[px-1,py] is TMushroom) then begin
                // If mushroom accepts bug
                if TMushRoom(Entities.EntityAt[px-1,py]).AddBug(Self,DIR_RIGHT) then begin
                  // Decrease moving bug count
                  dec(CurrentMovingBugs);
                  // If the bug did not start flying out, it will be idling on the mushroom
                  if fState<>bsFlying then fState:=bsIdle;
                end else  // Turn back
                  fDirection:=DIR_RIGHT;
              end else
              // Color blocker
              if (px mod 5=3) and (Entities.EntityAt[px,py] is TBlocker) then begin
                // If bug color doesn't match blocker color, turn back
                if TBlocker(Entities.EntityAt[px,py]).Color<>fColor then fDirection:=DIR_RIGHT;
              end else
              // Teleport
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TTeleport) then begin
                // Teleport bug to new position
                TTeleport(Entities.EntityAt[px,py]).GetNewCoords(x,y);
                dec(X);  // To prevent re-teleporting
                fdX:=X;
                fdY:=Y;
              end else
              // Painter
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TPainter) then begin
                // Recolor bug
                fColor:=TPainter(Entities.EntityAt[px,py]).Color;
                SetAnimByColor;
              end;
            end;
          end;
          DIR_RIGHT:begin
            // If moving in the top row and can hop onto a mushroom
            if (py=0) and (fMap.Tiles[px,py+1] and MAP_DIR_BIT_DOWN=0) and
                (Entities.EntityAt[px,py+1] is TMushroom) and
                TMushRoom(Entities.EntityAt[px,py+1]).AddBug(Self,DIR_UP) then begin
              // If the bug did not start flying out, it will be idling on the mushroom
              if fState<>bsFlying then fState:=bsIdle;
              // We should create a new bug to the upper row
              ShouldCreateNewBug:=true;
              // Bug faces upwards
              fDirection:=DIR_UP;
              // Reset bug timer
              BugTimer.Reset;
            end else
            if not CanMoveRight(px,py) then begin  // Can't move left any more
              if CanMoveDown(px,py) then fDirection:=DIR_DOWN
              else if CanMoveUp(px,py) then fDirection:=DIR_UP
              else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
              else fDirection:=DIR_NONE;  // This shouldn't be happening ever
            end else begin  // Can move to the right, check special blocks
              // Mushroom
              if (px mod 5=4) and (Entities.EntityAt[px+1,py] is TMushroom) then begin
                // If mushroom accepts bug
                if TMushRoom(Entities.EntityAt[px+1,py]).AddBug(Self,DIR_LEFT) then begin
                  // Decrease moving bug count
                  dec(CurrentMovingBugs);
                  // If the bug did not start flying out, it will be idling on the mushroom
                  if fState<>bsFlying then fState:=bsIdle;
                end else // Turn back
                  fDirection:=DIR_LEFT;
              end else
              // Color blocker
              if (px mod 5=1) and (Entities.EntityAt[px,py] is TBlocker) then begin
                // If bug color doesn't match blocker color, turn back
                if TBlocker(Entities.EntityAt[px,py]).Color<>fColor then fDirection:=DIR_LEFT;
              end else
              // Teleport
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TTeleport) then begin
                // Teleport bug to new position
                TTeleport(Entities.EntityAt[px,py]).GetNewCoords(x,y);
                inc(X);  // To prevent re-teleporting
                fdX:=X;
                fdY:=Y;
              end else
              // Painter
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TPainter) then begin
                // Recolor bug
                fColor:=TPainter(Entities.EntityAt[px,py]).Color;
                SetAnimByColor;
              end;
            end;
          end;
        end;
      end;
      // When standing on whole block vertically
      if (Y mod 16)=0 then begin
        case predir of
          DIR_DOWN:begin
            if not CanMoveDown(px,py) then begin  // Can't move down any more
              if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
              else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
              else if CanMoveUp(px,py) then fDirection:=DIR_UP
              else fDirection:=DIR_NONE;  // This shouldn't be happening ever
            end else begin  // Can move down, check special blocks
              // Mushroom
              if ((py-1) mod 5=4) and (Entities.EntityAt[px,py+1] is TMushroom) then begin
                // If mushroom accepts bug
                if TMushRoom(Entities.EntityAt[px,py+1]).AddBug(Self,DIR_UP) then begin
                  // Decrease moving bug count
                  dec(CurrentMovingBugs);
                  // If the bug did not start flying out, it will be idling on the mushroom
                  if fState<>bsFlying then fState:=bsIdle;
                end else  // Turn back
                  fDirection:=DIR_UP;
              end else
              // Color blocker
              if ((py-1) mod 5=1) and (Entities.EntityAt[px,py] is TBlocker) then begin
                // If bug color doesn't match blocker color, turn back
                if TBlocker(Entities.EntityAt[px,py]).Color<>fColor then fDirection:=DIR_UP;
              end else
              // Teleport
              if ((py-1) mod 5=2) and (Entities.EntityAt[px,py] is TTeleport) then begin
                // Teleport bug to new position
                TTeleport(Entities.EntityAt[px,py]).GetNewCoords(x,y);
                inc(Y);  // To prevent re-teleporting
                fdX:=X;
                fdY:=Y;
              end else
              // Painter
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TPainter) then begin
                // Recolor bug
                fColor:=TPainter(Entities.EntityAt[px,py]).Color;
                SetAnimByColor;
              end;
            end;
          end;
          DIR_UP:begin
            if not CanMoveUp(px,py) then begin  // Can't move up any more
              if CanMoveRight(px,py) then fDirection:=DIR_RIGHT
              else if CanMoveLeft(px,py) then fDirection:=DIR_LEFT
              else if CanMoveDown(px,py) then fDirection:=DIR_DOWN
              else fDirection:=DIR_NONE;  // This shouldn't be happening ever
            end else begin  // Can move up, check special blocks
              // Mushroom
              if ((py-1) mod 5=0) and (Entities.EntityAt[px,py-1] is TMushroom) then begin
                // If mushroom accepts bug
                if TMushRoom(Entities.EntityAt[px,py-1]).AddBug(Self,DIR_DOWN) then begin
                  // Decrease moving bug count
                  dec(CurrentMovingBugs);
                  // If the bug did not start flying out, it will be idling on the mushroom
                  if fState<>bsFlying then fState:=bsIdle;
                end else  // turn back
                  fDirection:=DIR_DOWN;
              end;
              // Color blocker
              if ((py-1) mod 5=3) and (Entities.EntityAt[px,py] is TBlocker) then begin
                // If bug color doesn't match blocker color, turn back
                if TBlocker(Entities.EntityAt[px,py]).Color<>fColor then fDirection:=DIR_DOWN;
              end else
              // Teleport
              if ((py-1) mod 5=2) and (Entities.EntityAt[px,py] is TTeleport) then begin
                // Teleport bug to new position
                TTeleport(Entities.EntityAt[px,py]).GetNewCoords(x,y);
                dec(Y);  // To prevent re-teleporting
                fdX:=X;
                fdY:=Y;
              end else
              // Painter
              if (px mod 5=2) and (Entities.EntityAt[px,py] is TPainter) then begin
                // Recolor bug
                fColor:=TPainter(Entities.EntityAt[px,py]).Color;
                SetAnimByColor;
              end;
            end;
          end;
        end;
      end;
      // If direction changed, set animation accordingly.
      if predir<>fDirection then SetAnimByDirection;
    end;
    bsFlying:begin
      fAnimation.Animate(pElapsedTime);
      case fDirection of
        DIR_UP:begin
          fdY:=fdY-BUGFLYINGSPEED*pElapsedTime;
          fdX:=fdX+BUGFLYINGSPEED*pElapsedTime/6;
        end;
        DIR_RIGHT:begin
          fdX:=fdX+BUGFLYINGSPEED*pElapsedTime;
          fdY:=fdY+BUGFLYINGSPEED*pElapsedTime/6;
        end;
        DIR_DOWN:begin
          fdY:=fdY+BUGFLYINGSPEED*pElapsedTime;
          fdX:=fdX-BUGFLYINGSPEED*pElapsedTime/6;
        end;
        DIR_LEFT:begin
          fdX:=fdX-BUGFLYINGSPEED*pElapsedTime;
          fdY:=fdY-BUGFLYINGSPEED*pElapsedTime/6;
        end;
      end;
      X:=trunc(fdX);
      Y:=trunc(fdY);
      if (X<-16) or (X>WINDOWWIDTH) or (Y<-16) or (Y>WINDOWHEIGHT) then
        Bugs.Delete(Bugs.IndexOf(Self));
    end;
  end;
end;

procedure TBug.Draw;
begin
  case fState of
    bsIdle: ;  // No drawing (sitting in a mushroom)
    bsMovingOnPath: fAnimation.PutFrame(X+HorzDisplacement[Y mod 16],Y+VertDisplacement[X mod 16]+16);
    bsFlying: fAnimation.PutFrame(X,Y);
  end;
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

procedure TBug.StartMove(pX, pY: integer);
begin
  X:=pX;
  Y:=pY;
  fdX:=X;
  fdY:=Y;
  fstate:=bsMovingOnPath;
  inc(CurrentMovingBugs);
end;

procedure TBug.StartFly(pX,pY:integer);
begin
  X:=pX;
  Y:=pY;
  fdX:=X;
  fdY:=Y;
  fstate:=bsFlying;
  fAnimation.Free;
  fAnimation:=MM.Animations[Format('FBug%d%d',[fColor,fDirection])].SpawnAnimation;
end;

procedure TBug.SetAnimByDirection;
begin
  if fDirection=DIR_UP then fAnimation.Timer.CurrentFrameIndex:=0
  else if fDirection=DIR_RIGHT then fAnimation.Timer.CurrentFrameIndex:=1
  else if fDirection=DIR_DOWN then fAnimation.Timer.CurrentFrameIndex:=2
  else if fDirection=DIR_LEFT then fAnimation.Timer.CurrentFrameIndex:=3;
end;

procedure TBug.SetAnimByColor;
begin
  if Assigned(fAnimation) then fAnimation.Free;
  fAnimation:=MM.Animations[Format('Bug%d',[fColor])].SpawnAnimation;
  SetAnimByDirection;
end;

{ TBugs }

procedure TBugs.CreateNewBug(pMap:TMap);
begin
  Add(TBug.Create((MAPWIDTH-1)*16,0,NextBugColor,pMap));
  NextBugColor:=TBugs.GetRandomBugColor;
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

class function TBugs.GetRandomBugColor:integer;
begin
  Result:=random(4)+1;
end;

procedure TBugs.MoveEx(pElapsedTime:double);
var i:integer;
begin
  // Must be backwards, may remove self from the list when moving.
  for i:=Count-1 downto 0 do
    Items[i].Move(pElapsedTime);
end;

end.

