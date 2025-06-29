{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBMapEntities;

{$mode Delphi}

interface

uses
  SysUtils, fgl, fpjson, MKMouse2, ARGBImageUnit, Animation2Unit, LBMap, LBBugs;

type
  TZIndex=(ziBackground,ziForeground);

  { TMapEntity }

  TMapEntity=class(TMouseObject)
    // ipX, ipY is in blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Move and/or animate the entity based on elapsed time.
    procedure Move(const pElapsedTime:double); virtual;
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); virtual;
  protected
//    fLeft,fTop:integer;
    fX,fY:integer;
    fMap:TMap;
    fZIndex:TZIndex;
  public
    property X:integer read fX;
    property Y:integer read fY;
    property ZIndex:TZIndex read fZIndex;
  end;

  { TMapEntities }

  TMapEntities=class(TFPGObjectList<TMapEntity>)
    // Draw all entities at once where ZIndex=ziBackground
    procedure DrawBackground;
    // Draw all entities at once where ZIndex=ziForeground
    procedure DrawForeground;
    // Move all entities at once
    procedure Move(pElapsedTime:double);
    // Is the entity at map position x,y mushroom?
    function IsMushroomAt(pX,pY:integer):boolean;
    // Add bug to the mushroom at px,py into the desired slot
    procedure AddBug(pX,pY:integer;pBug:TBug;pFromDirection:integer);
  private
    // Move all entities for no more than MAXTIMESLICE
    procedure MoveEx(pElapsedTime:double);
    // Getter form EntityAt
    function fGetEntityAt(x,y:integer):TMapEntity;
  public
    // Gives back entity at x,y map position or nil if no entity are at the
    // given position.
    property EntityAt[x,y:integer]:TMapEntity read fGetEntityAt;
  end;


  { TSimplePath }

  TSimplePath=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    // procedure Move(const pElapsedTime:double); override;
  protected
    fExits:integer;
  end;

  { TMushroom }

  TMushroom=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); override;
    // Add bug to the desired slot
    function AddBug(pBug:TBug;pFromDirection:integer):boolean;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
    // Check if there is four same coloured bug in the mushroom.
    // Also checks traffic light and combination lock too.
    procedure CheckCompleteness;
  private
    fAnimation:TAnimation;
    fMovingState:(mstIdle,mstRotating,mstTransitioning);
    fVisualState:(vstDark,vstLight);
    fBugs:array[0..3] of TBug;
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
  end;

  { TCounter }

  TCounter=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    // procedure DrawBack(const pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    // procedure Move(const pElapsedTime:double); override;
  private
    fPotAnim,fFlowerAnim:TAnimation;
    fPotsTop:integer;
  end;

  { TTimer }

  TTimer=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
  private
    fTime:double;
  end;

  { TNext }

  TNext=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    // procedure Move(pElapsedTime:double); override;
  private
    fBugAnims:array[1..4] of TAnimation;
    fBugTop:integer;
  end;

  { TBlocker }

  TBlocker=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
  private
    fAnimation:TAnimation;
    fColor:integer;
  public
    property Color:integer read fColor;
  end;

  { TTeleport }

  TTeleport=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
    // Add another teleport that can be the target of this teleport
    procedure AddTarget(const pTeleport:TTeleport);
    // Can be the target of another teleport having pExits as exits.
    // True, when bug can continue moving without changing direction
    // after teleporting.
    function HasCompatibleExits(pExits:integer):boolean;
    // Get bug new position (in pixels)
    procedure GetNewCoords(out x,y:integer);
    // Log target coordinates for debugging purposes.
    procedure LogTargets;
  private
    fAnimation:TAnimation;
    fGroup:integer;
    fTargets:array of TTeleport;
  public
    property Group:integer read fGroup;
  end;

  { TPainter }

  TPainter=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
  private
    fAnimation:TAnimation;
    fColor:integer;
  public
    property Color:integer read fColor;
  end;

  { TTrafficLight }

  TTrafficLight=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
    // Next color to assemble (or COLOR_ANY if no color request)
    function NextColor:integer;
    // Color assembled, step to the next color.
    procedure Step;
  private
    fAnimation:TAnimation;
    fColors:array [0..2] of integer;
    fMaxTime,fTime:double;
    // Set the traffic light to three different colors.
    procedure Fill;
  end;

  { TArrow }

  TArrow=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
  private
    fAnimation:TAnimation;
  end;

  { TPatternLock }

  TPatternLock=class(TMapEntity)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(const pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(const pElapsedTime:double); override;
    // Checks if the four color matches the pattern lock. // 0-No, 1-Yes, 2-No lock yet
    function CheckPattern(pColorUp,pColorRight,pColorDown,pColorLeft:integer):integer;
  private
    fAnimation:TAnimation;
    fColors:array [0..3] of integer;
    fMaxTime,fTime:double;
    // Set the pattern lock to a combination containing more than one color.
    procedure Fill;
  end;

implementation

uses LBShared, Logger, SDL2, MKToolbox;

{ TMapEntity }
{$region /fold}

constructor TMapEntity.Create(iMap:TMap; ipX,ipY:integer);
begin
  if ipX<0 then ipX:=0;
  if ipX>7 then ipX:=7;
  if ipY<0 then ipY:=0;
  if ipY>4 then ipY:=4;
  fX:=ipX;
  fY:=ipY;
  fLeft:=fX*80;
  fTop:=fY*80+REALMAPTOP;
  fMap:=iMap;
  fZIndex:=ziBackground;
end;

procedure TMapEntity.Draw;
begin
  // Override if want to draw something
end;

procedure TMapEntity.Move(const pElapsedTime:double);
begin
  // Override if want to do something
end;

procedure TMapEntity.DrawBack(const pBack:TARGBImage);
begin
  // Override if want to do draw something to the background
end;

{$endregion}

{ TMapEntities }
{$region /fold}

procedure TMapEntities.DrawBackground;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    if Self[i].ZIndex=ziBackground then Self[i].Draw;
end;

procedure TMapEntities.DrawForeground;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    if Self[i].ZIndex=ziForeground then Self[i].Draw;
end;

procedure TMapEntities.Move(pElapsedTime:double);
begin
  // Feed only MAXTIMESLICE a time to entities.
  while pElapsedTime>MAXTIMESLICE do begin
    MoveEx(MAXTIMESLICE);
    pElapsedTime:=pElapsedTime-MAXTIMESLICE;
  end;
  MoveEx(pElapsedTime);
end;

function TMapEntities.IsMushroomAt(pX,pY:integer):boolean;
begin
  Result:=(fGetEntityAt(pX,pY) is TMushroom);
end;

procedure TMapEntities.AddBug(pX,pY:integer; pBug:TBug; pFromDirection:integer);
begin
  TMushroom(fGetEntityAt(pX,py)).AddBug(pBug,pFromDirection);
end;

procedure TMapEntities.MoveEx(pElapsedTime:double);
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Move(pElapsedTime);
end;

function TMapEntities.fGetEntityAt(x,y:integer):TMapEntity;
var i:integer;
begin
  Result:=nil;
  x:=x div 5;
  if (y<1) then exit;
  y:=(y-1) div 5;
  for i:=0 to Count-1 do
    if (Items[i].X=x) and (Items[i].Y=y) then begin
      Result:=Items[i];
      break;
    end;
end;

{$endregion}

{ TSimplePath }
{$region /fold}

constructor TSimplePath.Create(iMap: TMap; ipX, ipY: integer);
var px,py:integer;
begin
  inherited Create(iMap,ipX,ipY);
  fExits:=fMap.OrigTiles[ipX,ipY];
  px:=fX*5;
  py:=fY*5+1;
  if (fExits and MAP_DIR_BIT_UP)=MAP_DIR_BIT_UP then begin
    fMap.Tiles[pX+2,pY]:=0;
    fMap.Tiles[pX+2,pY+1]:=0;
  end;
  if (fExits and MAP_DIR_BIT_RIGHT)=MAP_DIR_BIT_RIGHT then begin
    fMap.Tiles[pX+3,pY+2]:=0;
    fMap.Tiles[pX+4,pY+2]:=0;
  end;
  if (fExits and MAP_DIR_BIT_DOWN)=MAP_DIR_BIT_DOWN then begin
    fMap.Tiles[pX+2,pY+3]:=0;
    fMap.Tiles[pX+2,pY+4]:=0;
  end;
  if (fExits and MAP_DIR_BIT_LEFT)=MAP_DIR_BIT_LEFT then begin
    fMap.Tiles[pX,pY+2]:=0;
    fMap.Tiles[pX+1,pY+2]:=0;
  end;
  if fExits>0 then begin
    fMap.Tiles[pX+2,pY+2]:=0;
  end;
end;

procedure TSimplePath.DrawBack(const pBack:TARGBImage);
var tmp:TARGBImage;
begin
  tmp:=MM.Images.ItemByName['Paths'];
  if (fExits and MAP_DIR_BIT_UP)=MAP_DIR_BIT_UP then begin
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,tmp,true);
  end;
  if (fExits and MAP_DIR_BIT_RIGHT)=MAP_DIR_BIT_RIGHT then begin
    pBack.PutImagePart(fLeft+48,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+64,fTop+32,16,0,16,16,tmp,true);
  end;
  if (fExits and MAP_DIR_BIT_DOWN)=MAP_DIR_BIT_DOWN then begin
    pBack.PutImagePart(fLeft+32,fTop+48,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+64,0,0,16,16,tmp,true);
  end;
  if (fExits and MAP_DIR_BIT_LEFT)=MAP_DIR_BIT_LEFT then begin
    pBack.PutImagePart(fLeft   ,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+16,fTop+32,16,0,16,16,tmp,true);
  end;
  if fExits>0 then begin
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits]*16,0,16,16,tmp,true);
  end;
end;

{$endregion}

{ TMushroom }
{$region /fold}

const
  // Slot positions relative to tile top,left
  SLOTPOSITIONS:array[0..3,0..1] of integer=((32,5),(59,32),(32,59),(5,32));
  // Slot positions on map relative to big tile*5
  SLOTMAPPOS:array[0..3,0..1] of integer=((2,0),(4,2),(2,4),(0,2));
  // Slot positions for checking for road relative to big tile*5
  SLOTCHECKMAPPOS:array[0..3,0..1] of integer=((2,-1),(5,2),(2,5),(-1,2));
  // Bug start moving position relative to tile top,left
//  BUGSTARTMOVEPOS:array[0..3,0..1] of integer=((32,-8),(72,32),(32,72),(-8,32));
  BUGSTARTMOVEPOS:array[0..3,0..1] of integer=((32,-16),(80,32),(32,80),(-16,32));
  // Bug start flying position relative to tile top,left
  BUGSTARTFLYPOS:array[0..3,0..1] of integer=((28,5),(51,28),(28,51),(5,28));
  // Slot-direction pairs
  SLOTFROMDIRECTIONS:array[0..3] of integer=(DIR_DOWN,DIR_LEFT,DIR_UP,DIR_RIGHT);

constructor TMushroom.Create(iMap: TMap; ipX, ipY: integer);
var i:integer;
begin
  inherited Create(iMap,ipX,ipY);
  SetBoundsWH(fLeft,fTop,80,80);
  Visible:=true;
  Enabled:=true;
  fMovingState:=mstIdle;
  fVisualState:=vstDark;
  fAnimation:=MM.Animations['MushroomD'].SpawnAnimation;
  for i:=0 to 3 do fBugs[i]:=nil;
  OnMouseDown:=MouseDown;
  Name:=Format('Mushroom (%d,%d)',[fX,fY]);
  MouseObjects.Add(Self);
end;

destructor TMushroom.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TMushroom.Draw;
const REORDER:array[0..3] of integer=(0,3,2,1);
var i:integer;
begin
  fAnimation.PutFrame(fLeft,fTop);
  case fMovingState of
    mstIdle:begin
      for i:=0 to 3 do
        if Assigned(fBugs[i]) then
          fBugs[i].Draw(fLeft+SLOTPOSITIONS[i,0],fTop+SLOTPOSITIONS[i,1]);
    end;
    mstRotating:begin
      for i:=0 to 3 do
        if Assigned(fBugs[i]) then
          fBugs[i].Draw(
            fLeft+SLOTROTATEPOSITIONS[REORDER[i]*15+fAnimation.Timer.CurrentFrameIndex,0]-8,
            fTop+SLOTROTATEPOSITIONS[REORDER[i]*15+fAnimation.Timer.CurrentFrameIndex,1]-8);
    end;
    mstTransitioning:;  // No additional drawing needed
  end;
end;

procedure TMushroom.DrawBack(const pBack: TARGBImage);
begin
  inherited DrawBack(pBack);
  if fY=0 then begin
    fMap.Tiles[fX*5+2,fY*5+1]:=MAP_DIR_BIT_ALL xor MAP_DIR_BIT_DOWN;
    fMap.Tiles[fX*5+2,fY*5+2]:=MAP_DIR_BIT_ALL xor MAP_DIR_BIT_DOWN;
    pBack.PutImagePart(fLeft+32,fTop-16,PATHIMAGEINDEX[14]*16,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits or MAP_DIR_BIT_UP]*16,0,16,16,MM.Images.ItemByName['Paths'],true);
  end;
end;

function TMushroom.AddBug(pBug:TBug; pFromDirection:integer):boolean;

  procedure BugToSlot(pSlot:integer;pBug:TBug;pDirection:integer);
  begin
    // If no bug in the slot
    if not assigned(fBugs[pSlot]) then begin
      // Put bug into slot
      fBugs[pSlot]:=pBug;
      // Set bug direction
      pBug.SetDirection(pDirection);
//      pBug.X:=fX*80+SLOTPOSITIONS[pSlot,0];
//      pBug.Y:=fY*80+SLOTPOSITIONS[pSlot,1]+32;
      // Set map tile to occupied
      fMap.Tiles[fX*5+SLOTMAPPOS[pSlot,0],fY*5+1+SLOTMAPPOS[pSlot,1]]:=15;
    end else
      raise Exception.Create(Format('There''s already a bug in slot %d!',[pSlot]));
  end;

begin
  Result:=false;
  // No bug can be added when the mushroom is rotating.
  if fMovingState=mstRotating then exit;
  // Add bug to the appropriate slot
  if pFromDirection=DIR_UP then BugToSlot(0,pBug,pFromDirection)
  else if pFromDirection=DIR_RIGHT then BugToSlot(1,pBug,pFromDirection)
  else if pFromDirection=DIR_DOWN then BugToSlot(2,pBug,pFromDirection)
  else if pFromDirection=DIR_LEFT then BugToSlot(3,pBug,pFromDirection);
  Result:=true;
end;

procedure TMushroom.Move(const pElapsedTime:double);
var tmpBug:TBug;
begin
  // Animate animation
  fAnimation.Animate(pElapsedTime);
  case fMovingState of
    mstIdle:;  // No moving needed, waiting for clicking.
    mstRotating:begin      // The mushroom is rotating
      // If the rotating is half-time, set bugs new direction.
      if fAnimation.Timer.CurrentFrameIndex=7 then begin
        if Assigned(fBugs[0]) then fBugs[0].SetDirection(DIR_LEFT);
        if Assigned(fBugs[1]) then fBugs[1].SetDirection(DIR_UP);
        if Assigned(fBugs[2]) then fBugs[2].SetDirection(DIR_RIGHT);
        if Assigned(fBugs[3]) then fBugs[3].SetDirection(DIR_DOWN);
      end;
      // If animation (thus rotating) is finished
      if fAnimation.Timer.Finished then begin
        // Moving state is idle, can be rotated by mouse again.
        fMovingState:=mstIdle;
        // Reset animation
        fAnimation.Timer.ResetFrameIndex;
        // Pause animation
        fAnimation.Timer.Paused:=true;
        // Move bugs around
        tmpBug:=fBugs[0];
        fBugs[0]:=fBugs[1];
        fBugs[1]:=fBugs[2];
        fBugs[2]:=fBugs[3];
        fBugs[3]:=tmpBug;
        // Set map free state for unused slots
        // (occupied state is set when started rotating)
        if not Assigned(fBugs[0]) then fMap.Tiles[fX*5+SLOTMAPPOS[0,0],fY*5+1+SLOTMAPPOS[0,1]]:=0;
        if not Assigned(fBugs[1]) then fMap.Tiles[fX*5+SLOTMAPPOS[1,0],fY*5+1+SLOTMAPPOS[1,1]]:=0;
        if not Assigned(fBugs[2]) then fMap.Tiles[fX*5+SLOTMAPPOS[2,0],fY*5+1+SLOTMAPPOS[2,1]]:=0;
        if not Assigned(fBugs[3]) then fMap.Tiles[fX*5+SLOTMAPPOS[3,0],fY*5+1+SLOTMAPPOS[3,1]]:=0;
      end;
    end;
    mstTransitioning:begin
      // Transitioning from dark to light finished ?
      if fAnimation.Timer.Finished then begin
        // Visual state is light, don't need another transition.
        fVisualState:=vstLight;
        // Moving state is idle, can be rotated by mouse again.
        fMovingState:=mstIdle;
        // Free previous animation.
        fAnimation.Free;
        // Set animation to light mushroom.
        fAnimation:=MM.Animations['MushroomL'].SpawnAnimation;
      end;
    end;
  end;
end;

procedure TMushroom.CheckCompleteness;
var i,pl:integer;
begin
  // If all slots have bugs
  if Assigned(fBugs[0]) and Assigned(fBugs[1]) and Assigned(fBugs[2]) and Assigned(fBugs[3]) then begin
    // 1. Check if there is pattern lock and we matched the pattern.
    if Assigned(PatternLock) then begin
      pl:=PatternLock.CheckPattern(fBugs[0].Color,fBugs[1].Color,fBugs[2].Color,fBugs[3].Color);
    end else pl:=2;  // no lock.
    // Check if all slots have the bugs of same color and traffic light allows this color
    if (pl=1) or ((pl=2) and
       (fBugs[0].Color=fBugs[1].Color) and
       (fBugs[1].Color=fBugs[2].Color) and
       (fBugs[2].Color=fBugs[3].Color) and ValidColor(fBugs[0].Color)) then begin
        // If mushroom is not yet lighted
        if fVisualState=vstDark then begin
          // Moving state is transitioning, no clicking is allowed
          fMovingState:=mstTransitioning;
          // Free previous animation
          fAnimation.Free;
          // Set animation to transitioning mushroom
          fAnimation:=MM.Animations['MushroomC'].SpawnAnimation;
          // Unpause animation
          fAnimation.Timer.Paused:=false;
        end;
        // Release bugs from mushroom and free up slot in map
        for i:=0 to 3 do begin
          fBugs[i].StartFly(fX*80+BUGSTARTFLYPOS[i,0],fY*80+BUGSTARTFLYPOS[i,1]+32);
          fBugs[i]:=nil;
          fMap.Tiles[fX*5+SLOTMAPPOS[i,0],fY*5+1+SLOTMAPPOS[i,1]]:=0;
        end;
      end;
  end;
end;

procedure TMushroom.MouseDown(Sender:TObject; x,y,buttons:integer);

  procedure CheckSlotClick(pSlot,pDirBit:integer);
  begin
    // If there's a bug in that slot and the slot is clicked
    if Assigned(fBugs[pSlot]) and
       (x>=SLOTPOSITIONS[pSlot,0]) and (x<SLOTPOSITIONS[pSlot,0]+16) and
       (y>=SLOTPOSITIONS[pSlot,1]) and (y<SLOTPOSITIONS[pSlot,1]+16) then begin
      // If there's road rightwards
      if (fMap.Tiles[fX*5+SLOTCHECKMAPPOS[pSlot,0],fY*5+1+SLOTCHECKMAPPOS[pSlot,1]] and pDirBit=0) then begin
        // Check if the adjacent tile is Mushroom
        if (Entities.EntityAt[fX*5+SLOTCHECKMAPPOS[pSlot,0],fY*5+1+SLOTCHECKMAPPOS[pSlot,1]] is TMushroom) then begin
          // If mushroom accepts bug
          if TMushroom(Entities.EntityAt[fX*5+SLOTCHECKMAPPOS[pSlot,0],fY*5+1+SLOTCHECKMAPPOS[pSlot,1]]).AddBug(fBugs[pSlot],SLOTFROMDIRECTIONS[pSlot]) then begin
            // Remove bug from array (still referenced in LBShared.Bugs)
            fBugs[pSlot]:=nil;
            // Set map tile to free to allow another bug come in
            fMap.Tiles[fX*5+SLOTMAPPOS[pSlot,0],fY*5+1+SLOTMAPPOS[pSlot,1]]:=0;
          end;
        end else begin  // If not
          // Start moving the bug (direction is already set)
          fBugs[pSlot].StartMove(fX*80+BUGSTARTMOVEPOS[pSlot,0],fY*80+16+BUGSTARTMOVEPOS[pSlot,1]);
          // Remove bug from array (still referenced in LBShared.Bugs)
          fBugs[pSlot]:=nil;
          // Set map tile to free to allow another bug come in
          fMap.Tiles[fX*5+SLOTMAPPOS[pSlot,0],fY*5+1+SLOTMAPPOS[pSlot,1]]:=0;
        end;
      end;
    end;
  end;

begin
  if (fMovingState=mstIdle) and (buttons=SDL_BUTTON_LEFT) and
     (CurrentMovingBugs<MaximumMovingBugs) then begin
    // Release clicked ladybug
    // Set coordinates from window coordinates to inside object coordinates.
    x:=x-fLeft;
    y:=y-fTop;

    // If not top row, check upper slot click
    if (fY>0) then CheckSlotClick(0,MAP_DIR_BIT_UP);
    // If not rightmost column, check right slot click
    if (fX<BIGTILEMAPWIDTH-1) then CheckSlotClick(1,MAP_DIR_BIT_RIGHT);
    // If not bottom row, check bottom slot click
    if (fY<BIGTILEMAPHEIGHT-1) then CheckSlotClick(2,MAP_DIR_BIT_DOWN);
    // If not leftmost column, check left slot click
    if (fX>0) then CheckSlotClick(3,MAP_DIR_BIT_LEFT);
  end
  else if Buttons=SDL_BUTTON_RIGHT then begin
    if fMovingState=mstIdle then begin
      fAnimation.Timer.Looped:=false;
      fAnimation.Timer.Paused:=false;
      fAnimation.LogData;
      fMap.Tiles[fX*5+SLOTMAPPOS[0,0],fY*5+1+SLOTMAPPOS[0,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[1,0],fY*5+1+SLOTMAPPOS[1,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[2,0],fY*5+1+SLOTMAPPOS[2,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[3,0],fY*5+1+SLOTMAPPOS[3,1]]:=15;
      fMovingState:=mstRotating;
    end;
  end;
end;

{$endregion}

{ TCounter }
{$region /fold}

constructor TCounter.Create(iMap:TMap; ipX,ipY:integer; pJ:TJSONData);
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pj.FindPath('Maximum')) then
    MaximumMovingBugs:=pj.FindPath('Maximum').AsInteger
  else begin
    Log.LogWarning('Maximum moving bug count is not specified in map! Setting it to 4.');
    MaximumMovingBugs:=4;
  end;
  if MaximumMovingBugs<4 then begin
    Log.LogWarning('Maximum moving bug count is below 4! Setting it to 4.');
    MaximumMovingBugs:=4;
  end else
  if MaximumMovingBugs>16 then begin
    Log.LogWarning('Maximum moving bug count is more than 16! Setting it to 16.');
    MaximumMovingBugs:=16;
  end;
  CurrentMovingBugs:=0;
  fPotAnim:=MM.Animations['Pot'].SpawnAnimation;
  fFlowerAnim:=MM.Animations['Flower'].SpawnAnimation;
  fPotsTop:=(80-((MaximumMovingBugs-1) div 4+1)*20) div 2;
end;

destructor TCounter.Destroy;
begin
  fFlowerAnim.Free;
  fPotAnim.Free;
  inherited Destroy;
end;

procedure TCounter.Draw;
var i,j,k,r:integer;
begin
  r:=MaximumMovingBugs;
  for j:=0 to (MaximumMovingBugs-1) div 4 do begin
    k:=(80-((min(r,4)-1) mod 4+1)*20) div 2;
    for i:=0 to min(r,4)-1 do begin
      fPotAnim.PutFrame(fLeft+k+i*20+3,fTop+fPotsTop+j*20+3);
      if r>MaximumMovingBugs-CurrentMovingBugs then
        fFlowerAnim.PutFrame(fLeft+k+i*20+1,fTop+fPotsTop+j*20+1);
      dec(r);
    end;
  end;
end;

{$endregion}

{ TTimer }
{$region /fold}

constructor TTimer.Create(iMap:TMap; ipX,ipY:integer; pJ:TJSONData);
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pJ.FindPath('Seconds')) then
    fTime:=pJ.FindPath('Seconds').AsFloat
  else begin
    Log.LogWarning('Timer.Seconds is not specified in map! Setting it to 180.');
    fTime:=180;
  end;
  if fTime<0 then begin
    Log.LogWarning('Timer.Seconds is below 0! Setting it to 180.');
    fTime:=180;
  end else
  if fTime>900 then begin
    Log.LogWarning('Timer.Seconds is more than 900! Setting it to 900.');
    fTime:=900;
  end;
end;

destructor TTimer.Destroy;
begin
  inherited Destroy;
end;

procedure TTimer.Draw;
begin
  MM.Fonts['Timer'].OutText(Format('%d:%.2d',[trunc(fTime) div 60,trunc(fTime) mod 60]),fLeft+40,fTop+24,1);
end;

procedure TTimer.Move(const pElapsedTime:double);
begin
  if fTime>0 then begin
    fTime:=fTime-pElapsedTime;
    if fTime<0 then fTime:=0;
  end;
end;

{$endregion}

{ TNext }
{$region /fold}

constructor TNext.Create(iMap:TMap; ipX,ipY:integer);
begin
  inherited Create(iMap,ipX,ipY);
  fBugAnims[1]:=MM.Animations[Format('Bug%d',[1])].SpawnAnimation;
  fBugAnims[2]:=MM.Animations[Format('Bug%d',[2])].SpawnAnimation;
  fBugAnims[3]:=MM.Animations[Format('Bug%d',[3])].SpawnAnimation;
  fBugAnims[4]:=MM.Animations[Format('Bug%d',[4])].SpawnAnimation;
end;

destructor TNext.Destroy;
begin
  fBugAnims[4].Free;
  fBugAnims[3].Free;
  fBugAnims[2].Free;
  fBugAnims[1].Free;
  inherited Destroy;
  fBugTop:=0;
end;

procedure TNext.Draw;
begin
  fBugAnims[NextBugColor].PutFrame(fLeft+32,fTop+fBugTop);
end;

procedure TNext.DrawBack(const pBack:TARGBImage);
var tmp:TARGBImage;
begin
  tmp:=MM.Images['Next'];
  pBack.PutImage(fLeft+(80-tmp.Width) div 2,fTop+(80-tmp.Height) div 2,tmp,true);
  fBugTop:=(80-tmp.Height) div 2+20;
end;

{$endregion}

{ TBlocker }
{$region /fold}

constructor TBlocker.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var s:String;
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pJ.FindPath('Color')) then begin
    s:=pJ.FindPath('Color').AsString;
    if uppercase(s)='RED' then fColor:=COLOR_RED
    else if uppercase(s)='YELLOW' then fColor:=COLOR_YELLOW
    else if uppercase(s)='BLUE' then fColor:=COLOR_BLUE
    else if UpperCase(s)='GREEN' then fColor:=COLOR_GREEN
    else raise Exception.Create(Format('Unknown color in blocker! (%s)',[s]));
  end else
    raise Exception.Create('Color is not specified in blocker!');
  fMap.Tiles[ipX*5+2,ipY*5+2]:=fMap.Tiles[ipX*5+2,ipY*5+2] or MAP_BIT_BLOCKER;
  fAnimation:=MM.Animations[Format('Blocker%d',[fColor])].SpawnAnimation;
  fZIndex:=ziForeground;
end;

destructor TBlocker.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBlocker.Draw;
begin
  fAnimation.PutFrame(fLeft+29,fTop+29);
end;

procedure TBlocker.Move(const pElapsedTime: double);
begin
  fAnimation.Animate(pElapsedTime);
end;

{$endregion}

{ TTeleport }
{$region /fold}

constructor TTeleport.Create(iMap:TMap; ipX,ipY:integer; pJ:TJSONData);
var i:integer;
begin
  inherited Create(iMap,ipX,ipY);
  fMap.Tiles[fX*5+2,fY*5+1+1]:=0;
  fMap.Tiles[fX*5+3,fY*5+1+2]:=0;
  fMap.Tiles[fX*5+2,fY*5+1+3]:=0;
  fMap.Tiles[fX*5+1,fY*5+1+2]:=0;
  fAnimation:=MM.Animations['Teleport'].SpawnAnimation;
  if Assigned(pJ.FindPath('Group')) then
    fGroup:=pJ.FindPath('Group').AsInteger
  else
    Log.LogWarning('No group specified for teleport, assigning to group 1.');
  fZIndex:=ziForeground;
  SetLength(fTargets,0);
  for i:=0 to Entities.Count-1 do
    if (Entities[i] is TTeleport) then
      if (TTeleport(Entities[i]).Group=fGroup) and (TTeleport(Entities[i]).HasCompatibleExits(fExits)) then begin
        AddTarget(TTeleport(Entities[i]));
        TTeleport(Entities[i]).AddTarget(Self);
      end;
end;

destructor TTeleport.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TTeleport.Draw;
begin
  fAnimation.PutFrame(fLeft+24,fTop+24);
end;

procedure TTeleport.Move(const pElapsedTime:double);
begin
  inherited Move(pElapsedTime);
end;

procedure TTeleport.AddTarget(const pTeleport:TTeleport);
begin
  SetLength(fTargets,length(fTargets)+1);
  fTargets[length(fTargets)-1]:=pTeleport;
end;

function TTeleport.HasCompatibleExits(pExits:integer):boolean;
begin
  Result:=
    ((fExits and MAP_DIR_BIT_UP<>0) and (pExits and MAP_DIR_BIT_DOWN<>0)) or
    ((fExits and MAP_DIR_BIT_RIGHT<>0) and (pExits and MAP_DIR_BIT_LEFT<>0)) or
    ((fExits and MAP_DIR_BIT_DOWN<>0) and (pExits and MAP_DIR_BIT_UP<>0)) or
    ((fExits and MAP_DIR_BIT_LEFT<>0) and (pExits and MAP_DIR_BIT_RIGHT<>0));
end;

procedure TTeleport.GetNewCoords(out x,y:integer);
var i:integer;
begin
  if length(fTargets)>0 then begin
    i:=random(length(fTargets));
    X:=(fTargets[i].X*5+2)*16;
    Y:=(fTargets[i].Y*5+2+1)*16;
  end else
    raise Exception.Create('Teleport has no pair!');
end;

procedure TTeleport.LogTargets;
var i:integer;
begin
  Log.LogDebug(Format('Listing pairs of teleport at %d,%d:',[X,Y]));
  for i:=0 to length(fTargets)-1 do
    Log.LogDebug(Format('  %d,%d',[TTeleport(fTargets[i]).X,TTeleport(fTargets[i]).Y]));
end;

{$endregion}

{ TPainter }
{$region /fold}

constructor TPainter.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var s:String;
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pJ.FindPath('Color')) then begin
    s:=pJ.FindPath('Color').AsString;
    if uppercase(s)='RED' then fColor:=COLOR_RED
    else if uppercase(s)='YELLOW' then fColor:=COLOR_YELLOW
    else if uppercase(s)='BLUE' then fColor:=COLOR_BLUE
    else if UpperCase(s)='GREEN' then fColor:=COLOR_GREEN
    else raise Exception.Create(Format('Unknown color in painter! (%s)',[s]));
  end else
    raise Exception.Create('Color is not specified in painter!');
  fMap.Tiles[ipX*5+2,ipY*5+2]:=fMap.Tiles[ipX*5+2,ipY*5+2] or MAP_BIT_BLOCKER;
  fAnimation:=MM.Animations[Format('Painter%d',[fColor])].SpawnAnimation;
  fZIndex:=ziForeground;
end;

destructor TPainter.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TPainter.Draw;
begin
  fAnimation.PutFrame(fLeft+24,fTop+24);
end;

procedure TPainter.Move(const pElapsedTime: double);
begin
  fAnimation.Animate(pElapsedTime);
end;

{$endregion}

{ TTrafficLight }
{$region /fold}

constructor TTrafficLight.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pJ.FindPath('RefillSeconds')) then
    fMaxTime:=pJ.FindPath('RefillSeconds').AsFloat
  else begin
    Log.LogWarning('TrafficLight.RefillSeconds is not specified in map! Setting it to -1 (never refill).');
    fMaxTime:=-1;
  end;
  fTime:=0;
  Fill;
  fAnimation:=MM.Animations['TrafficLights'].SpawnAnimation;
end;

destructor TTrafficLight.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TTrafficLight.Draw;
var i:integer;
begin
  for i:=0 to 2 do
    if fColors[i]>0 then fAnimation.PutFrame(fLeft+34,fTop+20+i*14,fColors[i]-1);
end;

procedure TTrafficLight.DrawBack(const pBack: TARGBImage);
var tmp:TARGBImage;
begin
  tmp:=MM.Images['TrafficLightBase'];
  pBack.PutImage(fLeft+(80-tmp.Width) div 2,fTop+(80-tmp.Height) div 2,tmp,true);
end;

procedure TTrafficLight.Move(const pElapsedTime: double);
begin
  if fTime>0 then begin
    fTime:=fTime-pElapsedTime;
    if fTime<=0 then begin
      Fill;
      fTime:=0;
    end;
  end;
end;

function TTrafficLight.NextColor: integer;
begin
  if fColors[0]<>-1 then Result:=fColors[0]
  else if fColors[1]<>-1 then Result:=fColors[1]
  else if fColors[2]<>-1 then Result:=fColors[2]
  else Result:=COLOR_ANY;
end;

procedure TTrafficLight.Step;
begin
  if fColors[0]<>-1 then fColors[0]:=-1
  else if fColors[1]<>-1 then fColors[1]:=-1
  else if fColors[2]<>-1 then begin
    fColors[2]:=-1;
    fTime:=fMaxTime;
  end;
end;

procedure TTrafficLight.Fill;
begin
  fColors[0]:=random(4)+1;
  repeat
    fColors[1]:=random(4)+1;
  until fColors[0]<>fColors[1];
  repeat
    fColors[2]:=random(4)+1;
  until (fColors[0]<>fColors[2]) and (fColors[1]<>fColors[2]);
end;

{$endregion}

{ TArrow }
{$region /fold}

constructor TArrow.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var s:string;px,py:integer;
begin
  inherited Create(iMap,ipX,ipY);
  px:=fX*5;
  py:=fY*5+1;
  fAnimation:=MM.Animations['Arrows'].SpawnAnimation;
  if Assigned(pJ.FindPath('Direction')) then begin
    s:=pJ.FindPath('Direction').AsString;
    if UpperCase(s)='UP' then begin
      fMap.Tiles[pX+2,pY+3]:=fMap.Tiles[pX+2,pY+3] or MAP_DIR_BIT_DOWN;
      fMap.Tiles[pX+1,pY+2]:=fMap.Tiles[pX+1,pY+2] or MAP_DIR_BIT_LEFT;
      fMap.Tiles[pX+3,pY+2]:=fMap.Tiles[pX+3,pY+2] or MAP_DIR_BIT_RIGHT;
      fAnimation.Timer.CurrentFrameIndex:=0;
    end else
    if UpperCase(s)='RIGHT' then begin
      fMap.Tiles[pX+2,pY+3]:=fMap.Tiles[pX+2,pY+3] or MAP_DIR_BIT_DOWN;
      fMap.Tiles[pX+2,pY+1]:=fMap.Tiles[pX+2,pY+1] or MAP_DIR_BIT_UP;
      fMap.Tiles[pX+1,pY+2]:=fMap.Tiles[pX+1,pY+2] or MAP_DIR_BIT_LEFT;
      fAnimation.Timer.CurrentFrameIndex:=1;
    end else
    if UpperCase(s)='DOWN' then begin
      fMap.Tiles[pX+2,pY+1]:=fMap.Tiles[pX+2,pY+1] or MAP_DIR_BIT_UP;
      fMap.Tiles[pX+1,pY+2]:=fMap.Tiles[pX+1,pY+2] or MAP_DIR_BIT_LEFT;
      fMap.Tiles[pX+3,pY+2]:=fMap.Tiles[pX+3,pY+2] or MAP_DIR_BIT_RIGHT;
      fAnimation.Timer.CurrentFrameIndex:=2;
    end else
    if UpperCase(s)='LEFT' then begin
      fMap.Tiles[pX+2,pY+3]:=fMap.Tiles[pX+2,pY+3] or MAP_DIR_BIT_DOWN;
      fMap.Tiles[pX+2,pY+1]:=fMap.Tiles[pX+2,pY+1] or MAP_DIR_BIT_UP;
      fMap.Tiles[pX+3,pY+2]:=fMap.Tiles[pX+3,pY+2] or MAP_DIR_BIT_RIGHT;
      fAnimation.Timer.CurrentFrameIndex:=3;
    end else
      raise Exception.Create(Format('Unknown direction in Arrow! (%s)',[s]));
  end else
    raise Exception.Create('Arrow.Direction is not specified in map!');
  fZIndex:=ziForeground;
end;

destructor TArrow.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TArrow.Draw;
begin
  fAnimation.PutFrame(Left+29,Top+29);
end;

{$endregion}

{ TPatternLock }
{$region /fold}

constructor TPatternLock.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pJ.FindPath('RefillSeconds')) then
    fMaxTime:=pJ.FindPath('RefillSeconds').AsFloat
  else begin
    Log.LogWarning('PatternLock.RefillSeconds is not specified in map! Setting it to -1 (never refill).');
    fMaxTime:=-1;
  end;
  fTime:=0;
  Fill;
  fAnimation:=MM.Animations['TrafficLights'].SpawnAnimation;
end;

destructor TPatternLock.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TPatternLock.Draw;
begin
  if fTime=0 then begin
    fAnimation.PutFrame(fLeft+34,fTop+20,fColors[0]-1);
    fAnimation.PutFrame(fLeft+48,fTop+34,fColors[1]-1);
    fAnimation.PutFrame(fLeft+34,fTop+48,fColors[2]-1);
    fAnimation.PutFrame(fLeft+20,fTop+34,fColors[3]-1);
  end;
end;

procedure TPatternLock.DrawBack(const pBack: TARGBImage);
var tmp:TARGBImage;
begin
  tmp:=MM.Images['LockBase'];
  pBack.PutImage(fLeft+(80-tmp.Width) div 2,fTop+(80-tmp.Height) div 2,tmp,true);
end;

procedure TPatternLock.Move(const pElapsedTime: double);
begin
  if fTime>0 then begin
    fTime:=fTime-pElapsedTime;
    if fTime<=0 then begin
      Fill;
      fTime:=0;
    end;
  end;
end;

function TPatternLock.CheckPattern(pColorUp, pColorRight, pColorDown,
  pColorLeft: integer): integer;
begin
  if fTime=0 then begin // Pattern lock is filled
    if (fColors[0]=pColorUp) and (fColors[1]=pColorRight) and
       (fColors[2]=pColorDown) and (fColors[3]=pColorLeft) then begin
      Result:=1;
      fTime:=fMaxTime;
    end else
      Result:=0;
  end else Result:=2;  // No lock
end;

procedure TPatternLock.Fill;
var i,c,s:integer;
begin
  repeat
    s:=0;
    for i:=0 to 3 do begin
      c:=random(4)+1;
      fColors[i]:=c;
      inc(s,c);
    end;
  until s mod 4<>0;
end;

{$endregion}

end.

