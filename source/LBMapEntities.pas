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

  { TMapEntity }

  TMapEntity=class(TMouseObject)
    // ipX, ipY is in blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    // Draw the non-static part of the entity.
//    procedure Draw; virtual; abstract;
    // Move and/or animate the entity based on elapsed time.
    procedure Move(pElapsedTime:double); virtual; abstract;
    // Draws static background image onto pBack.
    procedure DrawBack(pBack:TARGBImage); virtual; abstract;
  protected
    fLeft,fTop:integer;
    fX,fY:integer;
    fMap:TMap;
  public
    property X:integer read fX;
    property Y:integer read fY;
  end;

  { TMapEntities }

  TMapEntities=class(TFPGObjectList<TMapEntity>)
    // Draw all entities at once
    procedure Draw;
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
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(pElapsedTime:double); override;
  private
    fExits:integer;
  end;

  { TMushroom }

  TMushroom=class(TSimplePath)
    // ipX, ipY is in big blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer;pJ:TJSONData);
    destructor Destroy; override;
    // Draw the non-static part of the entity.
    procedure Draw; override;
    // Draws static background image onto pBack.
    procedure DrawBack(pBack:TARGBImage); override;
    // Add bug to the desired slot
    procedure AddBug(pBug:TBug;pFromDirection:integer);
    // Move entity for no more than MAXTIMESLICE
    procedure Move(pElapsedTime:double); override;
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
    procedure DrawBack(pBack:TARGBImage); override;
    // Move entity for no more than MAXTIMESLICE
    procedure Move(pElapsedTime:double); override;
  private
    fPotAnim,fFlowerAnim:TAnimation;
    fPotsTop:integer;
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
  fTop:=fY*80+32;
  fMap:=iMap;
end;

{$endregion}

{ TMapEntities }
{$region /fold}

procedure TMapEntities.Draw;
var i:integer;
begin
  for i:=0 to Self.Count-1 do
    Self[i].Draw;
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

constructor TSimplePath.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var s,s2:String;none:boolean;
begin
  inherited Create(iMap,ipX,ipY);
  if Assigned(pj.FindPath('Exits')) then
    s:=pj.FindPath('Exits').AsString
  else
    s:='None';
  fExits:=0;
  none:=false;
//  Log.Trace('----');
//  Log.Trace(s);
  while length(s)>0 do begin
    s2:=copy(s,1,pos(',',s+',')-1);
    delete(s,1,length(s2)+1);
    s2:=Trim(s2);
//    Log.Trace('  '+s2);
    if UpperCase(s2)='UP' then fExits:=fExits or DIR_BIT_UP
    else if UpperCase(s2)='RIGHT' then fExits:=fExits or DIR_BIT_RIGHT
    else if UpperCase(s2)='DOWN' then fExits:=fExits or DIR_BIT_DOWN
    else if UpperCase(s2)='LEFT' then fExits:=fExits or DIR_BIT_LEFT
    else if UpperCase(s2)='NONE' then none:=true;
  end;
//  Log.Trace(fExits);
  if none and (fExits<>0) then
    raise Exception.Create('Both NONE and Directions are specified in Exits!');
  if not none and (fExits=0) then
    raise Exception.Create('Neither NONE nor Directions are specified in Exits!');
end;

procedure TSimplePath.Draw;
begin
  // Nothing to do.
end;

procedure TSimplePath.DrawBack(pBack:TARGBImage);
var tmp:TARGBImage;px,py:integer;
begin
  px:=fX*5;
  py:=fY*5+1;
  tmp:=MM.Images.ItemByName['Paths'];
  if (fExits and DIR_BIT_UP)=DIR_BIT_UP then begin
    fMap.Tiles[pX+2,pY]:=0;
    fMap.Tiles[pX+2,pY+1]:=0;
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_RIGHT)=DIR_BIT_RIGHT then begin
    fMap.Tiles[pX+3,pY+2]:=0;
    fMap.Tiles[pX+4,pY+2]:=0;
    pBack.PutImagePart(fLeft+48,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+64,fTop+32,16,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_DOWN)=DIR_BIT_DOWN then begin
    fMap.Tiles[pX+2,pY+3]:=0;
    fMap.Tiles[pX+2,pY+4]:=0;
    pBack.PutImagePart(fLeft+32,fTop+48,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+64,0,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_LEFT)=DIR_BIT_LEFT then begin
    fMap.Tiles[pX,pY+2]:=0;
    fMap.Tiles[pX+1,pY+2]:=0;
    pBack.PutImagePart(fLeft   ,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+16,fTop+32,16,0,16,16,tmp,true);
  end;
  if fExits>0 then begin
    fMap.Tiles[pX+2,pY+2]:=0;
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits]*16,0,16,16,tmp,true);
  end;
end;

procedure TSimplePath.Move(pElapsedTime:double);
begin
end;

{$endregion}

{ TMushroom }
{$region /fold}

const
  // Slot positions relative to tile top,left
  SLOTPOSITIONS:array[0..3,0..1] of integer=((32,9),(55,32),(32,55),(9,32));
  // Slot positions on map relative to big tile*5
  SLOTMAPPOS:array[0..3,0..1] of integer=((2,0),(4,2),(2,4),(0,2));
  // Slot positions for checking for road relative to big tile*5
  SLOTCHECKMAPPOS:array[0..3,0..1] of integer=((2,-1),(5,2),(2,5),(-1,2));
  // Bug start moving position relative to tile top,left
  BUGSTARTMOVEPOS:array[0..3,0..1] of integer=((32,-8),(72,32),(32,72),(-8,32));

constructor TMushroom.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var i:integer;
begin
  inherited Create(iMap,ipX,ipY,pJ);
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
  fAnimation.PutFrame(fX*80+8,fY*80+32+8);
  case fMovingState of
    mstIdle:begin
      for i:=0 to 3 do
        if Assigned(fBugs[i]) then
          fBugs[i].Draw(fX*80+SLOTPOSITIONS[i,0],fY*80+SLOTPOSITIONS[i,1]+32);
    end;
    mstRotating:begin
      for i:=0 to 3 do
        if Assigned(fBugs[i]) then
          fBugs[i].Draw(
            fX*80+SLOTROTATEPOSITIONS[REORDER[i]*15+fAnimation.Timer.CurrentFrameIndex,0],
            fY*80+SLOTROTATEPOSITIONS[REORDER[i]*15+fAnimation.Timer.CurrentFrameIndex,1]+32);
    end;
    mstTransitioning:;  // No additional drawing needed
  end;
end;

procedure TMushroom.DrawBack(pBack: TARGBImage);
begin
  inherited DrawBack(pBack);
  if fY=0 then begin
    fMap.Tiles[fX*5+2,fY*5+1]:=DIR_BIT_ALL xor DIR_BIT_DOWN;
    fMap.Tiles[fX*5+2,fY*5+2]:=DIR_BIT_ALL xor DIR_BIT_DOWN;
    pBack.PutImagePart(fLeft+32,fTop-16,PATHIMAGEINDEX[14]*16,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits or DIR_BIT_UP]*16,0,16,16,MM.Images.ItemByName['Paths'],true);
  end;
end;

procedure TMushroom.AddBug(pBug:TBug; pFromDirection:integer);
var i:integer;

  procedure BugToSlot(pSlot:integer;pBug:TBug;pDirection:integer);
  begin
    if not assigned(fBugs[pSlot]) then begin
      fBugs[pSlot]:=pBug;
      pBug.SetDirection(pDirection);
      pBug.X:=fX*80+SLOTPOSITIONS[pSlot,0];
      pBug.Y:=fY*80+SLOTPOSITIONS[pSlot,1]+32;
      fMap.Tiles[fX*5+SLOTMAPPOS[pSlot,0],fY*5+1+SLOTMAPPOS[pSlot,1]]:=15;
    end else
      raise Exception.Create(Format('There''s already a bug in slot %d!',[pSlot]));
  end;

begin
  // Add bug to the appropriate slot
  if pFromDirection=DIR_UP then BugToSlot(0,pBug,pFromDirection)
  else if pFromDirection=DIR_RIGHT then BugToSlot(1,pBug,pFromDirection)
  else if pFromDirection=DIR_DOWN then BugToSlot(2,pBug,pFromDirection)
  else if pFromDirection=DIR_LEFT then BugToSlot(3,pBug,pFromDirection);
  // Check if all slots have the bugs of same color
  if Assigned(fBugs[0]) and Assigned(fBugs[1]) and Assigned(fBugs[2]) and Assigned(fBugs[3]) then
    if (fBugs[0].Color=fBugs[1].Color) and
       (fBugs[1].Color=fBugs[2].Color) and
       (fBugs[2].Color=fBugs[3].Color) then begin
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
        fBugs[i]:=nil;
        fMap.Tiles[fX*5+SLOTMAPPOS[i,0],fY*5+1+SLOTMAPPOS[i,1]]:=0;
      end;
    end;
end;

procedure TMushroom.Move(pElapsedTime:double);
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

procedure TMushroom.MouseDown(Sender:TObject; x,y,buttons:integer);

  procedure CheckSlotClick(pSlot,pDirBit:integer);
  begin
    // If there's a bug in that slot and the slot is clicked
    if Assigned(fBugs[pSlot]) and
       (x>=SLOTPOSITIONS[pSlot,0]) and (x<SLOTPOSITIONS[pSlot,0]+16) and
       (y>=SLOTPOSITIONS[pSlot,1]) and (y<SLOTPOSITIONS[pSlot,1]+16) then begin
      // If there's road rightwards
      if (fMap.Tiles[fX*5+SLOTCHECKMAPPOS[pSlot,0],fY*5+1+SLOTCHECKMAPPOS[pSlot,1]] and pDirBit=0) then begin
        // Start moving the bug (direction is already set)
        fBugs[pSlot].StartMove(fX*80+BUGSTARTMOVEPOS[pSlot,0],fY*80+16+BUGSTARTMOVEPOS[pSlot,1]);
        // Remove bug from array (still referenced in LBShared.Bugs)
        fBugs[pSlot]:=nil;
        // Set map tile to free to allow another bug come in
        fMap.Tiles[fX*5+SLOTMAPPOS[pSlot,0],fY*5+1+SLOTMAPPOS[pSlot,1]]:=0;
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
    if (fY>0) then CheckSlotClick(0,DIR_BIT_UP);
    // Check right slot click
    CheckSlotClick(1,DIR_BIT_RIGHT);
    // Check bottom slot click
    CheckSlotClick(2,DIR_BIT_DOWN);
    // Check left slot click
    CheckSlotClick(3,DIR_BIT_LEFT);
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
      fPotAnim.PutFrame(fX*80+k+i*20+3,fY*80+32+fPotsTop+j*20+3);
      if r>MaximumMovingBugs-CurrentMovingBugs then
        fFlowerAnim.PutFrame(fX*80+k+i*20+1,fY*80+32+fPotsTop+j*20+1);
      dec(r);
    end;
  end;
end;

procedure TCounter.DrawBack(pBack:TARGBImage);
begin
  // Nothing to do
end;

procedure TCounter.Move(pElapsedTime:double);
begin
  // Nothing to do
end;

{$endregion}

end.

