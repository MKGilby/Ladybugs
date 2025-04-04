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
    fState:(mstIdle,mstRotating);
    fBugs:array[0..3] of TBug;
    procedure MouseDown(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses LBShared, Logger, SDL2;

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
  SLOTPOSITIONS:array[0..3,0..1] of integer=((32,9),(55,32),(32,55),(9,32));
  SLOTMAPPOS:array[0..3,0..1] of integer=((2,0),(4,2),(2,4),(0,2));

constructor TMushroom.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var i:integer;
begin
  inherited Create(iMap,ipX,ipY,pJ);
  SetBoundsWH(fLeft,fTop,80,80);
  Visible:=true;
  Enabled:=true;
  fState:=mstIdle;
  fAnimation:=MM.Animations.ItemByName['MushroomD'].SpawnAnimation;
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
  case fState of
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

  procedure BugToSlot(pSlot:integer;pBug:TBug;pDirection:integer); {inline;}
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
  if pFromDirection=DIR_UP then BugToSlot(0,pBug,pFromDirection)
  else if pFromDirection=DIR_RIGHT then BugToSlot(1,pBug,pFromDirection)
  else if pFromDirection=DIR_DOWN then BugToSlot(2,pBug,pFromDirection)
  else if pFromDirection=DIR_LEFT then BugToSlot(3,pBug,pFromDirection);
end;

procedure TMushroom.Move(pElapsedTime:double);
var tmpBug:TBug;
begin
  fAnimation.Animate(pElapsedTime);
  if fAnimation.Timer.CurrentFrameIndex=7 then begin
    if Assigned(fBugs[0]) then fBugs[0].SetDirection(DIR_LEFT);
    if Assigned(fBugs[1]) then fBugs[1].SetDirection(DIR_UP);
    if Assigned(fBugs[2]) then fBugs[2].SetDirection(DIR_RIGHT);
    if Assigned(fBugs[3]) then fBugs[3].SetDirection(DIR_DOWN);
  end;
  if fAnimation.Timer.Finished then begin
    fState:=mstIdle;
    fAnimation.Timer.ResetFrameIndex;
    fAnimation.Timer.Paused:=true;
    tmpBug:=fBugs[0];
    fBugs[0]:=fBugs[1];
    fBugs[1]:=fBugs[2];
    fBugs[2]:=fBugs[3];
    fBugs[3]:=tmpBug;
    if not Assigned(fBugs[0]) then fMap.Tiles[fX*5+SLOTMAPPOS[0,0],fY*5+1+SLOTMAPPOS[0,1]]:=0;
    if not Assigned(fBugs[1]) then fMap.Tiles[fX*5+SLOTMAPPOS[1,0],fY*5+1+SLOTMAPPOS[1,1]]:=0;
    if not Assigned(fBugs[2]) then fMap.Tiles[fX*5+SLOTMAPPOS[2,0],fY*5+1+SLOTMAPPOS[2,1]]:=0;
    if not Assigned(fBugs[3]) then fMap.Tiles[fX*5+SLOTMAPPOS[3,0],fY*5+1+SLOTMAPPOS[3,1]]:=0;
  end;
end;

procedure TMushroom.MouseDown(Sender:TObject; x,y,buttons:integer);
begin
  if buttons=SDL_BUTTON_LEFT then begin
    // Release clicked ladybug
  end
  else if Buttons=SDL_BUTTON_RIGHT then begin
    if fState=mstIdle then begin
      fAnimation.Timer.Looped:=false;
      fAnimation.Timer.Paused:=false;
      fAnimation.LogData;
      fMap.Tiles[fX*5+SLOTMAPPOS[0,0],fY*5+1+SLOTMAPPOS[0,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[1,0],fY*5+1+SLOTMAPPOS[1,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[2,0],fY*5+1+SLOTMAPPOS[2,1]]:=15;
      fMap.Tiles[fX*5+SLOTMAPPOS[3,0],fY*5+1+SLOTMAPPOS[3,1]]:=15;
      fState:=mstRotating;
    end;
  end;
end;

{$endregion}

end.

