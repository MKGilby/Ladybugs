{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBMapEntities;

{$mode Delphi}

interface

uses
  SysUtils, fgl, ARGBImageUnit, LBMap, fpjson, Animation2Unit;

type

  { TMapEntity }

  TMapEntity=class
    // ipX, ipY is in blocks (0..7,0..4)
    constructor Create(iMap:TMap;ipX,ipY:integer);
    // Draw the non-static part of the entity.
    procedure Draw; virtual; abstract;
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
  private
    fAnimation:TAnimation;
  end;

implementation

uses LBShared, Logger;

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
  Log.Trace('----');
  Log.Trace(s);
  while length(s)>0 do begin
    s2:=copy(s,1,pos(',',s+',')-1);
    delete(s,1,length(s2)+1);
    s2:=Trim(s2);
    Log.Trace('  '+s2);
    if UpperCase(s2)='UP' then fExits:=fExits or DIR_BIT_UP
    else if UpperCase(s2)='RIGHT' then fExits:=fExits or DIR_BIT_RIGHT
    else if UpperCase(s2)='DOWN' then fExits:=fExits or DIR_BIT_DOWN
    else if UpperCase(s2)='LEFT' then fExits:=fExits or DIR_BIT_LEFT
    else if UpperCase(s2)='NONE' then none:=true;
  end;
  Log.Trace(fExits);
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
var tmp:TARGBImage;
begin
  tmp:=MM.Images.ItemByName['Paths'];
  if (fExits and DIR_BIT_UP)=DIR_BIT_UP then begin
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_RIGHT)=DIR_BIT_RIGHT then begin
    pBack.PutImagePart(fLeft+48,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+64,fTop+32,16,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_DOWN)=DIR_BIT_DOWN then begin
    pBack.PutImagePart(fLeft+32,fTop+48,0,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+32,fTop+64,0,0,16,16,tmp,true);
  end;
  if (fExits and DIR_BIT_LEFT)=DIR_BIT_LEFT then begin
    pBack.PutImagePart(fLeft   ,fTop+32,16,0,16,16,tmp,true);
    pBack.PutImagePart(fLeft+16,fTop+32,16,0,16,16,tmp,true);
  end;
  if fExits>0 then
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits]*16,0,16,16,tmp,true);
end;

{$endregion}

{ TMushroom }
{$region /fold}

constructor TMushroom.Create(iMap: TMap; ipX, ipY: integer; pJ: TJSONData);
var i:integer;
begin
  inherited Create(iMap,ipX,ipY,pJ);
  for i:=0 to MM.Animations.Count-1 do
    Log.Trace(MM.Animations.Strings[i]);
  i:=MM.Animations.IndexOf('MushroomD');
  fAnimation:=MM.Animations.ItemByName['MushroomD'].SpawnAnimation;
end;

destructor TMushroom.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TMushroom.Draw;
begin
  fAnimation.PutFrame(fX*80+8,fY*80+32+8);
end;

procedure TMushroom.DrawBack(pBack: TARGBImage);
begin
  inherited DrawBack(pBack);
  if fY=0 then begin
    pBack.PutImagePart(fLeft+32,fTop   ,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+16,0,0,16,16,MM.Images.ItemByName['Paths'],true);
    pBack.PutImagePart(fLeft+32,fTop+32,PATHIMAGEINDEX[fExits or DIR_BIT_UP]*16,0,16,16,MM.Images.ItemByName['Paths'],true);
  end;
end;

{$endregion}

end.

