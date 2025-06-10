program BuildMushrooms;

uses
  SysUtils,
  ARGBImageUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  TextureAtlasGeneratorUnit,
  AnimationDataUnit,
  GradientUnit,
  Logger,
  MKStream;

const
  MUSHROOMSIZE=80;
  FPS=48;

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fShroomOverlay,
    fShroomInside,
    fShroomSlot,
    fShroomTop:TARGBImage;
    fAtlas:TTextureAtlasGeneratorFree;
    fRaw:TARGBImage;
    fColors:TGradient;
    function CreateMushroom(pRotation:integer;pOverlay:TARGBImage):TARGBImage;
    function CreateMushroom2(pFrameNo:integer;pFrom,pTo:TARGBImage):TARGBImage;
    procedure FillInside(pImage:TARGBImage;pRotation:integer);
    function GetColorAt(pX,pY,pRotation:integer):dword;
    procedure PutSlot(pImage:TARGBImage;pRotation:integer);
    function GetAngleAt(pX,pY:integer):integer;
  end;

{ TMain }

constructor TMain.Create;
begin
  {$ifdef DEBUG}
  MKStreamOpener.AddDirectory('..\..\work\gfx\data',0);
  {$else}
  MKStreamOpener.AddDirectory('.\data',0);
  {$endif}
  fShroomOverlay:=TARGBImage.Create('shroomoverlay.png');
  fShroomOverlay.SetColorkey(0,0,0);
  fShroomInside:=TARGBImage.Create('shroominside.png');
  fShroomSlot:=TARGBImage.Create('shroomslot.png');
  fShroomSlot.SetColorkey(0,0,0);
  fShroomTop:=TARGBImage.Create('shroomtop.png');
  fRaw:=TARGBImage.Create(15*MUSHROOMSIZE,MUSHROOMSIZE);
  fAtlas:=TTextureAtlasGeneratorFree.Create(15*(MUSHROOMSIZE+1)+1,(MUSHROOMSIZE+1)*3+1,1);
end;

destructor TMain.Destroy;
begin
  fRaw.Free;
  if Assigned(fAtlas) then begin
    fAtlas.TextureAtlas.WriteFile('mushroom.png','PNG');
    fAtlas.Free;
  end;
  fShroomTop.Free;
  fShroomOverlay.Free;
  fShroomInside.Free;
  fShroomSlot.Free;
  inherited Destroy;
end;

procedure TMain.Run;
var
  raw,tmp,tmpfrom,tmpto:TARGBImage;
  i:integer;
  tmpA:TTimeBasedAnimationData;
begin
  tmpfrom:=TARGBImage.Create(MUSHROOMSIZE,MUSHROOMSIZE);
  tmpto:=TARGBImage.Create(MUSHROOMSIZE,MUSHROOMSIZE);
  raw:=TARGBImage.Create(MUSHROOMSIZE*15,MUSHROOMSIZE);
  try
    tmpA:=TTimeBasedAnimationData.Create(MUSHROOMSIZE,MUSHROOMSIZE);
    tmpA.RandomStart:=false;
    tmpA.Paused:=true;
    tmpA.FPS:=FPS;
    tmpA.Name:='MushroomL';
    for i:=0 to 14 do begin
      tmp:=CreateMushroom(i*6,fShroomTop);
      try
        if i=0 then tmpto.PutImage(0,0,tmp);
        raw.PutImage(i*MUSHROOMSIZE,0,tmp,false);
        tmpA.AddFrame(i*MUSHROOMSIZE,0);
      finally
        tmp.Free;
      end;
    end;
    raw.SetColorkey(0,0,0);
    raw.Animations.AddObject(tmpA.Name,tmpA);
    fAtlas.AddImage(raw);
  finally
    raw.Free;
  end;
  fColors:=TGradient.Create($FFAC6444,$FF804620);
  try
    fColors.PingPong:=true;
    raw:=TARGBImage.Create(MUSHROOMSIZE*15,MUSHROOMSIZE);
    try
      tmpA:=TTimeBasedAnimationData.Create(MUSHROOMSIZE,MUSHROOMSIZE);
      tmpA.RandomStart:=false;
      tmpA.Paused:=true;
      tmpA.FPS:=FPS;
      for i:=0 to 14 do begin
        tmp:=CreateMushroom(i*6,fShroomOverlay);
        try
          if i=0 then tmpfrom.PutImage(0,0,tmp);
          raw.PutImage(i*MUSHROOMSIZE,0,tmp,false);
          tmpA.AddFrame(i*MUSHROOMSIZE,0);
        finally
          tmp.Free;
        end;
      end;
      tmpA.Name:='MushroomD';
      raw.SetColorkey(0,0,0);
      raw.Animations.AddObject(tmpA.Name,tmpA);
      fAtlas.AddImage(raw);
    finally
      raw.Free;
    end;
  finally
    FreeAndNil(fColors);
  end;
  raw:=TARGBImage.Create(MUSHROOMSIZE*15,MUSHROOMSIZE);
  try
    tmpA:=TTimeBasedAnimationData.Create(MUSHROOMSIZE,MUSHROOMSIZE);
    tmpA.RandomStart:=false;
    tmpA.Paused:=true;
    tmpA.FPS:=FPS;
    for i:=0 to 14 do begin
      tmp:=CreateMushroom2(i,tmpFrom,tmpTo);
      try
        raw.PutImage(i*MUSHROOMSIZE,0,tmp,false);
        tmpA.AddFrame(i*MUSHROOMSIZE,0);
      finally
        tmp.Free;
      end;
    end;
    tmpA.Name:='MushroomC';
    raw.SetColorkey(0,0,0);
    raw.Animations.AddObject(tmpA.Name,tmpA);
    fAtlas.AddImage(raw);
  finally
    raw.Free;
  end;
  tmpto.Free;
  tmpfrom.Free;
end;

function TMain.CreateMushroom(pRotation: integer; pOverlay: TARGBImage): TARGBImage;
begin
  Result:=TARGBImage.Create(MUSHROOMSIZE,MUSHROOMSIZE);
  Result.Bar(0,0,Result.Width,Result.Height,0);
  Result.SetColorkey(0,0,0);
  if Assigned(fColors) then FillInside(Result,pRotation);
  Result.PutImage(0,0,pOverlay,true);
  PutSlot(Result,pRotation);
  PutSlot(Result,pRotation+90);
  PutSlot(Result,pRotation+180);
  PutSlot(Result,pRotation+270);
end;

function TMain.CreateMushroom2(pFrameNo: integer; pFrom, pTo: TARGBImage): TARGBImage;
var i,j,angle:integer;
begin
  Result:=TARGBImage.Create(MUSHROOMSIZE,MUSHROOMSIZE);
  Result.Bar(0,0,Result.Width,Result.Height,0);
  Result.SetColorkey(0,0,0);
  for j:=0 to MUSHROOMSIZE-1 do
    for i:=0 to MUSHROOMSIZE-1 do begin
      angle:=GetAngleAt(i,j) mod 90;
      if (angle<43-pFrameNo*3) or (angle>47+pFrameNo*3) then
        Result.Putpixel(i,j,pFrom.GetPixel(i,j))
      else
        Result.Putpixel(i,j,pTo.GetPixel(i,j))
    end;
end;

procedure TMain.FillInside(pImage:TARGBImage; pRotation:integer);
var i,j:integer;
begin
  for i:=0 to MUSHROOMSIZE-1 do
    for j:=0 to MUSHROOMSIZE-1 do
      if fShroomInside.GetPixel(i,j)<>$ff000000 then
        pImage.PutPixel(i,j,GetColorAt(i,j,pRotation));
end;

function TMain.GetColorAt(pX,pY,pRotation:integer):dword;
const dith=0.125;
var i:integer;d:double;
begin
  i:=GetAngleAt(pX,pY);
  d:=((round((i+pRotation)*32)) mod 360)/359;
  d+=random*dith-(dith/2);
  Result:=fColors.GetColorAt(d);
end;

procedure TMain.PutSlot(pImage: TARGBImage; pRotation: integer);
const pirad=PI/180;
var x,y:integer;
begin
  x:=round(sin(pRotation*pirad)*27)+(MUSHROOMSIZE div 2)+1;
  y:=round(cos(pRotation*pirad)*27)+(MUSHROOMSIZE div 2)+1;
  Log.LogStatus(Format('%.3d(%d, %d),',[pRotation,x-1,y-1]));
  fShroomSlot.CopyTo(0,0,16,16,x-8,y-8,pImage,true);
end;

function TMain.GetAngleAt(pX, pY: integer): integer;
begin
  if ((MUSHROOMSIZE div 2)>pX) then begin
    Result:=trunc(arctan(((MUSHROOMSIZE div 2)-pY)/((MUSHROOMSIZE div 2)-pX))*180/pi)+270;
  end else
  if ((MUSHROOMSIZE div 2)<pX) then begin
    Result:=trunc(arctan(((MUSHROOMSIZE div 2)-pY)/((MUSHROOMSIZE div 2)-pX))*180/pi)+90;
  end else begin
    if ((MUSHROOMSIZE div 2)>=pY) then begin
      Result:=0;
    end else begin
      Result:=180;
    end;
  end;
end;

begin
  with TMain.Create do try
    Run;
  finally
    Free;
  end;
end.

