program BuildMushrooms;

uses
  ARGBImageUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  TextureAtlasGeneratorUnit,
  AnimationDataUnit,
  GradientUnit,
  MKStream;

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
    fAtlas:TTextureAtlasGenerator;
    fRaw:TARGBImage;
    fColors:TGradient;
    procedure CreateMushroom(pLeft,pRotation:integer;pOverlay:TARGBImage);
    procedure FillInside(pImage:TARGBImage;pRotation:integer);
    function GetColorAt(pX,pY,pRotation:integer):dword;
    procedure PutSlot(pImage:TARGBImage;pRotation:integer);
  end;

{ TMain }

constructor TMain.Create;
begin
  {$ifdef DEBUG}
  MKStreamOpener.AddDirectory('..\data',0);
  {$else}
  MKStreamOpener.AddDirectory('.\data',0);
  {$endif}
  fShroomOverlay:=TARGBImage.Create('shroomoverlay.png');
  fShroomOverlay.SetColorkey(0,0,0);
  fShroomInside:=TARGBImage.Create('shroominside.png');
  fShroomSlot:=TARGBImage.Create('shroomslot.png');
  fShroomSlot.SetColorkey(0,0,0);
  fShroomTop:=TARGBImage.Create('shroomtop.png');
  fRaw:=TARGBImage.Create(15*64,64);
  fAtlas:=TTextureAtlasGenerator.Create(15*65+1,66+65,1);
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
var i:integer;tmpA:TFrameBasedAnimationData;
begin
  fColors:=TGradient.Create($FFD8D8D8,$FFAC6444);
  fColors.PingPong:=true;
  fColors.Reversed:=true;
  tmpA:=TFrameBasedAnimationData.Create(64,64);
  tmpA.Paused:=true;
  for i:=0 to 14 do begin
    CreateMushroom(i*64,i*6,fShroomTop);
    tmpA.AddFrame(i*64,0);
  end;
  tmpA.Name:='MushroomL';
  fRaw.SetColorkey(0,0,0);
  fRaw.Animations.AddObject(tmpA.Name,tmpA);
  fAtlas.AddImage(fRaw);
  fColors.Free;
  fColors:=TGradient.Create($FFAC6444,$FF804620);
  fColors.PingPong:=true;
  tmpA:=TFrameBasedAnimationData.Create(64,64);
  tmpA.Paused:=true;
  for i:=0 to 14 do begin
    CreateMushroom(i*64,i*6,fShroomOverlay);
    tmpA.AddFrame(i*64,0);
  end;
  tmpA.Name:='MushroomD';
  fRaw.SetColorkey(0,0,0);
  fRaw.Animations.Clear;
  fRaw.Animations.AddObject(tmpA.Name,tmpA);
  fAtlas.AddImage(fRaw);
  fColors.Free;
end;

procedure TMain.CreateMushroom(pLeft, pRotation: integer; pOverlay: TARGBImage);
var tmp:TARGBImage;
begin
  tmp:=TARGBImage.Create(64,64);
  tmp.Bar(0,0,64,64,0);
  tmp.SetColorkey(0,0,0);
  FillInside(tmp,pRotation);
  pOverlay.CopyTo(0,0,64,64,0,0,tmp,true);
  PutSlot(tmp,pRotation);
  PutSlot(tmp,pRotation+90);
  PutSlot(tmp,pRotation+180);
  PutSlot(tmp,pRotation+270);
  tmp.CopyTo(0,0,64,64,pLeft,0,fRaw);
  tmp.Free;
end;

procedure TMain.FillInside(pImage:TARGBImage; pRotation:integer);
var i,j:integer;
begin
  for i:=0 to 63 do
    for j:=0 to 63 do
      if fShroomInside.GetPixel(i,j)<>$ff000000 then
        pImage.PutPixel(i,j,GetColorAt(i,j,pRotation));
end;

function TMain.GetColorAt(pX,pY,pRotation:integer):dword;
const dith=0.125;
var i:integer;d:double;
begin
  if (32>pX) then begin
    i:=trunc(arctan((32-pY)/(32-pX))*180/pi)+270;
  end else
  if (32<pX) then begin
    i:=trunc(arctan((32-pY)/(32-pX))*180/pi)+90;
  end else begin
    if (32>=pY) then begin
      i:=0;
    end else begin
      i:=180;
    end;
  end;
  d:=((round((i+pRotation)*32)) mod 360)/359;
  d+=random*dith-(dith/2);
  Result:=fColors.GetColorAt(d);
end;

procedure TMain.PutSlot(pImage: TARGBImage; pRotation: integer);
const pirad=PI/180;
var x,y:integer;
begin
  x:=round(sin(pRotation*pirad)*23)+32+1;
  y:=round(cos(pRotation*pirad)*23)+32+1;
  fShroomSlot.CopyTo(0,0,16,16,x-8,y-8,pImage,true);
end;

begin
  with TMain.Create do try
    Run;
  finally
    Free;
  end;
end.

