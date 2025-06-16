unit BuildGFX_Painter;

{$mode Delphi}

interface

uses
  SysUtils,
  ARGBImageUnit,
  ARGBImagePNGReaderUnit,
  ARGBImagePNGWriterUnit,
  TextureAtlasGeneratorUnit,
  AnimationDataUnit,
  GradientUnit;

type

  { TPainterBuilder }

  TPainterBuilder=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fOverlay:TARGBImage;
    fAtlas:TTextureAtlasGenerator;
    procedure BuildPainter(pNumber,r,g,b:integer);
    function GetColorAt(const pGrad:TGradient;const pX,pY,pRotation:integer):uint32;
    function GetAngleAt(pX,pY:integer):integer;
  end;

implementation

uses BuildGFX_Shared;

const
  ANIMFRAMECOUNT=15;
  PAINTERSIZE=32;
  FPS=19;

{ TPainterBuilder }

constructor TPainterBuilder.Create;
begin
  fOverlay:=TARGBImage.Create('paintertop.png');
//  fOverlay.SetColorkey(0,0,0);
  fAtlas:=TTextureAtlasGenerator.Create(ANIMFRAMECOUNT*33+1,4*33+1,1);
end;

destructor TPainterBuilder.Destroy;
begin
  if Assigned(fAtlas) then begin
    fAtlas.Crop;
    fAtlas.TextureAtlas.WriteFile('painters.png','PNG');
    fAtlas.Free;
  end;
  fOverlay.Free;
  inherited Destroy;
end;

procedure TPainterBuilder.Run;
var i:integer;
begin
  for i:=1 to 4 do
    BuildPainter(i,COLORS[i,0],COLORS[i,1],COLORS[i,2]);
end;

procedure TPainterBuilder.BuildPainter(pNumber, r, g, b: integer);
var tmp:TARGBImage;grad:TGradient;c:uint32;f,i,j:integer;tmpA:TTimeBasedAnimationData;
begin
  tmp:=TARGBImage.Create(ANIMFRAMECOUNT*PAINTERSIZE,PAINTERSIZE);
  c:=$FF000000+(r and $ff)<<16+(g and $ff)<<8+(b and $ff);
  grad:=TGradient.Create($FFFFFFFF,$FF111111);
  grad.Colors[3]:=c;
  grad.ColorPositions[3]:=0.7;
  grad.ColorUsed[3]:=true;
  grad.Colors[4]:=c;
  grad.ColorPositions[4]:=0.3;
  grad.ColorUsed[4]:=true;
  tmpA:=TTimeBasedAnimationData.Create(PAINTERSIZE,PAINTERSIZE);
  tmpA.Name:=Format('Painter%d',[pNumber]);
  tmpA.RandomStart:=true;
  tmpA.Paused:=false;
  tmpA.Looped:=true;
  tmpA.FPS:=FPS;
  try
    for f:=0 to ANIMFRAMECOUNT-1 do begin
      for j:=0 to PAINTERSIZE-1 do
        for i:=0 to PAINTERSIZE-1 do
          if fOverlay.GetPixel(i,j) and $FF000000=0 then
            tmp.PutPixel(f*PAINTERSIZE+i,j,GetColorAt(grad,i,j,f*6));
      tmp.PutImage(f*PAINTERSIZE,0,fOverlay,true);
      tmpA.AddFrame(f*PAINTERSIZE,0);
    end;
    tmp.SetColorkey(0,0,0);
    tmp.Animations.AddObject(tmpA.Name,tmpA);
    fAtlas.AddImage(tmp);
//    tmp.WriteFile(Format('temp%d.png',[pNumber]),'PNG');
  finally
    grad.Free;
    tmp.Free;
  end;
end;

function TPainterBuilder.GetColorAt(const pGrad: TGradient; const pX, pY,
  pRotation: integer): uint32;
const dith=0.100;
var i:integer;d:double;
begin
  i:=GetAngleAt(pX,pY);
  d:=((round((i+pRotation)*4)) mod 360)/359;
  d+=random*dith-(dith/2);
  Result:=pGrad.GetColorAt(d);
end;

function TPainterBuilder.GetAngleAt(pX, pY: integer): integer;
begin
  if ((PAINTERSIZE div 2)>pX) then begin
    Result:=trunc(arctan(((PAINTERSIZE div 2)-pY)/((PAINTERSIZE div 2)-pX))*180/pi)+270;
  end else
  if ((PAINTERSIZE div 2)<pX) then begin
    Result:=trunc(arctan(((PAINTERSIZE div 2)-pY)/((PAINTERSIZE div 2)-pX))*180/pi)+90;
  end else begin
    if ((PAINTERSIZE div 2)>=pY) then begin
      Result:=0;
    end else begin
      Result:=180;
    end;
  end;
end;

end.

