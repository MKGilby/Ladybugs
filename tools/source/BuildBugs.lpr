program BuildBugs;

{$mode delphi}

uses
  SysUtils, ARGBImageUnit, ARGBImagepngReaderUnit, ARGBImagePNGWriterUnit,
  TextureAtlasGeneratorUnit, AnimationDataUnit, MKStream;

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fBug1,fBug2:TARGBImage;
    fCount:integer;
    fAtlas:TTextureAtlasGenerator;
    procedure GenerateBug(r,g,b:integer);
  end;

{ TMain }

constructor TMain.Create;
var fBaseBug:TARGBImage;
begin
  {$ifdef DEBUG}
  MKStreamOpener.AddDirectory('..\data',0);
  {$else}
  MKStreamOpener.AddDirectory('.\data',0);
  {$endif}
  fBaseBug:=TARGBImage.Create('ladybug_base.png');
  fBug1:=TARGBImage.Create(16,16);
  fBaseBug.CopyTo(16,0,16,16,0,0,fBug1);
  fBug2:=TARGBImage.Create(16,16);
  fBaseBug.CopyTo(0,0,16,16,0,0,fBug2);
  FreeAndNil(fBaseBug);
  fBug2.SetColorkey(0,0,0);
  fAtlas:=TTextureAtlasGenerator.Create(33*20+1,33+1,1);
  fCount:=1;
end;

destructor TMain.Destroy;
begin
  if Assigned(fAtlas) then begin
    fAtlas.Crop;
    fAtlas.TextureAtlas.WriteFile('bugs.png','PNG');
    FreeAndNil(fAtlas);
  end;
  if Assigned(fBug1) then FreeAndNil(fBug1);
  if Assigned(fBug2) then FreeAndNil(fBug2);
  inherited Destroy;
end;

procedure TMain.Run;
begin
  GenerateBug(240,40,20);
  GenerateBug(240,200,20);
  GenerateBug(20,96,240);
  GenerateBug(20,240,64);
  GenerateBug(200,20,240);
end;

procedure TMain.GenerateBug(r, g, b: integer);
var tmp:TARGBImage;tmpA:TFrameBasedAnimationData;i:integer;
begin
  tmp:=TARGBImage.Create(64,16);
  for i:=0 to 3 do begin
    fBug1.CopyTo(0,0,16,16,i*16,0,tmp);
    fBug1.Rotate(1);
  end;
  tmp.RecolorRGB(r,g,b);
  for i:=0 to 3 do begin
    fBug2.CopyTo(0,0,16,16,i*16,0,tmp,true);
    fBug2.Rotate(1);
  end;
  tmp.SetColorkey(0,0,0);
  tmpA:=TFrameBasedAnimationData.Create(16,16);
  tmpA.Paused:=true;
  tmpA.AddFrame(0,0);
  tmpA.AddFrame(16,0);
  tmpA.AddFrame(32,0);
  tmpA.AddFrame(48,0);
  tmpA.Name:='Bug'+inttostr(fCount);
  inc(fCount);
  tmp.Animations.AddObject(tmpA.Name,tmpA);
  fAtlas.AddImage(tmp);
  FreeAndNil(tmp);
end;

begin
  with TMain.Create do try
    Run;
  finally
    Free;
  end;
end.

