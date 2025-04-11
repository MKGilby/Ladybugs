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
    fBug1,fBug2:TARGBImage; // Normal
    fBug3,fBug4,fBug5:TARGBImage; // Flying
    fAtlas:TTextureAtlasGenerator;
    procedure GenerateBug(cnt,r,g,b:integer);
    procedure GenerateBug2(cnt,r,g,b:integer);
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
  try
    fBug1:=TARGBImage.Create(16,16);
    fBaseBug.CopyTo(16,0,16,16,0,0,fBug1);
    fBug2:=TARGBImage.Create(16,16);
    fBaseBug.CopyTo(0,0,16,16,0,0,fBug2);
  finally
    fBaseBug.Free;
  end;
  fBug2.SetColorkey(0,0,0);
  fBaseBug:=TARGBImage.Create('flying_ladybug_base.png');
  try
    fBug3:=TARGBImage.Create(24,16);
    fBaseBug.CopyTo(48,0,24,16,0,0,fBug3);
    fBug4:=TARGBImage.Create(24,16);
    fBaseBug.CopyTo(0,0,24,16,0,0,fBug4);
    fBug5:=TARGBImage.Create(24,16);
    fBaseBug.CopyTo(24,0,24,16,0,0,fBug5);
  finally
    fBaseBug.Free;
  end;
  fBug4.SetColorkey(0,0,0);
  fBug5.SetColorkey(0,0,0);
  fAtlas:=TTextureAtlasGenerator.Create(640-24,480,1);
end;

destructor TMain.Destroy;
begin
  if Assigned(fAtlas) then begin
    fAtlas.Crop;
    fAtlas.TextureAtlas.WriteFile('bugs.png','PNG');
    fAtlas.Free;
  end;
  fBug5.Free;
  fBug4.Free;
  fBug3.Free;
  fBug2.Free;
  fBug1.Free;
  inherited Destroy;
end;

procedure TMain.Run;
begin
  GenerateBug(1,240,40,20);
  GenerateBug2(1,240,40,20);
  GenerateBug(2,240,200,20);
  GenerateBug2(2,240,200,20);
  GenerateBug(3,20,96,240);
  GenerateBug2(3,20,96,240);
  GenerateBug(4,20,240,64);
  GenerateBug2(4,20,240,64);
  GenerateBug(5,200,20,240);
  GenerateBug2(5,200,20,240);
end;

procedure TMain.GenerateBug(cnt, r, g, b: integer);
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
  tmpA.Name:='Bug'+inttostr(cnt);
  tmp.Animations.AddObject(tmpA.Name,tmpA);
  fAtlas.AddImage(tmp);
  FreeAndNil(tmp);
end;

procedure TMain.GenerateBug2(cnt, r, g, b: integer);
var tmp:TARGBImage;tmpA:TFrameBasedAnimationData;
begin
  tmp:=TARGBImage.Create(96,48);
  try
    tmp.Clear;
    fBug3.CopyTo(0,0,24,16,0,0,tmp);
    fBug3.CopyTo(0,0,24,16,0,24,tmp);
    fBug3.Rotate(1);
    fBug3.CopyTo(0,0,16,24,24+8,0,tmp);
    fBug3.CopyTo(0,0,16,24,24+8,24,tmp);
    fBug3.Rotate(1);
    fBug3.CopyTo(0,0,24,16,48,8,tmp);
    fBug3.CopyTo(0,0,24,16,48,32,tmp);
    fBug3.Rotate(1);
    fBug3.CopyTo(0,0,16,24,72,0,tmp);
    fBug3.CopyTo(0,0,16,24,72,24,tmp);
    fBug3.Rotate(1);
    tmp.RecolorRGB(r,g,b);

    fBug4.CopyTo(0,0,24,16,0,0,tmp,true);
    fBug4.Rotate(1);
    fBug4.CopyTo(0,0,16,24,24+8,0,tmp,true);
    fBug4.Rotate(1);
    fBug4.CopyTo(0,0,24,16,48,8,tmp,true);
    fBug4.Rotate(1);
    fBug4.CopyTo(0,0,16,24,72,0,tmp,true);
    fBug4.Rotate(1);

    fBug5.CopyTo(0,0,24,16,0,24,tmp,true);
    fBug5.Rotate(1);
    fBug5.CopyTo(0,0,16,24,24+8,24,tmp,true);
    fBug5.Rotate(1);
    fBug5.CopyTo(0,0,24,16,48,8+24,tmp,true);
    fBug5.Rotate(1);
    fBug5.CopyTo(0,0,16,24,72,24,tmp,true);
    fBug5.Rotate(1);

    tmp.SetColorkey(0,0,0);
    tmpA:=TFrameBasedAnimationData.Create(24,24);
    tmpA.Paused:=true;
    tmpA.AddFrame(0,0);
    tmpA.AddFrame(0,24);
    tmpA.Name:='FBug'+inttostr(cnt)+'1';
    tmp.Animations.AddObject(tmpA.Name,tmpA);
    tmpA:=TFrameBasedAnimationData.Create(24,24);
    tmpA.Paused:=true;
    tmpA.AddFrame(24,0);
    tmpA.AddFrame(24,24);
    tmpA.Name:='FBug'+inttostr(cnt)+'2';
    tmp.Animations.AddObject(tmpA.Name,tmpA);
    tmpA:=TFrameBasedAnimationData.Create(24,24);
    tmpA.Paused:=true;
    tmpA.AddFrame(48,0);
    tmpA.AddFrame(48,24);
    tmpA.Name:='FBug'+inttostr(cnt)+'3';
    tmp.Animations.AddObject(tmpA.Name,tmpA);
    tmpA:=TFrameBasedAnimationData.Create(24,24);
    tmpA.Paused:=true;
    tmpA.AddFrame(72,0);
    tmpA.AddFrame(72,24);
    tmpA.Name:='FBug'+inttostr(cnt)+'4';
    tmp.Animations.AddObject(tmpA.Name,tmpA);
    fAtlas.AddImage(tmp);
  finally
    tmp.Free;
  end;
end;

begin
  with TMain.Create do try
    Run;
  finally
    Free;
  end;
end.

