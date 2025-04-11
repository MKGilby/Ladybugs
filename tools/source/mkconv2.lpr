{
  This file is part of the source code of MKConv2.
  See "copyright.txt" for details.
}

program mkconv2;

{$mode delphi}
{$smartlink on}

uses
  MKToolBox, Classes, MKINIFile, SysUtils, Lists, Logger,
  AnimationDataUnit, FontDataUnit, ARGBImageUnit,
//  ARGBImageGSDReaderUnit, ARGBImageGSDWriterUnit,
  ARGBImagePNGReaderUnit, ARGBImagePNGWriterUnit,
//  ARGBImageTGAReaderUnit, ARGBImageTGAWriterUnit,
//  ARGBImageCELReaderUnit, ARGBImageCELWriterUnit,
//  ARGBImageBMPReaderUnit,
//  ARGBImageBMFReaderUnit, ARGBImageGIFReaderUnit,
  CommandsUnit, TextureAtlasGeneratorUnit, MKStream,
  FileInfo,
  winpeimagereader, AnimXMLReader, AnimJSONReader;

const
  BDate={$I %DATE%};


type
  TMemoryAreaType=(maImage,maTextureAtlas);

  { TMemoryArea }

  TMemoryArea=class
    constructor CreateImage; overload;
    constructor CreateImage(iWidth,iHeight:integer); overload;
    constructor CreateTextureAtlas(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
  private
    fType:TMemoryAreaType;
    fImage:TARGBImage;
    fAtlas:TTextureAtlasGenerator;
    function fGetImage:TARGBImage;
  public
    property Image:TARGBImage read fGetImage;
    property Atlas:TTextureAtlasGenerator read fAtlas;
    property AreaType:TMemoryAreaType read fType;
  end;


var
  t:text;s,p1:string;i:integer;
  mems:array[0..16] of TMemoryArea;
  clmode:boolean;
  Macros:TStringList;
  CommandValidator:TCommandValidator;
  Mode:(moNormal,moMacroRecord);
  newMacro:string;

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then begin
    if PV.Build=0 then
      Result:=Format('%d.%d',[PV.Major,PV.Minor])
    else
      Result:=Format('%d.%d.%d',[PV.Major,PV.Minor,PV.Build])
  end else begin
    if PV.Build=0 then
      Result:=Format('%d.%d.%d',[PV.Major,PV.Minor,PV.Revision])
    else
      Result:=Format('%d.%d.%d.%d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
  end;
end;

function GetWord(var s:String):string;
begin
  s:=alltrim(s)+' ';
  Result:=copy(s,1,pos(' ',s)-1);
  delete(s,1,pos(' ',s));
  if (length(s)>0) and (s[length(s)]=' ') then delete(s,length(s),1);
end;

procedure CollectFiles(pFilename:string;pTargetArea:integer);
var SL:TFileSearchList;atm:TARGBImage;wi,he:integer;
begin
  SL:=TFileSearchList.Create(pFilename,faAnyfile-faDirectory);
  if SL.Count>0 then begin
    if Assigned(mems[pTargetArea]) then FreeAndNil(mems[pTargetArea]);
    atm:=TARGBImage.Create;
    atm.ReadFile(SL[0]);
    wi:=atm.Width;
    he:=atm.Height;
    mems[pTargetArea]:=TMemoryArea.CreateImage(wi*SL.Count,he);

//    TARGBImage.Create(wi*SL.Count,he);
    atm.CopyTo(0,0,wi,he,0,0,mems[pTargetArea].Image);
    FreeAndNil(atm);
    for i:=1 to SL.Count-1 do begin
      atm:=TARGBImage.Create;
      atm.ReadFile(SL[i]);
      if (atm.Width<>wi) or (atm.Height<>he) then
        raise Exception.Create('All images must have the same size!');
      atm.CopyTo(0,0,wi,he,wi*i,0,mems[pTargetArea].Image);
      FreeAndNil(atm);
    end;
  end;
  FreeAndNil(SL);
end;

procedure Comm_R(Parms:TStringList;Help:boolean);
var area,bef:integer;
begin
  if not Help then begin
    area:=strtoint(parms[3]);
    if (uppercase(Parms[1])<>'BMF') and (uppercase(Parms[1])<>'ANIMXML') and (uppercase(Parms[1])<>'JSON') then begin
      if Assigned(mems[area]) then FreeAndNil(mems[area]);
      mems[area]:=TMemoryArea.CreateImage;
    end;
    if ExtractFileExt(parms[2])='' then parms[2]:=parms[2]+'.'+parms[1];
    bef:=mems[area].Image.Animations.Count;
    if uppercase(Parms[1])='ANIMXML' then
      LoadAnimXML(mems[area].Image,parms[2])
    else if uppercase(Parms[1])='JSON' then
      LoadAnimJSON(mems[area].Image,parms[2])
    else
      mems[area].Image.ReadFile(parms[2]);
//    Log.trace(mems[area].Image.Animations.Count);
//    for i:=0 to mems[area].Image.Animations.Count-1 do
//      mems[area].Image.Animations.Items[i].LogData;
    if (uppercase(Parms[1])='BMF') then
      writeln('File read.')
    else if (uppercase(Parms[1])='ANIMXML') or (uppercase(Parms[1])='JSON') then
      writeln(Format('%d animation(s) read from file.',[mems[area].Image.Animations.Count-bef]))
    else
      writeln(Format('File read. (%dx%d)',[mems[area].Image.Width,mems[area].Image.Height]));
  end else begin
    writeln('Syntax:'#13#10);
    writeln('R <PNG|TGA|GSD|CEL|BMP|GIF|BDC> filename.ext area');
    writeln('  to read a picture file.');
    writeln('R BMF filename.ext area');
    writeln('  to read font data for an already readed/created picture.');
    writeln('R <ANIMXML|JSON> filename.ext area');
    writeln('  to read animation data for an already readed/created picture.');
    writeln;
    writeln('  When reading animated files, W TGA and W PNG will save them animated.');
    writeln('  To write them unanimated issue an "M CLEAREXTRA area" before saving.');
//    writeln('  Note to BMFs: Read a picture then read a BMF file to the same area,');
//    writeln('  then write TGA or PNG to save a TGAFont or PNGFont.');
  end;
end;

procedure Comm_W(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[3])]) then
      mems[strtoint(parms[3])].Image.WriteFile(parms[2],parms[1]);
    writeln('File written.');
  end else begin
    writeln('Syntax:'#13#10);
    writeln('W [PNG|TGA|GSD|CEL|BMP] filename.ext area');
    writeln('  In case of writing a nonPNG file, alpha data will be omitted.');
    writeln('  Writing PNG file will contain all animation/font data.');
    writeln('  Writing TGA file will contain the first animation/font data.');
    writeln('  You can add animation data with the "M AddAnimation" command.');
  end;
end;

procedure Comm_C_MUL(Parms:TStringList;Help:boolean);
begin
  if not Help then
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.CombineMul(mems[strtoint(parms[3])].Image)
  else begin
    writeln('Syntax:'#13#10);
    writeln('C MUL sourcearea1 sourcearea2');
    writeln('  Multiplies all R G B and A values in sourcearea1 with the');
    writeln('  corresponding R G B A values in sourcearea2 and takes the');
    writeln('  modulo 256 value.');
  end;
end;

procedure Comm_C_MASK(Parms:TStringList;Help:boolean);
begin
  if not Help then
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.CombineMask(mems[strtoint(parms[3])].Image)
  else begin
    writeln('Syntax:'#13#10);
    writeln('C MASK sourcearea1 maskarea');
    writeln('  Multiplies all R G B and A values in sourcearea1 with the');
    writeln('  corresponding R G B A values in sourcearea2 and takes the');
    writeln('  modulo 256 value.');
  end;
end;

procedure Comm_M_CREATE(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[4])]) then FreeAndNil(mems[strtoint(parms[4])]);
    mems[strtoint(parms[4])]:=TMemoryArea.CreateImage(
      strtoint(parms[2]), strtoint(parms[3]));
    mems[strtoint(parms[4])].Image.Clear;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CREATE width height area');
    writeln('  Creates an empty image with the specified dimensions,');
    writeln('  filled with RGBA 0,0,0,255 (full black opaque).');
  end;
end;

procedure Comm_M_COPY(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[6])]) then
      mems[strtoint(parms[6])].Image.Copy(
        strtoint(parms[2]),  // x1
        strtoint(parms[3]),  // y1
        strtoint(parms[4]),  // wi
        strtoint(parms[5]),  // he
        mems[strtoint(parms[7])].Image);  // trg
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COPY sx sy wi he sourcearea targetarea');
    writeln('  Copies a portion of source to target and sets target size');
    writeln('  according to the copied area.');
    writeln('  (You don''t have to pre-create target.)');
  end;
end;

procedure Comm_M_COPYTO(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[8])]) then
      mems[strtoint(parms[8])].Image.CopyTo(
        strtoint(parms[2]),  // x1
        strtoint(parms[3]),  // y1
        strtoint(parms[4]),  // wi
        strtoint(parms[5]),  // he
        strtoint(parms[6]),  // x2
        strtoint(parms[7]),  // y2
        mems[strtoint(parms[9])].Image);  // trg
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COPYTO sx sy wi he tx ty sourcearea targetarea');
    writeln('  Copies a portion of source to a specified position on Target.');
    writeln('  Target must be pre-created!');
  end;
end;

procedure Comm_M_RECOLORHSV(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[5])]) then
      mems[strtoint(parms[5])].Image.RecolorHSV(
        strtoint(parms[2]),  // h
        strtoint(parms[3]),  // s
        strtoint(parms[4])); // v
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M RECOLORHSV h s v area');
    writeln('  Recolors the image to a given HSV value.');
  end;
end;

procedure Comm_M_RECOLORRGB(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[5])]) then
      mems[strtoint(parms[5])].Image.RecolorRGB(
        strtoint(parms[2]),  // r
        strtoint(parms[3]),  // g
        strtoint(parms[4])); // b
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M RECOLORRGB r g b area');
    writeln('  Recolors the image to a given RGB value.');
  end;
end;

procedure Comm_M_WMATRIX(Parms:TStringList;Help:boolean);
var matrix:TLMatrix;
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[11])]) then
      Matrix[-1,-1]:=strtoint(parms[2]);
      Matrix[0,-1]:=strtoint(parms[3]);
      Matrix[1,-1]:=strtoint(parms[4]);
      Matrix[-1,0]:=strtoint(parms[5]);
      Matrix[0,0]:=strtoint(parms[6]);
      Matrix[1,0]:=strtoint(parms[7]);
      Matrix[-1,1]:=strtoint(parms[8]);
      Matrix[0,1]:=strtoint(parms[9]);
      Matrix[1,1]:=strtoint(parms[10]);
      mems[strtoint(parms[11])].Image.WeightedMatrix(Matrix);
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M WMATRIX v(-1,-1) v(0,-1) v(1,-1) ... v(1,1) area');
    writeln('  Processes the image with the given 3x3 weighted matrix');
  end;
end;

procedure Comm_M_INVERT(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.Invert;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M INVERT area');
    writeln('  Inverts the image''s RGB channels. The alpha channel left unchanged.');
  end;
end;

procedure Comm_M_CLEAR(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.Clear;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CLEAR area');
    writeln('  Clears the image to black opaque.');
  end;
end;

procedure Comm_M_ROTATE(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      if Parms.Count=4 then
        mems[strtoint(parms[2])].Image.Rotate(StrToInt(Parms[3]))
      else
        mems[strtoint(parms[2])].Image.Rotate(1);
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ROTATE area [quadrats]');
    writeln('  Rotates the image by 90° (or quadrats*90°) clockwise.');
  end;
end;

procedure Comm_M_FLIPH(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.FlipH;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M FLIPH area');
    writeln('  Flips the image horizontally.');
  end;
end;

procedure Comm_M_FLIPV(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.FlipV;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M FLIPV area');
    writeln('  Flips the image vertically.');
  end;
end;

procedure Comm_M_PUTPIXEL(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[7])]) then
      mems[strtoint(parms[7])].Image.PutPixel(
        strtoint(parms[2]),  // x
        strtoint(parms[3]),  // y
        strtoint(parms[4]),  // r
        strtoint(parms[5]),  // g
        strtoint(parms[6]),  // b
        strtoint(parms[7])); // a
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M PUTPIXEL x y r g b a area');
    writeln('  Sets a pixel to the given RGBA value.');
  end;
end;

procedure Comm_M_CLEAREXTRA(Parms:TStringList;Help:boolean);
var i:integer;
begin
  if not Help then begin
    i:=strtoint(parms[2]);
    if Assigned(mems[i]) then begin
      if Assigned(mems[i].Image.FontData) then begin
        mems[i].Image.FontData.Free;
        mems[i].Image.FontData:=nil;
      end;
      mems[i].Image.Animations.Clear;
    end;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CLEAREXTRA area');
    writeln('  Clears font and animation data from the image.');
  end;
end;

procedure Comm_M_GRAYSCALE(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[StrToInt(Parms[2])].Image.Grayscale;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M GRAYSCALE area');
    writeln('  Grayscales the image.');
  end;
end;

procedure Comm_M_RESIZE2X(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[StrToInt(Parms[2])].Image.Resize2x;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M RESIZE2X area');
    writeln('  Enlarges the image doubling its size.');
  end;
end;

procedure Comm_M_HALVE(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      if parms.Count=4 then
        mems[strtoint(parms[3])].Image.HalveImage(StrToInt(Parms[2]))
      else
        mems[strtoint(parms[2])].Image.HalveImage(1);
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M HALVE [n] area');
    writeln('  Halves the image n times (default 1).');
  end;
end;

procedure Comm_M_COLLECT(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    CollectFiles(parms[2],strtoint(parms[3]));
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COLLECT filenamewithwildcards area');
    writeln('  Collects all matching image files into one image.');
    WriteLn('  All images must have the same dimensions!');
  end;
end;

procedure Comm_M_CROP(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[6])]) then begin
      if mems[strtoint(parms[6])].AreaType=maImage then begin
        mems[strtoint(parms[6])].Image.Crop(
          strtoint(parms[2]),  // r
          strtoint(parms[3]),  // g
          strtoint(parms[4]),  // b
          strtoint(parms[5])); // a
      end else writeln('* Error: Crop can be used on Images only! (Use Crop2 on TextureAtlas!)');
    end else writeln('* Error: area is empty!');
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CROP r g b a area');
    writeln('  Crops the image to the possible smallest size, considering the');
    WriteLn('  given RGBA color as background.');
  end;
end;

procedure Comm_M_CROP2(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then begin
      if mems[strtoint(parms[2])].AreaType=maTextureAtlas then begin
        mems[strtoint(parms[2])].Atlas.Crop;
      end else writeln('* Error: Crop2 can be used on TextureAtlas only! (Use Crop on Images!)');
    end else writeln('* Error: area is empty!');
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CROP2 area');
    writeln('  Crops the textureatlas to the possible smallest size, keeping padding.');
  end;
end;

procedure Comm_M_SETCOLORKEY(Parms:TStringList;Help:boolean);
begin
  if not Help then
    if Assigned(mems[strtoint(parms[5])]) then
      mems[strtoint(parms[5])].Image.SetColorkey(strtoint(parms[2]),strtoint(parms[3]),strtoint(parms[4]))
  else begin
    writeln('Syntax:'#13#10);
    writeln('M SETCOLORKEY r g b area');
    writeln('  Sets the specified color to transparent, all others to opaque.');
  end;
end;

procedure Comm_M_ADDFRAMEBASEDANIM(Parms:TStringList;Help:boolean);
var atm:TFrameBasedAnimationData;b:byte;
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[parms.Count-1])]) then begin
      atm:=TFrameBasedAnimationData.Create(
        strtoint(parms[3]),  // Frame Width
        strtoint(parms[4])); // Frame Height
      atm.Name:=parms[2];
      atm.FrameDelay:=strtoint(parms[6]);
      atm.LoopDelay:=strtoint(parms[7]);
      atm.StartFrame:=strtoint(parms[8]);
      b:=strtoint(parms[9]);
      atm.Looped:=(b and 1)>0;
      atm.RandomStart:=(b and 2)>0;
      atm.Paused:=(b and 4)>0;
      atm.PingPong:=(b and 8)>0;
      atm.ReverseAnim:=(b and 16)>0;
      Log.LogDebug(Format('Adding animation %s.',[atm.Name]));
      for i:=0 to strtoint(parms[5])-1 do
        atm.AddFrame(strtoint(parms[10+i*2]),strtoint(parms[11+i*2]));
      atm.LogData;
      mems[strtoint(parms[parms.Count-1])].Image.Animations.AddObject(atm.Name,atm);
    end;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ADDFRAMEBASEDANIM name framewidth frameheight framecount framedelay');
    writeln('  loopdelay startframe flags <frames> area');
    writeln('  Flags add up: 1-Looped, 2-RandomStart, 4-Paused, 8-Pingpong, 16-ReverseAnim.');
    writeln('  <frames> contains framecount*2 integers containing left and top');
    writeln('  coordinates of each frame in the image.');
  end;
end;

procedure Comm_M_ADDTIMEBASEDANIM(Parms:TStringList;Help:boolean);
var atm:TTimeBasedAnimationData;b:byte;
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[parms.Count-1])]) then begin
      atm:=TTimeBasedAnimationData.Create(
        strtoint(parms[3]),  // Frame Width
        strtoint(parms[4])); // Frame Height
      atm.Name:=parms[2];
      atm.FPS:=StrToFloat(parms[6],FS);
      atm.LoopDelay:=StrToFloat(parms[7],FS);
      atm.StartFrame:=strtoint(parms[8]);
      b:=strtoint(parms[9]);
      atm.Looped:=(b and 1)>0;
      atm.RandomStart:=(b and 2)>0;
      atm.Paused:=(b and 4)>0;
      atm.PingPong:=(b and 8)>0;
      atm.ReverseAnim:=(b and 16)>0;
      Log.LogDebug(Format('Adding animation %s.',[atm.Name]));
      for i:=0 to strtoint(parms[5])-1 do
        atm.AddFrame(strtoint(parms[10+i*2]),strtoint(parms[11+i*2]));
      atm.LogData;
      mems[strtoint(parms[parms.Count-1])].Image.Animations.AddObject(atm.Name,atm);
    end;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ADDTIMEBASEDANIM name framewidth frameheight framecount FPS');
    writeln('  loopdelay startframe flags <frames> area');
    writeln('  Flags add up: 1-Looped, 2-RandomStart, 4-Paused, 8-Pingpong, 16-ReverseAnim.');
    writeln('  <frames> contains framecount*2 integers containing left and top');
    writeln('  coordinates of each frame in the image.');
  end;
end;

procedure Comm_M_ADDPADDING(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[8])]) then
      mems[strtoint(parms[8])].Image.AddPadding(
        strtoint(parms[2]),  // HorizontalFrameCount
        strtoint(parms[3]),  // VerticalFrameCount
        strtoint(parms[4]),  // r
        strtoint(parms[5]),  // g
        strtoint(parms[6]),  // b
        strtoint(parms[7])); // a
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ADDPADDING horizontalframecount verticalframecount r g b a area');
    writeln('  Adds padding between frames to avoid artifacts with fullscreen scaling.');
    writeln('  Does not modifies already added animation data!');
  end;
end;

procedure Comm_M_COPYRTOALPHA(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.CopyRToAlpha;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COPYRTOALPHA area');
    writeln('  Copies the red channel to the alpha channel.');
  end;
end;

procedure Comm_M_COPYGTOALPHA(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.CopyGToAlpha;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COPYGTOALPHA area');
    writeln('  Copies the green channel to the alpha channel.');
  end;
end;

procedure Comm_M_COPYBTOALPHA(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[2])]) then
      mems[strtoint(parms[2])].Image.CopyBToAlpha;
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M COPYBTOALPHA area');
    writeln('  Copies the blue channel to the alpha channel.');
  end;
end;

procedure Comm_M_BAR(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[10])]) then
    mems[strtoint(parms[10])].Image.Bar(
      strtoint(parms[2]),  // x
      strtoint(parms[3]),  // y
      strtoint(parms[4]),  // wi
      strtoint(parms[5]),  // he
      strtoint(parms[6]),  // r
      strtoint(parms[7]),  // g
      strtoint(parms[8]),  // b
      strtoint(parms[9])); // a
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M BAR x y wi he r g b a area');
    writeln('  Draws a filled bar to the image. Any color channel value set to -1');
    writeln('  will be left unchanged.');
  end;
end;

procedure Comm_M_CREATEATLAS(Parms:TStringList;Help:boolean);
begin
  if not Help then begin
    if Assigned(mems[strtoint(parms[5])]) then FreeAndNil(mems[strtoint(parms[5])]);
    mems[strtoint(parms[5])]:=TMemoryArea.CreateTextureAtlas(
      strtoint(parms[2]), strtoint(parms[3]), strtoint(parms[4]));
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M CREATEATLAS width height padding area');
    writeln('  Creates a new textureatlas with the specified dimensions.');
    writeln('  You can add animations to it with ADD or ADDALL.');
  end;
end;

procedure Comm_M_ADD(Parms:TStringList;Help:boolean);
var SourceArea,TargetArea:integer;
begin
  if not Help then begin
    SourceArea:=strtoint(parms[2]);
    TargetArea:=strtoint(parms[4]);
    if Assigned(mems[SourceArea]) then begin
      if Assigned(mems[TargetArea]) then begin
        if mems[TargetArea].AreaType=maTextureAtlas then begin
          if mems[SourceArea].Image.Animations.IndexOf(parms[3])>-1 then begin
            mems[TargetArea].Atlas.AddImage(mems[SourceArea].Image,parms[3]);
          end else
            writeln(Format('Animation (%s) not found in source area(%d)!',[parms[3],SourceArea]));
        end else
          writeln(Format('Target (%d) is not textureatlas!',[TargetArea]));
      end else
        writeln(Format('Target (%d) is not yet created!',[TargetArea]));
    end else
      writeln(Format('Source area (%d) is empty!',[SourceArea]));
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ADD sourcearea animationname targetarea');
    writeln('  Adds the specified animation from sourcearea to textureatlas in');
    writeln('  targetarea.');
  end;
end;

procedure Comm_M_ADDALL(Parms:TStringList;Help:boolean);
var SourceArea,TargetArea:integer;
begin
  if not Help then begin
    SourceArea:=strtoint(parms[2]);
    TargetArea:=strtoint(parms[3]);
    if Assigned(mems[SourceArea]) then begin
      if Assigned(mems[TargetArea]) then begin
        if mems[TargetArea].AreaType=maTextureAtlas then begin
          mems[TargetArea].Atlas.AddImage(mems[SourceArea].Image);
        end else
          writeln(Format('Target (%d) is not textureatlas!',[TargetArea]));
      end else
        writeln(Format('Target (%d) is not yet created!',[TargetArea]));
    end else
      writeln(Format('Source area (%d) is empty!',[SourceArea]));
  end else begin
    writeln('Syntax:'#13#10);
    writeln('M ADDALL sourcearea targetarea');
    writeln('  Adds all animations from sourcearea to textureatlas in targetarea.');
  end;
end;

procedure CommGroup_W(Parms:TStringList;{%H-}Help:boolean);
begin
  Comm_W(Parms,true);
end;

procedure CommGroup_R(Parms:TStringList;{%H-}Help:boolean);
begin
  Comm_R(Parms,true);
end;

procedure CommGroup_M({%H-}Parms:TStringList;{%H-}Help:boolean);
begin
  writeln('M commands for images:');
  writeln;
  writeln('  M CREATE width height area');
  writeln('  M COPY sx sy wi he sourcearea targetarea');
  writeln('  M COPYTO sx sy wi he tx ty sourcearea targetarea');
  writeln('  M RECOLORHSV h s v area');
  writeln('  M RECOLORRGB r g b area');
  writeln('  M MATRIX v(-1,-1) v(0,-1) v(1,-1) ... v(1,1) area');
  writeln('  M INVERT area');
  writeln('  M CLEAR area');
  writeln('  M ROTATE area [quadrats]');
  writeln('  M FLIPH area');
  writeln('  M FLIPV area');
  writeln('  M PUTPIXEL x y r g b a area');
  writeln('  M CLEAREXTRA area');
  writeln('  M GRAYSCALE area');
  writeln('  M RESIZE2X area');
  writeln('  M COLLECT filenamewithwildcards area');
  writeln('  M CROP r g b a area');
  writeln('  M SETCOLORKEY r g b area');
  writeln('  M ADDANIMATION name framewidth frameheight framecount framedelay');
  writeln('    loopdelay startframe flags <frames>');
  writeln('  M ADDPADDING horizontalframecount verticalframecount r g b a area');
  writeln('  M COPY<R|G|B>TOALPHA area');
  writeln('  M BAR x y wi he r g b a area');
  writeln;
  writeln('M commands for textureatlases:');
  writeln('  M CREATEATLAS width height padding area');
  writeln('  M ADD sourcearea animationname targetarea');
  writeln('  M ADDALL sourcearea targetarea');
  writeln;
  writeln('Issue each command without parameters for help.');
end;

procedure CommGroup_C({%H-}Parms:TStringList;{%H-}Help:boolean);
begin
  writeln('C commands:');
  writeln;
  writeln('  C MUL sourcearea1 sourcearea2');
  writeln('  C MASK sourcearea1 maskarea');
  writeln;
  writeln('Issue each command without parameters for help.');
end;

procedure ProcessLine2(s:String);
var s2:string;
    parms:TStringList;
//    w,w2:word;

  procedure ProcessMacros;
  var i,j,maci:integer;
      s,mac,s2:String;b:Byte;
      sl:TFileSearchList;
  begin
    maci:=-1;
    for i:=0 to Macros.Count-1 do begin
      mac:=Macros.Strings[i];
      if uppercase(copy(mac,1,pos('|',mac)-1))=uppercase(Parms[1]) then begin
        maci:=i;
        break;
      end;
    end;
    if maci>-1 then begin
      mac:=Macros.Strings[maci];
      delete(mac,1,pos('|',mac));
      if (pos('*',parms[2])>0) or (pos('?',parms[2])>0) then begin
        sl:=TFileSearchList.Create;
        sl.SearchAndFill(parms[2],faAnyFile-faDirectory);
        for j:=0 to sl.Count-1 do begin
          parms[2]:=copy(sl.Strings[j],1,rpos('.',sl.Strings[j],1)-1);
          s2:=mac;
          while length(s2)>0 do begin
            s:=copy(s2,1,pos('|',s2)-1);
            delete(s2,1,pos('|',s2));
            while pos('%',s)>0 do begin
              b:=pos('%',s);
              s:=copy(s,1,b-1)+parms[ord(s[b+1])-47]+copy(s,b+2,255);
            end;
            ProcessLine2(s);
          end;
        end;
      end else begin
        s2:=mac;
        while length(s2)>0 do begin
          s:=copy(s2,1,pos('|',s2)-1);
          delete(s2,1,pos('|',s2));
          while pos('%',s)>0 do begin
            b:=pos('%',s);
            s:=copy(s,1,b-1)+parms[ord(s[b+1])-47]+copy(s,b+2,255);
          end;
          ProcessLine2(s);
        end;
      end;
    end;
  end;

begin
  s:=alltrim(s);
  if (length(s)=0) or (s[1]=';') then exit;
  if (mode=moMacroRecord) and (s[1]<>'@') then begin
    newmacro+=s+'|';
    exit;
  end;
  parms:=TStringList.Create;
// ------- Splitting line -------
  repeat
    s2:=GetWord(s);
    if (s2[1]='%') and (mode=moNormal) then begin   // Decode macros
      delete(s2,1,1);
      if uppercase(s2[1])='W' then begin
        delete(s2,1,1);
        s2:=inttostr(mems[strtoint(s2)].Image.Width);
      end;
      if uppercase(s2[1])='H' then begin
        delete(s2,1,1);
        s2:=inttostr(mems[strtoint(s2)].Image.Height);
      end;
    end;
    parms.Add(s2);
  until length(s)=0;
  if parms.Count>0 then parms[0]:=uppercase(parms[0]);
  if parms.Count>1 then parms[1]:=uppercase(parms[1]);
  if parms[0]='!' then begin
    ProcessMacros;
  end else
  if parms[0]='@' then begin
    if mode=moMacroRecord then begin
      Macros.Add(newmacro);
      mode:=moNormal;
      writeln('Macro recorded: ',newmacro);
    end else begin
      newmacro:=parms[1]+'|';
      mode:=moMacroRecord;
      writeln('Macro recording mode');
    end;
  end else
    CommandValidator.ValidateAndCallCommand(Parms);
  FreeAndNil(Parms);
end;

procedure BaseHelp(Parms:TStringList;{%H-}Help:boolean);
begin
  writeln('Unknown command! (',parms[0],')');
  writeln('Commands:');
  writeln('R - Read an image');
  writeln('W - Write an image');
  writeln('C - Combine images');
  writeln('M - Manipulate images');
  writeln('Q - Quit program'#10);
  writeln('Issue each command without parameters for help.');
end;

procedure AddCommands;
begin
  CommandValidator.AddCommand('"W"',CommGroup_W);
  CommandValidator.AddCommand('"R"',CommGroup_R);
  CommandValidator.AddCommand('"M"',CommGroup_M);
  CommandValidator.AddCommand('"C"',CommGroup_C);
  CommandValidator.AddCommand('"W" <"PNG"|"TGA"|"GSD"|"CEL"|"BMP"> s i',Comm_W);
  CommandValidator.AddCommand('"R" <"PNG"|"TGA"|"GSD"|"CEL"|"BMP"|"BMF"|"GIF"|"BDC"|"ANIMXML"|"JSON"> s i',Comm_R);
  CommandValidator.AddCommand('"C" "MUL" i i',Comm_C_MUL);
  CommandValidator.AddCommand('"C" "MASK" i i',Comm_C_MASK);
  CommandValidator.AddCommand('"M" "CREATE" i i i',Comm_M_CREATE);
  CommandValidator.AddCommand('"M" "COPY" i i i i i i',Comm_M_COPY);
  CommandValidator.AddCommand('"M" "COPYTO" i i i i i i i i',Comm_M_COPYTO);
  CommandValidator.AddCommand('"M" "RECOLORHSV" b b b i',Comm_M_RECOLORHSV);
  CommandValidator.AddCommand('"M" "RECOLORRGB" b b b i',Comm_M_RECOLORRGB);
  CommandValidator.AddCommand('"M" "WMATRIX" i i i i i i i i i i',Comm_M_WMATRIX);
  CommandValidator.AddCommand('"M" "SETCOLORKEY" b b b i',Comm_M_SETCOLORKEY);
  CommandValidator.AddCommand('"M" "ADDFRAMEBASEDANIM" s i i i i i i b @5[i i] i',Comm_M_ADDFRAMEBASEDANIM);
  CommandValidator.AddCommand('"M" "ADDTIMEBASEDANIM" s i i i i i i b @5[i i] i',Comm_M_ADDTIMEBASEDANIM);
  CommandValidator.AddCommand('"M" "INVERT" i',Comm_M_INVERT);
  CommandValidator.AddCommand('"M" "CLEAR" i',Comm_M_CLEAR);
  CommandValidator.AddCommand('"M" "ROTATE" i',Comm_M_ROTATE);
  CommandValidator.AddCommand('"M" "ROTATE" i i',Comm_M_ROTATE);
  CommandValidator.AddCommand('"M" "FLIPH" i',Comm_M_FLIPH);
  CommandValidator.AddCommand('"M" "FLIPV" i',Comm_M_FLIPV);
  CommandValidator.AddCommand('"M" "PUTPIXEL" i i b b b b i',Comm_M_PUTPIXEL);
  CommandValidator.AddCommand('"M" "CLEAREXTRA" i',Comm_M_CLEAREXTRA);
  CommandValidator.AddCommand('"M" "GRAYSCALE" i',Comm_M_GRAYSCALE);
  CommandValidator.AddCommand('"M" "RESIZE2X" i',Comm_M_RESIZE2X);
  CommandValidator.AddCommand('"M" "COLLECT" s i',Comm_M_COLLECT);
  CommandValidator.AddCommand('"M" "CROP" b b b b i',Comm_M_CROP);
  CommandValidator.AddCommand('"M" "CROP2" i',Comm_M_CROP2);
  CommandValidator.AddCommand('"M" "ADDPADDING" i i b b b b i',Comm_M_ADDPADDING);
  CommandValidator.AddCommand('"M" "COPYRTOALPHA" i',Comm_M_COPYRTOALPHA);
  CommandValidator.AddCommand('"M" "COPYGTOALPHA" i',Comm_M_COPYGTOALPHA);
  CommandValidator.AddCommand('"M" "COPYBTOALPHA" i',Comm_M_COPYBTOALPHA);
  CommandValidator.AddCommand('"M" "BAR" i i i i i i i i i',Comm_M_BAR);
  CommandValidator.AddCommand('"M" "CREATEATLAS" i i i i',Comm_M_CREATEATLAS);
  CommandValidator.AddCommand('"M" "ADD" i s i',Comm_M_ADD);
  CommandValidator.AddCommand('"M" "ADDALL" i i',Comm_M_ADDALL);
  CommandValidator.AddCommand('"M" "HALVE" i i',Comm_M_HALVE);
  CommandValidator.AddCommand('"M" "HALVE" i',Comm_M_HALVE);
end;

procedure LoadMacros;
var i,j:integer;
    INI:TINIFile;
    s,s2:String;
begin
  if MKStreamOpener.FileExists('MKConv.ini') then begin
    INI:=TINIFile.Create('MKConv.ini');
    for i:=0 to INI.ListItemCount('Macros')-1 do begin
      s:=INI.ReadListItem('Macros',i);
      s2:=s+'|';
      for j:=0 to INI.ListItemCount(s)-1 do
        s2:=s2+INI.ReadListItem(s,j)+'|';
      Macros.Add(s2);
    end;
    INI.Free;
  end;
end;

{ TMemoryArea }

constructor TMemoryArea.CreateImage;
begin
  fType:=maImage;
  fImage:=TARGBImage.Create;
  fAtlas:=nil;
end;

constructor TMemoryArea.CreateImage(iWidth, iHeight: integer);
begin
  fType:=maImage;
  fImage:=TARGBImage.Create(iWidth, iHeight);
  fAtlas:=nil;
end;

constructor TMemoryArea.CreateTextureAtlas(iWidth, iHeight, iPadding: integer);
begin
  fType:=maTextureAtlas;
  fImage:=nil;
  fAtlas:=TTextureAtlasGenerator.Create(iWidth,iHeight,iPadding);
end;

destructor TMemoryArea.Destroy;
begin
  if Assigned(fImage) then FreeAndNil(fImage);
  if Assigned(fAtlas) then FreeAndNil(fAtlas);
  inherited Destroy;
end;

function TMemoryArea.fGetImage: TARGBImage;
begin
  if Assigned(fImage) then Result:=fImage
  else if Assigned(fAtlas) then Result:=fAtlas.TextureAtlas
  else Result:=nil;
end;

{$R *.res}

begin
  if paramcount=0 then clmode:=true
                  else clmode:=false;

{$ifdef DEBUG}
  Log.SetLogLevel(llAll);
{$else}
  Log.SetLogLevel(llWarning);
{$endif}

  Macros:=TStringList.Create;
  LoadMacros;
  mode:=moNormal;
//  Log.SetScreenOutputOn;

  MKStreamOpener.AddDirectory('.',0);
  CommandValidator:=TCommandValidator.Create(BaseHelp);
  AddCommands;

  for i:=0 to 16 do mems[i]:=nil;

  writeln(Format('MKSZTSZ Converter %s - (C) 2020-%d MKSZTSZ',[GetVersionString,{$I %DATEYEAR%}]));
  writeln('Build date: ',StringReplace(BDate,'/','.',[rfReplaceAll]),#10);
  if clmode then begin
    writeln('Enter commands:');
    repeat
      write('>');
      readln(s);
      if (uppercase(s)='EXIT') or (uppercase(s)='QUIT') or (uppercase(s)='BYE') or (uppercase(s)='Q') then break;
      ProcessLine2(s);
    until false;
  end else begin
    p1:=paramstr(1);
    assign(t,p1);
    reset(t);
    repeat
      readln(t,s);
      writeln(s);
      if (uppercase(s)='EXIT') or (uppercase(s)='QUIT') or (uppercase(s)='BYE') or (uppercase(s)='Q') then break;
      ProcessLine2(s);
    until eof(t);
    close(t);
  end;
//  Log.SetScreenOutputOff;
  for i:=0 to 16 do if mems[i]<>nil then mems[i].Free;
//  Macros.SaveToFile('macros.txt');
  FreeAndNil(CommandValidator);
  FreeAndNil(Macros);
end.
