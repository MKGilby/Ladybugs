{ -[Name]-------------------------------------------

              TextureAtlasGenerator class

  -[Disclaimer]-------------------------------------

    See copyright.txt in project sources.

    Written by Gilby/MKSZTSZ   Hungary, 2020-2024

  -[Description]------------------------------------

    You can add TARGBImages with animations to it, and
    it creates a TextureAtlas from them, with the specified
    padding.

    The result is a TARGBImage with the transformed
    animation data.

    You can feed it to a TMediaManager and simply create
    sprites from MediaManager.Animations.ItemByName['Player'].SpawnAnimation.

  --------------------------------------------------
}

// Version info:
//
//  V1.00: Gilby - 2020.07.09
//     * Initial creation
//  V1.01: Gilby - 2020.07.10
//     + LoadImage added (Loads image from file. PNGs with animations.)
//  V1.02: Gilby - 2022.03.18
//     * Following changes in AnimationDataUnit
//     * AddImage now have an optional second parameter to specify one animation
//       name to add.
//  V1.03: Gilby - 2022.07.19
//     * Leaving out duplicate frames within one image.
//  V1.04: Gilby - 2023.05.12
//     * Changing Lists.TGenericList to fgl.TFPGObjectList.
//  V1.05: Gilby - 2023.12.05
//     * Added Crop method to crop image to the minimum size required.
//  V1.06: Gilby - 2023.12.14
//     * Following changes in AnimationDataUnit.
//  V1.06a: Gilby - 2023.12.14
//     * Bugfix in Addimage.
//  V1.07: Gilby - 2024.06.22
//     * Added full frame deduplication.
//  V1.08: Gilby - 2025.04.03
//     * Fix in cropping.
//  V1.09: Gilby - 2025.04.11
//     * Following changes in used units.
//  V1.10: Gilby - 2025.06.05
//     * Added another filling method.

unit TextureAtlasGeneratorUnit;

{$mode delphi}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

interface

uses
  Classes, ARGBImageUnit, fgl;

type

  { TTextureLine }

  TTextureLine=class
    constructor Create(iTop,iHeight,iMaxWidth,iPadding:integer);
    function IsTherePlaceFor(pWidth:integer):boolean;
    function AddImage(pWidth:integer):boolean;
  private
    fTop:integer;
    fHeight:integer;
    fPadding:integer;
    fCurrentLeft:integer;
    fMaxWidth:integer;
  public
    property CurrentLeft:integer read fCurrentLeft;
    property Height:integer read fHeight;
    property Top:integer read fTop;
  end;

  { TTextureLines }

  TTextureLines=class(TFPGObjectList<TTextureLine>)
    function SearchLine(pWidth,pHeight:integer):TTextureLine;
    function CurrentTop:integer;
  end;

  // Fill method of textureatlas:
  //   fmLines - Same height textures goes in a row.
  //   fmFree - The texture goes in the uppermost position where it fits.
  TFillMethod=(fmLines,fmFree);

  { TTextureAtlasGeneratorBase }

  TTextureAtlasGeneratorBase=class
    constructor Create(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
    procedure AddImage(pImage:TARGBImage;pAnimationName:string='');
    procedure LoadImage(pFilename:string);
    procedure Crop;
  protected
    fTextureAtlas:TARGBImage;
    fPadding:integer;
    fFreeTextureAtlas:boolean;
    function fGetTextureAtlas:TARGBImage;
    function SearchForIdenticalFrame(fFrame:TARGBImage;out x:integer;out y:integer):boolean;
    procedure SearchForPlace(const pWidth,pHeight:integer;out x,y:integer);virtual;abstract;
  public
    property TextureAtlas:TARGBImage read fGetTextureAtlas;
    property FreeImage:boolean read fFreeTextureAtlas write fFreeTextureAtlas;
  end;

  { TTextureAtlasGeneratorLines }

  TTextureAtlasGeneratorLines=class(TTextureAtlasGeneratorBase)
    constructor Create(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
  private
    fLines:TTextureLines;
    procedure SearchForPlace(const pWidth,pHeight:integer;out x,y:integer);override;
  end;

  { TTextureAtlasGeneratorFree }

  TTextureAtlasGeneratorFree=class(TTextureAtlasGeneratorBase)
    constructor Create(iWidth,iHeight,iPadding:integer);
    destructor Destroy; override;
  private
    fBitmap:pointer;
    procedure SearchForPlace(const pWidth,pHeight:integer;out x,y:integer); override;
  end;

  { TTextureAtlasGenerator }

  TTextureAtlasGenerator=class
    constructor Create(iWidth,iHeight,iPadding:integer;iFillMethod:TFillMethod=fmLines);
    destructor Destroy; override;
    procedure AddImage(pImage:TARGBImage;pAnimationName:string='');
    procedure LoadImage(pFilename:string);
    procedure Crop;
  private
    fTextureAtlasGenerator:TTextureAtlasGeneratorBase;
    function fGetTextureAtlas:TARGBImage;
    function fGetFreeImage:boolean;
    procedure fSetFreeimage(value:boolean);
  public
    property TextureAtlas:TARGBImage read fGetTextureAtlas;
    property FreeImage:boolean read fGetFreeImage write fSetFreeimage;
  end deprecated 'Use TTextureAtlasGeneratorLines instead';

implementation

uses sysutils, AnimationDataUnit, Logger, MKToolbox;

const
  Fstr={$I %FILE%}+', ';
  Version='1.10';

{ TTextureLine }
{$region /fold}

constructor TTextureLine.Create(iTop,iHeight,iMaxWidth,iPadding:integer);
begin
  fTop:=iTop;
  fHeight:=iHeight;
  fMaxWidth:=iMaxWidth;
  fPadding:=iPadding;
  fCurrentLeft:=fPadding;
end;

function TTextureLine.IsTherePlaceFor(pWidth:integer):boolean;
begin
  Result:=fCurrentLeft+pWidth+fPadding<=fMaxWidth;
end;

function TTextureLine.AddImage(pWidth:integer):boolean;
begin
  if IsTherePlaceFor(pWidth) then begin
    Result:=true;
    fCurrentLeft+=pWidth+fPadding;
  end else Result:=false;
end;

{$endregion}

{ TTextureLines }
{$region /fold}
function TTextureLines.SearchLine(pWidth,pHeight:integer):TTextureLine;
var i:integer;
begin
  for i:=0 to Count-1 do
    if (Self[i].Height=pHeight) and (Self[i].IsTherePlaceFor(pWidth)) then begin
      Result:=Self[i];
      exit;
    end;
  Result:=nil;
end;

function TTextureLines.CurrentTop:integer;
var i:integer;
begin
  Result:=0;
  for i:=0 to Count-1 do
    if Self[i].Top+Self[i].Height>Result then Result:=Self[i].Top+Self[i].Height;
end;

{$endregion}

{ TTextureAtlasGeneratorBase }
{$region /fold}

constructor TTextureAtlasGeneratorBase.Create(iWidth,iHeight,iPadding:integer);
begin
  fTextureAtlas:=TARGBImage.Create(iWidth,iHeight);
  fTextureAtlas.Clear(0);
  fPadding:=iPadding;
  fFreeTextureAtlas:=true;
end;

destructor TTextureAtlasGeneratorBase.Destroy;
begin
  if fFreeTextureAtlas then FreeAndNil(fTextureAtlas);
  inherited Destroy;
end;

procedure TTextureAtlasGeneratorBase.AddImage(pImage:TARGBImage;pAnimationName:string);
var
  anim,frame:integer;
  tmpAnim:TBaseAnimationData;
  tmpFrame:TARGBImage;
  x,y:integer;
begin
  // Iterate trough all animations
  for anim:=0 to pImage.Animations.Count-1 do begin
    // If this is the animation we want to add, or we want to add all animations
    if (pImage.Animations.Items[anim].Name=pAnimationName) or (pAnimationName='') then begin
      // Clone animation data to tmpAnim
      if pImage.Animations.Items[anim] is TFrameBasedAnimationData then
        tmpAnim:=TFrameBasedAnimationData(pImage.Animations.Items[anim]).Clone(true)
      else if pImage.Animations.Items[anim] is TTimeBasedAnimationData then
        tmpAnim:=TTimeBasedAnimationData(pImage.Animations.Items[anim]).Clone(true);
      // Create temporary image for one frame
      tmpFrame:=TARGBImage.Create(pImage.Animations.Items[anim].Width,pImage.Animations.Items[anim].Height);
      try
        // For all frames...
        for frame:=0 to pImage.Animations.Items[anim].FrameCount-1 do begin
          // Copy out current frame to tmpFrame
          with pImage.Animations.Items[anim] do
            pImage.CopyTo(Frames[frame].Left,Frames[frame].Top,tmpAnim.Width,tmpAnim.Height,0,0,tmpFrame);
          // If already have this frame in the atlas
          if SearchForIdenticalFrame(tmpFrame,x,y) then begin
            // Just add frame
            tmpAnim.AddFrame(x,y);
          end else begin
            // Search a place for the frame
            SearchForPlace(tmpFrame.Width,tmpFrame.Height,x,y);
            // Add frame data
            tmpAnim.AddFrame(x,y);
            // Copy frame to atlas
            with pImage.Animations.Items[anim] do
              pImage.CopyTo(Frames[frame].Left,Frames[frame].Top,tmpAnim.Width,tmpAnim.Height,x,y,fTextureAtlas);
          end;
        end;
      finally
        // Free temporary image of one frame
        tmpFrame.Free;
      end;
      // Add tmpAnim to atlas
      fTextureAtlas.Animations.AddObject(tmpAnim.Name,tmpAnim);
    end;
  end;
end;

procedure TTextureAtlasGeneratorBase.LoadImage(pFilename:string);
var image:TARGBImage;
begin
  image:=TARGBImage.Create(pFilename);
  try
    AddImage(image);
  finally
    image.Free;
  end;
end;

procedure TTextureAtlasGeneratorBase.Crop;
var tmp:TARGBImage;
begin
  fTextureAtlas.CropRightBottom(0,0,0,0);
  tmp:=TARGBImage.Create(fTextureAtlas.Width+fPadding,fTextureAtlas.Height+fPadding);
  tmp.Clear(0);
  fTextureAtlas.CopyTo(0,0,fTextureAtlas.Width,fTextureAtlas.Height,0,0,tmp);

  fTextureAtlas.Resize(tmp.Width,tmp.Height);
  tmp.CopyTo(0,0,tmp.Width,tmp.Height,0,0,fTextureAtlas);
  tmp.Free;
end;

function TTextureAtlasGeneratorBase.fGetTextureAtlas:TARGBImage;
begin
  Result:=fTextureAtlas;
end;

function TTextureAtlasGeneratorBase.SearchForIdenticalFrame(fFrame:TARGBImage;
  out x:integer; out y:integer):boolean;
var tmp:TARGBImage;i,j:integer;
begin
  Result:=false;
  x:=-1;
  i:=0;
  while not Result and (i<fTextureAtlas.Animations.Count) do begin
    if (fTextureAtlas.Animations.Items[i].Width=fFrame.Width) and
       (fTextureAtlas.Animations.Items[i].Height=fFrame.Height) then begin
      tmp:=TARGBImage.Create(fFrame.Width,fFrame.Height);
      try
        for j:=0 to fTextureAtlas.Animations.Items[i].FrameCount-1 do
          with fTextureAtlas.Animations.Items[i].Frames[j] do begin
            fTextureAtlas.CopyTo(Left,Top,Width,Height,0,0,tmp);
            if fFrame.IsIdentical(tmp) then begin
              Result:=true;
              x:=Left;
              y:=top;
              break;
            end;
          end;
      finally
        tmp.Free;
      end;
    end;
    inc(i);
  end;
end;

{$endregion}

{ TTextureAtlasGeneratorLines }
{$region /fold}

constructor TTextureAtlasGeneratorLines.Create(iWidth,iHeight,iPadding:integer);
begin
  inherited Create(iWidth,iHeight,iPadding);
  fLines:=TTextureLines.Create;
end;

destructor TTextureAtlasGeneratorLines.Destroy;
begin
  fLines.Free;
  inherited Destroy;
end;

procedure TTextureAtlasGeneratorLines.SearchForPlace(const pWidth,pHeight:integer;
  out x,y:integer);
var
  Line:TTextureLine;
begin
  Line:=fLines.SearchLine(pWidth,pHeight);
  if not Assigned(Line) then begin
    Line:=TTextureLine.Create(fLines.CurrentTop+fPadding,pHeight,fTextureAtlas.Width,fPadding);
    fLines.Add(Line);
  end;
  x:=Line.CurrentLeft;
  y:=Line.Top;
  Line.AddImage(pWidth);
end;

{$endregion}

{ TTextureAtlasGeneratorFree }
{$region /fold}

constructor TTextureAtlasGeneratorFree.Create(iWidth,iHeight,iPadding:integer);
begin
  inherited Create(iWidth,iHeight,iPadding);
  fBitmap:=GetMem(fTextureAtlas.Width*fTextureAtlas.Height);
  fillchar(fBitmap^,fTextureAtlas.Width*fTextureAtlas.Height,0);
end;

destructor TTextureAtlasGeneratorFree.Destroy;
begin
  Freemem(fBitmap);
  inherited Destroy;
end;

procedure TTextureAtlasGeneratorFree.SearchForPlace(const pWidth,pHeight:integer;
  out x,y:integer);
var
  i,j,k,l:integer;
  p:pointer;
  w:boolean;
begin
  for l:=0 to fTextureAtlas.Height-pHeight+fPadding do begin
    p:=fBitmap+l*fTextureAtlas.Width;
    for k:=0 to fTextureAtlas.Width-pWidth-fPadding do begin
      if byte(p^)=0 then begin
        w:=false;
        j:=0;
        while (j<pHeight+fPadding) and not w do begin
          i:=0;
          while (i<pWidth+fPadding) and not w do begin
            w:=byte((p+j*fTextureAtlas.Width+i)^)<>0;
            inc(i);
          end;
          inc(j);
        end;
        if not w then begin
          for j:=0 to pHeight+fPadding-1 do
            for i:=0 to pWidth+fPadding-1 do
              byte((p+j*fTextureAtlas.Width+i)^):=1;
          x:=k;
          y:=l;
          exit;
        end;
      end;
      inc(p);
    end;
  end;
end;

{$endregion}

{ TTextureAtlasGenerator }
{$region /fold}

constructor TTextureAtlasGenerator.Create(iWidth,iHeight,iPadding:integer;
  iFillMethod:TFillMethod);
begin
  if iFillMethod=fmLines then
    fTextureAtlasGenerator:=TTextureAtlasGeneratorLines.Create(iWidth,iHeight,iPadding)
  else
    fTextureAtlasGenerator:=TTextureAtlasGeneratorFree.Create(iWidth,iHeight,iPadding);
end;

destructor TTextureAtlasGenerator.Destroy;
begin
  fTextureAtlasGenerator.Free;
  inherited ;
end;

function TTextureAtlasGenerator.fGetTextureAtlas:TARGBImage;
begin
  Result:=fTextureAtlasGenerator.TextureAtlas;
end;

function TTextureAtlasGenerator.fGetFreeImage:boolean;
begin
  Result:=fTextureAtlasGenerator.FreeImage;
end;

procedure TTextureAtlasGenerator.fSetFreeimage(value:boolean);
begin
  fTextureAtlasGenerator.FreeImage:=value;
end;

procedure TTextureAtlasGenerator.AddImage(pImage: TARGBImage; pAnimationName: string);
begin
  fTextureAtlasGenerator.AddImage(pImage,pAnimationName);
end;

procedure TTextureAtlasGenerator.LoadImage(pFilename:string);
begin
  fTextureAtlasGenerator.LoadImage(pFilename);
end;

procedure TTextureAtlasGenerator.Crop;
begin
  fTextureAtlasGenerator.Crop;
end;

{$endregion}

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

