unit LBFirstRun;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, vcc2_ButtonStatic;

type

  { TPasswordButton }

  TPasswordButton=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight,iKind:integer;iColor:UInt32);
    procedure MouseEnter(Sender:TObject);
    procedure MouseLeave(Sender:TObject);
  protected
    procedure ReDraw; override;
  private
    fKind:integer;  // 0 - Original passwords, 1 - New passwords
    fOver:boolean;
    fColor:UInt32;
    function Brighter(const pColor:UInt32;pStrength:double):UInt32;
    function Darker(const pColor:UInt32;pStrength:double):UInt32;
  end;

  { TFirstRun }

  TFirstRun=class
    constructor Create;
    destructor Destroy; override;
    function Run:integer;
  private
    fLogo:TTexture;
    fButtonClicked:boolean;
    fButtons:array [0..1] of TPasswordButton;
    procedure Click(Sender:TObject;x, y, buttons: integer);
  end;

implementation

uses sdl2, LBShared, MKMouse2, MKToolbox;

{ TPasswordButton }

constructor TPasswordButton.Create(iX,iY,iWidth,iHeight,iKind:integer;
  iColor:UInt32);
begin
  inherited Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=iHeight;
  TextAlignX:=1;
  TextOffsetY:=0;
  fKind:=iKind;
  if fKind and 1=0 then begin
    fCaption:='Original passwords';
    fName:='ORGPWD_BTN';
    Font:=MM.Fonts['White'];
  end else begin
    fCaption:='New passwords';
    fName:='NEWPWD_BTN';
    Font:=MM.Fonts['Yellow'];
  end;
  fNeedRedraw:=true;
  OnMouseEnter:=MouseEnter;
  OnMouseLeave:=MouseLeave;
  fOver:=false;
  fColor:=iColor;
end;

procedure TPasswordButton.MouseEnter(Sender:TObject);
begin
  fOver:=true;
  fNeedRedraw:=true;
end;

procedure TPasswordButton.MouseLeave(Sender:TObject);
begin
  fOver:=false;
  fNeedRedraw:=true;
end;

procedure TPasswordButton.ReDraw;
var c:array[0..4] of UInt32;
begin
  with fImage do begin
    if fOver then begin
      c[0]:=Brighter(fColor,0.4);
      c[1]:=Brighter(fColor,0.3);
      c[2]:=Brighter(fColor,0.2);
      c[3]:=Brighter(fColor,0.1);
      c[4]:=fColor;
    end else begin
      c[0]:=Brighter(fColor,0.2);
      c[1]:=Brighter(fColor,0.1);
      c[2]:=fColor;
      c[3]:=Darker(fColor,0.1);
      c[4]:=Darker(fColor,0.2);
    end;
    Rectangle(0,0,Width  ,Height  ,$ff000000);
    HLine(1,1,Width-2,c[0]);
    VLine(Width-2,1,Height-2,c[0]);
    HLine(2,2,Width-4,c[1]);
    VLine(Width-3,2,Height-4,c[1]);
    bar(3,3,Width-6,Height-6,c[2]);
    HLine(1,Height-2,Width-2,c[3]);
    VLine(1,1,Height-2,c[3]);
    HLine(2,Height-3,Width-4,c[4]);
    VLine(2,2,Height-4,c[4]);
    if Assigned(fFont) then
      fFont.OutText(fImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
  end;
end;

function TPasswordButton.Brighter(const pColor:UInt32; pStrength:double):UInt32;
var i:integer;
begin
  if pStrength<0 then pStrength:=0;
  Result:=pColor and $ff000000;
  i:=trunc(((pColor and $ff0000)>>16)*(pStrength+1));
  if i>255 then i:=255;
  Result:=Result+i<<16;
  i:=trunc(((pColor and $ff00)>>8)*(pStrength+1));
  if i>255 then i:=255;
  Result:=Result+i<<8;
  i:=trunc((pColor and $ff)*(pStrength+1));
  if i>255 then i:=255;
  Result:=Result+i;
end;

function TPasswordButton.Darker(const pColor:UInt32; pStrength:double):UInt32;
var i:integer;
begin
  if pStrength<0 then pStrength:=0;
  Result:=pColor and $ff000000;
  i:=trunc(((pColor and $ff0000)>>16)*(1-pStrength));
  if i>255 then i:=255;
  Result:=Result+i<<16;
  i:=trunc(((pColor and $ff00)>>8)*(1-pStrength));
  if i>255 then i:=255;
  Result:=Result+i<<8;
  i:=trunc((pColor and $ff)*(1-pStrength));
  if i>255 then i:=255;
  Result:=Result+i;
end;

{ TFirstRun }

constructor TFirstRun.Create;
begin
  fLogo:=MM.Textures['Logo'];
  fButtons[0]:=TPasswordButton.Create(60,400,200,40,0,$ff104005);
  MouseObjects.Add(fButtons[0]);
  fButtons[0].OnClick:=Click;
  fButtons[1]:=TPasswordButton.Create(60+320,400,200,40,1,$ff104005);
  MouseObjects.Add(fButtons[1]);
  fButtons[1].OnClick:=Click;
  fButtonClicked:=false;
end;

destructor TFirstRun.Destroy;
begin
  MouseObjects.Remove(fButtons[1]);
  fButtons[1].Free;
  MouseObjects.Remove(fButtons[0]);
  fButtons[0].Free;
  inherited Destroy;
end;

function TFirstRun.Run:integer;
const TEXTTOP=100;
begin
  Result:=0;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,32,8,12,255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(162,8,fLogo);

    MM.Fonts['Red'].OutText('This is the first time you run Ladybugs.',WINDOWWIDTH div 2,TEXTTOP+0,1);
    MM.Fonts['Blue'].OutText('You must choose which password set you want to use!',WINDOWWIDTH div 2,TEXTTOP+40,1);
    MM.Fonts['White'].OutText('Original passwords are the ones were used in both Logical',WINDOWWIDTH div 2,TEXTTOP+80,1);
    MM.Fonts['White'].OutText('and Cat''s Eye Chaos. They can be easily found on internet.',WINDOWWIDTH div 2,TEXTTOP+100,1);
    MM.Fonts['White'].OutText('If you are stuck, you can search the next level''s password.',WINDOWWIDTH div 2,TEXTTOP+120,1);
    MM.Fonts['Yellow'].OutText('New passwords are freshly created for this game and',WINDOWWIDTH div 2,TEXTTOP+160,1);
    MM.Fonts['Yellow'].OutText('not yet can be found on the internet.',WINDOWWIDTH div 2,TEXTTOP+180,1);
    MM.Fonts['Yellow'].OutText('Choose this if you want to fight yourself through every level,',WINDOWWIDTH div 2,TEXTTOP+200,1);
    MM.Fonts['Yellow'].OutText('and don''t want the temptation to skip a few of them.',WINDOWWIDTH div 2,TEXTTOP+220,1);
    MM.Fonts['Blue'].OutText('If you don''t know what to do, choose "New passwords".',WINDOWWIDTH div 2,TEXTTOP+260,1);
    MouseObjects.Draw;
    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Terminate:=true;
    if fButtonClicked then Result:=1;
    if Terminate then Result:=-1;
  until Result<>0;
end;

procedure TFirstRun.Click(Sender: TObject; x, y, buttons: integer);
begin
  VMU.PassFileIndex:=TPasswordButton(Sender).fKind;
  fButtonClicked:=true;
end;

end.

