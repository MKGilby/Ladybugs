unit LBMenu;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, LBButton, ColorUnit;

type

  { TMenu }

  TMenu=class
    constructor Create;
    destructor Destroy; override;
    function Run:integer;
  private
    fLogo:TTexture;
    fButtonClicked:integer;
    fButtons:array [0..2] of TLBButton;
    fColors:array[0..2] of TColor;
    procedure Click(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses sdl2, LBShared, MKMouse2;

const
  BTN_COLOR=$FF206040;
  BTN_WIDTH=200;
  BTN_HEIGHT=40;
  BTNS_TOP=220;

{ TMenu }

constructor TMenu.Create;
begin
  fLogo:=TStaticTexture.Create('logo.png');
  fButtons[0]:=TLBButton.Create((WINDOWWIDTH-BTN_WIDTH) div 2, BTNS_TOP,
    BTN_WIDTH, BTN_HEIGHT, BTN_COLOR, 'PLAY_BTN', 'Play', MM.Fonts['Yellow']);
  fButtons[0].Tag:=0;
  fButtons[0].OnClick:=Click();
  MouseObjects.Add(fButtons[0]);
  fButtons[1]:=TLBButton.Create((WINDOWWIDTH-BTN_WIDTH) div 2, BTNS_TOP+60,
    BTN_WIDTH, BTN_HEIGHT, BTN_COLOR, 'PWD_BTN', 'Enter password', MM.Fonts['White']);
  fButtons[1].Tag:=1;
  fButtons[1].OnClick:=Click();
  MouseObjects.Add(fButtons[1]);
  fButtons[2]:=TLBButton.Create((WINDOWWIDTH-BTN_WIDTH) div 2, BTNS_TOP+120,
    BTN_WIDTH, BTN_HEIGHT, BTN_COLOR, 'EXIT_BTN', 'Exit', MM.Fonts['Red']);
  fButtons[2].Tag:=2;
  fButtons[2].OnClick:=Click();
  MouseObjects.Add(fButtons[2]);
  fColors[0].Color32:=$ff183060;
  fColors[1]:=fColors[0].Brighten(0.15);
  fColors[2]:=fColors[0].Darken(0.25);
end;

destructor TMenu.Destroy;
var i:integer;
begin
  for i:=0 to 2 do begin
    MouseObjects.Remove(fButtons[i]);
    fButtons[i].Free;
  end;
  fLogo.Free;
  inherited Destroy;
end;

function TMenu.Run:integer;
const TEXTTOP=100;
begin
  Result:=0;
  fButtonClicked:=-1;
  ClearKeys;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,fColors[2].r,fColors[2].g,fColors[2].b,fColors[2].a);
    SDL_RenderClear(PrimaryWindow.Renderer);
    bar(0,TEXTTOP+55,WINDOWWIDTH,420-TEXTTOP-55,fColors[0].r,fColors[0].g,fColors[0].b,fColors[0].a);
    bar(0,TEXTTOP+54,WINDOWWIDTH,2,fColors[1].r,fColors[1].g,fColors[1].b,fColors[1].a);
    bar(0,419,WINDOWWIDTH,2,fColors[1].r,fColors[1].g,fColors[1].b,fColors[1].a);

    PutTexture(162,20,fLogo);
    MM.Fonts.OutText(#2'Remake of '#0'Logical @1991 Rainbow Arts',WINDOWWIDTH div 2,TEXTTOP,1);
    MM.Fonts.OutText(#2'and '#0'Cat''s Eye Chaos @2003 JP Hamilton',WINDOWWIDTH div 2,TEXTTOP+20,1);

    MM.Fonts.OutText(Format(#0'Current level: '#3'%.2d',[CurrentLevel]),WINDOWWIDTH div 2,TEXTTOP+80,1);

    MM.Fonts.OutText(#2'This version '#0'@2025 MKSZTSZ',WINDOWWIDTH div 2,TEXTTOP+342,1);
    MouseObjects.Draw;
    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Terminate:=True;
    case fButtonClicked of
      0:Result:=1;
      1:Result:=2;
      2:Terminate:=true;
    end;
    if Terminate then Result:=-1;
  until Result<>0;
end;

procedure TMenu.Click(Sender: TObject; x, y, buttons: integer);
begin
  fButtonClicked:=TLBButton(Sender).Tag;
end;

end.

