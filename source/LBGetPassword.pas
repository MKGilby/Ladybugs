unit LBGetPassword;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2, LBButton, ColorUnit;

type

  { TGetPassword }

  TGetPassword=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    fLogo:TTexture;
    fButtonClicked:integer;
    fButton:TLBButton;
    fColors:array[0..2] of TColor;
    procedure Click(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses LBShared, MKMouse2, sdl2;

{ TGetPassword }

constructor TGetPassword.Create;
const
  BTN_COLOR=$FF206040;
  BTNS_TOP=320;
begin
  fLogo:=TStaticTexture.Create('logo.png');
  fButton:=TLBButton.Create(220,BTNS_TOP,200,40,BTN_COLOR,'PWD_BACK_BTN','Back',MM.Fonts['White']);
  MouseObjects.Add(fButton);
  fButton.OnClick:=Click();
  fColors[0].Color32:=$ff183060;
  fColors[1]:=fColors[0].Brighten(0.15);
  fColors[2]:=fColors[0].Darken(0.25);
end;

destructor TGetPassword.Destroy;
begin
  MouseObjects.Remove(fButton);
  fButton.Free;
  fLogo.Free;
  inherited Destroy;
end;

procedure TGetPassword.Run;
const TEXTTOP=100;
var
  i,Res:integer;
  Password,s:string;
  pre,now:UInt64;
begin
  fButtonClicked:=-1;
  ClearKeys;
  Res:=0;
  Password:='';
  pre:=GetTickCount64;
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

    now:=GetTickCount64;
    s:=Password;
    if (fButtonClicked<2) then begin
      if ((now-pre) mod 1024<512) then s:=s+'_';
      MM.Fonts['Red'].OutText('Enter password',WINDOWWIDTH div 2,TEXTTOP+120,1);
    end else
      MM.Fonts['Red'].OutText('Correct password!',WINDOWWIDTH div 2,TEXTTOP+120,1);
    bar(100,TEXTTOP+158,WINDOWWIDTH-200,22,fColors[1].r,fColors[1].g,fColors[1].b,fColors[1].a);
    MM.Fonts['Yellow'].OutText(s,100,TEXTTOP+160,0);

    MM.Fonts.OutText(#2'This version '#0'@2025 MKSZTSZ',WINDOWWIDTH div 2,TEXTTOP+342,1);
    MouseObjects.Draw;
    FlipNoLimit;
    HandleMessages;
    if fButtonClicked<2 then begin
      for i:=SDL_SCANCODE_A to SDL_SCANCODE_Z do
        if keys[i] then begin
          keys[i]:=false;
          if length(Password)=0 then
            Password:=Password+chr(65+i-SDL_SCANCODE_A)
          else
            Password:=Password+chr(97+i-SDL_SCANCODE_A);
        end;
      if keys[SDL_SCANCODE_SPACE] then begin
        keys[SDL_SCANCODE_SPACE]:=false;
        Password:=Password+' ';
      end;
      if keys[SDL_SCANCODE_BACKSPACE] then begin
        keys[SDL_SCANCODE_BACKSPACE]:=false;
        if length(Password)>0 then delete(Password,length(Password),1);
      end;
      if Passwords.IndexOf(uppercase(Password))>-1 then begin
        CurrentLevel:=Passwords.IndexOf(uppercase(Password))+1;
        fButton.Caption:='OK';
        fButtonClicked:=2;
      end;
    end;
    if fButtonClicked=3 then Res:=1;
    if keys[SDL_SCANCODE_ESCAPE] then Res:=1;
    if Terminate then Res:=-1;
  until Res<>0;
end;

procedure TGetPassword.Click(Sender:TObject; x,y,buttons:integer);
begin
  fButtonClicked:=3;
end;

end.

