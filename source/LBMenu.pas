unit LBMenu;

{$mode Delphi}

interface

uses
  SysUtils, mk_sdl2;

type

  { TMenu }

  TMenu=class
    constructor Create;
    destructor Destroy; override;
    function Run:integer;
  private
    fLogo:TTexture;
  end;

implementation

uses sdl2, LBShared;

{ TMenu }

constructor TMenu.Create;
begin
  fLogo:=TStaticTexture.Create('logo.png');
end;

destructor TMenu.Destroy;
begin
  fLogo.Free;
  inherited Destroy;
end;

function TMenu.Run:integer;
const TEXTTOP=320;
begin
  Result:=0;
  repeat
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,12,32,8,255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(162,8,fLogo);
{    if GetTickCount64 mod 1000<500 then
      if Assigned(Controller) then
        MM.Fonts['White'].OutText('PRESS '#128' TO START',LOGICALWINDOWWIDTH div 2,64,1)
      else
        MM.Fonts['White'].OutText('PRESS SPACE TO START',LOGICALWINDOWWIDTH div 2,64,1);
    MM.Fonts['Blue'].OutText('FRANK N. STEIN RE-BOOTED',LOGICALWINDOWWIDTH div 2,TEXTTOP,1);
    MM.Fonts['Blue'].OutText('@1984-2011 COLIN STEWART',LOGICALWINDOWWIDTH div 2,TEXTTOP+10,1);
    MM.Fonts['Pink'].OutText('THIS REFURBICATION @2023 MKSZTSZ',LOGICALWINDOWWIDTH div 2,TEXTTOP+22,1);
    MM.Fonts['Yellow'].OutText('MUSIC AND SOUND - MIKE FRALEY',LOGICALWINDOWWIDTH div 2,TEXTTOP+34,1);
    MM.Fonts['Yellow'].OutText('GFX AND CODE - GILBY',LOGICALWINDOWWIDTH div 2,TEXTTOP+44,1);
    MM.Fonts['Purple'].OutText('DEVELOPED USING SDL2, BASS',LOGICALWINDOWWIDTH div 2,TEXTTOP+56,1);
    MM.Fonts['Purple'].OutText('AND LAZARUS 3.2 (FPC 3.2.2)',LOGICALWINDOWWIDTH div 2,TEXTTOP+66,1);}

    MM.Fonts['Red'].OutText('Test red text...',WINDOWWIDTH div 2,TEXTTOP+60,1);
    MM.Fonts['White'].OutText('This version @ 2025 MKSZTSZ',WINDOWWIDTH div 2,TEXTTOP+80,1);
    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=-1;
    if keys[SDL_SCANCODE_RETURN] or keys[SDL_SCANCODE_SPACE] then Result:=1;
{    if controllerbuttons[SDL_CONTROLLER_BUTTON_A] then Result:=1;
    if controllerbuttons[SDL_CONTROLLER_BUTTON_B] then Result:=-1;}
    if Terminate then Result:=-1;
  until Result<>0;
end;

end.

