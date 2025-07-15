unit LBFirstRun;

{$mode Delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  SysUtils, mk_sdl2, vcc2_ButtonStatic, LBButton;

type

  { TFirstRun }

  TFirstRun=class
    constructor Create;
    destructor Destroy; override;
    function Run:integer;
  private
    fLogo:TTexture;
    fButtonClicked:boolean;
    fButtons:array [0..1] of TLBButton;
    procedure Click(Sender:TObject;x, y, buttons: integer);
  end;

implementation

uses sdl2, LBShared, MKMouse2, MKToolbox;

const
  BTN_COLOR=$FF206040;
  BTN_WIDTH=200;
  BTN_HEIGHT=40;
  BTNS_TOP=400;

{ TFirstRun }

constructor TFirstRun.Create;
begin
  fLogo:=TStaticTexture.Create('logo.png');
  fButtons[0]:=TLBButton.Create(WINDOWWIDTH div 4-BTN_WIDTH div 2, BTNS_TOP,
    BTN_WIDTH, BTN_HEIGHT, BTN_COLOR, 'ORGPWD_BTN', 'Original passwords', MM.Fonts['White']);
  fButtons[0].Tag:=0;
  fButtons[0].OnClick:=Click;
  MouseObjects.Add(fButtons[0]);
  fButtons[1]:=TLBButton.Create(WINDOWWIDTH div 4*3-BTN_WIDTH div 2, BTNS_TOP,
    BTN_WIDTH, BTN_HEIGHT, BTN_COLOR, 'NEWPWD_BTN', 'New passwords', MM.Fonts['Yellow']);
  fButtons[1].Tag:=1;
  fButtons[1].OnClick:=Click;
  MouseObjects.Add(fButtons[1]);
  fButtonClicked:=false;
end;

destructor TFirstRun.Destroy;
begin
  MouseObjects.Remove(fButtons[1]);
  fButtons[1].Free;
  MouseObjects.Remove(fButtons[0]);
  fButtons[0].Free;
  fLogo.Free;
  inherited Destroy;
end;

function TFirstRun.Run:integer;
const TEXTTOP=100;
begin
  Result:=0;
  repeat
//    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,32,8,12,255);
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,72,18,36,255);
    SDL_RenderClear(PrimaryWindow.Renderer);

    PutTexture(162,20,fLogo);

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
  VMU.PassFileIndex:=TLBButton(Sender).Tag;
  fButtonClicked:=true;
end;

end.

