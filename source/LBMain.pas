{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBMain;

{$mode Delphi}{$H+}

interface

uses
  SysUtils, mk_sdl2;

type

  { TMain }

  TMain=class
    constructor Create(iVersion,iBuildDate:string);
    destructor Destroy; override;
    procedure Run;
  private
    fMainWindow:TWindow;
  end;

implementation

uses
  MKStream, Logger, sdl2, MKToolbox, LBShared, ARGBImageUnit, LBPlay1Map
  {$ifndef debug},MAD4MidLevelUnit{$endif};

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
{$IFNDEF DEBUG}var MAD4:TMAD4MidLevel;{$ENDIF}
begin
  randomize;
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
{$ELSE}
// Set logging level
  Log.SetLogLevel(llStatus);
// Try to mount the datafile.
  if FileExists(ExtractFileDir(Paramstr(0))+'\'+DATAFILE) then begin
    try
      MAD4:=TMAD4MidLevel.Create(ExtractFileDir(Paramstr(0))+'\'+DATAFILE);
      MKStreamOpener.AddOtherSource(MAD4, 0);
    except
      on exception do ;
    end;
  end else begin
    Log.LogError('Datafile not found!');
    Log.LogStatus(ExtractFileDir(Paramstr(0))+'\'+DATAFILE);
    raise Exception.Create('Datafile not found!');
  end;
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);
  SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, '1');
  SDL_SetHint(SDL_HINT_RENDER_VSYNC,'1');
  SDL_Init(SDL_INIT_VIDEO);

  fMainWindow:=TWindow.CreateDoubleSized(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format(WINDOWCAPTION,[iVersion,iBuildDate]));

  LoadAssets;
end;

destructor TMain.Destroy;
begin
  FreeAssets;
  fMainWindow.Free;
  SDL_Quit;
  inherited Destroy;
end;

procedure TMain.Run;
var Play1Map:TPlay1Map;
begin
  Play1Map:=TPlay1Map.Create('map01.json');
  try
    Play1Map.Run;
  finally
    Play1Map.Free;
  end;
end;

end.

