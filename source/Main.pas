unit Main;

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
  MKStream, Logger, sdl2, MKToolbox, Shared;

{ TMain }

constructor TMain.Create(iVersion,iBuildDate:string);
{$IFNDEF DEBUG}var MAD4:TMAD4MidLevel;{$ENDIF}
begin
{$IFDEF DEBUG}
  // Set logging level
  Log.SetLogLevel(llAll);
  // Set data directory path to allow running without datafile
  MKStreamOpener.AddDirectory('..\data',100);
{$ELSE}
// Set logging level
  Log.SetLogLevel(llStatus);
// Try to mount the datafile.
  if FileExists(ExtractFileDir(Parameters[0])+'\'+DATAFILE) then begin
    try
      MAD4:=TMAD4MidLevel.Create(ExtractFileDir(Parameters[0])+'\'+DATAFILE);
      MKStreamOpener.AddOtherSource(MAD4, 0);
    except
      on exception do ;
    end;
  end else begin
    Log.LogError('Datafile not found!');
    Log.LogStatus(ExtractFileDir(Parameters[0])+'\'+DATAFILE);
    raise Exception.Create('Datafile not found!');
  end;
{$ENDIF}

  MKStreamOpener.AddDirectory('.',0);
  SDL_SetHint(SDL_HINT_MOUSE_FOCUS_CLICKTHROUGH, '1');

  fMainWindow:=TWindow.CreateDoubleSized(
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    WINDOWWIDTH,
    WINDOWHEIGHT,
    Format('Ladybug Chaos V%s (%s)',[iVersion,StringReplace(iBuildDate,'/','.',[rfReplaceAll])]));

  SetFPS(60);
end;

destructor TMain.Destroy;
begin
  if Assigned(fMainWindow) then FreeAndNil(fMainWindow);
  inherited Destroy;
end;

procedure TMain.Run;
begin
  repeat
    Flip;
    HandleMessages;
  until keys[SDL_SCANCODE_ESCAPE];
end;

end.

