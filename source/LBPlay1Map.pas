{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBPlay1Map;

{$mode Delphi}

interface

uses
  SysUtils, LBMap, mk_sdl2;

type

  { TPlay1Map }

  TPlay1Map=class
    constructor Create(iMapFilename:string);
    destructor Destroy; override;
    function Run:integer;
  private
    fBack:TTexture;
    fMap:TMap;
    procedure CreateBack;
  end;

implementation

uses LBShared, LBMapEntities, sdl2, ARGBImageUnit, LBBugs;

{ TPlay1Map }

constructor TPlay1Map.Create(iMapFilename:string);
begin
  Entities:=TMapEntities.Create;
  Bugs:=TBugs.Create;
  fMap:=TMap.Create;
  fMap.LoadFromFile(iMapFilename);
  CreateBack;
end;

destructor TPlay1Map.Destroy;
begin
  fBack.Free;
  fMap.Free;
  Bugs.Free;
  Entities.Free;
  inherited Destroy;
end;

function TPlay1Map.Run:integer;
var pre,now:QWord;
begin
  ShouldCreateNewBug:=true;
  pre:=GetTickCount64;
  repeat
    if ShouldCreateNewBug then Bugs.CreateNewBug(fMap);
    now:=GetTickCount64;
    Entities.Move((now-pre)/1000);
    Bugs.Move((now-pre)/1000);
    pre:=now;
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,64,16,24,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    PutTexture(0,0,fBack);
    Entities.Draw;
    Bugs.Draw;
    if keys[SDL_SCANCODE_TAB] then fMap.ShowValues;
    MM.Fonts['Small'].OutText('FPS:'+inttostr(FPS),0,0,0);
    FlipNoLimit;
    HandleMessages;
  until keys[SDL_SCANCODE_ESCAPE] or Terminate;
  Result:=-1;
end;

procedure TPlay1Map.CreateBack;
var tmp:TARGBImage;i:integer;
begin
  tmp:=TARGBImage.Create(WINDOWWIDTH,WINDOWHEIGHT);
  try
    tmp.FillImage(MM.Images.ItemByName['Grass']);
    for i:=0 to MAPWIDTH-1 do begin
      fMap.Tiles[i,0]:=16;
      tmp.PutImagePart(i*16,16,16,0,16,16,MM.Images.ItemByName['Paths'],true);
    end;
    for i:=0 to Entities.Count-1 do
      Entities[i].DrawBack(tmp);
    fBack:=TStaticTexture.Create(tmp);
  finally
    tmp.Free;
  end;
end;

end.

