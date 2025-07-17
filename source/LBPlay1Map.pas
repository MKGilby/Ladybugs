{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBPlay1Map;

{$mode Delphi}

interface

uses
  SysUtils, LBMap, mk_sdl2, LBBugTimer, MKMouse2;

type

  { TClickWaiter }

  TClickWaiter=class(TMouseObject)
    constructor Create;
    procedure Draw; override;
  end;

  { TPlay1Map }

  TPlay1Map=class
    constructor Create(iMapFilename:string);
    destructor Destroy; override;
    function Run:integer;
  private
    fBack:TTexture;
    fMap:TMap;
    fState:(sIntro,sPlaying,sCompleted1,sCompleted2,sPaused,sExitLevel);
    fClickWaiter:TClickWaiter;
    procedure CreateBack;
    procedure ClickWaiterClick(Sender:TObject;x,y,buttons:integer);
  end;

implementation

uses LBShared, LBMapEntities, sdl2, ARGBImageUnit, LBBugs;

{ TClickWaiter }

constructor TClickWaiter.Create;
begin
  inherited Create;
  SetBoundsWH(0,0,WINDOWWIDTH,WINDOWHEIGHT);
  Visible:=false;
end;

procedure TClickWaiter.Draw;
begin
  // Nothing to draw
end;

{ TPlay1Map }

constructor TPlay1Map.Create(iMapFilename:string);
begin
  MushroomNeeded:=0;
  Entities:=TMapEntities.Create;
  TrafficLight:=nil;
  PatternLock:=nil;
  Bugs:=TBugs.Create;
  fMap:=TMap.Create;
  fMap.LoadFromFile(iMapFilename);
  NextBugColor:=TBugs.GetRandomBugColor;
  CreateBack;
  BugTimer:=TBugTimer.Create(fMap.BugTimeMultiplier);
  fState:=sIntro;
  fClickWaiter:=TClickWaiter.Create;
  fClickWaiter.Visible:=true;
  fClickWaiter.OnClick:=ClickWaiterClick();
  MouseObjects.Add(fClickWaiter);
end;

destructor TPlay1Map.Destroy;
begin
  MouseObjects.Remove(fClickWaiter);
  fClickWaiter.Free;
  BugTimer.Free;
  fBack.Free;
  fMap.Free;
  Bugs.Free;
  Entities.Free;
  inherited Destroy;
end;

function TPlay1Map.Run:integer;
var pre,now:QWord;i:integer;
  leveltxt,passwordtxt:string;
begin
  ShouldCreateNewBug:=true;
  pre:=GetTickCount64;
  leveltxt:=Format(#0'Level: '#2'%d',[CurrentLevel]);
  passwordtxt:=Format(#0'Password: '#2'%s',[Passwords[CurrentLevel]]);
  Result:=RES_NONE;
  repeat
    now:=GetTickCount64;
    if fState=sPlaying then begin
      if ShouldCreateNewBug then Bugs.CreateNewBug(fMap);
      Entities.Move((now-pre)/1000);
      Bugs.Move((now-pre)/1000);
      BugTimer.Move((now-pre)/1000);
      for i:=0 to Entities.Count-1 do
        if Entities[i] is TMushroom then
          TMushroom(Entities[i]).CheckCompleteness;
      if MushroomNeeded=0 then begin
        for i:=0 to Entities.Count-1 do
          if Entities[i] is TMushroom then
            TMushroom(Entities[i]).ReleaseBugs;
        for i:=0 to Bugs.Count-1 do
          if Bugs[i].State<>bsFlying then
            Bugs[i].StartFly(Bugs[i].X,Bugs[i].Y);
        fState:=sCompleted1;
      end;
    end else
    if fState=sCompleted1 then begin
      Bugs.Move((now-pre)/1000);
      if Bugs.Count=0 then begin
        fState:=sCompleted2;
        fClickWaiter.Visible:=true;
      end;
    end;
    pre:=now;
    SDL_SetRenderDrawColor(PrimaryWindow.Renderer,64,16,24,255);
    SDL_RenderClear(PrimaryWindow.Renderer);
    PutTexture(0,0,fBack);
    BugTimer.Draw;
    Entities.DrawBackground;
    Bugs.Draw;
    Entities.DrawForeground;
    if fState=sIntro then begin
      Bar(0,WINDOWHEIGHT div 2-14,WINDOWWIDTH,26,0,0,0,80);
      MM.Fonts['White'].OutText('Click anywhere to start!',WINDOWWIDTH div 2,WINDOWHEIGHT div 2-10,1);
    end else
    if fState=sCompleted1 then begin
      Bar(0,WINDOWHEIGHT div 2-14,WINDOWWIDTH,26,0,0,0,80);
      MM.Fonts['White'].OutText('Level completed!',WINDOWWIDTH div 2,WINDOWHEIGHT div 2-10,1);
    end else
    if fState=sCompleted2 then begin
      Bar(0,WINDOWHEIGHT div 2-14,WINDOWWIDTH,26,0,0,0,80);
      MM.Fonts['White'].OutText('Level completed! Click anywhere to continue!',WINDOWWIDTH div 2,WINDOWHEIGHT div 2-10,1);
    end;
    if keys[SDL_SCANCODE_TAB] then fMap.ShowValues;
    MM.Fonts['Small'].OutText('FPS:'+inttostr(FPS),0,0,0);
    MM.Fonts.OutText(leveltxt,10,WINDOWHEIGHT-26,0);
    MM.Fonts.OutText(passwordtxt,WINDOWWIDTH-10,WINDOWHEIGHT-26,2);
    FlipNoLimit;
    HandleMessages;
    if keys[SDL_SCANCODE_P] then begin
      if fState=sPlaying then fState:=sPaused
      else if fstate=sPaused then fState:=sPlaying;
      keys[SDL_SCANCODE_P]:=false;
    end;
    if fState=sExitLevel then Result:=RES_PLAYLEVEL;
    if keys[SDL_SCANCODE_ESCAPE] then Result:=RES_ESCAPED;
    if Terminate then Result:=RES_TERMINATE;
  until Result<>0;
  // Got out the game loop by completing the level, so increase current level
  if Result=RES_PLAYLEVEL then begin
    inc(CurrentLevel);
    if CurrentLevel=100 then CurrentLevel:=99;
  end;
end;

procedure TPlay1Map.CreateBack;
var tmp:TARGBImage;i,j:integer;
begin
  tmp:=TARGBImage.Create(WINDOWWIDTH,WINDOWHEIGHT);
  try
    tmp.FillImagePart(0,32,WINDOWWIDTH,WINDOWHEIGHT-32,MM.Images.ItemByName['Grass']);
    for i:=0 to BIGTILEMAPWIDTH-1 do
      for j:=0 to BIGTILEMAPHEIGHT-1 do
        tmp.PutImage(i*80,j*80+32,MM.Images.ItemByName['Grass']);
    for i:=0 to BIGTILEMAPWIDTH-1 do
      tmp.PutImagePart(i*80,0,0,80-32,80,32,MM.Images.ItemByName['Grass']);
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

procedure TPlay1Map.ClickWaiterClick(Sender:TObject; x,y,buttons:integer);
begin
  if fState=sIntro then begin
    TClickWaiter(Sender).Visible:=false;
    fState:=sPlaying;
  end else
  if fState=sCompleted2 then begin
    TClickWaiter(Sender).Visible:=false;
    fState:=sExitLevel;
  end;
end;

end.

