{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBBugTimer;

{$mode Delphi}

interface

uses
  SysUtils, Animation2Unit;

type

  { TBugTimer }

  TBugTimer=class
    // How many times will the "bar" move slower than the bugs
    constructor Create(pMultiplier:integer);
    destructor Destroy; override;
    procedure Draw;
    procedure Move(pElapsedTime:double);
    procedure Reset;
  private
    fAnimation:TAnimation;
    fMaxTime,fTime:double;
  end;

implementation

uses LBShared;

{ TBugTimer }

constructor TBugTimer.Create(pMultiplier:integer);
begin
  fMaxTime:=WINDOWWIDTH/BUGWALKINGSPEED*pMultiplier;
  fTime:=fMaxTime;
  fAnimation:=MM.Animations['GrayPath'].SpawnAnimation;
end;

destructor TBugTimer.Destroy;
begin
  fAnimation.Free;
  inherited Destroy;
end;

procedure TBugTimer.Draw;
var Left,i:integer;
begin
  Left:=trunc(fTime/fMaxTime*(WINDOWWIDTH-1));
  i:=WINDOWWIDTH;
  while Left<=i-16 do begin
    dec(i,16);
    fAnimation.PutFrame(i,REALMAPTOP-16);
  end;
  if Left<i then fAnimation.PutFramePart(Left,REALMAPTOP-16,Left mod 16,0,i-Left,16);
end;

procedure TBugTimer.Move(pElapsedTime:double);
begin
  fTime:=fTime-pElapsedTime;
  if fTime<0 then fTime:=0;
end;

procedure TBugTimer.Reset;
begin
  fTime:=fMaxTime;
end;

end.

