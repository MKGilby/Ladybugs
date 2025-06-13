{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

program BuildGFX;

{$mode delphi}

uses
  BuildGFX_Bugs,
  BuildGFX_Mushroom,
  MKStream, BuildGFX_Painter, BuildGFX_Shared;

type

  { TMain }

  TMain=class
    constructor Create;
    procedure Run;
  end;

{ TMain }

constructor TMain.Create;
begin
  {$ifdef DEBUG}
  MKStreamOpener.AddDirectory('..\work\gfx\data',0);
  {$else}
  MKStreamOpener.AddDirectory('.\data',0);
  {$endif}
end;

procedure TMain.Run;
begin
  with TBugBuilder.Create do try Run; finally Free; end;
  with TMushroomBuilder.Create do try Run; finally Free; end;
  with TPainterBuilder.Create do try Run; finally Free; end;
end;

begin
  with TMain.Create do try
    Run;
  finally
    Free;
  end;
end.

