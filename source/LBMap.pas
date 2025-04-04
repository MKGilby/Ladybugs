{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBMap;

{$mode Delphi}

interface

uses
  Classes, SysUtils, fpjson, TileMapUnit;

type

  { TMap }

  TMap=class(TTileMap)
    constructor Create;
    destructor Destroy; override;
    procedure ResetMap;
    procedure LoadFromFile(iFilename:string);
    procedure ShowValues;
  private
    procedure CreateEntity(pX,pY:integer;pTileDef:string;pJ:TJSONData);
  end;

implementation

uses MKStream, LBShared, LBMapEntities, Logger;

{ TMap }

constructor TMap.Create;
begin
  inherited Create(MAPWIDTH,MAPHEIGHT);
  ResetMap;
end;

destructor TMap.Destroy;
begin
  inherited Destroy;
end;

procedure TMap.ResetMap;
var i,j:integer;
begin
  for i:=0 to MAPWIDTH-1 do
    for j:=0 to MAPHEIGHT-1 do
      Tiles[i,j]:=15;
end;

procedure TMap.LoadFromFile(iFilename:string);
var J:TJSONData;Xs:TStream;JA:TJSONArray;x,y:integer;s:string;

  function min(i1,i2:integer):integer; inline;
  begin
    if i1>i2 then Result:=i2 else Result:=i1;
  end;

begin
  ResetMap;
  Xs:=MKStreamOpener.OpenStream(iFilename);
  try
    J:=GetJSON(Xs);
  finally
    Xs.Free;
  end;
  try
    if Assigned(J.FindPath('Tiles')) then begin
      JA:=TJSONArray(J.FindPath('Tiles'));
      for y:=0 to min(JA.Count-1,MAPHEIGHT-1) do begin
        s:=JA[y].AsString;
        for x:=1 to min(length(s),MAPWIDTH) do begin
//          Log.LogStatus(Format('x=%d, y=%d, value=%d',[x-1,y,ord(s[x])]));
          CreateEntity(x-1,y,inttostr(ord(s[x])),J)
        end;
      end;
    end;
  finally
    J.Free;
  end;
end;

procedure TMap.ShowValues;
var i,j:integer;
begin
  for j:=0 to MAPHEIGHT-1 do
    for i:=0 to MAPWIDTH-1 do
      MM.Fonts['Small'].OutText(inttostr(Tiles[i,j]),i*16+8,(j+1)*16+3,1);
end;

procedure TMap.CreateEntity(pX, pY: integer; pTileDef: string; pJ: TJSONData);
var EntityType:string;JD:TJSONData;
begin
  JD:=pJ.FindPath('TileDefinitions.'+pTileDef);
  if Assigned(JD) then begin
    EntityType:=JD.FindPath('Type').AsString;
    if UpperCase(EntityType)='SIMPLEPATH' then
      Entities.Add(TSimplePath.Create(Self,pX,pY,JD))
    else if UpperCase(EntityType)='MUSHROOM' then
      Entities.Add(TMushroom.Create(Self,pX,pY,JD));
  end else
    raise Exception.Create(Format('Tile definition not found! (%s)',[pTileDef]));
end;

end.

