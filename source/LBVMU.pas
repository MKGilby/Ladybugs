unit LBVMU;

{$mode Delphi}

interface

uses
  Classes, SysUtils;

type

  { TVMU }

  TVMU=class
    constructor Create(iVMUName:string);
    destructor Destroy; override;
    procedure LoadFromFile(pFilename:string);
    procedure SaveToFile(pFilename:string);
    procedure LoadFromStream(pSource:TStream);
    procedure SaveToStream(pTarget:TStream);
  private
    fFilename:string;
    fPassFileIndex:integer;
    procedure fSetPassFileIndex(pValue:integer);
    procedure LoadFromStreamV1(pSource:TStream);
  public
    FirstRun:boolean;
    property PassFileIndex:integer read fPassFileIndex write fSetPassFileIndex;
  end;

implementation

const
  FLAG_FIRSTRUN=1;

{ TVMU }

constructor TVMU.Create(iVMUName:string);
begin
  FirstRun:=true;
  fPassFileIndex:=0;
  fFilename:=iVMUName;
  if FileExists(fFilename) then LoadFromFile(fFilename);
end;

destructor TVMU.Destroy;
begin
  SaveToFile(fFilename);
  inherited Destroy;
end;

procedure TVMU.LoadFromFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmOpenRead);
  try
    LoadFromStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TVMU.SaveToFile(pFilename:string);
var Xs:TStream;
begin
  Xs:=TFileStream.Create(pFilename,fmCreate);
  try
    SaveToStream(Xs);
  finally
    Xs.Free;
  end;
end;

procedure TVMU.LoadFromStream(pSource:TStream);
var i:integer;
begin
  if pSource.Size=0 then exit;
  i:=0;
  pSource.Read(i,1);
  case i of
    1:LoadFromStreamV1(pSource);
  end;
end;

procedure TVMU.SaveToStream(pTarget:TStream);
var i:integer;
begin
  i:=1;pTarget.Write(i,1);  // Version 1
  i:=0;
  if FirstRun then i:=i or FLAG_FIRSTRUN;
  pTarget.Write(i,1);
  pTarget.Write(fPassFileIndex,1);
end;

procedure TVMU.fSetPassFileIndex(pValue:integer);
begin
  if (pValue in [0,1]) and (pValue<>fPassFileIndex) then
    fPassFileIndex:=pValue;
end;

procedure TVMU.LoadFromStreamV1(pSource:TStream);
var i:integer;
begin
  i:=0;
  pSource.Read(i,1);
  FirstRun:=(i and FLAG_FIRSTRUN)<>0;
  pSource.Read(i,1);
  fPassFileIndex:=i;
end;

end.

{

  VMU file format

    Start  Size   Content
      0      1    version (current version is 1, all other values should cause error)
      1      ?    data


  VMU V1 file format

    Start  Size   Content
      1      1    Flags
                    bit 0 - FirstRun
      2      1    PassFileIndex (0 or 1)

}
