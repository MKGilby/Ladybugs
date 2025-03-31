{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

program LadyBugs;

uses
  // For Format tool
  SysUtils,
  // For ProgramVersion
  FileInfo,
  winpeimagereader,
  // For parsing json files
  jsonparser,
  // For reading PNG files in TARGBImage
  ARGBImagePNGReaderUnit,
  ARGBImageMKRReaderUnit,
  // The game itself.
  LBMain;

const
  BDATE={$i %DATE%};

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then
    Result:=Format('%d.%d build %d',[PV.Major,PV.Minor,PV.Build])
  else
    Result:=Format('%d.%d.%d build %d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
end;

{$R *.res}

begin
  with TMain.Create(GetVersionString,StringReplace(BDATE,'/','.',[rfReplaceAll])) do
    try
      Run;
    finally
      Free;
    end;
end.

