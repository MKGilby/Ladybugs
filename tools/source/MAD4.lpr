{
  V4.05 - 2025.03.14
    - Code modernized.
    - Removed repository.
}

{$mode delphi}

uses Classes, SysUtils, MAD4MidLevelUnit, MKToolBox, Lists, Logger, MD5,
     ParametersUnit, FileInfo, winpeimagereader;

const
  BDate:string={$I %DATE%};

type

  { TMain }

  TMain=class
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    MAD4LL:TMAD4MidLevel;
    List:TFileSearchList;
    AddFiles:TStringList;
    switchr,switchn:boolean;
    FullSize:longint;
    FullDb:integer;
    procedure HeadAndParms;
  end;

function GetVersionString:string;
var
  PV:TProgramVersion;
begin
  GetProgramVersion(PV);
  if PV.Revision=0 then begin
    if PV.Build=0 then
      Result:=Format('%d.%d',[PV.Major,PV.Minor])
    else
      Result:=Format('%d.%d.%d',[PV.Major,PV.Minor,PV.Build])
  end else begin
    if PV.Build=0 then
      Result:=Format('%d.%d.%d',[PV.Major,PV.Minor,PV.Revision])
    else
      Result:=Format('%d.%d.%d.%d',[PV.Major,PV.Minor,PV.Revision,PV.Build]);
  end;
end;

var
  dir,
  filename,
  files:string;
  cl:integer;
  i:integer;
  s:string;

{$R *.res}

{ TMain }

constructor TMain.Create;
begin
  FullSize:=0;
  FullDb:=0;
  List:=TFileSearchList.Create;
  AddFiles:=TStringList.Create;
end;

destructor TMain.Destroy;
begin
  List.Free;
  AddFiles.Free;
  inherited Destroy;
end;

procedure TMain.Run;
begin
  HeadAndParms;
  MAD4LL:=TMAD4MidLevel.Create(filename,false);
  MAD4LL.CompressionLevel:=cl;

  for cl:=0 to AddFiles.Count-1 do
    if switchr then
      List.SearchAndFillRecursive(dir,AddFiles.Strings[cl],faAnyFile)
    else
      List.SearchAndFill(dir+'/'+AddFiles.Strings[cl],faAnyFile-faDirectory);

  if List.Count=0 then begin
    writeln('No files to add!');
    halt;
  end;
  for i:=0 to List.Count-1 do begin
    write('Adding '+List.Strings[i]+'...');
    if switchr then begin
      s:=List.Strings[i];
      delete(s,1,length(dir)+1);
      cl:=MAD4LL.Add(s,List.Strings[i]);
      FullSize:=FullSize+SizeOfFile(List.Strings[i]);
    end else begin
      cl:=MAD4LL.Add(List.Strings[i],dir+'\'+List.Strings[i]);
      FullSize:=FullSize+SizeOfFile(dir+'\'+List.Strings[i]);
    end;
    inc(FullDb);
    writeln(MAD4LL.LastCompression+'...',cl/100:6:2,'%');
  end;

  MAD4LL.Free;
  i:=SizeOfFile(filename);
  writeln(#13#10'Summary:');
  writeln('   Files processed:   ',FullDB:8);
  writeln('   Original size:     ',FullSize:8,' bytes');
  writeln('   MAD4 size:         ',i:8,' bytes');
  writeln('   Compression ratio: ',(i/FullSize*100):6:2,'%');
end;

procedure TMain.HeadAndParms;
var s:String;i,j:integer;
begin
  BDate:=StringReplace(BDate,'/','.',[rfReplaceAll]);
  writeln(Format('Advanced Datafile Creator V%s - (C) 2012-%s MKSZTSZ',[GetVersionString,copy(BDate,1,4)]));
  writeln(Format('Build date: %s',[BDate]));
  j:=0;
  cl:=1;
  dir:='*';
  filename:='*';
  files:='*';
  switchr:=Parameters.IndexOfSwitch('r')>-1;
  if switchr then Parameters.Delete(Parameters.IndexOfSwitch('r'));
  switchn:=Parameters.IndexOfSwitch('n')>-1;
  if switchn then Parameters.Delete(Parameters.IndexOfSwitch('n'));
  for i:=1 to Parameters.Count-1 do begin
    s:=parameters[i];
    if s[1] in ['/','-'] then begin
      delete(s,1,1);
      if s[1] in ['0'..'4'] then cl:=ord(s[1])-48
      else if s='nolog' then
      else begin
        writeln('Unknown switch: '+paramstr(i));
        halt;
      end;
    end else begin
      if j=2 then AddFiles.Add(s);
      if j=1 then begin
        dir:=s;
        inc(j);
      end;
      if j=0 then begin
        filename:=s;
        inc(j);
      end;
    end;
  end;
  if (filename='*') or (dir='*') then begin
    writeln('Syntax:'#13#10);
    writeln('  MAD4 MADFilename sourcedir [filename] [-0|1|2|3|4] [-r]'#13#10);
    writeln('MADFilename is file name to create');
    writeln('sourcedir   is the directory contains the files to add');
    writeln('filename    is the filename (with wildcards) to add (*.* default)');
    writeln('-0|1|2|3|4  sets compression level');
    writeln('-r          scan sourcedir recursively (adds path to filename!)'#13#10);
    writeln('-n          don''t use repository to determine best compression'#13#10);
    writeln('Example:'#13#10'MAD3 data.mad data *.* -4 -r');
    Halt;
  end;
  if AddFiles.Count=0 then AddFiles.Add('*');
  writeln('* Compression level: '+inttostr(cl));
end;


begin
  with TMain.Create do
    try
      try
        Run;
      except
        on e:Exception do
          writeln('*** ',e.Message);
      end;
    finally
      Free;
    end;
end.

