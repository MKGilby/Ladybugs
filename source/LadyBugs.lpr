program LadyBugs;

uses
  Main, Shared;

const
  VERSION='0.9';
  BDATE={$i %DATE%};

begin
  with TMain.Create(VERSION,BDATE) do
    try
      Run;
    finally
      Free;
    end;
end.

