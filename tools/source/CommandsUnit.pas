{
  This file is part of the source code of MKConv2.
  See "copyright.txt" for details.
}

unit CommandsUnit;

{$mode delphi}

interface

uses
  Classes, fgl;

type
  TCommandProc=procedure(Parms:TStringList;Help:boolean);

  TCommandItem=class
    _string:string;
    _proc:TCommandProc;
  end;

  TCommandList=TFPGObjectList<TCommandItem>;

  TCommandValidator=class
    constructor Create(iBaseHelp:TCommandProc);
    destructor Destroy; override;
    procedure AddCommand(pCommandString:string;pProc:TCommandProc);
    procedure ValidateAndCallCommand(pParms:TStringList);
  private
    fItems:TCommandList;
    fBaseHelp:TCommandProc;
  end;

implementation

uses SysUtils, MKToolbox, Logger;

constructor TCommandValidator.Create;
begin
  fItems:=TCommandList.Create;
  fBaseHelp:=iBaseHelp;
end;

destructor TCommandValidator.Destroy;
begin
  fItems.Free;
  inherited ;
end;

procedure TCommandValidator.AddCommand(pCommandString:string;pProc:TCommandProc);
var atm:TCommandItem;
  mode,i:integer;
begin
  atm:=TCommandItem.Create;
  mode:=0;
  for i:=1 to length(pCommandString) do begin
    case mode of
      0:if pCommandString[i]='[' then mode:=1;
      1:begin
          if pCommandString[i]=' ' then pCommandString[i]:=#255
          else if pCommandString[i]=']' then mode:=0;
        end;
    end;
  end;
  atm._string:=pCommandString;
  atm._proc:=pProc;
  fItems.Add(atm);
end;

procedure TCommandValidator.ValidateAndCallCommand(pParms:TStringList);
var i,j,k,maxc,maxi:integer;comm,s,s2:string;match:boolean;
begin
  try
    i:=0;
    maxc:=0;
    maxi:=-1;
    repeat
      j:=0;
      comm:=fItems[i]._string;
      repeat
        match:=false;
        s:=StringReplace(GetNthSegment(comm,' ',j+1),#255,' ',[rfReplaceAll]);

//        Log.Trace(pParms[j]+' vs. '+s);
        if (length(s)>2) and (s[1]='"') and (s[length(s)]='"') then begin  // String literal
          match:=uppercase(copy(s,2,length(s)-2))=uppercase(pParms[j])
        end else
        if (length(s)>2) and (s[1]='<') and (s[length(s)]='>') then begin  // Item of a set
          s:=copy(s,2,length(s)-2)+'|';
          while (length(s)>0) and not match do begin
            s2:=copy(s,1,pos('|',s)-1);
            delete(s,1,pos('|',s));
            if (length(s2)>2) and (s2[1]='"') and (s2[length(s2)]='"') then s2:=copy(s2,2,length(s2)-2);
            match:=uppercase(s2)=uppercase(pParms[j]);
          end;
        end else
        if uppercase(s)='S' then begin  // String
          match:=true;  // everything is a string... :)
        end else
        if uppercase(s)='I' then begin  // Integer
          match:=IsNumeric(pParms[j]);
        end else
        if uppercase(s)='B' then begin  // Byte
          if IsNumeric(pParms[j]) then
            match:=(strtoint(pParms[j])>=0) and (strtoint(pParms[j])<=255);
        end;
        if (length(s)>4) and (s[1]='@') then begin  // Repeated based on data
          delete(s,1,1);
          s2:=copy(s,1,pos('[',s)-1);
          delete(s,1,pos('[',s)-1);
          if not IsNumeric(s2) or (length(s)<3) or ((length(s)>=3) and ((s[1]<>'[') or (s[length(s)]<>']'))) then
            raise Exception.Create('Invalid command validation string:'#13#10+fItems[i]._string);
          s:=copy(s,2,length(s)-2);
          if strtoint(s2)<pParms.Count then begin
            s2:=pParms[strtoint(s2)];
            if IsNumeric(s2) then begin
              k:=strtoint(s2);
              s2:='';
              while k>0 do begin
                s2:=alltrim(s2)+' '+s;
                dec(k);
              end;
              comm:=ReplaceNthSegment(comm,' ',j+1,alltrim(s2));
              dec(j);  // Skip back, this validation chunk is removed. (And others are added)
              match:=true;
            end;
          end;
        end;
        inc(j);
      until (j=pParms.Count) or not match;
      if match then begin
//        Log.Trace(Format('j-1, maxc, i: %d,%d,%d',[j-1,maxc,i]));
        if j-1>maxc then begin maxc:=j-1;maxi:=i;end;
      end;
      match:=match and (GetNthSegment(comm,' ',j+1)='');
      inc(i);
    until (i=fItems.Count) or match;
    dec(i);
    if Match then
      fItems[i]._proc(pParms,false)
    else begin
      if maxc>0 then
        fItems[maxi]._proc(pParms,true)
      else
        fBaseHelp(pParms,true);
    end;
  except
    on E:Exception do begin
      writeln('*** ',e.Message);
    end;
  end;
end;

end.

