{
  This file is part of the source code of MKConv2.
  See "copyright.txt" for details.
}

unit AnimXMLReader;

{$mode Delphi}

interface

uses
  SysUtils, ARGBImageUnit;

procedure LoadAnimXML(pImage:TARGBImage;pFilename:string);

implementation

uses DOM, XmlRead, AnimationDataUnit, MKToolbox;

procedure GetXY(pNode:TDOMNode;out x:integer;out y:integer);
var tmp:TDOMNode;
begin
  tmp:=pNode.Attributes.GetNamedItem('x');
  if assigned(tmp) then
    x:=strtoint(string(tmp.NodeValue))
  else
    raise Exception.Create(Format('Attribute named "x" is missing from %s tag!',[String(pNode.NodeName)]));
  tmp:=pNode.Attributes.GetNamedItem('y');
  if assigned(tmp) then
    y:=strtoint(string(tmp.NodeValue))
  else
    raise Exception.Create(Format('Attribute named "y" is missing from %s tag!',[String(pNode.NodeName)]));
end;

procedure ProcessAnimation(pImage:TARGBImage;pNode:TDOMNode);
type TFrame=record x,y:integer;end;
var tc,name:String;ty,i,j,wi,he,fdly,ldlyi,hpx,hpy,stfr:integer;fps,ldlyt,pps:double;
  looped,randomstart,paused,pingpong,reversed:boolean;
  frames:array of TFrame;
  atmF:TFrameBasedAnimationData;
  atmT:TTimeBasedAnimationData;
begin
  ty:=strtoint(decode(uppercase(string(pNode.Attributes.GetNamedItem('type').NodeValue)),
    'FRAMEBASED,1,TIMEBASED,2,0'));
  if ty=0 then
    raise Exception.Create(Format('Unknown Animation type attribute! (%s)',[string(pNode.Attributes.GetNamedItem('type').NodeValue)]));
  wi:=0;he:=0;
  name:='';
  hpx:=0;
  hpy:=0;
  stfr:=0;
  fdly:=0;
  ldlyi:=0;
  fps:=1;
  ldlyt:=0;
  looped:=false;
  randomstart:=false;
  paused:=false;
  pingpong:=false;
  reversed:=false;
  SetLength(frames,0);
  with pNode.ChildNodes do try
    for i:=0 to Count-1 do begin
      tc:=String(Item[i].TextContent);
      if uppercase(Item[i].NodeName)='NAME' then name:=tc
      else if uppercase(Item[i].NodeName)='WIDTH' then wi:=strtoint(tc)
      else if uppercase(Item[i].NodeName)='HEIGHT' then he:=strtoint(tc)
      else if uppercase(Item[i].NodeName)='STARTFRAME' then stfr:=strtoint(tc)
      else if uppercase(Item[i].NodeName)='FRAMEDELAY' then begin
        if ty=1 then
          fdly:=strtoint(tc)
        else
          raise Exception.Create('TimeBased animation can''t have FrameDelay tag!');
      end
      else if uppercase(Item[i].NodeName)='FPS' then begin
        if ty=2 then
          fps:=StrToFloat(tc,FS)
        else
          raise Exception.Create('FrameBased animation can''t have FPS tag!');
      end
      else if uppercase(Item[i].NodeName)='PPS' then begin
        if ty=2 then
          pps:=StrToFloat(tc,FS)
        else
          raise Exception.Create('FrameBased animation can''t have PPS tag!');
      end
      else if uppercase(Item[i].NodeName)='LOOPDELAY' then begin
        if ty=1 then
          ldlyi:=strtoint(tc)
        else
          ldlyt:=StrToFloat(tc,FS);
      end
      else if uppercase(Item[i].NodeName)='HOTPOINT' then GetXY(Item[i],hpx,hpy)
      else if uppercase(Item[i].NodeName)='LOOPED' then looped:=true
      else if uppercase(Item[i].NodeName)='RANDOMSTART' then randomstart:=true
      else if uppercase(Item[i].NodeName)='PAUSED' then paused:=true
      else if uppercase(Item[i].NodeName)='PINGPONG' then pingpong:=true
      else if uppercase(Item[i].NodeName)='REVERSED' then reversed:=true
      else if uppercase(Item[i].NodeName)='FRAMES' then begin
        with Item[i].ChildNodes do try
          for j:=0 to Count-1 do begin
            if uppercase(Item[j].NodeName)='FRAME' then begin
              SetLength(frames,system.length(frames)+1);
              GetXY(Item[j],frames[system.length(frames)-1].x,frames[system.length(frames)-1].y);
            end else raise Exception.Create('Only Frame tags expected within Frames!');
          end;
        finally
          Free;
        end;
      end
      else raise Exception.Create(Format('Unknown tag within ANIMATION! (%s)',[Item[i].NodeName]));
    end;
  finally
    Free;
  end;
  if (wi=0) then raise Exception.Create('Width is not specified!');
  if (he=0) then raise Exception.Create('Height is not specified!');
  if length(frames)=0 then raise Exception.Create('No frames are specified!');
  if ty=1 then begin
    atmf:=TFrameBasedAnimationData.Create(wi,he);
    atmf.Name:=name;
    atmf.StartFrame:=stfr;
    atmf.FrameDelay:=fdly;
    atmf.LoopDelay:=ldlyi;
    atmf.HotPointX:=hpx;
    atmf.HotPointY:=hpy;
    atmf.Looped:=looped;
    atmf.RandomStart:=randomstart;
    atmf.Paused:=paused;
    atmf.PingPong:=pingpong;
    atmf.ReverseAnim:=reversed;
    for i:=0 to length(frames)-1 do atmf.AddFrame(frames[i].x,frames[i].y);
    pImage.Animations.AddObject(atmf.Name,atmf);
  end else
  if ty=2 then begin
    atmt:=TTimeBasedAnimationData.Create(wi,he);
    atmt.Name:=name;
    atmt.StartFrame:=stfr;
    atmt.FPS:=fps;
    atmt.PPS:=pps;
    atmt.LoopDelay:=ldlyt;
    atmt.HotPointX:=hpx;
    atmt.HotPointY:=hpy;
    atmt.Looped:=looped;
    atmt.RandomStart:=randomstart;
    atmt.Paused:=paused;
    atmt.PingPong:=pingpong;
    atmt.ReverseAnim:=reversed;
    for i:=0 to length(frames)-1 do atmt.AddFrame(frames[i].x,frames[i].y);
    pImage.Animations.AddObject(atmt.Name,atmt);
  end;
end;

procedure LoadAnimXML(pImage:TARGBImage; pFilename:string);
var XML:TXMLDocument;Node:TDOMNode;i:integer;
begin
  ReadXMLFile(XML,pFilename);
  try
    Node:=XML.DocumentElement;
    with Node.ChildNodes do try
      for i:=0 to Count-1 do
        if uppercase(Item[i].NodeName)='ANIMATION' then ProcessAnimation(pImage,Item[i])
        else raise Exception.Create('ANIMATION tag expected within ANIMATIONS!');
    finally
      Free;
    end;
  finally
    XML.Free;
  end;
end;



end.

