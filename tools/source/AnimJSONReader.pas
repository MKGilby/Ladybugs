{
  This file is part of the source code of MKConv2.
  See "copyright.txt" for details.
}

unit AnimJSONReader;

{$mode Delphi}

interface

uses
  SysUtils, ARGBImageUnit;

procedure LoadAnimJSON(pImage:TARGBImage;pFilename:string);

implementation

uses Classes, fpjson, jsonparser, AnimationDataUnit, MKToolbox;

procedure ProcessAnimation(pImage:TARGBImage;JD:TJSONData);
type TFrame=record x,y:integer;end;
var name:String;ty,i,wi,he,fdly,ldlyi,hpx,hpy,stfr:integer;fps,ldlyt,pps:double;
  looped,randomstart,paused,pingpong,reversed:boolean;
  frames:array of TFrame;
  atmF:TFrameBasedAnimationData;
  atmT:TTimeBasedAnimationData;
  JA:TJSONArray;
begin
  ty:=strtoint(decode(uppercase(JD.FindPath('Type').AsString),
    'FRAMEBASED,1,TIMEBASED,2,0'));
  if ty=0 then
    raise Exception.Create(Format('Unknown Animation type attribute! (%s)',[string(JD.FindPath('Type').AsString)]));
  wi:=0;he:=0;
  name:='';
  hpx:=0;
  hpy:=0;
  stfr:=0;
  fdly:=0;
  ldlyi:=0;
  fps:=1;
  ldlyt:=0;
  pps:=1;
  looped:=false;
  randomstart:=false;
  paused:=false;
  pingpong:=false;
  reversed:=false;
  SetLength(frames,0);
  name:=JD.FindPath('Name').AsString;
  wi:=JD.FindPath('Width').AsInteger;
  he:=JD.FindPath('Height').AsInteger;
  stfr:=JD.FindPath('StartFrame').AsInteger;
  if ty=1 then begin
    fdly:=JD.FindPath('FrameDelay').AsInteger;
    ldlyi:=JD.FindPath('LoopDelay').AsInteger;
  end else
  if ty=2 then begin
    fps:=JD.FindPath('FPS').AsFloat;
    ldlyt:=JD.FindPath('LoopDelay').AsFloat;
    if Assigned(jd.FindPath('PPS')) then pps:=JD.FindPath('PPS').AsFloat;
  end;
  hpx:=JD.FindPath('HotPoint.x').AsInteger;
  hpy:=JD.FindPath('HotPoint.y').AsInteger;
  if Assigned(jd.FindPath('Looped')) then looped:=jd.FindPath('Looped').AsBoolean;
  if Assigned(jd.FindPath('RandomStart')) then randomstart:=jd.FindPath('RandomStart').AsBoolean;
  if Assigned(jd.FindPath('Paused')) then paused:=jd.FindPath('Paused').AsBoolean;
  if Assigned(jd.FindPath('Pingpong')) then pingpong:=jd.FindPath('Pingpong').AsBoolean;
  if Assigned(jd.FindPath('Reversed')) then reversed:=jd.FindPath('Reversed').AsBoolean;
  JA:=TJSONArray(JD.FindPath('Frames'));
  SetLength(frames,JA.Count);
  for i:=0 to JA.Count-1 do begin
    frames[i].x:=JA[i].FindPath('x').AsInteger;
    frames[i].y:=JA[i].FindPath('y').AsInteger;
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

procedure LoadAnimJSON(pImage:TARGBImage;pFilename:string);
var
  J:TJSONData;
  JA:TJSONArray;
  Xs:TStream;
  i:integer;
begin
  Xs:=TFileStream.Create(pFilename, fmOpenRead or fmShareDenyWrite);
  try
    J:=GetJSON(Xs);
  finally
    Xs.Free;
  end;
  try
    JA:=TJSONArray(J.FindPath('Animations'));
    for i:=0 to JA.Count-1 do
      ProcessAnimation(pImage,JA.Items[i]);
  finally
    J.Free;
  end;
end;

end.

