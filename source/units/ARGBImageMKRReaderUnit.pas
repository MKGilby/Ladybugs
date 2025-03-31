{
  MKSZTSZ MKR reader for TARGBImage
  ------------------------------------------------------------------
  You can freely distribute the sources under the GNU GPL Version 3.

  Written by Gilby/MKSZTSZ
  Hungary, 2024
  ------------------------------------------------------------------

  This reader reads the MKR font as a two-color image,
  white pixels on black background.

  File-format specification for MKAR (.MKR) files

  Ofs    Len   Abbr  Description
   0      4     ID   'MKAR' - FourCC
   4      1     FL   Flags: 0. - y g q should be drawn at y+1...
                            1. - Char map exists after HE (Char map is 32 bytes,
                                 contains one bit for each char. If bit is set
                                 the char data exists in data stream.)
   5      1     WI   Width of chars (in pixel)
   6      1     HE   Height of chars
 7 or 39  (1+(WI-1) div 8)*HE*256 bytes of charactes definitions

   One char length in bytes =(1+(WI-1) div 8)*HE.

   Format (example: Letter A in 12*16 pixels):

     This is only filling to 8 bits. It can be anything.
      |
      |  Important bits!
      |  ||||||||||||
     +--++----------+
     0000000011110000  $00 $F0
     0000001111111100  $03 $FC
     0000011111111110  $07 $FE
     0000011110011110  $07 $9E
     0000111100001111  $0F $0F
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111111111111  $0F $FF
     0000111111111111  $0F $FF
     0000111111111111  $0F $FF
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07
     0000111000000111  $0E $07

  Version info:
    1.00 - Gilby - 2024.03.27
     * Initial creation.
    1.00a - Gilby - 2024.10.02
     + Added MKR file format specification above.
    1.01 - Gilby - 2025.02.21
     + Pre-filling rawdata with opaque black.
}

unit ARGBImageMKRReaderUnit;

{$mode delphi}

interface

implementation

uses
  Classes, SysUtils,
  ARGBImageUnit, AnimationDataUnit, FontDataUnit, MKToolbox, Logger;

const
  Fstr={$I %FILE%}+', ';
  Version='1.01';

procedure ReadMKR(pSource:TStream;out Width,Height:integer;out RawData:pointer;{%H-}Animations:TAnimationDatas;out FontData:TFontData);
const
  ad2:array[0..7] of integer=(128,64,32,16,8,4,2,1);
  FL_SLIDEDOWN_GQY=1;
  FL_HASCHARTABLE=2;

var s:String;chars:array[0..31] of byte;
    i,x,y:integer;
    b:integer;
    Flags,CharWidth,CharHeight,CharCount,CurrentChar,Slide:integer;
begin
  Flags:=0;CharWidth:=0;CharHeight:=0;chars[0]:=0;

  s:=#0#0#0#0;
  pSource.Read(s[1],4);
  if s='MKAR' then begin
    pSource.Read(Flags,1);
    pSource.Read(CharWidth,1);
    pSource.Read(CharHeight,1);
    Height:=CharHeight;
    if Flags and FL_SLIDEDOWN_GQY<>0 then
      inc(Height);  // this flag means that g q y and p goes one pixel down
                    // so the real height is increased by one pixel

    if Flags and FL_HASCHARTABLE<>0 then begin  // Has char table, so count chars
      pSource.Read(chars[0],32);
      CharCount:=0;
      for i:=0 to 31 do CharCount+=CountBitsInByte(chars[i]);
    end else begin  // No char table, every char is there.
      CharCount:=256;
    end;

    Width:=CharCount*(CharWidth+1)-1;
    RawData:=getmem(Width*Height*4);
    for i:=0 to Width*Height-1 do
      uint32((RawData+i*4)^):=$ff000000;
    FontData:=TFontData.Create;

    // And now create image from each char that exists.
    // We have to create the coords info too.
    CurrentChar:=0;b:=0;
    for i:=0 to 255 do begin
      if (CharCount=256) or
         ((CharCount<256) and (chars[i div 8] and ad2[i mod 8]<>0)) then begin  // Character exists in data file

        FontData.SetCharBox(i,CurrentChar*(CharWidth+1),0,CharWidth,Height);
        if (Flags and FL_SLIDEDOWN_GQY<>0) and (i in [121,112,113,103]) then Slide:=1 else Slide:=0;
        for y:=0 to CharHeight-1 do begin
          for x:=0 to CharWidth-1 do begin
            if x mod 8=0 then begin
              pSource.Read(b,1);
              if (CharWidth mod 8<>0) and (x=0) then b:=b<<(8-(CharWidth mod 8));  // Skip filling bits
            end;
            if b and 128<>0 then
              uint32((RawData+((y+Slide)*Width+CurrentChar*(CharWidth+1)+x)*4)^):=$ffffffff
            else
              uint32((RawData+((y+Slide)*Width+CurrentChar*(CharWidth+1)+x)*4)^):=$ff000000;
            b:=(b<<1) and $ff;
          end;
        end;
        inc(CurrentChar);
      end;
    end;
  end;
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');
  RegisterARGBImageReader('MKR',@ReadMKR,true);

end.

