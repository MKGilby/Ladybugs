{
  --[Name]-------------------------------------------------

   32 bit TColor record and some manipulation tools.

  --[Disclaimer]-------------------------------------------

   Copyright (c) 2025 MKSZTSZ

   Permission is hereby granted, free of charge, to any
   person obtaining a copy of this software and associated
   documentation files (the "Software"), to deal in the
   Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute,
   sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice
   shall be included in all copies or substantial portions
   of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY
   OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
   LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
   OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   (MIT License)

   ---------------------------------------------------------
}

// Version info:
//   1.00 - Gilby - 2025.07.11
//     * Initial creation.

unit ColorUnit;

{$mode Delphi}

interface

const
  Fstr={$I %FILE%}+', ';
  Version='1.00';

type

  { TColor }

  TColor=record
    function Darken(pStrength:double):TColor;
    function Brighten(pStrength:double):TColor;
    case byte of
      0: (Color32: UInt32);
      1: (b,g,r,a: byte);
  end;

implementation

uses Logger;

{ TColor }

function TColor.Darken(pStrength:double):TColor;
begin
  if pStrength<0 then pStrength:=0
  else if pStrength>1 then pStrength:=1;
  Result.Color32:=Color32;
  Result.r:=trunc( (Result.r*(1-pStrength)) );
  Result.g:=trunc( (Result.g*(1-pStrength)) );
  Result.b:=trunc( (Result.b*(1-pStrength)) );
end;

function TColor.Brighten(pStrength:double):TColor;
begin
  if pStrength<0 then pStrength:=0
  else if pStrength>1 then pStrength:=1;
  Result.Color32:=Color32;
  Result.r:=255-trunc( ((255-Result.r)*(1-pStrength)) );
  Result.g:=255-trunc( ((255-Result.g)*(1-pStrength)) );
  Result.b:=255-trunc( ((255-Result.b)*(1-pStrength)) );
end;

initialization
  Log.LogStatus(Fstr+'version '+Version,'uses');

end.

