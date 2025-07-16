{
  This file is part of the source code of Ladybugs.
  See "copyright.txt" for details.
}

unit LBButton;

{$mode Delphi}

interface

uses
  SysUtils, vcc2_ButtonStatic, vcc2_ButtonLogicStatic, Font2Unit, ColorUnit;

type

  { TLBButton }

  TLBButton=class(TButton)
    constructor Create(iX,iY,iWidth,iHeight:integer;iColor:UInt32;iName,iCaption:string;iFont:TFont);
  protected
    procedure ReDraw; override;
  private
    fColor:TColor;
  end;

implementation

{ TLBButton }

constructor TLBButton.Create(iX,iY,iWidth,iHeight:integer; iColor:UInt32;
  iName,iCaption:string; iFont:TFont);
begin
  inherited Create;
  Left:=iX;
  Top:=iY;
  Width:=iWidth;
  Height:=iHeight;
  TextAlignX:=1;
  TextOffsetY:=0;
  fCaption:=iCaption;
  fName:=iName;
  Font:=iFont;
  fNeedRedraw:=true;
  fColor.Color32:=iColor;
end;

procedure TLBButton.ReDraw;
var c:array[0..4] of TColor;
begin
  with fImage do begin
    if fState=cHighlighted then begin
      c[0]:=fColor.Brighten(0.3);
      c[1]:=fColor.Brighten(0.2);
      c[2]:=fColor.Brighten(0.1);
      c[3]:=fColor;
      c[4]:=fColor.Darken(0.1);
    end else begin
      c[0]:=fColor.Brighten(0.2);
      c[1]:=fColor.Brighten(0.1);
      c[2]:=fColor;
      c[3]:=fColor.Darken(0.1);
      c[4]:=fColor.Darken(0.2);
    end;
    Rectangle(0,0,Width  ,Height  ,$ff000000);
    HLine(1,1,Width-2,c[0].Color32);
    VLine(Width-2,1,Height-2,c[0].Color32);
    HLine(2,2,Width-4,c[1].Color32);
    VLine(Width-3,2,Height-4,c[1].Color32);
    bar(3,3,Width-6,Height-6,c[2].Color32);
    HLine(1,Height-2,Width-2,c[4].Color32);
    VLine(1,1,Height-2,c[4].Color32);
    HLine(2,Height-3,Width-4,c[3].Color32);
    VLine(2,2,Height-4,c[3].Color32);
    if Assigned(fFont) then
      fFont.OutText(fImage,fCaption,fTextAlignPointX-fLeft,fTextAlignPointY+fTextOffsetY-fTop,fTextAlignX);
  end;
end;

end.

