{ Copied to Castle Game Engine from FPC RTL (FPC RTL uses the same license
  as Castle Game Engine, so no problem).
  Adjusted to use CastleFreeType and CastleFreeTypeH.
}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    This file is adapted from the FPC RTL source code, as such
    the license and copyright information of FPC RTL applies here.
    That said, the license of FPC RTL happens to be *exactly*
    the same as used by the "Castle Game Engine": LGPL (version 2.1)
    with "static linking exception" (with exactly the same wording
    of the "static linking exception").
    See the file COPYING.txt, included in this distribution, for details about
    the copyright of "Castle Game Engine".
    See http://www.freepascal.org/faq.var#general-license about the copyright
    of FPC RTL.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ @exclude Not ready for PasDoc. }
unit CastleFtFont;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FPCanvas, fpimgcmn,
  CastleFreeType, CastleFreeTypeH, CastleUtils;

type

  FreeTypeFontException = class (TFPFontException);

  TFreeTypeFont = class (TFPCustomDrawFont)
  private
    FResolution : longword;
    FAntiAliased : boolean;
    FLastText : TStringBitmaps;
    FIndex, FFontID : integer;
    FFace : PFT_Face;
    FAngle : real;
    procedure ClearLastText;
  protected
    procedure DrawChar (x,y:integer; data:CastleUtils.PByteArray; pitch, width, height:integer); virtual;
    procedure DrawCharBW (x,y:integer; data:CastleUtils.PByteArray; pitch, width, height:integer); virtual;
    procedure SetName (AValue:string); override;
    procedure SetIndex (AValue : integer);
    procedure SetSize (AValue : integer); override;
    function GetFlags (index:integer) : boolean; override;
    procedure SetFlags (index:integer; AValue:boolean); override;
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps (From:TFPCanvasHelper); override;
    procedure DoDrawText (atx,aty:integer; atext:string); override;
    procedure DoGetTextSize (text:string; var w,h:integer); override;
    function DoGetTextHeight (text:string) : integer; override;
    function DoGetTextWidth (text:string) : integer; override;
    procedure GetText (aText:string);
    procedure GetFace;
  public
    constructor create; override;
    destructor Destroy; override;
    property FontIndex : integer read FIndex write SetIndex;
    property Resolution : longword read FResolution write FResolution;
    property AntiAliased : boolean read FAntiAliased write FAntiAliased;
    property Angle : real read FAngle write FAngle;
  end;

var
  FontMgr : TFontManager;

procedure InitEngine;
procedure DoneEngine;


implementation

uses fpimage;

procedure InitEngine;

begin
  if not assigned (FontMgr) then
    FontMgr := TFontManager.create;
end;

procedure DoneEngine;
begin
  if assigned (FontMgr) then
    FontMgr.Free;
end;

constructor TFreeTypeFont.Create;
begin
  inherited;
  FFontID := -1;
  FAntiAliased := True;
  FResolution := DefaultResolution;
end;

destructor TFreeTypeFont.Destroy;
begin
  ClearLastText;
  inherited Destroy;
end;

procedure TFreeTypeFont.DoCopyProps (From:TFPCanvasHelper);
var f : TFreeTypeFont;
begin
  inherited;
  if from is TFreeTypeFont then
    begin
    f := TFreeTypeFont(from);
    FIndex := F.Findex;
    FAntiAliased := f.FAntiAliased;
    FResolution := f.FResolution;
    FAngle := f.FAngle;
    end;
end;

procedure TFreeTypeFont.SetName (AValue:string);
begin
  inherited;
  ClearLastText;
  if allocated then
    FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.SetIndex (AValue : integer);
begin
  FIndex := AValue;
  ClearLastText;
  if allocated then
    FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.SetSize (AValue : integer);
begin
  ClearLastText;
  inherited;
end;

procedure TFreeTypeFont.ClearLastText;
begin
  if assigned(FLastText) then
    begin
    FLastText.Free;
    FlastText := nil;
    end;
end;

procedure TFreeTypeFont.DoAllocateResources;
begin
  InitEngine;
  FFontID := FontMgr.RequestFont(Name, FIndex);
end;

procedure TFreeTypeFont.DoDeAllocateResources;
begin
end;

procedure TFreeTypeFont.DoGetTextSize (text:string; var w,h:integer);
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    begin
    w := right - left;
    h := top - bottom;
    end;
end;

function TFreeTypeFont.DoGetTextHeight (text:string) : integer;
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    result := top - bottom;
end;

function TFreeTypeFont.DoGetTextWidth (text:string) : integer;
var r : TRect;
begin
  GetText (text);
  FLastText.GetBoundRect (r);
  with r do
    result := right - left;
end;

procedure TFreeTypeFont.SetFlags (index:integer; AValue:boolean);
begin
  if not (index in [5,6]) then   // bold,italic
    inherited SetFlags (index, AValue);
end;

procedure TFreeTypeFont.GetFace;
begin
  if not assigned(FFace) then
    FFace := FontMgr.GetFreeTypeFont (FFontID);
end;

function TFreeTypeFont.GetFlags (index:integer) : boolean;
begin
  if index = 5 then        //bold
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_BOLD) <> 0;
    end
  else if index = 6 then    //italic
    begin
    GetFace;
    result := (FFace^.style_flags and FT_STYLE_FLAG_ITALIC) <> 0;
    end
  else
    result := inherited GetFlags (index);
end;

procedure TFreeTypeFont.GetText (aText:string);
var b : boolean;
begin
  if assigned (FLastText) then
    begin
    if CompareStr(FLastText.Text,aText) <> 0 then
      begin
      FLastText.Free;
      b := true;
      end
    else
      begin
      if FAntiAliased then
        b := (FLastText.mode <> bt256Gray)
      else
        b := (FLastText.mode <> btBlackWhite);
      if b then
        FLastText.Free;
      end;
    end
  else
    b := true;
  if b then
    begin
    FontMgr.Resolution := FResolution;
    if FAntiAliased then
      FLastText := FontMgr.GetStringGray (FFontId, aText, Size, Angle)
    else
      FLastText := FontMgr.GetString (FFontId, aText, Size, Angle);
    end;
end;

procedure TFreeTypeFont.DoDrawText (atX,atY:integer; atext:string);
var r : integer;
begin
  GetText (atext);
  with FLastText do
    for r := 0 to count-1 do
      with Bitmaps[r]^ do
        begin
        if mode = btBlackWhite then
          DrawCharBW (atX+x, atY+y, data, pitch, width, height)
        else
          DrawChar (atX+x, atY+y, data, pitch, width, height);
        end;
end;

const
  //bits : array[0..7] of byte = (1,2,4,8,16,32,64,128);
  bits : array[0..7] of byte = (128,64,32,16,8,4,2,1);

procedure TFreeTypeFont.DrawChar (x,y:integer; data:CastleUtils.PByteArray; pitch, width, height:integer);

  procedure Combine (canv:TFPCustomCanvas; x,y:integer; c : TFPColor; t:longword);
  var
    pixelcolor: TFPColor;
  begin
    pixelcolor := AlphaBlend(canv.colors[x,y], FPImage.FPColor(c.red, c.green,c.blue, (t+1) shl 8 - 1));
    canv.colors[x,y] := pixelcolor;
  end;

var b,rx,ry : integer;
begin
  b := 0;
  for ry := 0 to height-1 do
    begin
    for rx := 0 to width-1 do
      combine (canvas, x+rx, y+ry, FPColor, data^[b+rx]);
    inc (b, pitch);
    end;
end;

procedure TFreeTypeFont.DrawCharBW (x,y:integer; data:CastleUtils.PByteArray; pitch, width, height:integer);
var rb : byte;
    rx,ry,b,l : integer;
begin
  b := 0;
  for ry := 0 to height-1 do
    begin
    l := 0;
    for rx := 0 to width-1 do
      begin
      rb := rx mod 8;
      if (data^[b+l] and bits[rb]) <> 0 then
        canvas.colors[x+rx,y+ry] := FPColor;
      if rb = 7 then
        inc (l);
      end;
    inc (b, pitch);
    end;
end;


finalization
  DoneEngine;
end.
