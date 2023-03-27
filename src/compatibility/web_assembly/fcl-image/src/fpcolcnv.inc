{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Some color conversion routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{function FillOtherBits (initial:word;CorrectBits:byte):word;
var r,c : byte;
begin
  c := 16 div CorrectBits;
  result := initial;
  for r := 1 to c do
    result := (result shr CorrectBits) or result;
end;

function ShiftAndFill (initial:word; CorrectBits:byte):word;
begin
  result := FillOtherBits (initial shl (16-correctbits), correctbits);
end;
}
function CalculateGray (const from : TFPcolor) : word;
var temp : longword;
begin
  with GrayConvMatrix do
    temp := round(red*from.red + green*from.green + blue*from.blue);
  if temp > $ffff then
    result := $ffff
  else
    result := temp;
end;

(*
type
  TColorBits = array [0..3] of TColorData;
     // 0:alpha, 1:red, 2:green, 3:blue
  TShiftBits = array [0..3] of shortint;

const
  ColorBits : array[cfRGB15..cfABGR64] of TColorBits = (
    //          alpha       red        green      blue
    {cfRGB15} ($00000000, $00007C00, $000003E0, $0000001F),
    {cfRGB16} ($00000000, $00007C00, $000003E0, $0000001F),
    {cfRGB24} ($00000000, $00FF0000, $0000FF00, $000000FF),
    {cfRGB32} ($00000000, $00FF0000, $0000FF00, $000000FF),
    {cfRGB48} ($00000000, $FFFF0000, $FFFF0000, $0000FFFF),
    //                     shl 16
    {cfRGBA8} ($00000003, $000000C0, $00000030, $0000000C),
    {cfRGBA16}($0000000F, $0000F000, $00000F00, $000000F0),
    {cfRGBA32}($000000FF, $FF000000, $00FF0000, $0000FF00),
    {cfRGBA64}($0000FFFF, $FFFF0000, $FFFF0000, $FFFF0000),
    //                    shl 32     shl 16
    {cfBGR15} ($00000000, $0000001F, $000003E0, $00007C00),
    {cfBGR16} ($00000000, $0000001F, $000003E0, $00007C00),
    {cfBGR24} ($00000000, $000000FF, $0000FF00, $00FF0000),
    {cfBGR32} ($00000000, $000000FF, $0000FF00, $00FF0000),
    {cfBGR48} ($00000000, $0000FFFF, $FFFF0000, $FFFF0000),
    //                                           shl 16
    {cfABGR8} ($000000C0, $00000003, $0000000C, $00000030),
    {cfABGR16}($0000F000, $0000000F, $000000F0, $00000F00),
    {cfABGR32}($FF000000, $000000FF, $0000FF00, $00FF0000),
    {cfABGR64}($FFFF0000, $0000FFFF, $FFFF0000, $FFFF0000)
    //          shl 32                          shl 16
  );
  ShiftBits : array[cfRGB15..cfABGR64] of TShiftBits = (  // <0:shl, >0:shr
    {cfRGB15} (  0,  -1,  -6, -11),
    {cfRGB16} (  0,  -1,  -6, -11),
    {cfRGB24} (  0,   8,   0,  -8),
    {cfRGB32} (  0,   8,   0,  -8),
    {cfRGB48} (  0,  32,  16,   0),
    {cfRGBA8} (-14,  -8, -10, -12),
    {cfRGBA16}(-12,   0,  -4,  -8),
    {cfRGBA32}( -8,  16,   8,   0),
    {cfRGBA64}(  0,  48,  32,  16),
    {cfBGR15} (  0, -11,  -6,  -1),
    {cfBGR16} (  0, -11,  -6,  -1),
    {cfBGR24} (  0,  -8,   0,   8),
    {cfBGR32} (  0,  -8,   0,   8),
    {cfBGR48} (  0,   0,  16,  32),
    {cfBGRA8} ( -8, -14, -12, -10),
    {cfBGRA16}(  0, -12,  -8,  -4),
    {cfBGRA32}( 16,  -8,   0,   8),
    {cfBGRA64}( 48,   0,  16,  32)
    );
  Bitdepths : array[cfRGB15..cfABGR64] of byte=
    (5,5,8,8,16, 2,4,8,16, 5,5,8,8,16, 2,4,8,16);

function EnlargeColor (data:TColorData;CFmt:TColorFormat;component:byte):word;
var w : word;
    i : TColorData;
    s : shortint;
begin
  i := data and ColorBits[CFmt,component];
  s := ShiftBits[CFmt,component];
  if s = 0 then
    w := i
  else if s < 0 then
    w := i shl -s
  else
    w := i shr s;
  result := FillOtherBits (w ,BitDepths[CFmt]);
end;

function ConvertColor (const From : TColorData; FromFmt:TColorFormat) : TFPColor;
  function SetGrayScale (value : word) : TFPColor;
  begin
    with result do
      begin
      red := Value;
      green := value;
      blue := Value;
      end;
  end;
  function SetGrayScaleA (value : word) : TFPColor;
  begin
    result := SetGrayScale (value);
    result.alpha := alphaOpaque;
  end;
var m : qword;
begin
  case FromFmt of
    cfMono : result := SetGrayScaleA (ShiftAndFill(From,1));
    cfGray2 : result := SetGrayScaleA (ShiftAndFill(From,2));
    cfGray4 : result := SetGrayScaleA (ShiftAndFill(From,4));
    cfGray8 : result := SetGrayScaleA (ShiftAndFill(From,8));
    cfGray16 : result := SetGrayScaleA (From);
    cfGray24 : result := SetGrayScaleA ((From and $00FFFF00) shr 8);
    cfGrayA8 :
      begin
      result := SetGrayScale (FillOtherBits((From and $000000F0) shl 8,4));
      result.alpha := ShiftAndFill((From and $0000000F),4);
      end;
    cfGrayA16 :
      begin
      result := SetGrayScale (FillOtherBits((From and $0000FF00),8));
      result.alpha := ShiftAndFill((From and $000000FF),8);
      end;
    cfGrayA32 :
      begin
      result := SetGrayScale ((From and $FFFF0000) shr 16);
      result.alpha := (From and $0000FFFF);
      end;
    cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
    cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48 :
      begin
      result.alpha := AlphaOpaque;
      result.red := EnlargeColor(From, FromFmt, 1);
      result.green := EnlargeColor(From, FromFmt, 2);
      result.blue := EnlargeColor(From, FromFmt, 3);
      end;
    cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
    cfABGR8,cfABGR16,cfABGR32,cfABGR64 :
      begin
      result.alpha := EnlargeColor(From, FromFmt, 0);
      result.red := EnlargeColor(From, FromFmt, 1);
      result.green := EnlargeColor(From, FromFmt, 2);
      result.blue := EnlargeColor(From, FromFmt, 3);
      end;
  end;
end;

function ConvertColor (const From : TDeviceColor) : TFPColor;
begin
  result := ConvertColor (From.data, From.Fmt)
end;

const BitMasks : array[1..32] of longword =
    ($8000000, $C000000, $E000000, $F000000,
     $F800000, $FC00000, $FE00000, $FF00000,
     $FF80000, $FFC0000, $FFE0000, $FFF0000,
     $FFF8000, $FFFC000, $FFFE000, $FFFF000,
     $FFFF800, $FFFFC00, $FFFFE00, $FFFFF00,
     $FFFFF80, $FFFFFC0, $FFFFFE0, $FFFFFF0,
     $FFFFFF8, $FFFFFFC, $FFFFFFE, $FFFFFFF,
     $FFFFFFF, $FFFFFFF, $FFFFFFF, $FFFFFFF);

procedure PrepareBitMasks;
{ Putting the correct bits in the array (problem with constants in compiler 1.0)}
var r : integer;
begin
  for r := 1 to 32 do
    BitMasks[r] := BitMasks[r] shl 4;
  inc (BitMasks[29], $8);
  inc (BitMasks[30], $C);
  inc (BitMasks[31], $E);
  inc (BitMasks[32], $F);
end;

function CalculateGray (const c : TFPcolor; Bits:byte) : TColorData;
var temp : longword;
begin
  with GrayConvMatrix do
    temp := round(red*c.red + green*c.green + blue*c.blue);
  result := temp;
  //temp := temp + (result shl 16);
  //result := temp and BitMasks[Bits];
  {if not (c = colBlack) then
    with c do
      //writeln ('red:',red,' - green:',green,' - blue:',blue, ' : result=',result);
      writeln (format('red:%4x - green:%4x - blue:%4x => result:%4x',[integer(red),
               integer(green),integer(blue),integer(result)]));}
end;

function CalculateGrayA (const c : TFPcolor; Bits:byte) : TColorData;
var r : longword;
    d : byte;
begin
  d := bits div 2;
  r := CalculateGray (c, d);
  result := r shl d;
  r := c.alpha shr (16-d);
  result := result or r;
end;

function ConvertColorToData (const From : TFPColor; Fmt : TColorFormat) : TColorData;
var sb : TShiftBits;
    cb : TColorBits;
  function MakeSample (Value:word; ToShift:shortint; ToUse:TColorData) : TColorData;
  var sh : word;
  begin
    result := Value;
    if ToShift >= 0 then
      begin
      sh := ToShift;            // if not converting first to word, there will be a
      result := result shl Sh;  // color shift
      end
    else
      begin
      sh := -ToShift;
      result := result shr Sh;
      end;
    result := result and ToUse;
  end;
begin
  case Fmt of
    cfMono : result := CalculateGray (From,1);
    cfGray2 : result := CalculateGray (From,2);
    cfGray4 : result := CalculateGray (From,4);
    cfGray8 : result := CalculateGray (From,8);
    cfGray16 : result := CalculateGray (From,16);
    cfGray24 : result := CalculateGray (From,24);
    cfGrayA8 : result := CalculateGrayA (From, 8);
    cfGrayA16 : result := CalculateGrayA (From, 16);
    cfGrayA32 : result := CalculateGrayA (From, 32);
    cfRGB15,cfRGB16,cfRGB24,cfRGB32,cfRGB48,
    cfBGR15,cfBGR16,cfBGR24,cfBGR32,cfBGR48 :
      begin
      sb := ShiftBits[Fmt];
      cb := ColorBits[Fmt];
      result := MakeSample(From.blue, sb[3], cb[3]) or
                MakeSample(From.red, sb[1], cb[1]) or
                MakeSample(From.green, sb[2], cb[2]);
     {$ifdef FPC_Debug_Image}
      with From do
        writeln (red,',',green,',',blue,',',result);
      end;
     {$endif}
    cfRGBA8,cfRGBA16,cfRGBA32,cfRGBA64,
    cfABGR8,cfABGR16,cfABGR32,cfABGR64 :
      begin
      sb := ShiftBits[Fmt];
      cb := ColorBits[Fmt];
      result := MakeSample(From.alpha, sb[0], cb[0]) or
                MakeSample(From.red, sb[1], cb[1]) or
                MakeSample(From.green, sb[2], cb[2]) or
                MakeSample(From.blue, sb[3], cb[3]);
      end;
  end;
end;

function ConvertColor (const From : TFPColor; Fmt : TColorFormat) : TDeviceColor;
begin
  result.Fmt := Fmt;
  result.data := convertColorToData(From, Fmt);
end;

function ConvertColorToData (const From : TDeviceColor; Fmt : TColorFormat) : TColorData;
var c : TFPColor;
begin
  c := ConvertColor (From);
  result := ConvertColorToData (c, Fmt);
end;

function ConvertColor (const From : TDeviceColor; Fmt : TColorFormat) : TDeviceColor;
begin
  result.Fmt := Fmt;
  result.data := ConvertColorToData (From, Fmt);
end;
*)

function AlphaBlend(const color1, color2: TFPColor): TFPColor;
var
  factor1, factor2: single;
begin
  if color2.alpha = $ffff then
    Result := color2
  else
  if color2.alpha = 0 then
    Result := color1
  else
  if color1.alpha = 0 then
    Result := color2
  else
  begin
    factor1 := (color1.alpha / $ffff) * (1 - color2.alpha / $ffff);
    factor2 := color2.alpha / $ffff;

    Result.red := Round(color1.red * factor1 + color2.red * factor2);
    Result.green := Round(color1.green * factor1 + color2.green * factor2);
    Result.blue := Round(color1.blue * factor1 + color2.blue * factor2);
    Result.alpha := Round(factor1 * $ffff + color2.alpha);
  end;
end;

function CompareColors(const Color1, Color2: TFPColor): integer;
begin
  Result:=integer(Color1.Red)-integer(Color2.Red);
  if Result<>0 then exit;
  Result:=integer(Color1.Green)-integer(Color2.Green);
  if Result<>0 then exit;
  Result:=integer(Color1.Blue)-integer(Color2.Blue);
  if Result<>0 then exit;
  Result:=integer(Color1.Alpha)-integer(Color2.Alpha);
end;

