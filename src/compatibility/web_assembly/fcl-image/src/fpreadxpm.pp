{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    XPM reader class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}
unit FPReadXPM;

interface

uses FPImage, classes, sysutils;

type
  TFPReaderXPM = class (TFPCustomImageReader)
    private
      width, height, ncols, cpp, xhot, yhot : integer;
      xpmext : boolean;
      palette : TStringList;
      function HexToColor(s : string) : TFPColor;
      function NameToColor(s : string) : TFPColor;
      function DiminishWhiteSpace (s : string) : string;
    protected
      procedure InternalRead  (Str:TStream; Img:TFPCustomImage); override;
      function  InternalCheck (Str:TStream) : boolean; override;
    public
      constructor Create; override;
      destructor Destroy; override;
  end;

implementation

const
  WhiteSpace = ' '#9#10#13;

constructor TFPReaderXPM.create;
begin
  inherited create;
  palette := TStringList.Create;
end;

destructor TFPReaderXPM.Destroy;
begin
  Palette.Free;
  inherited destroy;
end;

function TFPReaderXPM.HexToColor(s : string) : TFPColor;
var l : integer;
  function CharConv (c : char) : longword;
  begin
    if (c >= 'A') and (c <= 'F') then
      result := ord (c) - ord('A') + 10
    else if (c >= '0') and (c <= '9') then
      result := ord (c) - ord('0')
    else
      raise exception.CreateFmt ('Wrong character (%s) in hexadecimal number', [c]);
  end;
  function convert (n : string) : word;
  var t,r: integer;
  begin
    result := 0;
    t := length(n);
    if t > 4 then
      raise exception.CreateFmt ('Too many bytes for color (%s)',[s]);
    for r := 1 to length(n) do
      result := (result shl 4) or CharConv(n[r]);
    // fill missing bits
    case t of
      1: result:=result or (result shl 4) or (result shl 8) or (result shl 12);
      2: result:=result or (result shl 8);
      3: result:=result or (result shl 12);
    end;
  end;
begin
  s := uppercase (s);
  l := length(s) div 3;
  result.red   := (Convert(copy(s,1,l)));
  result.green := (Convert(copy(s,l+1,l)));
  result.blue  :=  Convert(copy(s,l+l+1,l));
  result.alpha := AlphaOpaque;
end;

function TFPReaderXPM.NameToColor(s : string) : TFPColor;
begin
  s := lowercase (s);
  if s = 'transparent' then
    result := colTransparent
  else if s = 'none' then
    result := colTransparent
  else if s = 'black' then
    result := colBlack
  else if s = 'blue' then
    result := colBlue
  else if s = 'green' then
    result := colGreen
  else if s = 'cyan' then
    result := colCyan
  else if s = 'red' then
    result := colRed
  else if s = 'magenta' then
    result := colMagenta
  else if s = 'yellow' then
    result := colYellow
  else if s = 'white' then
    result := colWhite
  else if s = 'gray' then
    result := colGray
  else if s = 'ltgray' then
    result := colLtGray
  else if s = 'dkblue' then
    result := colDkBlue
  else if s = 'dkgreen' then
    result := colDkGreen
  else if s = 'dkcyan' then
    result := colDkCyan
  else if s = 'dkred' then
    result := colDkRed
  else if s = 'dkmagenta' then
    result := colDkMagenta
  else if s = 'dkyellow' then
    result := colDkYellow
  else if s = 'maroon' then
    result := colMaroon
  else if s = 'ltgreen' then
    result := colLtGreen
  else if s = 'olive' then
    result := colOlive
  else if s = 'navy' then
    result := colNavy
  else if s = 'purple' then
    result := colPurple
  else if s = 'teal' then
    result := colTeal
  else if s = 'silver' then
    result := colSilver
  else if s = 'lime' then
    result := colLime
  else if s = 'fuchsia' then
    result := colFuchsia
  else if s = 'aqua' then
    result := colAqua
  else
    result := colTransparent;
end;

function TFPReaderXPM.DiminishWhiteSpace (s : string) : string;
var r : integer;
    Doit : boolean;
begin
  Doit := true;
  result := '';
  for r := 1 to length(s) do
    if pos(s[r],WhiteSpace)>0 then
      begin
      if DoIt then
        result := result + ' ';
      DoIt := false;
      end
    else
      begin
      DoIt := True;
      result := result + s[r];
      end;
end;

procedure TFPReaderXPM.InternalRead  (Str:TStream; Img:TFPCustomImage);
var l : TStringList;

  procedure TakeInteger (var s : string; var i : integer);
  var r : integer;
  begin
    r := pos (' ', s);
    if r = 0 then
      begin
      i := StrToInt(s);
      s := '';
      end
    else
      begin
      i := StrToInt(copy(s,1,r-1));
      delete (s, 1, r);
      end;
  end;

  procedure ParseFirstLine;
  var s : string;
  begin
    s := l[0];
    // diminish all whitespace to 1 blank
    s := DiminishWhiteSpace (trim(s));
    Takeinteger (s, width);
    Takeinteger (s, height);
    Takeinteger (s, ncols);
    Takeinteger (s, cpp);
    if s <> '' then
      begin
      Takeinteger (s, xhot);
      Takeinteger (s, yhot);
      xpmext := (comparetext(s, 'XPMEXT') = 0);
      if (s <> '') and not xpmext then
        Raise Exception.Create ('Wrong word for XPMEXT tag');
      end;
  end;

  procedure AddPalette (const code:string;const Acolor:TFPColor);
  var r : integer;
  begin
    r := Palette.Add(code);
    img.palette.Color[r] := Acolor;
  end;

  procedure AddToPalette(s : string);
  var code : string;
      c : TFPColor;
       p : integer;
  begin
    code := copy(s,1,cpp);
    s := trim(diminishWhiteSpace (copy(s,cpp+1,maxint)));
    // Search for c-key in the color values
    if s[1] = 'c' then
      delete (s, 1, 2)
    else
      begin
      p := pos (' c ',s);
      if p = 0 then
        s := ''
      else
        delete (s, 1, p+2);
      end;
    // c color value is first word, remove the rest of the line
    p := pos(' ', s);
    if p > 0 then
      delete (s, p, maxint);
    // check if exists
    if s = '' then
      raise exception.Create ('Only c-key is used for colors');
    // convert #hexadecimal value to integer and place in palette
    if s[1] = '#' then
      c := HexToColor(copy(s,2,maxint))
    else
      c := NameToColor(s);
    AddPalette(code,c);
  end;

  procedure ReadPalette;
  var r : integer;
  begin
    Palette.Clear;
    Img.Palette.Count := ncols;
    for r := 1 to ncols do
      AddToPalette (l[r]);
  end;

  procedure ReadLine (const s : string; imgindex : integer);
  var color, r, p : integer;
      code : string;
  begin
    p := 1;
    for r := 1 to width do
      begin
      code := copy(s, p, cpp);
      inc(p,cpp);
      for color := 0 to Palette.Count-1 do
        { Can't use indexof, as compare must be case sensitive }
        if code = Palette[color] then begin
          img.pixels[r-1,imgindex] := color;
          Break;
        end;
      end;
  end;

  procedure ReadData;
  var r : integer;
  begin
    for r := 1 to height do
      ReadLine (l[ncols+r], r-1);
  end;

var p, r : integer;
begin
  l := TStringList.Create;
  try
    l.LoadFromStream (Str);
    for r := l.count-1 downto 0 do
      begin
      p := pos ('"', l[r]);
      if p > 0 then
        l[r] := copy(l[r], p+1, lastdelimiter('"',l[r])-p-1)
      else
        l.delete(r);
      end;
    ParseFirstLine;
    Img.SetSize (width, height);
    Img.UsePalette := True;
    ReadPalette;
    ReadData;
  finally
    l.Free;
  end;
end;

function  TFPReaderXPM.InternalCheck (Str:TStream) : boolean;
var s : string[9];
    l : integer;
begin
  try
    l := str.Read (s[1],9);
    s[0] := char(l);
    if l <> 9 then
      result := False
    else
      result := (s = '/* XPM */');
  except
    result := false;
  end;
end;

initialization
  ImageHandlers.RegisterImageReader ('XPM Format', 'xpm', TFPReaderXPM);
end.
