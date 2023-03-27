{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    TFPPalette implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ TFPPalette }

constructor TFPPalette.create (ACount : integer);
begin
  inherited create;
  if aCount > 0 then
    getmem (FData, sizeof(TFPColor)*ACount)
  else
    FData := nil;
  FCapacity := ACount;
  SetCount (0);
end;

destructor TFPPalette.destroy;
begin
  if FCapacity > 0 then
    freemem (FData);
  inherited;
end;

procedure TFPPalette.Build (Img : TFPCustomImage);
var x,y : integer;
begin
  if (Img.Palette <> self) then
    begin
    Count := 0;
    for x := 0 to img.width-1 do
      for y := 0 to img.height-1 do
        IndexOf(img[x,y]);
    end;
end;

procedure TFPPalette.Copy(APalette: TFPPalette);
var
  x: integer;
begin
  if (APalette <> Self) then
  begin
    Self.Clear;
    for x := 0 to APalette.Count - 1 do
        Add(APalette.Color[x])
  end;
end;

procedure TFPPalette.Merge (pal : TFPPalette);
var r : integer;
begin
  for r := 0 to pal.count-1 do
    IndexOf (pal[r]);
end;

procedure TFPPalette.CheckIndex (index:integer);
begin
  if (index >= FCount) or (index < 0) then
    FPImgError (StrInvalidIndex,[ErrorText[StrPalette],index]);
end;

function TFPPalette.Add (const Value:TFPColor) : integer;
begin
  result := FCount;
  inc (FCount);
  if FCount > FCapacity then
    EnlargeData;
  FData^[result] := Value;
end;

procedure TFPPalette.SetColor (index:integer; const Value:TFPColor);
begin
  if index = FCount then
    Add (Value)
  else
    begin
    CheckIndex (index);
    FData^[index] := Value;
    end;
end;

function TFPPalette.GetColor (index:integer) : TFPColor;
begin
  CheckIndex (index);
  result := FData^[index];
end;

function TFPPalette.GetCount : integer;
begin
  result := FCount;
end;

procedure TFPPalette.EnlargeData;
var old : integer;
    NewData : PFPColorArray;
begin
  old := FCapacity;
  if FCapacity <= 16 then
    FCapacity := 32
  else if FCapacity <= 128 then
    FCapacity := 256
  else
    // MG: changed to exponential growth
    inc (FCapacity, FCapacity);
  GetMem (NewData, sizeof(TFPColor)*FCapacity);
  if old > 0 then
    begin
    move (FData^[0], NewData^[0], sizeof(TFPColor)*FCount);
    FreeMem (FData);
    end;
  FData := NewData;
end;

procedure TFPPalette.SetCount (Value:integer);
var
    O : integer;
begin
  if Value <> FCount then
    begin
    if Value > FCapacity then
      begin
        FCapacity := Value+8;
        Reallocmem(FData,sizeof(TFPColor)*FCapacity);
      end;
    for o := FCount to Value-1 do
      FData^[o] := colBlack;
    FCount := Value;
    end;
end;

procedure TFPPalette.SetCapacity (ind : Integer);
var o : Integer;
begin
  if ind<count then ind:=count;
  if ind<>fcapacity then
    begin
      fcapacity:=ind;
      Reallocmem(FData,sizeof(TFPColor)*FCapacity);
    end;
  if ind>count then
    begin
      for o := FCount to ind-1 do
        FData^[o] := colBlack;
    end;
end;

function TFPPalette.IndexOf (const AColor:TFPColor) : integer;
begin
  result := FCount;
  repeat
    dec (result);
  until (result < 0) or (FData^[result]=AColor);
  if result < 0 then
    result := Add (AColor);
end;

procedure TFPPalette.Clear;
begin
  SetCount (0);
end;


{ Functions to create standard palettes, by Giulio Bernardi 2005 }

{ A simple 1 bit black and white palette }
function CreateBlackAndWhitePalette : TFPPalette;
var fppal : TFPPalette;
    Col : TFPColor;
begin
  fppal:=TFPPalette.Create(2);
  Col.Alpha:=AlphaOpaque;
  Col.Red:=$FFFF; Col.Green:=$FFFF; Col.Blue:=$FFFF;
  fppal.Color[0]:=Col;
  Col.Red:=$0000; Col.Green:=$0000; Col.Blue:=$0000;
  fppal.Color[1]:=Col;
  Result:=fppal;
end;

{ The "standard" netscape 216-color palette (aka: web safe palette) }
function CreateWebSafePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(216);
  Col.Alpha:=AlphaOpaque;
  i:=0;
  Col.Red:=$FFFF;
  while true do
  begin
    Col.Green:=$FFFF;
    while true do
    begin
      Col.Blue:=$FFFF;
      while true do
      begin
        fppal.Color[i]:=Col;
        if Col.Blue=0 then break;
        dec(Col.Blue,$3333);
      end;
      if Col.Green=0 then break;
      dec(Col.Green,$3333);
    end;
    if Col.Red=0 then break;
    dec(Col.Red,$3333);
  end;
  Result:=fppal;
end;

{ A grayscale palette. Not very useful. }
function CreateGrayScalePalette : TFPPalette;
var Col : TFPColor;
    i : integer;
    fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(256);
  Col.Alpha:=AlphaOpaque;
  for i:=0 to $FF do
  begin
    Col.Red:=i;
    Col.Red:=(Col.Red shl 8) + Col.Red;
    Col.Green:=Col.Red;
    Col.Blue:=Col.Red;
    fppal.Color[i]:=Col;
  end;
  Result:=fppal;
end;

{ Standard VGA 16 color palette. }
function CreateVGAPalette : TFPPalette;
var fppal : TFPPalette;
begin
  fppal:=TFPPalette.Create(16);
  fppal.Color[0]:=colBlack;
  fppal.Color[1]:=colNavy;
  fppal.Color[2]:=colBlue;
  fppal.Color[3]:=colMaroon;
  fppal.Color[4]:=colPurple;
  fppal.Color[5]:=colDkGreen;
  fppal.Color[6]:=colRed;
  fppal.Color[7]:=colTeal;
  fppal.Color[8]:=colFuchsia;
  fppal.Color[9]:=colOlive;
  fppal.Color[10]:=colGray;
  fppal.Color[11]:=colLime;
  fppal.Color[12]:=colAqua;
  fppal.Color[13]:=colSilver;
  fppal.Color[14]:=colYellow;
  fppal.Color[15]:=colWhite;
  Result:=fppal;
end;
