{%MainUnit fpimage.pp}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team
       
    Compact images (images with less than 64-bit depth) support, by Mattias Gaertner
           
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
    
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    
 **********************************************************************}

function GetFPCompactImgDesc(Gray: boolean; Depth: word; HasAlpha: boolean
  ): TFPCompactImgDesc;
begin
  Result.Gray:=Gray;
  Result.Depth:=Depth;
  Result.HasAlpha:=HasAlpha;
end;

function GetFPCompactImgClass(const Desc: TFPCompactImgDesc): TFPCompactImgBaseClass;
begin
  if Desc.Gray then begin
    if Desc.HasAlpha then begin
      // gray, alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgGrayAlpha8Bit
      else
        Result:=TFPCompactImgGrayAlpha16Bit;
    end else begin
      // gray, no alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgGray8Bit
      else
        Result:=TFPCompactImgGray16Bit;
    end;
  end else begin
    // RGB
    if Desc.HasAlpha then begin
      // RGB, alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgRGBA8Bit
      else
        Result:=TFPCompactImgRGBA16Bit;
    end else begin
      // RGB, no alpha
      if Desc.Depth<=8 then
        Result:=TFPCompactImgRGB8Bit
      else
        Result:=TFPCompactImgRGB16Bit;
    end;
  end;
end;

function CreateFPCompactImg(const Desc: TFPCompactImgDesc; Width, Height: integer
  ): TFPCustomImage;
var
  ImgClass: TFPCompactImgBaseClass;
begin
  ImgClass:=GetFPCompactImgClass(Desc);
  Result:=ImgClass.Create(Width,Height);
end;

function CreateCompatibleFPCompactImg(Img: TFPCustomImage; Width, Height: integer
  ): TFPCustomImage;
begin
  if Img is TFPCompactImgBase then
    Result:=CreateFPCompactImg(TFPCompactImgBase(Img).Desc,Width,Height)
  else
    Result:=CreateFPCompactImg(GetMinimumPTDesc(Img),Width,Height);
end;

function CreateCompatibleFPCompactImgWithAlpha(Img: TFPCustomImage; Width,
  Height: integer): TFPCustomImage;
var
  Desc: TFPCompactImgDesc;
begin
  if Img is TFPCompactImgBase then
    Desc:=TFPCompactImgBase(Img).Desc
  else
    Desc:=GetMinimumPTDesc(Img);
  Desc.HasAlpha:=true;
  Result:=CreateFPCompactImg(Desc,Width,Height);
end;

function GetMinimumPTDesc(Img: TFPCustomImage; FuzzyDepth: word = 4): TFPCompactImgDesc;
var
  AllLoEqualsHi, AllLoAre0: Boolean;
  FuzzyMaskLoHi: Word;

  procedure Need16Bit(c: word); inline;
  var
    l: Byte;
  begin
    c:=c and FuzzyMaskLoHi;
    l:=Lo(c);
    AllLoAre0:=AllLoAre0 and (l=0);
    AllLoEqualsHi:=AllLoEqualsHi and (l=Hi(c));
  end;

var
  TestGray: Boolean;
  TestAlpha: Boolean;
  Test16Bit: Boolean;
  BaseImg: TFPCompactImgBase;
  ImgDesc: TFPCompactImgDesc;
  y: Integer;
  x: Integer;
  col: TFPColor;
  FuzzyMaskWord: Word;
  FuzzyOpaque: Word;
begin
  TestGray:=true;
  TestAlpha:=true;
  Test16Bit:=FuzzyDepth<8;
  Result.HasAlpha:=false;
  Result.Gray:=true;
  Result.Depth:=8;
  if Img is TFPCompactImgBase then begin
    BaseImg:=TFPCompactImgBase(Img);
    ImgDesc:=BaseImg.Desc;
    if ImgDesc.Depth<=8 then Test16Bit:=false;
    if ImgDesc.Gray then TestGray:=false;
    if not ImgDesc.HasAlpha then TestAlpha:=false;
  end;

  if (not TestGray) and (not TestAlpha) and (not Test16Bit) then exit;

  FuzzyMaskWord:=Word($ffff) shl FuzzyDepth;
  FuzzyOpaque:=alphaOpaque and FuzzyMaskWord;
  FuzzyMaskLoHi:=Word(lo(FuzzyMaskWord))+(Word(lo(FuzzyMaskWord)) shl 8);
  AllLoAre0:=true;
  AllLoEqualsHi:=true;
  for y:=0 to Img.Height-1 do begin
    for x:=0 to Img.Width-1 do begin
      col:=Img.Colors[x,y];
      if TestAlpha and ((col.alpha and FuzzyMaskWord)<>FuzzyOpaque) then begin
        TestAlpha:=false;
        Result.HasAlpha:=true;
        if (not TestGray) and (not Test16Bit) then break;
      end;
      if TestGray
      and ((col.red and FuzzyMaskWord)<>(col.green and FuzzyMaskWord))
      or ((col.red and FuzzyMaskWord)<>(col.blue and FuzzyMaskWord)) then begin
        TestGray:=false;
        Result.Gray:=false;
        if (not TestAlpha) and (not Test16Bit) then break;
      end;
      if Test16Bit then begin
        Need16Bit(col.red);
        Need16Bit(col.green);
        Need16Bit(col.blue);
        Need16Bit(col.alpha);
        if (not AllLoAre0) and (not AllLoEqualsHi) then begin
          Test16Bit:=false;
          Result.Depth:=16;
          if (not TestAlpha) and (not TestGray) then break;
        end;
      end;
    end;
  end;
end;

function GetMinimumFPCompactImg(Img: TFPCustomImage; FreeImg: boolean;
  FuzzyDepth: word = 4): TFPCustomImage;
var
  Desc: TFPCompactImgDesc;
  ImgClass: TFPCompactImgBaseClass;
  y: Integer;
  x: Integer;
begin
  Desc:=GetMinimumPTDesc(Img,FuzzyDepth);
  ImgClass:=GetFPCompactImgClass(Desc);
  if Img.ClassType=ImgClass then
    exit(Img);
  Result:=CreateFPCompactImg(Desc,Img.Width,Img.Height);
  for y:=0 to Img.Height-1 do
    for x:=0 to Img.Width-1 do
      Result.Colors[x,y]:=Img.Colors[x,y];
  if FreeImg then
    Img.Free;
end;

function ColorRound (c : double) : word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;

{ TFPCompactImgGrayAlpha16Bit }

function TFPCompactImgGrayAlpha16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgGrayAlpha16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=v.a;
end;

function TFPCompactImgGrayAlpha16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgGrayAlpha16BitValue;
begin
  v.g:=Value.red;
  v.a:=Value.alpha;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TFPCompactImgGrayAlpha16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGrayAlpha16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGrayAlpha16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgGrayAlpha16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TFPCompactImgGrayAlpha8Bit }

function TFPCompactImgGrayAlpha8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgGrayAlpha8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.g shl 8)+v.g;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TFPCompactImgGrayAlpha8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgGrayAlpha8BitValue;
begin
  v.g:=Value.red shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetInternalPixel(x, y: integer; Value: integer
  );
begin

end;

constructor TFPCompactImgGrayAlpha8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGrayAlpha8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGrayAlpha8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgGrayAlpha8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth, AHeight);
end;

{ TFPCompactImgGray16Bit }

function TFPCompactImgGray16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgGray16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGray16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red;
end;

procedure TFPCompactImgGray16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgGray16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGray16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGray16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Word)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgGray8Bit }

function TFPCompactImgGray8Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result.red:=FData[x+y*Width];
  Result.red:=(Word(Result.red) shl 8)+Result.red;
  Result.green:=Result.red;
  Result.blue:=Result.red;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgGray8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgGray8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value.red shr 8;
end;

procedure TFPCompactImgGray8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgGray8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(true,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgGray8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgGray8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(Byte)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGBA8Bit }

function TFPCompactImgRGBA8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGBA8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=(v.a shl 8)+v.a;
end;

function TFPCompactImgRGBA8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGBA8Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgRGBA8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  v.a:=Value.alpha shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGBA8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGBA8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,8,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGBA8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGBA8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGBA8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGB8Bit }

function TFPCompactImgRGB8Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGB8BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=(v.r shl 8)+v.r;
  Result.green:=(v.g shl 8)+v.g;
  Result.blue:=(v.b shl 8)+v.b;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgRGB8Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGB8Bit.SetInternalColor(x, y: integer; const Value: TFPColor
  );
var
  v: TFPCompactImgRGB8BitValue;
begin
  v.r:=Value.red shr 8;
  v.g:=Value.green shr 8;
  v.b:=Value.blue shr 8;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGB8Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGB8Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,8,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGB8Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGB8Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGB8BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGB16Bit }

function TFPCompactImgRGB16Bit.GetInternalColor(x, y: integer): TFPColor;
var
  v: TFPCompactImgRGB16BitValue;
begin
  v:=FData[x+y*Width];
  Result.red:=v.r;
  Result.green:=v.g;
  Result.blue:=v.b;
  Result.alpha:=alphaOpaque;
end;

function TFPCompactImgRGB16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGB16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
var
  v: TFPCompactImgRGB16BitValue;
begin
  v.r:=Value.red;
  v.g:=Value.green;
  v.b:=Value.blue;
  FData[x+y*Width]:=v;
end;

procedure TFPCompactImgRGB16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGB16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,16,false);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGB16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGB16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPCompactImgRGB16BitValue)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;

{ TFPCompactImgRGBA16Bit }

function TFPCompactImgRGBA16Bit.GetInternalColor(x, y: integer): TFPColor;
begin
  Result:=FData[x+y*Width];
end;

function TFPCompactImgRGBA16Bit.GetInternalPixel(x, y: integer): integer;
begin
  Result:=0;
end;

procedure TFPCompactImgRGBA16Bit.SetInternalColor(x, y: integer;
  const Value: TFPColor);
begin
  FData[x+y*Width]:=Value;
end;

procedure TFPCompactImgRGBA16Bit.SetInternalPixel(x, y: integer; Value: integer);
begin

end;

constructor TFPCompactImgRGBA16Bit.Create(AWidth, AHeight: integer);
begin
  FDesc:=GetFPCompactImgDesc(false,16,true);
  inherited Create(AWidth, AHeight);
end;

destructor TFPCompactImgRGBA16Bit.Destroy;
begin
  ReAllocMem(FData,0);
  inherited Destroy;
end;

procedure TFPCompactImgRGBA16Bit.SetSize(AWidth, AHeight: integer);
begin
  if (AWidth=Width) and (AHeight=Height) then exit;
  ReAllocMem(FData,SizeOf(TFPColor)*AWidth*AHeight);
  inherited SetSize(AWidth,AHeight);
end;


