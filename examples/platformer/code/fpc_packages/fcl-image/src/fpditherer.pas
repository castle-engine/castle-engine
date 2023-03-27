{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2005 by Giulio Bernardi

    This file contains classes used to dither images.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{$mode objfpc}{$h+}
unit FPDitherer;

interface

uses sysutils, classes, fpimage, fpcolhash;

type
  FPDithererException = class (exception);

type
  TFPDithererProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte;
                                         const Msg: AnsiString; var Continue : Boolean) of object;

type
  TFPBaseDitherer = class
    private
      FPalette : TFPPalette;
      FOnProgress : TFPDithererProgressEvent;
      procedure QuickSort(const l, r : integer);
    protected
      FImage : TFPCustomImage;
      FHashMap : TFPColorHashTable;
      FSorted : boolean;
      FUseHash : boolean;
      FUseAlpha : boolean;
      function ColorCompare(const c1, c2 : TFPColor) : shortint;
      function GetColorDinst(const c1, c2 : TFPColor) : integer;
      function SubtractColorInt(const c1, c2 : TFPColor) : int64;
      function SubtractColor(const c1, c2 : TFPColor) : TFPColor;
      procedure InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage); virtual;
      function FindBestColor(OrigColor : TFPColor; var PalIndex : integer) : integer; virtual;
      procedure Progress (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean); virtual;
      procedure SetUseHash(Value : boolean); virtual;
      procedure SetSorted(Value : boolean); virtual;
    public
      property OnProgress : TFPDithererProgressEvent read FOnProgress write FOnProgress;
      property Palette : TFPPalette read FPalette;
      property PaletteSorted : boolean read FSorted write SetSorted;
      property UseHashMap : boolean read FUseHash write SetUseHash;
      property UseAlpha : boolean read FUseAlpha write FUseAlpha;
      procedure Dither(const Source : TFPCustomImage; Dest : TFPCustomImage);
      procedure SortPalette; virtual;
      constructor Create(ThePalette : TFPPalette); virtual;
      destructor Destroy; override;
  end;

type
  PFPPixelReal = ^TFPPixelReal;
  TFPPixelReal = record   { pixel in real form }
    a, r, g, b : real;
  end;

  PFSPixelLine = ^TFSPixelLine;
  TFSPixelLine = record
    pixels : PFPPixelReal;             { a line of pixels }
    Next : PFSPixelLine;               { next line of pixels }
  end;

type
  TFPFloydSteinbergDitherer = class(TFPBaseDitherer)
    private
      Lines : PFSPixelLine;
      function Color2Real(const c : TFPColor) : TFPPixelReal;
      function Real2Color(r : TFPPixelReal) : TFPColor;
      procedure CreatePixelLine(var line : PFSPixelLine; const row : integer);
      function GetError(const c1, c2 : TFPColor) : TFPPixelReal;
      procedure DistributeErrors(var line : PFSPixelLine; const row : integer; Img : TFPCustomImage);
      procedure DeleteAllPixelLines(var line : PFSPixelLine);
    protected
      procedure InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage); override;
    public
      constructor Create(ThePalette : TFPPalette); override;
  end;

implementation

{ TFPBaseDitherer }

procedure TFPBaseDitherer.Dither(const Source : TFPCustomImage; Dest : TFPCustomImage);
begin
  if FPalette.Count=0 then
    raise FPDithererException.Create('Palette is empty');
  if Source=Dest then
    raise FPDithererException.Create('Source and Destination images must be different');
  InternalDither(Source,Dest);
  if FUseHash then
    FHashMap.Clear;
end;

constructor TFPBaseDitherer.Create(ThePalette : TFPPalette);
begin
  FSorted:=false;
  FUseAlpha:=false;
  FImage:=nil;
  FPalette:=ThePalette;
  FUseHash:=true;
  FHashMap:=TFPColorHashTable.Create;
end;

destructor TFPBaseDitherer.Destroy;
begin
  if Assigned(FHashMap) then
    FHashMap.Free;
end;

procedure TFPBaseDitherer.SetUseHash(Value : boolean);
begin
  if Value=FUseHash then exit;
  if Value then
    FHashMap:=TFPColorHashTable.Create
  else
  begin
    FHashMap.Free;
    FHashMap:=nil;
  end;
  FUseHash:=Value;
end;

procedure TFPBaseDitherer.SetSorted(Value : boolean);
begin
  FSorted:=Value;
end;

procedure TFPBaseDitherer.Progress(Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender,Stage,PercentDone,Msg,Continue);
end;

{ rgb triplets are considered like a number having msb in msb(r) and lsb in lsb(b) }

function TFPBaseDitherer.SubtractColorInt(const c1, c2 : TFPColor) : int64;
var whole1, whole2 : int64;
begin
  whole1:= ((c1.Red and $FF00) shl 8) or (c1.Green and $FF00) or ((c1.Blue and $FF00) shr 8);
  whole2:= ((c2.Red and $FF00) shl 8) or (c2.Green and $FF00) or ((c2.Blue and $FF00) shr 8);
  if FUseAlpha then
  begin
    whole1:=whole1 or ((c1.Alpha and $FF00) shl 16);
    whole2:=whole2 or ((c2.Alpha and $FF00) shl 16);
  end;
  Result:= whole1 - whole2;
end;

{ this is more efficient than calling subtractcolorint and then extracting r g b values }
function TFPBaseDitherer.GetColorDinst(const c1, c2 : TFPColor) : integer;
var dinst : integer;
begin
  dinst:=abs(((c1.Red and $FF00) shr 8) - ((c2.Red and $FF00) shr 8));
  dinst:=dinst+abs(((c1.Green and $FF00) shr 8) - ((c2.Green and $FF00) shr 8));
  dinst:=dinst+abs(((c1.Blue and $FF00) shr 8) - ((c2.Blue and $FF00) shr 8));
  if FUseAlpha then
    dinst:=dinst+abs(((c1.Alpha and $FF00) shr 8) - ((c2.Alpha and $FF00) shr 8));
  Result:= dinst;
end;

function TFPBaseDitherer.SubtractColor(const c1, c2 : TFPColor) : TFPColor;
var whole : int64;
begin
  whole:=abs(SubtractColorInt(c1,c2));
  if FUseALpha then
    Result.Alpha:=(whole and $FF000000) shr 16
  else
    Result.Alpha:=AlphaOpaque;
  Result.Red:=(whole and $00FF0000) shr 8;
  Result.Green:=(whole and $0000FF00);
  Result.Blue:=(whole and $000000FF) shl 8;
end;

function TFPBaseDitherer.ColorCompare(const c1, c2 : TFPColor) : shortint;
var whole : int64;
begin
  whole:=SubtractColorInt(c1,c2);
  if whole>0 then Result:=1
  else if whole<0 then Result:=-1
  else Result:=0;
end;

procedure TFPBaseDitherer.QuickSort(const l, r : integer);
var i, j : integer;
    pivot, temp : TFPColor;
begin
  if l<r then
  begin
    pivot:=FPalette[l];
    i:=l+1;
    j:=r;
    repeat
      while ((i<=r) and (ColorCompare(FPalette[i],pivot)<=0)) do
        inc(i);
      while (ColorCompare(FPalette[j],pivot)=1) do
        dec(j);
      if i<j then
      begin
        temp:=FPalette[i];
        FPalette[i]:=FPalette[j];
        FPalette[j]:=temp;
      end;
    until i > j;
    { don't swap if they are equal }
    if ColorCompare(FPalette[j],pivot)<>0 then
    begin
      Fpalette[l]:=Fpalette[j];
      Fpalette[j]:=pivot;
    end;
    Quicksort(l,j-1);
    Quicksort(i,r);
  end;
end;

procedure TFPBaseDitherer.SortPalette;
begin
  QuickSort(0,FPalette.Count-1);
  FSorted:=true;
end;

type
  PBestColorData = ^TBestColorData;
  TBestColorData = record
    palindex, dinst : integer;
  end;

function TFPBaseDitherer.FindBestColor(OrigColor : TFPColor; var PalIndex : integer) : integer;
var i, curr, dinst, tmpdinst, top, bottom : integer;
    hashval : PBestColorData;
begin
  dinst:=$7FFFFFFF;
  curr:=0;

  if FUseHash then { use the hashmap to improve speed }
  begin
    hashval:=FHashMap.Get(OrigColor);
    if hashval<>nil then
    begin
      PalIndex:=hashval^.palindex;
      Result:=hashval^.dinst;
      exit;
    end;
  end;

  { with a sorted palette, proceed by binary search. this is more efficient with large images or large palettes }
  if FSorted then 
  begin
    top:=0;
    bottom:=FPalette.Count-1;
    while top<=bottom do
    begin
      i:=(bottom+top) div 2;
      tmpdinst:=ColorCompare(OrigColor,Fpalette[i]);
      if tmpdinst<0 then bottom:=i-1
      else if tmpdinst>0 then top:=i+1
      else break; { we found it }
    end;
    curr:=i;
    dinst:=GetColorDinst(OrigColor,Fpalette[i]);
  end
  else
    for i:=0 to FPalette.Count-1 do
    begin
      tmpdinst:=GetColorDinst(OrigColor,FPalette[i]);
      if tmpdinst<dinst then
      begin
        dinst:=tmpdinst;
        curr:=i;
      end;
      if tmpdinst=0 then break; { There can't be anything better, stop searching }
    end;

  if FUseHash then { if we are using a hashmap, remember this value}
  begin
    hashval:=GetMem(sizeof(TBestColorData));
    if hashval=nil then
      raise FPDithererException.Create('Out of memory');
    hashval^.PalIndex:=curr;
    hashval^.dinst:=dinst;
    FHashMap.Insert(OrigColor,hashval);
  end;
  PalIndex:=curr;
  Result:=dinst;
end;

procedure TFPBaseDitherer.InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage);
var i,j, palindex : integer;
    percent : byte;
    percentinterval : longword;
    percentacc : longword;
    FContinue : boolean;
begin
  FImage:=Source;
  percent:=0;
  percentinterval:=(FImage.Width*FImage.Height*4) div 100;
  if percentinterval=0 then percentinterval:=$FFFFFFFF;
  percentacc:=0;
  FContinue:=true;
  Progress (self,psStarting,0,'',FContinue);
  Dest.SetSize(0,0);
  Dest.UsePalette:=true;
  Dest.Palette.Clear;
  Dest.Palette.Merge(FPalette);
  Dest.SetSize(FImage.Width,FImage.Height);
  for j:=0 to FImage.Height-1 do
    for i:=0 to FImage.Width-1 do
    begin
      FindBestColor(FImage[i,j], palindex);
      Dest.Pixels[i,j]:=palindex;
      inc(percentacc,4);
      if percentacc>=percentinterval then
      begin
        percent:=percent+(percentacc div percentinterval);
        percentacc:=percentacc mod percentinterval;
        Progress (self,psRunning,percent,'',FContinue);
        if not fcontinue then exit;
      end;
    end;
  Progress (self,psEnding,100,'',FContinue);
end;

{ TFPFloydSteinbergDitherer }

const FSNullPixel : TFPPixelReal = (a : 0.0; r : 0.0; g : 0.0; b : 0.0);

constructor TFPFloydSteinbergDitherer.Create(ThePalette : TFPPalette);
begin
  inherited Create(ThePalette);
  Lines:=nil;
end;

function TFPFloydSteinbergDitherer.GetError(const c1, c2 : TFPColor) : TFPPixelReal;
var temp : TFPPixelReal;
begin
  if FUseAlpha then
    temp.a:=((c1.Alpha and $FF00) shr 8) - ((c2.Alpha and $FF00) shr 8);
  temp.r:=((c1.Red and $FF00) shr 8) - ((c2.Red and $FF00) shr 8);
  temp.g:=((c1.Green and $FF00) shr 8) - ((c2.Green and $FF00) shr 8);
  temp.b:=((c1.Blue and $FF00) shr 8) - ((c2.Blue and $FF00) shr 8);
  Result:=temp;
end;

function TFPFloydSteinbergDitherer.Color2Real(const c : TFPColor) : TFPPixelReal;
var temp : TFPPixelReal;
begin
  if FUseAlpha then
    temp.a:=((c.Alpha and $FF00) shr 8);
  temp.r:=((c.Red and $FF00) shr 8);
  temp.g:=((c.Green and $FF00) shr 8);
  temp.b:=((c.Blue and $FF00) shr 8);
  Result:=temp;
end;

function TFPFloydSteinbergDitherer.Real2Color(r : TFPPixelReal) : TFPColor;
var temp : TFPColor;
begin
  { adjust overflows and underflows }
  if r.r> 255 then r.r:=255; if r.r<0 then r.r:=0;
  if r.g> 255 then r.g:=255; if r.g<0 then r.g:=0;
  if r.b> 255 then r.b:=255; if r.b<0 then r.b:=0;
  if FUseAlpha then
  begin
    if r.a> 255 then r.a:=255; if r.a<0 then r.a:=0;
  end;

  temp.Red:=round(r.r);
  temp.Red:=(temp.Red shl 8) + temp.Red;
  temp.Green:=round(r.g);
  temp.Green:=(temp.Green shl 8) + temp.Green;
  temp.Blue:=round(r.b);
  temp.Blue:=(temp.Blue shl 8) + temp.Blue;
  if FUseAlpha then
  begin
    temp.Alpha:=round(r.a);
    temp.Alpha:=(temp.Alpha shl 8) + temp.Alpha;
  end
  else
    temp.Alpha:=AlphaOpaque;
  Result:=temp;
end;

procedure TFPFloydSteinbergDitherer.CreatePixelLine(var line : PFSPixelLine; const row : integer);
var i : integer;
begin
  line:=GetMem(sizeof(TFSPixelLine));
  if line=nil then
    raise FPDithererException.Create('Out of memory');
  line^.next:=nil;
  { two extra pixels so we don't have to check if the pixel is on start or end of line  }
  getmem(line^.pixels,sizeof(TFPPixelReal)*(FImage.Width+2));
  if line^.pixels=nil then
    raise FPDithererException.Create('Out of memory');
  if row<FImage.Height-1 then
  begin
    line^.pixels[0]:=FSNullPixel;
    line^.pixels[FImage.Width+1]:=FSNullPixel;
    for i:=0 to FImage.Width-1 do
      line^.pixels[i+1]:=Color2Real(FImage[i,row]);
  end
  else
    for i:=0 to FImage.Width+1 do
      line^.pixels[i]:=FSNullPixel;
end;

const e716 = 0.4375;
      e516 = 0.3125;
      e316 = 0.1875;
      e116 = 0.0625;

procedure TFPFloydSteinbergDitherer.DistributeErrors(var line : PFSPixelLine; const row : integer; Img : TFPCustomImage);
var i, width : integer;
    palindex : integer;
    OldColor : TFPColor;
    dir : shortint;
    nextline : PFSPixelLine;
begin
  width:=FImage.Width;
  if (row mod 2)=0 then
  begin
    dir:=1;
    i:=1;
  end
  else
  begin
    dir:=-1;
    i:=width;
  end;
  if width<1 then exit;

  repeat
    OldColor:=Real2Color(line^.pixels[i]);
    FindBestColor(OldColor, palindex);
    Img.Pixels[i-1,row]:=palindex; { we use this color for this pixel... }
    line^.pixels[i]:=GetError(OldColor,Palette[palindex]);
    { now distribute this error to the other pixels, in this way: }
    { note: for odd lines this is mirrored and we start from right}
    {    0      0      0  }
    {    0      X    7/16 }
    {  3/16   5/16   1/16 }
    line^.pixels[i+dir].r:=line^.pixels[i+dir].r+(line^.pixels[i].r*e716);
    line^.pixels[i+dir].g:=line^.pixels[i+dir].g+(line^.pixels[i].g*e716);
    line^.pixels[i+dir].b:=line^.pixels[i+dir].b+(line^.pixels[i].b*e716);
    if FUseAlpha then
      line^.pixels[i+dir].a:=line^.pixels[i+dir].a+(line^.pixels[i].a*e716);
    nextline:=line^.next;

    nextline^.pixels[i].r:=nextline^.pixels[i].r+(line^.pixels[i].r*e516);
    nextline^.pixels[i].g:=nextline^.pixels[i].g+(line^.pixels[i].g*e516);
    nextline^.pixels[i].b:=nextline^.pixels[i].b+(line^.pixels[i].b*e516);
    if FUseAlpha then
      nextline^.pixels[i].a:=nextline^.pixels[i].a+(line^.pixels[i].a*e516);

    nextline^.pixels[i+dir].r:=nextline^.pixels[i+dir].r+(line^.pixels[i].r*e116);
    nextline^.pixels[i+dir].g:=nextline^.pixels[i+dir].g+(line^.pixels[i].g*e116);
    nextline^.pixels[i+dir].b:=nextline^.pixels[i+dir].b+(line^.pixels[i].b*e116);
    if FUseAlpha then
      nextline^.pixels[i+dir].a:=nextline^.pixels[i+dir].a+(line^.pixels[i].a*e116);

    nextline^.pixels[i-dir].r:=nextline^.pixels[i-dir].r+(line^.pixels[i].r*e316);
    nextline^.pixels[i-dir].g:=nextline^.pixels[i-dir].g+(line^.pixels[i].g*e316);
    nextline^.pixels[i-dir].b:=nextline^.pixels[i-dir].b+(line^.pixels[i].b*e316);
    if FUseAlpha then
      nextline^.pixels[i-dir].a:=nextline^.pixels[i-dir].a+(line^.pixels[i].a*e316);

    i:=i+dir;
  until ((i<1) or (i>width));
end;

procedure TFPFloydSteinbergDitherer.DeleteAllPixelLines(var line : PFSPixelLine);
var tmp : PFSPixelLine;
begin
  while line<>nil do
  begin
    tmp:=line^.next;
    FreeMem(line^.pixels);
    FreeMem(line);
    line:=tmp;
  end;
end;

procedure TFPFloydSteinbergDitherer.InternalDither(const Source : TFPCustomImage; Dest : TFPCustomImage);
var i : integer;
    tmpline : PFSPixelLine;
    percent : byte;
    percentinterval : longword;
    percentacc : longword;
    FContinue : boolean;
begin
  FImage:=Source;
  if FImage.Height=0 then exit;
  Dest.SetSize(0,0);
  try
    Dest.UsePalette:=true;
    Dest.Palette.Clear;
    Dest.Palette.Merge(FPalette);
    Dest.SetSize(FImage.Width,FImage.Height);
    percent:=0;
    percentinterval:=(FImage.Height*4) div 100;
    if percentinterval=0 then percentinterval:=$FFFFFFFF;
    percentacc:=0;
    FContinue:=true;
    Progress (self,psStarting,0,'',FContinue);
    if not FContinue then exit;
    CreatePixelLine(Lines,0);
    CreatePixelLine(Lines^.next,1);

    for i:=0 to FImage.Height-1 do
    begin
      DistributeErrors(Lines, i, Dest);
      tmpline:=Lines;
      Lines:=Lines^.next;
      FreeMem(tmpline^.pixels);
      FreeMem(tmpline);
      CreatePixelLine(Lines^.next,i+2);
      inc(percentacc,4);
      if percentacc>=percentinterval then
      begin
        percent:=percent+(percentacc div percentinterval);
        percentacc:=percentacc mod percentinterval;
        Progress (self,psRunning,percent,'',FContinue);
        if not FContinue then exit;
      end;
    end;
    Progress (self,psEnding,100,'',FContinue);
  finally
    DeleteAllPixelLines(lines);
  end;
end;


end.