{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{ This unit contains image format loader for X Window Pixmap images. }
unit ImagingXpm;
{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Contnrs, ImagingTypes, Imaging, ImagingUtility,
  ImagingFormats, ImagingIO, ImagingCanvases;

type
  { Class for loading X Window Pixmap images known as XPM.
    It is ASCII-text-based format, basically a fragment of C code
    declaring static array. Loaded image is in ifA8R8G8B8 data format.
    Loading as well as saving is supported now. }
  TXPMFileFormat = class(TImageFileFormat)
  protected
    procedure Define; override;
    function LoadData(Handle: TImagingHandle; var Images: TDynImageDataArray;
      OnlyFirstLevel: Boolean): Boolean; override;
    function SaveData(Handle: TImagingHandle; const Images: TDynImageDataArray;
      Index: LongInt): Boolean; override;
    procedure ConvertToSupported(var Image: TImageData;
      const Info: TImageFormatInfo); override;
  public
    function TestFormat(Handle: TImagingHandle): Boolean; override;
  end;

implementation

const
  SXPMFormatName = 'X Window Pixmap';
  SXPMMasks = '*.xpm';
  XPMSupportedFormats: TImageFormats = [ifA8R8G8B8];

const
  SXPMId = '/* XPM */';
  WhiteSpaces = [#9, #10, #13, #32];

const
  BucketCount = 257;

type
  TColorHolder = class
  public
    Color: TColor32;
  end;

  TBucketItem = record
    Key: TColor32;
    Data: string[8];
  end;

  TBucketItemArray = array of TBucketItem;

  TBucket = record
    Count: Integer;
    ItemIdxStart: Integer;
    Items: TBucketItemArray;
  end;

  TBucketArray = array of TBucket;

  { Simple color-string hash table for faster than linear searches
    during XPM saving. }
  TSimpleBucketList = class
  private
    FBuckets: TBucketArray;
    FItemCount: Integer;
    FABucket, FAIndex: Integer;
    function GetData(AKey: TColor32): string;
    procedure SetData(AKey: TColor32; const AData: string);
    function FindItem(AKey: TColor32; out ABucket, AIndex: Integer): Boolean;
  public
    constructor Create;
    procedure Add(AKey: TColor32; const AData: string);
    function Exists(AKey: TColor32): Boolean;
    function EnumNext(out AData: string): TColor32;
    property Data[AKey: TColor32]: string read GetData write SetData; default;
    property ItemCount: Integer read FItemCount;
  end;

  { TSimpleBucketList }

constructor TSimpleBucketList.Create;
begin
  SetLength(FBuckets, BucketCount);
end;

function TSimpleBucketList.GetData(AKey: TColor32): string;
var
  Bucket, Index: Integer;
begin
  Result := '';
  if FindItem(AKey, Bucket, Index) then
    Result := string(FBuckets[Bucket].Items[Index].Data);
end;

procedure TSimpleBucketList.SetData(AKey: TColor32; const AData: string);
var
  Bucket, Index: Integer;
begin
  if FindItem(AKey, Bucket, Index) then
    FBuckets[Bucket].Items[Index].Data := ShortString(AData);
end;

function TSimpleBucketList.EnumNext(out AData: string): TColor32;
begin
  // Skip empty buckets
  while FAIndex >= FBuckets[FABucket].Count do
  begin
    Inc(FABucket);
    if FABucket >= Length(FBuckets) then
      FABucket := 0;
    FAIndex := 0;
  end;

  Result := FBuckets[FABucket].Items[FAIndex].Key;
  AData := string(FBuckets[FABucket].Items[FAIndex].Data);
  Inc(FAIndex);
end;

function TSimpleBucketList.FindItem(AKey: TColor32; out ABucket,
  AIndex: Integer): Boolean;
var
  I: Integer;
  Col: TColor32Rec;
begin
  Result := False;
  Col := TColor32Rec(AKey);
  ABucket := (Col.A + 11 * Col.B + 59 * Col.R + 119 * Col.G) mod BucketCount;
  with FBuckets[ABucket] do
  for I := 0 to Count - 1 do
    if Items[I].Key = AKey then
    begin
      AIndex := I;
      Result := True;
      Break;
    end;
end;

procedure TSimpleBucketList.Add(AKey: TColor32; const AData: string);
var
  Bucket, Index, Delta, Size: Integer;
begin
  if not FindItem(AKey, Bucket, Index) then
  with FBuckets[Bucket] do
  begin
    Size := Length(Items);
    if Count = Size then
    begin
      if Size > 64 then
        Delta := Size div 4
      else
        Delta := 16;
      SetLength(Items, Size + Delta);
    end;

    with Items[Count] do
    begin
      Key := AKey;
      Data := ShortString(AData);
    end;
    Inc(Count);
    Inc(FItemCount);
  end;
end;

function TSimpleBucketList.Exists(AKey: TColor32): Boolean;
var
  Bucket, Index: Integer;
begin
  Result := FindItem(AKey, Bucket, Index);
end;

{
  TXPMFileFormat implementation
}

procedure TXPMFileFormat.Define;
begin
  inherited;
  FName := SXPMFormatName;
  FFeatures := [ffLoad, ffSave];
  FSupportedFormats := XPMSupportedFormats;

  AddMasks(SXPMMasks);
end;

function TXPMFileFormat.LoadData(Handle: TImagingHandle;
  var Images: TDynImageDataArray; OnlyFirstLevel: Boolean): Boolean;
var
  Contents, PalLookup: TStringList;
  S: AnsiString;
  I, J, NumColors, Cpp, Line: Integer;

  procedure SkipWhiteSpace(var Line: string);
  begin
    while (Length(Line) > 0) and (AnsiChar(Line[1]) in WhiteSpaces) do
      Delete(Line, 1, 1);
  end;

  function ReadString(var Line: string): string;
  begin
    Result := '';
    SkipWhiteSpace(Line);
    while (Length(Line) > 0) and not (AnsiChar(Line[1]) in WhiteSpaces) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result)] := Line[1];
      Delete(Line, 1, 1);
    end;
  end;

  function ReadInt(var Line: string): Integer;
  begin
    Result := StrToInt(ReadString(Line));
  end;

  function ParseHeader: Boolean;
  var
    S: string;
  begin
    S := Contents[0];
    try
      Images[0].Width := ReadInt(S);
      Images[0].Height := ReadInt(S);
      NumColors := ReadInt(S);
      Cpp := ReadInt(S);
      Line := 1;
      Result := True;
    except
      Result := False;
    end;
  end;

  function NamedToColor(const ColStr: string): TColor32;
  var
    S: string;
  begin
    S := LowerCase(ColStr);
    if (S = 'transparent') or (S = 'none') then
      Result := pcClear
    else if S = 'black' then
      Result := pcBlack
    else if S = 'blue' then
      Result := pcBlue
    else if S = 'green' then
      Result := pcGreen
    else if S = 'cyan' then
      Result := pcAqua
    else if S = 'red' then
      Result := pcRed
    else if S = 'magenta' then
      Result := pcFuchsia
    else if S = 'yellow' then
      Result := pcYellow
    else if S = 'white' then
      Result := pcWhite
    else if S = 'gray' then
      Result := pcLtGray
    else if S = 'dkblue' then
      Result := pcNavy
    else if S = 'dkgreen' then
      Result := pcGreen
    else if S = 'dkcyan' then
      Result := pcTeal
    else if S = 'dkred' then
      Result := pcMaroon
    else if S = 'dkmagenta' then
      Result := pcPurple
    else if S = 'dkyellow' then
      Result := pcOlive
    else if S = 'maroon' then
      Result := pcMaroon
    else if S = 'olive' then
      Result := pcOlive
    else if S = 'navy' then
      Result := pcNavy
    else if S = 'purple' then
      Result := pcPurple
    else if S = 'teal' then
      Result := pcTeal
    else if S = 'silver' then
      Result := pcSilver
    else if S = 'lime' then
      Result := pcLime
    else if S = 'fuchsia' then
      Result := pcFuchsia
    else if S = 'aqua' then
      Result := pcAqua
    else
      Result := pcClear;
  end;

  procedure ParsePalette;
  var
    I: Integer;
    S, ColType, ColStr, Code: string;
    Color: TColor32;
    Holder: TColorHolder;
  begin
    for I := 0 to NumColors - 1 do
    begin
      Holder := TColorHolder.Create;
      // Parse pixel code and color
      S := Contents[Line + I];
      Code := Copy(S, 1, Cpp);
      Delete(S, 1, Cpp);
      ColType := ReadString(S);
      ColStr := ReadString(S);
      // Convert color from hex number or named constant
      if ColStr[1] = '#' then
      begin
        Delete(ColStr, 1, 1);
        Color := UInt32(StrToInt('$' + Trim(ColStr))) or $FF000000;
      end
      else
        Color := NamedToColor(ColStr);
      // Store code and color in table for later lookup
      Holder.Color := Color;
      PalLookup.AddObject(Code, Holder);
    end;
    Inc(Line, NumColors);
  end;

  procedure ParsePixels;
  var
    X, Y, Idx: Integer;
    S, Code: string;
    Pix: PColor32;
  begin
    Pix := Images[0].Bits;
    for Y := 0 to Images[0].Height - 1 do
    begin
      S := Contents[Line + Y];
      for X := 0 to Images[0].Width - 1 do
      begin
        // Read code and look up color in the palette
        Code := Copy(S, X * Cpp + 1, Cpp);
        if PalLookup.Find(Code, Idx) then
          Pix^ := TColorHolder(PalLookup.Objects[Idx]).Color
        else
          Pix^ := pcClear;
        Inc(Pix);
      end;
    end;
  end;

begin
  Result := False;
  SetLength(Images, 1);
  with GetIO, Images[0] do
  begin
    // Look up table for XPM palette entries
    PalLookup := TStringList.Create;
    PalLookup.Sorted := True;
    PalLookup.CaseSensitive := True;
    // Read whole file and assign it to string list
    Contents := TStringList.Create;
    SetLength(S, GetInputSize(GetIO, Handle));
    Read(Handle, @S[1], Length(S));
    Contents.Text := string(S);
    // Remove quotes and other stuff
    for I := Contents.Count - 1 downto 0 do
    begin
      J := Pos('"', Contents[I]);
      if J > 0 then
        Contents[I] := Copy(Contents[I], J + 1, LastDelimiter('"', Contents[I]) - J - 1)
      else
        Contents.Delete(I);
    end;
    // Parse header and create new image
    if not ParseHeader then
      Exit;
    NewImage(Width, Height, ifA8R8G8B8, Images[0]);
    // Read palette entries and assign colors to pixels
    ParsePalette;
    ParsePixels;

    Contents.Free;
    for I := 0 to PalLookup.Count - 1 do
      PalLookup.Objects[I].Free;
    PalLookup.Free;
    Result := True;
  end;
end;

function TXPMFileFormat.SaveData(Handle: TImagingHandle;
  const Images: TDynImageDataArray; Index: LongInt): Boolean;
const
  ColorCharsCount = 92;
  ColorChars = ' .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`''][{}|';
var
  X, Y: Integer;
  ImageToSave: TImageData;
  MustBeFreed: Boolean;
  StrFile: TStringList;
  ColTable: TSimpleBucketList;
  Stream: TMemoryStream;
  Line, Id: string;
  CharsPerPixel: Integer;
  Ptr: PColor32Rec;
  ColRec: TColor32Rec;

  procedure BuildColorTables(const Img: TImageData);
  var
    I: Integer;
  begin
    Ptr := Img.Bits;
    for I := 0 to Img.Width * Img.Height - 1 do
    begin
      if not ColTable.Exists(Ptr.Color) then
        ColTable.Add(Ptr.Color, '');
      Inc(Ptr);
    end;
  end;

  procedure MakeStrIdsForColors;
  var
    I, J, K: Integer;
    Id, Data: string;
  begin
    SetLength(Id, CharsPerPixel);
    for I := 0 to ColTable.ItemCount - 1 do
    begin
      ColRec.Color := ColTable.EnumNext(Data);
      K := I;
      for J := 0 to CharsPerPixel - 1 do
      begin
        Id[J + 1] := ColorChars[K mod ColorCharsCount + 1];
        K := K div ColorCharsCount;
      end;
      ColTable.Data[ColRec.Color] := Id;
    end;
  end;

begin
  Result := False;

  StrFile := TStringList.Create;
  ColTable := TSimpleBucketList.Create;
  Stream := TMemoryStream.Create;

  if MakeCompatible(Images[Index], ImageToSave, MustBeFreed) then
  try
    // Put all unique colors of image to table
    BuildColorTables(ImageToSave);
    // Compute the character per pixel
    CharsPerPixel := 1;
    X := ColorCharsCount;
    while ColTable.ItemCount > X do
    begin
      X := X * ColorCharsCount;
      Inc(CharsPerPixel);
    end;
    // Assign char id to each color
    MakeStrIdsForColors;

    // Start writing XPM file
    StrFile.Add(SXPMId);
    StrFile.Add('static char *graphic[] = {');
    StrFile.Add('/* width height num_colors chars_per_pixel */');
    StrFile.Add(SysUtils.Format('"%d %d %d %d", ', [ImageToSave.Width,
        ImageToSave.Height, ColTable.ItemCount, CharsPerPixel]));
    StrFile.Add('/* colors */');

    // Write 'colors' part of XPM file
    for X := 0 to ColTable.ItemCount - 1 do
    begin
      ColRec.Color := ColTable.EnumNext(Id);
      if ColRec.A >= 128 then
        StrFile.Add(Format('"%s c #%.2x%.2x%.2x",', [Id, ColRec.R, ColRec.G, ColRec.B]))
      else
        StrFile.Add(Format('"%s c None",', [Id]));
    end;

    StrFile.Add('/* pixels */');

    // Write pixels - for each pixel of image find its char id
    // and append it to line
    Ptr := ImageToSave.Bits;
    for Y := 0 to ImageToSave.Height - 1 do
    begin
      Line := '';
      for X := 0 to ImageToSave.Width - 1 do
      begin
        Line := Line + ColTable.Data[Ptr.Color];
        Inc(Ptr);
      end;
      Line := '"' + Line + '"';
      if Y < ImageToSave.Height - 1 then
        Line := Line + ',';
      StrFile.Add(Line);
    end;

    StrFile.Add('};');

    // Finally save strings to stream and write stream's data to output
    // (we could directly write lines from list to output, but stream method
    // takes care of D2009+ Unicode strings).
    StrFile.SaveToStream(Stream);
    GetIO.Write(Handle, Stream.Memory, Stream.Size);

    Result := True;
  finally
    StrFile.Free;
    ColTable.Free;
    Stream.Free;
    if MustBeFreed then
      FreeImage(ImageToSave);
  end;
end;

procedure TXPMFileFormat.ConvertToSupported(var Image: TImageData;
  const Info: TImageFormatInfo);
begin
  ConvertImage(Image, ifA8R8G8B8)
end;

function TXPMFileFormat.TestFormat(Handle: TImagingHandle): Boolean;
var
  Id: array[0 .. 8] of AnsiChar;
  ReadCount: Integer;
begin
  Result := False;
  if Handle <> nil then
  begin
    ReadCount := GetIO.Read(Handle, @Id, SizeOf(Id));
    GetIO.Seek(Handle, -ReadCount, smFromCurrent);
    Result := (Id = SXPMId) and (ReadCount = SizeOf(Id));
  end;
end;

initialization

RegisterImageFileFormat(TXPMFileFormat);

{
  File Notes:

  -- TODOS ----------------------------------------------------
  - nothing now

  -- 0.26.3 Changes/Bug Fixes -----------------------------------
  - Added XPM saving.

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
  - Added XPM loading.
  - Unit created.
}

end.


