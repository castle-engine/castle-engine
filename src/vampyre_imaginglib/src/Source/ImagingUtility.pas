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

{ This unit contains utility functions and types for Imaging library.}
unit ImagingUtility;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Types;

const
  STrue = 'True';
  SFalse = 'False';

type
{$IF Defined(DELPHI)}
  {$IF not Defined(UInt32)}
    UInt32 = Cardinal;
  {$IFEND}
  {$IF not Defined(PUInt32)}
    PUInt32 = ^UInt32;
  {$IFEND}
{$IFEND}

  TByteArray = array[0..MaxInt - 1] of Byte;
  PByteArray = ^TByteArray;
  TWordArray = array[0..MaxInt div 2 - 1] of Word;
  PWordArray = ^TWordArray;
  TUInt32Array = array[0..MaxInt div 4 - 1] of UInt32;
  PUInt32Array = ^TUInt32Array;
  TInt64Array = array[0..MaxInt div 8 - 1] of Int64;
  PInt64Array = ^TInt64Array;
  TSingleArray = array[0..MaxInt div 4 - 1] of Single;
  PSingleArray = ^TSingleArray;
  TBooleanArray = array[0..MaxInt - 1] of Boolean;
  PBooleanArray = ^TBooleanArray;

  TDynByteArray = array of Byte;
  TDynIntegerArray = array of Integer;
  TDynBooleanArray = array of Boolean;
  TDynStringArray = array of string;

  TWordRec = packed record
    case Integer of
      0: (WordValue: Word);
      1: (Low, High: Byte);
  end;
  PWordRec = ^TWordRec;
  TWordRecArray = array[0..MaxInt div 2 - 1] of TWordRec;
  PWordRecArray = ^TWordRecArray;

  TUInt32Rec = packed record
    case Integer of
      0: (UInt32Value: UInt32);
      1: (Low, High: Word);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..1] of Word);
      3: (Bytes: array[0..3] of Byte);
  end;
  PUInt32Rec = ^TUInt32Rec;
  TUInt32RecArray = array[0..MaxInt div 4 - 1] of TUInt32Rec;
  PUInt32RecArray = ^TUInt32RecArray;

  TInt64Rec = packed record
    case Integer of
      0: (Int64Value: Int64);
      1: (Low, High: UInt32);
      { Array variants - Index 0 means lowest significant byte (word, ...).}
      2: (Words: array[0..3] of Word);
      3: (Bytes: array[0..7] of Byte);
  end;
  PInt64Rec = ^TInt64Rec;
  TInt64RecArray = array[0..MaxInt div 8 - 1] of TInt64Rec;
  PInt64RecArray = ^TInt64RecArray;

  TFloatHelper = record
    Data: Int64;
    case Integer of
      0: (Data64: Int64);
      1: (Data32: UInt32);
  end;
  PFloatHelper = ^TFloatHelper;

  TFloatPoint = record
    X, Y: Single;
  end;

  TFloatRect = record
    Left, Top, Right, Bottom: Single;
  end;

  TChar2 = array[0..1] of AnsiChar;
  TChar3 = array[0..2] of AnsiChar;
  TChar4 = array[0..3] of AnsiChar;
  TChar8 = array[0..7] of AnsiChar;
  TChar16 = array[0..15] of AnsiChar;
  TAnsiCharSet = set of AnsiChar;

  ENotImplemented = class(Exception)
  public
    constructor Create;
  end;

  { Options for BuildFileList function:
    flFullNames - file names in result will have full path names
                (ExtractFileDir(Path) + FileName)
    flRelNames  - file names in result will have names relative to
                ExtractFileDir(Path) dir
    flRecursive - adds files in subdirectories found in Path.}
  TFileListOption = (flFullNames, flRelNames, flRecursive);
  TFileListOptions = set of TFileListOption;


{ Frees class instance and sets its reference to nil.}
procedure FreeAndNil(var Obj);
{ Frees pointer and sets it to nil.}
procedure FreeMemNil(var P); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Replacement of standard System.FreeMem procedure which checks if P is nil
  (this is only needed for Free Pascal, Delphi makes checks in its FreeMem).}
procedure FreeMem(P: Pointer); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns current exception object. Do not call outside exception handler.}
function GetExceptObject: Exception; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns time value with microsecond resolution.}
function GetTimeMicroseconds: Int64;
{ Returns time value with millisecond resolution.}
function GetTimeMilliseconds: Int64;

{ Returns file extension (without "." dot)}
function GetFileExt(const FileName: string): string;
{ Returns file name of application's executable.}
function GetAppExe: string;
{ Returns directory where application's executable is located without
  path delimiter at the end.}
function GetAppDir: string;
{ Works like SysUtils.ExtractFileName but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileName supports on default delimiter on current platform).}
function GetFileName(const FileName: string): string;
{ Works like SysUtils.ExtractFileDir but supports '/' and '\' dir delimiters
  at the same time (whereas ExtractFileDir supports on default delimiter on current platform).}
function GetFileDir(const FileName: string): string;
{ Returns True if Subject matches given Mask with optional case sensitivity.
  Mask can contain ? and * special characters: ? matches
  one character, * matches zero or more characters.}
function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean = False): Boolean;
{ This function fills Files string list with names of files found
  with FindFirst/FindNext functions (See details on Path/Atrr here).
  - BuildFileList('c:\*.*', faAnyFile, List, [flRecursive]) returns
    list of all files (only name.ext - no path) on C drive
  - BuildFileList('d:\*.*', faDirectory, List, [flFullNames]) returns
    list of all directories (d:\dirxxx) in root of D drive.}
function BuildFileList(Path: string; Attr: LongInt; Files: TStrings;
  Options: TFileListOptions = []): Boolean;
{ Similar to RTL's Pos function but with optional Offset where search will start.
  In recent FPC and Delphi XE3+ regular SysUtils.Pos has the Offset parameter as well.
  This function is in the RTL StrUtils unit, it's here to depend on additional
  unit for just this one function. }
function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
{ Same as PosEx but without case sensitivity.}
function PosNoCase(const SubStr, S: string; Offset: LongInt = 1): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns a sub-string from S which is followed by
  Sep separator and deletes the sub-string from S including the separator.}
function StrToken(var S: string; Sep: Char): string;
{ Same as StrToken but searches from the end of S string.}
function StrTokenEnd(var S: string; Sep: Char): string;
{ Fills instance of TStrings with tokens from string S where tokens are separated by
  one of Seps characters.}
procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
{ Returns string representation of integer number (with digit grouping).
  Uses current locale.}
function IntToStrFmt(const I: Int64): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns string representation of float number (with digit grouping).
  Uses current locale.}
function FloatToStrFmt(const F: Double; Precision: Integer = 2): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns format settings for parsing floats (dot as decimal separator).
  Useful when formatting/parsing floats etc.}
function GetFormatSettingsForFloats: TFormatSettings;
{ Returns True if S contains at least one of the substrings in SubStrs array. Case sensitive.}
function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
{ Extracts substring starting at IdxStart ending at IdxEnd.
  S[IdxEnd] is not included in the result.}
function SubString(const S: string; IdxStart, IdxEnd: Integer): string; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Similar to Trim() but removes only characters in a given set.
  Part of FPC RTL here for Delphi compatibility. }
function TrimSet(const S: string; const CharSet: TSysCharSet): string;
{ Similar to TrimLeft() but removes only characters in a given set.
  Part of FPC RTL here for Delphi compatibility. }
function TrimLeftSet(const S: string; const CharSet:TSysCharSet): string;

{ Clamps integer value to range <Min, Max>}
function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps float value to range <Min, Max>}
function ClampFloat(Number: Single; Min, Max: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Byte boundaries.}
function ClampToByte(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clamps integer value to Word boundaries.}
function ClampToWord(Value: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns True if Num is power of 2.}
function IsPow2(Num: LongInt): Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns next power of 2 greater than or equal to Num
  (if Num itself is power of 2 then it returns Num).}
function NextPow2(Num: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises 2 to the given integer power (in range [0, 30]).}
function Pow2Int(Exponent: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Raises Base to any power.}
function Power(const Base, Exponent: Single): Single;
{ Returns log base 2 of integer X (max 2^30) or -1 if X is not power of 2.}
function Log2Int(X: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns log base 2 of X.}
function Log2(X: Single): Single;
{ Returns log base 10 of X.}
function Log10(X: Single): Single;
{ Returns largest integer <= Val (for 5.9 returns 5).}
function Floor(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns smallest integer >= Val (for 5.1 returns 6).}
function Ceil(Value: Single): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two integer numbers.}
function Min(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns lesser of two float numbers.}
function MinFloat(A, B: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two integer numbers.}
function Max(A, B: LongInt): LongInt; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two float numbers.}
function MaxFloat(A, B: Single): Single; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns greater of two float numbers.}
function MaxFloat(const A, B: Double): Double; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns result from multiplying Number by Numerator and then dividing by Denominator.
  Denominator must be greater than 0.}
function MulDiv(Number, Numerator, Denominator: Word): Word; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns true if give floats are the equal within given delta.}
function SameFloat(A, B: Single; Delta: Single = 0.001): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns true if give floats are the equal within given delta.}
function SameFloat(const A, B: Double; const Delta: Double = 0.000001): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Switches Boolean value.}
procedure Switch(var Value: Boolean); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Integer): Integer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function IffUnsigned(Condition: Boolean; TruePart, FalsePart: Cardinal): Cardinal; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition, TruePart, FalsePart: Boolean): Boolean; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: string): string; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ If Condition is True then TruePart is returned, otherwise
  FalsePart is returned.}
function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps two Boolean values}
procedure SwapValues(var A, B: Boolean); overload;
{ Swaps two Byte values}
procedure SwapValues(var A, B: Byte); overload;
{ Swaps two Word values}
procedure SwapValues(var A, B: Word); overload;
{ Swaps two Integer values}
procedure SwapValues(var A, B: Integer); overload;
{$IFDEF LONGINT_IS_NOT_INTEGER}
{ Swaps two LongInt values}
procedure SwapValues(var A, B: LongInt); overload;
{$ENDIF}
{ Swaps two Single values}
procedure SwapValues(var A, B: Single); overload;
{ Swaps two values if necessary to ensure that Min <= Max.}
procedure SwapMin(var Min, Max: LongInt); {$IFDEF USE_INLINE}inline;{$ENDIF}
{ This function returns True if running on little endian machine.}
function IsLittleEndian: Boolean; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of Word value.}
function SwapEndianWord(Value: Word): Word; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple Word values.}
procedure SwapEndianWord(P: PWordArray; Count: LongInt); overload;
{ Swaps byte order of UInt32 value.}
function SwapEndianUInt32(Value: UInt32): UInt32; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Swaps byte order of multiple UInt32 values.}
procedure SwapEndianUInt32(P: PUInt32; Count: LongInt); overload;

{ Calculates CRC32 for the given data.}
procedure CalcCrc32(var Crc: UInt32; Data: Pointer; Size: LongInt);
{ Fills given memory with given Byte value. Size is size of buffer in bytes.}
procedure FillMemoryByte(Data: Pointer; Size: LongInt; Value: Byte);
{ Fills given memory with given Word value. Size is size of buffer in bytes.}
procedure FillMemoryWord(Data: Pointer; Size: LongInt; Value: Word);
{ Fills given memory with given UInt32 value. Size is size of buffer in bytes.}
procedure FillMemoryUInt32(Data: Pointer; Size: LongInt; Value: UInt32);
{ Fills given memory zeroes.}
{$EXTERNALSYM ZeroMemory} // Conflicts with WinAPI ZeroMemory in C++ Builder
procedure ZeroMemory(Data: Pointer; Size: Integer); {$IFDEF USE_INLINE}inline;{$ENDIF}

{ Returns how many mipmap levels can be created for image of given size.}
function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
{ Returns total number of levels of volume texture with given depth and
  mipmap count (this is not depth * mipmaps!).}
function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;
{ Returns rectangle (X, Y, X + Width, Y + Height).}
function BoundsToRect(X, Y, Width, Height: LongInt): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Left + R.Right, R.Top + R.Bottom).}
function BoundsToRect(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Returns rectangle (R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top).}
function RectToBounds(const R: TRect): TRect; overload; {$IFDEF USE_INLINE}inline;{$ENDIF}
{ Clips given bounds to Clip rectangle.}
procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);
{ Clips given source bounds and dest position. It is used by various CopyRect
  functions that copy rect from one image to another. It handles clipping the same way
  as Win32 BitBlt function. }
procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt;
  SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);
{ Clips given source bounds and dest bounds. It is used by various StretchRect
  functions that stretch rectangle of pixels from one image to another.
  It handles clipping the same way as Win32 StretchBlt function. }
procedure ClipStretchBounds(var SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY,
  DstWidth, DstHeight: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);
{ Scales one rectangle to fit into another. Proportions are preserved so
  it could be used for 'Stretch To Fit Window' image drawing for instance.}
function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
{ Scales given size to fit into max size while keeping the original aspect ratio.
  Useful for calculating thumbnail dimensions etc.}
function ScaleSizeToFit(const CurrentSize, MaxSize: TSize): TSize;
{ Returns width of given rect. Part of RTL in newer Delphi.}
function RectWidth(const Rect: TRect): Integer;
{ Returns height of given rect. Part of RTL in newer Delphi.}
function RectHeight(const Rect: TRect): Integer;
{ Returns True if R1 fits into R2.}
function RectInRect(const R1, R2: TRect): Boolean;
{ Returns True if R1 and R2 intersects.}
function RectIntersects(const R1, R2: TRect): Boolean;
{ Ensures that rect's right>left and bottom>top. }
procedure NormalizeRect(var R: TRect);

{ Converts pixel size in micrometers to corresponding DPI.}
function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
{ Converts DPI to corresponding pixel size in micrometers.}
function DpiToPixelSize(Dpi: Single): Single;

function FloatPoint(AX, AY: Single): TFloatPoint; {$IFDEF USE_INLINE}inline;{$ENDIF}
function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
function FloatRectWidth(const R: TFloatRect): Single;
function FloatRectHeight(const R: TFloatRect): Single;
function FloatRectFromRect(const R: TRect): TFloatRect;

{ Formats given message for usage in Exception.Create(..). Use only
  in except block - returned message contains message of last raised exception.}
function FormatExceptMsg(const Msg: string; const Args: array of const): string;
{ Outputs debug message - shows message dialog in Windows and writes to console
  in Linux/Unix.}
procedure DebugMsg(const Msg: string; const Args: array of const);

implementation

uses
{$IF Defined(MSWINDOWS)}
  Windows;
{$ELSEIF Defined(FPC)}
  Dos, BaseUnix, Unix;
{$ELSEIF Defined(DELPHI)}
  Posix.SysTime;
{$IFEND}

var
  FloatFormatSettings: TFormatSettings;

constructor ENotImplemented.Create;
begin
  inherited Create('Not implemented');
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

procedure FreeMemNil(var P);
begin
  FreeMem(Pointer(P));
  Pointer(P) := nil;
end;

procedure FreeMem(P: Pointer);
begin
  if P <> nil then
    System.FreeMem(P);
end;

function GetExceptObject: Exception;
begin
  Result := Exception(ExceptObject);
end;

{$IF Defined(MSWINDOWS)}
var
  PerfFrequency: Int64;
  InvPerfFrequency: Extended;

function GetTimeMicroseconds: Int64;
var
  Time: Int64;
begin
  QueryPerformanceCounter(Time);
  Result := Round(1000000 * InvPerfFrequency * Time);
end;
{$ELSEIF Defined(DELPHI)}
function GetTimeMicroseconds: Int64;
var
  Time: TimeVal;
begin
  Posix.SysTime.GetTimeOfDay(Time, nil);
  Result := Int64(Time.tv_sec) * 1000000 + Time.tv_usec;
end;
{$ELSEIF Defined(FPC)}
function GetTimeMicroseconds: Int64;
var
  TimeVal: TTimeVal;
begin
  fpGetTimeOfDay(@TimeVal, nil);
  Result := Int64(TimeVal.tv_sec) * 1000000 + TimeVal.tv_usec;
end;
{$IFEND}

function GetTimeMilliseconds: Int64;
begin
  Result := GetTimeMicroseconds div 1000;
end;

function GetFileExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if Length(Result) > 1 then
    Delete(Result, 1, 1);
end;

function GetAppExe: string;
{$IF Defined(MSWINDOWS)}
var
  FileName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, FileName,
    Windows.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSEIF Defined(DELPHI)} // Delphi non Win targets
var
  FileName: array[0..1024] of Char;
begin
  SetString(Result, FileName,
    System.GetModuleFileName(MainInstance, FileName, SizeOf(FileName)));
{$ELSE}
begin
  Result := ExpandFileName(ParamStr(0));
{$IFEND}
end;

function GetAppDir: string;
begin
  Result := ExtractFileDir(GetAppExe);
end;

function GetFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('\/' + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function GetFileDir(const FileName: string): string;
const
  Delims = '\/' + DriveDelim;
var
  I: Integer;
begin
  I := LastDelimiter(Delims, Filename);
  if (I > 1) and
    ((FileName[I] = Delims[1]) or (FileName[I] = Delims[2])) and
    (not IsDelimiter(Delims, FileName, I - 1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;

function StrMaskMatch(const Subject, Mask: string; CaseSensitive: Boolean): Boolean;
var
  MaskLen, KeyLen : LongInt;

  function CharMatch(A, B: Char): Boolean;
  begin
    if CaseSensitive then
      Result := A = B
    else
      Result := AnsiUpperCase (A) = AnsiUpperCase (B);
  end;

  function MatchAt(MaskPos, KeyPos: LongInt): Boolean;
  begin
    while (MaskPos <= MaskLen) and (KeyPos <= KeyLen) do
    begin
      case Mask[MaskPos] of
        '?' :
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
        '*' :
          begin
            while (MaskPos <= MaskLen) and (Mask[MaskPos] = '*') do
              Inc(MaskPos);
            if MaskPos > MaskLen then
            begin
              Result := True;
              Exit;
            end;
            repeat
              if MatchAt(MaskPos, KeyPos) then
              begin
                Result := True;
                Exit;
              end;
              Inc(KeyPos);
            until KeyPos > KeyLen;
            Result := False;
            Exit;
          end;
        else
          if not CharMatch(Mask[MaskPos], Subject[KeyPos]) then
          begin
            Result := False;
            Exit;
          end
          else
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
      end;
    end;

    while (MaskPos <= MaskLen) and (AnsiChar(Mask[MaskPos]) in ['?', '*']) do
      Inc(MaskPos);
    if (MaskPos <= MaskLen) or (KeyPos <= KeyLen) then
    begin
      Result := False;
      Exit;
    end;

    Result := True;
  end;

begin
  MaskLen := Length(Mask);
  KeyLen := Length(Subject);
  if MaskLen = 0 then
  begin
    Result := True;
    Exit;
  end;
  Result := MatchAt(1, 1);
end;

function BuildFileList(Path: string; Attr: LongInt;
  Files: TStrings; Options: TFileListOptions): Boolean;
var
  FileMask: string;
  RootDir: string;
  Folders: TStringList;
  CurrentItem: LongInt;
  Counter: LongInt;
  LocAttr: LongInt;

  procedure BuildFolderList;
  var
    FindInfo: TSearchRec;
    Rslt: LongInt;
  begin
    Counter := Folders.Count - 1;
    CurrentItem := 0;
    while CurrentItem <= Counter do
    begin
      // Searching for subfolders
      Rslt := SysUtils.FindFirst(Folders[CurrentItem] + '*', faDirectory, FindInfo);
      try
        while Rslt = 0 do
        begin
          if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') and
            (FindInfo.Attr and faDirectory = faDirectory) then
            Folders.Add(Folders[CurrentItem] + FindInfo.Name + PathDelim);
          Rslt := SysUtils.FindNext(FindInfo);
        end;
      finally
        SysUtils.FindClose(FindInfo);
      end;
      Counter := Folders.Count - 1;
      Inc(CurrentItem);
    end;
  end;

  procedure FillFileList(CurrentCounter: LongInt);
  var
    FindInfo: TSearchRec;
    Res: LongInt;
    CurrentFolder: string;
  begin
    CurrentFolder := Folders[CurrentCounter];
    Res := SysUtils.FindFirst(CurrentFolder + FileMask, LocAttr, FindInfo);
    if flRelNames in Options then
      CurrentFolder := ExtractRelativePath(RootDir, CurrentFolder);
    try
      while Res = 0 do
      begin
        if (FindInfo.Name <> '.') and (FindInfo.Name <> '..') then
        begin
          if (flFullNames in Options) or (flRelNames in Options) then
            Files.Add(CurrentFolder + FindInfo.Name)
          else
            Files.Add(FindInfo.Name);
        end;
        Res := SysUtils.FindNext(FindInfo);
      end;
    finally
      SysUtils.FindClose(FindInfo);
    end;
  end;

begin
  FileMask := ExtractFileName(Path);
  RootDir := ExtractFilePath(Path);
  Folders := TStringList.Create;
  Folders.Add(RootDir);
  Files.Clear;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  if Attr = faAnyFile then
    LocAttr := faSysFile or faHidden or faArchive or faReadOnly
  else
    LocAttr := Attr;
{$IFDEF DCC}
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  // Here's the recursive search for nested folders
  if flRecursive in Options then
    BuildFolderList;
  if Attr <> faDirectory then
    for Counter := 0 to Folders.Count - 1 do
      FillFileList(Counter)
  else
    Files.AddStrings(Folders);
  Folders.Free;
  Result := True;
end;

function PosEx(const SubStr, S: string; Offset: LongInt = 1): LongInt;
var
  I, X: LongInt;
  Len, LenSubStr: LongInt;
begin
  I := Offset;
  LenSubStr := Length(SubStr);
  Len := Length(S) - LenSubStr + 1;
  while I <= Len do
  begin
    if S[I] = SubStr[1] then
    begin
      X := 1;
      while (X < LenSubStr) and (S[I + X] = SubStr[X + 1]) do
        Inc(X);
      if (X = LenSubStr) then
      begin
        Result := I;
        Exit;
      end;
    end;
    Inc(I);
  end;
  Result := 0;
end;

function PosNoCase(const SubStr, S: string; Offset: LongInt): LongInt;
begin
  Result := PosEx(AnsiLowerCase(SubStr), AnsiLowerCase(S), Offset);
end;

function StrToken(var S: string; Sep: Char): string;
var
  I: LongInt;
begin
  I := Pos(Sep, S);
  if I <> 0 then
  begin
    Result := Copy(S, 1, I - 1);
    Delete(S, 1, I);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function StrTokenEnd(var S: string; Sep: Char): string;
var
  I, J: LongInt;
begin
  J := 0;
  I := Pos(Sep, S);
  while I <> 0 do
  begin
    J := I;
    I := PosEx(Sep, S, J + 1);
  end;
  if J <> 0 then
  begin
    Result := Copy(S, J + 1, MaxInt);
    Delete(S, J, MaxInt);
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

procedure StrTokensToList(const S: string; Sep: Char; Tokens: TStrings);
var
  Token, Str: string;
begin
  Tokens.Clear;
  Str := S;
  while Str <> '' do
  begin
    Token := StrToken(Str, Sep);
    Tokens.Add(Token);
  end;
end;

function IntToStrFmt(const I: Int64): string;
begin
  Result := Format('%.0n', [I * 1.0]);
end;

function FloatToStrFmt(const F: Double; Precision: Integer): string;
begin
  Result := Format('%.' + IntToStr(Precision) + 'n', [F]);
end;

function GetFormatSettingsForFloats: TFormatSettings;
begin
  Result := FloatFormatSettings;
end;

function ContainsAnySubStr(const S: string; const SubStrs: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(SubStrs) do
  begin
    Result := Pos(SubStrs[I], S) > 0;
    if Result then
      Exit;
  end;
end;

function SubString(const S: string; IdxStart, IdxEnd: Integer): string;
begin
  Result := Copy(S, IdxStart, IdxEnd - IdxStart);
end;

function TrimSet(const S: string; const CharSet: TSysCharSet): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] in CharSet) do
    Inc(I);
  if I > L then
    Result := ''
  else
  begin
    while S[L] in CharSet do
      Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimLeftSet(const S: string; const CharSet: TSysCharSet): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] in CharSet) do
    Inc(I);
  Result := Copy(S, I, MaxInt);
end;

function ClampInt(Number: LongInt; Min, Max: LongInt): LongInt;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampFloat(Number: Single; Min, Max: Single): Single;
begin
  Result := Number;
  if Result < Min then
    Result := Min
  else if Result > Max then
    Result := Max;
end;

function ClampToByte(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 255 then
    Result := 255
  else if Result < 0 then
    Result := 0;
end;

function ClampToWord(Value: LongInt): LongInt;
begin
  Result := Value;
  if Result > 65535 then
    Result := 65535
  else if Result < 0 then
    Result := 0;
end;

function IsPow2(Num: LongInt): Boolean;
begin
  Result := (Num and -Num) = Num;
end;

function NextPow2(Num: LongInt): LongInt;
begin
  Result := Num and -Num;
  while Result < Num do
    Result := Result shl 1;
end;

function Pow2Int(Exponent: LongInt): LongInt;
begin
  Result := 1 shl Exponent;
end;

function Power(const Base, Exponent: Single): Single;
begin
  if Exponent = 0.0 then
    Result := 1.0
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0
  else
    Result := Exp(Exponent * Ln(Base));
end;

function Log2Int(X: LongInt): LongInt;
begin
  case X of
    1: Result := 0;
    2: Result := 1;
    4: Result := 2;
    8: Result := 3;
    16: Result := 4;
    32: Result := 5;
    64: Result := 6;
    128: Result := 7;
    256: Result := 8;
    512: Result := 9;
    1024: Result := 10;
    2048: Result := 11;
    4096: Result := 12;
    8192: Result := 13;
    16384: Result := 14;
    32768: Result := 15;
    65536: Result := 16;
    131072: Result := 17;
    262144: Result := 18;
    524288: Result := 19;
    1048576: Result := 20;
    2097152: Result := 21;
    4194304: Result := 22;
    8388608: Result := 23;
    16777216: Result := 24;
    33554432: Result := 25;
    67108864: Result := 26;
    134217728: Result := 27;
    268435456: Result := 28;
    536870912: Result := 29;
    1073741824: Result := 30;
  else
    Result := -1;
  end;
end;

function Log2(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLD1
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln2: Single = 0.6931471;
begin
  Result := Ln(X) / Ln2;
end;
{$ENDIF}

function Log10(X: Single): Single;
{$IFDEF USE_ASM}
asm
  FLDLG2
  FLD     X
  FYL2X
  FWAIT
end;
{$ELSE}
const
  Ln10: Single = 2.30258509299405;
begin
  Result := Ln(X) / Ln10;
end;
{$ENDIF}

function Floor(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Value < Result then
    Dec(Result);
end;

function Ceil(Value: Single): LongInt;
begin
  Result := Trunc(Value);
  if Value > Result then
    Inc(Result);
end;

procedure Switch(var Value: Boolean);
begin
  Value := not Value;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffUnsigned(Condition: Boolean; TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition, TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function Iff(Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

procedure SwapValues(var A, B: Boolean);
var
  Tmp: Boolean;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Byte);
var
  Tmp: Byte;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Word);
var
  Tmp: Word;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapValues(var A, B: Integer);
var
  Tmp: Integer;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

{$IFDEF LONGINT_IS_NOT_INTEGER}
procedure SwapValues(var A, B: LongInt);
var
  Tmp: LongInt;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;
{$ENDIF}

procedure SwapValues(var A, B: Single);
var
  Tmp: Single;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure SwapMin(var Min, Max: LongInt);
var
  Tmp: LongInt;
begin
  if Min > Max then
  begin
    Tmp := Min;
    Min := Max;
    Max := Tmp;
  end;
end;

function Min(A, B: LongInt): LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function MinFloat(A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(A, B: LongInt): LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxFloat(A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MaxFloat(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function MulDiv(Number, Numerator, Denominator: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
         MUL DX
         DIV CX
end;
{$ELSE}
begin
  Result := Number * Numerator div Denominator;
end;
{$IFEND}

function SameFloat(A, B: Single; Delta: Single): Boolean;
begin
  Result := Abs(A - B) <= Delta;
end;

function SameFloat(const A, B: Double; const Delta: Double): Boolean;
begin
  Result := Abs(A - B) <= Delta;
end;

function IsLittleEndian: Boolean;
var
  W: Word;
begin
  W := $00FF;
  Result := PByte(@W)^ = $FF;
end;

function SwapEndianWord(Value: Word): Word;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  XCHG   AH, AL
end;
{$ELSE}
begin
  TWordRec(Result).Low := TWordRec(Value).High;
  TWordRec(Result).High := TWordRec(Value).Low;
end;
{$IFEND}

procedure SwapEndianWord(P: PWordArray; Count: LongInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    CX, [EAX]
  XCHG   CH, CL
  MOV    [EAX], CX
  ADD    EAX, 2
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: LongInt;
  Temp: Word;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := P[I];
    TWordRec(P[I]).Low := TWordRec(Temp).High;
    TWordRec(P[I]).High := TWordRec(Temp).Low;
  end;
end;
{$ENDIF}

function SwapEndianUInt32(Value: UInt32): UInt32;
{$IF Defined(USE_ASM) and (not Defined(USE_INLINE))}
asm
  BSWAP   EAX
end;
{$ELSE}
begin
  TUInt32Rec(Result).Bytes[0] := TUInt32Rec(Value).Bytes[3];
  TUInt32Rec(Result).Bytes[1] := TUInt32Rec(Value).Bytes[2];
  TUInt32Rec(Result).Bytes[2] := TUInt32Rec(Value).Bytes[1];
  TUInt32Rec(Result).Bytes[3] := TUInt32Rec(Value).Bytes[0];
end;
{$IFEND}

procedure SwapEndianUInt32(P: PUInt32; Count: LongInt);
{$IFDEF USE_ASM}
asm
@Loop:
  MOV    ECX, [EAX]
  BSWAP  ECX
  MOV    [EAX], ECX
  ADD    EAX, 4
  DEC    EDX
  JNZ    @Loop
end;
{$ELSE}
var
  I: LongInt;
  Temp: UInt32;
begin
  for I := 0 to Count - 1 do
  begin
    Temp := PUInt32Array(P)[I];
    TUInt32Rec(PUInt32Array(P)[I]).Bytes[0] := TUInt32Rec(Temp).Bytes[3];
    TUInt32Rec(PUInt32Array(P)[I]).Bytes[1] := TUInt32Rec(Temp).Bytes[2];
    TUInt32Rec(PUInt32Array(P)[I]).Bytes[2] := TUInt32Rec(Temp).Bytes[1];
    TUInt32Rec(PUInt32Array(P)[I]).Bytes[3] := TUInt32Rec(Temp).Bytes[0];
  end;
end;
{$ENDIF}

type
  TCrcTable = array[Byte] of UInt32;
var
  CrcTable: TCrcTable;

procedure InitCrcTable;
const
  Polynom = $EDB88320;
var
  I, J: LongInt;
  C: UInt32;
begin
  for I := 0 to 255 do
  begin
    C := I;
    for J := 0 to 7 do
    begin
      if (C and $01) <> 0 then
        C := Polynom xor (C shr 1)
      else
        C := C shr 1;
    end;
    CrcTable[I] := C;
  end;
end;

procedure CalcCrc32(var Crc: UInt32; Data: Pointer; Size: LongInt);
var
  I: LongInt;
  B: PByte;
begin
  B := Data;
  for I := 0 to Size - 1 do
  begin
    Crc := (Crc shr 8) xor CrcTable[B^ xor Byte(Crc)];
    Inc(B);
  end
end;

procedure FillMemoryByte(Data: Pointer; Size: LongInt; Value: Byte);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    AH, AL
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SAR    ECX, 2
  JS     @Exit
  REP    STOSD
  MOV    ECX, EDX
  AND    ECX, 3
  REP    STOSB
  POP    EDI
@Exit:
end;
{$ELSE}
begin
  FillChar(Data^, Size, Value);
end;
{$ENDIF}

procedure FillMemoryWord(Data: Pointer; Size: LongInt; Value: Word);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    CX, AX
  SHL    EAX, 16
  MOV    AX, CX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I, V: UInt32;
begin
  V := Value * $10000 + Value;
  for I := 0 to Size div 4 - 1 do
    PUInt32Array(Data)[I] := V;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := Lo(Value);
    2: PWordArray(Data)[Size div 2] := Value;
    3:
      begin
        PWordArray(Data)[Size  div 2 - 1] := Value;
        PByteArray(Data)[Size - 1] := Lo(Value);
      end;
  end;
end;
{$ENDIF}

procedure FillMemoryUInt32(Data: Pointer; Size: LongInt; Value: UInt32);
{$IFDEF USE_ASM}
asm
  PUSH   EDI
  PUSH   EBX
  MOV    EBX, EDX
  MOV    EDI, EAX
  MOV    EAX, ECX
  MOV    ECX, EDX
  SHR    ECX, 2
  JZ     @Word
  REP    STOSD
@Word:
  MOV    ECX, EBX
  AND    ECX, 2
  JZ     @Byte
  MOV    [EDI], AX
  ADD    EDI, 2
@Byte:
  MOV    ECX, EBX
  AND    ECX, 1
  JZ     @Exit
  MOV    [EDI], AL
@Exit:
  POP    EBX
  POP    EDI
end;
{$ELSE}
var
  I: LongInt;
begin
  for I := 0 to Size div 4 - 1 do
    PUInt32Array(Data)[I] := Value;
  case Size mod 4 of
    1: PByteArray(Data)[Size - 1] := TUInt32Rec(Value).Bytes[0];
    2: PWordArray(Data)[Size div 2] := TUInt32Rec(Value).Words[0];
    3:
      begin
        PWordArray(Data)[Size div 2 - 1] := TUInt32Rec(Value).Words[0];
        PByteArray(Data)[Size - 1] := TUInt32Rec(Value).Bytes[0];
      end;
  end;
end;
{$ENDIF}

procedure ZeroMemory(Data: Pointer; Size: Integer);
begin
  FillMemoryByte(Data, Size, 0);
end;

function GetNumMipMapLevels(Width, Height: LongInt): LongInt;
begin
  Result := 0;
  if (Width > 0) and (Height > 0) then
  begin
    Result := 1;
    while (Width <> 1) or (Height <> 1) do
    begin
      Width := Width div 2;
      Height := Height div 2;
      if Width < 1 then Width := 1;
      if Height < 1 then Height := 1;
      Inc(Result);
    end;
  end;
end;

function GetVolumeLevelCount(Depth, MipMaps: LongInt): LongInt;
var
  I: LongInt;
begin
  Result := Depth;
  for I := 1 to MipMaps - 1 do
    Inc(Result, ClampInt(Depth shr I, 1, Depth));
end;

function BoundsToRect(X, Y, Width, Height: LongInt): TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;

function BoundsToRect(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Left + R.Right;
  Result.Bottom := R.Top + R.Bottom;
end;

function RectToBounds(const R: TRect): TRect;
begin
  Result.Left := R.Left;
  Result.Top := R.Top;
  Result.Right := R.Right - R.Left;
  Result.Bottom := R.Bottom - R.Top;
end;

procedure ClipRectBounds(var X, Y, Width, Height: LongInt; const Clip: TRect);

  procedure ClipDim(var AStart, ALength: LongInt; ClipMin, ClipMax: LongInt);
  begin
    if AStart < ClipMin then
    begin
      ALength := ALength - (ClipMin - AStart);
      AStart := ClipMin;
    end;
    if AStart + ALength > ClipMax then ALength := Max(0, ClipMax - AStart);
  end;

begin
  ClipDim(X, Width, Clip.Left, Clip.Right);
  ClipDim(Y, Height, Clip.Top, Clip.Bottom);
end;

procedure ClipCopyBounds(var SrcX, SrcY, Width, Height, DstX, DstY: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, Size: LongInt; SrcClipMax,
    DstClipMin, DstClipMax: LongInt);
  var
    OldDstPos: LongInt;
    Diff: LongInt;
  begin
    OldDstPos := Iff(DstPos < 0, DstPos, 0);
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      Size := Size - Diff;
      SrcPos := SrcPos + Diff;
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      Size := Size + SrcPos - OldDstPos;
      DstPos := DstPos - SrcPos + OldDstPos;
      SrcPos := 0;
    end;
    if SrcPos + Size > SrcClipMax then Size := SrcClipMax - SrcPos;
    if DstPos + Size > DstClipMax then Size := DstClipMax - DstPos;
  end;

begin
  ClipDim(SrcX, DstX, Width, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, Height, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

procedure ClipStretchBounds(var SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY,
  DstWidth, DstHeight: LongInt; SrcImageWidth, SrcImageHeight: LongInt; const DstClip: TRect);

  procedure ClipDim(var SrcPos, DstPos, SrcSize, DstSize: LongInt; SrcClipMax,
    DstClipMin, DstClipMax: LongInt);
  var
    OldSize: LongInt;
    Diff: LongInt;
    Scale: Single;
  begin
    Scale := DstSize / SrcSize;
    if DstPos < DstClipMin then
    begin
      Diff := DstClipMin - DstPos;
      DstSize := DstSize - Diff;
      SrcPos := SrcPos + Round(Diff / Scale);
      SrcSize := SrcSize - Round(Diff / Scale);
      DstPos := DstClipMin;
    end;
    if SrcPos < 0 then
    begin
      SrcSize := SrcSize + SrcPos;
      DstPos := DstPos - Round(SrcPos * Scale);
      DstSize := DstSize + Round(SrcPos * Scale);
      SrcPos := 0;
    end;
    if SrcPos + SrcSize > SrcClipMax then
    begin
      OldSize := SrcSize;
      SrcSize := SrcClipMax - SrcPos;
      DstSize := Round(DstSize * (SrcSize / OldSize));
    end;
    if DstPos + DstSize > DstClipMax then
    begin
      OldSize := DstSize;
      DstSize := DstClipMax - DstPos;
      SrcSize := Round(SrcSize * (DstSize / OldSize));
    end;
  end;

begin
  ClipDim(SrcX, DstX, SrcWidth, DstWidth, SrcImageWidth, DstClip.Left, DstClip.Right);
  ClipDim(SrcY, DstY, SrcHeight, DstHeight, SrcImageHeight, DstClip.Top, DstClip.Bottom);
end;

function ScaleRectToRect(const SourceRect, TargetRect: TRect): TRect;
var
  SourceWidth: LongInt;
  SourceHeight: LongInt;
  TargetWidth: LongInt;
  TargetHeight: LongInt;
  ScaledWidth: LongInt;
  ScaledHeight: LongInt;
begin
  SourceWidth := SourceRect.Right - SourceRect.Left;
  SourceHeight := SourceRect.Bottom - SourceRect.Top;
  TargetWidth := TargetRect.Right - TargetRect.Left;
  TargetHeight := TargetRect.Bottom - TargetRect.Top;

  if SourceWidth * TargetHeight < SourceHeight * TargetWidth then
  begin
    ScaledWidth := (SourceWidth * TargetHeight) div SourceHeight;
    Result := BoundsToRect(TargetRect.Left + ((TargetWidth - ScaledWidth) div 2),
      TargetRect.Top, ScaledWidth, TargetHeight);
  end
  else
  begin
    ScaledHeight := (SourceHeight * TargetWidth) div SourceWidth;
    Result := BoundsToRect(TargetRect.Left, TargetRect.Top + ((TargetHeight - ScaledHeight) div 2),
      TargetWidth, ScaledHeight);
  end;
end;

function ScaleSizeToFit(const CurrentSize, MaxSize: Types.TSize): Types.TSize;
var
  SR, TR, ScaledRect: TRect;
begin
  SR := Types.Rect(0, 0, CurrentSize.CX, CurrentSize.CY);
  TR := Types.Rect(0, 0, MaxSize.CX, MaxSize.CY);
  ScaledRect := ScaleRectToRect(SR, TR);
  Result.CX := ScaledRect.Right - ScaledRect.Left;
  Result.CY := ScaledRect.Bottom - ScaledRect.Top;
end;

function RectWidth(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

function RectInRect(const R1, R2: TRect): Boolean;
begin
  Result:=
    (R1.Left >= R2.Left) and
    (R1.Top >= R2.Top) and
    (R1.Right <= R2.Right) and
    (R1.Bottom <= R2.Bottom);
end;

function RectIntersects(const R1, R2: TRect): Boolean;
begin
  Result :=
    not (R1.Left > R2.Right) and
    not (R1.Top > R2.Bottom) and
    not (R1.Right < R2.Left) and
    not (R1.Bottom < R2.Top);
end;

procedure NormalizeRect(var R: TRect);
begin
  if R.Right < R.Left then
    SwapValues(R.Right, R.Left);
  if R.Bottom < R.Top then
    SwapValues(R.Bottom, R.Top);
end;

function PixelSizeToDpi(SizeInMicroMeters: Single): Single;
begin
  Result := 25400 / SizeInMicroMeters;
end;

function DpiToPixelSize(Dpi: Single): Single;
begin
  Result := 1e03 / (Dpi / 25.4);
end;

function FloatPoint(AX, AY: Single): TFloatPoint;
begin
  Result.X := AX;
  Result.Y := AY;
end;

function FloatRect(ALeft, ATop, ARight, ABottom: Single): TFloatRect;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function FloatRectWidth(const R: TFloatRect): Single;
begin
  Result := R.Right - R.Left;
end;

function FloatRectHeight(const R: TFloatRect): Single;
begin
  Result := R.Bottom - R.Top;
end;

function FloatRectFromRect(const R: TRect): TFloatRect;
begin
  Result := FloatRect(R.Left, R.Top, R.Right, R.Bottom);
end;

function FormatExceptMsg(const Msg: string; const Args: array of const): string;
begin
  Result := Format(Msg + SLineBreak + 'Message: ' + GetExceptObject.Message, Args);
end;

procedure DebugMsg(const Msg: string; const Args: array of const);
var
  FmtMsg: string;
begin
  FmtMsg := Format(Msg, Args);
{$IFDEF MSWINDOWS}
  if IsConsole then
    WriteLn('DebugMsg: ' + FmtMsg)
  else
    MessageBox(GetActiveWindow, PChar(FmtMsg), 'DebugMsg', MB_OK);
{$ENDIF}
{$IFDEF UNIX}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
{$IFDEF MSDOS}
  WriteLn('DebugMsg: ' + FmtMsg);
{$ENDIF}
end;

initialization
  InitCrcTable;
{$IFDEF MSWINDOWS}
  QueryPerformanceFrequency(PerfFrequency);
  InvPerfFrequency := 1.0 / PerfFrequency;
{$ENDIF}

{$IF Defined(DELPHI)}
  {$IF CompilerVersion >= 23}
  FloatFormatSettings := TFormatSettings.Create('en-US');
  {$ELSE}
  GetLocaleFormatSettings(1033, FloatFormatSettings);
  {$IFEND}
{$ELSE FPC}
  FloatFormatSettings := DefaultFormatSettings;
  FloatFormatSettings.DecimalSeparator := '.';
  FloatFormatSettings.ThousandSeparator := ',';
{$IFEND}

{
  File Notes:

  -- 0.77.1 ----------------------------------------------------
    - Added GetFileName, GetFileDir, RectWidth, RectHeight function.
    - Added ScaleSizeToFit function.
    - Added ZeroMemory and SwapValues for Booleans.
    - Added Substring function.
    - Renamed MatchFileNameMask to StrMaskMatch (it's for general use not
      just filenames).
    - Delphi XE2 new targets (Win64, OSX32) compatibility changes.
    - Added GetFormatSettingsForFloats function.

  -- 0.26.5 Changes/Bug Fixes -----------------------------------
    - Added Log10 function.
    - Added TFloatRect type and helper functions FloatRect, FloatRectWidth,
      FloatRectHeight.
    - Added string function ContainsAnySubStr.
    - Added functions PixelSizeToDpi, DpiToPixelSize.

  -- 0.26.1 Changes/Bug Fixes -----------------------------------
    - Some formatting changes.
    - Changed some string functions to work with localized strings.
    - ASM version of PosEx had bugs, removed it.
    - Added StrTokensToList function.

  -- 0.25.0 Changes/Bug Fixes -----------------------------------
    - Fixed error in ClipCopyBounds which was causing ... bad clipping!

  -- 0.24.3 Changes/Bug Fixes -----------------------------------
    - Added GetTimeMilliseconds function.
    - Added IntToStrFmt and FloatToStrFmt helper functions.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added RectInRect and RectIntersects functions
    - Added some string utils: StrToken, StrTokenEnd, PosEx, PosNoCase.
    - Moved BuildFileList here from DemoUtils.

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Moved GetVolumeLevelCount from ImagingDds here.
    - Renamed FillMemory to FillMemoryByte to avoid name collision in C++ Builder.
    - Added Iff function for Char, Pointer, and Int64 types.
    - Added IsLittleEndian function.
    - Added array types for TWordRec, TLongWordRec, and TInt64Rec.
    - Added MatchFileNameMask function.

  -- 0.19 Changes/Bug Fixes -----------------------------------
    - added ScaleRectToRect (thanks to Paul Michell)
    - added BoundsToRect, ClipBounds, ClipCopyBounds, ClipStretchBounds functions
    - added MulDiv function
    - FreeAndNil is not inline anymore - caused AV in one program

  -- 0.17 Changes/Bug Fixes -----------------------------------

    - GetAppExe didn't return absolute path in FreeBSD, fixed
    - added debug message output
    - fixed Unix compatibility issues (thanks to Ales Katona).
      Imaging now compiles in FreeBSD and maybe in other Unixes as well.

  -- 0.15 Changes/Bug Fixes -----------------------------------
    - added some new utility functions

  -- 0.13 Changes/Bug Fixes -----------------------------------
    - added many new utility functions
    - minor change in SwapEndian to avoid range check error

}
end.


