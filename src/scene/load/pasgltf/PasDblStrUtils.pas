(******************************************************************************
 *                               PasDblStrUtils                               *
 ******************************************************************************
 *                        Version 2016-09-12-08-18-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasdblstrutils                               *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 * 10. Make sure the code runs on platforms with weak and strong memory       *
 *     models without any issues.                                             *
 *                                                                            *
 ******************************************************************************)
 unit PasDblStrUtils;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define CanInline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
 {$if declared(UnicodeString)}
  {$define HAS_TYPE_UNICODESTRING}
 {$else}
  {$undef HAS_TYPE_UNICODESTRING}
 {$ifend}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
  {$if declared(UnicodeString)}
   {$define HAS_TYPE_UNICODESTRING}
  {$else}
   {$undef HAS_TYPE_UNICODESTRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
  {$undef HAS_TYPE_UNICODESTRING}
 {$endif}
 {$ifndef BCB}
  {$ifdef ver120}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
  {$ifdef ver140}
   {$define Delphi6}
  {$endif}
  {$ifdef ver150}
   {$define Delphi7}
  {$endif}
  {$ifdef ver170}
   {$define Delphi2005}
  {$endif}
 {$else}
  {$ifdef ver120}
   {$define Delphi4or5}
   {$define BCB4}
  {$endif}
  {$ifdef ver130}
   {$define Delphi4or5}
  {$endif}
 {$endif}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24}
   {$legacyifend on}
  {$ifend}
  {$if CompilerVersion>=14.0}
   {$if CompilerVersion=14.0}
    {$define Delphi6}
   {$ifend}
   {$define Delphi6AndUp}
  {$ifend}
  {$if CompilerVersion>=15.0}
   {$if CompilerVersion=15.0}
    {$define Delphi7}
   {$ifend}
   {$define Delphi7AndUp}
  {$ifend}
  {$if CompilerVersion>=17.0}
   {$if CompilerVersion=17.0}
    {$define Delphi2005}
   {$ifend}
   {$define Delphi2005AndUp}
  {$ifend}
  {$if CompilerVersion>=18.0}
   {$if CompilerVersion=18.0}
    {$define BDS2006}
    {$define Delphi2006}
   {$ifend}
   {$define Delphi2006AndUp}
  {$ifend}
  {$if CompilerVersion>=18.5}
   {$if CompilerVersion=18.5}
    {$define Delphi2007}
   {$ifend}
   {$define Delphi2007AndUp}
  {$ifend}
  {$if CompilerVersion=19.0}
   {$define Delphi2007Net}
  {$ifend}
  {$if CompilerVersion>=20.0}
   {$if CompilerVersion=20.0}
    {$define Delphi2009}
   {$ifend}
   {$define Delphi2009AndUp}
  {$ifend}
  {$if CompilerVersion>=21.0}
   {$if CompilerVersion=21.0}
    {$define Delphi2010}
   {$ifend}
   {$define Delphi2010AndUp}
  {$ifend}
  {$if CompilerVersion>=22.0}
   {$if CompilerVersion=22.0}
    {$define DelphiXE}
   {$ifend}
   {$define DelphiXEAndUp}
  {$ifend}
  {$if CompilerVersion>=23.0}
   {$if CompilerVersion=23.0}
    {$define DelphiXE2}
   {$ifend}
   {$define DelphiXE2AndUp}
  {$ifend}
  {$if CompilerVersion>=24.0}
   {$if CompilerVersion=24.0}
    {$define DelphiXE3}
   {$ifend}
   {$define DelphiXE3AndUp}
  {$ifend}
  {$if CompilerVersion>=25.0}
   {$if CompilerVersion=25.0}
    {$define DelphiXE4}
   {$ifend}
   {$define DelphiXE4AndUp}
  {$ifend}
  {$if CompilerVersion>=26.0}
   {$if CompilerVersion=26.0}
    {$define DelphiXE5}
   {$ifend}
   {$define DelphiXE5AndUp}
  {$ifend}
  {$if CompilerVersion>=27.0}
   {$if CompilerVersion=27.0}
    {$define DelphiXE6}
   {$ifend}
   {$define DelphiXE6AndUp}
  {$ifend}
  {$if CompilerVersion>=28.0}
   {$if CompilerVersion=28.0}
    {$define DelphiXE7}
   {$ifend}
   {$define DelphiXE7AndUp}
  {$ifend}
  {$if CompilerVersion>=29.0}
   {$if CompilerVersion=29.0}
    {$define DelphiXE8}
   {$ifend}
   {$define DelphiXE8AndUp}
  {$ifend}
  {$if CompilerVersion>=30.0}
   {$if CompilerVersion=30.0}
    {$define Delphi10Seattle}
   {$ifend}
   {$define Delphi10SeattleAndUp}
  {$ifend}
  {$if CompilerVersion>=31.0}
   {$if CompilerVersion=31.0}
    {$define Delphi10Berlin}
   {$ifend}
   {$define Delphi10BerlinAndUp}
  {$ifend}
 {$endif}
 {$ifndef Delphi4or5}
  {$ifndef BCB}
   {$define Delphi6AndUp}
  {$endif}
   {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
     {$define Delphi7AndUp}
    {$endif}
    {$ifndef BCB}
     {$ifndef Delphi7}
      {$ifndef Delphi2005}
       {$define BDS2006AndUp}
      {$endif}
     {$endif}
    {$endif}
   {$endif}
 {$endif}
 {$ifdef Delphi6AndUp}
  {$warn symbol_platform off}
  {$warn symbol_deprecated off}
 {$endif}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

// CGE: fix for Delphi on non-Windows, also makes sense for WebAssembly
{$if (not defined(FPC)) and (not defined(MSWINDOWS))}
  {$define USE_TRIVIAL_IMPLEMENTATION}
{$ifend}
{$if defined(WASI)}
  {$define USE_TRIVIAL_IMPLEMENTATION}
{$ifend}

{ CGE: Delphi on Linux doesn't define
  - TFPUPrecisionMode, SetPrecisionMode
  - TFPUExceptionMask, SetFPUExceptionMask }
{$if defined(FPC) or defined(MSWINDOWS)}
  {$define HAS_FPU_TYPES}
{$ifend}

interface

uses SysUtils,Math;

type PPasDblStrUtilsInt8=^TPasDblStrUtilsInt8;
     TPasDblStrUtilsInt8={$ifdef fpc}Int8{$else}ShortInt{$endif};

     PPasDblStrUtilsUInt8=^TPasDblStrUtilsUInt8;
     TPasDblStrUtilsUInt8={$ifdef fpc}UInt8{$else}Byte{$endif};

     PPasDblStrUtilsInt16=^TPasDblStrUtilsInt16;
     TPasDblStrUtilsInt16={$ifdef fpc}Int16{$else}SmallInt{$endif};

     PPasDblStrUtilsUInt16=^TPasDblStrUtilsUInt16;
     TPasDblStrUtilsUInt16={$ifdef fpc}UInt16{$else}Word{$endif};

     PPasDblStrUtilsInt32=^TPasDblStrUtilsInt32;
     TPasDblStrUtilsInt32={$ifdef fpc}Int32{$else}Integer{$endif};

     PPasDblStrUtilsUInt32=^TPasDblStrUtilsUInt32;
     TPasDblStrUtilsUInt32={$ifdef fpc}UInt32{$else}Cardinal{$endif};

     PPasDblStrUtilsInt64=^TPasDblStrUtilsInt64;
     TPasDblStrUtilsInt64=Int64;

     PPasDblStrUtilsUInt64=^TPasDblStrUtilsUInt64;
     TPasDblStrUtilsUInt64=UInt64;

     PPasDblStrUtilsDouble=^TPasDblStrUtilsDouble;
     TPasDblStrUtilsDouble=Double;

     PPasDblStrUtilsBoolean=^TPasDblStrUtilsBoolean;
     TPasDblStrUtilsBoolean=Boolean;

     PPasDblStrUtilsPtrUInt=^TPasDblStrUtilsPtrUInt;
     PPasDblStrUtilsPtrInt=^TPasDblStrUtilsPtrInt;

{$ifdef fpc}
     TPasDblStrUtilsPtrUInt=PtrUInt;
     TPasDblStrUtilsPtrInt=PtrInt;
{$else}
{$if Declared(CompilerVersion) and (CompilerVersion>=23.0)}
     TPasDblStrUtilsPtrUInt=NativeUInt;
     TPasDblStrUtilsPtrInt=NativeInt;
{$else}
{$ifdef cpu64}
     TPasDblStrUtilsPtrUInt=TPasDblStrUtilsUInt64;
     TPasDblStrUtilsPtrInt=TPasDblStrUtilsInt64;
{$else}
     TPasDblStrUtilsPtrUInt=TPasDblStrUtilsUInt32;
     TPasDblStrUtilsPtrInt=TPasDblStrUtilsInt32;
{$endif}
{$ifend}
{$endif}

     PPasDblStrUtilsNativeUInt=^TPasDblStrUtilsNativeUInt;
     PPasDblStrUtilsNativeInt=^TPasDblStrUtilsNativeInt;
     TPasDblStrUtilsNativeUInt=TPasDblStrUtilsPtrUInt;
     TPasDblStrUtilsNativeInt=TPasDblStrUtilsPtrInt;

     PPasDblStrUtilsRawByteChar=PAnsiChar;
     TPasDblStrUtilsRawByteChar=AnsiChar;

     PPasDblStrUtilsRawByteCharSet=^TPasDblStrUtilsRawByteCharSet;
     TPasDblStrUtilsRawByteCharSet=set of TPasDblStrUtilsRawByteChar;

     PPasDblStrUtilsRawByteString=^TPasDblStrUtilsRawByteString;
     TPasDblStrUtilsRawByteString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     PPasDblStrUtilsUTF8Char=PAnsiChar;
     TPasDblStrUtilsUTF8Char=AnsiChar;

     PPasDblStrUtilsUTF8String=^TPasDblStrUtilsUTF8String;
     TPasDblStrUtilsUTF8String={$ifdef HAS_TYPE_UTF8STRING}UTF8String{$else}AnsiString{$endif};

     PPasDblStrUtilsUTF16Char={$ifdef HAS_TYPE_UNICODESTRING}{$ifdef fpc}PUnicodeChar{$else}PWideChar{$endif}{$else}PWideChar{$endif};
     TPasDblStrUtilsUTF16Char={$ifdef HAS_TYPE_UNICODESTRING}{$ifdef fpc}UnicodeChar{$else}WideChar{$endif}{$else}WideChar{$endif};

     PPasDblStrUtilsUTF16String=^TPasDblStrUtilsUTF16String;
     TPasDblStrUtilsUTF16String={$ifdef HAS_TYPE_UNICODESTRING}UnicodeString{$else}WideString{$endif};

     PPasDblStrUtilsChar=PAnsiChar;
     TPasDblStrUtilsChar=AnsiChar;

     PPasDblStrUtilsString=^TPasDblStrUtilsString;
     TPasDblStrUtilsString={$ifdef HAS_TYPE_RAWBYTESTRING}RawByteString{$else}AnsiString{$endif};

     PPasDblStrUtilsPointer=^TPasDblStrUtilsPointer;
     TPasDblStrUtilsPointer=Pointer;

     PPasDblStrUtilsRoundingMode=^TPasDblStrUtilsRoundingMode;
     TPasDblStrUtilsRoundingMode=
       {$ifdef HAS_FPU_TYPES}
         type TFPURoundingMode
       {$else}
         (rmNearest, rmDown, rmUp, rmTruncate)
       {$endif};

     TPasDblStrUtilsOutputMode=(
      omStandard,
      omStandardExponential,
      omFixed,
      omExponential,
      omPrecision,
      omRadix
     );

function ConvertStringToDouble(const StringValue:TPasDblStrUtilsString;const RoundingMode:TPasDblStrUtilsRoundingMode=rmNearest;const OK:PPasDblStrUtilsBoolean=nil;const Base:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsDouble;

function ConvertDoubleToString(const AValue:TPasDblStrUtilsDouble;const OutputMode:TPasDblStrUtilsOutputMode=omStandard;RequestedDigits:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsString;

implementation

{$ifdef USE_TRIVIAL_IMPLEMENTATION}
{$warn IMPLICIT_STRING_CAST off}
{$warn IMPLICIT_STRING_CAST_LOSS off}

uses CastleUtils;

function ConvertStringToDouble(const StringValue:TPasDblStrUtilsString;const RoundingMode:TPasDblStrUtilsRoundingMode=rmNearest;const OK:PPasDblStrUtilsBoolean=nil;const Base:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsDouble;
var
  Success: Boolean;
begin
  Success := TryStrToFloatDot(StringValue, Result);
  { Assign OK^, if OK non-nil. }
  if OK <> nil then
    OK^ := Success;
end;

function ConvertDoubleToString(const AValue:TPasDblStrUtilsDouble;const OutputMode:TPasDblStrUtilsOutputMode=omStandard;RequestedDigits:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsString;
begin
  Result := FloatToStrDot(AValue);
end;

{$else}

type PDoubleHiLo=^TDoubleHiLo;
     TDoubleHiLo=packed record
{$ifdef BIG_ENDIAN}
      Hi,Lo:TPasDblStrUtilsUInt32;
{$else}
      Lo,Hi:TPasDblStrUtilsUInt32;
{$endif}
     end;

     PDoubleBytes=^TDoubleBytes;
     TDoubleBytes=array[0..sizeof(TPasDblStrUtilsDouble)-1] of TPasDblStrUtilsUInt8;

{$ifdef cpu64}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PPasDblStrUtilsInt64(@AValue)^ and $7ff0000000000000)=$7ff0000000000000) and ((PPasDblStrUtilsInt64(@AValue)^ and $000fffffffffffff)<>$0000000000000000);
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PPasDblStrUtilsInt64(@AValue)^ and $7fffffffffffffff)=$7ff0000000000000;
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PPasDblStrUtilsInt64(@AValue)^ and $7ff0000000000000)<>$7ff0000000000000;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($7ff0000000000000);
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=TPasDblStrUtilsUInt64(pointer(@AValue)^)=TPasDblStrUtilsUInt64($fff0000000000000);
{$else}
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($fff0000000000000);
{$endif}
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($0000000000000000);
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=TPasDblStrUtilsUInt64(pointer(@AValue)^)=TPasDblStrUtilsUInt64($8000000000000000);
{$else}
 result:=PPasDblStrUtilsInt64(@AValue)^=TPasDblStrUtilsInt64($8000000000000000);
{$endif}
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(TPasDblStrUtilsUInt64(pointer(@AValue)^) and TPasDblStrUtilsUInt64($7fffffffffffffff))=TPasDblStrUtilsUInt64($0000000000000000);
{$else}
 result:=(PPasDblStrUtilsInt64(@AValue)^ and TPasDblStrUtilsInt64($7fffffffffffffff))=TPasDblStrUtilsInt64($0000000000000000);
{$endif}
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(TPasDblStrUtilsUInt64(pointer(@AValue)^) and TPasDblStrUtilsUInt64($8000000000000000))<>0;
{$else}
 result:=(PPasDblStrUtilsInt64(@AValue)^ shr 63)<>0;
{$endif}
end;
{$else}
{$ifdef TrickyNumberChecks}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var l:TPasDblStrUtilsUInt32;
begin
 l:=PDoubleHiLo(@AValue)^.Lo;
 result:=(TPasDblStrUtilsUInt32($7ff00000-TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) or ((l or (-l)) shr 31))) shr 31)<>0;
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32((TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(TPasDblStrUtilsUInt32((PDoubleHiLo(@AValue)^.Hi and $7fffffff)-$7ff00000) shr 31)<>0;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(((TPasDblStrUtilsUInt32(h and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31))=0;
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(((TPasDblStrUtilsUInt32(h and $7fffffff) xor $7ff00000) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(not h) shr 31))=0;
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7fffffff) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(h shr 31))=0;
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
var h:TPasDblStrUtilsUInt32;
begin
 h:=PDoubleHiLo(@AValue)^.Hi;
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(h and $7fffffff) or PDoubleHiLo(@AValue)^.Lo) or TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(not h) shr 31))=0;
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and $7fffffff) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=TPasDblStrUtilsUInt32(PDoubleHiLo(@AValue)^.Hi and TPasDblStrUtilsUInt32($80000000))<>0;
end;
{$else}
function IsNaN(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7ff00000)=$7ff00000) and (((PDoubleHiLo(@AValue)^.Hi and $000fffff) or PDoubleHiLo(@AValue)^.Lo)<>0);
end;

function IsInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7fffffff)=$7ff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsFinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi and $7ff00000)<>$7ff00000;
end;

function IsPosInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$7ff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsNegInfinite(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$fff00000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsPosZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi=$80000000) and (PDoubleHiLo(@AValue)^.Lo=0);
end;

function IsZero(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PDoubleHiLo(@AValue)^.Hi and $7fffffff) or PDoubleHiLo(@AValue)^.Lo)=0;
end;

function IsNegative(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsBoolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PDoubleHiLo(@AValue)^.Hi and $80000000)<>0;
end;
{$endif}
{$endif}

function DoubleAbsolute(const AValue:TPasDblStrUtilsDouble):TPasDblStrUtilsDouble; {$ifdef caninline}inline;{$endif}
begin
{$ifdef cpu64}
 PPasDblStrUtilsInt64(@result)^:=PPasDblStrUtilsInt64(@AValue)^ and $7fffffffffffffff;
{$else}
 PDoubleHiLo(@result)^.Hi:=PDoubleHiLo(@AValue)^.Hi and $7fffffff;
 PDoubleHiLo(@result)^.Lo:=PDoubleHiLo(@AValue)^.Lo;
{$endif}
end;

const DoubleToStringPowerOfTenTable:array[0..86,0..2] of TPasDblStrUtilsInt64=((TPasDblStrUtilsInt64($fa8fd5a0081c0288),-1220,-348),
                                                                               (TPasDblStrUtilsInt64($baaee17fa23ebf76),-1193,-340),
                                                                               (TPasDblStrUtilsInt64($8b16fb203055ac76),-1166,-332),
                                                                               (TPasDblStrUtilsInt64($cf42894a5dce35ea),-1140,-324),
                                                                               (TPasDblStrUtilsInt64($9a6bb0aa55653b2d),-1113,-316),
                                                                               (TPasDblStrUtilsInt64($e61acf033d1a45df),-1087,-308),
                                                                               (TPasDblStrUtilsInt64($ab70fe17c79ac6ca),-1060,-300),
                                                                               (TPasDblStrUtilsInt64($ff77b1fcbebcdc4f),-1034,-292),
                                                                               (TPasDblStrUtilsInt64($be5691ef416bd60c),-1007,-284),
                                                                               (TPasDblStrUtilsInt64($8dd01fad907ffc3c),-980,-276),
                                                                               (TPasDblStrUtilsInt64($d3515c2831559a83),-954,-268),
                                                                               (TPasDblStrUtilsInt64($9d71ac8fada6c9b5),-927,-260),
                                                                               (TPasDblStrUtilsInt64($ea9c227723ee8bcb),-901,-252),
                                                                               (TPasDblStrUtilsInt64($aecc49914078536d),-874,-244),
                                                                               (TPasDblStrUtilsInt64($823c12795db6ce57),-847,-236),
                                                                               (TPasDblStrUtilsInt64($c21094364dfb5637),-821,-228),
                                                                               (TPasDblStrUtilsInt64($9096ea6f3848984f),-794,-220),
                                                                               (TPasDblStrUtilsInt64($d77485cb25823ac7),-768,-212),
                                                                               (TPasDblStrUtilsInt64($a086cfcd97bf97f4),-741,-204),
                                                                               (TPasDblStrUtilsInt64($ef340a98172aace5),-715,-196),
                                                                               (TPasDblStrUtilsInt64($b23867fb2a35b28e),-688,-188),
                                                                               (TPasDblStrUtilsInt64($84c8d4dfd2c63f3b),-661,-180),
                                                                               (TPasDblStrUtilsInt64($c5dd44271ad3cdba),-635,-172),
                                                                               (TPasDblStrUtilsInt64($936b9fcebb25c996),-608,-164),
                                                                               (TPasDblStrUtilsInt64($dbac6c247d62a584),-582,-156),
                                                                               (TPasDblStrUtilsInt64($a3ab66580d5fdaf6),-555,-148),
                                                                               (TPasDblStrUtilsInt64($f3e2f893dec3f126),-529,-140),
                                                                               (TPasDblStrUtilsInt64($b5b5ada8aaff80b8),-502,-132),
                                                                               (TPasDblStrUtilsInt64($87625f056c7c4a8b),-475,-124),
                                                                               (TPasDblStrUtilsInt64($c9bcff6034c13053),-449,-116),
                                                                               (TPasDblStrUtilsInt64($964e858c91ba2655),-422,-108),
                                                                               (TPasDblStrUtilsInt64($dff9772470297ebd),-396,-100),
                                                                               (TPasDblStrUtilsInt64($a6dfbd9fb8e5b88f),-369,-92),
                                                                               (TPasDblStrUtilsInt64($f8a95fcf88747d94),-343,-84),
                                                                               (TPasDblStrUtilsInt64($b94470938fa89bcf),-316,-76),
                                                                               (TPasDblStrUtilsInt64($8a08f0f8bf0f156b),-289,-68),
                                                                               (TPasDblStrUtilsInt64($cdb02555653131b6),-263,-60),
                                                                               (TPasDblStrUtilsInt64($993fe2c6d07b7fac),-236,-52),
                                                                               (TPasDblStrUtilsInt64($e45c10c42a2b3b06),-210,-44),
                                                                               (TPasDblStrUtilsInt64($aa242499697392d3),-183,-36),
                                                                               (TPasDblStrUtilsInt64($fd87b5f28300ca0e),-157,-28),
                                                                               (TPasDblStrUtilsInt64($bce5086492111aeb),-130,-20),
                                                                               (TPasDblStrUtilsInt64($8cbccc096f5088cc),-103,-12),
                                                                               (TPasDblStrUtilsInt64($d1b71758e219652c),-77,-4),
                                                                               (TPasDblStrUtilsInt64($9c40000000000000),-50,4),
                                                                               (TPasDblStrUtilsInt64($e8d4a51000000000),-24,12),
                                                                               (TPasDblStrUtilsInt64($ad78ebc5ac620000),3,20),
                                                                               (TPasDblStrUtilsInt64($813f3978f8940984),30,28),
                                                                               (TPasDblStrUtilsInt64($c097ce7bc90715b3),56,36),
                                                                               (TPasDblStrUtilsInt64($8f7e32ce7bea5c70),83,44),
                                                                               (TPasDblStrUtilsInt64($d5d238a4abe98068),109,52),
                                                                               (TPasDblStrUtilsInt64($9f4f2726179a2245),136,60),
                                                                               (TPasDblStrUtilsInt64($ed63a231d4c4fb27),162,68),
                                                                               (TPasDblStrUtilsInt64($b0de65388cc8ada8),189,76),
                                                                               (TPasDblStrUtilsInt64($83c7088e1aab65db),216,84),
                                                                               (TPasDblStrUtilsInt64($c45d1df942711d9a),242,92),
                                                                               (TPasDblStrUtilsInt64($924d692ca61be758),269,100),
                                                                               (TPasDblStrUtilsInt64($da01ee641a708dea),295,108),
                                                                               (TPasDblStrUtilsInt64($a26da3999aef774a),322,116),
                                                                               (TPasDblStrUtilsInt64($f209787bb47d6b85),348,124),
                                                                               (TPasDblStrUtilsInt64($b454e4a179dd1877),375,132),
                                                                               (TPasDblStrUtilsInt64($865b86925b9bc5c2),402,140),
                                                                               (TPasDblStrUtilsInt64($c83553c5c8965d3d),428,148),
                                                                               (TPasDblStrUtilsInt64($952ab45cfa97a0b3),455,156),
                                                                               (TPasDblStrUtilsInt64($de469fbd99a05fe3),481,164),
                                                                               (TPasDblStrUtilsInt64($a59bc234db398c25),508,172),
                                                                               (TPasDblStrUtilsInt64($f6c69a72a3989f5c),534,180),
                                                                               (TPasDblStrUtilsInt64($b7dcbf5354e9bece),561,188),
                                                                               (TPasDblStrUtilsInt64($88fcf317f22241e2),588,196),
                                                                               (TPasDblStrUtilsInt64($cc20ce9bd35c78a5),614,204),
                                                                               (TPasDblStrUtilsInt64($98165af37b2153df),641,212),
                                                                               (TPasDblStrUtilsInt64($e2a0b5dc971f303a),667,220),
                                                                               (TPasDblStrUtilsInt64($a8d9d1535ce3b396),694,228),
                                                                               (TPasDblStrUtilsInt64($fb9b7cd9a4a7443c),720,236),
                                                                               (TPasDblStrUtilsInt64($bb764c4ca7a44410),747,244),
                                                                               (TPasDblStrUtilsInt64($8bab8eefb6409c1a),774,252),
                                                                               (TPasDblStrUtilsInt64($d01fef10a657842c),800,260),
                                                                               (TPasDblStrUtilsInt64($9b10a4e5e9913129),827,268),
                                                                               (TPasDblStrUtilsInt64($e7109bfba19c0c9d),853,276),
                                                                               (TPasDblStrUtilsInt64($ac2820d9623bf429),880,284),
                                                                               (TPasDblStrUtilsInt64($80444b5e7aa7cf85),907,292),
                                                                               (TPasDblStrUtilsInt64($bf21e44003acdd2d),933,300),
                                                                               (TPasDblStrUtilsInt64($8e679c2f5e44ff8f),960,308),
                                                                               (TPasDblStrUtilsInt64($d433179d9c8cb841),986,316),
                                                                               (TPasDblStrUtilsInt64($9e19db92b4e31ba9),1013,324),
                                                                               (TPasDblStrUtilsInt64($eb96bf6ebadf77d9),1039,332),
                                                                               (TPasDblStrUtilsInt64($af87023b9bf0ee6b),1066,340));

      DoubleToStringPowerOfTenBinaryExponentTable:array[-1220..(1066+27)-1] of TPasDblStrUtilsUInt8=(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                                                                     1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,
                                                                                                     2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                                                                     2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,
                                                                                                     3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                                                                     3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                                                                                     4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,
                                                                                                     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                                                                                     5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,
                                                                                                     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                                                                     6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                                                                     7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,
                                                                                                     8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                                                                                     8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,
                                                                                                     9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                                                                                                     9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                                                                                                     10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,
                                                                                                     11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
                                                                                                     11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,
                                                                                                     12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                                                                                     13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
                                                                                                     13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,
                                                                                                     14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
                                                                                                     14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,
                                                                                                     15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
                                                                                                     16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                                                                                     16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,
                                                                                                     17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
                                                                                                     17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,
                                                                                                     18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
                                                                                                     19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,
                                                                                                     19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,
                                                                                                     20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                                                                                                     20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,
                                                                                                     21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                                                                                     22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,
                                                                                                     22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,
                                                                                                     23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
                                                                                                     23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,
                                                                                                     24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,
                                                                                                     25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,
                                                                                                     25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,
                                                                                                     26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
                                                                                                     26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,
                                                                                                     27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,
                                                                                                     28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
                                                                                                     28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,
                                                                                                     29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
                                                                                                     29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,
                                                                                                     30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,31,
                                                                                                     31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
                                                                                                     31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                                     32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
                                                                                                     32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,
                                                                                                     33,33,33,33,33,33,33,33,33,33,33,33,33,33,34,34,
                                                                                                     34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
                                                                                                     34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,
                                                                                                     35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
                                                                                                     35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,
                                                                                                     36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,37,
                                                                                                     37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
                                                                                                     37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                                     38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,
                                                                                                     38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,
                                                                                                     39,39,39,39,39,39,39,39,39,39,39,39,39,39,40,40,
                                                                                                     40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
                                                                                                     40,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,
                                                                                                     41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,
                                                                                                     41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42,
                                                                                                     42,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,
                                                                                                     43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,
                                                                                                     43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,
                                                                                                     44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,
                                                                                                     44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,
                                                                                                     45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,
                                                                                                     46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
                                                                                                     46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,
                                                                                                     47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,
                                                                                                     47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,
                                                                                                     48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,
                                                                                                     49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,
                                                                                                     49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,
                                                                                                     50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,
                                                                                                     50,50,51,51,51,51,51,51,51,51,51,51,51,51,51,51,
                                                                                                     51,51,51,51,51,51,51,51,51,51,51,51,51,52,52,52,
                                                                                                     52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,
                                                                                                     52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,
                                                                                                     53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,
                                                                                                     53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,
                                                                                                     54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,
                                                                                                     55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                                                                                                     55,55,55,55,55,55,55,56,56,56,56,56,56,56,56,56,
                                                                                                     56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
                                                                                                     56,56,57,57,57,57,57,57,57,57,57,57,57,57,57,57,
                                                                                                     57,57,57,57,57,57,57,57,57,57,57,57,58,58,58,58,
                                                                                                     58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,
                                                                                                     58,58,58,58,58,58,58,59,59,59,59,59,59,59,59,59,
                                                                                                     59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,
                                                                                                     59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,
                                                                                                     60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,
                                                                                                     61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,
                                                                                                     61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,62,
                                                                                                     62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,
                                                                                                     62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
                                                                                                     63,63,63,63,63,63,63,63,63,63,63,63,64,64,64,64,
                                                                                                     64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,
                                                                                                     64,64,64,64,64,64,65,65,65,65,65,65,65,65,65,65,
                                                                                                     65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,
                                                                                                     65,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,
                                                                                                     66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,
                                                                                                     67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
                                                                                                     67,67,67,67,67,67,68,68,68,68,68,68,68,68,68,68,
                                                                                                     68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,
                                                                                                     68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                                                                                                     69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,
                                                                                                     70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,
                                                                                                     70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,
                                                                                                     71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,
                                                                                                     72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,
                                                                                                     72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,
                                                                                                     73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,
                                                                                                     73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,
                                                                                                     74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,
                                                                                                     75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,
                                                                                                     75,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,
                                                                                                     76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,
                                                                                                     76,76,76,76,76,77,77,77,77,77,77,77,77,77,77,77,
                                                                                                     77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,
                                                                                                     78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,
                                                                                                     78,78,78,78,78,78,78,78,78,78,79,79,79,79,79,79,
                                                                                                     79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,
                                                                                                     79,79,79,79,79,80,80,80,80,80,80,80,80,80,80,80,
                                                                                                     80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,
                                                                                                     81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,
                                                                                                     81,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,
                                                                                                     82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,
                                                                                                     82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,
                                                                                                     83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,84,
                                                                                                     84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,
                                                                                                     84,84,84,84,84,84,84,84,84,84,85,85,85,85,85,85,
                                                                                                     85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,
                                                                                                     85,85,85,85,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                                     86,86,86,86,86,86,86,86,86);

      DoubleToStringPowerOfTenDecimalExponentTable:array[-348..(340+8)-1] of TPasDblStrUtilsUInt8=(0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,
                                                                                                   2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,
                                                                                                   4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,
                                                                                                   6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,
                                                                                                   8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,
                                                                                                   10,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
                                                                                                   12,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,
                                                                                                   14,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,
                                                                                                   16,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,
                                                                                                   18,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,
                                                                                                   20,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,
                                                                                                   22,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,
                                                                                                   24,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,
                                                                                                   26,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,
                                                                                                   28,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,
                                                                                                   30,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                                   32,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,
                                                                                                   34,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,
                                                                                                   36,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                                   38,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,
                                                                                                   40,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,
                                                                                                   42,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,
                                                                                                   44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,
                                                                                                   46,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,
                                                                                                   48,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,
                                                                                                   50,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,
                                                                                                   52,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,
                                                                                                   54,55,55,55,55,55,55,55,55,56,56,56,56,56,56,56,
                                                                                                   56,57,57,57,57,57,57,57,57,58,58,58,58,58,58,58,
                                                                                                   58,59,59,59,59,59,59,59,59,60,60,60,60,60,60,60,
                                                                                                   60,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,
                                                                                                   62,63,63,63,63,63,63,63,63,64,64,64,64,64,64,64,
                                                                                                   64,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,
                                                                                                   66,67,67,67,67,67,67,67,67,68,68,68,68,68,68,68,
                                                                                                   68,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,
                                                                                                   70,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,
                                                                                                   72,73,73,73,73,73,73,73,73,74,74,74,74,74,74,74,
                                                                                                   74,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,
                                                                                                   76,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,
                                                                                                   78,79,79,79,79,79,79,79,79,80,80,80,80,80,80,80,
                                                                                                   80,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,
                                                                                                   82,83,83,83,83,83,83,83,83,84,84,84,84,84,84,84,
                                                                                                   84,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,
                                                                                                   86,86,86,86,86,86,86,86);

      DoubleToStringEstimatePowerFactorTable:array[2..36] of TPasDblStrUtilsInt64=(4294967296, // round((ln(2)/ln(Radix))*4294967296.0);
                                                                                   2709822658,
                                                                                   2147483648,
                                                                                   1849741732,
                                                                                   1661520155,
                                                                                   1529898219,
                                                                                   1431655765,
                                                                                   1354911329,
                                                                                   1292913986,
                                                                                   1241523975,
                                                                                   1198050829,
                                                                                   1160664035,
                                                                                   1128071163,
                                                                                   1099331346,
                                                                                   1073741824,
                                                                                   1050766077,
                                                                                   1029986701,
                                                                                   1011073584,
                                                                                   993761859,
                                                                                   977836272,
                                                                                   963119891,
                                                                                   949465783,
                                                                                   936750801,
                                                                                   924870866,
                                                                                   913737342,
                                                                                   903274219,
                                                                                   893415894,
                                                                                   884105413,
                                                                                   875293062,
                                                                                   866935226,
                                                                                   858993459,
                                                                                   851433729,
                                                                                   844225782,
                                                                                   837342623,
                                                                                   830760078);

function ConvertStringToDouble(const StringValue:TPasDblStrUtilsString;const RoundingMode:TPasDblStrUtilsRoundingMode=rmNearest;const OK:PPasDblStrUtilsBoolean=nil;const Base:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsDouble;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case TPasDblStrUtilsUInt8 of
       0:(Value:TPasDblStrUtilsDouble);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:TPasDblStrUtilsUInt32);
       2:(Value64:TPasDblStrUtilsInt64);
     end;
const MantissaWords=12; //6; // 12
      MantissaDigits=52; //28; // 52
      WordTopBit=$8000;
      WordBits=16;
      WordBitShift=4;
      WordBitMask=WordBits-1;
      WordMask=$ffff;
      IEEEFormatBytes=8;
      IEEEFormatBits=IEEEFormatBytes shl 3;
      IEEEFormatExplicit=0;
      IEEEFormatExponent=11;
      IEEEFormatOneMask=WordTopBit shr ((IEEEFormatExponent+IEEEFormatExplicit) and WordBitMask);
      IEEEFormatOnePos=(IEEEFormatExponent+IEEEFormatExplicit) shr WordBitShift;
      IEEEFormatExpMax=1 shl (IEEEFormatExponent-1);
      Bit53=TPasDblStrUtilsInt64(TPasDblStrUtilsInt64(1) shl 53);
      InvBit53Mask=TPasDblStrUtilsInt64($ffe0000000000000);
      MaximumMultiplier=TPasDblStrUtilsUInt32(TPasDblStrUtilsUInt32($ffffffff) div 36);
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
      MaxFastPathDigits=16;
      TenPowers:array[0..18] of TPasDblStrUtilsInt64=
       (
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000
       );
type PWords=^TWords;
     TWords=array[0..MantissaWords] of TPasDblStrUtilsUInt16;
     PTemp=^TTemp;
     TTemp=array[0..MantissaWords*2] of TPasDblStrUtilsUInt32;
     PDigits=^TDigits;
     TDigits=array[0..MantissaDigits] of TPasDblStrUtilsUInt8;
var MantissaPosition,Exponent,TenPower,TwoPower,ExtraTwos,Shift,i,DigitPos,StoredDigitPos,DigitPosBackwards,Digit,Overflow,OverflowBits,DroppedBits,DroppedBitsMask,MiddleValue,ExponentPower,ExponentValue:TPasDblStrUtilsInt32;
    Bit,Carry:TPasDblStrUtilsUInt16;
    Negative,ExponentNegative,HasDigits,Started,ZeroTail,Done:TPasDblStrUtilsBoolean;
    ResultCasted:PDoubleCasted;
    Temp:PTemp;
    Digits:PDigits;
    MantissaMultiplicator,Mantissa:PWords;
    Value:TPasDblStrUtilsInt64;
    c:TPasDblStrUtilsChar;
    Part,Multiplier,NextMultiplier:TPasDblStrUtilsUInt32;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
 function MantissaMultiply(vTo,vFrom:PWords):TPasDblStrUtilsInt32;
 var i,j,k:TPasDblStrUtilsInt32;
     v:TPasDblStrUtilsUInt32;
     t:PTemp;
 begin
  t:=Temp;
  FillChar(t^,sizeof(TTemp),#0);
  for i:=0 to MantissaWords-1 do begin
   for j:=0 to MantissaWords-1 do begin
    v:=TPasDblStrUtilsUInt32(vTo^[i]+0)*TPasDblStrUtilsUInt32(vFrom^[j]+0);
    k:=i+j;
    inc(t^[k],v shr WordBits);
    inc(t^[k+1],v and WordMask);
   end;
  end;
  for i:=high(TTemp) downto 1 do begin
   inc(t^[i-1],t^[i] shr WordBits);
   t^[i]:=t^[i] and WordMask;
  end;
  if (t^[0] and WordTopBit)<>0 then begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=t^[i] and WordMask;
   end;
   result:=0;
  end else begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=(t^[i] shl 1)+TPasDblStrUtilsUInt16(ord((t^[i+1] and WordTopBit)<>0));
   end;
   result:=-1;
  end;
 end;
 procedure MantissaShiftRight(var Mantissa:TWords;Shift:TPasDblStrUtilsInt32);
 var Bits,Words,InvBits,Position:TPasDblStrUtilsInt32;
     Carry,Current:TPasDblStrUtilsUInt32;
 begin
  Bits:=Shift and WordBitMask;
  Words:=Shift shr WordBitShift;
  InvBits:=WordBits-Bits;
  Position:=high(TWords);
  if Bits=0 then begin
   if Words<>0 then begin
    while Position>=Words do begin
     Mantissa[Position]:=Mantissa[Position-Words];
     dec(Position);
    end;
   end;
  end else begin
   if (high(TWords)-Words)>=0 then begin
    Carry:=Mantissa[high(TWords)-Words] shr Bits;
   end else begin
    Carry:=0;
   end;
   while Position>Words do begin
    Current:=Mantissa[Position-(Words+1)];
    Mantissa[Position]:=(Current shl InvBits) or Carry;
    Carry:=Current shr Bits;
    dec(Position);
   end;
   Mantissa[Position]:=Carry;
   dec(Position);
  end;
  while Position>=0 do begin
   Mantissa[Position]:=0;
   dec(Position);
  end;
 end;
 procedure MantissaSetBit(var Mantissa:TWords;i:TPasDblStrUtilsInt32); {$ifdef CanInline}inline;{$endif}
 begin
  Mantissa[i shr WordBitShift]:=Mantissa[i shr WordBitShift] or (WordTopBit shr (i and WordBitMask));
 end;
 function MantissaTestBit(var Mantissa:TWords;i:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean; {$ifdef CanInline}inline;{$endif}
 begin
  result:=(Mantissa[i shr WordBitShift] shr ((not i) and WordBitMask))<>0;
 end;
 function MantissaIsZero(var Mantissa:TWords):TPasDblStrUtilsBoolean;
 var i:TPasDblStrUtilsInt32;
 begin
  result:=true;
  for i:=low(TWords) to High(TWords) do begin
   if Mantissa[i]<>0 then begin
    result:=false;
    break;
   end;
  end;
 end;
 function MantissaRound(Negative:TPasDblStrUtilsBoolean;var Mantissa:TWords;BitPos:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var i,p:TPasDblStrUtilsInt32;
     Bit:TPasDblStrUtilsUInt32;
  function RoundAbsDown:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
  begin
   Mantissa[i]:=Mantissa[i] and not (Bit-1);
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   result:=false;
  end;
  function RoundAbsUp:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
  begin
   Mantissa[i]:=(Mantissa[i] and not (Bit-1))+Bit;
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   while (i>0) and (Mantissa[i]=0) do begin
    dec(i);
    inc(Mantissa[i]);
   end;
   result:=Mantissa[0]=0;
  end;
  function RoundTowardsInfinity:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
      m:TPasDblStrUtilsUInt32;
  begin
   m:=Mantissa[i] and ((Bit shl 1)-1);
   for j:=i+1 to high(TWords) do begin
    m:=m or Mantissa[j];
   end;
   if m<>0 then begin
    result:=RoundAbsUp;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
  function RoundNear:TPasDblStrUtilsBoolean;
  var j:TPasDblStrUtilsInt32;
      m:TPasDblStrUtilsUInt32;
  begin
   if (Mantissa[i] and Bit)<>0 then begin
    Mantissa[i]:=Mantissa[i] and not Bit;
    m:=Mantissa[i] and ((Bit shl 1)-1);
    for j:=i+1 to high(TWords) do begin
     m:=m or Mantissa[j];
    end;
    Mantissa[i]:=Mantissa[i] or Bit;
    if m<>0 then begin
     result:=RoundAbsUp;
    end else begin
     if MantissaTestBit(Mantissa,BitPos-1) then begin
      result:=RoundAbsUp;
     end else begin
      result:=RoundAbsDown;
     end;
    end;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
 begin
  i:=BitPos shr WordBitShift;
  p:=BitPos and WordBitMask;
  Bit:=WordTopBit shr p;
  case RoundingMode of
   rmNearest:begin
    result:=RoundNear;
   end;
   rmTruncate:begin
    result:=RoundAbsDown;
   end;
   rmUp:begin
    if Negative then begin
     result:=RoundAbsDown;
    end else begin
     result:=RoundTowardsInfinity;
    end;
   end;
   rmDown:begin
    if Negative then begin
     result:=RoundTowardsInfinity;
    end else begin
     result:=RoundAbsDown;
    end;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
 function CountLeadingZeros32(a:TPasDblStrUtilsUInt32):TPasDblStrUtilsInt32;
 const CountLeadingZerosHigh:array[TPasDblStrUtilsUInt8] of TPasDblStrUtilsUInt8=(8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,
                                                  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 begin
  result:=0;
  if a<$10000 then begin
   inc(result,16);
   a:=a shl 16;
  end;
  if a<$1000000 then begin
   inc(result,8);
   a:=a shl 8;
  end;
  inc(result,CountLeadingZerosHigh[a shr 24]);
 end;
 function CountLeadingZeros64(a:TPasDblStrUtilsInt64):TPasDblStrUtilsInt32;
 begin
 if a<TPasDblStrUtilsInt64($100000000) then begin
   result:=32;
  end else begin
   result:=0;
   a:=a shr 32;
  end;
  inc(result,CountLeadingZeros32(a));
 end;
begin
 if assigned(OK) then begin
  OK^:=false;
 end;
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
 i:=1;
 while (i<=length(StringValue)) and (StringValue[i] in [#0..#32]) do begin
  inc(i);
 end;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  inc(i);
 end else begin
  Negative:=false;
 end;
 HasDigits:=false;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
  if assigned(OK) then begin
   OK^:=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
  if assigned(OK) then begin
   OK^:=true;
  end;
 end else if (Base in [2,4,8,16,32]) or ((not (Base in [2..36])) and ((((i+1)<=length(StringValue)) and ((StringValue[i]='0') and (StringValue[i+1] in ['b','o','x']))))) then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  case Base of
   2:begin
    Shift:=1;
   end;
   4:begin
    Shift:=2;
   end;
   8:begin
    Shift:=3;
   end;
   16:begin
    Shift:=4;
   end;
   32:begin
    Shift:=5;
   end;
   else begin
    inc(i);
    case StringValue[i] of
     'b':begin
      Shift:=1;
     end;
     'o':begin
      Shift:=3;
     end;
     else {'x':}begin
      Shift:=4;
     end;
    end;
    inc(i);
   end;
  end;
  TwoPower:=1 shl Shift;
  Value:=0;
  Exponent:=0;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   //q:=0;
   ExponentPower:=1;
   Digit:=0;
   while i<=length(StringValue) do begin
    c:=StringValue[i];
    if c='.' then begin
     if ExponentPower>0 then begin
      ExponentPower:=0;
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       if Done then begin
       end;
       break;
      end;
      continue;
     end else begin
      break;
     end;
    end else if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
     Digit:=ord(c)-ord('0');
    end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('a'))+10;
    end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('A'))+10;
    end else begin
     break;
    end;
    inc(i);
    HasDigits:=true;
    if ExponentPower<=0 then begin
     dec(ExponentPower);
    end;
    Value:=(Value shl Shift) or Digit;
    Overflow:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(Value shr 53));
    if Overflow<>0 then begin
     OverflowBits:=1;
     while Overflow>1 do begin
      inc(OverflowBits);
      Overflow:=Overflow shr 1;
     end;
     DroppedBitsMask:=(1 shl OverflowBits)-1;
     DroppedBits:=Value and DroppedBitsMask;
     Value:=Value shr OverflowBits;
     Exponent:=OverflowBits;
     ZeroTail:=true;
     while i<=length(StringValue) do begin
      c:=StringValue[i];
      if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
       Digit:=ord(c)-ord('0');
      end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       break;
      end;
      inc(i);
      if Digit<>0 then begin
       ZeroTail:=false;
      end;
      inc(Exponent,Shift);
     end;
     MiddleValue:=1 shl (OverflowBits-1);
     if DroppedBits>MiddleValue then begin
      inc(Value);
     end else if DroppedBits=MiddleValue then begin
      if ((Value and 1)<>0) or not ZeroTail then begin
       inc(Value);
      end;
     end;
     while (Value and Bit53)<>0 do begin
      Value:=Value shr 1;
      inc(Exponent);
     end;
     break;
    end;
   end;
   if ExponentPower>0 then begin
    ExponentPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if ExponentNegative then begin
    dec(ExponentPower,ExponentValue);
   end else begin
    inc(ExponentPower,ExponentValue);
   end;
   inc(Exponent,Shift*ExponentPower);
   Shift:=CountLeadingZeros64(Value);
   ExponentValue:=$432-((Shift-IEEEFormatExponent)-Exponent);
   if (((ExponentValue>$34) and (ExponentValue<$7fe)) and (Exponent<IEEEFormatExponent)) and (Value<Bit53) then begin
    dec(Shift,IEEEFormatExponent);
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((TPasDblStrUtilsInt64(ExponentValue) shl 52)+Value) and TPasDblStrUtilsInt64($7fffffffffffffff);
   end else begin
    New(Mantissa);
    try
     FillChar(Mantissa^,sizeof(TWords),#0);
     Value:=Value shl Shift;
     inc(Exponent,64-Shift);
     Mantissa^[0]:=(Value shr 48) and $ffff;
     Mantissa^[1]:=(Value shr 32) and $ffff;
     Mantissa^[2]:=(Value shr 16) and $ffff;
     Mantissa^[3]:=(Value shr 0) and $ffff;
     if (Mantissa^[0] and WordTopBit)<>0 then begin
      dec(Exponent);
      if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
       inc(Exponent,IEEEFormatExpMax-1);
       MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
        MantissaShiftRight(Mantissa^,1);
        inc(Exponent);
       end;
       if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end;
      end else if Exponent>0 then begin
       ResultCasted^.Hi:=$7ff00000;
       ResultCasted^.Lo:=$00000000;
      end else begin
       Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
       MantissaShiftRight(Mantissa^,Shift);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
        Exponent:=1;
        if IEEEFormatExplicit=0 then begin
         Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
        end;
        Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end else begin
        if MantissaIsZero(Mantissa^) then begin
         ResultCasted^.Hi:=$00000000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end;
      end;
     end else begin
      ResultCasted^.Hi:=$00000000;
      ResultCasted^.Lo:=$00000000;
     end;
    finally
     Dispose(Mantissa);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    OK^:=true;
   end;
  end;
 end else if Base in [2..9,11..36] then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   case RoundingMode of
    rmNearest:begin
     NewFPURoundingMode:=rmNearest;
    end;
    rmTruncate:begin
     NewFPURoundingMode:=rmTruncate;
    end;
    rmUp:begin
     NewFPURoundingMode:=rmUp;
    end;
    rmDown:begin
     NewFPURoundingMode:=rmDown;
    end;
    else begin
     NewFPURoundingMode:=rmNearest;
    end;
   end;
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
   try
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(NewFPURoundingMode);
    end;
    Part:=0;
    Multiplier:=1;
    Digit:=0;
    Done:=false;
    ExponentPower:=1;
    while not Done do begin
     while true do begin
      c:=StringValue[i];
      if c='.' then begin
       if ExponentPower>0 then begin
        ExponentPower:=0;
        inc(i);
        if i>length(StringValue) then begin
         Done:=true;
         break;
        end;
        continue;
       end else begin
        Done:=true;
        break;
       end;
      end else if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+Base)) then begin
       Digit:=ord(c)-ord('0');
      end else if (Base>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+Base)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (Base>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+Base)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       Done:=true;
       break;
      end;
      HasDigits:=true;
      NextMultiplier:=Multiplier*TPasDblStrUtilsUInt32(Base);
      if NextMultiplier>MaximumMultiplier then begin
       break;
      end;
      if ExponentPower<=0 then begin
       dec(ExponentPower);
      end;
      Part:=(Part*TPasDblStrUtilsUInt32(Base))+TPasDblStrUtilsUInt32(Digit);
      Multiplier:=NextMultiplier;
      Assert(Multiplier>Part);
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       break;
      end;
     end;
     ResultCasted^.Value:=(ResultCasted^.Value*Multiplier)+Part;
    end;
    if ExponentPower>0 then begin
     ExponentPower:=0;
    end;
    ExponentValue:=0;
    ExponentNegative:=false;
    if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
     inc(i);
     if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
      ExponentNegative:=StringValue[i]='-';
      inc(i);
     end;
     HasDigits:=false;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
      HasDigits:=true;
      inc(i);
     end;
    end;
    if ExponentNegative then begin
     dec(ExponentPower,ExponentValue);
    end else begin
     inc(ExponentPower,ExponentValue);
    end;
    if ExponentPower<>0 then begin
     ResultCasted^.Value:=ResultCasted^.Value*power(Base,ExponentPower);
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    OK^:=true;
   end;
  end;
 end else begin
  HasDigits:=false;
  Value:=0;
  StoredDigitPos:=i;
  DigitPos:=0;
  TenPower:=1;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  while i<=length(StringValue) do begin
   c:=StringValue[i];
   case c of
    '0'..'9':begin
     HasDigits:=true;
     Value:=(Value*10)+(ord(c)-ord('0'));
     inc(DigitPos);
     if (Value and InvBit53Mask)<>0 then begin
      HasDigits:=false;
      break;
     end;
     if TenPower<=0 then begin
      dec(TenPower);
      if TenPower>high(TenPowers) then begin
       HasDigits:=false;
       break;
      end;
     end;
    end;
    '.':begin
     if TenPower<=0 then begin
      HasDigits:=false;
      break;
     end else begin
      TenPower:=0;
     end;
    end;
    'e','E':begin
     break;
    end;
    else begin
     HasDigits:=false;
     break;
    end;
   end;
   inc(i);
  end;
  if HasDigits then begin
   if TenPower>0 then begin
    TenPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if Value=0 then begin
    TenPower:=0;
   end else begin
    if ExponentNegative then begin
     dec(TenPower,ExponentValue);
    end else begin
     inc(TenPower,ExponentValue);
    end;
    if TenPower<>0 then begin
     if TenPower>0 then begin
      if ((DigitPos+TenPower)>MaxFastPathDigits) or (TenPower>high(TenPowers)) then begin
       HasDigits:=false;
      end else begin
       Value:=Value*TenPowers[TenPower];
       TenPower:=0;
       if (Value and InvBit53Mask)<>0 then begin
        HasDigits:=false;
       end;
      end;
     end else begin
      if (-TenPower)>high(TenPowers) then begin
       HasDigits:=false;
      end else begin
       i:=-TenPower;
       while (i>0) and (TenPower<0) do begin
        if (Value mod TenPowers[i])=0 then begin
         Value:=Value div TenPowers[i];
         inc(TenPower,i);
        end else begin
         if (i and 1)=0 then begin
          i:=i shr 1;
         end else begin
          dec(i);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
  if HasDigits then begin
   if Value=0 then begin
    ResultCasted^.Hi:=$00000000;
    ResultCasted^.Lo:=$00000000;
   end else begin
    Shift:=CountLeadingZeros64(Value)-11;
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((TPasDblStrUtilsInt64($432-Shift) shl 52)+Value) and TPasDblStrUtilsInt64($7fffffffffffffff);
   end;
   if TenPower<>0 then begin
    case RoundingMode of
     rmNearest:begin
      NewFPURoundingMode:=rmNearest;
     end;
     rmTruncate:begin
      NewFPURoundingMode:=rmTruncate;
     end;
     rmUp:begin
      NewFPURoundingMode:=rmUp;
     end;
     rmDown:begin
      NewFPURoundingMode:=rmDown;
     end;
     else begin
      NewFPURoundingMode:=rmNearest;
     end;
    end;
    OldFPUExceptionMask:=GetExceptionMask;
    OldFPUPrecisionMode:=GetPrecisionMode;
    OldFPURoundingMode:=GetRoundMode;
    try
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(DtoAFPUExceptionMask);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(DtoAFPUPrecisionMode);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(NewFPURoundingMode);
     end;
     if TenPower>0 then begin
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[TenPower];
      end;
     end else begin
      TenPower:=-TenPower;
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[TenPower];
      end;
     end;
    finally
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(OldFPUExceptionMask);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(OldFPUPrecisionMode);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(OldFPURoundingMode);
     end;
    end;
   end;
   if Negative then begin
    ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
   end;
   if assigned(OK) then begin
    OK^:=true;
   end;
  end else begin
   i:=StoredDigitPos;
   GetMem(MantissaMultiplicator,SizeOf(TWords));
   GetMem(Mantissa,SizeOf(TWords));
   GetMem(Temp,SizeOf(TTemp));
   GetMem(Digits,SizeOf(TDigits));
   try
    FillChar(Digits^,SizeOf(TDigits),#0);

    DigitPos:=0;
    TenPower:=0;
    HasDigits:=false;
    Started:=false;
    ExponentNegative:=false;
    ExponentValue:=0;
    while (i<=length(StringValue)) and (StringValue[i]='0') do begin
     HasDigits:=true;
     inc(i);
    end;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     HasDigits:=true;
     Started:=true;
     if DigitPos<=high(TDigits) then begin
      Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
      inc(DigitPos);
     end;
     inc(TenPower);
     inc(i);
    end;
    if (i<=length(StringValue)) and (StringValue[i]='.') then begin
     inc(i);
     if not Started then begin
      while (i<=length(StringValue)) and (StringValue[i]='0') do begin
       HasDigits:=true;
       dec(TenPower);
       inc(i);
      end;
     end;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      HasDigits:=true;
      if DigitPos<=high(TDigits) then begin
       Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
       inc(DigitPos);
      end;
      inc(i);
     end;
    end;
    if HasDigits then begin
     if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
      inc(i);
      if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
       ExponentNegative:=StringValue[i]='-';
       inc(i);
      end;
      HasDigits:=false;
      while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
       ExponentValue:=(ExponentValue*10)+TPasDblStrUtilsInt32(ord(StringValue[i])-ord('0'));
       HasDigits:=true;
       inc(i);
      end;
     end;
     if HasDigits then begin
      if ExponentNegative then begin
       dec(TenPower,ExponentValue);
      end else begin
       inc(TenPower,ExponentValue);
      end;

      FillChar(Mantissa^,sizeof(TWords),#0);

      Bit:=WordTopBit;
      StoredDigitPos:=0;
      Started:=false;
      TwoPower:=0;
      MantissaPosition:=0;
      while MantissaPosition<MantissaWords do begin
       Carry:=0;
       while (DigitPos>StoredDigitPos) and (Digits^[DigitPos-1]=0) do begin
        dec(DigitPos);
       end;
       if DigitPos<=StoredDigitPos then begin
        break;
       end;
       DigitPosBackwards:=DigitPos;
       while DigitPosBackwards>StoredDigitPos do begin
        dec(DigitPosBackwards);
        i:=(2*Digits^[DigitPosBackwards])+Carry;
        if i>=10 then begin
         dec(i,10);
         Carry:=1;
        end else begin
         Carry:=0;
        end;
        Digits^[DigitPosBackwards]:=i;
       end;
       if Carry<>0 then begin
        Mantissa^[MantissaPosition]:=Mantissa^[MantissaPosition] or Bit;
        Started:=true;
       end;
       if Started then begin
        if Bit=1 then begin
         Bit:=WordTopBit;
         inc(MantissaPosition);
        end else begin
         Bit:=Bit shr 1;
        end;
       end else begin
        dec(TwoPower);
       end;
      end;
      inc(TwoPower,TenPower);

      if TenPower<0 then begin
       for i:=0 to high(TWords)-1 do begin
        MantissaMultiplicator^[i]:=$cccc;
       end;
       MantissaMultiplicator^[high(TWords)]:=$cccd;
       ExtraTwos:=-2;
       TenPower:=-TenPower;
      end else if TenPower>0 then begin
       MantissaMultiplicator^[0]:=$a000;
       for i:=1 to high(TWords) do begin
        MantissaMultiplicator^[i]:=$0000;
       end;
       ExtraTwos:=3;
      end else begin
       ExtraTwos:=0;
      end;
      while TenPower<>0 do begin
       if (TenPower and 1)<>0 then begin
        inc(TwoPower,ExtraTwos+MantissaMultiply(Mantissa,MantissaMultiplicator));
       end;
       inc(ExtraTwos,ExtraTwos+MantissaMultiply(MantissaMultiplicator,MantissaMultiplicator));
       TenPower:=TenPower shr 1;
      end;

      Exponent:=TwoPower;
      if (Mantissa^[0] and WordTopBit)<>0 then begin
       dec(Exponent);

       if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
        inc(Exponent,IEEEFormatExpMax-1);
        MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
         MantissaShiftRight(Mantissa^,1);
         inc(Exponent);
        end;
        if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
         ResultCasted^.Hi:=$7ff00000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end else if Exponent>0 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
        MantissaShiftRight(Mantissa^,Shift);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
         Exponent:=1;
         if IEEEFormatExplicit=0 then begin
          Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
         end;
         Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end else begin
         if MantissaIsZero(Mantissa^) then begin
          ResultCasted^.Hi:=$00000000;
          ResultCasted^.Lo:=$00000000;
         end else begin
          ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
          ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
         end;
        end;
       end;
      end else begin
       ResultCasted^.Hi:=$00000000;
       ResultCasted^.Lo:=$00000000;
      end;
      if Negative then begin
       ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
      end;
      if assigned(OK) then begin
       OK^:=true;
      end;
     end;
    end;
   finally
    FreeMem(MantissaMultiplicator);
    FreeMem(Mantissa);
    FreeMem(Temp);
    FreeMem(Digits);
   end;
  end;
 end;
end;

function ConvertDoubleToString(const AValue:TPasDblStrUtilsDouble;const OutputMode:TPasDblStrUtilsOutputMode=omStandard;RequestedDigits:TPasDblStrUtilsInt32=-1):TPasDblStrUtilsString;
const SignificantMantissaSize=64;
      MinimalTargetExponent=-60;
      MaximalTargetExponent=-32;
      ModeShortest=0;
      ModeFixed=1;
      ModePrecision=2;
      BigNumMaxSignificantMantissaBits=3584;
      BigitChunkSize=32;
      BigitDoubleChunkSize=64;
      BigitSize=28;
      BigitMask=(1 shl BigitSize)-1;
      BigNumCapacity=(BigNumMaxSignificantMantissaBits+(BigitSize-1)) div BigitSize;
type TDoubleValue=record
      SignificantMantissa:TPasDblStrUtilsUInt64;
      Exponent:TPasDblStrUtilsInt32;
     end;
     TBigNumChunk=TPasDblStrUtilsUInt32;
     TBigNumDoubleChunk=TPasDblStrUtilsUInt64;
     TBigNum=record
      Bigits:array[0..BigNumCapacity] of TBigNumChunk;
      UsedDigits:TPasDblStrUtilsInt32;
      Exponent:TPasDblStrUtilsInt32;
     end;
 function QWordLessOrEqual(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=(a=b) or (((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff))));
 end;
 function QWordGreaterOrEqual(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=(a=b) or (((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff))));
 end;
 function QWordLess(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff)));
 end;
 function QWordGreater(a,b:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 begin
  result:=((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff)));
 end;
 function DoubleValue(SignificantMantissa:TPasDblStrUtilsUInt64=0;Exponent:TPasDblStrUtilsInt32=0):TDoubleValue;
 begin
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure SplitDouble(Value:TPasDblStrUtilsDouble;var SignificantMantissa:TPasDblStrUtilsUInt64;var Exponent:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
 begin
  SignificantMantissa:=Casted and TPasDblStrUtilsUInt64($000fffffffffffff);
  if (Casted and TPasDblStrUtilsUInt64($7ff0000000000000))<>0 then begin
   inc(SignificantMantissa,TPasDblStrUtilsUInt64($0010000000000000));
   Exponent:=((Casted and TPasDblStrUtilsUInt64($7ff0000000000000)) shr 52)-($3ff+52);
  end else begin
   Exponent:=(-($3ff+52))+1;
  end;
 end;
 function DoubleValueGet(Value:TPasDblStrUtilsDouble):TDoubleValue;
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value>0);
  SplitDouble(Value,SignificantMantissa,Exponent);
  while (SignificantMantissa and TPasDblStrUtilsUInt64($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  SignificantMantissa:=SignificantMantissa shl (SignificantMantissaSize-53);
  dec(Exponent,SignificantMantissaSize-53);
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure DoubleValueSubtract(var Left:TDoubleValue;const Right:TDoubleValue);
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  dec(Left.SignificantMantissa,Right.SignificantMantissa);
 end;
 function DoubleValueMinus(const Left,Right:TDoubleValue):TDoubleValue;
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  result.Exponent:=Left.Exponent;
  result.SignificantMantissa:=Left.SignificantMantissa-Right.SignificantMantissa;
 end;
 procedure DoubleValueMuliply(var Left:TDoubleValue;const Right:TDoubleValue);
 var a,b,c,d,ac,bc,ad,bd:TPasDblStrUtilsUInt64;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  inc(Left.Exponent,Right.Exponent+64);
  Left.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(TPasDblStrUtilsUInt64(((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(TPasDblStrUtilsUInt64(1) shl 31)) shr 32);
 end;
 function DoubleValueMul(const Left,Right:TDoubleValue):TDoubleValue;
 var a,b,c,d,ac,bc,ad,bd:TPasDblStrUtilsUInt64;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  result.Exponent:=Left.Exponent+(Right.Exponent+64);
  a:=((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(TPasDblStrUtilsUInt64(1) shl 31);
  result.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(a shr 32);
 end;
 procedure DoubleValueNormalize(var Value:TDoubleValue);
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  Value.SignificantMantissa:=SignificantMantissa;
  Value.Exponent:=Exponent;
 end;
 function DoubleValueNorm(const Value:TDoubleValue):TDoubleValue;
 var SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent:TPasDblStrUtilsInt32;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and TPasDblStrUtilsUInt64($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 function BigNumNew:TBigNum;
 begin
  FillChar(result,sizeof(TBigNum),#0);
 end;
 procedure BigNumZero(var BigNum:TBigNum);
 begin
  BigNum.UsedDigits:=0;
  BigNum.Exponent:=0;
 end;
 procedure BigNumEnsureCapacity(var BigNum:TBigNum;Size:TPasDblStrUtilsInt32);
 begin
 end;
 procedure BigNumClamp(var BigNum:TBigNum);
 begin
  while (BigNum.UsedDigits>0) and (BigNum.Bigits[BigNum.UsedDigits-1]=0) do begin
   dec(BigNum.UsedDigits);
  end;
  if BigNum.UsedDigits=0 then begin
   BigNum.Exponent:=0;
  end;
 end;
 function BigNumIsClamped(const BigNum:TBigNum):TPasDblStrUtilsBoolean;
 begin
  result:=(BigNum.UsedDigits=0) or (BigNum.Bigits[BigNum.UsedDigits-1]<>0);
 end;
 procedure BigNumAlign(var BigNum:TBigNum;const Other:TBigNum);
 var ZeroDigits,i:TPasDblStrUtilsInt32;
 begin
  if BigNum.Exponent>Other.Exponent then begin
   ZeroDigits:=BigNum.Exponent-Other.Exponent;
   BigNumEnsureCapacity(BigNum,Bignum.UsedDigits+ZeroDigits);
   for i:=BigNum.UsedDigits-1 downto 0 do begin
    BigNum.Bigits[i+ZeroDigits]:=BigNum.Bigits[i];
   end;
   for i:=0 to ZeroDigits-1 do begin
    BigNum.Bigits[i]:=0;
   end;
   inc(BigNum.UsedDigits,ZeroDigits);
   dec(BigNum.Exponent,ZeroDigits);
   Assert(BigNum.UsedDigits>=0);
   Assert(BigNum.Exponent>=0);
  end;
 end;
 procedure BigNumAssignUInt16(var BigNum:TBigNum;Value:TPasDblStrUtilsUInt16);
 begin
  Assert(BigitSize>=(sizeof(TPasDblStrUtilsUInt16)*8));
  BigNumZero(BigNum);
  if Value<>0 then begin
   BigNumEnsureCapacity(BigNum,1);
   BigNum.Bigits[0]:=Value;
   BigNum.UsedDigits:=1;
  end;
 end;
 procedure BigNumAssignUInt64(var BigNum:TBigNum;Value:TPasDblStrUtilsUInt64);
 var i,j:TPasDblStrUtilsInt32;
 begin
  BigNumZero(BigNum);
  if Value<>0 then begin
   j:=(64 div BigitSize)+1;
   BigNumEnsureCapacity(BigNum,j);
   for i:=0 to j-1 do begin
    BigNum.Bigits[i]:=Value and BigitMask;
    Value:=Value shr BigitSize;
   end;
   BigNum.UsedDigits:=j;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumAssignBigNum(var BigNum:TBigNum;const Other:TBigNum);
 begin
  BigNum.Exponent:=Other.Exponent;
  BigNum.Bigits:=Other.Bigits;
  BigNum.UsedDigits:=Other.UsedDigits;
 end;
 procedure BigNumAddBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Carry,Sum:TBigNumChunk;
     BigitPos,i:TPasDblStrUtilsInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  BigNumAlign(BigNum,Other);
  BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+Other.UsedDigits);
  BigitPos:=Other.Exponent-BigNum.Exponent;
  Assert(BigitPos>=0);
  Carry:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Sum:=BigNum.Bigits[BigitPos]+Other.Bigits[i]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  while Carry<>0 do begin
   Sum:=BigNum.Bigits[BigitPos]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  if BigNum.UsedDigits<BigitPos then begin
   BigNum.UsedDigits:=BigitPos;
  end;
  Assert(BigNumIsClamped(BigNum));
 end;
 procedure BigNumAddUInt64(var BigNum:TBigNum;const Value:TPasDblStrUtilsUInt64);
 var Other:TBigNum;
 begin
  Other:=BigNumNew;
  BigNumAssignUInt64(Other,Value);
  BigNumAddBigNum(BigNum,Other);
 end;
 function BigNumBigitAt(const BigNum:TBigNum;Index:TPasDblStrUtilsInt32):TBigNumChunk;
 begin
  if (Index<BigNum.Exponent) or (Index>=(BigNum.UsedDigits+BigNum.Exponent)) then begin
   result:=0;
  end else begin
   result:=BigNum.Bigits[Index-BigNum.Exponent];
  end;
 end;
 function BigNumCompare(const a,b:TBigNum):TPasDblStrUtilsInt32;
 var la,lb,i,j:TPasDblStrUtilsInt32;
     ba,bb:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  if la<lb then begin
   result:=-1;
  end else if la>lb then begin
   result:=1;
  end else begin
   if a.Exponent<b.Exponent then begin
    j:=a.Exponent;
   end else begin
    j:=b.Exponent;
   end;
   result:=0;
   for i:=la-1 downto j do begin
    ba:=BigNumBigItAt(a,i);
    bb:=BigNumBigItAt(b,i);
    if ba<bb then begin
     result:=-1;
     break;
    end else if ba>bb then begin
     result:=1;
     break;
    end;
   end;
  end;
 end;
 function BigNumPlusCompare(const a,b,c:TBigNum):TPasDblStrUtilsInt32;
 var la,lb,lc,i,j:TPasDblStrUtilsInt32;
     ba,bb,bc,br,Sum:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  Assert(BigNumIsClamped(c));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  lc:=c.UsedDigits+c.Exponent;
  if la<lb then begin
   result:=BigNumPlusCompare(b,a,c);
  end else begin
   if (la+1)<lc then begin
    result:=-1;
   end else if la>lc then begin
    result:=1;
   end else if (a.Exponent>=lb) and (la<lc) then begin
    result:=-1;
   end else begin
    if a.Exponent<b.Exponent then begin
     if a.Exponent<c.Exponent then begin
      j:=a.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end else begin
     if b.Exponent<c.Exponent then begin
      j:=b.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end;
    br:=0;
    for i:=lc-1 downto j do begin
     ba:=BigNumBigItAt(a,i);
     bb:=BigNumBigItAt(b,i);
     bc:=BigNumBigItAt(c,i);
     Sum:=ba+bb;
     if Sum>(bc+br) then begin
      result:=1;
      exit;
     end else begin
      br:=(bc+br)-Sum;
      if br>1 then begin
       result:=-1;
       exit;
      end;
      br:=br shl BigitSize;
     end;
    end;
    if br=0 then begin
     result:=0;
    end else begin
     result:=-1;
    end;
   end;
  end;
 end;
 procedure BigNumSubtractBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Borrow,Difference:TBigNumChunk;
     i,Offset:TPasDblStrUtilsInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(BigNumCompare(Other,BigNum)<=0);
  BigNumAlign(BigNum,Other);
  Offset:=Other.Exponent-BigNum.Exponent;
  Borrow:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Assert((Borrow=0) or (Borrow=1));
   Difference:=(BigNum.Bigits[i+Offset]-Other.Bigits[i])-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
  end;
  i:=Other.UsedDigits;
  while Borrow<>0 do begin
   Difference:=BigNum.Bigits[i+Offset]-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
   inc(i);
  end;
  BigNumClamp(BigNum);
 end;
 procedure BigNumBigitsShiftLeft(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 var Carry,NextCarry:TBigNumChunk;
     i:TPasDblStrUtilsInt32;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  Carry:=0;
  for i:=0 to BigNum.UsedDigits-1 do begin
   NextCarry:=BigNum.Bigits[i] shr (BigitSize-Shift);
   BigNum.Bigits[i]:=((BigNum.Bigits[i] shl Shift)+Carry) and BigitMask;
   Carry:=NextCarry;
  end;
  if Carry<>0 then begin
   BigNum.Bigits[BigNum.UsedDigits]:=Carry;
   inc(BigNum.UsedDigits);
  end;
 end;
 procedure BigNumBigitsShiftRight(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 var Carry,NextCarry:TBigNumChunk;
     i:TPasDblStrUtilsInt32;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  if BigNum.UsedDigits>0 then begin
   Carry:=0;
   for i:=BigNum.UsedDigits-1 downto 1 do begin
    NextCarry:=BigNum.Bigits[i] shl (BigitSize-Shift);
    BigNum.Bigits[i]:=((BigNum.Bigits[i] shr Shift)+Carry) and BigitMask;
    Carry:=NextCarry;
   end;
   BigNum.Bigits[0]:=(BigNum.Bigits[0] shr Shift)+Carry;
  end;
  BigNumClamp(BigNum);
 end;
 procedure BignumSubtractTimes(var BigNum:TBigNum;const Other:TBigNum;Factor:TPasDblStrUtilsInt32);
 var i,ExponentDiff:TPasDblStrUtilsInt32;
     Borrow,Difference:TBigNumChunk;
     Product,Remove:TBigNumDoubleChunk;
 begin
  Assert(BigNum.Exponent<=Other.Exponent);
  if Factor<3 then begin
   for i:=1 to Factor do begin
    BigNumSubtractBignum(BigNum,Other);
   end;
  end else begin
   Borrow:=0;
   ExponentDiff:=Other.Exponent-BigNum.Exponent;
   for i:=0 to Other.UsedDigits-1 do begin
    Product:=TBigNumDoubleChunk(Factor)*Other.Bigits[i];
    Remove:=Borrow+Product;
    Difference:=BigNum.Bigits[i+ExponentDiff]-TBigNumChunk(Remove and BigitMask);
    BigNum.Bigits[i+ExponentDiff]:=Difference and BigitMask;
    Borrow:=TBigNumChunk((Difference shr (BigitChunkSize-1))+(Remove shr BigitSize));
   end;
   for i:=Other.UsedDigits+ExponentDiff to BigNum.UsedDigits-1 do begin
    if Borrow=0 then begin
     exit;
    end;
    Difference:=BigNum.Bigits[i]-Borrow;
    BigNum.Bigits[i]:=Difference and BigitMask;
    Borrow:=TBigNumChunk(Difference shr (BigitChunkSize-1));
   end;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumShiftLeft(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 begin
  if BigNum.UsedDigits<>0 then begin
   inc(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
   BigNumBigitsShiftLeft(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumShiftRight(var BigNum:TBigNum;Shift:TPasDblStrUtilsInt32);
 begin
  if BigNum.UsedDigits<>0 then begin
   dec(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits);
   BigNumBigitsShiftRight(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumMultiplyByUInt32(var BigNum:TBigNum;Factor:TPasDblStrUtilsUInt16);
 var Carry,Product:TPasDblStrUtilsUInt64;
     i:TPasDblStrUtilsInt32;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   for i:=0 to BigNum.UsedDigits-1 do begin
    Product:=(Factor*BigNum.Bigits[i])+Carry;
    BigNum.Bigits[i]:=Product and BigitMask;
    Carry:=Product shr BigitSize;
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumMultiplyByUInt64(var BigNum:TBigNum;Factor:TPasDblStrUtilsUInt64);
 var Carry,Low,High,ProductLow,ProductHigh,Tmp:TPasDblStrUtilsUInt64;
     i:TPasDblStrUtilsInt32;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   Low:=Factor and $ffffffff;
   High:=Factor shr 32;
   for i:=0 to BigNum.UsedDigits-1 do begin
    ProductLow:=Low*BigNum.Bigits[i];
    ProductHigh:=High*BigNum.Bigits[i];
    Tmp:=(Carry and BigitMask)+ProductLow;
    BigNum.Bigits[i]:=Tmp and BigitMask;
    Carry:=(Carry shr BigitSize)+(Tmp shr BigitSize)+(ProductHigh shl (32-BigitSize));
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumSquare(var BigNum:TBigNum);
 var ProductLength,CopyOffset,i,BigitIndex1,BigitIndex2:TPasDblStrUtilsInt32;
     Accumulator:TBigNumDoubleChunk;
     Chunk1,Chunk2:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(BigNum));
  ProductLength:=2*BigNum.UsedDigits;
  BigNumEnsureCapacity(BigNum,ProductLength);
  Assert(not ((1 shl (2*(BigItChunkSize-BigitSize)))<=BigNum.UsedDigits));
  Accumulator:=0;
  CopyOffset:=BigNum.UsedDigits;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigNum.Bigits[i+CopyOffset]:=BigNum.Bigits[i];
  end;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigitIndex1:=i;
   BigitIndex2:=0;
   while BigitIndex1>=0 do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  for i:=BigNum.UsedDigits-1 to ProductLength-1 do begin
   BigitIndex1:=BigNum.UsedDigits-1;
   BigitIndex2:=i-BigitIndex1;
   while BigitIndex2<BigNum.UsedDigits do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  Assert(Accumulator=0);
  BigNum.UsedDigits:=ProductLength;
  inc(BigNum.Exponent,BigNum.Exponent);
  BigNumClamp(BigNum);
 end;
 procedure BigNumAssignPowerUInt16(var BigNum:TBigNum;Base:TPasDblStrUtilsUInt16;PowerExponent:TPasDblStrUtilsInt32);
 var Shifts,BitSize,TmpBase,FinalSize,Mask:TPasDblStrUtilsInt32;
     ThisValue:TPasDblStrUtilsUInt64;
     DelayedMultipliciation:TPasDblStrUtilsBoolean;
 begin
  Assert(Base<>0);
  Assert(PowerExponent>=0);
  if PowerExponent=0 then begin
   BigNumAssignUInt16(BigNum,1);
  end else begin
   BigNumZero(BigNum);
   Shifts:=0;
   while (Base and 1)=0 do begin
    Base:=Base shr 1;
    inc(Shifts);
   end;
   BitSize:=0;
   TmpBase:=Base;
   while TmpBase<>0 do begin
    TmpBase:=TmpBase shr 1;
    inc(BitSize);
   end;
   FinalSize:=BitSize*PowerExponent;
   BigNumEnsureCapacity(BigNum,FinalSize);
   Mask:=1;
   while Mask<=PowerExponent do begin
    inc(Mask,Mask);
   end;
   Mask:=Mask shr 2;
   ThisValue:=Base;
   DelayedMultipliciation:=false;
   while (Mask<>0) and (ThisValue<=$ffffffff) do begin
    ThisValue:=ThisValue*ThisValue;
    if (PowerExponent and Mask)<>0 then begin
     if (ThisValue and not ((TPasDblStrUtilsUInt64(1) shl (64-BitSize))-1))=0 then begin
      ThisValue:=ThisValue*Base;
     end else begin
      DelayedMultipliciation:=true;
     end;
    end;
    Mask:=Mask shr 1;
   end;
   BigNumAssignUInt64(BigNum,ThisValue);
   if DelayedMultipliciation then begin
    BigNumMultiplyByUInt32(BigNum,Base);
   end;
   while Mask<>0 do begin
    BigNumSquare(BigNum);
    if (PowerExponent and Mask)<>0 then begin
     BigNumMultiplyByUInt32(BigNum,Base);
    end;
    Mask:=Mask shr 1;
   end;
   BigNumShiftLeft(BigNum,Shifts*PowerExponent);
  end;
 end;
 function BigNumDivideModuloIntBigNum(var BigNum:TBigNum;const Other:TBigNum):TPasDblStrUtilsUInt16;
 var ThisBigit,OtherBigit:TBigNumChunk;
     Quotient,DivisionEstimate:TPasDblStrUtilsUInt32;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(Other.UsedDigits>0);
  result:=0;
  if (BigNum.UsedDigits+BigNum.Exponent)>=(Other.UsedDigits+Other.Exponent) then begin
   BigNumAlign(BigNum,Other);
   while (BigNum.UsedDigits+BigNum.Exponent)>(Other.UsedDigits+Other.Exponent) do begin
    Assert(Other.Bigits[Other.UsedDigits-1]>=((1 shl BigitSize) div 16));
    inc(result,BigNum.Bigits[BigNum.UsedDigits-1]);
    BigNumSubtractTimes(BigNum,Other,BigNum.Bigits[BigNum.UsedDigits-1]);
   end;
   Assert((BigNum.UsedDigits+BigNum.Exponent)=(Other.UsedDigits+Other.Exponent));
   ThisBigit:=BigNum.Bigits[BigNum.UsedDigits-1];
   OtherBigit:=Other.Bigits[Other.UsedDigits-1];
   if Other.UsedDigits=1 then begin
    Quotient:=ThisBigit div OtherBigit;
    BigNum.Bigits[BigNum.UsedDigits-1]:=ThisBigit-(OtherBigit*Quotient);
    inc(result,Quotient);
    BigNumClamp(BigNum);
   end else begin
    DivisionEstimate:=ThisBigit div (OtherBigit+1);
    inc(result,DivisionEstimate);
    BigNumSubtractTimes(BigNum,Other,DivisionEstimate);
    if (OtherBigit*(DivisionEstimate+1))<=ThisBigit then begin
     while BigNumCompare(Other,BigNum)<=0 do begin
      BigNumSubtractBigNum(BigNum,Other);
      inc(result);
     end;
    end;
   end;
  end;
 end;
 function BigNumDivideModuloInt(var BigNum:TBigNum;Divisor:TPasDblStrUtilsUInt16):TPasDblStrUtilsUInt16;
 var q0,r0,q1,r1:TPasDblStrUtilsUInt64;
     i:integer;
 begin
  Assert(BigNumIsClamped(BigNum));
  q0:=0;
  for i:=BigNum.UsedDigits-1 downto 1 do begin
   q1:=(BigNum.Bigits[i] div Divisor)+q0;
   r1:=((BigNum.Bigits[i] mod Divisor) shl 16)+(BigNum.Bigits[i-1] shr 16);
   q0:=((r1 div Divisor) shl 16);
   r0:=r1 mod Divisor;
   BigNum.Bigits[i]:=q1;
   BigNum.Bigits[i-1]:=(r0 shl 16)+(BigNum.Bigits[i-1] and $ffff);
  end;
  q1:=(BigNum.Bigits[0] div Divisor)+q0;
  r1:=BigNum.Bigits[0] mod Divisor;
  BigNum.Bigits[0]:=q1;
  result:=r1;
  BigNumClamp(BigNum);
 end;
 function NormalizedExponent(SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  Assert(SignificantMantissa<>0);
  while (SignificantMantissa and TPasDblStrUtilsUInt64($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result:=Exponent;
 end;
 function GetEstimatePower(Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  result:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(((Exponent+52)*TPasDblStrUtilsInt64(1292913986))-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*0.30102999566398114)-(1e-10)));
 end;
 function GetEstimatePowerOf(Exponent,Radix:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
 begin
  result:=TPasDblStrUtilsInt32(TPasDblStrUtilsInt64(((Exponent+52)*DoubleToStringEstimatePowerFactorTable[Radix])-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*(ln(2)/ln(Radix)))-(1e-10)));
 end;
 procedure GenerateShortestDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:TPasDblStrUtilsBoolean;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 var Digit,Compare:TPasDblStrUtilsInt32;
     InDeltaRoomMinus,InDeltaRoomPlus:TPasDblStrUtilsBoolean;
 begin
  Len:=0;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,10);
    BigNumMultiplyByUInt32(DeltaMinus,10);
    BigNumMultiplyByUInt32(DeltaPlus,10);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(Buffer[Len]<>'9');
     inc(Buffer[Len]);
    end else begin
     if ((ord(Buffer[Len])-ord('0')) and 1)<>0 then begin
      Assert(Buffer[Len]<>'9');
      inc(Buffer[Len]);
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(Buffer[Len]<>'9');
    inc(Buffer[Len]);
    exit;
   end;
  end;
 end;
 procedure GenerateCountedDigits(Count:TPasDblStrUtilsInt32;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator:TBigNum;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 var i,Digit:TPasDblStrUtilsInt32;
 begin
  Assert(Count>=0);
  for i:=1 to Count-1 do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
   BigNumMultiplyByUInt32(Numerator,10);
  end;
  Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
  if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
   inc(Digit);
  end;
  inc(Len);
  if Len>=length(Buffer) then begin
   SetLength(Buffer,Len*2);
  end;
  Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
  for i:=Len downto 2 do begin
   if ord(Buffer[i])<>(ord('0')+10) then begin
    break;
   end;
   Buffer[i]:='0';
   inc(Buffer[i-1]);
  end;
  if ord(Buffer[1])=(ord('0')+10) then begin
   Buffer[1]:='1';
   inc(DecimalPoint);
  end;
 end;
 procedure GenerateFixedDigits(RequestedDigits:TPasDblStrUtilsInt32;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator:TBigNum;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
 begin
  if (-DecimalPoint)>RequestedDigits then begin
   DecimalPoint:=-RequestedDigits;
   Len:=0;
  end else if (-DecimalPoint)=RequestedDigits then begin
   Assert(DecimalPoint=(-RequestedDigits));
   BigNumMultiplyByUInt32(Denominator,10);
   if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
    Buffer:='1';
    Len:=1;
  end else begin
    Len:=0;
   end;
  end else begin
   GenerateCountedDigits(DecimalPoint+RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
  end;
 end;
 procedure FixupMultiplyBase(EstimatedPower:TPasDblStrUtilsInt32;IsEven:TPasDblStrUtilsBoolean;var DecimalPoint:TPasDblStrUtilsInt32;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 var InRange:TPasDblStrUtilsBoolean;
 begin
  if IsEven then begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
  end else begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
  end;
  if InRange then begin
   DecimalPoint:=EstimatedPower+1;
  end else begin
   DecimalPoint:=EstimatedPower;
   BigNumMultiplyByUInt32(Numerator,Base);
   if BigNumCompare(DeltaMinus,DeltaPlus)=0 then begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumAssignBigNum(DeltaPlus,DeltaMinus);
   end else begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumMultiplyByUInt32(DeltaPlus,Base);
   end;
  end;
 end;
 procedure InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  Assert(EstimatedPower>=0);

  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumShiftLeft(Numerator,Exponent);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumShiftLeft(DeltaPlus,Exponent);

   BigNumAssignUInt16(DeltaMinus,1);
   BigNumShiftLeft(DeltaMinus,Exponent);

   if (Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumAssignUInt16(DeltaMinus,1);

   if (Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  BigNumAssignPowerUInt16(Numerator,Base,-EstimatedPower);
  if NeedBoundaryDeltas then begin
   BigNumAssignBigNum(DeltaPlus,Numerator);
   BigNumAssignBigNum(DeltaMinus,Numerator);
  end;
  BigNumMultiplyByUInt64(Numerator,SignificantMantissa);

  BigNumAssignUInt16(Denominator,1);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);
   if ((Casted and TPasDblStrUtilsUInt64($000fffffffffffff))=0) and ((Casted and TPasDblStrUtilsUInt64($7ff0000000000000))<>TPasDblStrUtilsUInt64($0010000000000000)) then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValues(Casted,SignificantMantissa:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;EstimatedPower:TPasDblStrUtilsInt32;NeedBoundaryDeltas:TPasDblStrUtilsBoolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:TPasDblStrUtilsInt32);
 begin
  if Exponent>=0 then begin
   InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else if EstimatedPower>=0 then begin
   InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else begin
   InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end;
 end;
 procedure DoubleToDecimal(Value:TPasDblStrUtilsDouble;Mode,RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
     SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent,EstimatedPower:TPasDblStrUtilsInt32;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:TPasDblStrUtilsBoolean;
 begin
  Assert(Value>0);
  Assert(IsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePower(NormalizedExponent(SignificantMantissa,Exponent));
  if (Mode=ModeFixed) and (((-EstimatedPower)-1)>RequestedDigits) then begin
   Buffer:='';
   Len:=0;
   DecimalPoint:=-RequestedDigits;
  end else begin
   Assert(BigNumMaxSignificantMantissaBits>=(324*4));
   NeedBoundaryDeltas:=Mode=ModeShortest;
   InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   case Mode of
    ModeShortest:begin
     GenerateShortestDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len);
    end;
    ModeFixed:begin
     GenerateFixedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
    else {ModePrecision:}begin
     GenerateCountedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
   end;
  end;
 end;
 procedure GenerateRadixDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:TPasDblStrUtilsBoolean;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32;Radix:TPasDblStrUtilsInt32);
 const Base36:array[0..36] of TPasDblStrUtilsChar='0123456789abcdefghijklmnopqrstuvwxyz{';
 var Digit,Compare,MaxDigit:TPasDblStrUtilsInt32;
     InDeltaRoomMinus,InDeltaRoomPlus:TPasDblStrUtilsBoolean;
  function ValueOf(c:TPasDblStrUtilsChar):TPasDblStrUtilsInt32;
  begin
   case c of
    '0'..'9':begin
     result:=ord(c)-ord('0');
    end;
    else begin
     result:=(ord(c)-ord('a'))+$a;
    end;
   end;
  end;
 begin
  Len:=0;
  MaxDigit:=Radix-1;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=MaxDigit));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=Base36[Digit];
   BigNumClamp(Numerator);
   BigNumClamp(DeltaMinus);
   BigNumClamp(DeltaPlus);
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,Radix);
    BigNumMultiplyByUInt32(DeltaMinus,Radix);
    BigNumMultiplyByUInt32(DeltaPlus,Radix);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(ValueOf(Buffer[Len])<>MaxDigit);
     Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    end else begin
     if (ValueOf(Buffer[Len]) and 1)<>0 then begin
      Assert(ValueOf(Buffer[Len])<>MaxDigit);
      Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(ValueOf(Buffer[Len])<>MaxDigit);
    Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    exit;
   end;
  end;
 end;
 procedure DoubleToRadix(Value:TPasDblStrUtilsDouble;Radix:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 var Casted:TPasDblStrUtilsUInt64 absolute Value;
     SignificantMantissa:TPasDblStrUtilsUInt64;
     Exponent,EstimatedPower:TPasDblStrUtilsInt32;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:TPasDblStrUtilsBoolean;
 begin
  Assert(Value>0);
  Assert(IsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePowerOf(NormalizedExponent(SignificantMantissa,Exponent),Radix);
  Assert(BigNumMaxSignificantMantissaBits>=(324*4));
  NeedBoundaryDeltas:=true;
  InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  GenerateRadixDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len,Radix);
 end;
 {$warnings off}
 procedure FastDoubleToRadix(v:TPasDblStrUtilsDouble;Radix:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
 const Base36:array[0..35] of TPasDblStrUtilsChar='0123456789abcdefghijklmnopqrstuvwxyz';
       DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
       DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
       DtoAFPURoundingMode:TFPURoundingMode=rmNEAREST;
 var IntPart,FracPart,Old,Epsilon:TPasDblStrUtilsDouble;
     Digit,i,j:TPasDblStrUtilsInt32;
     TempBuffer:TPasDblStrUtilsString;
     OldFPUExceptionMask:TFPUExceptionMask;
     OldFPUPrecisionMode:TFPUPrecisionMode;
     OldFPURoundingMode:TFPURoundingMode;
     IntPart64:TPasDblStrUtilsInt64;
 begin
  if (Radix<2) or (Radix>36) then begin
   result:='';
  end else begin
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
   try
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(DtoAFPURoundingMode);
    end;
    try
     TempBuffer:='';
     IntPart:=System.Int(v);
     FracPart:=System.Frac(v);
     if IntPart=0 then begin
      result:='0';
     end else begin
      if IntPart<4294967295.0 then begin
       IntPart64:=trunc(IntPart);
       while IntPart64>0 do begin
        Digit:=IntPart64 mod Radix;
        Assert((Digit>=0) and (Digit<Radix));
        IntPart64:=IntPart64 div Radix;
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end else begin
       while IntPart>0 do begin
        Old:=IntPart;
        IntPart:=System.Int(IntPart/Radix);
        Digit:=trunc(Old-(IntPart*Radix));
        Assert((Digit>=0) and (Digit<Radix));
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end;
      SetLength(Buffer,Len);
      j:=1;
      for i:=Len downto 1 do begin
       Buffer[j]:=TempBuffer[i];
       inc(j);
      end;
     end;
     if FracPart<>0 then begin
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:='.';
      Epsilon:=0.001/Radix;
      while (FracPart>=Epsilon) and (Len<32) do begin
       FracPart:=FracPart*Radix;
       Digit:=trunc(FracPart);
       FracPart:=System.Frac(FracPart);
       Assert((Digit>=0) and (Digit<Radix));
       inc(Len);
       if Len>=length(Buffer) then begin
        SetLength(Buffer,Len*2);
       end;
       Buffer[Len]:=Base36[Digit];
      end;
     end;
    finally
     TempBuffer:='';
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end;
 end;
 {$warnings on}
 function GetCachedPowerForBinaryExponentRange(MinExponent,MaxExponent:TPasDblStrUtilsInt32;var Power:TDoubleValue;var DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var Index:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if (low(DoubleToStringPowerOfTenBinaryExponentTable)<=MinExponent) and (MinExponent<=high(DoubleToStringPowerOfTenBinaryExponentTable)) then begin
   Index:=DoubleToStringPowerOfTenBinaryExponentTable[MinExponent];
   if ((Index>=0) and (Index<length(DoubleToStringPowerOfTenTable))) and ((MinExponent<=DoubleToStringPowerOfTenTable[Index,1]) and (DoubleToStringPowerOfTenTable[Index,1]<=MaxExponent)) then begin
    Power.SignificantMantissa:=DoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=DoubleToStringPowerOfTenTable[Index,1];
    DecimalExponent:=DoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function GetCachedPowerForDecimalExponent(RequestedExponent:TPasDblStrUtilsInt32;var Power:TDoubleValue;var FoundExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var Index:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if (low(DoubleToStringPowerOfTenDecimalExponentTable)<=RequestedExponent) and (RequestedExponent<=high(DoubleToStringPowerOfTenDecimalExponentTable)) then begin
   Index:=DoubleToStringPowerOfTenDecimalExponentTable[RequestedExponent];
   if (Index>=0) and (Index<length(DoubleToStringPowerOfTenTable)) then begin
    Power.SignificantMantissa:=DoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=DoubleToStringPowerOfTenTable[Index,1];
    FoundExponent:=DoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function RoundWeed(var Buffer:TPasDblStrUtilsString;Len:TPasDblStrUtilsInt32;DistanceTooHighW,UnsafeInterval,Rest,TenCapacity,UnitValue:TPasDblStrUtilsUInt64):TPasDblStrUtilsBoolean;
 var SmallDistance,BigDistance:TPasDblStrUtilsUInt64;
 begin
  SmallDistance:=DistanceTooHighW-UnitValue;
  BigDistance:=DistanceTooHighW+UnitValue;
  Assert(QWordLessOrEqual(Rest,UnsafeInterval));
  while (QWordLess(Rest,SmallDistance) and (QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity))) and (QWordLess(Rest+TenCapacity,SmallDistance) or QWordGreaterOrEqual(SmallDistance-Rest,((Rest+TenCapacity)-SmallDistance))) do begin
   dec(Buffer[Len]);
   inc(Rest,TenCapacity);
  end;
  if ((QWordLess(Rest,BigDistance) and QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity)) and (QWordLess(Rest+TenCapacity,BigDistance) or QWordGreater(BigDistance-Rest,((Rest+TenCapacity)-BigDistance)))) then begin
   result:=false;
  end else begin
   result:=(QWordLessOrEqual(2*UnitValue,Rest) and QWordLessOrEqual(Rest,UnsafeInterval-(4*UnitValue)));
  end;
 end;
 function RoundWeedCounted(var Buffer:TPasDblStrUtilsString;Len:TPasDblStrUtilsInt32;Rest,TenCapacity,UnitValue:TPasDblStrUtilsUInt64;var Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var i:TPasDblStrUtilsInt32;
 begin
  Assert(QWordLess(Rest,TenCapacity));
  result:=false;
  if QWordGreater(TenCapacity-UnitValue,UnitValue) then begin
   result:=QWordGreater(TenCapacity-Rest,Rest) and QWordGreaterOrEqual(TenCapacity-(2*Rest),2*UnitValue);
   if not result then begin
    result:=QWordGreater(Rest,UnitValue) and QWordLessOrEqual(TenCapacity-(Rest-UnitValue),Rest-UnitValue);
    if result then begin
     inc(Buffer[Len]);
     for i:=Len downto 2 do begin
      if ord(Buffer[i])<>(ord('0')+10) then begin
       break;
      end;
      Buffer[i]:='0';
      inc(Buffer[i-1]);
     end;
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(Capacity);
    end;
   end;
  end;
 end;
 function BiggestPowerTen(Number:TPasDblStrUtilsUInt32;NumberBits:TPasDblStrUtilsInt32;var Power:TPasDblStrUtilsUInt32;var Exponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 label c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11;
 begin
  result:=true;
  case NumberBits of
   30,31,32:begin
    c1:
    if 1000000000<=Number then begin
     Power:=1000000000;
     Exponent:=9;
    end else begin
     goto c2;
    end;
   end;
   27,28,29:begin
    c2:
    if 100000000<=Number then begin
     Power:=100000000;
     Exponent:=8;
    end else begin
     goto c3;
    end;
   end;
   24,25,26:begin
    c3:
    if 10000000<=Number then begin
     Power:=10000000;
     Exponent:=7;
    end else begin
     goto c4;
    end;
   end;
   20,21,22,23:begin
    c4:
    if 1000000<=Number then begin
     Power:=1000000;
     Exponent:=6;
    end else begin
     goto c5;
    end;
   end;
   17,18,19:begin
    c5:
    if 100000<=Number then begin
     Power:=100000;
     Exponent:=5;
    end else begin
     goto c6;
    end;
   end;
   14,15,16:begin
    c6:
    if 10000<=Number then begin
     Power:=10000;
     Exponent:=4;
    end else begin
     goto c7;
    end;
   end;
   10,11,12,13:begin
    c7:
    if 1000<=Number then begin
     Power:=1000;
     Exponent:=3;
    end else begin
     goto c8;
    end;
   end;
   7,8,9:begin
    c8:
    if 100<=Number then begin
     Power:=100;
     Exponent:=2;
    end else begin
     goto c9;
    end;
   end;
   4,5,6:begin
    c9:
    if 10<=Number then begin
     Power:=10;
     Exponent:=1;
    end else begin
     goto c10;
    end;
   end;
   1,2,3:begin
    c10:
    if 1<=Number then begin
     Power:=1;
     Exponent:=0;
    end else begin
     goto c11;
    end;
   end;
   0:begin
    c11:
    Power:=0;
    Exponent:=-1;
   end;
   else begin
    Power:=0;
    Exponent:=0;
    result:=false;
   end;
  end;
 end;
 function DigitGen(Low,w,High:TDoubleValue;var Buffer:TPasDblStrUtilsString;var Len,Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var UnitValue,Fractionals,Rest:TPasDblStrUtilsUInt64;
     TooLow,TooHigh,UnsafeInterval,One:TDoubleValue;
     Integrals,Divisor,Digit:TPasDblStrUtilsUInt32;
     DivisorExponent:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if ((Low.Exponent=w.Exponent) and (w.Exponent=High.Exponent)) and (QWordLessOrEqual(Low.SignificantMantissa+1,High.SignificantMantissa-1) and
     ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent))) then begin
   UnitValue:=1;
   TooLow.SignificantMantissa:=Low.SignificantMantissa-UnitValue;
   TooLow.Exponent:=Low.Exponent;
   TooHigh.SignificantMantissa:=High.SignificantMantissa+UnitValue;
   TooHigh.Exponent:=High.Exponent;
   UnsafeInterval:=DoubleValueMinus(TooHigh,TooLow);
   One.SignificantMantissa:=TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=TooHigh.SignificantMantissa shr (-One.Exponent);
   Fractionals:=TooHigh.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(Capacity);
     Rest:=TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals) shl (-One.Exponent))+Fractionals;
     if QWordLess(Rest,UnsafeInterval.SignificantMantissa) then begin
      result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa,UnsafeInterval.SignificantMantissa,Rest,TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent),UnitValue);
      exit;
     end;
     Divisor:=Divisor div 10;
    end;
    if (One.Exponent>=-60) and (QWordLess(Fractionals,One.SignificantMantissa) and QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),One.SignificantMantissa)) then begin
     while true do begin
      Fractionals:=Fractionals*10;
      UnitValue:=UnitValue*10;
      UnsafeInterval.SignificantMantissa:=UnsafeInterval.SignificantMantissa*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
      if QWordLess(Fractionals,UnsafeInterval.SignificantMantissa) then begin
       result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa*UnitValue,UnsafeInterval.SignificantMantissa,Fractionals,One.SignificantMantissa,UnitValue);
       exit;
      end;
     end;
    end;
   end;
  end;
 end;
 function DigitGenCounted(w:TDoubleValue;RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,Capacity:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var wError,Fractionals,Rest:TPasDblStrUtilsUInt64;
     One:TDoubleValue;
     Integrals,Divisor,Digit:TPasDblStrUtilsUInt32;
     DivisorExponent:TPasDblStrUtilsInt32;
 begin
  result:=false;
  if ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent)) and ((MinimalTargetExponent>=-60) and (MaximalTargetExponent<=-32)) then begin
   wError:=1;
   One.SignificantMantissa:=TPasDblStrUtilsUInt64(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=w.SignificantMantissa shr (-One.Exponent);
   Fractionals:=w.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(RequestedDigits);
     dec(Capacity);
     if RequestedDigits=0 then begin
      break;
     end;
     Divisor:=Divisor div 10;
    end;
    if RequestedDigits=0 then begin
     Rest:=TPasDblStrUtilsUInt64(TPasDblStrUtilsUInt64(Integrals) shl (-One.Exponent))+Fractionals;
     result:=RoundWeedCounted(Buffer,Len,Rest,TPasDblStrUtilsUInt64(Divisor) shl (-One.Exponent),wError,Capacity);
     exit;
    end;
    if ((One.Exponent>=-60) and QWordLess(Fractionals,One.SignificantMantissa)) and QWordGreaterOrEqual(TPasDblStrUtilsUInt64($1999999999999999),One.SignificantMantissa) then begin
     while (RequestedDigits>0) and (Fractionals>wError) do begin
      Fractionals:=Fractionals*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
      dec(RequestedDigits);
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
     end;
     if RequestedDigits=0 then begin
      result:=RoundWeedCounted(Buffer,Len,Fractionals,One.SignificantMantissa,wError,Capacity);
     end else begin
      result:=false;
     end;
    end;
   end;
  end;
 end;
 procedure NormalizedBoundaries(Value:TPasDblStrUtilsDouble;var BoundaryMinus,BoundaryPlus:TDoubleValue);
 var v:TDoubleValue;
     SignificantMantissaIsZero:TPasDblStrUtilsBoolean;
 begin
  Assert(not IsNegative(Value));
  Assert(IsFinite(Value));
  SplitDouble(Value,v.SignificantMantissa,v.Exponent);
  SignificantMantissaIsZero:=v.SignificantMantissa=TPasDblStrUtilsUInt64($0010000000000000);
  BoundaryPlus.SignificantMantissa:=(v.SignificantMantissa shl 1)+1;
  BoundaryPlus.Exponent:=v.Exponent-1;
  DoubleValueNormalize(BoundaryPlus);
  if SignificantMantissaIsZero and (v.Exponent<>((-($3ff+52))+1)) then begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 2)-1;
   BoundaryMinus.Exponent:=v.Exponent-2;
  end else begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 1)-1;
   BoundaryMinus.Exponent:=v.Exponent-1;
  end;
  BoundaryMinus.SignificantMantissa:=BoundaryMinus.SignificantMantissa shl (BoundaryMinus.Exponent-BoundaryPlus.Exponent);
  BoundaryMinus.Exponent:=BoundaryPlus.Exponent;
 end;
 function DoFastShortest(Value:TPasDblStrUtilsDouble;var Buffer:TPasDblStrUtilsString;var Len,DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var w,BoundaryMinus,BoundaryPlus,TenMK,ScaledW,ScaledBoundaryMinus,ScaledBoundaryPlus:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:TPasDblStrUtilsInt32;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  NormalizedBoundaries(Value,BoundaryMinus,BoundaryPlus);
  Assert(BoundaryPlus.Exponent=w.Exponent);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    if ScaledW.Exponent=(BoundaryPlus.Exponent+TenMK.Exponent+SignificantMantissaSize) then begin
     ScaledBoundaryMinus:=DoubleValueMul(BoundaryMinus,TenMK);
     ScaledBoundaryPlus:=DoubleValueMul(BoundaryPlus,TenMK);
     Capacity:=0;
     result:=DigitGen(ScaledBoundaryMinus,ScaledW,ScaledBoundaryPlus,Buffer,Len,Capacity);
     DecimalExponent:=Capacity-mK;
    end;
   end;
  end;
 end;
 function DoFastPrecision(Value:TPasDblStrUtilsDouble;RequestedDigits:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalExponent:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 var w,TenMK,ScaledW:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:TPasDblStrUtilsInt32;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    Capacity:=0;
    result:=DigitGenCounted(ScaledW,RequestedDigits,Buffer,Len,Capacity);
    DecimalExponent:=Capacity-mK;
   end;
  end;
 end;
 function DoFastFixed(Value:TPasDblStrUtilsDouble;FracitionalCount:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
 const Five17=$b1a2bc2ec5; // 5^17
 type TInt128=record
       High,Low:TPasDblStrUtilsUInt64;
      end;
  procedure Int128Mul(var a:TInt128;const Multiplicand:TPasDblStrUtilsUInt32);
  var Accumulator:TPasDblStrUtilsUInt64;
      Part:TPasDblStrUtilsUInt32;
  begin
   Accumulator:=(a.Low and $ffffffff)*Multiplicand;
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.Low shr 32)*Multiplicand);
   a.Low:=(Accumulator shl 32)+Part;
   Accumulator:=(Accumulator shr 32)+((a.High and $ffffffff)*Multiplicand);
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.High shr 32)*Multiplicand);
   a.High:=(Accumulator shl 32)+Part;
   Assert((Accumulator shr 32)=0);
  end;
  procedure Int128Shift(var a:TInt128;const Shift:TPasDblStrUtilsInt32);
  begin
   Assert(((-64)<=Shift) and (Shift<=64));
   if Shift<>0 then begin
    if Shift=-64 then begin
     a.High:=a.Low;
     a.Low:=0;
    end else if Shift=64 then begin
     a.Low:=a.High;
     a.High:=0;
    end else if Shift<=0 then begin
     a.High:=(a.High shl (-Shift))+(a.Low shr (64+Shift));
     a.Low:=a.Low shl (-Shift);
    end else begin
     a.Low:=(a.Low shr Shift)+(a.High shl (64-Shift));
     a.High:=a.High shr Shift;
    end;
   end;
  end;
  function Int128DivModPowerOfTwo(var a:TInt128;const Power:TPasDblStrUtilsInt32):TPasDblStrUtilsInt32;
  begin
   if Power>=64 then begin
    result:=a.High shr (Power-64);
    dec(a.High,result shl (Power-64));
   end else begin
    result:=(a.Low shr Power)+(a.High shl (64-Power));
    a.High:=0;
    dec(a.Low,(a.Low shr Power) shl Power);
   end;
  end;
  function Int128IsZero(const a:TInt128):TPasDblStrUtilsBoolean;
  begin
   result:=(a.High=0) and (a.Low=0);
  end;
  function Int128BitAt(const a:TInt128;const Position:TPasDblStrUtilsInt32):TPasDblStrUtilsBoolean;
  begin
   if Position>=64 then begin
    result:=((a.High shr (Position-64)) and 1)<>0;
   end else begin
    result:=((a.LOw shr Position) and 1)<>0;
   end;
  end;
  procedure FillDigits32FixedLength(Number:TPasDblStrUtilsUInt32;RequestedLength:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var i,l:TPasDblStrUtilsInt32;
  begin
   l:=Len;
   inc(Len,RequestedLength);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   for i:=RequestedLength downto 1 do begin
    Buffer[l+i]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+(Number mod 10)));
    Number:=Number div 10;
   end;
  end;
  procedure FillDigits32(Number:TPasDblStrUtilsUInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var NumberLength,i,l:TPasDblStrUtilsInt32;
      OldNumber:TPasDblStrUtilsUInt32;
  begin
   OldNumber:=Number;
   NumberLength:=0;
   while Number<>0 do begin
    Number:=Number div 10;
    inc(NumberLength);
   end;
   if NumberLength<>0 then begin
    l:=Len;
    inc(Len,NumberLength);
    if Len>=length(Buffer) then begin
     SetLength(Buffer,Len*2);
    end;
    Number:=OldNumber;
    for i:=NumberLength downto 1 do begin
     Buffer[l+i]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+(Number mod 10)));
     Number:=Number div 10;
    end;
   end;
  end;
  procedure FillDigits64FixedLength(Number:TPasDblStrUtilsUInt64;RequestedLength:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var p0,p1,p2:TPasDblStrUtilsUInt32;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   FillDigits32FixedLength(p0,3,Buffer,Len);
   FillDigits32FixedLength(p1,7,Buffer,Len);
   FillDigits32FixedLength(p2,7,Buffer,Len);
  end;
  procedure FillDigits64(Number:TPasDblStrUtilsUInt64;var Buffer:TPasDblStrUtilsString;var Len:TPasDblStrUtilsInt32);
  var p0,p1,p2:TPasDblStrUtilsUInt32;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   if p0<>0 then begin
    FillDigits32(p0,Buffer,Len);
    FillDigits32FixedLength(p1,7,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else if p1<>0 then begin
    FillDigits32(p1,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else begin
    FillDigits32(p2,Buffer,Len);
   end;
  end;
  procedure RoundUp(var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var i:TPasDblStrUtilsInt32;
  begin
   if Len=0 then begin
    Buffer:='1';
    Len:=1;
    DecimalPoint:=1;
   end else begin
    inc(Buffer[Len]);
    for i:=Len downto 2 do begin
     if ord(Buffer[i])<>(ord('0')+10) then begin
      exit;
     end;
     Buffer[i]:='0';
     inc(Buffer[i-1]);
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(DecimalPoint);
    end;
   end;
  end;
  procedure FillFractionals(Fractionals:TPasDblStrUtilsUInt64;Exponent:TPasDblStrUtilsInt32;FractionalCount:TPasDblStrUtilsInt32;var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var Point,i,Digit:TPasDblStrUtilsInt32;
      Fractionals128:TInt128;
  begin
   Assert(((-128)<=Exponent) and (Exponent<=0));
   if (-Exponent)<=64 then begin
    Assert((Fractionals shr 56)=0);
    Point:=-Exponent;
    for i:=1 to FracitionalCount do begin
     Fractionals:=Fractionals*5;
     dec(Point);
     Digit:=Fractionals shr Point;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
     dec(Fractionals,TPasDblStrUtilsUInt64(Digit) shl Point);
    end;
    if ((Fractionals shr (Point-1)) and 1)<>0 then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end else begin
    Assert((64<(-Exponent)) and ((-Exponent)<=128));
    Fractionals128.High:=Fractionals;
    Fractionals128.Low:=0;
    Int128Shift(Fractionals128,(-Exponent)-64);
    Point:=128;
    for i:=1 to FracitionalCount do begin
     if Int128IsZero(Fractionals128) then begin
      break;
     end;
     Int128Mul(Fractionals128,5);
     dec(Point);
     Digit:=Int128DivModPowerOfTwo(Fractionals128,Point);
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=TPasDblStrUtilsChar(TPasDblStrUtilsUInt8(TPasDblStrUtilsUInt8(TPasDblStrUtilsChar('0'))+Digit));
    end;
    if Int128BitAt(Fractionals128,Point-1) then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end;
  end;
  procedure TrimZeros(var Buffer:TPasDblStrUtilsString;var Len,DecimalPoint:TPasDblStrUtilsInt32);
  var i:TPasDblStrUtilsInt32;
  begin
   while (Len>0) and (Buffer[Len]='0') do begin
    dec(Len);
   end;
   i:=0;
   while (i<Len) and (Buffer[i+1]='0') do begin
    inc(i);
   end;
   if i<>0 then begin
    Delete(Buffer,1,i);
    dec(Len,i);
    dec(DecimalPoint,i);
   end;
  end;
 var SignificantMantissa,Divisor,Dividend,Remainder,Integrals,Fractionals:TPasDblStrUtilsUInt64;
     Exponent,DivisorPower:TPasDblStrUtilsInt32;
     Quotient:TPasDblStrUtilsUInt32;
 begin
  result:=false;
  SplitDouble(Value,SignificantMantissa,Exponent);
  if (Exponent<=20) and (FracitionalCount<=20) then begin
   Len:=0;
   if (Exponent+53)>74 then begin
    Divisor:=Five17;
    DivisorPower:=17;
    Dividend:=SignificantMantissa;
    if Exponent>DivisorPower then begin
     Dividend:=Dividend shl (Exponent-DivisorPower);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl DivisorPower;
    end else begin
     Dividend:=Dividend shl (DivisorPower-Exponent);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl Exponent;
    end;
    FillDigits32(Quotient,Buffer,Len);
    FillDigits64FixedLength(Remainder,DivisorPower,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>=0 then begin
    SignificantMantissa:=SignificantMantissa shl Exponent;
    FillDigits64(SignificantMantissa,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>-53 then begin
    Integrals:=SignificantMantissa shr (-Exponent);
    Fractionals:=SignificantMantissa-(Integrals shl (-Exponent));
    if Integrals>$ffffffff then begin
     FillDigits64(Integrals,Buffer,Len);
    end else begin
     FillDigits32(Integrals,Buffer,Len);
    end;
    DecimalPoint:=Len;
    FillFractionals(Fractionals,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end else if Exponent<-128 then begin
    Assert(FracitionalCount>=20);
    Buffer:='';
    Len:=0;
    DecimalPoint:=-FracitionalCount;
   end else begin
    DecimalPoint:=0;
    FillFractionals(SignificantMantissa,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end;
   TrimZeros(Buffer,Len,DecimalPoint);
   SetLength(Buffer,Len);
   if Len=0 then begin
    DecimalPoint:=-FracitionalCount;
   end;
   result:=true;
  end;
 end;
var OK,Fast:TPasDblStrUtilsBoolean;
    Len,DecimalPoint,ZeroPrefixLength,ZeroPostfixLength,i:TPasDblStrUtilsInt32;
    LocalOutputMode:TPasDblStrUtilsOutputMode;
begin
 if IsNaN(AValue) then begin
  result:='NaN';
 end else if IsZero(AValue) then begin
  result:='0';
 end else if IsNegInfinite(AValue) then begin
  result:='-Infinity';
 end else if IsNegative(AValue) then begin
  result:='-'+ConvertDoubleToString(DoubleAbsolute(AValue),OutputMode,RequestedDigits);
 end else if IsInfinite(AValue) then begin
  result:='Infinity';
 end else begin
  result:='0';
  if AValue<>0 then begin
   Len:=0;
   DecimalPoint:=0;
   OK:=false;
   Fast:=false;
   if ((OutputMode=omFixed) and (AValue>=1e21)) or ((OutputMode=omRadix) and (RequestedDigits=10)) then begin
    LocalOutputMode:=omStandard;
   end else begin
    LocalOutputMode:=OutputMode;
   end;
   case LocalOutputMode of
    omStandard,omStandardExponential:begin
     OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
     inc(DecimalPoint,Len);
    end;
    omFixed:begin
     OK:=DoFastFixed(AValue,RequestedDigits,result,Len,DecimalPoint);
    end;
    omExponential,omPrecision:begin
     if RequestedDigits<=0 then begin
      OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
      RequestedDigits:=Len-1;
     end else begin
      OK:=DoFastPrecision(AValue,RequestedDigits,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
     end;
     Assert((Len>0) and (Len<=(RequestedDigits+1)));
    end;
    omRadix:begin
     if ((RequestedDigits>=2) and (RequestedDigits<=36)) and (IsFinite(AValue) and (AValue<4294967295.0) and (System.Int(AValue)=AValue)) then begin
      FastDoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
      Fast:=true;
      OK:=true;
     end;
    end;
   end;
   if not OK then begin
    case LocalOutputMode of
     omStandard,omStandardExponential:begin
      DoubleToDecimal(AValue,ModeShortest,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     omFixed:begin
      DoubleToDecimal(AValue,ModeFixed,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     omExponential,omPrecision:begin
      if RequestedDigits<=0 then begin
       DoubleToDecimal(AValue,ModeShortest,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
       RequestedDigits:=Len-1;
      end else begin
       DoubleToDecimal(AValue,ModePrecision,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
      Assert((Len>0) and (Len<=(RequestedDigits+1)));
     end;
     omRadix:begin
      if (RequestedDigits>=2) and (RequestedDigits<=36) then begin
       DoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
     end;
    end;
   end;
   if OK then begin
    SetLength(result,Len);
    case LocalOutputMode of
     omStandard:begin
      if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
       SetLength(result,DecimalPoint);
       FillChar(result[Len+1],DecimalPoint-Len,'0');
      end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
       Insert('.',result,DecimalPoint+1);
      end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
       for i:=1 to -DecimalPoint do begin
        result:='0'+result;
       end;
       result:='0.'+result;
      end else begin
       if Len<>1 then begin
        Insert('.',result,2);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end;
      end;
     end;
     omStandardExponential:begin
      if Len<>1 then begin
       Insert('.',result,2);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     omFixed:begin
      ZeroPrefixLength:=0;
      ZeroPostfixLength:=0;
      if DecimalPoint<=0 then begin
       ZeroPrefixLength:=(-DecimalPoint)+1;
       DecimalPoint:=1;
      end;
      if (ZeroPrefixLength+Len)<(DecimalPoint+RequestedDigits) then begin
       ZeroPostfixLength:=((DecimalPoint+RequestedDigits)-Len)-ZeroPrefixLength;
      end;
      for i:=1 to ZeroPrefixLength do begin
       result:='0'+result;
      end;
      for i:=1 to ZeroPostfixLength do begin
       result:=result+'0';
      end;
      if (RequestedDigits>0) and (DecimalPoint>0) and (DecimalPoint<=length(result)) then begin
       Insert('.',result,DecimalPoint+1);
      end;
     end;
     omExponential:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if RequestedDigits<>1 then begin
       Insert('.',result,2);
       for i:=Len+1 to RequestedDigits do begin
        result:=result+'0';
       end;
      end else begin
       SetLength(result,1);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     omPrecision:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if (DecimalPoint<-6) or (DecimalPoint>=RequestedDigits) then begin
       if RequestedDigits<>1 then begin
        Insert('.',result,2);
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,1);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
       end;
      end else begin
       if DecimalPoint<=0 then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,RequestedDigits);
        for i:=Len+1 to RequestedDigits do begin
         result[i]:='0';
        end;
        if DecimalPoint<RequestedDigits then begin
         if Len<>1 then begin
          Insert('.',result,DecimalPoint+1);
         end;
        end;
       end;
      end;
     end;
     omRadix:begin
      if not Fast then begin
       if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
        SetLength(result,DecimalPoint);
        FillChar(result[Len+1],DecimalPoint-Len,'0');
       end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
        Insert('.',result,DecimalPoint+1);
       end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
       end else begin
        if Len<>1 then begin
         Insert('.',result,2);
        end;
        if DecimalPoint>=0 then begin
         result:=result+'p+'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end else begin
         result:=result+'p-'+TPasDblStrUtilsString(IntToStr(abs(DecimalPoint-1)));
        end;
       end;
       while (length(result)>1) and ((result[1]='0') and (result[2] in ['0'..'9','a'..'f'])) do begin
        Delete(result,1,1);
       end;
      end;
     end;
    end;
   end else begin
    result:='';
   end;
  end;
 end;
end;

{$endif}

initialization
finalization
end.
