{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Yury Sidorov,
    member of the Free Pascal development team.

    Wide string support for Android

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

{$I castleconf.inc}
{$inline on}
{$implicitexceptions off}

{ Reworked FPC CWString, to delay loading of library until Android
  activity started, necessary on some Android versions.
  @exclude Not documented for PasDoc. }
unit CastleAndroidInternalCWString;

{ This should be defined only in FPC >= 3.0.2. }
{$define FPC_NEW_VERSION_WITH_UNICODE}
{$ifdef VER3_0_0}
  {$undef FPC_NEW_VERSION_WITH_UNICODE}
{$endif}
{$ifdef VER2_6}
  {$undef FPC_NEW_VERSION_WITH_UNICODE}
{$endif}

interface

procedure SetCWidestringManager;

{ Call this once Android activity started to initialize CWString. }
procedure InitializeAndroidCWString;

implementation

uses dynlibs, CastleAndroidInternalLog;

type
  UErrorCode = SizeInt;
  int32_t = Integer;
  uint32_t = Cardinal;
  PUConverter = pointer;
  PUCollator = pointer;
  UBool = LongBool;

var
  hlibICU: TLibHandle;
  hlibICUi18n: TLibHandle;
  ucnv_open: function (converterName: PAnsiChar; var pErrorCode: UErrorCode): PUConverter; cdecl;
  ucnv_close: procedure (converter: PUConverter); cdecl;
  ucnv_setSubstChars: procedure (converter: PUConverter; subChars: PAnsiChar; len: byte; var pErrorCode: UErrorCode); cdecl;
  ucnv_setFallback: procedure (cnv: PUConverter; usesFallback: UBool); cdecl;
  ucnv_fromUChars: function (cnv: PUConverter; dest: PAnsiChar; destCapacity: int32_t; src: PUnicodeChar; srcLength: int32_t; var pErrorCode: UErrorCode): int32_t; cdecl;
  ucnv_toUChars: function (cnv: PUConverter; dest: PUnicodeChar; destCapacity: int32_t; src: PAnsiChar; srcLength: int32_t; var pErrorCode: UErrorCode): int32_t; cdecl;
  u_strToUpper: function (dest: PUnicodeChar; destCapacity: int32_t; src: PUnicodeChar; srcLength: int32_t; locale: PAnsiChar; var pErrorCode: UErrorCode): int32_t; cdecl;
  u_strToLower: function (dest: PUnicodeChar; destCapacity: int32_t; src: PUnicodeChar; srcLength: int32_t; locale: PAnsiChar; var pErrorCode: UErrorCode): int32_t; cdecl;
  u_strCompare: function (s1: PUnicodeChar; length1: int32_t; s2: PUnicodeChar; length2: int32_t; codePointOrder: UBool): int32_t; cdecl;
  u_strCaseCompare: function (s1: PUnicodeChar; length1: int32_t; s2: PUnicodeChar; length2: int32_t; options: uint32_t; var pErrorCode: UErrorCode): int32_t; cdecl;

  ucol_open: function(loc: PAnsiChar; var status: UErrorCode): PUCollator; cdecl;
  ucol_close: procedure (coll: PUCollator); cdecl;
  ucol_strcoll: function (coll: PUCollator; source: PUnicodeChar; sourceLength: int32_t; target: PUnicodeChar; targetLength: int32_t): int32_t; cdecl;
	ucol_setStrength: procedure (coll: PUCollator; strength: int32_t); cdecl;
  u_errorName: function (code: UErrorCode): PAnsiChar; cdecl;

threadvar
  ThreadDataInited: boolean;
  DefConv, LastConv: PUConverter;
  LastCP: TSystemCodePage;
  DefColl: PUCollator;

function OpenConverter(const name: ansistring): PUConverter;
var
  err: UErrorCode;
begin
  err:=0;
  Result:=ucnv_open(PAnsiChar(name), err);
  if Result <> nil then begin
    ucnv_setSubstChars(Result, '?', 1, err);
    ucnv_setFallback(Result, True);
  end;
end;

procedure InitThreadData;
var
  err: UErrorCode;
  col: PUCollator;
begin
  if (hlibICU = 0) or ThreadDataInited then
    exit;
  ThreadDataInited:=True;
  DefConv:=OpenConverter('utf8');
  err:=0;
  col:=ucol_open(nil, err);
  if col <> nil then
    ucol_setStrength(col, 2);
  DefColl:=col;
end;

{$ifdef FPC_NEW_VERSION_WITH_UNICODE}

function GetConverter(cp: TSystemCodePage): PUConverter;
var
  s: ansistring;
begin
  if hlibICU = 0 then begin
    Result:=nil;
    exit;
  end;
  InitThreadData;
  if (cp = CP_UTF8) or (cp = CP_ACP) then
    Result:=DefConv
  else begin
    if cp <> LastCP then begin
      Str(cp, s);
      LastConv:=OpenConverter('cp' + s);
      LastCP:=cp;
    end;
    Result:=LastConv;
  end;
end;

procedure Unicode2AnsiMove(source: PUnicodeChar; var dest: RawByteString; cp: TSystemCodePage; len: SizeInt);
var
  len2: SizeInt;
  conv: PUConverter;
  err: UErrorCode;
begin
  if len = 0 then begin
    dest:='';
    exit;
  end;
  conv:=GetConverter(cp);
  if (conv = nil) and not ( (cp = CP_UTF8) or (cp = CP_ACP) ) then begin
    // fallback implementation
    DefaultUnicode2AnsiMove(source,dest,DefaultSystemCodePage,len);
    exit;
  end;

  len2:=len*3;
  SetLength(dest, len2);
  err:=0;
  if conv <> nil then
    len2:=ucnv_fromUChars(conv, PAnsiChar(dest), len2, source, len, err)
  else begin
    // Use UTF-8 conversion from RTL
    cp:=CP_UTF8;
    len2:=UnicodeToUtf8(PAnsiChar(dest), len2, source, len) - 1;
  end;
  if len2 > Length(dest) then begin
    SetLength(dest, len2);
    err:=0;
    if conv <> nil then
      len2:=ucnv_fromUChars(conv, PAnsiChar(dest), len2, source, len, err)
    else
      len2:=UnicodeToUtf8(PAnsiChar(dest), len2, source, len) - 1;
  end;
  if len2 < 0 then
    len2:=0;
  SetLength(dest, len2);
  SetCodePage(dest, cp, False);
end;

procedure Ansi2UnicodeMove(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
var
  len2: SizeInt;
  conv: PUConverter;
  err: UErrorCode;
begin
  if len = 0 then begin
    dest:='';
    exit;
  end;
  conv:=GetConverter(cp);
  if (conv = nil) and not ( (cp = CP_UTF8) or (cp = CP_ACP) ) then begin
    // fallback implementation
    DefaultAnsi2UnicodeMove(source,DefaultSystemCodePage,dest,len);
    exit;
  end;

  len2:=len;
  SetLength(dest, len2);
  err:=0;
  if conv <> nil then
    len2:=ucnv_toUChars(conv, PUnicodeChar(dest), len2, source, len, err)
  else
    // Use UTF-8 conversion from RTL
    len2:=Utf8ToUnicode(PUnicodeChar(dest), len2, source, len) - 1;
  if len2 > Length(dest) then begin
    SetLength(dest, len2);
    err:=0;
    if conv <> nil then
      len2:=ucnv_toUChars(conv, PUnicodeChar(dest), len2, source, len, err)
    else
      len2:=Utf8ToUnicode(PUnicodeChar(dest), len2, source, len) - 1;
  end;
  if len2 < 0 then
    len2:=0;
  SetLength(dest, len2);
end;

function UpperUnicodeString(const s : UnicodeString) : UnicodeString;
var
  len, len2: SizeInt;
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=UnicodeString(UpCase(AnsiString(s)));
    exit;
  end;
  len:=Length(s);
  SetLength(Result, len);
  if len = 0 then
    exit;
  err:=0;
  len2:=u_strToUpper(PUnicodeChar(Result), len, PUnicodeChar(s), len, nil, err);
  if len2 > len then begin
    SetLength(Result, len2);
    err:=0;
    len2:=u_strToUpper(PUnicodeChar(Result), len2, PUnicodeChar(s), len, nil, err);
  end;
  SetLength(Result, len2);
end;

function LowerUnicodeString(const s : UnicodeString) : UnicodeString;
var
  len, len2: SizeInt;
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=UnicodeString(LowerCase(AnsiString(s)));
    exit;
  end;
  len:=Length(s);
  SetLength(Result, len);
  if len = 0 then
    exit;
  err:=0;
  len2:=u_strToLower(PUnicodeChar(Result), len, PUnicodeChar(s), len, nil, err);
  if len2 > len then begin
    SetLength(Result, len2);
    err:=0;
    len2:=u_strToLower(PUnicodeChar(Result), len2, PUnicodeChar(s), len, nil, err);
  end;
  SetLength(Result, len2);
end;

function _CompareStr(const S1, S2: UnicodeString): PtrInt;
var
  count, count1, count2: SizeInt;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareByte(PUnicodeChar(S1)^, PUnicodeChar(S2)^, Count*SizeOf(UnicodeChar));
  if result=0 then
    result:=Count1 - Count2;
end;

function CompareUnicodeString(const s1, s2 : UnicodeString; Options : TCompareOptions) : PtrInt;
const
  U_COMPARE_CODE_POINT_ORDER = $8000;
var
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=_CompareStr(s1, s2);
    exit;
  end;
  if (coIgnoreCase in Options) then begin
    err:=0;
    Result:=u_strCaseCompare(PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2), U_COMPARE_CODE_POINT_ORDER, err);
  end
  else begin
    InitThreadData;
    if DefColl <> nil then
      Result:=ucol_strcoll(DefColl, PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2))
    else
      Result:=u_strCompare(PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2), True);
  end;
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
begin
  Result:=AnsiString(UpperUnicodeString(UnicodeString(s)));
end;

function LowerAnsiString(const s : AnsiString) : AnsiString;
begin
  Result:=AnsiString(LowerUnicodeString(UnicodeString(s)));
end;

function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2), []);
end;

function StrCompAnsi(s1,s2 : PChar): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2), []);
end;

function AnsiCompareText(const S1, S2: ansistring): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2), [coIgnoreCase]);
end;

function AnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2), [coIgnoreCase]);
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  as1, as2: ansistring;
begin
  SetString(as1, S1, MaxLen);
  SetString(as2, S2, MaxLen);
  Result:=CompareUnicodeString(UnicodeString(as1), UnicodeString(as2), []);
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  as1, as2: ansistring;
begin
  SetString(as1, S1, MaxLen);
  SetString(as2, S2, MaxLen);
  Result:=CompareUnicodeString(UnicodeString(as1), UnicodeString(as2), [coIgnoreCase]);
end;

function AnsiStrLower(Str: PChar): PChar;
var
  s, res: ansistring;
begin
  s:=Str;
  res:=LowerAnsiString(s);
  if Length(res) > Length(s) then
    SetLength(res, Length(s));
  Move(PAnsiChar(res)^, Str, Length(res) + 1);
  Result:=Str;
end;

function AnsiStrUpper(Str: PChar): PChar;
var
  s, res: ansistring;
begin
  s:=Str;
  res:=UpperAnsiString(s);
  if Length(res) > Length(s) then
    SetLength(res, Length(s));
  Move(PAnsiChar(res)^, Str, Length(res) + 1);
  Result:=Str;
end;

function CodePointLength(const Str: PChar; MaxLookAead: PtrInt): Ptrint;
var
  c: byte;
begin
  // Only UTF-8 encoding is supported
  c:=byte(Str^);
  if c =  0 then
    Result:=0
  else begin
    Result:=1;
    if c < $80 then
      exit; // 1-byte ASCII char
    while c and $C0 = $C0 do begin
      Inc(Result);
      c:=c shl 1;
    end;
    if Result > 6 then
      Result:=1 // Invalid code point
    else
      if Result > MaxLookAead then
        Result:=-1; // Incomplete code point
  end;
end;

function GetStandardCodePage(const stdcp: TStandardCodePageEnum): TSystemCodePage;
begin
  Result := CP_UTF8; // Android always uses UTF-8
end;

procedure SetStdIOCodePage(var T: Text); inline;
begin
  case TextRec(T).Mode of
    fmInput:TextRec(T).CodePage:=DefaultSystemCodePage;
    fmOutput:TextRec(T).CodePage:=DefaultSystemCodePage;
  end;
end;

procedure SetStdIOCodePages; inline;
begin
  SetStdIOCodePage(Input);
  SetStdIOCodePage(Output);
  SetStdIOCodePage(ErrOutput);
  SetStdIOCodePage(StdOut);
  SetStdIOCodePage(StdErr);
end;

procedure Ansi2WideMove(source:pchar; cp:TSystemCodePage; var dest:widestring; len:SizeInt);
var
  us: UnicodeString;
begin
  Ansi2UnicodeMove(source,cp,us,len);
  dest:=us;
end;

function UpperWideString(const s : WideString) : WideString;
begin
  Result:=UpperUnicodeString(s);
end;

function LowerWideString(const s : WideString) : WideString;
begin
  Result:=LowerUnicodeString(s);
end;

function CompareWideString(const s1, s2 : WideString; Options : TCompareOptions) : PtrInt;
begin
  Result:=CompareUnicodeString(s1, s2, Options);
end;

Procedure SetCWideStringManager;
Var
  CWideStringManager : TUnicodeStringManager;
begin
  CWideStringManager:=widestringmanager;
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Unicode2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;
      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;
      CompareWideStringProc:=@CompareWideString;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@AnsiCompareText;
      StrCompAnsiStringProc:=@StrCompAnsi;
      StrICompAnsiStringProc:=@AnsiStrIComp;
      StrLCompAnsiStringProc:=@AnsiStrLComp;
      StrLICompAnsiStringProc:=@AnsiStrLIComp;
      StrLowerAnsiStringProc:=@AnsiStrLower;
      StrUpperAnsiStringProc:=@AnsiStrUpper;

      Unicode2AnsiMoveProc:=@Unicode2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2UnicodeMove;
      UpperUnicodeStringProc:=@UpperUnicodeString;
      LowerUnicodeStringProc:=@LowerUnicodeString;
      CompareUnicodeStringProc:=@CompareUnicodeString;

      GetStandardCodePageProc:=@GetStandardCodePage;
      CodePointLengthProc:=@CodePointLength;
    end;
  SetUnicodeStringManager(CWideStringManager);
end;

{$else}

function GetConverter(cp: TSystemCodePage): PUConverter;
var
  s: ansistring;
begin
  if hlibICU = 0 then begin
    Result:=nil;
    exit;
  end;
  InitThreadData;
  if (cp = DefaultSystemCodePage) or (cp = CP_ACP) then
    Result:=DefConv
  else begin
    if cp <> LastCP then begin
      Str(cp, s);
      LastConv:=OpenConverter('cp' + s);
      LastCP:=cp;
    end;
    Result:=LastConv;
  end;
end;

procedure Unicode2AnsiMove(source: PUnicodeChar; var dest: RawByteString; cp: TSystemCodePage; len: SizeInt);
var
  len2: SizeInt;
  conv: PUConverter;
  err: UErrorCode;
begin
  if len = 0 then begin
    dest:='';
    exit;
  end;
  conv:=GetConverter(cp);
  if conv = nil then begin
    DefaultUnicode2AnsiMove(source,dest,DefaultSystemCodePage,len);
    exit;
  end;

  len2:=len*3;
  SetLength(dest, len2);
  err:=0;
  len2:=ucnv_fromUChars(conv, PAnsiChar(dest), len2, source, len, err);
  if len2 > Length(dest) then begin
    SetLength(dest, len2);
    err:=0;
    len2:=ucnv_fromUChars(conv, PAnsiChar(dest), len2, source, len, err);
  end;
  SetLength(dest, len2);
  SetCodePage(dest, cp, False);
end;

procedure Ansi2UnicodeMove(source:pchar;cp : TSystemCodePage;var dest:unicodestring;len:SizeInt);
var
  len2: SizeInt;
  conv: PUConverter;
  err: UErrorCode;
begin
  if len = 0 then begin
    dest:='';
    exit;
  end;
  conv:=GetConverter(cp);
  if conv = nil then begin
    DefaultAnsi2UnicodeMove(source,DefaultSystemCodePage,dest,len);
    exit;
  end;

  len2:=len;
  SetLength(dest, len2);
  err:=0;
  len2:=ucnv_toUChars(conv, PUnicodeChar(dest), len2, source, len, err);
  if len2 > Length(dest) then begin
    SetLength(dest, len2);
    err:=0;
    len2:=ucnv_toUChars(conv, PUnicodeChar(dest), len2, source, len, err);
  end;
  SetLength(dest, len2);
end;

function UpperUnicodeString(const s : UnicodeString) : UnicodeString;
var
  len, len2: SizeInt;
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=UnicodeString(UpCase(AnsiString(s)));
    exit;
  end;
  len:=Length(s);
  SetLength(Result, len);
  if len = 0 then
    exit;
  err:=0;
  len2:=u_strToUpper(PUnicodeChar(Result), len, PUnicodeChar(s), len, nil, err);
  if len2 > len then begin
    SetLength(Result, len2);
    err:=0;
    len2:=u_strToUpper(PUnicodeChar(Result), len2, PUnicodeChar(s), len, nil, err);
  end;
  SetLength(Result, len2);
end;

function LowerUnicodeString(const s : UnicodeString) : UnicodeString;
var
  len, len2: SizeInt;
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=UnicodeString(LowerCase(AnsiString(s)));
    exit;
  end;
  len:=Length(s);
  SetLength(Result, len);
  if len = 0 then
    exit;
  err:=0;
  len2:=u_strToLower(PUnicodeChar(Result), len, PUnicodeChar(s), len, nil, err);
  if len2 > len then begin
    SetLength(Result, len2);
    err:=0;
    len2:=u_strToLower(PUnicodeChar(Result), len2, PUnicodeChar(s), len, nil, err);
  end;
  SetLength(Result, len2);
end;

function _CompareStr(const S1, S2: UnicodeString): PtrInt;
var
  count, count1, count2: SizeInt;
begin
  result := 0;
  Count1 := Length(S1);
  Count2 := Length(S2);
  if Count1>Count2 then
    Count:=Count2
  else
    Count:=Count1;
  result := CompareByte(PUnicodeChar(S1)^, PUnicodeChar(S2)^, Count*SizeOf(UnicodeChar));
  if result=0 then
    result:=Count1 - Count2;
end;

function CompareUnicodeString(const s1, s2 : UnicodeString) : PtrInt;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=_CompareStr(s1, s2);
    exit;
  end;
  InitThreadData;
  if DefColl <> nil then
    Result:=ucol_strcoll(DefColl, PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2))
  else
    Result:=u_strCompare(PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2), True);
end;

function CompareTextUnicodeString(const s1, s2 : UnicodeString): PtrInt;
const
  U_COMPARE_CODE_POINT_ORDER = $8000;
var
  err: UErrorCode;
begin
  if hlibICU = 0 then begin
    // fallback implementation
    Result:=_CompareStr(UpperUnicodeString(s1), UpperUnicodeString(s2));
    exit;
  end;
  err:=0;
  Result:=u_strCaseCompare(PUnicodeChar(s1), Length(s1), PUnicodeChar(s2), Length(s2), U_COMPARE_CODE_POINT_ORDER, err);
end;

function UpperAnsiString(const s : AnsiString) : AnsiString;
begin
  Result:=AnsiString(UpperUnicodeString(UnicodeString(s)));
end;

function LowerAnsiString(const s : AnsiString) : AnsiString;
begin
  Result:=AnsiString(LowerUnicodeString(UnicodeString(s)));
end;

function CompareStrAnsiString(const s1, s2: ansistring): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2));
end;

function StrCompAnsi(s1,s2 : PChar): PtrInt;
begin
  Result:=CompareUnicodeString(UnicodeString(s1), UnicodeString(s2));
end;

function AnsiCompareText(const S1, S2: ansistring): PtrInt;
begin
  Result:=CompareTextUnicodeString(UnicodeString(s1), UnicodeString(s2));
end;

function AnsiStrIComp(S1, S2: PChar): PtrInt;
begin
  Result:=CompareTextUnicodeString(UnicodeString(s1), UnicodeString(s2));
end;

function AnsiStrLComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  as1, as2: ansistring;
begin
  SetString(as1, S1, MaxLen);
  SetString(as2, S2, MaxLen);
  Result:=CompareUnicodeString(UnicodeString(as1), UnicodeString(as2));
end;

function AnsiStrLIComp(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  as1, as2: ansistring;
begin
  SetString(as1, S1, MaxLen);
  SetString(as2, S2, MaxLen);
  Result:=CompareTextUnicodeString(UnicodeString(as1), UnicodeString(as2));
end;

function AnsiStrLower(Str: PChar): PChar;
var
  s, res: ansistring;
begin
  s:=Str;
  res:=LowerAnsiString(s);
  if Length(res) > Length(s) then
    SetLength(res, Length(s));
  Move(PAnsiChar(res)^, Str, Length(res) + 1);
  Result:=Str;
end;

function AnsiStrUpper(Str: PChar): PChar;
var
  s, res: ansistring;
begin
  s:=Str;
  res:=UpperAnsiString(s);
  if Length(res) > Length(s) then
    SetLength(res, Length(s));
  Move(PAnsiChar(res)^, Str, Length(res) + 1);
  Result:=Str;
end;

function CodePointLength(const Str: PChar; MaxLookAead: PtrInt): Ptrint;
var
  c: byte;
begin
  // Only UTF-8 encoding is supported
  c:=byte(Str^);
  if c =  0 then
    Result:=0
  else begin
    Result:=1;
    if c < $80 then
      exit; // 1-byte ASCII char
    while c and $C0 = $C0 do begin
      Inc(Result);
      c:=c shl 1;
    end;
    if Result > 6 then
      Result:=1 // Invalid code point
    else
      if Result > MaxLookAead then
        Result:=-1; // Incomplete code point
  end;
end;

function GetStandardCodePage(const stdcp: TStandardCodePageEnum): TSystemCodePage;
begin
  Result := CP_UTF8; // Android always uses UTF-8
end;

procedure SetStdIOCodePage(var T: Text); {$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  case TextRec(T).Mode of
    fmInput:TextRec(T).CodePage:=DefaultSystemCodePage;
    fmOutput:TextRec(T).CodePage:=DefaultSystemCodePage;
  end;
end;

procedure SetStdIOCodePages; {$ifdef SUPPORTS_INLINE} inline; {$endif}
begin
  SetStdIOCodePage(Input);
  SetStdIOCodePage(Output);
  SetStdIOCodePage(ErrOutput);
  SetStdIOCodePage(StdOut);
  SetStdIOCodePage(StdErr);
end;

procedure Ansi2WideMove(source:pchar; cp:TSystemCodePage; var dest:widestring; len:SizeInt);
var
  us: UnicodeString;
begin
  Ansi2UnicodeMove(source,cp,us,len);
  dest:=us;
end;

function UpperWideString(const s : WideString) : WideString;
begin
  Result:=UpperUnicodeString(s);
end;

function LowerWideString(const s : WideString) : WideString;
begin
  Result:=LowerUnicodeString(s);
end;

function CompareWideString(const s1, s2 : WideString) : PtrInt;
begin
  Result:=CompareUnicodeString(s1, s2);
end;

function CompareTextWideString(const s1, s2 : WideString): PtrInt;
begin
  Result:=CompareTextUnicodeString(s1, s2);
end;

Procedure SetCWideStringManager;
Var
  CWideStringManager : TUnicodeStringManager;
begin
  CWideStringManager:=widestringmanager;
  With CWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Unicode2AnsiMove;
      Ansi2WideMoveProc:=@Ansi2WideMove;
      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;
      CompareWideStringProc:=@CompareWideString;
      CompareTextWideStringProc:=@CompareTextWideString;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@AnsiCompareText;
      StrCompAnsiStringProc:=@StrCompAnsi;
      StrICompAnsiStringProc:=@AnsiStrIComp;
      StrLCompAnsiStringProc:=@AnsiStrLComp;
      StrLICompAnsiStringProc:=@AnsiStrLIComp;
      StrLowerAnsiStringProc:=@AnsiStrLower;
      StrUpperAnsiStringProc:=@AnsiStrUpper;

      Unicode2AnsiMoveProc:=@Unicode2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2UnicodeMove;
      UpperUnicodeStringProc:=@UpperUnicodeString;
      LowerUnicodeStringProc:=@LowerUnicodeString;
      CompareUnicodeStringProc:=@CompareUnicodeString;
      CompareTextUnicodeStringProc:=@CompareTextUnicodeString;

      GetStandardCodePageProc:=@GetStandardCodePage;
      CodePointLengthProc:=@CodePointLength;
    end;
  SetUnicodeStringManager(CWideStringManager);
end;

{$endif}

procedure UnloadICU;
begin
  if hlibICUi18n <> 0 then begin
    if DefColl <> nil then
      ucol_close(DefColl);
    UnloadLibrary(hlibICUi18n);
    hlibICUi18n:=0;
  end;
  if hlibICU <> 0 then begin
    if DefConv <> nil then
      ucnv_close(DefConv);
    if LastConv <> nil then
      ucnv_close(LastConv);
    UnloadLibrary(hlibICU);
    hlibICU:=0;
  end;
end;

procedure LoadICU;
var
  LibVer: ansistring;

  function _GetProc(const Name: AnsiString; out ProcPtr; hLib: TLibHandle = 0): boolean;
  var
    p: pointer;
  begin
    if hLib = 0 then
      hLib:=hlibICU;
    p:=GetProcedureAddress(hlib, Name + LibVer);
    if p = nil then begin
      // unload lib on failure
      UnloadICU;
      Result:=False;
    end
    else begin
      pointer(ProcPtr):=p;
      Result:=True;
    end;
  end;

const
  ICUver: array [1..9] of ansistring = ('3_8', '4_2', '44', '46', '48', '50', '51', '53', '55');
  TestProcName = 'ucnv_open';

var
  i: int32_t;
  s: ansistring;
begin
  hlibICU:=LoadLibrary('libicuuc.so');
  hlibICUi18n:=LoadLibrary('libicui18n.so');
  if (hlibICU = 0) or (hlibICUi18n = 0) then begin
    UnloadICU;
    AndroidLog(alWarn, 'Cannot load libicuuc.so or libicui18n.so. WideString conversion will fail for special UTF-8 characters');
    exit;
  end;
  // Finding ICU version using known versions table
  for i:=High(ICUver) downto Low(ICUver) do begin
    s:='_' + ICUver[i];
    if GetProcedureAddress(hlibICU, TestProcName + s) <> nil then begin
      LibVer:=s;
      AndroidLog(alInfo, 'Found libicuuc.so version ' + LibVer);
      break;
    end;
  end;

  if LibVer = '' then begin
    // Finding unknown ICU version
    Val(ICUver[High(ICUver)], i);
    repeat
      Inc(i);
      Str(i, s);
      s:='_'  + s;
      if GetProcedureAddress(hlibICU, TestProcName + s) <> nil then begin
        LibVer:=s;
        AndroidLog(alInfo, 'Found libicuuc.so version (by looping) ' + LibVer);
        break;
      end;
    until i >= 100;
  end;

  if LibVer = '' then begin
    // Trying versionless name
    if GetProcedureAddress(hlibICU, TestProcName) = nil then begin
      // Unable to get ICU version
      UnloadICU;
      AndroidLog(alWarn, 'Cannot use libicuuc.so --- no versioned ucnv_open found, and unversioned ucnv_open not available. . WideString conversion will fail for special UTF-8 characters');
      exit;
    end;
  end;

  if not _GetProc('ucnv_open', ucnv_open) then exit;
  if not _GetProc('ucnv_close', ucnv_close) then exit;
  if not _GetProc('ucnv_setSubstChars', ucnv_setSubstChars) then exit;
  if not _GetProc('ucnv_setFallback', ucnv_setFallback) then exit;
  if not _GetProc('ucnv_fromUChars', ucnv_fromUChars) then exit;
  if not _GetProc('ucnv_toUChars', ucnv_toUChars) then exit;
  if not _GetProc('u_strToUpper', u_strToUpper) then exit;
  if not _GetProc('u_strToLower', u_strToLower) then exit;
  if not _GetProc('u_strCompare', u_strCompare) then exit;
  if not _GetProc('u_strCaseCompare', u_strCaseCompare) then exit;

  if not _GetProc('u_errorName', u_errorName) then exit;

  if not _GetProc('ucol_open', ucol_open, hlibICUi18n) then exit;
  if not _GetProc('ucol_close', ucol_close, hlibICUi18n) then exit;
  if not _GetProc('ucol_strcoll', ucol_strcoll, hlibICUi18n) then exit;
  if not _GetProc('ucol_setStrength', ucol_setStrength, hlibICUi18n) then exit;
end;

procedure InitializeAndroidCWString;
begin
  DefaultSystemCodePage:=GetStandardCodePage(scpAnsi);
  DefaultUnicodeCodePage:=CP_UTF16;
  LoadICU;
  SetCWideStringManager;
  SetStdIOCodePages;
end;

finalization
  UnloadICU;
end.
