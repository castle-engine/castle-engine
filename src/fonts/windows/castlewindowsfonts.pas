{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Windows-specific font utilities. }

unit CastleWindowsFonts;

{$I castleconf.inc}

interface

uses Windows, SysUtils, CastleUtils;

type
  { Windows font charset values. For each value csXxx below,
    WinAPI defines constant XXX_CHARSET.
    Useful for enumerating available charsets, displaying charset name etc. }
  TWinCharSet = (
    wcsANSI, wcsDEFAULT, wcsSYMBOL, wcsSHIFTJIS,
    wcsHANGEUL, wcsGB2312, wcsCHINESEBIG5, wcsOEM,
    wcsHEBREW, wcsARABIC, wcsGREEK,
    wcsTURKISH, wcsTHAI, wcsEASTEUROPE,
    wcsRUSSIAN, wcsBALTIC);

  { A wrapper for CreateFont WinAPI function.
    Create an instance of this class, setup some attributes, and call GetHandle.
    In the future this class may be extended to something less trivial.

    For the meaning of properties see WinAPI documentation for CreateFont
    function. }
  TWindowsFont = class
  private
    FHeight: Integer;
    FAngle: Integer;
    FWeight: Integer;
    FItalic: boolean;
    FUnderline: boolean;
    FStrikeOut: boolean;
    FCharSet: TWinCharSet;
    FOutputPrecision: DWord;
    FClipPrecision: DWord;
    FQuality: DWord;
    FPitch: DWord;
    FFamily: DWord;
    FFaceName: string;
  public
    property Height: Integer read FHeight write FHeight;

    { Value for both nEscapement and nOrientation parameters for
      CreateFont. The only portable way is to set them both to the same
      values. }
    property Angle: Integer read FAngle write FAngle default 0;

    property Weight: Integer read FWeight write FWeight default FW_REGULAR;
    property Italic: boolean read FItalic write FItalic default false;
    property Underline: boolean read FUnderline write FUnderline default false;
    property StrikeOut: boolean read FStrikeOut write FStrikeOut default false;
    property CharSet: TWinCharSet read FCharSet write FCharSet default wcsDEFAULT;

    property OutputPrecision: DWord read FOutputPrecision write FOutputPrecision
      default OUT_DEFAULT_PRECIS;

    property ClipPrecision: DWord read FClipPrecision write FClipPrecision
      default CLIP_DEFAULT_PRECIS;

    property Quality: DWord read FQuality write FQuality default DEFAULT_QUALITY;

    { Font pitch and family. They will be combined to create
      fdwPitchAndFamily param, i.e. fdwPitchAndFamily := Pitch or Family.
      Pitch is for XXX_PITCH consts, Family is for FF_XXX consts.
      @groupBegin }
    property Pitch: DWord read FPitch write FPitch default DEFAULT_PITCH;
    property Family: DWord read FFamily write FFamily default FF_DONTCARE;
    { @groupEnd }

    { Font face name. Default is ''. }
    property FaceName: string read FFaceName write FFaceName;

    { Create a font with given properties. Calls WinAPI CreateFont.
      Rememeber to free result somewhere by DeleteObject.

      Remeber that you may not get the font you asked for.
      Windows.CreateFont will try to return something as close as possible,
      but if exact match will not be possible -- it can return something else.
      E.g. specifying FaceName = 'some non-existing font name' will not
      cause some error (like EOSError).
      Instead it will result in default Windows font ("MS Sans Serif" usually)
      being returned.

      @raises(EOSError If font cannot be created
        (when WinAPI CreateFont returned error)) }
    function GetHandle: HFont;

    { Constructor, takes initial Height value.
      We require the height value to be passed to constructor,
      simply because there's no "generally sensible" default value
      for Height. }
    constructor Create(AHeight: Integer);
  end;

const
  CharSetsNames: array [TWinCharSet] of string=(
    'ANSI_CHARSET',  'DEFAULT_CHARSET',  'SYMBOL_CHARSET',  'SHIFTJIS_CHARSET',
    'HANGEUL_CHARSET',  'GB2312_CHARSET',  'CHINESEBIG5_CHARSET',  'OEM_CHARSET',
    'HEBREW_CHARSET',  'ARABIC_CHARSET',  'GREEK_CHARSET',
    'TURKISH_CHARSET', 'THAI_CHARSET',  'EASTEUROPE_CHARSET',
    'RUSSIAN_CHARSET',  'BALTIC_CHARSET');

{ TODO:
  Funcs below are a little old.
  They probably could use some improvements. }

{ Is given Windows font possibly true-type. }
function IsFontTrueType( Font: HFONT ): boolean;

type
  TEnumFontCharsetsProc_ByObject = procedure( FontCharset: byte ) of object;
  TEnumFontCharsetsProc = procedure( FontCharset: byte );

{ Enumerate charsets handled by given font. Warning: enumerated values
  may be repeated.
  @groupBegin }
procedure EnumFontCharsetsObj(const FontName: string; EnumProc : TEnumFontCharsetsProc_ByObject);
procedure EnumFontCharsets(const FontName: string; EnumProc : TEnumFontCharsetsProc);
{ @groupEnd }

function WinCharSetFromName(const Name: string): TWinCharSet;

implementation

uses CastleStringUtils;

const
  CharSetsValues: array [TWinCharSet] of DWord = (
    ANSI_CHARSET,  DEFAULT_CHARSET,  SYMBOL_CHARSET,  SHIFTJIS_CHARSET,
    HANGEUL_CHARSET,  GB2312_CHARSET,  CHINESEBIG5_CHARSET,  OEM_CHARSET,
    HEBREW_CHARSET,  ARABIC_CHARSET,  GREEK_CHARSET,
    TURKISH_CHARSET, THAI_CHARSET,  EASTEUROPE_CHARSET,
    RUSSIAN_CHARSET,  BALTIC_CHARSET);

{ TWindowsFont ------------------------------------------------------------ }

constructor TWindowsFont.Create(AHeight: Integer);
begin
  FHeight := AHeight;
  FAngle := 0;
  FWeight := FW_REGULAR;
  FItalic := false;
  FUnderline := false;
  FStrikeOut := false;
  FCharSet := wcsDEFAULT;
  FOutputPrecision := OUT_DEFAULT_PRECIS;
  FClipPrecision := CLIP_DEFAULT_PRECIS;
  FQuality := DEFAULT_QUALITY;
  FPitch := DEFAULT_PITCH;
  FFamily := FF_DONTCARE;
  FFaceName := '';
end;

function TWindowsFont.GetHandle: HFont;
const
  BoolTo01: array[boolean]of Cardinal = (0, 1);
begin
  Result := CreateFont(FHeight, 0, FAngle, FAngle,
    FWeight, BoolTo01[FItalic], BoolTo01[FUnderline], BoolTo01[FStrikeOut],
    CharSetsValues[FCharSet], FOutputPrecision, FClipPrecision, FQuality,
    FPitch or FFamily, PCharOrNil(FaceName));
  OSCheck( Result <> 0, 'CreateFont');
end;

{ Windows font query ------------------------------------------------------- }

function EnumFontFamProc_IsTrueType(var EnumLogfont: TEnumLogFont;
  var NewTextMetric: TNewTextMetric;
  FontType: Integer;
  FuncResultPtr: LongInt): integer; stdcall;
begin
  { powinnismy sprawdzic czy znaleziony EnumLogFont.LogFont zgadza sie z szukanym
    LogFontem. Skoro moze byc wiele fontow o tej samej nazwie ... wiemy ze do tej
    procedury trafiaja tylko te ktorych nazwa sie zgadza. Ale co z reszta ?
    Czysto teoretycznie np. wersja regular fontu moze byc realizowana bitmapowo,
    a wersja Italic - jako TrueType. Nietesty - trudno rozstrzygnac czy znaleiony font
    "pasuje" do naszego LogFontu - bo jesli np. wersja Italic zostala wygenerowana z
    wersji regular to w naszym LogFoncie moze byc ustawione Italic a w EnumLogFoncie - nie,
    ale to bedzie ten sam font ! W zasadzie powinnismy zapamietywac wszystkie
    znalezione Logfonty a potem sprawdzac czy ten z nich ktory jest "najblizszy"
    naszgeo szukanego jest czy nie jest true-type. Niestety, kompletny algorytm na
    to czym jest "najblizszy" zna tylko Microsoft (zaimplementowali go chociazby w
    CreateFont).

    wiec co robimy ? Przeszukujemy wszystkie fonty o naszej nazwie. Jesli chociaz jeden
    jest true type to uznajemy nasz font za true-type. }

  if (FontType and TRUETYPE_FONTTYPE) <> 0 then
    PBoolean(FuncResultPtr)^ := true;
  Result := 1;
end;

function IsFontTrueType( Font: HFONT ): boolean;
{ See EnumFontFamProc_IsTrueType implementation comments for more information. }
var
  LogFont: TLogFont;
  wynik: integer;
  dc: HDC;
  savedObj: HGDIOBJ;
begin
  wynik := GetObject(Font, SizeOf(TLogFont), @LogFont);
  if wynik = 0 then RaiseLastOSError else
    if wynik <> SizeOf(TLogFont) then
      raise Exception.Create('IsFontTrueType function : parameter is not a font !');
  Result := false;
  dc := GetDC(0);
  SavedObj := SelectObject(dc, Font);
  try
    EnumFontFamilies(dc, @LogFont.lfFaceName, @EnumFontFamProc_IsTrueType, PtrUInt(@Result));
  finally
    ReleaseDC(0, dc);
    SelectObject(dc, SavedObj);
  end;
end;

{ EnumFontCharsets ----------------------------------------------------------------------}

type
  TEnumCharsetsInternalInfo_ByObject = record
    UserEnumProc : TEnumFontCharsetsProc_ByObject;
  end;
  PEnumCharsetsInternalInfo_ByObject = ^TEnumCharsetsInternalInfo_ByObject;

function EnumFontFamExProc_ByObject(var LogFontData : TEnumLogFontEx;
  var PhysFontData: TNewTextMetricEx;
  FontType: Integer;
  InternalInfo: LongInt): integer; stdcall;
begin
  PEnumCharsetsInternalInfo_ByObject(InternalInfo)^.
    UserEnumProc( PhysFontData.NtmENtm.tmCharset );
  result := 1;
end;

procedure EnumFontCharsetsObj(const FontName: string; EnumProc : TEnumFontCharsetsProc_ByObject);
var
  InternalInfo: TEnumCharsetsInternalInfo_ByObject;
  DC: HDC;
  LogFont: TLogFont;
begin
  DC := GetDC(0); { device context desktopu }
  try
    FillChar(LogFont, SizeOf(LogFont), 0);
    LogFont.lfCharSet := DEFAULT_CHARSET;
    StrCopy(@LogFont.lfFaceName, PChar(FontName));
    InternalInfo.UserEnumProc := EnumProc;
    EnumFontFamiliesEx(Dc, {$ifdef FPC} @ {$endif} LogFont,
      { TODO: temporary, I just make this unchecked } @EnumFontFamExProc_ByObject,
      Integer(@InternalInfo), 0);
  finally ReleaseDC(0, DC) end;
end;

type
  TEnumCharsetsDisp = class
    NonObjectEnumProc : TEnumFontCharsetsProc;
    procedure ObjectEnumProc( FontCharset: byte );
  end;
  procedure TEnumCharsetsDisp.ObjectEnumProc(FontCharset: byte);
  begin { ObjectEnumProc przekazuje po prostu swoj argument do NonObjectenumProc }
   NonObjectEnumProc( FontCharset );
  end;

procedure EnumFontCharsets(const FontName: string; EnumProc : TEnumFontCharsetsProc);
var
  EnumObj: TEnumCharsetsDisp;
begin
  EnumObj := TEnumCharsetsDisp.Create;
  EnumObj.NonObjectEnumProc := EnumProc;
  try
    EnumFontCharsetsObj(FontName, @EnumObj.ObjectEnumProc );
  finally EnumObj.Free end;
end;

function WinCharSetFromName(const Name: string): TWinCharSet;
begin
  for Result := Low(Result) to High(Result) do
    if Name = CharSetsNames[Result] then
      Exit;
  raise Exception.CreateFmt('Invalid charset name "%s"', [Name]);
end;

end.
