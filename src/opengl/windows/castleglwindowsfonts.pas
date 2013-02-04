{
  Copyright 2004-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL fonts from TrueType fonts installed in Windows.

  We create Windows font using unit CastleWindowsFonts,
  then convert it to TOutlineFont/TBitmapFont using unit CastleWinFontConvert,
  then utilize it in OpenGL using unit CastleGLBitmapFonts / CastleGLOutlineFonts.
  So this unit is just a simple wrapper for some other units.
}

unit CastleGLWindowsFonts;

interface

uses CastleWindowsFonts, GL, GLU, GLExt, Windows,
  CastleBitmapFonts, CastleOutlineFonts, CastleWinFontConvert,
  CastleGLBitmapFonts, CastleGLOutlineFonts;

type
  { Outline OpenGL font from an installed Windows font. }
  TWindowsOutlineFont = class(TGLOutlineFont)
  private
    CreatedOutline: TOutlineFont;
  public
    { Create outline font from an installed Windows font.

      For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function.

      For a description of Depth and OnlyLines params:
      see docs for CastleGLOutlineFonts.TGLOutlineFont.Create. }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: TWinCharSet; Depth: TGLfloat; OnlyLines: boolean);
    destructor Destroy; override;
  end;

  { Bitmap OpenGL font from an installed Windows font. }
  TWindowsBitmapFont = class(TGLBitmapFont)
  private
    CreatedBitmap: TBitmapFont;
  public
    { Create bitmap font from an installed Windows font.
      For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function. }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: TWinCharSet);
    destructor Destroy; override;
  end;

implementation

uses SysUtils, CastleUtils;

constructor TWindowsOutlineFont.Create(const AFaceName: string;
  AHeight: Integer; AWeight: DWord; AItalic: boolean; ACharSet: TWinCharSet;
  Depth: TGLfloat; OnlyLines: boolean);
var
  WinFont: TWindowsFont;
  WinFontHandle: HFont;
begin
  WinFont := TWindowsFont.Create(AHeight);
  try
    WinFont.OutputPrecision := OUT_TT_ONLY_PRECIS {only TrueType fonts};
    WinFont.FaceName := AFaceName;
    WinFont.Height := AHeight;
    WinFont.Weight := AWeight;
    WinFont.Italic := AItalic;
    WinFont.CharSet := ACharSet;

    WinFontHandle := WinFont.GetHandle;
    try
      CreatedOutline := Font2OutlineFont(WinFontHandle);
      inherited Create(CreatedOutline, Depth, OnlyLines);
    finally DeleteObject(WinFontHandle) end;
  finally FreeAndNil(WinFont) end;
end;

destructor TWindowsOutlineFont.Destroy;
begin
  inherited;
  { free CreatedOutline AFTER calling inherited because TGLOutlineFont
    requires that CreatedOutline is valid for it's lifetime }
  FreeAndNilFont(CreatedOutline);
end;

{ TWindowsBitmapFont ---------------------------------------- }

constructor TWindowsBitmapFont.Create(const AFaceName: string;
  AHeight: Integer; AWeight: DWord; AItalic: boolean; ACharSet: TWinCharSet);
var
  WinFont: TWindowsFont;
  WinFontHandle: HFont;
begin
  WinFont := TWindowsFont.Create(AHeight);
  try
    WinFont.OutputPrecision := OUT_TT_ONLY_PRECIS {only TrueType fonts};
    WinFont.FaceName := AFaceName;
    WinFont.Height := AHeight;
    WinFont.Weight := AWeight;
    WinFont.Italic := AItalic;
    WinFont.CharSet := ACharSet;

    WinFontHandle := WinFont.GetHandle;
    try
      CreatedBitmap := Font2BitmapFont(WinFontHandle);
      inherited Create(CreatedBitmap);
    finally DeleteObject(WinFontHandle) end;
  finally FreeAndNil(WinFont) end;
end;

destructor TWindowsBitmapFont.Destroy;
begin
  inherited;
  { free CreatedBitmap AFTER calling inherited because TGLBitmapFont
    requires that BitmapFont is valid for it's lifetime }
  FreeAndNilFont(CreatedBitmap);
end;

end.