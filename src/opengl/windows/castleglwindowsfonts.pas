{
  Copyright 2004-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ OpenGL fonts from TrueType fonts installed in Windows.

  We create Windows font using unit WindowsFonts,
  then convert it to TOutlineFont/TBitmapFont using unit WinFontConvert,
  then utilize it in OpenGL using unit CastleGLBitmapFonts / CastleGLOutlineFonts.
  So this unit is just a simple wrapper for some other units.
}

unit CastleGLWindowsFonts;

{ TODO: I implemented an alternative approach, using Windows fonts +
  wglUseFontXxx, and it will be combined as an option into this unit
  in the future. }
{ $define USE_WGL}

interface

uses WindowsFonts, GL, GLU, GLExt, Windows
  {$ifndef USE_WGL}, CastleBitmapFonts, CastleOutlineFonts, WinFontConvert,
  CastleGLBitmapFonts, CastleGLOutlineFonts {$endif};

type
  { }
  TWindowsOutlineFont = class(TGLOutlineFont)
  private
    CreatedOutline: TOutlineFont;
  public
    { For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function.

      For a description of Depth and OnlyLines params:
      see docs for CastleGLOutlineFonts.TGLOutlineFont.Create. }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: DWord; Depth: TGLfloat; OnlyLines: boolean);
    destructor Destroy; override;
  end;

  TWindowsBitmapFont = class(TGLBitmapFont)
  private
    CreatedBitmap: TBitmapFont;
  public
    { For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function. }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: DWord);
    destructor Destroy; override;
  end;

implementation

uses CastleUtils;

constructor TWindowsOutlineFont.Create(const AFaceName: string;
  AHeight: Integer; AWeight: DWord; AItalic: boolean; ACharSet: DWord;
  Depth: TGLfloat; OnlyLines: boolean);
var WinFont: TWindowsFont;
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
   Font2OutlineFont(WinFontHandle, CreatedOutline);
   inherited Create(@CreatedOutline, Depth, OnlyLines);
  finally DeleteObject(WinFontHandle) end;
 finally WinFont.Free end;
end;

destructor TWindowsOutlineFont.Destroy;
begin
 inherited;
 { yes, FreeMem AFTER inherited because TGLOutlineFont
   requires that CreatedOutline is valid for it's lifetime }
 FreeMemNilingAllChars(CreatedOutline);
end;

{ TWindowsBitmapFont ---------------------------------------- }

{ This is almost identical to implementation of TWindowsOutlineFont.
  It's bad that I created this by some copy&pasting, I should merge
  implementation of TWindowsOutlineFont and TWindowsBitmapFont.
  However, when I tried to do that using macros, it turned out to be
  too much work. }

constructor TWindowsBitmapFont.Create(const AFaceName: string;
  AHeight: Integer; AWeight: DWord; AItalic: boolean; ACharSet: DWord);
var WinFont: TWindowsFont;
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
   Font2BitmapFont(WinFontHandle, CreatedBitmap);
   inherited Create(@CreatedBitmap);
  finally DeleteObject(WinFontHandle) end;
 finally WinFont.Free end;
end;

destructor TWindowsBitmapFont.Destroy;
begin
 inherited;
 { yes, FreeMem AFTER inherited because TGLBitmapFont
   requires that BitmapFont is valid for it's lifetime }
 FreeMemNilingAllChars(CreatedBitmap);
end;

end.