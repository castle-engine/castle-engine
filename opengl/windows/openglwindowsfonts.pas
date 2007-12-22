{
  Copyright 2004-2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ This unit provides OpenGL fonts from Windows installed TrueType fonts.

  Implementation details: we create Windows font using unit WindowsFonts,
  then convert it to TTrueTypeFont/TBmpFont using unit WinFontConvert,
  then utilize it in OpenGL using unit OpenGLTTFonts/OpenGLBmpFonts.
  So this unit is just a simple wrapper for some other units.
}

unit OpenGLWindowsFonts;

{ TODO: I implemented an alternative approach, using Windows fonts +
  wglUseFontXxx, and it will be combined as an option into this unit
  in the future. }
{ $define USE_WGL}

interface

uses WindowsFonts, GL, GLU, GLExt, Windows
  {$ifndef USE_WGL}, BmpFontsTypes, TTFontsTypes, WinFontConvert,
  OpenGLTTFonts, OpenGLBmpFonts {$endif};

type
  { }
  TGLWindowsOutlineFont = class(TGLOutlineFont)
  private
    CreatedTTF: TTrueTypeFont;
  public
    { For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function.

      For a description of Depth and OnlyLines params:
      see docs for OpenGLTTFonts.TGLOutlineTTFont.Create }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: DWord; Depth: TGLfloat; OnlyLines: boolean);
    destructor Destroy; override;
  end;

  TGLWindowsBitmapFont = class(TGLBitmapFont)
  private
    CreatedBFNT: TBmpFont;
  public
    { For a description of AFaceName, AHeight, AWeight, AItalic, ACharSet:
      see documentation for Windows.CreateFont function. }
    constructor Create(const AFaceName: string; AHeight: Integer; AWeight: DWord;
      AItalic: boolean; ACharSet: DWord);
    destructor Destroy; override;
  end;

implementation

uses KambiUtils;

constructor TGLWindowsOutlineFont.Create(const AFaceName: string;
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
   Font2TrueTypeFont(WinFontHandle, CreatedTTF);
   inherited Create(@CreatedTTF, Depth, OnlyLines);
  finally DeleteObject(WinFontHandle) end;
 finally WinFont.Free end;
end;

destructor TGLWindowsOutlineFont.Destroy;
begin
 inherited;
 { yes, FreeMem AFTER inherited because TGLOutlineFont
   requires that @TTFont is valid for it's lifetime }
 FreeMemNilingAllChars(CreatedTTF);
end;

{ TGLWindowsBitmapFont ---------------------------------------- }

{ This is almost identical to implementation of TGLWindowsOutlineFont.
  It's bad that I created this by some copy&pasting, I should merge
  implementation of TGLWindowsOutlineFont and TGLWindowsBitmapFont.
  However, when I tried to do that using macros, it turned out to be
  too much work. }

constructor TGLWindowsBitmapFont.Create(const AFaceName: string;
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
   Font2BitmapFont(WinFontHandle, CreatedBFNT);
   inherited Create(@CreatedBFNT);
  finally DeleteObject(WinFontHandle) end;
 finally WinFont.Free end;
end;

destructor TGLWindowsBitmapFont.Destroy;
begin
 inherited;
 { yes, FreeMem AFTER inherited because TGLBitmapFont
   requires that @BmpFont is valid for it's lifetime }
 FreeMemNilingAllChars(CreatedBFNT);
end;

end.