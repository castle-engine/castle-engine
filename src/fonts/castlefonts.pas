{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Fonts (TCastleAbstractFont, TCastleFont, TCastleBitmapFont, TCastleFontFamily and friends). }
unit CastleFonts;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections, Contnrs,
  CastleGLImages, CastleStringUtils, CastleColors, CastleVectors,
  CastleTextureFontData, CastleImages, CastleUnicode, CastleRectangles,
  CastleApplicationProperties, CastleClassUtils;

{$define read_interface}
{$I castlefonts_abstractfont.inc}
{$I castlefonts_bitmapfont.inc}
{$I castlefonts_fontfamily.inc}
{$I castlefonts_font.inc}
{$I castlefonts_fontsizevariants.inc}
{$I castlefonts_miscellaneous.inc}
{$undef read_interface}

implementation

uses Math, {$ifndef FPC} Character,{$endif}
  CastleGLUtils, CastleUtils, CastleComponentSerialize, CastleInternalRichText,
  CastleLog, CastleURIUtils, CastleRenderContext, CastleInternalGLUtils;

{$define read_implementation}
{$I castlefonts_miscellaneous.inc} // must be first at implementation, to define some internal consts
{$I castlefonts_abstractfont.inc}
{$I castlefonts_bitmapfont.inc}
{$I castlefonts_fontfamily.inc}
{$I castlefonts_font.inc}
{$I castlefonts_fontsizevariants.inc}
{$undef read_implementation}

initialization
  RegisterSerializableComponent(TCastleFont, 'Font');
  RegisterSerializableComponent(TCastleBitmapFont, 'Bitmap Font');
  RegisterSerializableComponent(TCastleFontFamily, 'Font Family');
end.
