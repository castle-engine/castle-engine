{
  Copyright 2016-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit CastleFontFamily deprecated 'use CastleFonts unit for TCastleFontFamily';

{$I castleconf.inc}

interface

uses CastleFonts;

type
  TCastleFontFamily = CastleFonts.TCastleFontFamily deprecated 'use TCastleFontFamily from CastleFonts unit';
  TFontFamily = CastleFonts.TCastleFontFamily deprecated 'use TCastleFontFamily from CastleFonts unit';

implementation

end.
