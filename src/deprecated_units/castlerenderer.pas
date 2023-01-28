{
  Copyright 2002-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Backward-compatibility, provide TRenderingAttributes type. }
unit CastleRenderer deprecated 'use CastleRenderOptions to access TCastleRenderOptions';

interface

uses CastleRenderOptions;

type
  { Various properties that control rendering. }
  TRenderingAttributes = CastleRenderOptions.TCastleRenderOptions;

implementation

end.
