{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container for 2D controls able to render using OpenGL (TGLContainer). }
unit CastleGLContainer deprecated 'everything from this unit is now in CastleUIControls';

{$I castleconf.inc}

interface

uses Classes,
  CastleUIControls, CastleRectangles, CastleColors, CastleImages;

type
  TGLContainer = TCastleContainer
    deprecated 'use TCastleContainer from CastleUIControls unit, no need to use CastleGLContainer unit';

function RenderControlToImage(const Container: TGLContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;
  deprecated 'use RenderControlToImage from CastleUIControls unit, no need to use CastleGLContainer unit';

implementation

{ global routines ------------------------------------------------------------ }

function RenderControlToImage(const Container: TGLContainer;
  const Control: TCastleUserInterface;
  const ViewportRect: TRectangle;
  const BackgroundColor: TCastleColor): TRGBAlphaImage;
begin
  Result := CastleUIControls.RenderControlToImage(
    Container, Control, ViewportRect, BackgroundColor);
end;

end.
