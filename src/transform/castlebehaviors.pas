{
  Copyright 2006-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Standard behaviors (TCastleBehavior descendants).
  These express simple mechanics, like
  playing sound (@link(TCastleSoundSource)), billboard (@link(TCastleBillboard)),
  sticking to the surface (@link(TCastleStickToSurface)). }
unit CastleBehaviors;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleTimeUtils, CastleClassUtils, CastleSectors,
  CastleSoundEngine;

{$define read_interface}
{$I castlebehaviors_soundsource.inc}
{$I castlebehaviors_billboard.inc}
{$I castlebehaviors_sticktosurface.inc}
{$undef read_interface}

implementation

uses SysUtils, Math,
  CastleUtils, CastleLog, CastleBoxes, CastleComponentSerialize,
  CastleCameras;

{$define read_implementation}
{$I castlebehaviors_soundsource.inc}
{$I castlebehaviors_billboard.inc}
{$I castlebehaviors_sticktosurface.inc}

initialization
  RegisterSerializableComponent(TCastleSoundSource, 'Sound Source');
  RegisterSerializableComponent(TCastleBillboard, 'Billboard');
  RegisterSerializableComponent(TCastleStickToSurface, 'Stick To Surface');
end.
