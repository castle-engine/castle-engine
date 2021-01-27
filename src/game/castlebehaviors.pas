{
  Copyright 2006-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Standard behaviors (TCastleBehavior descendants).
  For example to express life, or creature intelligence, or playing sound. }
unit CastleBehaviors;

{$I castleconf.inc}

interface

uses Classes,
  CastleVectors, CastleTransform, CastleTimeUtils, CastleClassUtils, CastleSectors,
  CastleSoundEngine;

{$define read_interface}
{$I castlebehaviors_alive.inc}
{$I castlebehaviors_sound.inc}
{$I castlebehaviors_billboard.inc}
//{$I castlebehaviors_moveattack.inc}
{$undef read_interface}

implementation

uses SysUtils, Math,
  CastleUtils, CastleLog, CastleScene, CastleBoxes, CastleComponentSerialize,
  CastleCameras;

{$define read_implementation}
{$I castlebehaviors_alive.inc}
{$I castlebehaviors_sound.inc}
{$I castlebehaviors_billboard.inc}
// TODO {$I castlebehaviors_moveattack.inc}

initialization
  RegisterSerializableComponent(TCastleAliveBehavior, 'Alive');
  RegisterSerializableComponent(TCastleSound, 'Sound');
  RegisterSerializableComponent(TCastleBillboard, 'Billboard');
  // TODO RegisterSerializableComponent(TCastleMoveAttack, 'Move Attack');
end.
