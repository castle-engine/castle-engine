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

{ Behaviors (TCastleBehavior descendants) useful in games, for example to express
  being alive, or creature intelligence.

  Using these classes in your own games is completely optional,
  for example you can implement "being alive" yourself in your own game trivially,
  without using TCastleAliveBehavior.
  And then you can make your own decisions about various details
  (e.g. is life a float, or integer? does life equal "precisely
  zero" means being still alive, or dead?). }
unit CastleBehaviors;

{$I castleconf.inc}

interface

uses Classes,
  CastleVectors, CastleTransform, CastleTimeUtils, CastleClassUtils, CastleSectors,
  CastleSoundEngine;

{$define read_interface}
{$I castlebehaviors_alive.inc}
{$I castlebehaviors_sound.inc}
//{$I castlebehaviors_moveattack.inc}
{$undef read_interface}

implementation

uses SysUtils, Math,
  CastleUtils, CastleLog, CastleScene, CastleBoxes, CastleComponentSerialize;

{$define read_implementation}
{$I castlebehaviors_alive.inc}
{$I castlebehaviors_sound.inc}
// TODO {$I castlebehaviors_moveattack.inc}

initialization
  RegisterSerializableComponent(TCastleAliveBehavior, 'Alive');
  RegisterSerializableComponent(TCastleSoundBehavior, 'Sound');
  // TODO RegisterSerializableComponent(TCastleMoveAttackBehavior, 'Move Attack');
end.
