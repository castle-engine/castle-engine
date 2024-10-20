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

{ Behaviors for living things that have life, can be alive or dead,
  can fight / cooperate with each other: good for players, NPCs, creatures,
  bots. }
unit CastleLivingBehaviors;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleVectors, CastleTransform, CastleTimeUtils, CastleClassUtils, CastleSectors,
  CastleSoundEngine, CastleBehaviors;

{$define read_interface}
{$I castlelivingbehaviors_living.inc}
{$I castlelivingbehaviors_damage.inc}
{$I castlelivingbehaviors_moveattack.inc}
{$I castlelivingbehaviors_missile.inc}
{$undef read_interface}

implementation

uses SysUtils, Math,
  CastleUtils, CastleLog, CastleBoxes, CastleComponentSerialize,
  CastleCameras, CastleScene;

{$define read_implementation}
{$I castlelivingbehaviors_living.inc}
{$I castlelivingbehaviors_damage.inc}
{$I castlelivingbehaviors_moveattack.inc}
{$I castlelivingbehaviors_missile.inc}

initialization
  RegisterSerializableComponent(TCastleLiving, ['Living (Early Preview - Functionality Will Change!)', 'Living']);
  RegisterSerializableComponent(TCastleMoveAttack, ['Living (Early Preview - Functionality Will Change!)', 'Move Attack']);
  RegisterSerializableComponent(TCastleMoveAttack, ['Living (Early Preview - Functionality Will Change!)', 'Missile']);
end.
