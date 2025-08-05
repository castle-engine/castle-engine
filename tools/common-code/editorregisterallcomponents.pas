{
  Copyright 2018-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Register the core CGE components for (de)serialization. }
unit EditorRegisterAllComponents;

interface

implementation

{$warnings off} // do not warn about deprecated Castle2DSceneManager usage

uses CastleViewport, CastleScene, CastleUiControls, CastleControls,
  Castle2DSceneManager, CastleNotifications, CastleThirdPersonNavigation,
  CastleSoundEngine, CastleBehaviors, CastleLivingBehaviors,
  CastleFlashEffect, CastleTiledMap;

{$warnings on}

end.