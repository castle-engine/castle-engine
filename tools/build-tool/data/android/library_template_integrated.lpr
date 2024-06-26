{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in the "Castle Game Engine" distribution,
  for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Library to run the game on Android. }

library ${NAME_PASCAL};

uses
  { CastleAndroidNativeAppGlue must be the 1st unit on uses clause,
    as it uses CThreads that must be loaded before any memory allocation happens. }
  CastleAndroidNativeAppGlue,
  { CastleAutoGenerated must be used before GAME_UNITS, so that it can do InitializeLog
    before any user unit "initialization" runs. }
  CastleAutoGenerated,
  CastleMessaging, CastleSystemLanguage,
  ${GAME_UNITS};

{ Qualify identifiers by unit names below,
  to prevent GAME_UNITS from changing the meaning of code below. }

exports
  CastleMessaging.Java_io_castleengine_MainActivity_jniMessage,
  CastleSystemLanguage.Java_io_castleengine_MainActivity_jniLanguage,
  CastleAndroidNativeAppGlue.ANativeActivity_onCreate;
end.
