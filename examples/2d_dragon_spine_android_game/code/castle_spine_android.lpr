{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Spine".

  "Castle Spine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Spine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Library to run the game on Android. }
library castle_spine_android;
uses CastleAndroidNativeAppGlue, Game, CastleMessaging;
exports
  { add CastleMesaging JNI stuff, to integrate with various libraries on Android,
    like Google Play Games Services. }
  Java_net_sourceforge_castleengine_MainActivity_jniMessage,
  ANativeActivity_onCreate;
end.
