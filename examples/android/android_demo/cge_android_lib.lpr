{ -*- compile-command: "sh compile.sh" -*- }
library cge_android_lib;

uses CMem, // TODO: CMem is possibly *not* needed here, only used by CastleAndroidNativeAppGlue
  CastleAndroidNativeAppGlue, Game;

exports ANativeActivity_onCreate;

end.
