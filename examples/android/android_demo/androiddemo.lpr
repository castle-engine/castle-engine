{ -*- compile-command: "sh compile.sh" -*- }
library AndroidDemo;

uses CMem, // TODO: CMem is possibly *not* needed here, only used by CastleAndroidNativeAppGlue
  CastleAndroidNativeAppGlue, Game;

exports ANativeActivity_onCreate;

end.
