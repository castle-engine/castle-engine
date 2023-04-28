{%MainUnit castleinternalopenal.pas}

{ Unused for now }
{ translation of AL/alut.h }

var
  alutInit : procedure(var argc: CInt; argv: PPChar); cdecl;
  alutExit : procedure(); cdecl;

  alutLoadWAV : function( fname:PChar; var wave:PALvoid;
    var format, size, bits, freq:TALsizei ):TALboolean; cdecl;

  alutLoadWAVFile : procedure(afile:PChar;
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); cdecl;
  alutLoadWAVMemory : procedure(memory:PALbyte;
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); cdecl;
  alutUnloadWAV : procedure(format:TALenum; data:PALvoid;
    size, freq:TALsizei); cdecl;
