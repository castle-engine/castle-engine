{ translation of AL/alut.h }

var 
  alutInit : procedure(var argc: CInt; argv: PPChar); extdecl;
  alutExit : procedure(); extdecl;
  
  alutLoadWAV : function( fname:PChar; var wave:PALvoid; 
    var format, size, bits, freq:TALsizei ):TALboolean; extdecl;
    
  alutLoadWAVFile : procedure(afile:PChar; 
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); extdecl;
  alutLoadWAVMemory : procedure(memory:PALbyte;
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); extdecl;  
  alutUnloadWAV : procedure(format:TALenum; data:PALvoid; 
    size, freq:TALsizei); extdecl;
