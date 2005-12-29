{ translation of AL/alut.h }

var 
  alutInit : procedure(var argc:LongInt; argv:PPChar); alut_call
  alutExit : procedure(); alut_call
  
  alutLoadWAV : function( fname:PChar; var wave:PALvoid; 
    var format, size, bits, freq:TALsizei ):TALboolean; alut_call
    
  alutLoadWAVFile : procedure(afile:PChar; 
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); alut_call
  alutLoadWAVMemory : procedure(memory:PALbyte;
    var format:TALenum; var data:PALvoid; var size, freq:TALsizei;
    var loop:TALboolean); alut_call  
  alutUnloadWAV : procedure(format:TALenum; data:PALvoid; 
    size, freq:TALsizei); alut_call
