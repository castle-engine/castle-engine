{$I castleconf.inc} // hide useless Delphi hints/warnings

{$ifdef FPC}
  {$mode objfpc}{$H+}
  {$if defined(fpc) or defined(NodeJS)}
    {$define HasFileWriter}
  {$endif}

  //{$IF FPC_FULLVERSION>30100}
  //  {$warn 6058 off} // cannot inline
  //{$ENDIF}
{$endif}
