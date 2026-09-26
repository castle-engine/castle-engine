{$I castleconf.inc} // hide useless Delphi hints/warnings

{$ifdef FPC}
  {$if defined(fpc) or defined(NodeJS)}
    {$define HasFileWriter}
  {$endif}

  //{$IF FPC_FULLVERSION>30100}
  //  {$warn 6058 off} // cannot inline
  //{$ENDIF}
{$else}
  {$warn USE_BEFORE_DEF off}
{$endif}
