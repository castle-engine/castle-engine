{$I castleconf.inc}
{$ifdef CASTLE_PNG_STATIC}
  {$I castlepng_static.inc}
{$else}
  {$I castlepng_dynamic.inc}
{$endif}
