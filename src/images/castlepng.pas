{$I castleconf.inc}
{$ifdef CASTLE_PNG_USING_FCL_IMAGE}

{ @exclude }
unit CastlePng;
interface
{ Empty. CASTLE_PNG_USING_FCL_IMAGE means that we want to use
  PNG implementation inside fcl-image, that does not depend on libpng API. }
implementation
end.

{$else}

{$ifdef CASTLE_PNG_STATIC}
  {$I castlepng_static.inc}
{$else}
  {$I castlepng_dynamic.inc}
{$endif}

{$endif}
