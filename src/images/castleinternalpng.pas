{$I castleconf.inc}

{ Internal libpng integration.
  @exclude This unit is not ready for PasDoc, as it is internal,
  and also has comments not formatted for PasDoc.
}

unit CastleInternalPng;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}

interface
{ Empty. CASTLE_PNG_USING_FCL_IMAGE means that we want to use
  PNG implementation inside fcl-image, that does not depend on libpng API. }
implementation
end.

{$else}

{$ifdef CASTLE_PNG_STATIC}
  {$I castleinternalpng_static.inc}
{$else}
  {$I castleinternalpng_dynamic.inc}
{$endif}

{$endif}
