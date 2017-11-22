{$I castleconf.inc}

{ Internal libpng integration.
  @exclude This unit is not ready for PasDoc, as it is internal,
  and also has comments not formatted for PasDoc.
}

{$ifdef CASTLE_PNG_USING_FCL_IMAGE} {$info PNG loading with FpImage} {$endif}
{$ifdef CASTLE_PNG_DYNAMIC} {$info PNG loading with dynamic libpng} {$endif}
{$ifdef CASTLE_PNG_STATIC} {$info PNG loading with static libpng} {$endif}

unit CastleInternalPng;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}

interface
{ Empty. CASTLE_PNG_USING_FCL_IMAGE means that we want to use
  PNG implementation inside fcl-image, that does not depend on libpng API. }
implementation
end.

{$else}

{$ifdef CASTLE_PNG_STATIC}
  {$ifdef CASTLE_PNG_DYNAMIC}
    {$error Cannot define both CASTLE_PNG_STATIC and CASTLE_PNG_DYNAMIC}
  {$endif}

  {$I castleinternalpng_static.inc}
{$else}
  {$ifndef CASTLE_PNG_DYNAMIC}
    {$error Not defined any PNG access method (CASTLE_PNG_USING_FCL_IMAGE or CASTLE_PNG_STATIC and CASTLE_PNG_DYNAMIC)}
  {$endif}

  {$I castleinternalpng_dynamic.inc}
{$endif}

{$endif}
