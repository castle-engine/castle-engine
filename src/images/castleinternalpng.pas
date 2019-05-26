{$I castleconf.inc}

{ Internal Libpng integration.
  @exclude This unit is not ready for PasDoc, as it is internal,
  and also has comments not formatted for PasDoc.
}

unit CastleInternalPng;

{ This is also checked by castleconf.inc, but check it here again to be sure. }
{$if defined(CASTLE_PNG_DYNAMIC) and defined(CASTLE_PNG_STATIC)}
  {$error Cannot define both CASTLE_PNG_DYNAMIC and CASTLE_PNG_STATIC. Define at most one of them.}
{$endif}

{$if defined(CASTLE_PNG_STATIC)}
  {$info PNG loading with static Libpng}
  {$I castleinternalpng_static.inc}

{$elseif defined(CASTLE_PNG_DYNAMIC)}
  {$info PNG loading with dynamic Libpng (fallback on FPImage)}
  {$I castleinternalpng_dynamic.inc}

{$else}
  {$info PNG loading with FPImage (Libpng disabled)}
  interface
  { Empty. We will not use Libpng API in this case. }
  const
    CastlePngInitialized = false;
  implementation
  end.
{$endif}
