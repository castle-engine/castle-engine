{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Add this unit to your uses clause to enable HTTPS support in
  TCastleDownload.

  In many cases, TCastleDownload can actually support HTTPS out of the box,
  in which case including this unit is unnecessary but still harmless.
  This includes situations when we're using Delphi,
  or when we're using FPC + Android (since then Android's "download_urls"
  service is taking care of HTTPS, https://castle-engine.io/android_services ).

  However, when TCastleDownload is based on top of FpHttpClient,
  you need to use this unit, to make FpHttpClient use the OpenSSL library
  to support HTTPS.
  We do not do this by default, for the same reason that FpHttpClient
  doesn't do this by default.
  This may change in the future and then this unit will do nothing.

  Note for FPC on Linux: If you use FPC on Linux,
  note that older FPC (including 3.2.2)
  does not handle latest OpenSSL library versions.
  If you get errors related to OpenSSL, upgrade to the latest FPC
  (from GitLab) e.g. using @url(https://castle-engine.io/fpcupdeluxe fpcupdeluxe).
}
unit CastleHttps;

{$I castleconf.inc}

interface

{$ifdef HAS_FP_HTTP_CLIENT}
  // FPC 3.x has FpHttpClient, but not OpenSSLSockets
  {$ifndef VER3_0}
    uses OpenSSLSockets;
  {$endif}
{$endif}

implementation

end.
