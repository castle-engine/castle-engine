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

{ Add this unit to your uses clause to enable HTTPS support with
  TCastleDownload.

  In some cases, TCastleDownload can actually support HTTPS out of the box,
  in which case including this unit is unnecessary but still harmless.

  This unit is unnecessary when:

  @unorderedList(
    @item(When we're using Delphi,)

    @item(When we're using FPC + Android (since then Android's "download_urls"
      service is taking care of HTTPS, https://castle-engine.io/android_services ).)
  )

  In other cases, we use FPC and FpHtpClient.
  Then this unit does something useful: we need to use FPC OpenSslSockets
  unit to enable HTTPS.

  Moreover, with FPC 3.2.2 and Linux (or Unix, more generally)
  we need to use our custom ForFpc32xOpenSslSockets unit, otherwise we
  get "Could not initialize OpenSSL library", as FPC is trying to access
  outdated OpenSSL version (not available in latest Linux distros).

  Note: We do not use this unit by default, for the same reason that FpHttpClient
  doesn't do this by default.
  This may change in the future and then this unit will do nothing.
}
unit CastleHttps;

{$I castleconf.inc}

interface

{$ifdef HAS_FP_HTTP_CLIENT}
  {$if defined(VER3_2) and defined(UNIX)}
    { FPC 3.2.2 OpenSsl on Linux does not work with OpenSSL library,
      it tries to access outdated OpenSSL version (not available on latest
      Linux). }
    uses ForFpc32xOpenSslSockets;
  {$else}
    uses OpenSslSockets;
  {$endif}
{$endif}

implementation

end.
