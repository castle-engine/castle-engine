{
  Copyright 2013-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Read and write stream contents from URLs. }
unit CastleDownload;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleVectors;

{$define read_interface}
{$I castledownload_register.inc}
{$I castledownload_synchronous.inc}
{$I castledownload_asynchronous.inc}
{$I castledownload_save.inc}
{$I castledownload_text.inc}
{$I castledownload_strings_helper.inc}
{$I castledownload_mime.inc}
{$I castledownload_utils.inc}
{$undef read_interface}

implementation

{ What to use with Delphi to download http(s) URLs?

  - Define CASTLE_DELPHI_NET_HTTP_CLIENT to use System.Net.HttpClient
    and TNetHTTPClient with Delphi.
    Doesn't require any external DLLs.
    But it is very slow to download larger files.

  - Do not define CASTLE_DELPHI_NET_HTTP_CLIENT to use IdHttp, Indy.
    Requires OpenSSL DLLs.
}
{.$define CASTLE_DELPHI_NET_HTTP_CLIENT}

uses URIParser, Math, Generics.Collections,
  {$define read_implementation_uses}
  {$I castledownload_url_http_android.inc}
  {$I castledownload_url_http_fphttpclient.inc}
  {$I castledownload_url_http_delphi_net.inc}
  {$I castledownload_url_http_indy.inc}
  {$undef read_implementation_uses}
  {$if defined(VER3_2) and defined(DARWIN) and not defined(CASTLE_IOS)}
    { for ESocketError } SSockets,
  {$endif}
  CastleURIUtils, CastleUtils, CastleLog, CastleInternalZStream,
  CastleClassUtils, CastleInternalDataUri, CastleStringUtils,
  CastleApplicationProperties, CastleFilesUtils;

{$define read_implementation}
{$I castledownload_internal_utils.inc}

// handlers of various URL protocols
{$I castledownload_url_castleandroidassets.inc}
{$I castledownload_url_castlescript.inc}
{$I castledownload_url_compiled.inc}
{$I castledownload_url_data.inc}
{$I castledownload_url_ecmascript.inc}
{$I castledownload_url_file.inc}
{$I castledownload_url_http_android.inc}
{$I castledownload_url_http_fphttpclient.inc}
{$I castledownload_url_http_delphi_net.inc}
{$I castledownload_url_http_indy.inc}

{$I castledownload_register.inc}
{$I castledownload_synchronous.inc}
{$I castledownload_asynchronous.inc}
{$I castledownload_save.inc}
{$I castledownload_text.inc}
{$I castledownload_strings_helper.inc}
{$I castledownload_mime.inc}
{$I castledownload_utils.inc}

initialization
finalization
  FreeAndNil(FRegisteredProtocols);
end.
