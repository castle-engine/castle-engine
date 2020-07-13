{
  Copyright 2013-2020 Michalis Kamburelis.

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

uses SysUtils, Classes {$ifdef HAS_FP_HTTP_CLIENT}, FpHttpClient {$endif},
  CastleVectors;

{$define read_interface}
{$I castledownload_register.inc}
{$I castledownload_synchronous.inc}
{$I castledownload_download.inc}
{$I castledownload_save.inc}
{$I castledownload_text.inc}
{$I castledownload_strings_helper.inc}
{$I castledownload_utils.inc}
{$undef read_interface}

implementation

uses URIParser, Math, Generics.Collections,
  CastleURIUtils, CastleUtils, CastleLog, CastleInternalZStream,
  CastleClassUtils, CastleDataURI, CastleProgress, CastleStringUtils,
  CastleApplicationProperties, CastleFilesUtils
  {$ifdef ANDROID}, CastleAndroidInternalAssetStream, CastleMessaging {$endif};

{$define read_implementation}
{$I castledownload_register.inc}
{$I castledownload_synchronous.inc}
{$I castledownload_download.inc}
{$I castledownload_save.inc}
{$I castledownload_text.inc}
{$I castledownload_strings_helper.inc}
{$I castledownload_utils.inc}

initialization
finalization
  FreeAndNil(FRegisteredProtocols);
end.
