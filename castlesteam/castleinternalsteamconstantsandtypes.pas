unit CastleInternalSteamConstantsAndTypes;

{$mode ObjFPC}{$H+}

interface

const
  SteamLib = 'steam_api64';

const
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; // in isteamclient.h, I don't know how to pull it from there

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  //CSteamId = UInt64; // Why the documentation says it's struct?

implementation

end.

