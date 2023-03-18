{ Translation of Steam headers (first of all steam_api_flat.h and steam_api_internal.h).
  Note that for yet unknown reason not all Steam calls are accessible from Pascal code
  Therefore only those calls that were tested and proven to work are included.
  Some features can be accessed in more than one way, through different calls
  which was accounted for in this unit.
  We'd like to note invaluable help that Apus Engine's sources by Ivan Polyacov (Cooler2)
  have provided in hunting down those specific calls and a few tricks to make them work properly. }
unit CastleInternalSteamApi;

{$I castleconf.inc}

interface

{$ifdef LINUX}{$ifdef CPU64}
  {$define STEAM_API}
{$endif}{$endif}
{$ifdef MSWINDOWS}{$ifdef CPU64}
  {$define STEAM_API}
{$endif}{$endif}

{$ifdef STEAM_API}
//{$define STEAM_DYNAMIC}

uses
  CastleInternalSteamConstantsAndTypes;

{$ifdef STEAM_DYNAMIC}
  {$I castleinternalsteamapi_dynamic.inc}
{$else}
  {$I castleinternalsteamapi_static.inc}
{$endif}

{$endif}



