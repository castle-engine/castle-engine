{ Provides basic classes to interact with Steam callbacks structures
  This is a slightly modified copy of
  https://github.com/Relfos/steamworks_wrappers/blob/master/headers/SteamCallback.pas (MIT license)
  by Sérgio Flores (Relfos) }
Unit CastleInternalSteamCallback;

{$I castleconf.inc}

interface

uses
  CastleInternalSteamConstantsAndTypes;

const
  SteamStatsCallbackID = k_iSteamUserStatsCallbacks + 1;
  SteamLeaderboardCallbackID = k_iSteamUserStatsCallbacks + 5;

type
  PSteam_UserStatsReceived = ^Steam_UserStatsReceived;
  Steam_UserStatsReceived = packed record
		GameID: CGameID;		      // Game these stats are for
		Result: EResult;	  // Success / error fetching the stats
		SteamID: CSteamId;	// The user for whom the stats are retrieved for
	end;

 { PSteam_LeaderboardScoresDownloaded = ^Steam_LeaderboardScoresDownloaded;
	Steam_LeaderboardScoresDownloaded = packed record
		LeaderboardID: SteamLeaderboard;
		LeaderboardEntries: SteamLeaderboardEntries;	// the handle to pass into GetDownloadedLeaderboardEntries()
		EntryCount: Integer; // the number of entries downloaded
  end; }

  PCCallbackBaseVTable = ^TCCallbackBaseVTable;
  TCCallbackBaseVTable = record
    Run,
    Run_2,
    GetCallbackSizeBytes: Pointer;
  end;

  SteamCallbackDispatcher = class;

  PCCallbackInt = ^TCCallbackInt;
  TCCallbackInt = record
    vtable: Pointer;
    _nCallbackFlags: Byte;
    _iCallback: Integer;
    _Dispatcher: SteamCallbackDispatcher;
  end;

  SteamCallbackDelegate = procedure (Answer: Pointer) of object;

  SteamCallbackDispatcher = class
  private
    _SteamInterface: TCCallbackInt;
    _Callback: SteamCallbackDelegate;
    _PropSize: Integer;
    _CallbackID:Integer;
  public
    constructor Create(iCallback: Integer; CallbackProc: SteamCallbackDelegate; a_propsize: Integer); reintroduce;
    destructor Destroy; override;
  end;


implementation
uses
  CastleInternalSteamApi;

{$define STEAM_CALLBACKS_ASM}
{$if defined(LINUX) and defined(CPUAARCH64)}
  {$undef STEAM_CALLBACKS_ASM}
{$endif}
{$if defined(LINUX) and defined(CPUARM)}
  {$undef STEAM_CALLBACKS_ASM}
{$endif}

var
  MyCallbackVTable: TCCallbackBaseVTable;

{$IFDEF CPU64}
procedure MySteamCallback_Run(pvParam: Pointer; pSelf: PCCallbackInt);
begin
  pSelf^._Dispatcher._Callback(Pointer(pvParam));
end;

procedure MySteamCallback_Run_2(Myself, pvParam: PCCallbackInt);
begin
  Myself^._Dispatcher._Callback(Pointer(pvParam));
end;

function MySteamCallback_GetCallbackSizeBytes(Myself: PCCallbackInt): Integer;
begin
  Result := Myself^._Dispatcher._PropSize;
end;
{$ELSE}
{$IFDEF LINUX}
procedure MySteamCallback_Run(pSelf: PCCallbackInt; pvParam: Pointer); Cdecl;
{$ELSE}
procedure MySteamCallback_Run(pvParam: Pointer;  pSelf: PCCallbackInt); Pascal;
{$ENDIF}
begin
  pSelf^._Dispatcher._Callback(Pointer(pvParam));
end;

procedure MySteamCallback_Run_2(pvParam: PCCallbackInt); Pascal;
var
  Myself: PCCallbackInt;
begin
  {$ifdef STEAM_CALLBACKS_ASM}
  asm
    {$IfDef FPC} // FPC uses AT&T syntax
    mov Myself, %ECX;
    {$else} // Delphi uses Intel syntax
    mov Myself, ECX
    {$endif}
  end;
  {$else}
  raise Exception.Create('MySteamCallback_Run_2 not implemented');
  {$endif}
  Myself^._Dispatcher._Callback(Pointer(pvParam));
end;

function MySteamCallback_GetCallbackSizeBytes: Integer; Pascal;
var
  Myself: PCCallbackInt;
begin
  {$ifdef STEAM_CALLBACKS_ASM}
  asm
    {$IfDef FPC} // FPC uses AT&T syntax
    mov Myself, %ECX;
    {$else} // Delphi uses Intel syntax
    mov Myself, ECX
    {$endif}
  end;
  {$else}
  raise Exception.Create('MySteamCallback_GetCallbackSizeBytes not implemented');
  {$endif}
  Result := Myself^._Dispatcher._PropSize;
end;
{$ENDIF}

constructor SteamCallbackDispatcher.Create(iCallback: Integer;
  CallbackProc: SteamCallbackDelegate; a_propsize: Integer);
begin
  _CallbackID := iCallback;
  _Callback := CallbackProc;
  _PropSize := a_propsize;

  _SteamInterface.vtable := @MyCallbackVTable;
  _SteamInterface._nCallbackFlags := 0;
  _SteamInterface._iCallback := 0;
  _SteamInterface._Dispatcher := Self;

  SteamAPI_RegisterCallback(@_SteamInterface, _CallbackID);
end;

destructor SteamCallbackDispatcher.Destroy;
begin
  SteamAPI_UnregisterCallback(@_SteamInterface);
  inherited;
end;

initialization
  MyCallbackVTable.Run := @MySteamCallback_Run;
  MyCallbackVTable.Run_2 := @MySteamCallback_Run_2;
  MyCallbackVTable.GetCallbackSizeBytes := @MySteamCallback_GetCallbackSizeBytes;
end.
