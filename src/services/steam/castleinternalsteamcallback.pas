{ Provides basic classes to interact with Steam callbacks structures
  This is a slightly modified copy of
  https://github.com/Relfos/steamworks_wrappers/blob/master/headers/SteamCallback.pas (MIT license)
  by Sérgio Flores (Relfos) }
Unit CastleInternalSteamCallback;

{$I castleconf.inc}

Interface

Uses
  CastleInternalSteamConstantsAndTypes;

Const
  SteamStatsCallbackID = k_iSteamUserStatsCallbacks + 1;
  SteamLeaderboardCallbackID = k_iSteamUserStatsCallbacks + 5;

Type
  PSteam_UserStatsReceived = ^Steam_UserStatsReceived;
  Steam_UserStatsReceived = packed record
		GameID: CGameID;		      // Game these stats are for
		Result: EResult;	  // Success / error fetching the stats
		SteamID: CSteamId;	// The user for whom the stats are retrieved for
	End;

 { PSteam_LeaderboardScoresDownloaded = ^Steam_LeaderboardScoresDownloaded;
	Steam_LeaderboardScoresDownloaded = Packed Record
		LeaderboardID: SteamLeaderboard;
		LeaderboardEntries: SteamLeaderboardEntries;	// the handle to pass into GetDownloadedLeaderboardEntries()
		EntryCount: Integer; // the number of entries downloaded
  End; }

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


Implementation
uses
  CastleInternalSteamApi;

Var
  MyCallbackVTable: TCCallbackBaseVTable;

{$IFDEF CPU64}
Procedure MySteamCallback_Run(pvParam: Pointer; pSelf: PCCallbackInt);
Begin
  pSelf^._Dispatcher._Callback(Pointer(pvParam));
End;

Procedure MySteamCallback_Run_2(Myself, pvParam: PCCallbackInt);
Begin
  Myself^._Dispatcher._Callback(Pointer(pvParam));
End;

Function MySteamCallback_GetCallbackSizeBytes(Myself: PCCallbackInt): Integer;
Begin
  Result := Myself^._Dispatcher._PropSize;
End;
{$ELSE}
{$IFDEF LINUX}
Procedure MySteamCallback_Run(pSelf: PCCallbackInt; pvParam: Pointer); Cdecl;
{$ELSE}
Procedure MySteamCallback_Run(pvParam: Pointer;  pSelf: PCCallbackInt); Pascal;
{$ENDIF}
Begin
  pSelf^._Dispatcher._Callback(Pointer(pvParam));
End;

Procedure MySteamCallback_Run_2(pvParam: PCCallbackInt); Pascal;
Var
  Myself: PCCallbackInt;
Begin
Asm
  mov Myself, %ECX;
End;
  Myself^._Dispatcher._Callback(Pointer(pvParam));
End;

Function MySteamCallback_GetCallbackSizeBytes: Integer; Pascal;
Var
  Myself: PCCallbackInt;
Begin
  Asm
    mov Myself, %ECX;
  End;
  Result := Myself^._Dispatcher._PropSize;
End;
{$ENDIF}

constructor SteamCallbackDispatcher.Create(iCallback: Integer;
  CallbackProc: SteamCallbackDelegate; a_propsize: Integer);
Begin
  _CallbackID := iCallback;
  _Callback := CallbackProc;
  _PropSize := a_propsize;

  _SteamInterface.vtable := @MyCallbackVTable;
  _SteamInterface._nCallbackFlags := 0;
  _SteamInterface._iCallback := 0;
  _SteamInterface._Dispatcher := Self;

  SteamAPI_RegisterCallback(@_SteamInterface, _CallbackID);
End;

destructor SteamCallbackDispatcher.Destroy;
Begin
  SteamAPI_UnregisterCallback(@_SteamInterface);
  inherited;
End;

Initialization
  MyCallbackVTable.Run := @MySteamCallback_Run;
  MyCallbackVTable.Run_2 := @MySteamCallback_Run_2;
  MyCallbackVTable.GetCallbackSizeBytes := @MySteamCallback_GetCallbackSizeBytes;
End.
