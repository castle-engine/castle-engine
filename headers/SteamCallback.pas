Unit SteamCallback;

Interface

Uses SteamAPI;

Const
  SteamStatsCallbackID = k_iSteamUserStatsCallbacks + 1;
  SteamLeaderboardCallbackID = k_iSteamUserStatsCallbacks + 5;

Type
  PSteam_UserStatsReceived = ^Steam_UserStatsReceived;
  Steam_UserStatsReceived = Packed Record
		GameID:SteamGameID;		      // Game these stats are for
		Result:SteamResult;	  // Success / error fetching the stats
		steamID:SteamID;	// The user for whom the stats are retrieved for
	End;

  PSteam_LeaderboardScoresDownloaded = ^Steam_LeaderboardScoresDownloaded;
	Steam_LeaderboardScoresDownloaded = Packed Record
		LeaderboardID:SteamLeaderboard;
		LeaderboardEntries:SteamLeaderboardEntries;	// the handle to pass into GetDownloadedLeaderboardEntries()
		EntryCount:Integer; // the number of entries downloaded
  End;

  PCCallbackBaseVTable = ^TCCallbackBaseVTable;
  TCCallbackBaseVTable = Record
    Run,
    Run_2,
    GetCallbackSizeBytes: pointer;
  End;

  SteamCallbackDispatcher = Class;

  PCCallbackInt = ^TCCallbackInt;
  TCCallbackInt = Record
    vtable:Pointer;
    _nCallbackFlags: byte;
    _iCallback: integer;
    _Dispatcher:SteamCallbackDispatcher;
  End;

  SteamCallbackDelegate = Procedure(answer:Pointer) of Object;

  SteamCallbackDispatcher = Class
    Private
      _SteamInterface: TCCallbackInt;
      _Callback: SteamCallbackDelegate;
      _PropSize: Integer;
      _CallbackID:Integer;

    Public
      Constructor Create(iCallback:integer; CallbackProc:SteamCallbackDelegate; _propsize:integer); Reintroduce;
      Destructor Destroy; Override;
  End;


Implementation

{ TSteamCallback }

Var
  MyCallbackVTable: TCCallbackBaseVTable;

{$IFDEF CPU64}
Procedure MySteamCallback_Run(pvParam: Pointer; pSelf: PCCallbackInt);
Begin
  pSelf._Dispatcher._Callback(Pointer(pvParam));
End;

Procedure MySteamCallback_Run_2(myself, pvParam: PCCallbackInt);
Begin
  Myself._Dispatcher._Callback(Pointer(pvParam));
End;

Function MySteamCallback_GetCallbackSizeBytes(myself: PCCallbackInt):Integer;
Begin
  Result := Myself._Dispatcher._PropSize;
End;
{$ELSE}
{$IFDEF LINUX}
Procedure MySteamCallback_Run(pSelf: PCCallbackInt; pvParam: Pointer); Cdecl; {$ELSE}
Procedure MySteamCallback_Run(pvParam: Pointer;  pSelf: PCCallbackInt); Pascal;{$ENDIF}
Begin
  pSelf._Dispatcher._Callback(Pointer(pvParam));
End;

Procedure MySteamCallback_Run_2(pvParam: PCCallbackInt); Pascal;
Var
  myself: PCCallbackInt;
Begin
Asm
  mov myself, ECX;
End;
  Myself._Dispatcher._Callback(Pointer(pvParam));
End;

Function MySteamCallback_GetCallbackSizeBytes:Integer; Pascal;
Var
  myself: PCCallbackInt;
Begin
  Asm
    mov myself, ECX;
  End;
  Result := Myself._Dispatcher._PropSize;
End;
{$ENDIF}

Constructor SteamCallbackDispatcher.Create(iCallback:integer; callbackProc:SteamCallbackDelegate; _propsize: integer);
Begin
  _CallbackID := iCallback;
  _Callback := callbackProc;
  _PropSize := _propsize;

  _SteamInterface.vtable := @MyCallbackVTable;
  _SteamInterface._nCallbackFlags := 0;
  _SteamInterface._iCallback := 0;
  _SteamInterface._Dispatcher := Self;

  SteamAPI_RegisterCallback(@_SteamInterface, _CallbackID);
End;

Destructor SteamCallbackDispatcher.Destroy;
Begin
  SteamAPI_UnregisterCallback(@_SteamInterface);
End;

Initialization
  MyCallbackVTable.Run := @MySteamCallback_Run;
  MyCallbackVTable.Run_2 := @MySteamCallback_Run_2;
  MyCallbackVTable.GetCallbackSizeBytes := @MySteamCallback_GetCallbackSizeBytes;
End.
