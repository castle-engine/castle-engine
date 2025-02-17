{
  Copyright 2023-2024 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit steamuserstats;
{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type
  TSteamUserStats = class(TSteamSubsystem)
  private
    FOnUserStatsReceived: TNotifyEvent;
    FUserStatsReceived: Boolean;
    FAchievements: TStrings;
    function CallbackHandler(const C: TCallbackMsg): Boolean;
    function GetNumAchievements: Int32;
    {$if STEAM_API_VERSION < 1.61}
    procedure CallbackUserStatsReceived(P: PUserStatsReceived);
    procedure DoUserStatsReceived;
    {$endif}
    procedure SetUserStatsReceived(const AnEvent: TNotifyEvent);
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    {$if STEAM_API_VERSION < 1.61}
    function RequestCurrentStats: Boolean;
    {$else}
    procedure DoUserStatsReceived;
    {$endif}
    procedure GetAchievements;
    function ClearAchievement(const AchievementId: String): Boolean;
    function GetAchievementName(I: Int32): PAnsiChar;
    function GetAchievement(AchievementIdAnsi: String; var Achieved: LongBool): Boolean;
    function SetAchievement(AchievementIdAnsi: String): Boolean;
    function IndicateAchievementProgress(const AchievementId: String; const CurrentProgress, MaxProgress: UInt32): Boolean;
    function StoreStats: Boolean;
    property NumAchievements: Int32 read GetNumAchievements;
    property Achievements: TStrings read FAchievements write FAchievements;
    property OnUserStatsReceived: TNotifyEvent
      read FOnUserStatsReceived write SetUserStatsReceived;
    property UserStatsReceived: Boolean read FUserStatsReceived;
  end;

implementation

uses CastleInternalSteamApi, CastleLog;

{ TSteamUserStats }

function TSteamUserStats.ClearAchievement(const AchievementId: String): Boolean;
var
  AchievementIdAnsi: AnsiString;
begin
  AchievementIdAnsi := AchievementId;
  if not SteamAPI_ISteamUserStats_ClearAchievement(FAPIHandle, PAnsiChar(AchievementIdAnsi)) then
    Raise Exception.Create('Failed SteamAPI_ISteamUserStats_ClearAchievement');
end;

constructor TSteamUserStats.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  FAPIHandle := SteamAPI_ISteamClient_GetISteamUserStats(
    SteamClient, SteamUserHandle, SteamPipeHandle, STEAMUSERSTATS_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
  FOnCallback := {$ifdef fpc}@{$endif}CallbackHandler;
  {$if STEAM_API_VERSION >= 1.61}
  FUserStatsReceived := true;
  {$endif}
end;

destructor TSteamUserStats.Destroy;
begin
  inherited;
end;

function TSteamUserStats.GetAchievement(AchievementIdAnsi: String;
  var Achieved: LongBool): Boolean;
var
  CAchieved: TSteamBool;
begin
  Result := False;
  if SteamAPI_ISteamUserStats_GetAchievement(FAPIHandle,
    PAnsiChar(AchievementIdAnsi), @CAchieved) then
      Achieved := CAchieved
  else
    Raise Exception.Create('Failed SteamAPI_ISteamUserStats_GetAchievement');
end;

function TSteamUserStats.GetAchievementName(I: Int32): PAnsiChar;
begin
  Result := SteamAPI_ISteamUserStats_GetAchievementName(FAPIHandle, I);
end;

function TSteamUserStats.GetNumAchievements: Int32;
begin
  Result := SteamAPI_ISteamUserStats_GetNumAchievements(FAPIHandle);
end;

function TSteamUserStats.IndicateAchievementProgress(
  const AchievementId: String; const CurrentProgress, MaxProgress: UInt32): Boolean;
var
  AchievementIdAnsi: AnsiString;
begin
  Result := False;
  AchievementIdAnsi := AchievementId;
  if SteamAPI_ISteamUserStats_IndicateAchievementProgress(FAPIHandle,
     PAnsiChar(AchievementIdAnsi), CurrentProgress, MaxProgress) then
    Result := True
  else
    Raise Exception.Create('Failed SteamAPI_ISteamUserStats_IndicateAchievementProgress');
end;

function TSteamUserStats.SetAchievement(AchievementIdAnsi: String): Boolean;
begin
  Result := SteamAPI_ISteamUserStats_SetAchievement(FAPIHandle, PAnsiChar(AchievementIdAnsi));
end;

procedure TSteamUserStats.SetUserStatsReceived(const AnEvent: TNotifyEvent);
begin
  FOnUserStatsReceived := AnEvent;
  { This caters for 1.61 not having a callback }
  if Assigned(FOnUserStatsReceived) and FUserStatsReceived then
    DoUserStatsReceived;
end;

function TSteamUserStats.StoreStats: Boolean;
begin
  Result := SteamAPI_ISteamUserStats_StoreStats(FAPIHandle);
end;

procedure TSteamUserStats.DoUserStatsReceived;
begin
  WriteLnLog('Steam', 'Received UserStats from Steam');
  FUserStatsReceived := true;
  GetAchievements;
  if Assigned(FOnUserStatsReceived) then
    FOnUserStatsReceived(Self);
end;

{$if STEAM_API_VERSION < 1.61}
function TSteamUserStats.RequestCurrentStats: Boolean;
begin
  Result := SteamAPI_ISteamUserStats_RequestCurrentStats(FAPIHandle);
end;
{$endif}

procedure TSteamUserStats.GetAchievements;
var
  SNumAchievements: UInt32;
  I: Integer;
begin
  FreeAndNil(FAchievements);
  FAchievements := TStringList.Create;

  SNumAchievements := NumAchievements;
  if SNumAchievements > 0 then
    for I := 0 to SNumAchievements - 1 do
      FAchievements.Add(GetAchievementName(I));
  WriteLnLog('Steam Achievements: %d', [Achievements.Count]);
end;

// Callbacks

function TSteamUserStats.CallbackHandler(const C: TCallbackMsg): Boolean;
begin
  Result := True;
  case C.m_iCallback of
    TUserStatsReceived.k_iCallback:
      begin
        {$if STEAM_API_VERSION < 1.61}
        CallbackUserStatsReceived(PUserStatsReceived(C.m_pubParam));
        WritelnLog('>>> UserStatsReceived Callback');
      {$endif}
      end;
    else
      Result := False;
  end;

end;

{$if STEAM_API_VERSION < 1.61}
procedure TSteamUserStats.CallbackUserStatsReceived(P: PUserStatsReceived);
begin
  DoUserStatsReceived;
end;
{$endif}


end.

