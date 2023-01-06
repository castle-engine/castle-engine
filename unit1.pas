unit Unit1;

{$apptype console}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DynLibs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
begin
  WriteLn(NSeverity, pchDebugText^);
end;

const
  SteamLib = 'steam_api64';

const
  AppId = UInt32(480);

{ char *ver = PAnsiChar }

{ steam_api.h : See full documentation at https://partner.steamgames.com/doc/api/steam_api }
function SteamAPI_Init(): Boolean; CDecl; external SteamLib;
//procedure SteamAPI_ReleaseCurrentThreadMemory(); CDecl; external SteamLib;
function SteamAPI_RestartAppIfNecessary(unOwnAppID: UInt32): Boolean; CDecl; external SteamLib;
procedure SteamAPI_RunCallbacks(); CDecl; external SteamLib;
//procedure SteamAPI_SetMiniDumpComment( const char *pchMsg );
procedure SteamAPI_Shutdown(); CDecl; external SteamLib;
//procedure SteamAPI_WriteMiniDump( uint32 uStructuredExceptionCode, void* pvExceptionInfo, uint32 uBuildID );

const
  STEAMCLIENT_INTERFACE_VERSION = 'SteamClient020'; // in isteamclient.h, I don't know how to pull it from there

{ steam_api_internal.h : undocumented? }

//function SteamInternal_ContextInit( void *pContextInitData ): Pointer;
function SteamInternal_CreateInterface(SteamClientInterfaceVersion: PAnsiChar): Pointer; CDecl; external SteamLib;
//function SteamInternal_FindOrCreateUserInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;
//function SteamInternal_FindOrCreateGameServerInterface( HSteamUser hSteamUser, const char *pszVersion ): Pointer;

{ isteamuser.h : See documentation at https://partner.steamgames.com/doc/api/ISteamClient }

type
  HSteamPipe = Int32;
  HSteamUser = Int32;
  //CSteamId = UInt64; // Why the documentation says it's struct?

{ steam_api_internal.h : undocumented? }

function SteamAPI_GetHSteamUser(): HSteamUser; CDecl; external SteamLib;
function SteamAPI_GetHSteamPipe(): HSteamPipe; CDecl; external SteamLib;
//function GetSteamID(): CSteamId; CDecl; external SteamLib;
//function GetPlayerSteamLevel(): Integer; CDecl; external SteamLib;

//function SteamAPI_ISteamUserStats_SetAchievement(pchName: PAnsiChar): Boolean; CDecl; external SteamLib;
//function SteamAPI_ISteamUtils_GetAppID(): UInt32; CDecl; external SteamLib;
//function SteamAPI_ISteamUser_BLoggedOn(): Boolean; CDecl; external SteamLib;
//procedure SteamAPI_ISteamClient_SetWarningMessageHook(pFunction: SteamAPIWarningMessageHook); CDecl; external SteamLib;

procedure TForm1.FormCreate(Sender: TObject);
var
  SteamInterface: Pointer;
  SteamUserHandle: HSteamUser;
  SteamPipeHandle: HSteamPipe;
begin
  if SteamAPI_Init() then
  begin
    WriteLn('log: SteamAPI_Init successfull');

    if SteamAPI_RestartAppIfNecessary(AppId) then
    begin
      WriteLn('The app was run through exe - restarting through Steam. DRM will do this automatically.');
      Halt(0);
    end else
      WriteLn('The Steam client is running and no restart is necessary');
  end else
    WriteLn('FATAL: SteamAPI_Init failed!');

  SteamUserHandle := SteamAPI_GetHSteamUser();
  SteamPipeHandle := SteamAPI_GetHSteamPipe();

  //SteamAPI_ISteamClient_SetWarningMessageHook(@WarningHook);

  //if SteamAPI_ISteamUser_BLoggedOn() then
    WriteLn('Login successful');

  SteamInterface := SteamInternal_CreateInterface(PAnsiChar(STEAMCLIENT_INTERFACE_VERSION));

  //https://partner.steamgames.com/doc/features/achievements

  // ISteamUserStats::RequestCurrentStats
  // ISteamUserStats::GetAchievement

  {If you want to display the achievements in your game you can use ISteamUserStats::GetAchievementDisplayAttribute to retrieve human-readable properties of the achievement, including its name ("name") and description ("desc"). These properties are localizable on the Steamworks Partner Website, and the returned data varies with the language in which the user is running the game. You can also get an achievement's icon using ISteamUserStats::GetAchievementIcon or the time each achievements was unlocked with ISteamUserStats::GetAchievementAndUnlockTime.}

  //ISteamUserStats::SetAchievement
  //ISteamUserStats::StoreStats
  //wait for ISteamUserStats::UserAchievementStored_t
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AchievementString: AnsiString;
begin
  //WriteLn('SteamAPI_ISteamUtils_GetAppID = ', SteamAPI_ISteamUtils_GetAppID());

  AchievementString := 'ACH_WIN_ONE_GAME';
  //SteamAPI_ISteamUserStats_SetAchievement(PAnsiChar(AchievementString));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SteamAPI_Shutdown();
  //Sleep(10000);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SteamAPI_RunCallbacks();
end;

end.

