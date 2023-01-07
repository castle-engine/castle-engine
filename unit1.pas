unit Unit1;

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
uses
  CastleSteam;

{$R *.lfm}

{ TForm1 }

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

procedure WarningHook(nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;
begin
  WriteLn(NSeverity, pchDebugText^);
end;

const
  AppId = UInt32(480);

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitSteam(AppId);

  //SteamAPI_ISteamClient_SetWarningMessageHook(@WarningHook);

  //if SteamAPI_ISteamUser_BLoggedOn() then
    //WriteLn('Login successful');



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
  ShutdownSteam;
  //Sleep(10000);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateSteam;
end;

end.

