unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  DynLibs;

type
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

type
  SteamAPIWarningMessageHook = procedure (nSeverity: Integer; pchDebugText: PAnsiChar); Cdecl;

const
  AppId = UInt32(480);

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitSteam(AppId);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if GetAchievement('ACH_WIN_ONE_GAME') then
  begin
    WriteLn('Achievement set, clearing');
    ClearAchievement('ACH_WIN_ONE_GAME');
  end else
  begin
    WriteLn('Achievement not set, adding');
    SetAchievement('ACH_WIN_ONE_GAME');
  end;
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

