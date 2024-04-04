{
  Copyright 2018-2024 Benedikt Magnus, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleNotifications, CastleClientServer;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    EditHostname: TCastleEdit;
    EditPort: TCastleIntegerEdit;
    ButtonCreateClient: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    LabelMessageContents: TCastleLabel;
    Notifications: TCastleNotifications;
  private
    FClient: TCastleTCPClient;
    procedure HandleConnected;
    procedure HandleDisconnected;
    procedure HandleMessageReceived(const AMessage: String);
    procedure ClickCreate(Sender: TObject);
    procedure ClickSend(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonCreateClient.OnClick := {$ifdef FPC}@{$endif} ClickCreate;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.HandleConnected;
begin
  Notifications.Show('Connected to server');
end;

procedure TViewMain.HandleDisconnected;
begin
  Notifications.Show('Disconnected from server');
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String);
begin
  Notifications.Show('Received message: ' + AMessage);
  LabelMessageContents.Caption := AMessage;
end;

procedure TViewMain.ClickCreate(Sender: TObject);
begin
  FClient := TCastleTCPClient.Create;
  FClient.Hostname := EditHostname.Text;
  FClient.Port := EditPort.Value;

  FClient.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
  FClient.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
  FClient.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;

  FClient.Connect;

  ButtonCreateClient.Enabled := false;
  ButtonSend.Enabled := true;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FClient.Send(EditSend.Text);
end;

end.
