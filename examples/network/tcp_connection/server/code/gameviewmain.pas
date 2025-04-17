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
  CastleKeysMouse, CastleClientServer, CastleNotifications;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    EditPort: TCastleIntegerEdit;
    ButtonCreateServer: TCastleButton;
    ButtonDestroyServer: TCastleButton;
    EditSend: TCastleEdit;
    ButtonSend: TCastleButton;
    LabelMessageContents: TCastleLabel;
    Notifications: TCastleNotifications;
  private
    FServer: TCastleTCPServer;
    procedure HandleConnected(AClient: TClientConnection);
    procedure HandleDisconnected(AClient: TClientConnection);
    procedure HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
    procedure ClickCreateServer(Sender: TObject);
    procedure ClickDestroyServer(Sender: TObject);
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

uses SysUtils,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonCreateServer.OnClick := {$ifdef FPC}@{$endif} ClickCreateServer;
  ButtonDestroyServer.OnClick := {$ifdef FPC}@{$endif} ClickDestroyServer;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption :=
    'FPS: ' + Container.Fps.ToString + NL +
    'Server Created: ' + BoolToStr(FServer <> nil, true) + NL +
    'Any Client Connected: ' + BoolToStr((FServer <> nil) and FServer.IsConnected, true);

  ButtonCreateServer.Enabled := FServer = nil;
  ButtonDestroyServer.Enabled := FServer <> nil;
  ButtonSend.Enabled := FServer <> nil;
end;

procedure TViewMain.HandleConnected(AClient: TClientConnection);
begin
  Notifications.Show('Client connected');
end;

procedure TViewMain.HandleDisconnected(AClient: TClientConnection);
begin
  Notifications.Show('Client disconnected');
end;

procedure TViewMain.HandleMessageReceived(const AMessage: String; AClient: TClientConnection);
begin
  Notifications.Show('Message from client: ' + AMessage);
  LabelMessageContents.Caption := AMessage;
end;

procedure TViewMain.ClickCreateServer(Sender: TObject);
begin
  FServer := TCastleTCPServer.Create;
  FServer.Port := EditPort.Value;

  FServer.OnConnected := {$ifdef FPC}@{$endif} HandleConnected;
  FServer.OnDisconnected := {$ifdef FPC}@{$endif} HandleDisconnected;
  FServer.OnMessageReceived := {$ifdef FPC}@{$endif} HandleMessageReceived;

  FServer.Start;
end;

procedure TViewMain.ClickDestroyServer(Sender: TObject);
begin
  if FServer <> nil then
  begin
    FServer.Stop;
    FreeAndNil(FServer);
  end;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  FServer.SendToAll(EditSend.Text);
end;

end.
