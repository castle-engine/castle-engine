{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

uses
  Classes, SysUtils,
  CastleWindow, CastleApplicationProperties,
  CastleControls, CastleUIControls,
  CastleClientServer;

type
  TClient = class
  protected
    FClient: TCastleTCPClient;
    procedure OnConnected;
    procedure OnDisconnected;
    procedure OnMessageReceived (const AMessage: String);
  public
    constructor Create (const AHost: String; const APort: Word);
    destructor Destroy; override;
    procedure Send (const AMessage: String);
  end;

type
  TEventsHandler = class(TComponent)
    procedure CreateClick(Sender: TObject);
    procedure SendClick(Sender: TObject);
  end;

var
  Window: TCastleWindow;
  HostEdit, PortEdit, SendEdit: TCastleEdit;
  ResponseLabel: TCastleLabel;
  Client: TClient;
  Connection: TClientConnection;
  EventsHandler: TEventsHandler;

implementation

uses CastleColors;

{ One-time initialization. }
procedure ApplicationInitialize;
var
  MyButton: TCastleButton;
  MyLabel: TCastleLabel;
begin
  EventsHandler := TEventsHandler.Create(Application);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Hostname:';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -10);
  MyLabel.Color := White;
  Window.Controls.InsertFront(MyLabel);

  HostEdit := TCastleEdit.Create(Application);
  HostEdit.Text := 'localhost';
  HostEdit.Anchor(hpMiddle);
  HostEdit.Anchor(vpTop, -60);
  Window.Controls.InsertFront(HostEdit);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Port:';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -110);
  MyLabel.Color := White;
  Window.Controls.InsertFront(MyLabel);

  PortEdit := TCastleEdit.Create(Application);
  PortEdit.Text := '10244';
  PortEdit.Anchor(hpMiddle);
  PortEdit.Anchor(vpTop, -160);
  Window.Controls.InsertFront(PortEdit);

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Create client';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -210);
  MyButton.OnClick := {$ifdef FPC}@{$endif} EventsHandler.CreateClick;
  Window.Controls.InsertFront(MyButton);

  SendEdit := TCastleEdit.Create(Application);
  SendEdit.Anchor(hpMiddle);
  SendEdit.Anchor(vpTop, -310);
  Window.Controls.InsertFront(SendEdit);

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Send';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -360);
  MyButton.OnClick := {$ifdef FPC}@{$endif} EventsHandler.SendClick;
  Window.Controls.InsertFront(MyButton);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Response:';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -410);
  MyLabel.Color := White;
  Window.Controls.InsertFront(MyLabel);

  ResponseLabel := TCastleLabel.Create(Application);
  ResponseLabel.Anchor(hpMiddle);
  ResponseLabel.Anchor(vpTop, -460);
  ResponseLabel.Color := White;
  Window.Controls.InsertFront(ResponseLabel);
end;

constructor TClient.Create (const AHost: String; const APort: Word);
begin
  FClient := TCastleTCPClient.Create;
  FClient.Hostname := AHost;
  FClient.Port := APort;

  FClient.OnConnected := {$ifdef FPC}@{$endif} OnConnected;
  FClient.OnDisconnected := {$ifdef FPC}@{$endif} OnDisconnected;
  FClient.OnMessageReceived := {$ifdef FPC}@{$endif} OnMessageReceived;

  FClient.Connect;
end;

destructor TClient.Destroy;
begin
  FClient.Free;

  inherited;
end;

procedure TClient.OnConnected;
begin
  ResponseLabel.Caption := 'Connected!';
end;

procedure TClient.OnDisconnected;
begin
  ResponseLabel.Caption := 'Disconnected!';
end;

procedure TClient.OnMessageReceived (const AMessage: String);
begin
  ResponseLabel.Caption := AMessage;
end;

procedure TClient.Send (const AMessage: String);
begin
  FClient.Send(SendEdit.Text);
end;

procedure TEventsHandler.CreateClick(Sender: TObject);
begin
  Client := TClient.Create(HostEdit.Text, StrToInt(PortEdit.Text));
end;

procedure TEventsHandler.SendClick(Sender: TObject);
begin
  if Assigned(Client) then
    Client.Send(SendEdit.Text);
end;

initialization
  ApplicationProperties.ApplicationName := 'client';

  { initialize Application callbacks }
  Application.OnInitialize := {$ifdef FPC}@{$endif} ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

finalization
  Window.Free;
end.
