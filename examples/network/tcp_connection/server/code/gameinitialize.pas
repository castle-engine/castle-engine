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
  TServer = class
  protected
    FServer: TCastleTCPServer;
    procedure OnConnected(AClient: TClientConnection);
    procedure OnDisconnected(AClient: TClientConnection);
    procedure OnMessageReceived(const AMessage: String; AClient: TClientConnection);
  public
    constructor Create(const APort: Word);
    destructor Destroy; override;
    procedure Send(const AMessage: String);
  end;

type
  { TODO: migrate to use TCastleView }
  TEventsHandler = class(TComponent)
    procedure CreateClick(Sender: TObject);
    procedure SendClick(Sender: TObject);
  end;

var
  Window: TCastleWindow;
  PortEdit, SendEdit: TCastleEdit;
  ResponseLabel: TCastleLabel;
  Server: TServer;
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
  MyLabel.Caption := 'Port:';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -10);
  MyLabel.Color := White;
  Window.Controls.InsertFront(MyLabel);

  PortEdit := TCastleEdit.Create(Application);
  PortEdit.Text := '10244';
  PortEdit.Anchor(hpMiddle);
  PortEdit.Anchor(vpTop, -60);
  Window.Controls.InsertFront(PortEdit);

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Create server';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -110);
  MyButton.OnClick := {$ifdef FPC}@{$endif} EventsHandler.CreateClick;
  Window.Controls.InsertFront(MyButton);

  SendEdit := TCastleEdit.Create(Application);
  SendEdit.Anchor(hpMiddle);
  SendEdit.Anchor(vpTop, -210);
  Window.Controls.InsertFront(SendEdit);

  MyButton := TCastleButton.Create(Application);
  MyButton.Caption := 'Send';
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -260);
  MyButton.OnClick := {$ifdef FPC}@{$endif} EventsHandler.SendClick;
  Window.Controls.InsertFront(MyButton);

  MyLabel := TCastleLabel.Create(Application);
  MyLabel.Caption := 'Response:';
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -310);
  MyLabel.Color := White;
  Window.Controls.InsertFront(MyLabel);

  ResponseLabel := TCastleLabel.Create(Application);
  ResponseLabel.Anchor(hpMiddle);
  ResponseLabel.Anchor(vpTop, -360);
  ResponseLabel.Color := White;
  Window.Controls.InsertFront(ResponseLabel);
end;

constructor TServer.Create(const APort: Word);
begin
  FServer := TCastleTCPServer.Create;
  FServer.Port := APort;

  FServer.OnConnected := {$ifdef FPC}@{$endif} OnConnected;
  FServer.OnDisconnected := {$ifdef FPC}@{$endif} OnDisconnected;
  FServer.OnMessageReceived := {$ifdef FPC}@{$endif} OnMessageReceived;

  FServer.Start;
end;

destructor TServer.Destroy;
begin
  FServer.Free;

  inherited;
end;

procedure TServer.OnConnected(AClient: TClientConnection);
begin
  ResponseLabel.Caption := 'Connected!';
end;

procedure TServer.OnDisconnected(AClient: TClientConnection);
begin
  ResponseLabel.Caption := 'Disconnected!';
end;

procedure TServer.OnMessageReceived(const AMessage: String; AClient: TClientConnection);
begin
  ResponseLabel.Caption := AMessage;
end;

procedure TServer.Send(const AMessage: String);
begin
  FServer.SendToAll(SendEdit.Text);
end;

procedure TEventsHandler.CreateClick(Sender: TObject);
begin
  Server := TServer.Create(StrToInt(PortEdit.Text));
end;

procedure TEventsHandler.SendClick(Sender: TObject);
begin
  if Assigned(Server) then
    Server.Send(SendEdit.Text);
end;

initialization
  ApplicationProperties.ApplicationName := 'server';

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

finalization
  Window.Free;
end.
