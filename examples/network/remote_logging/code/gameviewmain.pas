{
  Copyright 2020-2022 Michalis Kamburelis.

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
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleNotifications;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonSendLog: TCastleButton;
    Notifications: TCastleNotifications;

    procedure ClickSendLog(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses CastleLog,
  GameLogHandler;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  Notifications := DesignedComponent('Notifications') as TCastleNotifications;
  ButtonSendLog := DesignedComponent('ButtonSendLog') as TCastleButton;

  ButtonSendLog.OnClick := {$ifdef FPC}@{$endif} ClickSendLog;

  LogNotifications := Notifications;
end;

procedure TViewMain.Stop;
begin
  { Don't let global variable LogNotifications point to no-longer-existing
    TCastleNotifications instance. }
  LogNotifications := nil;
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  WritelnLog('Pressed: ' + Event.ToString);
end;

procedure TViewMain.ClickSendLog(Sender: TObject);
begin
  WritelnLog('Button pressed');
end;

end.
