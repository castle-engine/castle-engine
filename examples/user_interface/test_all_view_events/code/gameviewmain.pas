{
  Copyright 2004-2023 Michalis Kamburelis.

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
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    RootGroup: TCastleUserInterface;
    LabelFps: TCastleLabel;
    Timer: TCastleTimer;
    LabelCharsPressed: TCastleLabel;
    LabelKeysPressed: TCastleLabel;
    LabelMousePressed: TCastleLabel;
    LabelTouches: TCastleLabel;
    LabelModifierKeys: TCastleLabel;
  private
    Notifications: TCastleNotifications;
    procedure DoTimer(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
    procedure Resize; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleWindow, CastleVectors, CastleColors, CastleStringUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Add TCastleNotifications from code, as for now we don't expose them in editor. }
  Notifications := TCastleNotifications.Create(FreeAtStop);
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpTop, -5);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Color := Green;
  Notifications.MaxMessages := 15;
  Notifications.Timeout := 20000;
  InsertFront(Notifications);

  Timer.OnTimer := {$ifdef FPC}@{$endif}DoTimer;

  Notifications.Show('View started');
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
var
  W: TCastleWindow;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle event

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  Notifications.Show('Press: ' + Event.ToString);

  W := Application.MainWindow;

  case Event.KeyCharacter of
    { cursor tests: }
    'n': Container.OverrideCursor := mcNone;
    'd': Container.OverrideCursor := mcDefault;
    'w': Container.OverrideCursor := mcWait;
    's': Container.OverrideCursor := mcResizeHorizontal;
    { setting mouse position tests: }
    '1': Container.MousePosition := Vector2(0                 , 0);
    '2': Container.MousePosition := Vector2(Container.Width    , 0);
    '3': Container.MousePosition := Vector2(Container.Width    , Container.Height);
    '4': Container.MousePosition := Vector2(0                 , Container.Height);
    '5': Container.MousePosition := Vector2(Container.Width / 2, Container.Height / 2);
    { test TCastleWindow.MessageXxx }
    'm': W.MessageOK('Test information.', mtInfo);
    'q':
      begin
        if W.MessageYesNo('Test question. Would you like to?', mtError) then
          W.MessageOK('You answered yes.', mtInfo)
        else
          W.MessageOK('You answered no.', mtInfo);
      end;
  end;

  // switching FullScreen
  if Event.IsKey(keyF11) {$ifdef DARWIN} and Container.Pressed[keyCtrl] {$endif} then
    Application.MainWindow.FullScreen := not Application.MainWindow.FullScreen;
end;

function TViewMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle event

  Notifications.Show('Release: ' + Event.ToString);
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle event

  Notifications.Show(Format('Motion: old position %s, new position %s, delta %s', [
    Event.OldPosition.ToString,
    Event.Position.ToString,
    (Event.Position - Event.OldPosition).ToString
  ]));
end;

procedure TViewMain.Resize;
begin
  inherited; // allow the ancestor to handle event
  Notifications.Show(Format('Resize: new size (in real device pixels) %d %d, new size with UI scaling: %f %f', [
    Container.Width,
    Container.Height,
    RootGroup.EffectiveWidth,
    RootGroup.EffectiveHeight
  ]));
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  C: Char;
  Key: TKey;
  MouseButton: TCastleMouseButton;
  S: String;
  I: Integer;
begin
  inherited; // allow the ancestor to handle event

  { This virtual method is executed every frame (many times per second). }

  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  S := '';
  for C := Low(C) to High(C) do
    if Container.Pressed.Characters[C] then
      S := SAppendPart(S, ', ', CharToNiceStr(C));
  S := 'Characters pressed: [' + S + ']';
  LabelCharsPressed.Caption := S;

  S := '';
  for Key := Low(Key) to High(Key) do
    if Container.Pressed[Key] then
      S := SAppendPart(S, ', ', KeyToStr(Key));
  S := 'Keys pressed: [' + S + ']';
  LabelKeysPressed.Caption := S;

  LabelModifierKeys.Caption := 'Modifier keys pressed: ' +
    ModifierKeysToNiceStr(Container.Pressed.Modifiers);

  S := '';
  for MouseButton := Low(MouseButton) to High(MouseButton) do
    if MouseButton in Container.MousePressed then
      S := SAppendPart(S, ', ', MouseButtonStr[MouseButton]);
  S := 'Mouse buttons pressed: [' + S + ']';
  LabelMousePressed.Caption := S;

  S := '';
  for I := 0 to Container.TouchesCount - 1 do
    S := SAppendPart(S, ', ', IntToStr(Container.Touches[I].FingerIndex));
  S := 'Fingers touched: [' + S + ']';
  LabelTouches.Caption := S;

  // Just a test that checking for keys, and using MessageOk, works from Update
  if Container.Pressed[keyF12] then
    Application.MainWindow.MessageOk('F12 key pressed. This is just a test that MessageOk works.', mtInfo);
end;

procedure TViewMain.DoTimer(Sender: TObject);
begin
  Notifications.Show(Format('Timer (every 5 seconds): Time now is %s', [FormatDateTime('tt', Time)]));
end;

end.
