{
  Copyright 2024-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelScreenStatus: TCastleLabel;
    ButtonResolution1024x768: TCastleButton;
    ButtonResolution1280x720: TCastleButton;
    ButtonResolutionCustom: TCastleButton;
    EditResolutionWidth: TCastleIntegerEdit;
    EditResolutionHeight: TCastleIntegerEdit;
    ButtonResolutionDefault: TCastleButton;
  private
    procedure ClickResolution1024x768(Sender: TObject);
    procedure ClickResolution1280x720(Sender: TObject);
    procedure ClickResolutionCustom(Sender: TObject);
    procedure ClickResolutionDefault(Sender: TObject);
    procedure ChangeResolution(const NewWidth, NewHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleWindow, CastleMessages;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonResolution1024x768.OnClick := {$ifdef FPC}@{$endif} ClickResolution1024x768;
  ButtonResolution1280x720.OnClick := {$ifdef FPC}@{$endif} ClickResolution1280x720;
  ButtonResolutionCustom.OnClick := {$ifdef FPC}@{$endif} ClickResolutionCustom;
  ButtonResolutionDefault.OnClick := {$ifdef FPC}@{$endif} ClickResolutionDefault;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;

  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  LabelScreenStatus.Caption := Format('Current Screen Resolution: %d x %d', [
    Application.ScreenWidth,
    Application.ScreenHeight
  ]);
end;

procedure TViewMain.ChangeResolution(const NewWidth, NewHeight: Integer);
begin
  Application.VideoResizeWidth := NewWidth;
  Application.VideoResizeHeight := NewHeight;
  Application.VideoResize := true;
  if not Application.TryVideoChange then
    MessageOK(Application.MainWindow, Format(
      'Cannot change screen resolution to %d x %d', [
      NewWidth,
      NewHeight
    ]));
end;

procedure TViewMain.ClickResolution1024x768(Sender: TObject);
begin
  ChangeResolution(1024, 768);
end;

procedure TViewMain.ClickResolution1280x720(Sender: TObject);
begin
  ChangeResolution(1280, 720);
end;

procedure TViewMain.ClickResolutionCustom(Sender: TObject);
begin
  ChangeResolution(EditResolutionWidth.Value, EditResolutionHeight.Value);
end;

procedure TViewMain.ClickResolutionDefault(Sender: TObject);
begin
  Application.VideoReset;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;
end;

end.
