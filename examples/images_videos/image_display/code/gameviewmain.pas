{
  Copyright 2021-2023 Michalis Kamburelis.

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
    ButtonQuit, ButtonGoSpeedTest: TCastleButton;
  private
    procedure ClickQuit(Sender: TObject);
    procedure ClickGoSpeedTest(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleApplicationProperties, CastleWindow,
  GameViewSpeedTest;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  // on some platforms, showing "Quit" button in applications is unusual
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;

  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
  ButtonGoSpeedTest.OnClick := {$ifdef FPC}@{$endif} ClickGoSpeedTest;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TViewMain.ClickGoSpeedTest(Sender: TObject);
begin
  Container.View := ViewSpeedTest;
end;

end.
