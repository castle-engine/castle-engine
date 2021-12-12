{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonQuit, ButtonGoSpeedTest: TCastleButton;

    procedure ClickQuit(Sender: TObject);
    procedure ClickGoSpeedTest(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleApplicationProperties, CastleWindow,
  GameStateSpeedTest;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ButtonQuit := DesignedComponent('ButtonQuit') as TCastleButton;
  ButtonGoSpeedTest := DesignedComponent('ButtonGoSpeedTest') as TCastleButton;

  // on some platforms, showing "Quit" button in applications is unusual
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;

  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
  ButtonGoSpeedTest.OnClick := {$ifdef FPC}@{$endif} ClickGoSpeedTest;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TStateMain.ClickGoSpeedTest(Sender: TObject);
begin
  TUIState.Current := StateSpeedTest;
end;

end.
