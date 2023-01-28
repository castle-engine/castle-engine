{
  Copyright 2022-2022 Michalis Kamburelis.

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
  private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonAnimatedCursor: TCastleButton;
    ButtonImageCursor: TCastleButton;
    ButtonDefaultCursor: TCastleButton;

    procedure ClickAnimatedCursor(Sender: TObject);
    procedure ClickImageCursor(Sender: TObject);
    procedure ClickDefaultCursor(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  GameCustomCursor;

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
  ButtonAnimatedCursor := DesignedComponent('ButtonAnimatedCursor') as TCastleButton;
  ButtonImageCursor := DesignedComponent('ButtonImageCursor') as TCastleButton;
  ButtonDefaultCursor := DesignedComponent('ButtonDefaultCursor') as TCastleButton;

  ButtonAnimatedCursor.OnClick := {$ifdef FPC}@{$endif} ClickAnimatedCursor;
  ButtonImageCursor.OnClick := {$ifdef FPC}@{$endif} ClickImageCursor;
  ButtonDefaultCursor.OnClick := {$ifdef FPC}@{$endif} ClickDefaultCursor;

  { by default, initialize csAnimated, and hide system cursor. }
  ClickAnimatedCursor(nil);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickAnimatedCursor(Sender: TObject);
begin
  Cursor := mcForceNone;
  CustomCursor.Exists := true;
  CustomCursor.Style := csAnimated;
end;

procedure TViewMain.ClickImageCursor(Sender: TObject);
begin
  Cursor := mcForceNone;
  CustomCursor.Exists := true;
  CustomCursor.Style := csImage;
end;

procedure TViewMain.ClickDefaultCursor(Sender: TObject);
begin
  Cursor := mcDefault;
  CustomCursor.Exists := false;
end;

end.
