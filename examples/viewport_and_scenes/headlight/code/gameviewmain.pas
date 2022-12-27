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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
    LabelFps: TCastleLabel;
    ButtonHeadlightOff,
      ButtonHeadlightDirectional,
      ButtonHeadlightSpotSmooth,
      ButtonHeadlightSpotSharp,
      ButtonHeadlightPoint: TCastleButton;
    HeadlightDirectional,
      HeadlightSpotSmooth,
      HeadlightSpotSharp,
      HeadlightPoint: TCastleAbstractLight;
    procedure ClickHeadlightOff(Sender: TObject);
    procedure ClickHeadlightDirectional(Sender: TObject);
    procedure ClickHeadlightSpotSmooth(Sender: TObject);
    procedure ClickHeadlightSpotSharp(Sender: TObject);
    procedure ClickHeadlightPoint(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
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

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  ButtonHeadlightOff := DesignedComponent('ButtonHeadlightOff') as TCastleButton;
  ButtonHeadlightDirectional := DesignedComponent('ButtonHeadlightDirectional') as TCastleButton;
  ButtonHeadlightSpotSmooth := DesignedComponent('ButtonHeadlightSpotSmooth') as TCastleButton;
  ButtonHeadlightSpotSharp := DesignedComponent('ButtonHeadlightSpotSharp') as TCastleButton;
  ButtonHeadlightPoint := DesignedComponent('ButtonHeadlightPoint') as TCastleButton;
  HeadlightDirectional := DesignedComponent('HeadlightDirectional') as TCastleAbstractLight;
  HeadlightSpotSmooth := DesignedComponent('HeadlightSpotSmooth') as TCastleAbstractLight;
  HeadlightSpotSharp := DesignedComponent('HeadlightSpotSharp') as TCastleAbstractLight;
  HeadlightPoint := DesignedComponent('HeadlightPoint') as TCastleAbstractLight;

  ButtonHeadlightOff.OnClick := {$ifdef FPC}@{$endif} ClickHeadlightOff;
  ButtonHeadlightDirectional.OnClick := {$ifdef FPC}@{$endif} ClickHeadlightDirectional;
  ButtonHeadlightSpotSmooth.OnClick := {$ifdef FPC}@{$endif} ClickHeadlightSpotSmooth;
  ButtonHeadlightSpotSharp.OnClick := {$ifdef FPC}@{$endif} ClickHeadlightSpotSharp;
  ButtonHeadlightPoint.OnClick := {$ifdef FPC}@{$endif} ClickHeadlightPoint;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ClickHeadlightOff(Sender: TObject);
begin
  ButtonHeadlightOff.Pressed := true;
  ButtonHeadlightDirectional.Pressed := false;
  ButtonHeadlightSpotSmooth.Pressed := false;
  ButtonHeadlightSpotSharp.Pressed := false;
  ButtonHeadlightPoint.Pressed := false;

  HeadlightDirectional.Exists := false;
  HeadlightSpotSmooth.Exists := false;
  HeadlightSpotSharp.Exists := false;
  HeadlightPoint.Exists := false;
end;

procedure TViewMain.ClickHeadlightDirectional(Sender: TObject);
begin
  ButtonHeadlightOff.Pressed := false;
  ButtonHeadlightDirectional.Pressed := true;
  ButtonHeadlightSpotSmooth.Pressed := false;
  ButtonHeadlightSpotSharp.Pressed := false;
  ButtonHeadlightPoint.Pressed := false;

  HeadlightDirectional.Exists := true;
  HeadlightSpotSmooth.Exists := false;
  HeadlightSpotSharp.Exists := false;
  HeadlightPoint.Exists := false;
end;

procedure TViewMain.ClickHeadlightSpotSmooth(Sender: TObject);
begin
  ButtonHeadlightOff.Pressed := false;
  ButtonHeadlightDirectional.Pressed := false;
  ButtonHeadlightSpotSmooth.Pressed := true;
  ButtonHeadlightSpotSharp.Pressed := false;
  ButtonHeadlightPoint.Pressed := false;

  HeadlightDirectional.Exists := false;
  HeadlightSpotSmooth.Exists := true;
  HeadlightSpotSharp.Exists := false;
  HeadlightPoint.Exists := false;
end;

procedure TViewMain.ClickHeadlightSpotSharp(Sender: TObject);
begin
  ButtonHeadlightOff.Pressed := false;
  ButtonHeadlightDirectional.Pressed := false;
  ButtonHeadlightSpotSmooth.Pressed := false;
  ButtonHeadlightSpotSharp.Pressed := true;
  ButtonHeadlightPoint.Pressed := false;

  HeadlightDirectional.Exists := false;
  HeadlightSpotSmooth.Exists := false;
  HeadlightSpotSharp.Exists := true;
  HeadlightPoint.Exists := false;
end;

procedure TViewMain.ClickHeadlightPoint(Sender: TObject);
begin
  ButtonHeadlightOff.Pressed := false;
  ButtonHeadlightDirectional.Pressed := false;
  ButtonHeadlightSpotSmooth.Pressed := false;
  ButtonHeadlightSpotSharp.Pressed := false;
  ButtonHeadlightPoint.Pressed := true;

  HeadlightDirectional.Exists := false;
  HeadlightSpotSmooth.Exists := false;
  HeadlightSpotSharp.Exists := false;
  HeadlightPoint.Exists := true;
end;

end.
