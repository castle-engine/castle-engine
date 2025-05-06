{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Edit a chosen project. }
unit GameViewProject;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleInternalInspector,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Edit a chosen project. }
  TViewProject = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonBack: TCastleButton;
  private
    FInspector: TCastleInspector;
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewProject: TViewProject;

implementation

uses SysUtils,
  GameViewChooseProject;

{ TViewProject ----------------------------------------------------------------- }

constructor TViewProject.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewproject.castle-user-interface';
end;

procedure TViewProject.Start;
begin
  inherited;
  ButtonBack.OnClick := {$ifdef FPC}@{$endif} ClickBack;

  // a bit simplified inspector, to have room for rest
  TCastleInspector.PersistentState.RectLogExists := false;
  TCastleInspector.PersistentState.RectProfilerExists := false;
  FInspector := TCastleInspector.Create(FreeAtStop);
  InsertFront(FInspector);
end;

procedure TViewProject.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewProject.ClickBack(Sender: TObject);
begin
  Container.View := ViewChooseProject;
end;

end.
