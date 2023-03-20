{
  Copyright 2020-2023 Michalis Kamburelis.

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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTimeUtils,
  X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainScene: TCastleScene;
  private
    const
      XCount = 15;
      YCount = 15;
    var
      Time: TFloatTime;
      Transform: array [0 .. XCount - 1, 0 .. YCount - 1] of TTransformNode;
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

  function BuildSceneNode: TX3DRootNode;
  var
    Shape: TShapeNode;
    Mat: TMaterialNode;
    I, J: Integer;
  begin
    Result := TX3DRootNode.Create;

    Mat := TMaterialNode.Create;
    Mat.DiffuseColor := Vector3(1, 1, 0);

    Shape := TShapeNode.Create;
    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Material := Mat;
    Shape.Geometry := TBoxNode.Create;

    for I := 0 to XCount - 1 do
      for J := 0 to YCount - 1 do
      begin
        Transform[I, J] := TTransformNode.Create;
        Transform[I, J].Translation := Vector3(I * 2, J * 2, 0);
        Transform[I, J].AddChildren(Shape);

        Result.AddChildren(Transform[I, J]);
      end;
  end;

begin
  inherited;
  MainScene.Load(BuildSceneNode, { MainScene owns new node } true);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  I, J: Integer;
  T: TVector3;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  Time := Time + Container.Fps.SecondsPassed;

  for I := 0 to XCount - 1 do
    for J := 0 to YCount - 1 do
    begin
      T := Transform[I, J].Translation;
      T.Z := 2 *
        Sin(I / 2 + Time) *
        Cos(J / 2 + Time);
      Transform[I, J].Translation := T;
    end;
end;

end.
