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
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTimeUtils,
  X3DNodes;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    const
      XCount = 15;
      YCount = 15;
    var
      { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
      LabelFps: TCastleLabel;
      MainScene: TCastleScene;

      Time: TFloatTime;
      Transform: array [0 .. XCount - 1, 0 .. YCount - 1] of TTransformNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

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

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainScene := DesignedComponent('MainScene') as TCastleScene;

  { We use a lot of boxes, so make their rendering fastest. }
  DefaultTriangulationDivisions := 0;

  MainScene.Load(BuildSceneNode, { MainScene owns new node } true);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  I, J: Integer;
  T: TVector3;
begin
  inherited;
  { This virtual method is executed every frame.}
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
