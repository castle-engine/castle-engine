{
  Copyright 2023-2024 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, CastleScene,
  CastleUIControls, CastleControls, CastleKeysMouse, X3DNodes,
  CastleRenderOptions;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    SceneLevel1: TCastleScene;
  strict private
    Projector: TSpotLightNode;
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

  { Limit the texture display to the place where its coordinates are in 0..1 range.

    Explanation why this is needed:
    The texture rendered by projecting in AddProjection is by default
    not limited to its 0..1 texture coordinates.
    And ProjTexture.RepeatS/T cannot change this (ProjTexture.RepeatS/T
    only changes *what* is visible outside 0..1 range, not *if* it is visible).

    So we setup a small shader that will reject applying texture when
    the texture coordinate is outside 0..1 range. }
  function CreateEffectToLimitTexture: TEffectNode;
  var
    EffectPart: TEffectPartNode;
  begin
    Result := TEffectNode.Create;
    Result.Language := slGLSL;

    { Add TEffectPartNode which actually contains shader code.
      You can load shader code from file,
      you could also set it explicitly from string using EffectPart.Contents := '...'; }
    EffectPart := TEffectPartNode.Create;
    EffectPart.ShaderType := stFragment;
    EffectPart.SetUrl(['castle-data:/shaders/limit_texture.fs']);
    Result.SetParts([EffectPart]);
  end;

  { Add texture projected by Projector to given mesh in given scene. }
  procedure AddProjection(const Projector: TSpotLightNode;
    const ProjectedTextureUrl: String;
    const Scene: TCastleScene; const BlenderMeshName: String);
  var
    Shape: TShapeNode;
    MeshGroup: TGroupNode;
    MeshChild: TX3DNode;
    IndexedTriangleSet: TIndexedTriangleSetNode;
    ProjTexCooord: TProjectedTextureCoordinateNode;
    ProjTexture: TImageTextureNode;
    Material: TPhysicalMaterialNode;
  begin
    MeshGroup := Scene.Node(TGroupNode, BlenderMeshName) as TGroupNode;
    for MeshChild in MeshGroup.FdChildren do
      if MeshChild is TShapeNode then
      begin
        Shape := TShapeNode(MeshChild);

        ProjTexCooord := TProjectedTextureCoordinateNode.Create;
        ProjTexCooord.FdProjector.Value := Projector;

        IndexedTriangleSet := Shape.Geometry as TIndexedTriangleSetNode;
        IndexedTriangleSet.TexCoord := ProjTexCooord;

        ProjTexture := TImageTextureNode.Create;
        ProjTexture.SetUrl([ProjectedTextureUrl]);
        ProjTexture.RepeatS := false;
        ProjTexture.RepeatT := false;
        ProjTexture.SetEffects([CreateEffectToLimitTexture]);

        { In this demo we just assume that Appearance and Material nodes
          are already present in the model (so they have been created in Blender)
          and that material is physical. }
        Material := Shape.Appearance.Material as TPhysicalMaterialNode;
        Material.BaseTexture := ProjTexture;
      end;
  end;

begin
  inherited;

  Projector := TSpotLightNode.Create;

  { Set initial projector location/direction.

    Note: If you transform SceneLevel1, remember that Projector location/direction
    is relative to SceneLevel1. Conceptually, projector defined like this
    "moves along" with SceneLevel1.
    If you want to counter this, you have to set Projector location/direction
    based on some world-space value, converted to SceneLevel1 coordinates,
    like

      Projector.Location := SceneLevel1.WorldToLocal(Vector3(10, 10, 10));
      Projector.Direction := SceneLevel1.WorldToLocalDirection(Vector3(-10, -10, -10));

    In this demo, we assume SceneLevel1 is just not transformed,
    for simplicity.
  }
  Projector.Location := Vector3(10, 10, 10);
  Projector.Direction := Vector3(-10, -10, -10);

  // Allows to see the need for AddEffectToLimitTexture
  // Projector.Location := Vector3(0.5, 5, 0.5);
  // Projector.Direction := Vector3(0, -1, 0);

  { See ../data/projective_texturing_simple.x3dv for explanation of
    various TSpotLightNode fields. }
  Projector.CutOffAngle := 0.15;
  Projector.IsOn := false;

  AddProjection(Projector, 'castle-data:/test_texture.png',
    SceneLevel1, 'mesh_receiving_projection');
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
