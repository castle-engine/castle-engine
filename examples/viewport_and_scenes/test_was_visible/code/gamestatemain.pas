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
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, X3DNodes;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelWasVisible, LabelFps: TCastleLabel;
    SceneTestBlocks: TCastleScene;

    ShapeRed, ShapeGreen, ShapeBlue, ShapeWhite: TShapeNode;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleUtils;

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
  SceneTestBlocks := DesignedComponent('SceneTestBlocks') as TCastleScene;
  LabelWasVisible := DesignedComponent('LabelWasVisible') as TCastleLabel;

  { We get X3D Shape nodes knowing how the Blender->glTF exporter works,
    and then how glTF is converted into X3D nodes in CGE.

    - Each Blender object is X3D group (glTF transform node),
    - with Blender mesh inside as another X3D group (glTF mesh),
    - that contains a list of X3D shapes for each different material
      (one X3D shape is one glTF primitive).

    As all our Blender meshes have just 1 material, we just access the X3D shape
    node by accessing the 1st child or TGroupNode.

    To see the X3D nodes, just open ../data/test_blocks.gltf in view3dscene and "Save As" X3DV
    file, and then read it. }

  ShapeRed := (SceneTestBlocks.Node('RedMesh') as TGroupNode).FdChildren[0] as TShapeNode;
  ShapeGreen := (SceneTestBlocks.Node('GreenMesh') as TGroupNode).FdChildren[0] as TShapeNode;
  ShapeBlue := (SceneTestBlocks.Node('BlueMesh') as TGroupNode).FdChildren[0] as TShapeNode;
  ShapeWhite := (SceneTestBlocks.Node('WhiteMesh') as TGroupNode).FdChildren[0] as TShapeNode;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  LabelWasVisible.Caption := Format(
    'SceneTestBlocks.WasVisible = %s' + NL +
    'ShapeRed.WasVisible = %s' + NL +
    'ShapeGreen.WasVisible = %s' + NL +
    'ShapeBlue.WasVisible = %s' + NL +
    'ShapeWhite.WasVisible = %s', [
    BoolToStr(SceneTestBlocks.WasVisible,true),
    BoolToStr(ShapeRed.WasVisible,true),
    BoolToStr(ShapeGreen.WasVisible,true),
    BoolToStr(ShapeBlue.WasVisible,true),
    BoolToStr(ShapeWhite.WasVisible,true)
  ]);
end;

end.
