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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainScene: TCastleScene;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  X3DNodes, X3DLoad;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  function BuildNode: TX3DRootNode;
  var
    ModelBoxes, ModelRaptor: TX3DRootNode;
    TransformBoxes: TTransformNode;
    TransformRaptor: array [0..2] of TTransformNode;
    I: Integer;
  begin
    { Create an X3D graph like this:

      Result (TX3DRootNode)
      |- TransformBoxes (TTransformNode)
         |- ModelBoxes (TX3DRootNode, loaded from data/boxes.x3dv)

      |- TransformRaptor[0] (TTransformNode)
         |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

      |- TransformRaptor[1] (TTransformNode)
         |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

      |- TransformRaptor[2] (TTransformNode)
         |- ModelRaptor (TX3DRootNode, loaded from data/raptor_1.x3d)

      Note that the same TCastleScene instance "ModelRaptor" is added
      multiple times to the X3D nodes graph (just with different transformations).
      This is fully supported and valid.
    }

    Result := TX3DRootNode.Create;

    { add ModelBoxes and TransformBoxes }

    ModelBoxes := LoadNode('castle-data:/boxes.x3dv');

    TransformBoxes := TTransformNode.Create;
    TransformBoxes.Translation := Vector3(-5, 0, 0);
    TransformBoxes.AddChildren(ModelBoxes);

    Result.AddChildren(TransformBoxes);

    { add ModelRaptor and TransformRaptor[0..2] }

    ModelRaptor := LoadNode('castle-data:/raptor_1.x3d');

    for I := 0 to 2 do
    begin
      TransformRaptor[I] := TTransformNode.Create;
      TransformRaptor[I].Translation := Vector3(8, (I -1)  * 5, 0);
      TransformRaptor[I].AddChildren(ModelRaptor);

      Result.AddChildren(TransformRaptor[I]);
    end;
  end;

begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainScene := DesignedComponent('MainScene') as TCastleScene;

  MainScene.Load(BuildNode, true { Scene owns given node });
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
