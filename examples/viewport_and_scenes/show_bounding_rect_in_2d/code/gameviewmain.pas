{
  Copyright 2018-2023 Michalis Kamburelis.

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
  CastleVectors, CastleComponentSerialize, X3DNodes,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    DragonScene: TCastleScene;
  private
    SceneBoundingRect: TCastleScene;
    RectCoords: TCoordinateNode;
    { Update contents of RectCoords. }
    procedure UpdateRectangleCoordinates;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleColors, CastleRectangles;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;

  { Build node showing a wireframe rectangle.

    There are a lot of things you can customize here,
    adjusting the look of the resulting rectangle.

    Note: instead of TLineSetNode, one could also use TRectangle2DNode
    to show a rectangle in 2D. You would need to set TRectangle2DNode.Size,
    and place it within TTransformNode to position is correctly.
    Below we show a little different, but also more flexible, approach
    that uses TLineSetNode and TCoordinateNode. }
  function CreateRectangleNode: TX3DRootNode;
  var
    Shape: TShapeNode;
    LineSet: TLineSetNode;
    LineProperties: TLinePropertiesNode;
    Material: TMaterialNode;
    Appearance: TAppearanceNode;
  begin
    RectCoords := TCoordinateNode.Create;
    UpdateRectangleCoordinates;

    LineSet := TLineSetNode.CreateWithShape(Shape);
    LineSet.Coord := RectCoords;
    LineSet.SetVertexCount([RectCoords.FdPoint.Count]);

    Material := TMaterialNode.Create;
    Material.EmissiveColor := YellowRGB;
    //Material.Transparency := 0.8;

    LineProperties := TLinePropertiesNode.Create;
    LineProperties.LineWidthScaleFactor := 2;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;
    Appearance.LineProperties := LineProperties;

    Shape.Appearance := Appearance;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
  end;

begin
  inherited;

  SceneBoundingRect := TCastleScene.Create(FreeAtStop);
  SceneBoundingRect.Load(CreateRectangleNode, true);

  { Place SceneBoundingRect as a child of DragonScene,
    this way transformation of DragonScene is automatically accounted for,
    as in UpdateRectangleCoordinates we just use DragonScene.LocalBoundingBox,
    not DragonScene.WorldBoundingBox.

    OTOH it means we need to toggle SceneBoundingRect.Exists in
    UpdateRectangleCoordinates, to make sure DragonScene.LocalBoundingBox
    doesn't contain the visualization itself.

    An alternative approach, placing SceneBoundingRect directly
    in Viewport.Items, would also work, but then we'd have to look at
    DragonScene.WorldBoundingBox. }
  DragonScene.Add(SceneBoundingRect);
end;

procedure TViewMain.UpdateRectangleCoordinates;
var
  BoundingRect: TFloatRectangle;
  OldSceneBoundingRectExists: Boolean;
begin
  OldSceneBoundingRectExists := SceneBoundingRect.Exists;
  SceneBoundingRect.Exists := false;

  BoundingRect := DragonScene.LocalBoundingBox.RectangleXY;

  SceneBoundingRect.Exists := OldSceneBoundingRectExists;

  RectCoords.SetPoint([
    Vector3(BoundingRect.Left , BoundingRect.Bottom, 0),
    Vector3(BoundingRect.Right, BoundingRect.Bottom, 0),
    Vector3(BoundingRect.Right, BoundingRect.Top   , 0),
    Vector3(BoundingRect.Left , BoundingRect.Top   , 0),
    Vector3(BoundingRect.Left , BoundingRect.Bottom, 0)
  ]);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Keep updating RectCoords, as bounding rectangle changes as the DragonScene animates }
  UpdateRectangleCoordinates;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyB) then
  begin
    SceneBoundingRect.Exists := not SceneBoundingRect.Exists;
    Exit(true); // key was handled
  end;
end;

end.
