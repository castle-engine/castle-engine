{
  Copyright 2019-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Example how to draw a tiling background.
  See README.md for details.
}

uses SysUtils, Classes,
  CastleVectors, CastleWindow, CastleRectangles,
  CastleUtils, CastleUIControls, CastleStringUtils,
  CastleKeysMouse, CastleViewport, CastleScene, X3DNodes;

var
  Window: TCastleWindow;

type
  { User interface component that draws a tiling texture. }
  TTilingBackground = class(TCastleViewport)
  strict private
    Coordinate: TCoordinateNode;
    TextureCoordinate: TTextureCoordinateNode;
    Texture: TImageTextureNode;
  public
    { Image position visible at the control's left-bottom corner }
    ImageOrigin: TVector2;
    constructor Create(AOwner: TComponent); override;
    procedure Resize; override;
    { Call this always when ImageOrigin changes.
      Automatically called when control size changes too. }
    procedure UpdateCoordinates;
  end;

constructor TTilingBackground.Create(AOwner: TComponent);

  { Create X3D nodes graph with a textured rectangle.
    Set RepeatS, RepeatT to true, to repeat the texture. }
  function BuildRootNode: TX3DRootNode;
  var
    Shape: TShapeNode;
    Geometry: TIndexedFaceSetNode;
  begin
    { Create ImageTexture node (represents the texture from file) }
    Texture := TImageTextureNode.Create;
    Texture.SetUrl(['castle-data:/test_texture.png']);
    Texture.RepeatS := true;
    Texture.RepeatT := true;

    { Create Coordinate node (position of quad in 3D) }
    Coordinate := TCoordinateNode.Create;
    // these will be updated in each UpdateCoordinates
    Coordinate.SetPoint([
      Vector3(0  ,   0, 0),
      Vector3(100,   0, 0),
      Vector3(100, 100, 0),
      Vector3(0  , 100, 0)
    ]);

    { Create TextureCoordinate node (how the image is mapped onto a surface) }
    TextureCoordinate := TTextureCoordinateNode.Create;
    // these will be updated in each UpdateCoordinates
    TextureCoordinate.SetPoint([
      Vector2(0, 0),
      Vector2(10, 0),
      Vector2(10, 10),
      Vector2(0, 10)
    ]);

    { Create Shape and IndexedFaceSet node (mesh with coordinates, texture coordinates) }
    Geometry := TIndexedFaceSetNode.CreateWithShape(Shape);
    Geometry.Coord := Coordinate;
    Geometry.TexCoord := TextureCoordinate;
    Geometry.SetCoordIndex([0, 1, 2, 3]);

    { Create Appearance (refers to a texture, connects the Texture to Shape) }
    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Texture := Texture;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
  end;

var
  Scene: TCastleScene;
begin
  inherited;
  Scene := TCastleScene.Create(Self);
  Scene.Load(BuildRootNode, true);
  Items.Add(Scene);

  // set 2D orthographic view
  Setup2D;
end;

procedure TTilingBackground.Resize;
begin
  inherited;
  UpdateCoordinates;
end;

procedure TTilingBackground.UpdateCoordinates;
var
  TextureSizeX, TextureSizeY: Single;
begin
  { update Coordinate to make the shape fill entire viewport exactly }
  Coordinate.SetPoint([
    Vector3(             0,               0, 0),
    Vector3(EffectiveWidth,               0, 0),
    Vector3(EffectiveWidth, EffectiveHeight, 0),
    Vector3(             0, EffectiveHeight, 0)
  ]);

  { texture coordinates are expressed in the range 0..1 }
  TextureSizeX := EffectiveWidth  / Texture.TextureImage.Width;
  TextureSizeY := EffectiveHeight / Texture.TextureImage.Height;

  { update TextureCoordinate to adjust the amount of "repeat" based on control size
    on the screen (EffectiveWidth/Height), and apply ImageOrigin. }
  TextureCoordinate.SetPoint([
    ImageOrigin + Vector2(           0,            0),
    ImageOrigin + Vector2(TextureSizeX,            0),
    ImageOrigin + Vector2(TextureSizeX, TextureSizeY),
    ImageOrigin + Vector2(           0, TextureSizeY)
  ]);
end;

var
  { Single instance of TTilingBackground in this application. }
  TilingBackground: TTilingBackground;

procedure Update(Container: TCastleContainer);

  procedure Move(const X, Y: Single);
  const
    MoveSpeed = 1;
  begin
    TilingBackground.ImageOrigin := TilingBackground.ImageOrigin +
      MoveSpeed * Container.Fps.SecondsPassed * Vector2(X, Y);
    TilingBackground.UpdateCoordinates;
  end;

begin
  if Container.Pressed[keyArrowUp]    then Move(0,  1);
  if Container.Pressed[keyArrowDown]  then Move(0, -1);
  if Container.Pressed[keyArrowRight] then Move( 1, 0);
  if Container.Pressed[keyArrowLeft]  then Move(-1, 0);
end;

begin
  Window := TCastleWindow.Create(Application);
  Window.Open;

  TilingBackground := TTilingBackground.Create(Application);
  TilingBackground.FullSize := true;
  // you could also set FullSize := false and explicitly set control size and anchors
  // TilingBackground.FullSize := false;
  // TilingBackground.Anchor(vpMiddle);
  // TilingBackground.Anchor(hpMiddle);
  // TilingBackground.Width := 800;
  // TilingBackground.Height := 800;
  Window.Controls.InsertFront(TilingBackground);

  Window.OnUpdate := @Update;
  Application.Run;
end.
