{
  Copyright 2019-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleTransform component that draws a tiling (repeatable) texture. }
unit GameTilingBackground;

interface

uses Classes,
  CastleVectors, CastleTransform, CastleScene, X3DNodes, CastleViewport;

type
  { Draw a tiling (repeatable) texture.

    This is very efficient -- always draws only 1 quad,
    uses texture coordinats to repeat the texturein both directions as needed. }
  TTilingBackground = class(TCastleScene)
  strict private
    Coordinate: TCoordinateNode;
    TextureCoordinate: TTextureCoordinateNode;
    Texture: TImageTextureNode;
  public
    { Image position visible at the viewport's left-bottom corner.
      Change this to effectively move the image.
      Always call UpdateCoordinates after changing this. }
    ImageOrigin: TVector2Double;

    constructor Create(AOwner: TComponent); override;

    { Call this always when ImageOrigin or viewport size changes.
      Adjusts the coordinates (position of the quad in viewport)
      and texture coordinates (how the image is mapped onto a surface). }
    procedure UpdateCoordinates(const ParentViewport: TCastleViewport);
  end;

implementation

uses CastleRectangles;

constructor TTilingBackground.Create(AOwner: TComponent);

  { Create nodes graph with a textured rectangle. }
  function BuildRootNode: TX3DRootNode;
  var
    Shape: TShapeNode;
    Geometry: TIndexedFaceSetNode;
  begin
    { Create ImageTexture node (represents the texture from file) }
    Texture := TImageTextureNode.Create;
    Texture.SetUrl(['castle-data:/space/goldSpace.png']);
    // Set RepeatS, RepeatT to true, to repeat the texture.
    Texture.RepeatS := true;
    Texture.RepeatT := true;

    { Create Coordinate node (position of quad in 3D) }
    Coordinate := TCoordinateNode.Create;
    Coordinate.SetPoint([
      { Actual value of these vectors doesn't mater, they will be updated
        in each UpdateCoordinates.
        It is only important to provide 4 points here, to avoid warnings
        at creation: SetCoordIndex below will refer to these points. }
      Vector3(0  ,   0, 0),
      Vector3(100,   0, 0),
      Vector3(100, 100, 0),
      Vector3(0  , 100, 0)
    ]);

    { Create TextureCoordinate node (how the image is mapped onto a surface) }
    TextureCoordinate := TTextureCoordinateNode.Create;
    TextureCoordinate.SetPoint([
      { Actual value of these vectors doesn't mater, they will be updated
        in each UpdateCoordinates.
        It is only important to provide 4 points here, to avoid warnings
        at creation: SetCoordIndex below will refer to these points. }
      Vector2(0, 0),
      Vector2(10, 0),
      Vector2(10, 10),
      Vector2(0, 10)
    ]);

    { Create Shape and IndexedFaceSet node (mesh with coordinates, texture coordinates) }
    Geometry := TIndexedFaceSetNode.Create;
    Geometry.Coord := Coordinate;
    Geometry.TexCoord := TextureCoordinate;
    Geometry.SetCoordIndex([0, 1, 2, 3]);

    Shape := TShapeNode.Create;

    { Create Appearance (refers to a texture, connects the Texture to Shape) }
    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Texture := Texture;

    Shape.Geometry := Geometry;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
  end;

begin
  inherited;
  Load(BuildRootNode, true);
end;

procedure TTilingBackground.UpdateCoordinates(const ParentViewport: TCastleViewport);
var
  TextureSizeX, TextureSizeY: Single;
  VisibleRect: TFloatRectangle;
  ImageOriginFrac: TVector2;
begin
  VisibleRect := ParentViewport.Camera.Orthographic.EffectiveRect;
  { update Coordinate to make the shape fill entire viewport exactly }
  Coordinate.SetPoint([
    Vector3(VisibleRect.Left , VisibleRect.Bottom, 0),
    Vector3(VisibleRect.Right, VisibleRect.Bottom, 0),
    Vector3(VisibleRect.Right, VisibleRect.Top   , 0),
    Vector3(VisibleRect.Left , VisibleRect.Top   , 0)
  ]);

  { Texture coordinates are expressed in the range 0..1 }
  TextureSizeX := VisibleRect.Width  / Texture.TextureImage.Width;
  TextureSizeY := VisibleRect.Height / Texture.TextureImage.Height;

  { Pass to rendering only the fractional part of ImageOrigin.
    Only the fractional part is relevant,
    and this way we avoid passing huge floating point values (after
    a few minutes of gameplay) to GPU and losing precision, resulting
    in background "stuttering" visually. }
  ImageOriginFrac.X := Frac(ImageOrigin.X);
  ImageOriginFrac.Y := Frac(ImageOrigin.Y);

  { update TextureCoordinate to adjust the amount of "repeat" based on control size
    on the screen (VisibleRect.Width/Height), and apply ImageOrigin. }
  TextureCoordinate.SetPoint([
    ImageOriginFrac + Vector2(           0,            0),
    ImageOriginFrac + Vector2(TextureSizeX,            0),
    ImageOriginFrac + Vector2(TextureSizeX, TextureSizeY),
    ImageOriginFrac + Vector2(           0, TextureSizeY)
  ]);
end;

end.