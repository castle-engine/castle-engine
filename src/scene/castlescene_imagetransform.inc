{%MainUnit castlescene.pas}
{
  Copyright 2022-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Image (that you can place within TCastleViewport) with configurable size and repeat.
    Positioned in XY plane by defalut, so most suitable for 2D games. }
  TCastleImageTransform = class(TCastleTransform)
  strict private
    { scene and X3D nodes }
    FScene: TCastleScene;
    FPreciseCollisions: Boolean;
    FRootNode: TX3DRootNode;
    FAppearanceNode: TAppearanceNode;
    FShapeNode: TShapeNode;
    FMaterialNode: TUnlitMaterialNode;
    FTextureNode: TImageTextureNode;
    FTexPropertiesNode: TTexturePropertiesNode;
    FCoordinateNode: TCoordinateNode;
    FTexCoordNode: TTextureCoordinateNode;
    FIndexedTriangleSetNode: TIndexedTriangleSetNode;

    { private fields reflecting public properties }
    FUrl: String;
    FPivot, FSize, FRepeatImage, FShift: TVector2;
    FColor: TCastleColor;
    FSmoothScaling: Boolean;
    FAlphaChannel: TAutoAlphaChannel;
    FMipmaps: Boolean;

    FUrlMonitoring: TUrlMonitoring;
    FImageWidth, FImageHeight: Cardinal;

    { Update X3D TCoordinateNode, TTextureCoordinateNode. }
    procedure UpdateCoordinateNodes;
    procedure SetPivot(const Value: TVector2);
    procedure SetSize(const Value: TVector2);
    procedure SetRepeatImage(const Value: TVector2);
    procedure SetShift(const Value: TVector2);
    procedure SetColor(const Value: TCastleColor);
    procedure SetSmoothScaling(const Value: Boolean);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    procedure SetUrl(const Value: String);
    procedure SetMipmaps(const Value: Boolean);
    procedure SetPreciseCollisions(const Value: Boolean);
  protected
    function InternalBuildNodeInside: TObject; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure BeforeDestruction; override;

    { How is the image placed within its own coordinate system.
      Default value (0.5, 0.5) means that the middle of the image is at (0,0,0).
      Value (0, 0) means that the left-bottom corner of the image is at (0,0,0). }
    property Pivot: TVector2 read FPivot write SetPivot;

    { Optionally resize the image.
      This is equivalent o using TCastleTransform.Scale to scale the image,
      except you can specify a value in terms of image size (in image pixels).

      @unorderedList(
        @item(When both X and Y are zero, the image is not resized.)

        @item(When only X is non-zero, the image is resized to given width,
          preserving the image aspect ratio (proportions).)

        @item(When only Y is non-zero, the image is resized to given height,
          preserving the image aspect ratio (proportions).)

        @item(When both X and Y are non-zero, the image is resized to given width and height,
          ignoring the image aspect ratio.)
      )

      The size determines the size @italic(assuming that @link(RepeatImage) is (1,1)).
      When @link(RepeatImage) is different -- the size is actually multiplied by the @link(RepeatImage).
      This makes it easy to create a continuous block of images by just increasing @link(RepeatImage).
    }
    property Size: TVector2 read FSize write SetSize;

    { How many times to repeat the image, along X and Y. By default this is (1,1). }
    property RepeatImage: TVector2 read FRepeatImage write SetRepeatImage;

    { How is the image shifted. This just shifts the texture coordinates, not moving the geometry.
      By default this is (0,0). }
    property Shift: TVector2 read FShift write SetShift;

    { Color to multiply by image. By default, opaque white. }
    property Color: TCastleColor read FColor write SetColor;

    { Load again the image from current URL.
      This makes sense to be used when underlying file on disk
      changed, and you want to reload it.

      TODO: In case image is shared with other TCastleImageTransform instances,
      or TCastleScene (refering to the same image using TImageTextureNode),
      for now this method will not load new image from disk.
      That's because the previous image contents will "survive" in the cache. }
    procedure ReloadUrl;

    { Loaded image width.

      This is not affected by any resizing done by @link(Size)
      or scaling done by @link(TCastleTransform.Scale).
      This property reflects the underlying image size.

      Zero if no image is loaded. }
    property ImageWidth: Cardinal read FImageWidth;

    { Loaded image height.

      This is not affected by any resizing done by @link(Size)
      or scaling done by @link(TCastleTransform.Scale).
      This property reflects the underlying image size.

      Zero if no image is loaded. }
    property ImageHeight: Cardinal read FImageHeight;

    { Load image contents from a ready TEncodedImage instance.

      If TakeImageOwnership = @true then we take given Image reference,
      and the lifetime of this Image object will be further managed by this
      TImageTextureNode class (so you should no longer free it yourself).
      If TakeImageOwnership = @false then we only copy Image contents
      (this is slower, but it means that ownership and freeing of given
      Image instance remains on your side).

      After using this, @link(Url) is always set to an empty string.
      Using @link(ReloadUrl) will lose image contents.
      If developer changes the @link(Url), the image will change to reflect
      new URL contents. }
    procedure LoadFromImage(const Image: TEncodedImage;
      const TakeImageOwnership: Boolean);
  published
    { How is the image scaled.
      Determines scaling done both by @link(Size) and TCastleTransform.Scale. }
    property SmoothScaling: Boolean read FSmoothScaling write SetSmoothScaling default true;

    { How to treat alpha channel of the image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this property force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel read FAlphaChannel write SetAlphaChannel default acAuto;

    { URL of the image to be loaded.
      When empty, no image is loaded and this is invisible. }
    property Url: String read FUrl write SetUrl;

    { Mipmaps make the image look good when it scaled to be @italic(much)
      smaller on the screen. Using mipmaps also forces the image to have
      power-of-2 size (it will be resized to power-of-2 if necessary).

      Mipmaps typically make sense for 3D objects,
      when the camera may move arbitrarily far from the object.
      For 2D games, you usually don't want to use mipmaps, as you often want to avoid
      scaling the image to be power-of-2. }
    property Mipmaps: Boolean read FMipmaps write SetMipmaps default false;

    { Return the detailed information (triangle, and position within the triangle)
      when testing collisions with this image
      in the resulting @link(TRayCollision) from @link(TCastleViewport.MouseRayHit). }
    property PreciseCollisions: Boolean read FPreciseCollisions write SetPreciseCollisions default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleimagetransform_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TODO: Implement CropRectangle at some point.
  Note that it will force a different implementation of RepeatImage, when crop is used. }

{ TCastleImageTransform ---------------------------------------------------------------- }

constructor TCastleImageTransform.Create(AOwner: TComponent);
begin
  inherited;

  { set default values }
  FSmoothScaling := true;
  FAlphaChannel := acAuto;
  FPivot := Vector2(0.5, 0.5);
  FSize := Vector2(0, 0);
  FRepeatImage := Vector2(1, 1);
  FShift := Vector2(0, 0);
  FColor := White;

  { create scene }
  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  Add(FScene);

  { create X3D nodes }
  FTexPropertiesNode := TTexturePropertiesNode.Create;
  FTexPropertiesNode.MinificationFilter := minDefault; // corresponding to default SmoothScaling = true
  FTexPropertiesNode.MagnificationFilter := magDefault; // corresponding to default SmoothScaling = true
  FTexPropertiesNode.BoundaryModeS := bmClampToEdge; // correspoding to default FRepeatImage and FShift, UpdateCoordinateNodes may change it
  FTexPropertiesNode.BoundaryModeT := bmClampToEdge; // correspoding to default FRepeatImage and FShift, UpdateCoordinateNodes may change it
  { Do not force "power of 2" size, which may prevent mipmaps.
    This seems like a better default (otherwise the resizing underneath
    may cause longer loading time, and loss of quality, if not expected).
    Consistent with X3DLoadInternalImage. }
  FTexPropertiesNode.GuiTexture := not FMipmaps;

  FTextureNode := TImageTextureNode.Create;
  { No point in adjusting RepeatS/T: TextureProperties override it.
  FTextureNode.RepeatS := false;
  FTextureNode.RepeatT := false; }
  FTextureNode.TextureProperties := FTexPropertiesNode;

  FMaterialNode := TUnlitMaterialNode.Create;
  FMaterialNode.EmissiveColor := FColor.XYZ;
  FMaterialNode.Transparency := 1 - FColor.W;
  FMaterialNode.EmissiveTexture := FTextureNode;

  FAppearanceNode := TAppearanceNode.Create;
  FAppearanceNode.AlphaMode := amAuto; // corresponding to default AlphaChannel = acAuto
  FAppearanceNode.Material := FMaterialNode;

  FCoordinateNode := TCoordinateNode.Create;
  { Add some zero vectors, this way renderer can prepare VBO
    and we avoid X3D warnings about invalid vertex index when FCoordinateNode is empty. }
  FCoordinateNode.SetPoint([
    TVector3.Zero,
    TVector3.Zero,
    TVector3.Zero,
    TVector3.Zero
  ]);

  FTexCoordNode := TTextureCoordinateNode.Create;
  { Add some zero vectors, this way we avoid warnings that 4 tex coords are expected.
    Testcase: Add TCastleImageTransform, or any TCastleCamera or TCastlePunctualLight
    (since they all use TCastleImageTransform for design-time visualization),
    to a viewport and activate shadow volumes by setting Shadows=true on some light. }
  FTexCoordNode.SetPoint([
    TVector2.Zero,
    TVector2.Zero,
    TVector2.Zero,
    TVector2.Zero
  ]);

  FIndexedTriangleSetNode := TIndexedTriangleSetNode.Create;
  FIndexedTriangleSetNode.SetIndex([0, 1, 2, 0, 2, 3]);
  FIndexedTriangleSetNode.Coord := FCoordinateNode;
  FIndexedTriangleSetNode.TexCoord := FTexCoordNode;
  FIndexedTriangleSetNode.Solid := false;

  FShapeNode := TShapeNode.Create;
  FShapeNode.Appearance := FAppearanceNode;
  FShapeNode.Geometry := FIndexedTriangleSetNode;
  { Render will change to true once some image is loaded.
    This way we avoid rendering FCoordinateNode with zero contents. }
  FShapeNode.Visible := false;

  FRootNode := TX3DRootNode.Create;
  FRootNode.AddChildren(FShapeNode);
  FScene.Load(FRootNode, true);

  FUrlMonitoring.Init({$ifdef FPC}@{$endif} ReloadUrl);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleimagetransform_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleImageTransform.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleimagetransform_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  FreeAndNil(FScene);
  inherited;
end;

procedure TCastleImageTransform.BeforeDestruction;
begin
  FUrlMonitoring.Finish(FUrl);
  inherited;
end;

function TCastleImageTransform.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'PivotPersistent') or
     (PropertyName = 'SizePersistent') or
     (PropertyName = 'RepeatImagePersistent') or
     (PropertyName = 'ShiftPersistent') or
     (PropertyName = 'ColorPersistent') or
     (PropertyName = 'SmoothScaling') or
     (PropertyName = 'AlphaChannel') or
     (PropertyName = 'PreciseCollisions') or
     (PropertyName = 'Url') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleImageTransform.UpdateCoordinateNodes;
var
  X1, Y1, X2, Y2, W, H: Single;
begin
  FTextureNode.IsTextureLoaded := true;
  { Render is true if and only if FCoordinateNode contains useful content (not just zeros) }
  FShapeNode.Visible := FTextureNode.IsTextureImage;
  if FTextureNode.IsTextureImage then
  begin
    FImageWidth := FTextureNode.TextureImage.Width;
    FImageHeight := FTextureNode.TextureImage.Height;
    W := FImageWidth;
    H := FImageHeight;

    { change W, H according to Size }
    if not ((Size.X = 0) and (Size.Y = 0)) then
    begin
      if (Size.X <> 0) and (Size.Y <> 0) then
      begin
        W := Size.X;
        H := Size.Y;
      end else
      if Size.X <> 0 then
      begin
        H := H * Size.X / W;
        W := Size.X;
      end else
      begin
        Assert(Size.Y <> 0);
        Assert(Size.X = 0);
        W := W * Size.Y / H;
        H := Size.Y;
      end;
    end;

    { calculate coordinates }
    X1 := - W * Pivot.X;
    Y1 := - H * Pivot.Y;
    X2 := W * (RepeatImage.X - Pivot.X);
    Y2 := H * (RepeatImage.Y - Pivot.Y);
    FCoordinateNode.SetPoint([
      Vector3(X1, Y1, 0),
      Vector3(X2, Y1, 0),
      Vector3(X2, Y2, 0),
      Vector3(X1, Y2, 0)
    ]);

    { calculate texture coordinates }
    X1 := FShift.X;
    Y1 := FShift.Y;
    X2 := FShift.X + FRepeatImage.X;
    Y2 := FShift.Y + FRepeatImage.Y;

    FTexCoordNode.SetPoint([
      Vector2(X1, Y1),
      Vector2(X2, Y1),
      Vector2(X2, Y2),
      Vector2(X1, Y2)
    ]);

    FTexPropertiesNode.BoundaryModeS := BoolRepeatToBoundaryMode[(FShift.X <> 0) or (FRepeatImage.X > 1)];
    FTexPropertiesNode.BoundaryModeT := BoolRepeatToBoundaryMode[(FShift.Y <> 0) or (FRepeatImage.Y > 1)];
  end else
  begin
    FImageWidth := 0;
    FImageHeight := 0;

    FCoordinateNode.SetPoint([
      TVector3.Zero,
      TVector3.Zero,
      TVector3.Zero,
      TVector3.Zero
    ]);
    FTexCoordNode.SetPoint([
      TVector2.Zero,
      TVector2.Zero,
      TVector2.Zero,
      TVector2.Zero
    ]);
  end;
end;

procedure TCastleImageTransform.SetUrl(const Value: String);
begin
  if FUrl <> Value then
  begin
    FUrlMonitoring.ChangeUrl(FUrl, Value);
    if Value <> '' then
      FTextureNode.SetUrl([Value])
    else
      FTextureNode.SetUrl([]);
    UpdateCoordinateNodes;
  end;
end;

procedure TCastleImageTransform.ReloadUrl;
var
  SavedUrl: String;
begin
  SavedUrl := Url;
  Url := '';
  Url := SavedUrl;
end;

procedure TCastleImageTransform.SetPivot(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FPivot, Value) then
  begin
    FPivot := Value;
    UpdateCoordinateNodes;
  end;
end;

procedure TCastleImageTransform.SetSize(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FSize, Value) then
  begin
    FSize := Value;
    UpdateCoordinateNodes;
  end;
end;

procedure TCastleImageTransform.SetRepeatImage(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FRepeatImage, Value) then
  begin
    FRepeatImage := Value;
    UpdateCoordinateNodes;
  end;
end;

procedure TCastleImageTransform.SetShift(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FShift, Value) then
  begin
    FShift := Value;
    UpdateCoordinateNodes;
  end;
end;

procedure TCastleImageTransform.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    FMaterialNode.EmissiveColor := FColor.XYZ;
    FMaterialNode.Transparency := 1 - FColor.W;
  end;
end;

procedure TCastleImageTransform.SetSmoothScaling(const Value: Boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    if Value then
    begin
      FTexPropertiesNode.MinificationFilter := minDefault;
      FTexPropertiesNode.MagnificationFilter := magDefault;
    end else
    begin
      FTexPropertiesNode.MinificationFilter := minNearest;
      FTexPropertiesNode.MagnificationFilter := magNearest;
    end;
    FScene.ChangedAll; // TODO ChangedAll should not be needed
  end;
end;

procedure TCastleImageTransform.SetAlphaChannel(const Value: TAutoAlphaChannel);
const
  AlphaChannelToMode: array [TAutoAlphaChannel] of TAlphaMode = (
    { acAuto -> } amAuto,
    { acNone -> } amOpaque,
    { acTest -> } amMask,
    { acBlending -> } amBlend
  );
begin
  if FAlphaChannel <> Value then
  begin
    FAlphaChannel := Value;
    FAppearanceNode.AlphaMode := AlphaChannelToMode[Value];
    FScene.ChangedAll; // TODO ChangedAll should not be needed
  end;
end;

procedure TCastleImageTransform.SetMipmaps(const Value: Boolean);
begin
  if FMipmaps <> Value then
  begin
    FMipmaps := Value;
    FTexPropertiesNode.GuiTexture := not FMipmaps;
    FScene.ChangedAll; // TODO ChangedAll should not be needed
  end;
end;

function TCastleImageTransform.InternalBuildNodeInside: TObject;
begin
  Assert(FScene <> nil); // always <> nil in this class
  Assert(FScene.RootNode <> nil); // always <> nil in this class
  Result := FScene.RootNode.DeepCopy;
end;

procedure TCastleImageTransform.SetPreciseCollisions(const Value: Boolean);
begin
  if FPreciseCollisions <> Value then
  begin
    FPreciseCollisions := Value;
    FScene.PreciseCollisions := Value;
    { Note: This also adds ssRendering octree,
      somewhat useless as image is just 1 shape in an internal scene.
      But does not seem problematic either. }
  end;
end;

procedure TCastleImageTransform.LoadFromImage(const Image: TEncodedImage;
  const TakeImageOwnership: Boolean);
begin
  // unload the previous image, and make FUrlMonitoring not watch anything
  Url := '';
  FTextureNode.LoadFromImage(Image, TakeImageOwnership, '');
  UpdateCoordinateNodes;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleimagetransform_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
