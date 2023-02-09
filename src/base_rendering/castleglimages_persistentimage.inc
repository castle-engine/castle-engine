{%MainUnit castleglimages.pas}
{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLImages unit: TCastleImagePersistent. }

{$ifdef read_interface}

{$ifdef read_interface_type}
// type (continue type section from previous include file)
  { Image that can be easily loaded from URL (possibly reusing a cache),
    drawn, and serialized to/from file.

    @unorderedList(
      @item(
        Wraps TEncodedImage (image data in normal memory,
        usually one of TCastleImage descendants)
        and TDrawableImage (image data on GPU).)

      @item(
        Adds a cache of TDrawableImage underneath
        (which can be turned off, see @link(Cache)).)

      @item(
        Can be serialized (so you can use this as a SubComponent
        in various TCastleUserInterface descendants that want to expose
        some image).)
    )
  }
  TCastleImagePersistent = class(TPersistent)
  strict private
    FURL: string;
    FCache: boolean;
    FScaleFromUrl: Single;
    FDrawableImage: TDrawableImage;
    FDrawableImageFromCache: boolean;
    FOwnsDrawableImage: Boolean;
    FOnChange: TNotifyEvent;
    FInternalVisualizeProtectedSides: Boolean;

    { Parameters passed straight into FDrawableImage before Draw. }
    FSmoothScaling: Boolean;
    FClip: Boolean;
    FClipLine: TVector3;
    FRotation: Single;
    FRotationCenter: TVector2;
    FAlphaChannel: TAutoAlphaChannel;
    FColor: TCastleColor;
    FCustomShader: TGLSLProgram;
    FScaleCorners: Single;

    { Saved / restored by DrawUiBegin / DrawUiEnd. }
    FSavedOnChange: TNotifyEvent;
    FSavedColor: TCastleColor;
    FSavedScaleCorners: Single;

    { Parameters used in Draw. }
    FProtectedSides: TBorder;
    FRegion: TRegion;
    FRegionEnabled :Boolean;
    FFlipHorizontal: Boolean;
    FFlipVertical: Boolean;

    function GetImage: TEncodedImage;
    function GetOwnsImage: boolean;
    procedure SetOwnsImage(const Value: boolean);
    procedure SetCache(const Value: Boolean);
    procedure SetRegion(const AValue: TRegion);
    procedure SetRegionEnabled(const AValue: Boolean);
    procedure SetURL(const Value: string);
    procedure SetImage(const Value: TEncodedImage);
    procedure SetDrawableImage(const Value: TDrawableImage);
    procedure SetSmoothScaling(const Value: boolean);
    procedure SetFlipVertical(const Value: Boolean);
    procedure SetFlipHorizontal(const Value: Boolean);
    procedure SetRotation(const Value: Single);
    procedure SetRotationCenter(const Value: TVector2);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    procedure SetColor(const Value: TCastleColor);
    procedure SetCustomShader(const Value: TGLSLProgram);
    procedure SetScaleCorners(const Value: Single);
    procedure SetClip(const Value: Boolean);
    procedure SetClipLine(const Value: TVector3);

    procedure ProtectedSidesChange(Sender: TObject);
    procedure CreateDrawableImage;
    { Free FDrawableImage if needed, and make it @nil. }
    procedure FreeDrawableImage;
  protected
    procedure DoChange; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    { Reserved for parent UI control to listen for changes. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Image contents, or @nil if none. You can set it by setting @link(URL),
      or by setting this property directly,
      or by setting @link(DrawableImage).

      Note that by default the TEncodedImage instance assigned here is owned
      by this component (see @link(OwnsImage)).
      So if you set this property to your custom TEncodedImage instance you should
      leave memory management of this instance to this component.
      You can either create a copy by TEncodedImage.MakeCopy
      if you want to give here only a copy, or you can change @link(OwnsImage)
      to @false.

      It is allowed to modify the contents or even size of this image.
      Just make sure to call @link(ImageChanged) after the modifications
      are done to update the rendered image (and possibly
      the UI control size, e.g. in case TCastleImageControl
      uses this TCastleImagePersistent, and TCastleImageControl.Stretch=false). }
    property Image: TEncodedImage read GetImage write SetImage;

    { Whether the memory management of assigned @link(Image) is automatic.
      See @link(Image) documentation for details.

      Note that setting @link(URL) changes @link(Image),
      and changes OwnsImage unconditionally to @true.

      Note that setting @link(DrawableImage) changes @link(Image) too,
      and changes OwnsImage to the value present in @link(TDrawableImage.OwnsImage). }
    property OwnsImage: boolean read GetOwnsImage write SetOwnsImage default true;

    { Image used for drawing. Never @nil.
      Always @link(TDrawableImage.Image) equals our @link(Image).

      Usually you do not want to change this property.
      You can change @link(URL), or @link(Image), and the drawable
      image will reflect this.
      But there are good use-cases to change the @link(DrawableImage)
      directly: e.g. you can use fast image-to-image
      drawing using @link(TDrawableImage.DrawFrom).
      Or you can set a new instance of TDrawableImage here.
      Be sure to adjust @link(OwnsDrawableImage) as needed.

      Note that when OpenGL(ES) context is lost and recreated (which can
      happen at any moment on mobile devices), the contents of this are
      reinitialized from @link(Image). }
    property DrawableImage: TDrawableImage read FDrawableImage write SetDrawableImage;

    { Whether we should automatically free the @link(DrawableImage) instance.
      Note that this is restored to @true when we need to recreate the
      TDrawableImage internally, e.g. when @link(Image) instance changed
      (when even size changed, we have to recreate TDrawableImage). }
    property OwnsDrawableImage: boolean read FOwnsDrawableImage write FOwnsDrawableImage default true;

    { Center of rotation.
      Expressed as a fraction within the drawn ScreenRectangle,
      (0,0) means bottom-left corner, (1,1) means top-right corner.
      Default (0.5,0.5). }
    property RotationCenter: TVector2 read FRotationCenter write SetRotationCenter;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The equation is calculated in the coordinates in which image
      X, Y spans from (0, 0) (bottom-left) to (1, 1) (top-right).
      For example ClipLine = (1, 0, -0.5) means that we reject pixels
      where @code(1 * x + 0 * y - 0.5 < 0). In other words,
      we reject pixels where @code(x < 0.5), so we reject the left half of the image. }
    property ClipLine: TVector3 read FClipLine write SetClipLine;

    { Custom GLSL shading code.
      See TDrawableImage.CustomShader for explanation. }
    property CustomShader: TGLSLProgram read FCustomShader write SetCustomShader;

    { In case of @link(TDrawableImage.Draw3x3) the corners
      on screen are scaled by this amount. This is especially useful
      for UI scaling, see @link(TCastleContainer.UIScaling)
      and @link(TCastleUserInterface.UIScale).

      This is deliberately not published, because in typical usage
      this value is updated by UI control that owns this,
      before every render. }
    property ScaleCorners: Single
      read FScaleCorners write SetScaleCorners {$ifdef FPC}default 1{$endif};

    { Color tint of the image. This simply multiplies the image RGBA components,
      just like @link(TDrawableImage.Color). By default this is opaque white,
      which means that image colors are unchanged. }
    property Color: TCastleColor read FColor write SetColor;

    { Cropped region. "empty region"(Width<0 or Height<0) means using the whole image. }
    property Region: TRegion read FRegion write SetRegion ;

    property InternalVisualizeProtectedSides: Boolean
      read FInternalVisualizeProtectedSides write FInternalVisualizeProtectedSides default true;

    property ScaleFromUrl: Single read FScaleFromUrl;

    { If you modified the @link(Image) contents
      (colors, or maybe even image size) you need to call this. }
    procedure ImageChanged;

    { Value that have been cut by Region }
    function Width: Cardinal;

    { Value that have been cut by Region }
    function Height: Cardinal;

    function Empty: Boolean;

    { Determining if image should be clipped }
    function Clipped: Boolean;

    { Draw the image in given screen area. }
    procedure Draw(const ScreenRectangle: TFloatRectangle);

    { Prepare image for typical drawing from UI.

      - Saves OnChange and Color (to be later restored by CommonAfterDraw).

      - Clears OnChange (so that changes between CommonBeforeDraw and CommonAfterDraw
        do not cause OnChange, which is typically used to make repaint).

      - Sets ScaleCorners.

      Must be followed by DrawUiEnd.
    }
    procedure DrawUiBegin(const NewScaleCorners: Single);

    { Finish image usage from typical UI drawing.

      - Restores OnChange and Color. }
    procedure DrawUiEnd;
  published
    { URL of the image.
      Set this to load a new image, you can set to '' to clear the image.

      Changing this also changes @link(Image) and @link(DrawableImage).
      @link(OwnsDrawableImage) and @link(OwnsImage) always change to @true. }
    property URL: String read FURL write SetURL;

    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this to force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel
      read FAlphaChannel write SetAlphaChannel default acAuto;

    { If @true, the @link(DrawableImage) is loaded and freed using a cache.
      This can save memory and loading time a lot, if you reuse the same
      URL in many TCastleImagePersistent instances.

      The limitation is
      that you shouldn't change the @link(Image) contents
      (e.g. changing @link(TCastleImage.Colors))
      or @link(DrawableImage) contents
      (e.g. by using @link(TDrawableImage.RenderToImageBegin)),
      or you will change all shared images.

      Note that if you assign @link(DrawableImage) (instead of @link(URL))
      and we should free it ourselves (because @link(OwnsDrawableImage))
      then we assume it was @italic(not) loaded through our internal cache.

      Note that if you assign @link(Image) (instead of @link(URL))
      then we assume it was @italic(not) loaded through our internal cache.
      (We make sure to detach the TDrawableImage from cache too, in this case.)

      Changing this property reloads the image (if some @link(URL) was set). }
    property Cache: Boolean read FCache write SetCache default true;

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      See @link(TDrawableImage.SmoothScaling). }
    property SmoothScaling: boolean
      read FSmoothScaling write SetSmoothScaling default true;

    { Rotation in radians. Default value 0. }
    property Rotation: Single read FRotation write SetRotation {$ifdef FPC}default 0{$endif};

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine). }
    property Clip: boolean read FClip write SetClip default false;

    property FlipHorizontal: Boolean read FFlipHorizontal write SetFlipHorizontal
      default false;
    property FlipVertical: Boolean read FFlipVertical write SetFlipVertical
      default false;

    { Corners and edges of the image that are protected from scaling distortion
      when @link(TCastleImageControl.Stretch) is used.
      We use the 9-slicing algorithm,
      described at @link(TDrawableImage.Draw3x3)
      and https://en.wikipedia.org/wiki/9-slice_scaling to scale the image. }
    property ProtectedSides: TBorder read FProtectedSides;

    property RegionEnabled: Boolean read FRegionEnabled write SetRegionEnabled default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleimagepersistent_persistent_vectors.inc}
  {$undef read_interface_class}
  end;
{$endif read_interface_type}
{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleImagePersistent.Create;
begin
  inherited;
  { assign default values of properties }
  FCache := true;
  FSmoothScaling := true;
  FRotationCenter := Vector2(0.5, 0.5);
  FColor := White;
  FScaleCorners := 1;
  FScaleFromUrl := 1;
  FInternalVisualizeProtectedSides := true;
  FRegion := TFloatRectangle.Empty;

  FProtectedSides := TBorder.Create({$ifdef FPC}@{$endif}ProtectedSidesChange);
  CreateDrawableImage;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleimagepersistent_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleImagePersistent.Destroy;
begin
  FreeDrawableImage;
  FreeAndNil(FProtectedSides);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleimagepersistent_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleImagePersistent.ProtectedSidesChange(Sender: TObject);
begin
  DoChange;
end;

procedure TCastleImagePersistent.CreateDrawableImage;
begin
  FDrawableImage := TDrawableImage.Create(nil, FSmoothScaling,
    true { default OwnsImage });
  FOwnsDrawableImage := true;
  FDrawableImageFromCache := false;
end;

procedure TCastleImagePersistent.FreeDrawableImage;
begin
  if OwnsDrawableImage then
  begin
    if FDrawableImageFromCache then
      DrawableImageCache.FreeDrawableImage(FDrawableImage)
    else
      FreeAndNil(FDrawableImage);
  end else
    FDrawableImage := nil;
end;

procedure TCastleImagePersistent.SetURL(const Value: string);

  function GetScaleFromUrl: Single;
  var
    BaseName: String;
    P: Integer;
    Scale: Single;
  begin
    Result := 1;
    BaseName := DeleteURIExt(ExtractURIName(URL));
    if (Length(BaseName) >= 4) and // BaseName must look like at least '?@4x'
       (BaseName[Length(BaseName)] = 'x') then
    begin
      P := BackPos('@', BaseName);
      if (P > 1) and
         TryStrToFloatDot(CopyPos(BaseName, P + 1, Length(BaseName) - 1), Scale) and
         (Scale > 0) then
        Result := Scale;
    end;
  end;

begin
  if FURL <> Value then
  begin
    { We deliberately change URL even when loading image could fail.
      This way URL property is always correctly deserialized in CGE editor,
      even when loading the image failed. }
    FURL := Value;
    FScaleFromUrl := GetScaleFromUrl;

    FreeDrawableImage;

    if Value <> '' then
    begin
      try
        if Cache then
        begin
          FDrawableImage := DrawableImageCache.LoadDrawableImage(Value, FSmoothScaling);
          FDrawableImageFromCache := true;
          FOwnsDrawableImage := true;
          Assert(OwnsImage, 'TDrawableImage from cache should always have OwnsImage = true');
        end else
        begin
          FDrawableImage := TDrawableImage.Create(Value, FSmoothScaling);
          FDrawableImageFromCache := false;
          FOwnsDrawableImage := true;
          Assert(OwnsImage, 'New TDrawableImage should always have OwnsImage = true');
        end;
      except
        { If loading file failed, and we're inside CGE editor,
          merely report a warning. This allows deserializing in CGE editor
          designs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then
          begin
            WritelnWarning('TCastleImagePersistent', 'Failed to load image "%s": %s',
              [URIDisplay(Value), ExceptMessage(E)]);
            CreateDrawableImage;
            Exit;
          end else
            raise;
        end;
      end;
    end else
      CreateDrawableImage;

    Assert(OwnsImage);
    Assert(OwnsDrawableImage);
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetCache(const Value: Boolean);
var
  OldURL: String;
begin
  if FCache <> Value then
  begin
    FCache := Value;
    if URL <> '' then
    begin
      // free and reload the image, following new Cache value
      OldURL := URL;
      URL := '';
      URL := OldURL;
      DoChange; // this possibly changed rendered contents
    end;
  end;
end;

procedure TCastleImagePersistent.SetRegion(const AValue: TRegion);
function SameFloatRectangle(const a,b:TFloatRectangle):Boolean;
begin
 Result := (SameValue(a.Left  , b.Left)) and
      (SameValue(a.Bottom, b.Bottom)) and
      (SameValue(a.Width , b.Width)) and
      (SameValue(a.Height, b.Height));
end;

begin
  { don't use 'PerfectlyEquals' or 'Equals' to judge.
    Otherwise, if the Region is empty, it cannot be modified in the editor.}
  if SameFloatRectangle(FRegion, AValue) then Exit;

  FRegion := AValue;
  DoChange;
end;

procedure TCastleImagePersistent.SetRegionEnabled(const AValue: Boolean);
begin
  if FRegionEnabled = AValue then Exit;

  FRegionEnabled := AValue;
  DoChange;
end;

function TCastleImagePersistent.GetImage: TEncodedImage;
begin
  Result := FDrawableImage.Image;
end;

procedure TCastleImagePersistent.SetImage(const Value: TEncodedImage);
begin
  if FDrawableImage.Image <> Value then
  begin
    if FDrawableImageFromCache then
    begin
      FreeDrawableImage;
      CreateDrawableImage;
    end;
    Assert(not FDrawableImageFromCache);
    FDrawableImage.Image := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetDrawableImage(const Value: TDrawableImage);
begin
  if FDrawableImage <> Value then
  begin
    FreeDrawableImage;
    if Value = nil then
      raise Exception.Create('Cannot assign TCastleImagePersistent.DrawableImage to nil');
    FDrawableImage := Value;
    FDrawableImageFromCache := false;
    DoChange;
  end;
end;

function TCastleImagePersistent.GetOwnsImage: boolean;
begin
  Result := FDrawableImage.OwnsImage;
end;

procedure TCastleImagePersistent.SetOwnsImage(const Value: boolean);
begin
  { For all TDrawableImage in DrawableImageCache,
    we keep their OwnsImage as @true, to keep the memory management simple.
    So when changing this, and previous image was from cache,
    we need to lose previous image. }

  if FDrawableImage.OwnsImage <> Value then
  begin
    if FDrawableImageFromCache then
    begin
      FreeDrawableImage;
      CreateDrawableImage;
      DoChange;
    end;
    Assert(not FDrawableImageFromCache);
    FDrawableImage.OwnsImage := Value;
  end;
end;

procedure TCastleImagePersistent.ImageChanged;
begin
  FDrawableImage.Load(FDrawableImage.Image);
  DoChange;
end;

procedure TCastleImagePersistent.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TCastleImagePersistent.Width: Cardinal;
begin
  if Clipped then
    Result := Min(Floor(FRegion.Width), FDrawableImage.Width)
  else
    Result := FDrawableImage.Width;
end;

function TCastleImagePersistent.Height: Cardinal;
begin
  if Clipped then
    Result := Min(Floor(FRegion.Height), FDrawableImage.Height)
  else
    Result := FDrawableImage.Height;
end;

function TCastleImagePersistent.Empty: Boolean;
begin
  Result := (Width = 0) or (Height = 0);
end;

function TCastleImagePersistent.Clipped: Boolean;
begin
  Result := FRegionEnabled and (not FRegion.IsEmpty);
end;

procedure TCastleImagePersistent.DrawUiBegin(const NewScaleCorners: Single);
begin
  FSavedOnChange := OnChange;
  FSavedColor := Color;
  FSavedScaleCorners := ScaleCorners;

  OnChange := nil;

  ScaleCorners := NewScaleCorners;
end;

procedure TCastleImagePersistent.DrawUiEnd;
begin
  Color := FSavedColor;
  ScaleCorners := FSavedScaleCorners;

  // restore OnChange last, so that no OnChange is emitted between DrawUiBegin / DrawUiEnd
  OnChange := FSavedOnChange;
end;

procedure TCastleImagePersistent.Draw(
  const ScreenRectangle: TFloatRectangle);

  procedure VisualizeProtectedSides;

    { Make A + B <= Max.
      If A + B are too large, decrease them, but preserving A to B ratio.
      In effect e.g.
        A = 10, B = 100, MaxSize = 1
      ->
        A := 1/11, B = 10/11

      Copied from FitWithin inside TDrawableImage.Draw3x3, to mirror what it does.
    }
    procedure FitWithin(var A, B: Single; const Max: Single);
    begin
      if A + B > Max then
      begin
        A := Max * A / (A + B);
        B := Max * B / (A + B);
      end;
    end;

  var
    L, R, B, T: Single;
  begin
    L := ProtectedSides.TotalLeft   * ScaleCorners;
    R := ProtectedSides.TotalRight  * ScaleCorners;
    B := ProtectedSides.TotalBottom * ScaleCorners;
    T := ProtectedSides.TotalTop    * ScaleCorners;

    FitWithin(L, R, ScreenRectangle.Width);
    FitWithin(B, T, ScreenRectangle.Height);

    DrawPrimitive2D(pmLines, [
      // vertical lines
      Vector2(ScreenRectangle.Left + L, ScreenRectangle.Bottom),
      Vector2(ScreenRectangle.Left + L, ScreenRectangle.Top),
      Vector2(ScreenRectangle.Right - R, ScreenRectangle.Bottom),
      Vector2(ScreenRectangle.Right - R, ScreenRectangle.Top),
      // horizontal lines
      Vector2(ScreenRectangle.Left , ScreenRectangle.Bottom + B),
      Vector2(ScreenRectangle.Right, ScreenRectangle.Bottom + B),
      Vector2(ScreenRectangle.Left , ScreenRectangle.Top - T),
      Vector2(ScreenRectangle.Right, ScreenRectangle.Top - T)
    ], Vector4(1, 1, 0, 0.5));
  end;

  function ImageRect():TFloatRectangle;
  begin
    if Clipped then Result := FRegion else Result := FloatRectangle(0, 0, FDrawableImage.Width, FDrawableImage.Height);
  end;

var
  ProtectedSidesVector, SrcSidesVector: TVector4;
begin
  if (DrawableImage.Width = 0) or (DrawableImage.Height = 0) then Exit;

  { All the DrawableImage properties must be set here,
    in case muliple TCastleImagePersistent refer to the same
    DrawableImage instance through the cache.
    Fortunately, almost all TDrawableImage properties have zero cost
    of change. }
  DrawableImage.SmoothScaling := FSmoothScaling;
  DrawableImage.Clip := FClip;
  DrawableImage.ClipLine := FClipLine;
  DrawableImage.Rotation := FRotation;
  DrawableImage.RotationCenter := FRotationCenter;
  DrawableImage.Alpha := FAlphaChannel;
  DrawableImage.Color := FColor;
  DrawableImage.CustomShader := FCustomShader;
  DrawableImage.ScaleCorners := FScaleCorners;

  if not FProtectedSides.Exists then
  begin
    if FlipHorizontal or FlipVertical then
      FDrawableImage.DrawFlipped(ScreenRectangle, ImageRect, FlipHorizontal, FlipVertical)
    else
      FDrawableImage.Draw(ScreenRectangle, ImageRect);
  end else
  begin
    // TODO: FlipHorizontal, FlipVertical ignored in this case
    ProtectedSidesVector := Vector4(
      ProtectedSides.TotalTop,
      ProtectedSides.TotalRight,
      ProtectedSides.TotalBottom,
      ProtectedSides.TotalLeft
    );
    if Clipped then
      SrcSidesVector := Vector4(
        FDrawableImage.Height - FRegion.Top,
        FDrawableImage.Width - FRegion.Right,
        FRegion.Bottom,
        FRegion.Left
      ) 
	else
      SrcSidesVector := TVector4.Zero;

    FDrawableImage.Draw3x3(ScreenRectangle, ProtectedSidesVector, SrcSidesVector);

    if CastleDesignMode and InternalVisualizeProtectedSides and ProtectedSides.Exists then
      VisualizeProtectedSides;
  end;
end;

procedure TCastleImagePersistent.SetSmoothScaling(const Value: boolean);
begin
  if FSmoothScaling <> Value then
  begin
    FSmoothScaling := Value;
    DrawableImage.SmoothScaling := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetClip(const Value: Boolean);
begin
  if FClip <> Value then
  begin
    FClip := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetClipLine(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FClipLine, Value) then
  begin
    FClipLine := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetRotation(const Value: Single);
begin
  if FRotation <> Value then
  begin
    FRotation := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetRotationCenter(const Value: TVector2);
begin
  if not TVector2.PerfectlyEquals(FRotationCenter, Value) then
  begin
    FRotationCenter := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  if FAlphaChannel <> Value then
  begin
    FAlphaChannel := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetCustomShader(const Value: TGLSLProgram);
begin
  if FCustomShader <> Value then
  begin
    FCustomShader := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetScaleCorners(const Value: Single);
begin
  if FScaleCorners <> Value then
  begin
    FScaleCorners := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetFlipVertical(const Value: Boolean);
begin
  if FFlipVertical <> Value then
  begin
    FFlipVertical := Value;
    DoChange;
  end;
end;

procedure TCastleImagePersistent.SetFlipHorizontal(const Value: Boolean);
begin
  if FFlipHorizontal <> Value then
  begin
    FFlipHorizontal := Value;
    DoChange;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleimagepersistent_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
