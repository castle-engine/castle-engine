{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Indicate stretching approach for @link(TCastleImageControl.ProportionalScaling). }
  TProportionalScaling = (
    { Adjust to the requested area, ignoring proportions. }
    psNone,
    { Adjust to the requested area, honoring proportions, such that the image
      may be larger than the requested area. }
    psEnclose,
    { Adjust to the requested area, honoring proportions, such that the image
      may be smaller than the requested area. }
    psFit
  );

  { Image control.
    Size is automatically adjusted to the image size, if Stretch is @false (default).
    You should set TCastleImageControl.Left, TCastleImageControl.Bottom properties,
    and load your image by setting TCastleImageControl.URL property
    or straight TCastleImageControl.Image.

    We automatically use alpha test or alpha blending based
    on loaded image alpha channel (see @link(TGLImageCore.Alpha)).
    You can influence this by @link(AlphaChannel) property. }
  TCastleImageControl = class(TCastleUserInterface)
  strict private
    FURL: string;
    FScaleFromUrl: Single;
    FImage: TEncodedImage;
    FImageLoadedFromCache: boolean;
    FDrawableImage: TGLImage;
    FOwnsImage: boolean;
    FOwnsDrawableImage: boolean;
    FStretch: boolean;
    FProportionalScaling: TProportionalScaling;
    FProtectedSides: TBorder;
    FCache: boolean;
    FFlipHorizontal: Boolean;
    FFlipVertical: Boolean;
    FDetectScaleFromUrl: Boolean;

    { Properties simply synchronized with the same properties of FDrawableImage. }
    function GetCenterX: Single;
    function GetCenterY: Single;
    function GetRotation: Single;
    function GetAlphaChannel: TAutoAlphaChannel;
    function GetColor: TCastleColor;
    function GetSmoothScaling: boolean;
    function GetClip: boolean;
    function GetClipLine: TVector3;
    function GetCustomShader: TGLSLProgram;
    procedure SetCenterX(const Value: Single);
    procedure SetCenterY(const Value: Single);
    procedure SetRotation(const Value: Single);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    procedure SetColor(const Value: TCastleColor);
    procedure SetSmoothScaling(const Value: boolean);
    procedure SetClip(const Value: boolean);
    procedure SetClipLine(const Value: TVector3);
    procedure SetCustomShader(const Value: TGLSLProgram);

    procedure SetURL(const Value: string);
    procedure SetImage(Value: TEncodedImage);
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
    procedure SetStretch(const Value: boolean);
    procedure SetProportionalScaling(const Value: TProportionalScaling);
    function GetProportional: boolean;
    procedure SetProportional(const Value: boolean);
    procedure SetFlipVertical(const Value: Boolean);
    procedure SetFlipHorizontal(const Value: Boolean);
    procedure SetDetectScaleFromUrl(const Value: Boolean);

    { Free FImage if needed, and make it @nil. }
    procedure FreeImage;
    { Free FDrawableImage if needed, and make it @nil. }
    procedure FreeDrawableImage;
    procedure SetDrawableImage(const Value: TGLImage);
    procedure ProtectedSidesChange(Sender: TObject);

    function GetCorners: TVector4Integer;
    procedure SetCorners(const Value: TVector4Integer);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;

    procedure ImageChanged;

    { Image displayed, or @nil if none. You can set it by setting @link(URL),
      or you can set this property directly if you loaded/created the image contents
      yourself.

      Note that by default the TEncodedImage instance assigned here is owned
      by this component (see @link(OwnsImage)).
      So if you set this property to your custom TEncodedImage instance you should
      leave memory management of this instance to this component.
      You can either create a copy by TEncodedImage.MakeCopy
      if you want to give here only a copy, or you can change @link(OwnsImage)
      to @false.

      It is allowed to modify the contents or even size of this image.
      Just make sure to call @link(ImageChanged) after the modifications are done
      to update the actual rendered image.
      The control size will be updated immediately (taking into account current
      @link(Stretch) and related properties values). }
    property Image: TEncodedImage read FImage write SetImage;

    { Whether the memory management of assigned @link(Image) is automatic.
      See @link(Image) documentation for details. }
    property OwnsImage: boolean read FOwnsImage write FOwnsImage default true;

    { Image used for drawing.
      In most circumstances, you should treat it as an internal property,
      automatically managed by this class. You do not need to explicitly
      use it in any way.
      Usually you should change the @link(Image) instead, if you want to change
      the image at runtime.

      But in special cases it may make sense to change the DrawableImage explicitly.
      E.g. you can change it's contents using fast image-to-image
      drawing using @link(TGLImage.DrawFrom).
      Or you can set a new instance of TGLImage here.
      Be sure to adjust @link(OwnsDrawableImage) as needed.

      If you set this property manually, to your own @link(TGLImage) instance:

      @unorderedList(
        @item(Make sure that @link(TGLImage.OwnsImage) is always @false.
          We depend on it, otherwise it would conflict with
          our own memory management controlled by @link(OwnsImage) and @link(Cache).)

        @item(The TGLImage.Image should be equal to our current @link(Image)
          (which can be @nil). Although this is not strictly necessary now.)

        @item(Note that various properties of this class are actually references
          to analogous properties of @link(TGLImage) or @link(TGLImageCore).
          E.g. getting/setting our
          @link(Rotation) just gets/sets @link(TGLImageCore.Rotation).
          So if you directly change the DrawableImage, you're also changing
          these properties.)
      )

      Note that when OpenGL(ES) context is lost and recreated (which can
      happen at any moment on mobile devices), the contents of this are
      reinitialized from @link(Image). This functionality is actually
      provided by the @link(TGLImage) itself. }
    property DrawableImage: TGLImage read FDrawableImage write SetDrawableImage;

    { Whether we should automatically free the @link(DrawableImage) instance.
      Note that this is restored to @true when we need to recreate the
      TGLImage internally, e.g. when @link(Image) instance changed
      (when even size changed, we have to recreate TGLImage). }
    property OwnsDrawableImage: boolean read FOwnsDrawableImage write FOwnsDrawableImage default true;

    { Color tint of the image. This simply multiplies the image RGBA components,
      just like @link(TGLImageCore.Color). By default this is opaque white,
      which means that image colors are unchanged. }
    property Color: TCastleColor read GetColor write SetColor;

    { Corners and edges of the image that are not stretched even
      when @link(Stretch) is used.
      See @link(TGLImageCore.Draw3x3) for the details how such drawing works. }
    property Corners: TVector4Integer read GetCorners write SetCorners;
      deprecated 'use ProtectedSides';

    { X coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterX: Single read GetCenterX write SetCenterX default 0.5;

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read GetCenterY write SetCenterY default 0.5;

    { Rotation in radians. Default value 0. }
    property Rotation: Single read GetRotation write SetRotation default 0;

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine). }
    property Clip: boolean read GetClip write SetClip;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The equation is calculated in the coordinates in which image
      X, Y spans from (0, 0) (bottom-left) to (1, 1) (top-right).
      For example ClipLine = (1, 0, -0.5) means that we reject pixels
      where @code(1 * x + 0 * y - 0.5 < 0). In other words,
      we reject pixels where @code(x < 0.5), so we reject the left half of the image. }
    property ClipLine: TVector3 read GetClipLine write SetClipLine;

    { Custom GLSL shading code.
      See TGLImage.CustomShader for explanation. }
    property CustomShader: TGLSLProgram read GetCustomShader write SetCustomShader;

    property Blending: boolean read GetBlending write SetBlending stored false;
      deprecated 'use AlphaChannel';
    property Proportional: boolean read GetProportional write SetProportional
      stored false default false;
      deprecated 'use ProportionalScaling';
    property FileName: string read FURL write SetURL;
      deprecated 'use URL';
  published
    { URL of the image. Setting this also sets @link(Image).
      Set this to '' to clear the image. }
    property URL: string read FURL write SetURL;

    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this to force specific treatment. }
    property AlphaChannel: TAutoAlphaChannel
      read GetAlphaChannel write SetAlphaChannel default acAuto;

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      See @link(TGLImageCore.SmoothScaling). }
    property SmoothScaling: boolean
      read GetSmoothScaling write SetSmoothScaling default true;

    { How does the loaded image size affect the size of the image control.

      @unorderedList(
        @item(
          When @link(FullSize) or @link(AutoSizeToChildren) is @true,
          then the value of @link(Stretch), and loaded image size, are ignored.
          When @link(AutoSizeToChildren),
          the image is always stretched to surround the children.
          When @link(FullSize),
          the image is always stretched to fill the parent.

          Otherwise:
        )

        @item(If Stretch = @false (default), then
          the displayed size corresponds to the underlying image size.)

        @item(If Stretch = @true, the image will be stretched to fill
          the requested area. The property
          @link(ProportionalScaling) determines how the image will
          be adjusted to fit the requested size
          (by @link(Width), @link(WidthFraction), @link(Height), @link(HeightFraction)).

          @definitionList(
            @itemLabel psNone
            @item(The image will be scaled to exactly fill
              the requested Width and Height
              (without paying attention to the aspect ratio of the image).

              This is the case when you fully force the displayed size
              and position, regardless of image size. Displayed image will
              always exactly fill the requested area.)

            @itemLabel psFit
            @item(The image will be proportionally scaled to fit within
              the requested Width and Height.
              If the aspect ratio of image
              will be different than aspect ratio of Width/Height, the scaled image
              will be centered inside the Width/Height.)

            @itemLabel psEnclose
            @item(The image will be proportionally scaled to enclode
              the requested Width and Height.
              If the aspect ratio of image
              will be different than aspect ratio of Width/Height, the scaled image
              will be larger then the requested area.)
          )
        )
      )

      @groupBegin }
    property Stretch: boolean read FStretch write SetStretch default false;
    property ProportionalScaling: TProportionalScaling
      read FProportionalScaling write SetProportionalScaling default psNone;
    { @groupEnd }

    { If @true, the images are loaded and freed using the cache.
      That is, the TEncodedImage in @link(Image) is loaded and freed using
      @link(TTexturesVideosCache.TextureImage_IncReference TextureCache.TextureImage_IncReference)
      and
      @link(TTexturesVideosCache.TextureImage_DecReference TextureCache.TextureImage_DecReference)
      instead of @link(LoadEncodedImage) and @code(FreeAndNil).

      This can save memory and loading time a lot, if you reuse the same
      image in many TCastleControlImage instances. The limitation is
      that you shouldn't change the @link(Image) contents
      (e.g. changing @link(TCastleImage.Colors)), or you will change
      all shared images.

      This is also useful to "preload" images before creating multiple
      complicated complicated UI using TCastleImageControl. Like this:

      @longCode(#
        CachedImage := TextureCache.TextureImage_IncReference('my_image.png');
        // now creating TCastleImageControl with this image
        // and cache will be very fast
        ImageControl := TCastleImageControl.Create(Application);
        ImageControl.Cache := true:
        ImageControl.URL := 'my_image.png'; // use already cached URL
        // somewhere much later
        TextureCache.TextureImage_DecReference(CachedImage);
      #)

      This also affects releasing your custom images,
      if you assign @link(Image) property (instead of @link(URL))
      and leave @link(OwnsImage) as @true.
      Be sure in such case that your image was loaded from cache too.

      Right now we don't cache and share the @link(TGLImage) contents,
      so the memory on GPU is not shared, but this may change at some point
      to also be shared.

      Right now, this should be set before assigning Image or URL,
      otherwise the image will not be loaded/freed using the cache.
      This may change at some point. }
    property Cache: boolean read FCache write FCache default false;

    property FlipHorizontal: Boolean read FFlipHorizontal write SetFlipHorizontal
      default false;
    property FlipVertical: Boolean read FFlipVertical write SetFlipVertical
      default false;

    { Corners and edges of the image that are protected from scaling distortion
      when @link(Stretch) is used.
      We use the 9-slicing algorithm,
      described at @link(TGLImageCore.Draw3x3)
      and https://en.wikipedia.org/wiki/9-slice_scaling to scale the image. }
    property ProtectedSides: TBorder read FProtectedSides;

    { Set this to auto-detect that image is scaled compared to it's desired size.
      This detects URL endings like 'xxx@2x.png', and we will automatically adjust
      to it (the size determined by AutoSize will be actually 2x smaller than
      the PNG size).
      Preparing images at a 2x or 4x larger size than "desired" is a useful trick
      when using UI scaling, when you want the image to look good at various scales. }
    property DetectScaleFromUrl: Boolean
      read FDetectScaleFromUrl write SetDetectScaleFromUrl default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleImageControl ---------------------------------------------------------------- }

constructor TCastleImageControl.Create(AOwner: TComponent);
begin
  inherited;
  FOwnsImage := true;
  FOwnsDrawableImage := true;
  FDrawableImage := TGLImage.Create(FImage, true { default SmoothScaling }, false);
  FProtectedSides := TBorder.Create(@ProtectedSidesChange);

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleImageControl.Destroy;
begin
  FreeImage;
  FreeDrawableImage;
  FreeAndNil(FProtectedSides);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

procedure TCastleImageControl.FreeImage;
begin
  if OwnsImage then
  begin
    if FImageLoadedFromCache then
      TextureCache.TextureImage_DecReference(FImage)
    else
      FreeAndNil(FImage);
  end else
    FImage := nil;
end;

procedure TCastleImageControl.FreeDrawableImage;
begin
  if OwnsDrawableImage then
    FreeAndNil(FDrawableImage)
  else
    FDrawableImage := nil;
end;

function TCastleImageControl.GetSmoothScaling: boolean;
begin
  Result := FDrawableImage.SmoothScaling;
end;

procedure TCastleImageControl.SetSmoothScaling(const Value: boolean);
begin
  if SmoothScaling <> Value then
  begin
    FDrawableImage.SmoothScaling := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetClip: boolean;
begin
  Result := FDrawableImage.Clip;
end;

procedure TCastleImageControl.SetClip(const Value: boolean);
begin
  if Clip <> Value then
  begin
    FDrawableImage.Clip := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetClipLine: TVector3;
begin
  Result := FDrawableImage.ClipLine;
end;

procedure TCastleImageControl.SetClipLine(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(ClipLine, Value) then
  begin
    FDrawableImage.ClipLine := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetURL(const Value: string);

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
    { We deliberately change URL even though loading image failed.
      This way URL property is always correctly deserialized in CGE editor,
      even when loading the image failed. }
    FURL := Value;
    FScaleFromUrl := GetScaleFromUrl;

    if Value <> '' then
    begin
      try
        if Cache then
          Image := TextureCache.TextureImage_IncReference(Value)
        else
          Image := LoadEncodedImage(Value);
      except
        { If loading design file failed, and we're inside CGE editor,
          merely report a warning. This allows deserializing in CGE editor
          designs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then
          begin
            WritelnWarning('TCastleImageControl', 'Failed to load image "%s": %s', [URIDisplay(Value), ExceptMessage(E)]);
            Image := nil;
            Exit;
          end else
            raise;
        end;
      end;
    end else
      Image := nil;
  end;
end;

function TCastleImageControl.GetCenterX: Single;
begin
  Result := FDrawableImage.CenterX;
end;

procedure TCastleImageControl.SetCenterX(const Value: Single);
begin
  if CenterX <> Value then
  begin
    FDrawableImage.CenterX := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetCenterY: Single;
begin
  Result := FDrawableImage.CenterY;
end;

procedure TCastleImageControl.SetCenterY(const Value: Single);
begin
  if CenterY <> Value then
  begin
    FDrawableImage.CenterY := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetRotation: Single;
begin
  Result := FDrawableImage.Rotation;
end;

procedure TCastleImageControl.SetRotation(const Value: Single);
begin
  if Rotation <> Value then
  begin
    FDrawableImage.Rotation := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetImage(Value: TEncodedImage);
begin
  if FImage <> Value then
  begin
    FreeImage;

    FImage := Value;
    { Note that we must change FImageLoadedFromCache *after* FreeImage,
      not before, as previous FImageLoadedFromCache is used in FreeImage. }
    FImageLoadedFromCache := (FImage <> nil) and Cache;

    ImageChanged;
  end else

  { Special situation occurs when Value = FImage,
    but Value comes from TextureCache.TextureImage_IncReference.
    We need to release reference then, otherwise doing this:

      ImageControl.Cache := true;
      ImageControl.URL := 'blah.png';
      ImageControl.URL := 'blah.png';

    .. would always cause memory leak. }
  if Cache and (Value <> nil) and OwnsImage then
    TextureCache.TextureImage_DecReference(Value);
end;

procedure TCastleImageControl.GLContextOpen;
begin;
  inherited;
  { Calling this is not strictly necessary. It means we load the OpenGL
    resources *now*, instead of on-demand right before 1st render. }
  FDrawableImage.PrepareResources;
end;

procedure TCastleImageControl.Render;
var
  SR: TFloatRectangle;
  ProtectedSidesVector: TVector4;
begin
  inherited;
  if (FDrawableImage.Width = 0) or (FDrawableImage.Height = 0) then Exit;
  SR := RenderRect;

  if not FProtectedSides.Exists then
  begin
    if FlipHorizontal or FlipVertical then
      FDrawableImage.DrawFlipped(SR, FlipHorizontal, FlipVertical)
    else
      FDrawableImage.Draw(SR);
  end else
  begin
    // TODO: FlipHorizontal, FlipVertical ignored in this case
    FDrawableImage.ScaleCorners := UIScale;
    ProtectedSidesVector := Vector4(
      ProtectedSides.TotalTop,
      ProtectedSides.TotalRight,
      ProtectedSides.TotalBottom,
      ProtectedSides.TotalLeft
    );
    FDrawableImage.Draw3x3(SR, ProtectedSidesVector);

    if CastleDesignMode and Stretch then
    begin
      if ((ProtectedSides.TotalRight + ProtectedSides.TotalLeft) * UIScale < SR.Width) and
         ((ProtectedSides.TotalTop + ProtectedSides.TotalBottom) * UIScale < SR.Height) then
      begin
        DrawPrimitive2D(pmLines, [
          // vertical lines
          Vector2(SR.Left + ProtectedSides.TotalLeft * UIScale, SR.Bottom),
          Vector2(SR.Left + ProtectedSides.TotalLeft * UIScale, SR.Top),
          Vector2(SR.Right - ProtectedSides.TotalRight * UIScale, SR.Bottom),
          Vector2(SR.Right - ProtectedSides.TotalRight * UIScale, SR.Top),
          // horizontal lines
          Vector2(SR.Left , SR.Bottom + ProtectedSides.TotalBottom * UIScale),
          Vector2(SR.Right, SR.Bottom + ProtectedSides.TotalBottom * UIScale),
          Vector2(SR.Left , SR.Top - ProtectedSides.TotalTop * UIScale),
          Vector2(SR.Right, SR.Top - ProtectedSides.TotalTop * UIScale)
        ], Vector4(1, 1, 0, 0.5));
      end;
    end;
  end;
end;

procedure TCastleImageControl.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  ImageWidth, ImageHeight: Single;
begin
  inherited;

  ImageWidth  := FDrawableImage.Width;
  ImageHeight := FDrawableImage.Height;
  if FDetectScaleFromUrl and (FScaleFromUrl <> 1) then
  begin
    ImageWidth  /= FScaleFromUrl;
    ImageHeight /= FScaleFromUrl;
  end;

  if not Stretch then
  begin
    PreferredWidth  := ImageWidth  * UIScale;
    PreferredHeight := ImageHeight * UIScale;
  end else
  if (ProportionalScaling in [psFit, psEnclose]) and
     (ImageWidth <> 0) and
     (ImageHeight <> 0) then
  begin
    if (ProportionalScaling = psFit) =
       (PreferredWidth / PreferredHeight >
        ImageWidth / ImageHeight) then
    begin
      PreferredWidth := ImageWidth * PreferredHeight / ImageHeight;
    end else
    begin
      PreferredHeight := ImageHeight * PreferredWidth / ImageWidth;
    end;
  end;
end;

procedure TCastleImageControl.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  if not Stretch then
  begin
    ResizeWidth := false;
    ResizeHeight := false;
    Reason := SAppendPart(Reason, NL, 'Turn on "TCastleImageControl.Stretch" to change size.');
  end;
end;

procedure TCastleImageControl.ImageChanged;
begin
  FDrawableImage.Load(FImage);
  VisibleChange([chRectangle]);
end;

function TCastleImageControl.GetAlphaChannel: TAutoAlphaChannel;
begin
  Result := FDrawableImage.Alpha;
end;

procedure TCastleImageControl.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  if AlphaChannel <> Value then
  begin
    FDrawableImage.Alpha := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetBlending: boolean;
begin
  Result := AlphaChannel <> acBlending;
end;

procedure TCastleImageControl.SetBlending(const Value: boolean);
begin
  if Value then
    AlphaChannel := acBlending
  else
    AlphaChannel := acTest;
end;

procedure TCastleImageControl.SetStretch(const Value: boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleImageControl.GetProportional: boolean;
begin
  Result := ProportionalScaling <> psNone;
end;

procedure TCastleImageControl.SetProportional(const Value: boolean);
begin
  if Value then
    ProportionalScaling := psFit
  else
    ProportionalScaling := psNone;
end;

procedure TCastleImageControl.SetProportionalScaling(const Value: TProportionalScaling);
begin
  if FProportionalScaling <> Value then
  begin
    FProportionalScaling := Value;
    VisibleChange([chRectangle]);
  end;
end;

function TCastleImageControl.GetColor: TCastleColor;
begin
  Result := FDrawableImage.Color;
end;

procedure TCastleImageControl.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(Color, Value) then
  begin
    FDrawableImage.Color := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetCustomShader: TGLSLProgram;
begin
  Result := FDrawableImage.CustomShader;
end;

procedure TCastleImageControl.SetCustomShader(const Value: TGLSLProgram);
begin
  if CustomShader <> Value then
  begin
    FDrawableImage.CustomShader := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetDrawableImage(const Value: TGLImage);
begin
  if FDrawableImage <> Value then
  begin
    FreeDrawableImage;
    FDrawableImage := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetFlipVertical(const Value: Boolean);
begin
  if FFlipVertical <> Value then
  begin
    FFlipVertical := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetFlipHorizontal(const Value: Boolean);
begin
  if FFlipHorizontal <> Value then
  begin
    FFlipHorizontal := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleImageControl.SetDetectScaleFromUrl(const Value: Boolean);
begin
  if FDetectScaleFromUrl <> Value then
  begin
    FDetectScaleFromUrl := Value;
    VisibleChange([chRender]);
  end;
end;

function TCastleImageControl.GetCorners: TVector4Integer;
begin
  Result := Vector4Integer(
    Round(ProtectedSides.TotalTop),
    Round(ProtectedSides.TotalRight),
    Round(ProtectedSides.TotalBottom),
    Round(ProtectedSides.TotalLeft)
  );
end;

procedure TCastleImageControl.SetCorners(const Value: TVector4Integer);
begin
  ProtectedSides.AllSides := 0;
  ProtectedSides.Top    := Value[0];
  ProtectedSides.Right  := Value[1];
  ProtectedSides.Bottom := Value[2];
  ProtectedSides.Left   := Value[3];
end;

procedure TCastleImageControl.ProtectedSidesChange(Sender: TObject);
begin
  VisibleChange([chRectangle]);
end;

function TCastleImageControl.PropertySection(
  const PropertyName: String): TPropertySection;
begin
  if (PropertyName = 'URL') or
     (PropertyName = 'Stretch') or
     (PropertyName = 'ProportionalScaling') or
     (PropertyName = 'ColorPersistent') then
    Result := psBasic
  else
    Result := inherited PropertySection(PropertyName);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
