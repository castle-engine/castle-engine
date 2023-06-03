{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

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
    on loaded image alpha channel (see @link(TDrawableImage.Alpha)).
    You can influence this by @link(AlphaChannel) property. }
  TCastleImageControl = class(TCastleUserInterface)
  strict private
    FStretch: boolean;
    FProportionalScaling: TProportionalScaling;
    FDetectScaleFromUrl: Boolean;
    FContent: TCastleImagePersistent;

    { Properties simply synchronized with the same properties of FContent. }
    function GetURL: String;
    function GetDrawableImage: TDrawableImage;
    function GetOwnsDrawableImage: Boolean;
    function GetImage: TEncodedImage;
    function GetOwnsImage: Boolean;
    function GetRotation: Single;
    function GetRotationCenter: TVector2;
    function GetAlphaChannel: TAutoAlphaChannel;
    function GetColor: TCastleColor;
    function GetClip: boolean;
    function GetClipLine: TVector3;
    function GetCustomShader: TGLSLProgram;
    function GetFlipVertical: Boolean;
    function GetFlipHorizontal: Boolean;
    function GetSmoothScaling: Boolean;
    function GetCache: Boolean;
    function GetProtectedSides: TBorder;
    function GetRegion: TFloatRectangle;
    procedure SetURL(const Value: String);
    procedure SetDrawableImage(const Value: TDrawableImage);
    procedure SetOwnsDrawableImage(const Value: Boolean);
    procedure SetImage(const Value: TEncodedImage);
    procedure SetOwnsImage(const Value: Boolean);
    procedure SetRotation(const Value: Single);
    procedure SetRotationCenter(const Value: TVector2);
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
    procedure SetColor(const Value: TCastleColor);
    procedure SetClip(const Value: boolean);
    procedure SetClipLine(const Value: TVector3);
    procedure SetRegion(const Value: TFloatRectangle);
    procedure SetCustomShader(const Value: TGLSLProgram);
    procedure SetFlipVertical(const Value: Boolean);
    procedure SetFlipHorizontal(const Value: Boolean);
    procedure SetSmoothScaling(const Value: boolean);
    procedure SetCache(const Value: boolean);

    { Properties maintained for backward compatibility. }
    function GetCenterX: Single;
    function GetCenterY: Single;
    procedure SetCenterX(const Value: Single);
    procedure SetCenterY(const Value: Single);
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
    function GetCorners: TVector4Integer;
    procedure SetCorners(const Value: TVector4Integer);

    procedure SetStretch(const Value: boolean);
    procedure SetProportionalScaling(const Value: TProportionalScaling);
    function GetProportional: boolean;
    procedure SetProportional(const Value: boolean);
    procedure SetDetectScaleFromUrl(const Value: Boolean);

    procedure ContentChanged(Sender: TObject);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    procedure GLContextOpen; override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

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
      uses this TCastleImagePersistent, and TCastleImageControl.Stretch=false).

      @seealso TCastleImagePersistent.Image }
    property Image: TEncodedImage read GetImage write SetImage;

    { Whether the memory management of assigned @link(Image) is automatic.
      See @link(Image) documentation for details.

      Note that setting @link(URL) changes @link(Image),
      and changes OwnsImage unconditionally to @true.

      Note that setting @link(DrawableImage) changes @link(Image) too,
      and changes OwnsImage to the value present in @link(TDrawableImage.OwnsImage).

      @seealso TCastleImagePersistent.OwnsImage }
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
      reinitialized from @link(Image).

      @seealso TCastleImagePersistent.DrawableImage }
    property DrawableImage: TDrawableImage read GetDrawableImage write SetDrawableImage;

    { Whether we should automatically free the @link(DrawableImage) instance.
      Note that this is restored to @true when we need to recreate the
      TDrawableImage internally, e.g. when @link(Image) instance changed
      (when even size changed, we have to recreate TDrawableImage).

      @seealso TCastleImagePersistent.OwnsDrawableImage }
    property OwnsDrawableImage: boolean read GetOwnsDrawableImage write SetOwnsDrawableImage default true;

    { If you modified the @link(Image) contents
      (colors, or maybe even image size) you need to call this.

      @seealso TCastleImagePersistent.ImageChanged }
    procedure ImageChanged;

    { Color tint of the image. This simply multiplies the image RGBA components,
      just like @link(TDrawableImage.Color). By default this is opaque white,
      which means that image colors are unchanged.

      @seealso TCastleImagePersistent.Color }
    property Color: TCastleColor read GetColor write SetColor;

    {$ifdef FPC}
    { Corners and edges of the image that are not stretched even
      when @link(Stretch) is used.
      See @link(TDrawableImage.Draw3x3) for the details how such drawing works. }
    property Corners: TVector4Integer read GetCorners write SetCorners;
      deprecated 'use ProtectedSides';

    { X coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterX: Single read GetCenterX write SetCenterX default 0.5;
      deprecated 'use RotationCenter';

    { Y coordinate of the center of rotation. Value from 0 to 1. Default value 0.5. }
    property CenterY: Single read GetCenterY write SetCenterY default 0.5;
      deprecated 'use RotationCenter';
    {$endif}

    { Center of rotation.
      Expressed as a fraction within the drawn ScreenRectangle,
      (0,0) means bottom-left corner, (1,1) means top-right corner.
      Default (0.5,0.5).

      @seealso TCastleImagePersistent.RotationCenter }
    property RotationCenter: TVector2 read GetRotationCenter write SetRotationCenter;

    { If @link(Clip), this is the line equation used to determine whether
      we clip the given pixel. Given a line (A, B, C) and pixel (x, y),
      the pixel is clipped (rejected) if @code(A * x + B * y + C < 0).

      The equation is calculated in the coordinates in which image
      X, Y spans from (0, 0) (bottom-left) to (1, 1) (top-right).
      For example ClipLine = (1, 0, -0.5) means that we reject pixels
      where @code(1 * x + 0 * y - 0.5 < 0). In other words,
      we reject pixels where @code(x < 0.5), so we reject the left half of the image.

      @seealso TCastleImagePersistent.ClipLine }
    property ClipLine: TVector3 read GetClipLine write SetClipLine;

    { Image region to which we should limit the display.
      Empty (following @link(TFloatRectangle.Empty)) means using the whole image. }
    property Region: TFloatRectangle read GetRegion write SetRegion;

    { Custom GLSL shading code.
      See TDrawableImage.CustomShader for explanation.

      @seealso TCastleImagePersistent.CustomShader }
    property CustomShader: TGLSLProgram read GetCustomShader write SetCustomShader;

    {$ifdef FPC}
    property Blending: boolean read GetBlending write SetBlending stored false;
      deprecated 'use AlphaChannel';
    property Proportional: boolean read GetProportional write SetProportional stored false default false;
      deprecated 'use ProportionalScaling';
    property FileName: string read GetURL write SetURL stored false;
      deprecated 'use URL';
    {$endif}
  published
    { Image contents to display. }
    property Content: TCastleImagePersistent read FContent;

    { URL of the image.
      Set this to load a new image, you can set to '' to clear the image.

      @seealso TCastleImagePersistent.URL }
    property URL: string read GetURL write SetURL stored false;

    { How to treat alpha channel of the assigned image.
      By default, this is acAuto, which means that image contents
      together with current @link(Color) determine how
      the alpha of image is treated (opaque, alpha test, alpha blending).
      Set this to force specific treatment.

      @seealso TCastleImagePersistent.AlphaChannel }
    property AlphaChannel: TAutoAlphaChannel
      read GetAlphaChannel write SetAlphaChannel stored false default acAuto;

    { Is the image scaling mode smooth (bilinear filtering)
      or not (nearest-pixel filtering).
      See @link(TDrawableImage.SmoothScaling).

      @seealso TCastleImagePersistent.SmoothScaling }
    property SmoothScaling: boolean
      read GetSmoothScaling write SetSmoothScaling stored false default true;

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

    { If @true, the image is loaded and freed using a cache.
      This can save memory and loading time a lot, if you reuse the same
      URL in many TCastleImageControl instances.

      @seealso TCastleImagePersistent.Cache }
    property Cache: boolean read GetCache write SetCache stored false default true;

    { Display image horizontally flipped.
      @seealso TCastleImagePersistent.FlipHorizontal }
    property FlipHorizontal: Boolean read GetFlipHorizontal write SetFlipHorizontal
      stored false default false;

    { Display image vertically flipped.
      @seealso TCastleImagePersistent.FlipVertical }
    property FlipVertical: Boolean read GetFlipVertical write SetFlipVertical
      stored false default false;

    { Corners and edges of the image that are protected from scaling distortion
      when @link(Stretch) is used.
      We use the 9-slicing algorithm,
      described at @link(TDrawableImage.Draw3x3)
      and https://en.wikipedia.org/wiki/9-slice_scaling to scale the image.

      @seealso TCastleImagePersistent.ProtectedSides }
    property ProtectedSides: TBorder read GetProtectedSides stored false;

    { Set this to auto-detect that image is scaled compared to it's desired size.
      This detects URL endings like 'xxx@2x.png', and we will automatically adjust
      to it (the size determined by AutoSize will be actually 2x smaller than
      the PNG size).
      Preparing images at a 2x or 4x larger size than "desired" is a useful trick
      when using UI scaling, when you want the image to look good at various scales. }
    property DetectScaleFromUrl: Boolean
      read FDetectScaleFromUrl write SetDetectScaleFromUrl default false;

    { Rotation in radians. Default value 0.
      @seealso TCastleImagePersistent.Rotation }
    property Rotation: Single read GetRotation write SetRotation stored false {$ifdef FPC}default 0{$endif};

    { Clip the image by an arbitrary 2D line defined in @link(ClipLine).
      @seealso TCastleImagePersistent.Clip }
    property Clip: boolean read GetClip write SetClip stored false default false;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleImageControl -------------------------------------------------------- }

constructor TCastleImageControl.Create(AOwner: TComponent);
begin
  inherited;
  FContent := TCastleImagePersistent.Create;
  FContent.OnChange := {$ifdef FPC}@{$endif}ContentChanged;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleImageControl.Destroy;
begin
  FreeAndNil(FContent);

  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleImageControl.GetProtectedSides: TBorder;
begin
  Result := FContent.ProtectedSides;
end;

function TCastleImageControl.GetRegion: TFloatRectangle;
begin
  Result := FContent.Region;
end;

function TCastleImageControl.GetSmoothScaling: Boolean;
begin
  Result := FContent.SmoothScaling;
end;

procedure TCastleImageControl.SetSmoothScaling(const Value: boolean);
begin
  FContent.SmoothScaling := Value;
end;

function TCastleImageControl.GetClip: boolean;
begin
  Result := FContent.Clip;
end;

procedure TCastleImageControl.SetClip(const Value: boolean);
begin
  FContent.Clip := Value;
end;

function TCastleImageControl.GetClipLine: TVector3;
begin
  Result := FContent.ClipLine;
end;

procedure TCastleImageControl.SetClipLine(const Value: TVector3);
begin
  FContent.ClipLine := Value;
end;

procedure TCastleImageControl.SetRegion(const Value: TFloatRectangle);
begin
  FContent.Region := Value;
end;

function TCastleImageControl.GetRotation: Single;
begin
  Result := FContent.Rotation;
end;

procedure TCastleImageControl.SetRotation(const Value: Single);
begin
  FContent.Rotation := Value;
end;

function TCastleImageControl.GetRotationCenter: TVector2;
begin
  Result := FContent.RotationCenter;
end;

procedure TCastleImageControl.SetRotationCenter(const Value: TVector2);
begin
  FContent.RotationCenter := Value;
end;

procedure TCastleImageControl.GLContextOpen;
begin;
  inherited;
  { Calling this is not strictly necessary. It means we load the OpenGL
    resources *now*, instead of on-demand right before 1st render. }
  FContent.DrawableImage.PrepareResources;
end;

procedure TCastleImageControl.Render;
begin
  inherited;
  FContent.DrawUiBegin(UIScale);
  FContent.Draw(RenderRect);
  FContent.DrawUiEnd;
end;

procedure TCastleImageControl.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  ImageWidth, ImageHeight: Single;
begin
  inherited;

  ImageWidth  := FContent.Width;
  ImageHeight := FContent.Height;
  if FDetectScaleFromUrl and (FContent.ScaleFromUrl <> 1) then
  begin
    ImageWidth  := ImageWidth / FContent.ScaleFromUrl;
    ImageHeight := ImageHeight / FContent.ScaleFromUrl;
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

procedure TCastleImageControl.ContentChanged(Sender: TObject);
begin
  VisibleChange([chRectangle]);
end;

procedure TCastleImageControl.ImageChanged;
begin
  FContent.ImageChanged;
end;

function TCastleImageControl.GetAlphaChannel: TAutoAlphaChannel;
begin
  Result := FContent.AlphaChannel;
end;

procedure TCastleImageControl.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  FContent.AlphaChannel := Value;
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

function TCastleImageControl.GetURL: String;
begin
  Result := FContent.URL;
end;

procedure TCastleImageControl.SetURL(const Value: String);
begin
  FContent.URL := Value;
end;

function TCastleImageControl.GetColor: TCastleColor;
begin
  Result := FContent.Color;
end;

procedure TCastleImageControl.SetColor(const Value: TCastleColor);
begin
  FContent.Color := Value;
end;

function TCastleImageControl.GetCustomShader: TGLSLProgram;
begin
  Result := FContent.CustomShader;
end;

procedure TCastleImageControl.SetCustomShader(const Value: TGLSLProgram);
begin
  FContent.CustomShader := Value;
end;

function TCastleImageControl.GetDrawableImage: TDrawableImage;
begin
  Result := FContent.DrawableImage;
end;

procedure TCastleImageControl.SetDrawableImage(const Value: TDrawableImage);
begin
  FContent.DrawableImage := Value;
end;

function TCastleImageControl.GetOwnsDrawableImage: Boolean;
begin
  Result := FContent.OwnsDrawableImage;
end;

procedure TCastleImageControl.SetOwnsDrawableImage(const Value: Boolean);
begin
  FContent.OwnsDrawableImage := Value;
end;

function TCastleImageControl.GetImage: TEncodedImage;
begin
  Result := FContent.Image;
end;

procedure TCastleImageControl.SetImage(const Value: TEncodedImage);
begin
  FContent.Image := Value;
end;

function TCastleImageControl.GetOwnsImage: Boolean;
begin
  Result := FContent.OwnsImage;
end;

procedure TCastleImageControl.SetOwnsImage(const Value: Boolean);
begin
  FContent.OwnsImage := Value;
end;

function TCastleImageControl.GetFlipVertical: Boolean;
begin
  Result := FContent.FlipVertical;
end;

procedure TCastleImageControl.SetFlipVertical(const Value: Boolean);
begin
  FContent.FlipVertical := Value;
end;

function TCastleImageControl.GetFlipHorizontal: Boolean;
begin
  Result := FContent.FlipHorizontal;
end;

procedure TCastleImageControl.SetFlipHorizontal(const Value: Boolean);
begin
  FContent.FlipHorizontal := Value;
end;

function TCastleImageControl.GetCache: Boolean;
begin
  Result := FContent.Cache;
end;

procedure TCastleImageControl.SetCache(const Value: boolean);
begin
  FContent.Cache := Value;
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

function TCastleImageControl.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, ['URL', 'Stretch', 'ProportionalScaling',
      'ColPersistent', 'SmoothScaling', 'AlphaChannel', 'FlipHizontal',
      'FlipVertical', 'ProtectedSides', 'RegionPersistent',
      'Rotation', 'Content'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleImageControl.GetCenterX: Single;
begin
  Result := RotationCenter.X;
end;

function TCastleImageControl.GetCenterY: Single;
begin
  Result := RotationCenter.Y;
end;

procedure TCastleImageControl.SetCenterX(const Value: Single);
var
  V: TVector2;
begin
  V := RotationCenter;
  V.X := Value;
  RotationCenter := V;
end;

procedure TCastleImageControl.SetCenterY(const Value: Single);
var
  V: TVector2;
begin
  V := RotationCenter;
  V.Y := Value;
  RotationCenter := V;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleimagecontrol_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
