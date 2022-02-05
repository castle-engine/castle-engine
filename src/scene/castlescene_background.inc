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

{$ifdef read_interface}

type
  { Background, a skybox with a color gradient to represent sky and ground behind.

    See https://en.wikipedia.org/wiki/Cube_mapping#Skyboxes .

    You can find numerous free skyboxes e.g. on
    https://opengameart.org/art-search?keys=skybox and
    http://www.humus.name/index.php?page=Textures . }
  TCastleBackground = class(TCastleComponent)
  strict private
    FBackgroundNode: TBackgroundNode;
    FBackgroundRenderer: TBackgroundRenderer;
    FPendingUpdateRenderer: Boolean; // used by Loaded

    { private fields reflecting public properties }
    FSkyTopColor: TCastleColorRGB;
    FSkyEquatorColor: TCastleColorRGB;
    FGroundBottomColor: TCastleColorRGB;
    FGroundEquatorColor: TCastleColorRGB;
    FTextureBack, FTextureFront, FTextureLeft, FTextureRight, FTextureBottom, FTextureTop: String;
    FSmoothEquator: Single;

    procedure UpdateRenderer;
    procedure UpdateRendererOnceLoaded;
    procedure SetSkyTopColor(const Value: TCastleColorRGB);
    procedure SetSkyEquatorColor(const Value: TCastleColorRGB);
    procedure SetGroundBottomColor(const Value: TCastleColorRGB);
    procedure SetGroundEquatorColor(const Value: TCastleColorRGB);
    procedure SetTextureBack(const Value: String);
    procedure SetTextureFront(const Value: String);
    procedure SetTextureLeft(const Value: String);
    procedure SetTextureRight(const Value: String);
    procedure SetTextureBottom(const Value: String);
    procedure SetTextureTop(const Value: String);
    procedure SetSmoothEquator(const Value: Single);
  protected
    procedure Loaded; override;
  public
    const
      DefaultSmoothEquator = 0.025;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Sky color in the zenith.
      SkyTopColor and SkyEquatorColor are displayed
      as a gradient on the upper hemisphere around camera. }
    property SkyTopColor: TCastleColorRGB read FSkyTopColor write SetSkyTopColor;

    { Sky color at the equator.
      SkyTopColor and SkyEquatorColor are displayed
      as a gradient on the upper hemisphere around camera. }
    property SkyEquatorColor: TCastleColorRGB read FSkyEquatorColor write SetSkyEquatorColor;

    { Ground color at nadir (direction looking straight down).
      GroundBottomColor and GroundEquatorColor are displayed
      as a gradient on lower hemisphere around camera. }
    property GroundBottomColor: TCastleColorRGB read FGroundBottomColor write SetGroundBottomColor;

    { Ground color at equator.
      GroundBottomColor and GroundEquatorColor are displayed
      as a gradient on lower hemisphere around camera. }
    property GroundEquatorColor: TCastleColorRGB read FGroundEquatorColor write SetGroundEquatorColor;

    { Renderer used by viewport. @exclude }
    property InternalBackgroundRenderer: TBackgroundRenderer read FBackgroundRenderer;
  published
    { Back background texture.
      Displayed on -Z skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureBack: String read FTextureBack write SetTextureBack;

    { Front background texture.
      Displayed on +Z skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureFront: String read FTextureFront write SetTextureFront;

    { Top background texture.
      Displayed on +Y skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureTop: String read FTextureTop write SetTextureTop;

    { Bottom background texture.
      Displayed on -Y skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureBottom: String read FTextureBottom write SetTextureBottom;

    { Left background texture.
      Displayed on -X skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureLeft: String read FTextureLeft write SetTextureLeft;

    { Right background texture.
      Displayed on +X skybox face.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureRight: String read FTextureRight write SetTextureRight;

    { Make color transition from sky to ground less abrupt, adding a gradient
      from GroundEquatorColor to SkyEquatorColor.
      This property determines gradient size, in radians -- angle along the sky sphere. }
    property SmoothEquator: Single read FSmoothEquator write SetSmoothEquator {$ifdef FPC}default DefaultSmoothEquator{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastlebackground_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleBackground ---------------------------------------------------------------- }

constructor TCastleBackground.Create(AOwner: TComponent);
begin
  inherited;

  { set default values }
  FSkyTopColor := Vector3(0.33, 0.33, 1.0);
  FSkyEquatorColor := WhiteRGB;
  FGroundBottomColor := BlackRGB;
  FGroundEquatorColor := GrayRGB;
  FSmoothEquator := DefaultSmoothEquator;

  FBackgroundNode := TBackgroundNode.Create;

  UpdateRenderer;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastlebackground_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleBackground.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastlebackground_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  FreeAndNil(FBackgroundRenderer);
  FreeAndNil(FBackgroundNode);
  inherited;
end;

function TCastleBackground.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'SkyTopColorPersistent') or
     (PropertyName = 'SkyEquatorColorPersistent') or
     (PropertyName = 'GroundBottomColorPersistent') or
     (PropertyName = 'GroundEquatorColorPersistent') or
     (PropertyName = 'TextureBack') or
     (PropertyName = 'TextureFront') or
     (PropertyName = 'TextureLeft') or
     (PropertyName = 'TextureRight') or
     (PropertyName = 'TextureBottom') or
     (PropertyName = 'TextureTop') or
     (PropertyName = 'SmoothEquator') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleBackground.Loaded;
begin
  inherited; //< important, as inherited removes csLoading from ComponentState
  if FPendingUpdateRenderer then
    UpdateRenderer;
end;

procedure TCastleBackground.UpdateRenderer;
begin
  FreeAndNil(FBackgroundRenderer);

  if FSmoothEquator <= 0 then
  begin
    FBackgroundNode.SetSkyColor([FSkyTopColor, FSkyEquatorColor]);
    FBackgroundNode.SetGroundColor([FGroundBottomColor, FGroundEquatorColor]);
    FBackgroundNode.SetSkyAngle([ Pi / 2 ]);
    FBackgroundNode.SetGroundAngle([ Pi / 2 ]);
  end else
  begin
    FBackgroundNode.SetSkyColor([FSkyTopColor, FSkyEquatorColor]);
    FBackgroundNode.SetGroundColor([FGroundBottomColor, FGroundEquatorColor, FSkyEquatorColor]);
    FBackgroundNode.SetSkyAngle([ Pi / 2 - FSmoothEquator / 2 ]);
    FBackgroundNode.SetGroundAngle([
      Pi / 2 - FSmoothEquator / 2,
      Pi / 2 + FSmoothEquator / 2
    ]);
  end;

  if TextureBack <> '' then
    FBackgroundNode.SetBackUrl([TextureBack])
  else
    FBackgroundNode.SetBackUrl([]);
  if TextureFront <> '' then
    FBackgroundNode.SetFrontUrl([TextureFront])
  else
    FBackgroundNode.SetFrontUrl([]);
  if TextureLeft <> '' then
    FBackgroundNode.SetLeftUrl([TextureLeft])
  else
    FBackgroundNode.SetLeftUrl([]);
  if TextureRight <> '' then
    FBackgroundNode.SetRightUrl([TextureRight])
  else
    FBackgroundNode.SetRightUrl([]);
  if TextureBottom <> '' then
    FBackgroundNode.SetBottomUrl([TextureBottom])
  else
    FBackgroundNode.SetBottomUrl([]);
  if TextureTop <> '' then
    FBackgroundNode.SetTopUrl([TextureTop])
  else
    FBackgroundNode.SetTopUrl([]);

  FBackgroundRenderer := CreateBackgroundRenderer(FBackgroundNode);
  FPendingUpdateRenderer := false;
end;

procedure TCastleBackground.UpdateRendererOnceLoaded;
begin
  if IsLoading then
    FPendingUpdateRenderer := true
  else
    UpdateRenderer;
end;

procedure TCastleBackground.SetSkyTopColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FSkyTopColor, Value) then
  begin
    FSkyTopColor := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetSkyEquatorColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FSkyEquatorColor, Value) then
  begin
    FSkyEquatorColor := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetGroundBottomColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FGroundBottomColor, Value) then
  begin
    FGroundBottomColor := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetGroundEquatorColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FGroundEquatorColor, Value) then
  begin
    FGroundEquatorColor := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureBack(const Value: String);
begin
  if FTextureBack <> Value then
  begin
    FTextureBack := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureFront(const Value: String);
begin
  if FTextureFront <> Value then
  begin
    FTextureFront := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureLeft(const Value: String);
begin
  if FTextureLeft <> Value then
  begin
    FTextureLeft := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureRight(const Value: String);
begin
  if FTextureRight <> Value then
  begin
    FTextureRight := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureBottom(const Value: String);
begin
  if FTextureBottom <> Value then
  begin
    FTextureBottom := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureTop(const Value: String);
begin
  if FTextureTop <> Value then
  begin
    FTextureTop := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetSmoothEquator(const Value: Single);
begin
  if FSmoothEquator <> Value then
  begin
    FSmoothEquator := Value;
    UpdateRendererOnceLoaded;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlebackground_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
