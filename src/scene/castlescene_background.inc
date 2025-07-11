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
    FTextureNegativeZ, FTexturePositiveZ, FTextureNegativeX, FTexturePositiveX, FTextureNegativeY, FTexturePositiveY: String;
    FSmoothEquator: Single;
    FRotation: TVector4;

    procedure UpdateRenderer;
    procedure UpdateRendererOnceLoaded;
    procedure SetSkyTopColor(const Value: TCastleColorRGB);
    procedure SetSkyEquatorColor(const Value: TCastleColorRGB);
    procedure SetGroundBottomColor(const Value: TCastleColorRGB);
    procedure SetGroundEquatorColor(const Value: TCastleColorRGB);
    procedure SetTextureNegativeZ(const Value: String);
    procedure SetTexturePositiveZ(const Value: String);
    procedure SetTextureNegativeX(const Value: String);
    procedure SetTexturePositiveX(const Value: String);
    procedure SetTextureNegativeY(const Value: String);
    procedure SetTexturePositiveY(const Value: String);
    procedure SetSmoothEquator(const Value: Single);
    procedure SetRotation(const Value: TVector4);

    { Call before freeing the internal TCastleScene in FBackgroundRenderer
      that is done by UpdateRenderer.
      That scene is referenced by TX3DNode.Scene for our effect nodes,
      but destroying that scene is not setting TX3DNode.Scene to @nil,
      as documented in @link(TX3DNode.UnregisterScene).

      Testcase: Use examples/shaders/shader_effects_on_background,
      switch to color effect, then back to none.

      Without this fix, the EffectColor.Scene would be left pointing to the
      old TCastleScene, of old (destroyed at the beginning of
      UpdateRenderer) TBackgroundRenderer.
      And then the next EffectColorIntensity.Send call will crash,
      as TX3DNode(ParentNode).Scene.NextEventTime crashes,
      as TX3DNode(ParentNode).Scene is now a dangling pointer. }
    procedure UnregisterSceneFromEffects;
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

    { Rotate the background around any axis easily.
      By default this is (0, 1, 0, 0) and often you will want to just
      change the 4th rotation component, angle, to rotate around the Y axis.

      Set it like this:
      @longCode(#
      MyBackground.Rotation := Vector4(0, 1, 0, Pi / 4);
      #) }
    property Rotation: TVector4 read FRotation write SetRotation;

    { Renderer used by viewport. @exclude }
    property InternalBackgroundRenderer: TBackgroundRenderer read FBackgroundRenderer;

    { Build X3D node, used by TCastleViewport.InternalBuildNode.
      @exclude }
    function InternalBuildNode: TAbstractChildNode;

    { Add shader effects to configure how is the background rendered.
      See https://castle-engine.io/shaders for documentation
      how shader effects work in Castle Game Engine. }
    procedure SetEffects(const Value: array of TEffectNode);
  published
    { Back (-Z) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureNegativeZ: String read FTextureNegativeZ write SetTextureNegativeZ;

    { Front (+Z) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TexturePositiveZ: String read FTexturePositiveZ write SetTexturePositiveZ;

    { Top (+Y) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TexturePositiveY: String read FTexturePositiveY write SetTexturePositiveY;

    { Bottom (-Y) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureNegativeY: String read FTextureNegativeY write SetTextureNegativeY;

    { Left (-X) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TextureNegativeX: String read FTextureNegativeX write SetTextureNegativeX;

    { Right (+X) background texture.
      Orientation should match @url(https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD2/Part01/components/environmentalEffects.html#Background X3D Background node). }
    property TexturePositiveX: String read FTexturePositiveX write SetTexturePositiveX;

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
  FRotation := Vector4(0, 1, 0, 0);

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

  if FBackgroundNode <> nil then
    UnregisterSceneFromEffects;

  FreeAndNil(FBackgroundRenderer);
  FreeAndNil(FBackgroundNode);
  inherited;
end;

function TCastleBackground.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
      'SkyTopColorPersistent', 'SkyEquatorColorPersistent',
      'GroundBottomColorPersistent', 'GroundEquatorColorPersistent',
      'RotationPersistent', 'TextureNegativeZ', 'TexturePositiveZ',
      'TextureNegativeX', 'TexturePositiveX', 'TextureNegativeY',
      'TexturePositiveY', 'SmoothEquator'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleBackground.Loaded;
begin
  inherited;
  if FPendingUpdateRenderer then
    UpdateRenderer;
end;

procedure TCastleBackground.UnregisterSceneFromEffects;
var
  I: Integer;
begin
  for I := 0 to FBackgroundNode.FdEffects.Count - 1 do
    FBackgroundNode.FdEffects[I].UnregisterScene;
end;

procedure TCastleBackground.UpdateRenderer;
begin
  UnregisterSceneFromEffects;
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

  if TextureNegativeZ <> '' then
    FBackgroundNode.SetBackUrl([TextureNegativeZ])
  else
    FBackgroundNode.SetBackUrl([]);
  if TexturePositiveZ <> '' then
    FBackgroundNode.SetFrontUrl([TexturePositiveZ])
  else
    FBackgroundNode.SetFrontUrl([]);
  if TextureNegativeX <> '' then
    FBackgroundNode.SetLeftUrl([TextureNegativeX])
  else
    FBackgroundNode.SetLeftUrl([]);
  if TexturePositiveX <> '' then
    FBackgroundNode.SetRightUrl([TexturePositiveX])
  else
    FBackgroundNode.SetRightUrl([]);
  if TextureNegativeY <> '' then
    FBackgroundNode.SetBottomUrl([TextureNegativeY])
  else
    FBackgroundNode.SetBottomUrl([]);
  if TexturePositiveY <> '' then
    FBackgroundNode.SetTopUrl([TexturePositiveY])
  else
    FBackgroundNode.SetTopUrl([]);

  FBackgroundRenderer := CreateBackgroundRenderer(FBackgroundNode);
  FBackgroundRenderer.UpdateRotation(FRotation);
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

procedure TCastleBackground.SetTextureNegativeZ(const Value: String);
begin
  if FTextureNegativeZ <> Value then
  begin
    FTextureNegativeZ := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTexturePositiveZ(const Value: String);
begin
  if FTexturePositiveZ <> Value then
  begin
    FTexturePositiveZ := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureNegativeX(const Value: String);
begin
  if FTextureNegativeX <> Value then
  begin
    FTextureNegativeX := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTexturePositiveX(const Value: String);
begin
  if FTexturePositiveX <> Value then
  begin
    FTexturePositiveX := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTextureNegativeY(const Value: String);
begin
  if FTextureNegativeY <> Value then
  begin
    FTextureNegativeY := Value;
    UpdateRendererOnceLoaded;
  end;
end;

procedure TCastleBackground.SetTexturePositiveY(const Value: String);
begin
  if FTexturePositiveY <> Value then
  begin
    FTexturePositiveY := Value;
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

procedure TCastleBackground.SetRotation(const Value: TVector4);
begin
  if not TVector4.PerfectlyEquals(FRotation, Value) then
  begin
    FRotation := Value;
    if FBackgroundRenderer <> nil then
      FBackgroundRenderer.UpdateRotation(Value);
  end;
end;

function TCastleBackground.InternalBuildNode: TAbstractChildNode;
var
  Transform: TTransformNode;
begin
  Transform := TTransformNode.Create;
  Transform.Rotation := Rotation;
  Transform.AddChildren(FBackgroundNode.DeepCopy as TAbstractChildNode);
  Result := Transform;
end;

procedure TCastleBackground.SetEffects(const Value: array of TEffectNode);
begin
  { Call UnregisterSceneFromEffects to set TX3DNode.Scene := nil
    on old effect nodes. We cannot wait for UpdateRenderer to do it,
    as that happens after FBackgroundNode.SetEffects,
    so we would not fix old effects. }
  UnregisterSceneFromEffects;

  FBackgroundNode.SetEffects(Value);
  UpdateRendererOnceLoaded;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastlebackground_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
