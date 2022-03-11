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

{ Directional light. }

{$ifdef read_interface}

type
  { Directional light shines along a direction in 3D space, simulating
    a light source far away (like a sun).
    The direction is -Z (in local coordinates), simply rotate this
    transformation to change the lighting direction.
    The position of this transformation doesn't matter for lighting calculation
    (though it is useful to display the light gizmo at design-time).

    See @url(https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/lighting.html#DirectionalLight
    X3D DirectionalLight specification) for various details. }
  TCastleDirectionalLight = class(TCastleAbstractLight)
  strict private
    { Same thing as FLightNode, but with proper type. }
    FDirectionalLight: TDirectionalLightNode;

    FColor: TCastleColorRGB;
    FIntensity: Single;

    procedure SetColor(const Value: TCastleColorRGB);
    procedure SetIntensity(const Value: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Light color shining over surface. By default white. }
    property Color: TCastleColorRGB read FColor write SetColor;
  published
    { Intensity, multiplied by color. Can be anything >= 0. }
    property Intensity: Single read FIntensity write SetIntensity {$ifdef FPC}default 1.0{$endif};

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastledirectionallight_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleDirectionalLight.Create(AOwner: TComponent);

  function BuildNode: TX3DRootNode;
  const
    { Length of gizmo line along Z }
    D = 10.0;
  var
    Coord: TCoordinateNode;
    LineSet: TLineSetNode;
    Material: TUnlitMaterialNode;
    Appearance: TAppearanceNode;
    Shape: TShapeNode;
  begin
    Coord := TCoordinateNode.Create;
    Coord.SetPoint([
      Vector3(0, 0, 0),
      FDirectionalLight.Direction * D
    ]);

    LineSet := TLineSetNode.Create;
    LineSet.Coord := Coord;
    LineSet.SetVertexCount([2]);

    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := HexToColorRGB('FC9C2D');

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Geometry := LineSet;
    Shape.Appearance := Appearance;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
  end;

begin
  inherited;

  FIntensity := 1;
  FColor := WhiteRGB;

  { internal stuff create }
  FDirectionalLight := TDirectionalLightNode.Create;
  FDirectionalLight.Global := true;
  LightNode := FDirectionalLight;

  if Gizmo then
  begin
    GizmoTextureUrl := InternalCastleDesignData + 'gizmos/light/light_directional.png';
    GizmoScene.Load(BuildNode, true);
  end;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastledirectionallight_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleDirectionalLight.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastledirectionallight_persistent_vectors.inc}
  {$undef read_implementation_destructor}
  inherited;
end;

function TCastleDirectionalLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') or
     (PropertyName = 'Intensity') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleDirectionalLight.SetColor(const Value: TCastleColorRGB);
begin
  if not TCastleColorRGB.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    FDirectionalLight.Color := Value;
  end;
end;

procedure TCastleDirectionalLight.SetIntensity(const Value: Single);
begin
  if FIntensity <> Value then
  begin
    FIntensity := Value;
    FDirectionalLight.Intensity := Value;
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastledirectionallight_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
