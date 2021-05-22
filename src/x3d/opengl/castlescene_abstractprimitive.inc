{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Primitives that can be easily added and adjusted inside TCastleViewport. }

{$ifdef read_interface}

type
  TPrimitiveMaterial = (pmPhysical, pmPhong, pmUnlit);

  { Base class to express primitives that can be easily added and
    adjusted inside TCastleViewport.

    Such primitives can be anything visible that can be expressed using X3D nodes
    (like boxes or spheres).
    Particular descendants of this class, like TCastleBox or TCastleSphere,
    define a particular primitive.
    Note: for things not directly visible (like lights) descending from this class
    is not useful. Instead descend from TCastleTransform.

    Using this class is somewhat similar to using TCastleScene,
    and loading (using @link(TCastleScene.Load)) an X3D nodes graph
    with appropriate primitives. In fact, that is exactly what happens internally.
    But using this class is simpler (it hides X3D nodes from you, although it also
    hides some possibilities) and you can customize such primitives
    using the CGE editor. }
  TCastleAbstractPrimitive = class(TCastleTransform)
  strict private
    FScene: TCastleScene;
    FRootNode: TX3DRootNode;
    FAppearanceNode: TAppearanceNode;
    FShapeNode: TShapeNode;
    FColor: TCastleColor;
    FMaterial: TPrimitiveMaterial;
    FTexture: String;
    procedure SetMaterial(const Value: TPrimitiveMaterial);
    procedure SetColor(const Value: TCastleColor);
    procedure SetTexture(const Value: String);
    { Reflect current FColor, FMaterial, FTexture values in X3D nodes. }
    procedure UpdateMaterialNode;
  protected
    { Descendants should add primitive X3D geometry node here. }
    property ShapeNode: TShapeNode read FShapeNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { Color of the text. Opaque white by default.

      What exactly it means, depends on @link(Material):
      @underedList(
        @item(When Material is pmPhysical, then Color determines TPhysicalMaterialNode.BaseColor and TPhysicalMaterialNode.Transparency.)
        @item(When Material is pmPhong, then Color determines TMaterialNode.DiffuseColor and TMaterialNode.Transparency.)
        @item(When Material is pmUnlit, then Color determines TUnlitMaterialNode.EmissiveColor and TUnlitMaterialNode.Transparency.)
      )
    }
    property Color: TCastleColor read FColor write SetColor;
  published
    { Material type (which determines lighting calculation) for this primitive. }
    property Material: TPrimitiveMaterial read FMaterial write SetMaterial default pmPhysical;

    { Texture URL.

      The way texture interacts with lighting depends on the @link(Material) type:
      @underedList(
        @item(When Material is pmPhysical, then Texture determines TPhysicalMaterialNode.BaseTexture.)
        @item(When Material is pmPhong, then Texture determines TMaterialNode.DiffuseTexture.)
        @item(When Material is pmUnlit, then Texture determines TUnlitMaterialNode.EmissiveTexture.)
      )

      The texture mapping is done automatically, following a reasonable algorithm
      for the given primitive described in the X3D specification.
      See https://castle-engine.io/x3d_implementation_geometry3d.php for links to X3D specification.
      For example,
      https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-CD/Part01/components/geometry3D.html#Box
      describes how texture is applied to the 6 sides of the box. }
    property Texture: String read FTexture write SetTexture;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleAbstractPrimitive.Create(AOwner: TComponent);
begin
  inherited;

  { internal stuff create }
  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  Add(FScene);

  { build X3D nodes }
  FAppearanceNode := TAppearanceNode.Create;
  FShapeNode := TShapeNode.Create;
  FShapeNode.Appearance := FAppearanceNode;
  FRootNode := TX3DRootNode.Create;
  FRootNode.AddChildren(ShapeNode);
  FScene.Load(FRootNode, true);

  FMaterial := pmPhysical;
  FColor := White;
  UpdateMaterialNode;

  {$define read_implementation_constructor}
  {$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
  {$undef read_implementation_constructor}
end;

destructor TCastleAbstractPrimitive.Destroy;
begin
  {$define read_implementation_destructor}
  {$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
  {$undef read_implementation_destructor}

  FreeAndNil(FScene);
  inherited;
end;

procedure TCastleAbstractPrimitive.UpdateMaterialNode;

  procedure AdjustTexture(const NodeField: TSFNode);
  var
    TexNode: TImageTextureNode;
  begin
    if Texture <> '' then
    begin
      if NodeField.Value is TImageTextureNode then
        TexNode := TImageTextureNode(NodeField.Value)
      else
      begin
        TexNode := TImageTextureNode.Create;
        NodeField.Value := TexNode;
        NodeField.Changed;
      end;
      if (TexNode.FdUrl.Count <> 1) or
         (TexNode.FdUrl.Items[0] <> Texture) then
        TexNode.SetUrl([Texture]);
    end else
    begin
      if NodeField.Value <> nil then
      begin
        NodeField.Value := nil;
        { This causes time-consuming ChangedAll, which is why we checked
          NodeField.Value <> nil above, to not call this from every UpdateMaterialNode,
          e.g. if you changed something else than Texture. }
        NodeField.Changed;
      end;
    end;
  end;

  procedure DoPhysical;
  var
    PM: TPhysicalMaterialNode;
  begin
    if FAppearanceNode.Material is TPhysicalMaterialNode then
      PM := TPhysicalMaterialNode(FAppearanceNode.Material)
    else
    begin
      PM := TPhysicalMaterialNode.Create;
      FAppearanceNode.Material := PM;
    end;
    PM.BaseColor := Color.XYZ;
    PM.Transparency := 1 - Color.W;
    AdjustTexture(PM.FdBaseTexture);
  end;

  procedure DoPhong;
  var
    M: TMaterialNode;
  begin
    if FAppearanceNode.Material is TMaterialNode then
      M := TMaterialNode(FAppearanceNode.Material)
    else
    begin
      M := TMaterialNode.Create;
      FAppearanceNode.Material := M;
    end;
    M.DiffuseColor := Color.XYZ;
    M.Transparency := 1 - Color.W;
    AdjustTexture(M.FdDiffuseTexture);
  end;

  procedure DoUnlit;
  var
    UM: TUnlitMaterialNode;
  begin
    if FAppearanceNode.Material is TUnlitMaterialNode then
      UM := TUnlitMaterialNode(FAppearanceNode.Material)
    else
    begin
      UM := TUnlitMaterialNode.Create;
      FAppearanceNode.Material := UM;
    end;
    UM.EmissiveColor := Color.XYZ;
    UM.Transparency := 1 - Color.W;
    AdjustTexture(UM.FdEmissiveTexture);
  end;

begin
  case FMaterial of
    pmPhysical: DoPhysical;
    pmPhong   : DoPhong;
    pmUnlit   : DoUnlit;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleAbstractPrimitive.UpdateMaterialNode:FMaterial?');
    {$endif}
  end;
end;

procedure TCastleAbstractPrimitive.SetMaterial(const Value: TPrimitiveMaterial);
begin
  if FMaterial <> Value then
  begin
    FMaterial := Value;
    UpdateMaterialNode;
  end;
end;

procedure TCastleAbstractPrimitive.SetColor(const Value: TCastleColor);
begin
  if not TCastleColor.PerfectlyEquals(FColor, Value) then
  begin
    FColor := Value;
    UpdateMaterialNode;
  end;
end;

procedure TCastleAbstractPrimitive.SetTexture(const Value: String);
begin
  if FTexture <> Value then
  begin
    FTexture := Value;
    UpdateMaterialNode;
  end;
end;

function TCastleAbstractPrimitive.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'ColorPersistent', 'Material', 'Texture':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
