{%MainUnit castlescene.pas}
{
  Copyright 2021-2025 Michalis Kamburelis.

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
  { Primitive material type, used by @link(TCastleAbstractPrimitive.Material). }
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
    and loading (using @link(TCastleSceneCore.Load)) an X3D nodes graph
    with appropriate primitives. In fact, that is exactly what happens internally.
    But using this class is simpler (it hides X3D nodes from you, although it also
    hides some possibilities) and you can customize such primitives
    using the CGE editor. }
  TCastleAbstractPrimitive = class(TCastleTransform)
  private
    type
      { Information how to configure X3D texture node and the material field that
        describes the texture. Deliberately designed to make the default value
        equal to be filled by zeroes. }
      TMaterialTexture = record
        { Node of the texture, never @nil. }
        Node: TImageTextureNode;
        { URL of the texture. Empty means no texture, unless LoadedDirectly. }
        Url: String;
        { If @true, it means that Node has contents loaded using Node.LoadFromImage,
          so it should be shown even if Url is empty. }
        LoadedDirectly: Boolean;
      end;
  strict private
    { scene and X3D nodes }
    FScene: TCastleScene;
    FRootNode: TX3DRootNode;
    FAppearanceNode: TAppearanceNode;
    FShapeNode: TShapeNode;
    FBaseTexture, FNormalMapTexture: TMaterialTexture;

    { private fields reflecting public properties }
    FColor: TCastleColor;
    FMaterial: TPrimitiveMaterial;
    FPreciseCollisions: Boolean;

    procedure SetMaterial(const Value: TPrimitiveMaterial);
    procedure SetColor(const Value: TCastleColor);
    function GetTexture: String;
    function GetTextureNormalMap: String;
    procedure SetTexture(const Value: String);
    procedure SetTextureNormalMap(const Value: String);
    { Reflect current FColor, FMaterial, FTexture, FTextureNormalMap values in X3D nodes. }
    procedure UpdateMaterialNode;
    procedure SetPreciseCollisions(const Value: Boolean);
    function GetRenderOptions: TCastleRenderOptions;
  protected
    { Descendants should add primitive X3D geometry node here. }
    property ShapeNode: TShapeNode read FShapeNode;

    { Update collider, notifying it that we changed.
      This updates TCastleCollider with TCastleCollider.AutoSize.
      This also updates TCastleMeshCollider mesh, if it referred to us. }
    procedure UpdateCollider;

    function InternalBuildNodeInside: TObject; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    function HasColliderMesh: Boolean; override;
    procedure ColliderMesh(const TriangleEvent: TTriangleEvent); override;

    { Color of the primitive. Opaque white by default.

      What exactly it means, depends on @link(Material):
      @unorderedList(
        @item(When Material is pmPhysical, then Color determines TPhysicalMaterialNode.BaseColor and TPhysicalMaterialNode.Transparency.)
        @item(When Material is pmPhong, then Color determines TMaterialNode.DiffuseColor and TMaterialNode.Transparency.)
        @item(When Material is pmUnlit, then Color determines TUnlitMaterialNode.EmissiveColor and TUnlitMaterialNode.Transparency.)
      )
    }
    property Color: TCastleColor read FColor write SetColor;

    { Add shader effects to configure how is the component rendered.
      See https://castle-engine.io/shaders for documentation
      how shader effects work in Castle Game Engine. }
    procedure SetEffects(const Value: array of TEffectNode);
  published
    { Material type (which determines lighting calculation) for this primitive. }
    property Material: TPrimitiveMaterial read FMaterial write SetMaterial default pmPhysical;

    { Texture URL.

      The way texture interacts with lighting depends on the @link(Material) type:
      @unorderedList(
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
    property Texture: String read GetTexture write SetTexture;

    { Normal map texture URL.
      Providing normal enhances the details visible on the surface because of lighting.
      The normalmap texture mapping is the same as @link(Texture) mapping. }
    property TextureNormalMap: String read GetTextureNormalMap write SetTextureNormalMap;

    { Resolve collisions precisely with the primitive geometry.
      When this is @false we will only consider the primitive bounding box for collisions,
      so e.g. player will not be able to move within sphere's box corners. }
    property PreciseCollisions: Boolean read FPreciseCollisions write SetPreciseCollisions default false;

    { Rendering options of the scene. }
    property RenderOptions: TCastleRenderOptions read GetRenderOptions;

  {$define read_interface_class}
  {$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
  {$undef read_interface_class}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Utility to implement TPrimitiveMaterial, shared by
  TCastleAbstractPrimitive and TCastleImageTransform. }
procedure CommonUpdateMaterialNode(const Material: TPrimitiveMaterial;
  const Color: TCastleColor; const AppearanceNode: TAppearanceNode;
  const Base, NormalMap: TCastleAbstractPrimitive.TMaterialTexture);

  { Depending on Texture.* fields, adjust Texture.Node.FdUrl
    and make NodeField refer to Texture.Node or nil. }
  procedure AdjustTexture(const NodeField: TSFNode;
    const Texture: TCastleAbstractPrimitive.TMaterialTexture);
  begin
    if (Texture.Url <> '') or Texture.LoadedDirectly then
    begin
      // set NodeField.Value
      if NodeField.Value <> Texture.Node then
      begin
        NodeField.Value := Texture.Node;
        NodeField.Changed;
      end;
      // set Texture.Node.FdUrl (loads texture image)
      if (not Texture.LoadedDirectly) and
         ( (Texture.Node.FdUrl.Count <> 1) or
           (Texture.Node.FdUrl.Items[0] <> Texture.Url) ) then
        Texture.Node.SetUrl([Texture.Url]);
    end else
    begin
      // set NodeField.Value
      if NodeField.Value <> nil then
      begin
        NodeField.Value := nil;
        { This causes time-consuming ChangedAll, which is why we checked
          NodeField.Value <> nil above, to not call this from every UpdateMaterialNode,
          e.g. if you changed something else than Texture.Url. }
        NodeField.Changed;
      end;
      { Set Texture.Node.FdUrl (unloads texture image).

        This is not strictly necessary for functionality here, since we have
        disconnected Texture.Node from NodeField anyway.
        But it's nice to
        1. release memory of the image
        2. TCastleImageTransform.UpdateCoordinateNodes depends
           on using texture node's XxxTexture.Node.IsTextureImage. }
      if Texture.Node.FdUrl.Count <> 0 then
        Texture.Node.SetUrl([]);
    end;
  end;

  procedure DoPhysical;
  var
    PM: TPhysicalMaterialNode;
  begin
    if AppearanceNode.Material is TPhysicalMaterialNode then
      PM := TPhysicalMaterialNode(AppearanceNode.Material)
    else
    begin
      PM := TPhysicalMaterialNode.Create;
      AppearanceNode.Material := PM;
    end;
    PM.BaseColor := Color.XYZ;
    PM.Transparency := 1 - Color.W;
    AdjustTexture(PM.FdBaseTexture, Base);
    AdjustTexture(PM.FdNormalTexture, NormalMap);
  end;

  procedure DoPhong;
  var
    M: TMaterialNode;
  begin
    if AppearanceNode.Material is TMaterialNode then
      M := TMaterialNode(AppearanceNode.Material)
    else
    begin
      M := TMaterialNode.Create;
      AppearanceNode.Material := M;
    end;
    M.DiffuseColor := Color.XYZ;
    M.Transparency := 1 - Color.W;
    AdjustTexture(M.FdDiffuseTexture, Base);
    AdjustTexture(M.FdNormalTexture, NormalMap);
  end;

  procedure DoUnlit;
  var
    UM: TUnlitMaterialNode;
  begin
    if AppearanceNode.Material is TUnlitMaterialNode then
      UM := TUnlitMaterialNode(AppearanceNode.Material)
    else
    begin
      UM := TUnlitMaterialNode.Create;
      AppearanceNode.Material := UM;
    end;
    UM.EmissiveColor := Color.XYZ;
    UM.Transparency := 1 - Color.W;
    AdjustTexture(UM.FdEmissiveTexture, Base);
    AdjustTexture(UM.FdNormalTexture, NormalMap);
  end;

begin
  case Material of
    pmPhysical: DoPhysical;
    pmPhong   : DoPhong;
    pmUnlit   : DoUnlit;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TCastleAbstractPrimitive.UpdateMaterialNode:FMaterial?');
    {$endif}
  end;
end;

{ TCastleAbstractPrimitive --------------------------------------------------- }

constructor TCastleAbstractPrimitive.Create(AOwner: TComponent);
begin
  inherited;

  { set default values }
  FMaterial := pmPhysical;
  FColor := White;

  { create scene }
  FScene := TCastleScene.Create(nil);
  FScene.SetTransient;
  Add(FScene);

  { create X3D nodes }
  FAppearanceNode := TAppearanceNode.Create;
  FShapeNode := TShapeNode.Create;
  FShapeNode.Appearance := FAppearanceNode;
  FRootNode := TX3DRootNode.Create;
  FRootNode.AddChildren(ShapeNode);

  { create X3D nodes for textures }
  FBaseTexture.Node := TImageTextureNode.Create;
  FNormalMapTexture.Node := TImageTextureNode.Create;
  { Don't free texture nodes by ref-counting,
    which would happen otherwise from UpdateMaterialNode, when it does
    1. NodeField.Value := nil
    2. or when it frees previous material (that may contain the texture) }
  FBaseTexture.Node.WaitForRelease;
  FNormalMapTexture.Node.WaitForRelease;
  { Note: FBaseTexture.Node, FNormalMapTexture.Node are connected to
    FAppearanceNode (and thus, the rest of FRootNode) by UpdateMaterialNode. }
  UpdateMaterialNode;

  { load FRootNode to FScene at the end,
    to not reinitialize scene resources on changes to X3D nodes above }
  FScene.Load(FRootNode, true);

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
  NodeRelease(FBaseTexture.Node);
  NodeRelease(FNormalMapTexture.Node);
  inherited;
end;

procedure TCastleAbstractPrimitive.UpdateMaterialNode;
begin
  CommonUpdateMaterialNode(FMaterial, FColor, FAppearanceNode,
    FBaseTexture, FNormalMapTexture);
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

function TCastleAbstractPrimitive.GetTexture: String;
begin
  Result := FBaseTexture.Url;
end;

procedure TCastleAbstractPrimitive.SetTexture(const Value: String);
begin
  if FBaseTexture.Url <> Value then
  begin
    FBaseTexture.Url := Value;
    UpdateMaterialNode;
  end;
end;

function TCastleAbstractPrimitive.GetTextureNormalMap: String;
begin
  Result := FNormalMapTexture.Url;
end;

procedure TCastleAbstractPrimitive.SetTextureNormalMap(const Value: String);
begin
  if FNormalMapTexture.Url <> Value then
  begin
    FNormalMapTexture.Url := Value;
    UpdateMaterialNode;
  end;
end;

function TCastleAbstractPrimitive.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ColorPersistent') or
     (PropertyName = 'Material') or
     (PropertyName = 'TextureNormalMap') or
     (PropertyName = 'Texture') or
     (PropertyName = 'PreciseCollisions') or
     (PropertyName = 'RenderOptions') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

function TCastleAbstractPrimitive.HasColliderMesh: Boolean;
begin
  Result := true;
end;

procedure TCastleAbstractPrimitive.ColliderMesh(
  const TriangleEvent: TTriangleEvent);
begin
  FScene.ColliderMesh(TriangleEvent);
end;

procedure TCastleAbstractPrimitive.SetPreciseCollisions(const Value: Boolean);
begin
  if FPreciseCollisions <> Value then
  begin
    FPreciseCollisions := Value;
    FScene.PreciseCollisions := Value;
    { Note: This also adds ssRendering octree,
      somewhat useless as primitives are usually just 1 shape in an internal scene.
      But does not seem problematic either. }
  end;
end;

function TCastleAbstractPrimitive.GetRenderOptions: TCastleRenderOptions;
begin
  Result := FScene.RenderOptions;
end;

procedure TCastleAbstractPrimitive.UpdateCollider;
begin
  if Collider <> nil then
    Collider.InternalTransformChanged(Self);
end;

function TCastleAbstractPrimitive.InternalBuildNodeInside: TObject;
begin
  Result := FShapeNode.DeepCopy;
end;

procedure TCastleAbstractPrimitive.SetEffects(const Value: array of TEffectNode);
begin
  FScene.SetEffects(Value);
end;

{$define read_implementation_methods}
{$I auto_generated_persistent_vectors/tcastleabstractprimitive_persistent_vectors.inc}
{$undef read_implementation_methods}

{$endif read_implementation}
