{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D Shape component. }

{$ifdef read_interface}
  { }
  TAbstractShaderNode = class;
  TAbstractOneSidedMaterialNode = class;
  TAbstractSingleTextureNode = class;

  { Base node type for the child nodes of @link(TAbstractAppearanceNode). }
  TAbstractAppearanceChildNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dappearancechildnode.inc}
  end;

  { Base node type for all Appearance nodes. }
  TAbstractAppearanceNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dappearancenode.inc}
  end;

  { Base node type for all Material nodes. }
  TAbstractMaterialNode = class(TAbstractAppearanceChildNode)
  public
    { Material information based on this node.
      It is automatically updated when properties of this material change.
      Do not free it yourself, it will be automatically freed when
      this node is freed. }
    function MaterialInfo: TMaterialInfo; virtual; abstract;

    {$I auto_generated_node_helpers/x3dnodes_x3dmaterialnode.inc}
  end;

  TAppearanceNode = class;
  TMaterialNode = class;
  TComposedShaderNode = class;
  TLinePropertiesNode = class;
  TCommonSurfaceShaderNode = class;

  TShading = (shDefault, shGouraud, shPhong, shWireframe);
  TShapeCollision = (scDefault, scBox, scNone);

  { Alpha mode, for @link(TAppearanceNode.AlphaMode). }
  TAlphaMode = (
    { Automatically detect alpha treatment. }
    amAuto,
    { Render shape as opaque (ignore resulting alpha). }
    amOpaque,
    { Render shape using alpha testing (resulting alpha < 0.5 implies a transparent shape). }
    amMask,
    { Render shape using blending (supports partial transparency). }
    amBlend
  );

  { Base node type for all Shape nodes. }
  TAbstractShapeNode = class(TAbstractChildNode)
  strict private
    function GetMaterial: TAbstractMaterialNode;
    procedure SetMaterial(const Value: TAbstractMaterialNode);
    function GetTexture: TAbstractTextureNode;
    procedure SetTexture(const Value: TAbstractTextureNode);
    function GetTextureTransform: TAbstractTextureTransformNode;
    procedure SetTextureTransform(const Value: TAbstractTextureTransformNode);
    function GetLineProperties: TLinePropertiesNode;
    procedure SetLineProperties(const Value: TLinePropertiesNode);
    procedure SetAppearanceBefore(const Sender: TSFNode; const NewNode: TX3DNode);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    InternalWasVisibleFrameId: TFrameId;

    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;

    { The texture of this shape.
      This is a shortcut for accessing "texture" field of @link(Appearance),
      that is @code(TAppearanceNode(FdAppearance.Value).FdTexture.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Texture: TAbstractTextureNode read GetTexture write SetTexture;
      {$ifdef FPC}deprecated 'create and assign texture node instance explicitly, then set Appearance.Texture';{$endif}

    { Texture transformation of this shape.
      Similar to @link(Texture), this is just a comfortable property
      for getting and setting the appropriate Appearance field,
      checking class types along the way. }
    property TextureTransform: TAbstractTextureTransformNode
      read GetTextureTransform write SetTextureTransform;
      {$ifdef FPC}deprecated 'create and assign texture transformatiom node instance explicitly, then set Appearance.TextureTransform';{$endif}

    { Get / set the material of this shape.
      This is a shortcut to access @link(Appearance) and then set @code(Appearance.Material).
      When setting the material to something non-nil, we make sure
      to create @link(Appearance) node if necessary. }
    property Material: TAbstractMaterialNode read GetMaterial write SetMaterial;
      {$ifdef FPC}deprecated 'create and assign TAppearanceNode instance explicitly, then set Appearance.Material';{$endif}

    { Line properties of this shape. This is a clean, type-safe way
      for accessing Appearance.lineProperties field of this shape. }
    property LineProperties: TLinePropertiesNode
      read GetLineProperties write SetLineProperties;
      {$ifdef FPC}deprecated 'create and assign TLinePropertiesNode instance explicitly, then set Appearance.LineProperties';{$endif}

    { Find a suitable CommonSurfaceShader to be used when rendering this shape,
      or @nil. }
    function CommonSurfaceShader: TCommonSurfaceShaderNode;
      deprecated 'use TMaterialNode instead of TCommonSurfaceShaderNode (it has the same functionality and is standardized in X3D v4)';

    { Whether the shape was (potentially, at least partially) visible in the last rendering event.

      Note that the same shape reference may be present multiple times
      within one scene.
      Moreover, the same scene may be present multiple times within one viewport.
      Moreover, multiple viewports may render the same scene.
      The shape is marked as "was visible" if any instance of it was visible
      in any viewport in any scene. }
    function WasVisible: Boolean;

    {$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
  end;

  TFillPropertiesNode = class;

  { Visual properties of geometry.

    Note that the geometry is unlit (pure white, regardless of lighting)
    if no @link(Material) is assigned.
    To make the geometry lit, you can just set there a default
    material created by @code(TMaterialNode.Create). }
  TAppearanceNode = class(TAbstractAppearanceNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    { Find a suitable CommonSurfaceShader to be used when rendering this shape,
      or @nil. }
    function CommonSurfaceShader: TCommonSurfaceShaderNode;
      deprecated 'use TMaterialNode instead of TCommonSurfaceShaderNode (it has the same functionality and is standardized in X3D v4)';

    { Texture that determines the primary RGB color of the material,
      and transparency of the material.

      This is the texture used when rendering using Gouraud shading
      (following the new X3D 4 specification advise).

      For Phong shading, this is the texture:
      - diffuseTexture for Material
      - emissiveTexture for UnlitMaterial
      - baseTexture for PhysicalMaterial

      This is also the texture that determines the opacity of the material.

      This is also the texture which is affected by @link(FdTexture)
      field (if appropriate material slot is empty).

      The overloaded version with TexCoordIndex parameter also specifies which
      texture coordinates / transformations should be used for this texture.
      It returns -1 when we should use standard MultiTexture specification for this,
      which also covers the case when MainTexture is a MultiTexture node,
      and should use appropriate texture coordinates for MultiTexture children.

      Returns @nil if none. }
    function MainTexture: TAbstractTextureNode; overload;
    function MainTexture(const GeometryForTexCoord: TAbstractGeometryNode;
      out TexCoordIndex: Integer): TAbstractTextureNode; overload;

    function InternalMaterialProperty: TMaterialProperty;
    function MaterialProperty: TMaterialProperty; deprecated 'use InternalMaterialProperty, or (better) do not use it at all -- this is internal';

    {$I auto_generated_node_helpers/x3dnodes_appearance.inc}
  end;

  { Additional visual properties to be applied to all polygonal areas. }
  TFillPropertiesNode = class(TAbstractAppearanceChildNode)
  public
    {$I auto_generated_node_helpers/x3dnodes_fillproperties.inc}
  end;

  { Additional visible properties to be applied to all line geometry. }
  TLinePropertiesNode = class(TAbstractAppearanceChildNode)
  strict private
    function GetLineType: TLineType;
    procedure SetLineType(const Value: TLineType);
  public
    { Line type (pattern). }
    property LineType: TLineType read GetLineType write SetLineType default ltSolid;
    {$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
  end;

  TAbstractOneSidedMaterialNode = class(TAbstractMaterialNode)
  public
    {$I auto_generated_node_helpers/x3dnodes_x3donesidedmaterialnode.inc}
  end;

  { Material determines how the geometry looks on the screen,
    by providing parameters to the lighting equations.
    This is the material responsible for the Phong lighting model.

    It specifies how light affects one side of the surface.

    Note: Whether the shape is visible from both sides depends on the
    geometry @link(TAbstractGeometryNode.Solid) field,
    and is independent of whether you use this node or @link(TTwoSidedMaterialNode). }
  TMaterialNode = class(TAbstractOneSidedMaterialNode)
  strict private
    type
      TMyMaterialInfo = class(TPhongMaterialInfo)
      strict private
        FNode: TMaterialNode;
      protected
        function GetDiffuseColor: TVector3; override;
        procedure SetDiffuseColor(const Value: TVector3); override;
        function GetSpecularColor: TVector3; override;
        procedure SetSpecularColor(const Value: TVector3); override;
      public
        constructor Create(ANode: TMaterialNode);

        function AmbientColor: TVector3; override;
        function EmissiveColor: TVector3; override;
        function Shininess: Single; override;
        function ReflectionColor: TVector3; override;
        function Transparency: Single; override;

        {$ifndef CASTLE_SLIM_NODES}
        function ReflSpecular: TVector3; override;
        function ReflDiffuse: TVector3; override;
        function TransSpecular: TVector3; override;
        function TransDiffuse: TVector3; override;
        function ReflSpecularExp: Single; override;
        function TransSpecularExp: Single; override;
        {$endif}
      end;
    var
      FMaterialInfo: TMyMaterialInfo;
  public
    destructor Destroy; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    { Opacity is just a 1 - FdTransparency.Value.
      Defined for your comfort --- for
      OpenGL you will usually want to pass Opacity, not Transparency. }
    function Opacity: Single;

    { ShininessExp is just 128 * FdShininess.Value, this is the "real"
      exponent indicated by shininess field value.
      Defined for your comfort --- for any graphic library you will usually
      want to pass the "real" exponent given by this function, not just
      value of shininess field. }
    function ShininessExp: Single;

    function MaterialInfo: TMaterialInfo; override;

    { Only the emissiveColor is not black (zero),
      which means that the material behaves like unlit.

      This checks that ambient and diffuse and specular colors are all zero.
      It's an important information about the material sometimes.
      It is similar to the NULL material situation (when "Appearance.material=NULL"
      case), but the color and transparency are still
      configurable (using FdEmissiveColor and FdTransparency fields).

      We can optimize this case when rendering. }
    function PureEmissive: boolean;

    { Force the material pure emissive (see @link(PureEmissive)) by setting
      other colors to black. }
    procedure ForcePureEmissive; deprecated 'use TUnlitMaterialNode for unlit materials';

    {$I auto_generated_node_helpers/x3dnodes_material.inc}
  end;
  TMaterialNode_2 = TMaterialNode;

  TPhysicalMaterialNode = class(TAbstractOneSidedMaterialNode)
  strict private
    type
      TMyPhysicalMaterialInfo = class(TPhysicalMaterialInfo)
      strict private
        FNode: TPhysicalMaterialNode;
      public
        constructor Create(ANode: TPhysicalMaterialNode);
        function EmissiveColor: TVector3; override;
        function Transparency: Single; override;
        function BaseColor: TVector3; override;
        function Metallic: Single; override;
        function Roughness: Single; override;
      end;
    var
      FMaterialInfo: TMyPhysicalMaterialInfo;
  public
    destructor Destroy; override;
    function MaterialInfo: TMaterialInfo; override;

    {$I auto_generated_node_helpers/x3dnodes_physicalmaterial.inc}
  end;

  { Shape is a rendered object in the world, with an appearance and geometry. }
  TShapeNode = class(TAbstractShapeNode)
  public
    { Generate tangent vectors on geometry if they are
      (1) necessary and (2) possible to generate and (3) not present yet. }
    procedure GenerateTangents;

    {$I auto_generated_node_helpers/x3dnodes_shape.inc}
  end;

  { @deprecated

    Note: Whether the shape is visible from both sides depends on the
    geometry @link(TAbstractGeometryNode.Solid) field,
    and is independent of whether you use this or other (one-sided) material nodes. }
  TTwoSidedMaterialNode = class(TAbstractMaterialNode)
  strict private
    type
      TTwoSidedMaterialInfo = class(TPhongMaterialInfo)
      strict private
        WarningSeparateBackColorDone: Boolean;
        FNode: TTwoSidedMaterialNode;
      protected
        function GetDiffuseColor: TVector3; override;
        procedure SetDiffuseColor(const Value: TVector3); override;
        function GetSpecularColor: TVector3; override;
        procedure SetSpecularColor(const Value: TVector3); override;
      public
        constructor Create(ANode: TTwoSidedMaterialNode);

        function AmbientColor: TVector3; override;
        function EmissiveColor: TVector3; override;
        function Shininess: Single; override;
        function ReflectionColor: TVector3; override;
        function Transparency: Single; override;
      end;
    var
      FMaterialInfo: TMaterialInfo;
  public
    destructor Destroy; override;
    function MaterialInfo: TMaterialInfo; override;

    {$I auto_generated_node_helpers/x3dnodes_twosidedmaterial.inc}
  end;

  TUnlitMaterialNode = class(TAbstractOneSidedMaterialNode)
  strict private
    type
      TMyUnlitMaterialInfo = class(TUnlitMaterialInfo)
      strict private
        FNode: TUnlitMaterialNode;
      public
        constructor Create(ANode: TUnlitMaterialNode);
        function EmissiveColor: TVector3; override;
        function Transparency: Single; override;
      end;
    var
      FMaterialInfo: TMyUnlitMaterialInfo;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
    function MaterialInfo: TMaterialInfo; override;

    {$I auto_generated_node_helpers/x3dnodes_unlitmaterial.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  AlphaModeToString: array [TAlphaMode] of string =
  (
    'AUTO',
    'OPAQUE',
    'MASK',
    'BLEND'
  );

{ TSFShading ----------------------------------------------------------------- }

(*TODO:
  If wireframe changes, then ExecuteChange should be chWireframe.
  Below there's a convoluted way to do this.
  Not used for now, as FdShading is now just TSFStringEnum.

class function TAbstractShapeNode.TSFShading.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFString;
end;

procedure TAbstractShapeNode.TSFShading.ExposedEventReceive(Event: TX3DEvent; NewValue: TX3DField; const Time: TX3DTime);
var
  OldWireframe, NewWireframe: boolean;
begin
  OldWireframe := TShading(EnumValue) = shWireframe;
  NewWireframe := TShading(StringToEnumValue((NewValue as TSFString).Value)) = shWireframe;
  if OldWireframe <> NewWireframe then
    WireframeChanged := true;

  inherited;
end;

function TAbstractShapeNode.TSFShading.ExecuteChange: TX3DChange;
begin
  Result := inherited;
  if WireframeChanged then
  begin
    WireframeChanged := false;
    Exit(chWireframe);
  end;
end;
*)

{ TAbstractShapeNode --------------------------------------------------------- }

const
  ShadingNames: array [TShading] of String = (
    'DEFAULT',
    'GOURAUD',
    'PHONG',
    'WIREFRAME'
  );
  ShapeCollisionNames: array [TShapeCollision] of String = (
    'DEFAULT',
    'BOX',
    'NONE'
  );

constructor TAbstractShapeNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FdBboxCenter.ChangeAlways := chBBox;
  FdBboxSize.ChangeAlways := chBBox;
  FdAppearance.ChangeAlways := chEverything;
  FdGeometry.ChangeAlways := chEverything;
end;

procedure TAbstractShapeNode.SetAppearanceBefore(const Sender: TSFNode; const NewNode: TX3DNode);
var
  OldMaterial, NewMaterial: TX3DNode;
begin
  // caller of OnBeforeVisibleChange should have checked this
  Assert(Sender.Value <> NewNode);
  Assert(Sender = FdAppearance);

  if Scene <> nil then
  begin
    // move shape associations from old Appearance to new
    Scene.InternalMoveShapeAssociations(Sender.Value, NewNode, InternalSceneShape);

    // move shape associations from old XxxMaterial to new
    if Sender.Value is TAppearanceNode then
      OldMaterial := TAppearanceNode(Sender.Value).FdMaterial.Value
    else
      OldMaterial := nil;
    if NewNode is TAppearanceNode then
      NewMaterial := TAppearanceNode(NewNode).FdMaterial.Value
    else
      NewMaterial := nil;
    if OldMaterial <> NewMaterial then
      Scene.InternalMoveShapeAssociations(OldMaterial, NewMaterial, InternalSceneShape);
  end;
end;

function TAbstractShapeNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;

  { According to X3D spec, when geometry is NULL then object is not
    drawn so appearance doesn't matter. }

  if Geometry <> nil then // checks that FdGeometry has value with correct class
  begin
    Result := FdAppearance.Enumerate(Func);
    if Result <> nil then Exit;

    Result := FdGeometry.Enumerate(Func);
    if Result <> nil then Exit;
  end;
end;

procedure TAbstractShapeNode.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.ShapeNode := Self;
end;

procedure TAbstractShapeNode.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  StateStack.Top.ShapeNode := nil;
  inherited;
end;

function TAbstractShapeNode.GetMaterial: TAbstractMaterialNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    Result := App.Material
  else
    Result := nil;
end;

function TAbstractShapeNode.CommonSurfaceShader: TCommonSurfaceShaderNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    {$warnings off} // using deprecated in deprecated
    Result := App.CommonSurfaceShader
    {$warnings on}
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetMaterial(const Value: TAbstractMaterialNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.Material := Value
  else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', BaseUrl);
    App.Scene := Scene;
    App.Material := Value;
    Appearance := App;
  end;
end;

function TAbstractShapeNode.GetTexture: TAbstractTextureNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    Result := App.Texture
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetTexture(const Value: TAbstractTextureNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.Texture := Value
  else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', BaseUrl);
    App.Scene := Scene;
    App.Texture := Value;
    Appearance := App;
  end;
end;

function TAbstractShapeNode.GetTextureTransform: TAbstractTextureTransformNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    Result := App.TextureTransform
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetTextureTransform(const Value: TAbstractTextureTransformNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.TextureTransform := Value
  else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', BaseUrl);
    App.Scene := Scene;
    App.TextureTransform := Value;
    Appearance := App;
  end;
end;

function TAbstractShapeNode.GetLineProperties: TLinePropertiesNode;
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    Result := App.LineProperties
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetLineProperties(const Value: TLinePropertiesNode);
var
  App: TAppearanceNode;
begin
  App := Appearance;
  if App <> nil then
    App.LineProperties := Value
  else
  if Value <> nil then
  begin
    App := TAppearanceNode.Create('', BaseUrl);
    App.Scene := Scene;
    App.LineProperties := Value;
    Appearance := App;
  end;
end;

function TAbstractShapeNode.WasVisible: Boolean;
begin
  { Note: InternalWasVisibleFrameId is 0 by default,
    and TFramesPerSecond.RenderFrameId is always >= 1,
    which means that this will return (correctly) false at the beginning before any rendering run. }
  Result := TFramesPerSecond.RenderFrameId = InternalWasVisibleFrameId;
end;

function TAppearanceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  {$ifndef CASTLE_SLIM_NODES}
  Result := FFdFillProperties.Enumerate(Func);
  if Result <> nil then Exit;
  {$endif}

  Result := FFdLineProperties.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FFdMaterial.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FFdShaders.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FFdTexture.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FFdTextureTransform.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TAppearanceNode.CommonSurfaceShader: TCommonSurfaceShaderNode;
var
  I: Integer;
  Shader: TX3DNode;
begin
  Result := nil;
  for I := 0 to FdShaders.Count - 1 do
  begin
    Shader := FdShaders[I];
    if Shader is TCommonSurfaceShaderNode then
      Exit(TCommonSurfaceShaderNode(Shader));
  end;
end;

function TAppearanceNode.MainTexture: TAbstractTextureNode;
// var
//   TexCoordIndex: Integer;
var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  // It's like
  // Result := MainTexture(GeometryForTexCoord, TexCoordIndex);
  // but we don't need GeometryForTexCoord in this case.

  Result := nil;
  {$warnings off} // using deprecated to keep backward compatibility
  SurfaceShader := CommonSurfaceShader;
  {$warnings on}
  if SurfaceShader <> nil then
  begin
    if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
    begin
      Exit(SurfaceShader.MultiDiffuseAlphaTexture);
    end;
    if SurfaceShader.DiffuseTexture <> nil then
    begin
      Exit(SurfaceShader.DiffuseTexture);
    end;
  end else
  begin
    if (Material is TMaterialNode) and
       (TMaterialNode(Material).DiffuseTexture <> nil) then
    begin
      Exit(TMaterialNode(Material).DiffuseTexture);
    end;
    if (Material is TPhysicalMaterialNode) and
       (TPhysicalMaterialNode(Material).BaseTexture <> nil) then
    begin
      Exit(TPhysicalMaterialNode(Material).BaseTexture);
    end;
    if (Material is TUnlitMaterialNode) and
       (TUnlitMaterialNode(Material).EmissiveTexture <> nil) then
    begin
      Exit(TUnlitMaterialNode(Material).EmissiveTexture);
    end;
    Result := Texture;
  end;
end;

function TAppearanceNode.MainTexture(const GeometryForTexCoord: TAbstractGeometryNode;
  out TexCoordIndex: Integer): TAbstractTextureNode;
var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  Result := nil;
  TexCoordIndex := -1;
  {$warnings off} // using deprecated to keep backward compatibility
  SurfaceShader := CommonSurfaceShader;
  {$warnings on}
  if SurfaceShader <> nil then
  begin
    if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
    begin
      TexCoordIndex := SurfaceShader.DiffuseTextureCoordinatesId; // TODO: is this what X3DOM/InstantReality are doing? test.
      Exit(SurfaceShader.MultiDiffuseAlphaTexture);
    end;
    if SurfaceShader.DiffuseTexture <> nil then
    begin
      TexCoordIndex := SurfaceShader.DiffuseTextureCoordinatesId;
      Exit(SurfaceShader.DiffuseTexture);
    end;
  end else
  begin
    if (Material is TMaterialNode) and
       (TMaterialNode(Material).DiffuseTexture <> nil) then
    begin
      GeometryForTexCoord.FindTextureMapping(TMaterialNode(Material).DiffuseTextureMapping, TexCoordIndex, false);
      Exit(TMaterialNode(Material).DiffuseTexture);
    end;
    if (Material is TPhysicalMaterialNode) and
       (TPhysicalMaterialNode(Material).BaseTexture <> nil) then
    begin
      GeometryForTexCoord.FindTextureMapping(TPhysicalMaterialNode(Material).BaseTextureMapping, TexCoordIndex, false);
      Exit(TPhysicalMaterialNode(Material).BaseTexture);
    end;
    if (Material is TUnlitMaterialNode) and
       (TUnlitMaterialNode(Material).EmissiveTexture <> nil) then
    begin
      GeometryForTexCoord.FindTextureMapping(TUnlitMaterialNode(Material).EmissiveTextureMapping, TexCoordIndex, false);
      Exit(TUnlitMaterialNode(Material).EmissiveTexture);
    end;
    Result := Texture;
  end;
end;

function TAppearanceNode.MaterialProperty: TMaterialProperty;
begin
  Result := InternalMaterialProperty;
end;

function TAppearanceNode.InternalMaterialProperty: TMaterialProperty;

  function TryUrl(const Url: TMFString): TMaterialProperty;
  var
    TextureUrl: string;
  begin
    Result := nil;
    if Url.Count <> 0 then
    begin
      TextureUrl := Url.Items[0];
      if TextureUrl <> '' then
        Result := MaterialProperties.FindTextureBaseName(
          DeleteURIExt(ExtractURIName(TextureUrl)));
    end;
  end;

var
  Tex: TAbstractTextureNode;
begin
  Result := nil;

  Tex := MainTexture;
  if Tex is TImageTextureNode then
    Result := TryUrl(TImageTextureNode(Tex).FdUrl);
  if Tex is TMovieTextureNode then
    Result := TryUrl(TMovieTextureNode(Tex).FdUrl);
end;

procedure TAppearanceNode.ParseAfter(Reader: TX3DReaderNames);
var
  MP: TMaterialProperty;
  NormalMapNode: TImageTextureNode;
begin
  inherited;

  {$warnings off} // using deprecated material props to keep them working
  MP := InternalMaterialProperty;

  if (MP <> nil) and (MP.NormalMap <> '') and (FdNormalMap.Value = nil) then
  begin
    { use normalMap from MaterialProperty, if our normalMap field is empty now }
    NormalMapNode := TImageTextureNode.Create('', BaseUrl);
    NormalMapNode.FdUrl.Items.Add(MP.NormalMap);
    FdNormalMap.Value := NormalMapNode;
  end;

  if (MP <> nil) and
     (MP.AlphaChannel <> '') and
     (Texture is TAbstractSingleTextureNode) { checks also Texture <> nil } then
    TAbstractSingleTextureNode(Texture).FdAlphaChannel.Value := MP.AlphaChannel;
  {$warnings on}
end;

function TLinePropertiesNode.GetLineType: TLineType;
begin
  case FdLineType.Value of
    1: Result := ltSolid;
    2: Result := ltDashed;
    3: Result := ltDotted;
    4: Result := ltDashedDotted;
    5: Result := ltDashDotDot;
    else Result := ltSolid; // unsupported FdLineType.Value
  end;
end;

procedure TLinePropertiesNode.SetLineType(const Value: TLineType);
begin
  case Value of
    ltSolid       : FdLineType.Send(1);
    ltDashed      : FdLineType.Send(2);
    ltDotted      : FdLineType.Send(3);
    ltDashedDotted: FdLineType.Send(4);
    ltDashDotDot  : FdLineType.Send(5);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TLinePropertiesNode.SetLineType:this Value not implemented');
    {$endif}
  end;
end;

{ TMaterialNode -------------------------------------------------------------- }

destructor TMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

class function TMaterialNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TMaterialNode.Opacity: Single;
begin
  Result := 1- FdTransparency.Value;
end;

function TMaterialNode.ShininessExp: Single;
begin
  Result := Clamped(FdShininess.Value * 128.0, 0.0, 128.0);
end;

function TMaterialNode.MaterialInfo: TMaterialInfo;
begin
  if FMaterialInfo = nil then
    FMaterialInfo := TMyMaterialInfo.Create(Self);
  Result := FMaterialInfo;
end;

function TMaterialNode.PureEmissive: boolean;
begin
  Result := (FdAmbientIntensity.Value = 0) and
            FdDiffuseColor.Value.IsPerfectlyZero and
            FdSpecularColor.Value.IsPerfectlyZero;
end;

procedure TMaterialNode.ForcePureEmissive;
begin
  FdDiffuseColor.Send(TVector3.Zero);
  FdSpecularColor.Send(TVector3.Zero);
  FdAmbientIntensity.Send(0);
  Assert(PureEmissive);
end;

{ TMaterialNode.TMyMaterialInfo -------------------------------------------------------- }

constructor TMaterialNode.TMyMaterialInfo.Create(ANode: TMaterialNode);
begin
  inherited Create(ANode);
  FNode := ANode;
end;

function TMaterialNode.TMyMaterialInfo.AmbientColor: TVector3;
begin
  Result := FNode.DiffuseColor * FNode.AmbientIntensity;
end;

function TMaterialNode.TMyMaterialInfo.GetDiffuseColor: TVector3;
begin
  Result := FNode.DiffuseColor;
end;

procedure TMaterialNode.TMyMaterialInfo.SetDiffuseColor(const Value: TVector3);
begin
  FNode.DiffuseColor := Value;
end;

function TMaterialNode.TMyMaterialInfo.GetSpecularColor: TVector3;
begin
  Result := FNode.SpecularColor;
end;

procedure TMaterialNode.TMyMaterialInfo.SetSpecularColor(const Value: TVector3);
begin
  FNode.SpecularColor := Value;
end;

function TMaterialNode.TMyMaterialInfo.EmissiveColor: TVector3;
begin
  Result := FNode.EmissiveColor;
end;

function TMaterialNode.TMyMaterialInfo.Shininess: Single;
begin
  Result := FNode.Shininess;
end;

function TMaterialNode.TMyMaterialInfo.ReflectionColor: TVector3;
var
  M: Single;
begin
  M := FNode.Mirror;
  Result := Vector3(M, M, M);
end;

function TMaterialNode.TMyMaterialInfo.Transparency: Single;
begin
  Result := FNode.Transparency;
end;

{$ifndef CASTLE_SLIM_NODES}

function TMaterialNode.TMyMaterialInfo.ReflSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[0];
end;

function TMaterialNode.TMyMaterialInfo.ReflDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[0];
end;

function TMaterialNode.TMyMaterialInfo.TransSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[0];
end;

function TMaterialNode.TMyMaterialInfo.TransDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[0];
end;

function TMaterialNode.TMyMaterialInfo.ReflSpecularExp: Single;
begin
  Result := FNode.FdReflSpecularExp.Value;
end;

function TMaterialNode.TMyMaterialInfo.TransSpecularExp: Single;
begin
  Result := FNode.FdTransSpecularExp.Value;
end;

{$endif not CASTLE_SLIM_NODES}

{ TPhysicalMaterialNode.TMyPhysicalMaterialInfo --------------------------------------------------- }

constructor TPhysicalMaterialNode.TMyPhysicalMaterialInfo.Create(ANode: TPhysicalMaterialNode);
begin
  inherited Create(ANode);
  FNode := ANode;
end;

function TPhysicalMaterialNode.TMyPhysicalMaterialInfo.EmissiveColor: TVector3;
begin
  Result := FNode.EmissiveColor;
end;

function TPhysicalMaterialNode.TMyPhysicalMaterialInfo.Transparency: Single;
begin
  Result := FNode.Transparency;
end;

function TPhysicalMaterialNode.TMyPhysicalMaterialInfo.BaseColor: TVector3;
begin
  Result := FNode.BaseColor;
end;

function TPhysicalMaterialNode.TMyPhysicalMaterialInfo.Metallic: Single;
begin
  Result := FNode.Metallic;
end;

function TPhysicalMaterialNode.TMyPhysicalMaterialInfo.Roughness: Single;
begin
  Result := FNode.Roughness;
end;

{ TPhysicalMaterialNode ------------------------------------------------------ }

destructor TPhysicalMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

function TPhysicalMaterialNode.MaterialInfo: TMaterialInfo;
begin
  if FMaterialInfo = nil then
    FMaterialInfo := TMyPhysicalMaterialInfo.Create(Self);
  Result := FMaterialInfo;
end;

{ TShapeNode ----------------------------------------------------------------- }

procedure TShapeNode.GenerateTangents;

  { Calculate tangents, having all array (coords, normal, tex coords, tangents) prepared
    and equal length (CoordsCount). }
  procedure GenerateTangentsCore(const CoordsCount: Integer;
    const Coords, Normals: TVector3List;
    const TexCoords: TVector2List;
    const Tangents: TVector3List);
  var
    TriangleIndexes: TInt32List;
    Tangent: TVector3;
    I, Index0, Index1, Index2: Integer;
    TriangleCoord: TTriangle3;
    TriangleTexCoord: TTriangle2;
  begin
    // Too verbose.
    // Also, it seems (test on soldier1.blend) that Blender cannot generate these tangents, so this is not easy to address by author.
    // WritelnLog('Calculating tangents for %s with geometry %s. Consider exporting with tangents already generated, to make loading faster.', [NiceName, Geometry.NiceName]);

    { TODO: for now, we only handle TIndexedTriangleSetNode
      (it is checked that Geometry is of this class in the main body of TShapeNode.GenerateTangents).
      But we should be able to handle all geometries.

      We need for this something like TAbstractGeometryNode.Triangulate,
      instead of only helpers like TAbstractGeometryNode.InternalMakeCoordRanges .
      We have TShape.Triangulate (using constructed array by TArraysGenerator),
      but we cannot use TShape or TArraysGenerator here -- as lots of other data
      for this is not ready yet.
      We want GenerateTangents here, in X3DNodes, to use them when importing glTF). }

    TriangleIndexes := TIndexedTriangleSetNode(Geometry).FdIndex.Items;
    I := 0;
    while I + 2 < TriangleIndexes.Count do
    begin
      Index0 := TriangleIndexes[I    ];
      Index1 := TriangleIndexes[I + 1];
      Index2 := TriangleIndexes[I + 2];
      TriangleCoord.Data[0] := Coords.L[Index0];
      TriangleCoord.Data[1] := Coords.L[Index1];
      TriangleCoord.Data[2] := Coords.L[Index2];
      TriangleTexCoord.Data[0] := TexCoords.L[Index0];
      TriangleTexCoord.Data[1] := TexCoords.L[Index1];
      TriangleTexCoord.Data[2] := TexCoords.L[Index2];

      if not CalculateTangent(true, Tangent, TriangleCoord, TriangleTexCoord) then
        Tangent := TVector3.One[0];

      Tangents.L[Index0] := MakeVectorOrthogonal(Tangent, Normals.L[Index0]);
      Tangents.L[Index1] := MakeVectorOrthogonal(Tangent, Normals.L[Index1]);
      Tangents.L[Index2] := MakeVectorOrthogonal(Tangent, Normals.L[Index2]);

      Inc(I, 3);
    end;
  end;

var
  OneSidedMat: TAbstractOneSidedMaterialNode;
  NormalTextureCoords: TAbstractSingleTextureCoordinateNode;
  Normal: TNormalNode;
  Tangent: TTangentNode;
  CoordsCount: Integer;
  Coords, Normals: TVector3List;
  TexCoords: TVector2List;
  Tangents: TVector3List;
begin
  if { geometry has normals per-vertex }
     (Geometry is TAbstractComposedGeometryNode) and // also checks Geometry <> nil
     (TAbstractComposedGeometryNode(Geometry).Normal is TNormalNode) and // also checks Normal <> nil
     (TAbstractComposedGeometryNode(Geometry).NormalPerVertex) and
     { geometry doesn't have tangents }
     (TAbstractComposedGeometryNode(Geometry).Tangent = nil) and
     { appearance has normal texture }
     (Appearance <> nil) and
     (Appearance.Material is TAbstractOneSidedMaterialNode) and // checks also Appearance.Material <> nil
     (TAbstractOneSidedMaterialNode(Appearance.Material).NormalTexture <> nil) then
  begin
    OneSidedMat := TAbstractOneSidedMaterialNode(Appearance.Material);
    NormalTextureCoords := Geometry.FindTextureMapping(OneSidedMat.NormalTextureMapping, false);
    { geometry has matching normal texture coordinates }
    if NormalTextureCoords is TTextureCoordinateNode then // checks also NormalTextureCoords <> nil
    begin
      if (Geometry.CoordField <> nil) and
         (Geometry.CoordField.Value is TCoordinateNode) then
      begin
        { calculate Coords, CoordsCount }
        Coords := TCoordinateNode(Geometry.CoordField.Value).FdPoint.Items;
        CoordsCount := Coords.Count;

        { detect and abort in case normal / tex coord ordering is more complicated }
        if (Geometry is TIndexedFaceSetNode) and
           ( (TIndexedFaceSetNode(Geometry).FdNormalIndex.Items.Count <> 0) or
             (TIndexedFaceSetNode(Geometry).FdTexCoordIndex.Items.Count <> 0) ) then
          Exit;

        { calculate Normals }
        Normal := TNormalNode(TAbstractComposedGeometryNode(Geometry).Normal);
        Normals := Normal.FdVector.Items;
        if CoordsCount <> Normals.Count then
          Exit;

        { calculate TexCoords }
        TexCoords := TTextureCoordinateNode(NormalTextureCoords).FdPoint.Items;
        if CoordsCount <> TexCoords.Count then
          Exit;

        { calculate Tangents }
        Tangent := TTangentNode.Create;
        Tangents := Tangent.FdVector.Items;
        Tangents.Count := CoordsCount;

        // TODO: for now we can do this only for IndexedTriangleSet
        if not (Geometry is TIndexedTriangleSetNode) then
          Exit;

        GenerateTangentsCore(CoordsCount, Coords, Normals, TexCoords, Tangents);

        { assign this at the end, just in case it will cause TCastleSceneCore processing on change }
        TAbstractComposedGeometryNode(Geometry).Tangent := Tangent;
      end;
    end;
  end;
end;

{ TTwoSidedMaterialInfo --------------------------------------------------- }

constructor TTwoSidedMaterialNode.TTwoSidedMaterialInfo.Create(ANode: TTwoSidedMaterialNode);
begin
  inherited Create(ANode);
  FNode := ANode;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.AmbientColor: TVector3;
begin
  Result := FNode.DiffuseColor * FNode.AmbientIntensity;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.GetDiffuseColor: TVector3;
begin
  { Warn about SeparateBackColor not being implemented, in 1st call to DiffuseColor
    (most often used method). }
  if FNode.SeparateBackColor and not WarningSeparateBackColorDone then
  begin
    WritelnWarning('TwoSidedMaterial X3D node (%s) with SeparateBackColor = true is not supported yet', [
      FNode.NiceName
    ]);
    WarningSeparateBackColorDone := true;
  end;
  Result := FNode.DiffuseColor;
end;

procedure TTwoSidedMaterialNode.TTwoSidedMaterialInfo.SetDiffuseColor(const Value: TVector3);
begin
  FNode.DiffuseColor := Value;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.GetSpecularColor: TVector3;
begin
  Result := FNode.SpecularColor;
end;

procedure TTwoSidedMaterialNode.TTwoSidedMaterialInfo.SetSpecularColor(const Value: TVector3);
begin
  FNode.SpecularColor := Value;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.EmissiveColor: TVector3;
begin
  Result := FNode.EmissiveColor;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.Shininess: Single;
begin
  Result := FNode.Shininess;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.ReflectionColor: TVector3;
begin
  Result := TVector3.Zero;
end;

function TTwoSidedMaterialNode.TTwoSidedMaterialInfo.Transparency: Single;
begin
  Result := FNode.Transparency;
end;

{ TTwoSidedMaterialNode ------------------------------------------------------ }

destructor TTwoSidedMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

function TTwoSidedMaterialNode.MaterialInfo: TMaterialInfo;
begin
  if FMaterialInfo = nil then
    FMaterialInfo := TTwoSidedMaterialInfo.Create(Self);
  Result := FMaterialInfo;
end;

{ TUnlitMaterialNode.TMyUnlitMaterialInfo --------------------------------------------------- }

constructor TUnlitMaterialNode.TMyUnlitMaterialInfo.Create(ANode: TUnlitMaterialNode);
begin
  inherited Create(ANode);
  FNode := ANode;
end;

function TUnlitMaterialNode.TMyUnlitMaterialInfo.EmissiveColor: TVector3;
begin
  Result := FNode.EmissiveColor;
end;

function TUnlitMaterialNode.TMyUnlitMaterialInfo.Transparency: Single;
begin
  Result := FNode.Transparency;
end;

{ TUnlitMaterialNode ------------------------------------------------------ }

constructor TUnlitMaterialNode.Create(const AX3DName: string; const ABaseUrl: string);
begin
  inherited;
  // default is different
  FdEmissiveColor.DefaultValue := Vector3(1, 1, 1);
  FdEmissiveColor.Value := Vector3(1, 1, 1);
end;

destructor TUnlitMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

function TUnlitMaterialNode.MaterialInfo: TMaterialInfo;
begin
  if FMaterialInfo = nil then
    FMaterialInfo := TMyUnlitMaterialInfo.Create(Self);
  Result := FMaterialInfo;
end;

{ register ------------------------------------------------------------------- }

procedure RegisterShapeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAppearanceNode,
    TFillPropertiesNode,
    TLinePropertiesNode,
    TMaterialNode,
    TPhysicalMaterialNode,
    TShapeNode,
    TTwoSidedMaterialNode,
    TUnlitMaterialNode
  ]);
end;

{$endif read_implementation}
