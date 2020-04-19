{
  Copyright 2002-2018 Michalis Kamburelis.

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
  TKambiOctreePropertiesNode = class;
  TAbstractShaderNode = class;
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
  TAbstractTextureTransformNode = class;
  TMaterialNode = class;
  TComposedShaderNode = class;
  TLinePropertiesNode = class;
  TCommonSurfaceShaderNode = class;

  TShading = (shDefault, shGouraud, shPhong, shWireframe);
  TShapeCollision = (scDefault, scBox);

  { Base node type for all Shape nodes. }
  TAbstractShapeNode = class(TAbstractChildNode, IAbstractBoundedObject)
  strict private
    function GetMaterial: TAbstractMaterialNode;
    procedure SetMaterial(const Value: TAbstractMaterialNode);
    function GetTexture: TAbstractTextureNode;
    procedure SetTexture(const Value: TAbstractTextureNode);
    function GetTextureTransform: TAbstractTextureTransformNode;
    procedure SetTextureTransform(const Value: TAbstractTextureTransformNode);
    function GetLineProperties: TLinePropertiesNode;
    procedure SetLineProperties(const Value: TLinePropertiesNode);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;

    { The texture of this shape.
      This is a shortcut for accessing "texture" field of Apperance,
      that is @code(TAppearanceNode(FdAppearance.Value).FdTexture.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Texture: TAbstractTextureNode read GetTexture write SetTexture;

    { Texture transformation of this shape.
      Similar to @link(Texture), this is just a comfortable property
      for getting and setting the appropriate Appearance field,
      checking class types along the way. }
    property TextureTransform: TAbstractTextureTransformNode
      read GetTextureTransform write SetTextureTransform;

    { Get / set the material of this shape.
      When setting this material to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Material: TAbstractMaterialNode read GetMaterial write SetMaterial;

    { Line properties of this shape. This is a clean, type-safe way
      for accessing Appearance.lineProperties field of this shape. }
    property LineProperties: TLinePropertiesNode
      read GetLineProperties write SetLineProperties;

    { Find a suitable CommonSurfaceShader to be used when rendering this shape,
      or @nil. }
    function CommonSurfaceShader: TCommonSurfaceShaderNode;
      deprecated 'use TMaterialNode instead of TCommonSurfaceShaderNode (it has the same functionality and is standardized in X3D v4)';

    {$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
  end;

  TMFNodeShaders = class(TMFNode)
  strict protected
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
  public
    { Returns Items[Index], if it's a shader node suitable
      for GLSL  shader. Otherwise returns @nil. Checks the class of
      Items[Index] and it's @code(language) field. }
    function GLSLShader(Index: Integer): TComposedShaderNode;
  end;

  TFillPropertiesNode = class;

  { Visual properties of geometry.

    Note that the geometry is unlit (pure white, regardless of lighting)
    if no @link(Material) is assigned.
    To make the geometry lit, you can just set there a default
    material created by @code(TMaterialNode.Create). }
  TAppearanceNode = class(TAbstractAppearanceNode)
  strict private
    function GetTexture: TAbstractTextureNode;
    procedure SetTexture(const Value: TAbstractTextureNode);
    function GetMaterial: TAbstractMaterialNode;
    procedure SetMaterial(const Value: TAbstractMaterialNode);
    function GetNormalMap: TAbstractTexture2DNode;
    procedure SetNormalMap(const Value: TAbstractTexture2DNode);
    function GetAlphaChannel: TAutoAlphaChannel;
    procedure SetAlphaChannel(const Value: TAutoAlphaChannel);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    {$ifndef CASTLE_SLIM_NODES}
    strict private FFdFillProperties: TSFNode;
    public property FdFillProperties: TSFNode read FFdFillProperties;
    {$endif}

    strict private FFdLineProperties: TSFNode;
    public property FdLineProperties: TSFNode read FFdLineProperties;

    strict private FFdMaterial: TSFNode;
    public property FdMaterial: TSFNode read FFdMaterial;

    strict private FFdBackMaterial: TSFNode;
    public property FdBackMaterial: TSFNode read FFdBackMaterial;

    strict private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

    strict private FFdTexture: TSFNode;
    public property FdTexture: TSFNode read FFdTexture;

    strict private FFdTextureTransform: TSFNode;
    public property FdTextureTransform: TSFNode read FFdTextureTransform;

    strict private FFdReceiveShadows: TMFNode;
    public property FdReceiveShadows: TMFNode read FFdReceiveShadows;

    strict private FFdShadowCaster: TSFBool;
    public property FdShadowCaster: TSFBool read FFdShadowCaster;

    strict private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    strict private FFdNormalMap: TSFNode;
    public property FdNormalMap: TSFNode read FFdNormalMap;

    {$ifndef CASTLE_SLIM_NODES}
    strict private FFdHeightMap: TSFNode;
    public property FdHeightMap: TSFNode read FFdHeightMap;
    {$endif}

    strict private FFdHeightMapScale: TSFFloat;
    public property FdHeightMapScale: TSFFloat read FFdHeightMapScale;

    { blendMode property.

      This is modelled after InstantReality extension on [http://www.instantreality.org/].
      See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
      and [http://www.instantreality.org/documentation/nodetype/BlendMode/].
      It allows you to use BlendMode nodes, which I consider very useful. }
    strict private FFdBlendMode: TSFNode;
    public property FdBlendMode: TSFNode read FFdBlendMode;

    { The texture of this appearance.

      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property Texture: TAbstractTextureNode read GetTexture write SetTexture;

    { The normal map of this appearance.

      See https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_bump_mapping .
      This is a deprecated method of using bump mapping, a better method is to
      use CommonSurfaceShader:
      https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader .

      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property NormalMap: TAbstractTexture2DNode read GetNormalMap write SetNormalMap;

    { The material of this appearance.
      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property Material: TAbstractMaterialNode read GetMaterial write SetMaterial;

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

      The overloaded version with Mapping parameter also specifies which
      texture coordinates / transformations should be used for this texture.
      It returns -1 when we should use standard MultiTexture specification for this,
      which also covers the case when MainTexture is a MultiTexture node,
      and should use appropriate texture coordinates for MultiTexture children.

      Returns @nil if none. }
    function MainTexture: TAbstractTextureNode;
    function MainTexture(out Mapping: Integer): TAbstractTextureNode;

    function InternalMaterialProperty: TMaterialProperty;
    function MaterialProperty: TMaterialProperty; deprecated 'use InternalMaterialProperty, or (better) do not use it at all -- this is internal';

    strict private FFdAlphaChannel: TSFStringEnum;
    public property FdAlphaChannel: TSFStringEnum read FFdAlphaChannel;
    property AlphaChannel: TAutoAlphaChannel read GetAlphaChannel write SetAlphaChannel;

    {$I auto_generated_node_helpers/x3dnodes_appearance.inc}
  end;

  { Additional visual properties to be applied to all polygonal areas. }
  TFillPropertiesNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdFilled: TSFBool;
    public property FdFilled: TSFBool read FFdFilled;

    strict private FFdHatchColor: TSFColor;
    public property FdHatchColor: TSFColor read FFdHatchColor;

    strict private FFdHatched: TSFBool;
    public property FdHatched: TSFBool read FFdHatched;

    strict private FFdHatchStyle: TSFInt32;
    public property FdHatchStyle: TSFInt32 read FFdHatchStyle;

    {$I auto_generated_node_helpers/x3dnodes_fillproperties.inc}
  end;

  { Supported line types (patterns), for @link(TLinePropertiesNode.LineType). }
  TLineType = (
    ltSolid,
    ltDashed,
    ltDotted,
    ltDashedDotted,
    ltDashDotDot);

  { Additional visible properties to be applied to all line geometry. }
  TLinePropertiesNode = class(TAbstractAppearanceChildNode)
  strict private
    function GetLineType: TLineType;
    procedure SetLineType(const Value: TLineType);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdApplied: TSFBool;
    public property FdApplied: TSFBool read FFdApplied;

    strict private FFdLinetype: TSFInt32;
    public property FdLinetype: TSFInt32 read FFdLinetype;

    { Line type (pattern). }
    property LineType: TLineType read GetLineType write SetLineType default ltSolid;

    strict private FFdLinewidthScaleFactor: TSFFloat;
    public property FdLinewidthScaleFactor: TSFFloat read FFdLinewidthScaleFactor;

    {$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
  end;

  TAbstractOneSidedMaterialNode = class(TAbstractMaterialNode)
  public
    {$I auto_generated_node_helpers/x3dnodes_x3donesidedmaterialnode.inc}
  end;

  { Material determines how the geometry looks on the screen,
    by providing parameters to the lighting equations.
    This is the material responsible for the Phong lighting model.

    It may be one-sided material under some cirumstances (black from the back
    side).
    To have a two-sided material, either use TTwoSidedMaterialNode,
    or set the shading to Phong using @link(TRenderingAttributes.PhongShading)
    or @link(TShapeNode.Shading). }
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
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    strict private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    strict private FFdAmbientTexture: TSFNode;
    public property FdAmbientTexture: TSFNode read FFdAmbientTexture;

    strict private FFdAmbientTextureChannel: TSFInt32;
    public property FdAmbientTextureChannel: TSFInt32 read FFdAmbientTextureChannel;

    strict private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    strict private FFdDiffuseTexture: TSFNode;
    public property FdDiffuseTexture: TSFNode read FFdDiffuseTexture;

    strict private FFdDiffuseTextureChannel: TSFInt32;
    public property FdDiffuseTextureChannel: TSFInt32 read FFdDiffuseTextureChannel;

    strict private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    strict private FFdShininessTexture: TSFNode;
    public property FdShininessTexture: TSFNode read FFdShininessTexture;

    strict private FFdShininessTextureChannel: TSFInt32;
    public property FdShininessTextureChannel: TSFInt32 read FFdShininessTextureChannel;

    strict private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    strict private FFdSpecularTexture: TSFNode;
    public property FdSpecularTexture: TSFNode read FFdSpecularTexture;

    strict private FFdSpecularTextureChannel: TSFInt32;
    public property FdSpecularTextureChannel: TSFInt32 read FFdSpecularTextureChannel;

    strict private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    {$ifndef CASTLE_SLIM_NODES}
    strict private FFdFogImmune: TSFBool;
    { Make the object not affected by fog.
      This is a Castle Game Engine extension, not present in X3D standard. }
    public property FdFogImmune: TSFBool read FFdFogImmune;
    {$endif}

    strict private FFdMirror: TSFFloat;
    { Make the object a mirror.
      Right now used only by the classic ray-tracer, not by interactive renderer.
      This is a Castle Game Engine extension, not present in X3D standard. }
    public property FdMirror: TSFFloat read FFdMirror;

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

    {$ifndef CASTLE_SLIM_NODES}
    strict private FFdReflSpecular: TMFColor;
    public property FdReflSpecular: TMFColor read FFdReflSpecular;

    strict private FFdReflDiffuse: TMFColor;
    public property FdReflDiffuse: TMFColor read FFdReflDiffuse;

    strict private FFdTransSpecular: TMFColor;
    public property FdTransSpecular: TMFColor read FFdTransSpecular;

    strict private FFdTransDiffuse: TMFColor;
    public property FdTransDiffuse: TMFColor read FFdTransDiffuse;

    strict private FFdReflSpecularExp: TSFFloat;
    public property FdReflSpecularExp: TSFFloat read FFdReflSpecularExp;

    strict private FFdTransSpecularExp: TSFFloat;
    public property FdTransSpecularExp: TSFFloat read FFdTransSpecularExp;
    {$endif}

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
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
    function MaterialInfo: TMaterialInfo; override;

    {$I auto_generated_node_helpers/x3dnodes_physicalmaterial.inc}
  end;

  { Shape is a rendered object in the world, with an appearance and geometry. }
  TShapeNode = class(TAbstractShapeNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_shape.inc}
  end;

  { Two-sided material (always lit from both sides),
    with properties that allow to lit both sides with different properties (colors). }
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
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    function MaterialInfo: TMaterialInfo; override;

    strict private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    strict private FFdBackAmbientIntensity: TSFFloat;
    public property FdBackAmbientIntensity: TSFFloat read FFdBackAmbientIntensity;

    strict private FFdBackDiffuseColor: TSFColor;
    public property FdBackDiffuseColor: TSFColor read FFdBackDiffuseColor;

    strict private FFdBackEmissiveColor: TSFColor;
    public property FdBackEmissiveColor: TSFColor read FFdBackEmissiveColor;

    strict private FFdBackShininess: TSFFloat;
    public property FdBackShininess: TSFFloat read FFdBackShininess;

    strict private FFdBackSpecularColor: TSFColor;
    public property FdBackSpecularColor: TSFColor read FFdBackSpecularColor;

    strict private FFdBackTransparency: TSFFloat;
    public property FdBackTransparency: TSFFloat read FFdBackTransparency;

    strict private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    strict private FFdEmissiveColor: TSFColor;
    public property FdEmissiveColor: TSFColor read FFdEmissiveColor;

    strict private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    strict private FFdSeparateBackColor: TSFBool;
    public property FdSeparateBackColor: TSFBool read FFdSeparateBackColor;

    strict private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    strict private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

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

{ TSFShading ----------------------------------------------------------------- }

(*TODO:
  If wireframe changes, then ExecuteChanges should include chWireframe.
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

function TAbstractShapeNode.TSFShading.ExecuteChanges: TX3DChanges;
begin
  Result := inherited;
  if WireframeChanged then
  begin
    System.Include(Result, chWireframe);
    WireframeChanged := false;
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
    'BOX'
  );

constructor TAbstractShapeNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FdBboxCenter.ChangesAlways := [chBBox];
  FdBboxSize.ChangesAlways := [chBBox];
  FdAppearance.ChangesAlways := [chEverything];
  FdGeometry.ChangesAlways := [chEverything];
  {$ifndef CASTLE_SLIM_NODES}
  FdOctreeTriangles.ChangesAlways := [chEverything];
  {$endif}
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

procedure TAppearanceNode.CreateNode;
begin
  inherited;

  {$ifndef CASTLE_SLIM_NODES}
  FFdFillProperties := TSFNode.Create(Self, true, 'fillProperties', [TFillPropertiesNode]);
   FdFillProperties.ChangesAlways := [chEverything];
  AddField(FFdFillProperties);
  {$endif}

  FFdLineProperties := TSFNode.Create(Self, true, 'lineProperties', [TLinePropertiesNode]);
   FdLineProperties.ChangesAlways := [chEverything];
  AddField(FFdLineProperties);

  FFdMaterial := TSFNode.Create(Self, true, 'material', [TAbstractMaterialNode]);
   FdMaterial.ChangesAlways := [chEverything];
  AddField(FFdMaterial);

  FFdBackMaterial := TSFNode.Create(Self, true, 'backMaterial', [TAbstractMaterialNode]);
   FdBackMaterial.ChangesAlways := []; // not used now
  AddField(FFdBackMaterial);

  FFdShaders := TMFNodeShaders.Create(Self, true, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  AddField(FFdShaders);

  FFdTexture := TSFNode.Create(Self, true, 'texture', [TAbstractTextureNode]);
   FdTexture.ChangesAlways := [chEverything];
  AddField(FFdTexture);

  FFdTextureTransform := TSFNode.Create(Self, true, 'textureTransform', [TAbstractTextureTransformNode]);
   FdTextureTransform.ChangesAlways := [chEverything];
  AddField(FFdTextureTransform);

  FFdReceiveShadows := TMFNode.Create(Self, false, 'receiveShadows', [TAbstractPunctualLightNode]);
   FdReceiveShadows.ChangesAlways := [chShadowMaps];
  AddField(FFdReceiveShadows);

  FFdShadowCaster := TSFBool.Create(Self, true, 'shadowCaster', true);
   FdShadowCaster.ChangesAlways := [chShadowCasters];
  AddField(FFdShadowCaster);

  FFdNormalMap := TSFNode.Create(Self, true, 'normalMap', [TAbstractTextureNode]);
   FdNormalMap.ChangesAlways := [chEverything];
  AddField(FFdNormalMap);

  {$ifndef CASTLE_SLIM_NODES}
  FFdHeightMap := TSFNode.Create(Self, true, 'heightMap', [TAbstractTextureNode]);
   FdHeightMap.ChangesAlways := [chEverything];
  AddField(FFdHeightMap);
  {$endif CASTLE_SLIM_NODES}

  FFdHeightMapScale := TSFFloat.Create(Self, {$ifdef CASTLE_SLIM_NODES} false {$else} true {$endif}, 'heightMapScale', DefaultHeightMapScale);
   FdHeightMapScale.ChangesAlways := [chEverything];
  AddField(FFdHeightMapScale);

  FFdBlendMode := TSFNode.Create(Self, true, 'blendMode', [TBlendModeNode]);
   FdBlendMode.ChangesAlways := [chEverything];
  AddField(FFdBlendMode);

  FFdEffects := TMFNode.Create(Self, false, 'effects', [TEffectNode]);
   FdEffects.ChangesAlways := [chEverything];
  AddField(FFdEffects);

  FFdAlphaChannel := TSFStringEnum.Create(Self, false, 'alphaChannel', AlphaToString, Ord(acAuto));
  AddField(FFdAlphaChannel);

  { In edition 2 of X3D XML encoding, this is empty... but in earlier
    versions, this was "appearance" and this seems more sensible,
    Appearance node may only occur within Shape.appearance field
    so it should definitely have DefaultContainerField set. }
  DefaultContainerField := 'appearance';
end;

class function TAppearanceNode.ClassX3DType: string;
begin
  Result := 'Appearance';
end;

function TAppearanceNode.GetAlphaChannel: TAutoAlphaChannel;
begin
  Result := TAutoAlphaChannel(FdAlphaChannel.EnumValue);
end;

procedure TAppearanceNode.SetAlphaChannel(const Value: TAutoAlphaChannel);
begin
  FdAlphaChannel.SendEnumValue(Ord(Value));
end;

class function TMFNodeShaders.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TMFNode;
end;

function TMFNodeShaders.GLSLShader(Index: Integer): TComposedShaderNode;
begin
  if Items[Index] is TComposedShaderNode then
  begin
    Result := TComposedShaderNode(Items[Index]);
    if not (Result.Language in [slDefault, slGLSL]) then
      Result := nil;
  end else
    Result := nil;
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

function TAppearanceNode.GetTexture: TAbstractTextureNode;
begin
  if FdTexture.Value is TAbstractTextureNode then
    Result := TAbstractTextureNode(FdTexture.Value)
  else
    Result := nil;
end;

procedure TAppearanceNode.SetTexture(const Value: TAbstractTextureNode);
begin
  FdTexture.Send(Value);
end;

function TAppearanceNode.GetNormalMap: TAbstractTexture2DNode;
begin
  if FdNormalMap.Value is TAbstractTexture2DNode then
    Result := TAbstractTexture2DNode(FdNormalMap.Value)
  else
    Result := nil;
end;

procedure TAppearanceNode.SetNormalMap(const Value: TAbstractTexture2DNode);
begin
  FdNormalMap.Send(Value);
end;

function TAppearanceNode.GetMaterial: TAbstractMaterialNode;
begin
  if FdMaterial.Value is TAbstractMaterialNode then
    Result := TAbstractMaterialNode(FdMaterial.Value)
  else
    Result := nil;
end;

procedure TAppearanceNode.SetMaterial(const Value: TAbstractMaterialNode);
begin
  FdMaterial.Send(Value);
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
var
  IgnoredMapping: Integer;
begin
  Result := MainTexture(IgnoredMapping);
end;

function TAppearanceNode.MainTexture(out Mapping: Integer): TAbstractTextureNode;
var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  Result := nil;
  Mapping := -1;
  {$warnings off} // using deprecated to keep backward compatibility
  SurfaceShader := CommonSurfaceShader;
  {$warnings on}
  if SurfaceShader <> nil then
  begin
    if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
    begin
      Mapping := SurfaceShader.DiffuseTextureCoordinatesId; // TODO: is this what X3DOM/InstantReality are doing? test.
      Exit(SurfaceShader.MultiDiffuseAlphaTexture);
    end;
    if SurfaceShader.DiffuseTexture <> nil then
    begin
      Mapping := SurfaceShader.DiffuseTextureCoordinatesId;
      Exit(SurfaceShader.DiffuseTexture);
    end;
  end else
  begin
    if (Material is TMaterialNode) and
       (TMaterialNode(Material).DiffuseTexture <> nil) then
    begin
      Mapping := TMaterialNode(Material).DiffuseTextureChannel;
      Exit(TMaterialNode(Material).DiffuseTexture);
    end;
    if (Material is TPhysicalMaterialNode) and
       (TPhysicalMaterialNode(Material).BaseTexture <> nil) then
    begin
      Mapping := TPhysicalMaterialNode(Material).BaseTextureChannel;
      Exit(TPhysicalMaterialNode(Material).BaseTexture);
    end;
    if (Material is TUnlitMaterialNode) and
       (TUnlitMaterialNode(Material).EmissiveTexture <> nil) then
    begin
      Mapping := TUnlitMaterialNode(Material).EmissiveTextureChannel;
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
end;

procedure TFillPropertiesNode.CreateNode;
begin
  inherited;

  FFdFilled := TSFBool.Create(Self, true, 'filled', true);
  AddField(FFdFilled);

  FFdHatchColor := TSFColor.Create(Self, true, 'hatchColor', Vector3(1, 1, 1));
  AddField(FFdHatchColor);
  { X3D specification comment: [0,1] }

  FFdHatched := TSFBool.Create(Self, true, 'hatched', true);
  AddField(FFdHatched);

  FFdHatchStyle := TSFInt32.Create(Self, true, 'hatchStyle', 1);
  AddField(FFdHatchStyle);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'fillProperties';
end;

class function TFillPropertiesNode.ClassX3DType: string;
begin
  Result := 'FillProperties';
end;

procedure TLinePropertiesNode.CreateNode;
begin
  inherited;

  FFdApplied := TSFBool.Create(Self, true, 'applied', true);
  AddField(FFdApplied);

  FFdLinetype := TSFInt32.Create(Self, true, 'linetype', 1);
  AddField(FFdLinetype);
  { X3D specification comment: [1,Inf) }

  FFdLinewidthScaleFactor := TSFFloat.Create(Self, true, 'linewidthScaleFactor', 0);
  AddField(FFdLinewidthScaleFactor);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'lineProperties';
end;

class function TLinePropertiesNode.ClassX3DType: string;
begin
  Result := 'LineProperties';
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

procedure TMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, true, 'ambientIntensity', TPhongMaterialInfo.DefaultAmbientIntensity);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdAmbientTexture := TSFNode.Create(Self, true, 'ambientTexture', [TAbstractTextureNode]);
   FdAmbientTexture.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdAmbientTexture);

  FFdAmbientTextureChannel := TSFInt32.Create(Self, true, 'ambientTextureChannel', 0);
   FdAmbientTextureChannel.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdAmbientTextureChannel);

  FFdDiffuseColor := TSFColor.Create(Self, true, 'diffuseColor', TPhongMaterialInfo.DefaultDiffuseColor);
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdDiffuseTexture := TSFNode.Create(Self, true, 'diffuseTexture', [TAbstractTextureNode]);
   FdDiffuseTexture.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdDiffuseTexture);

  FFdDiffuseTextureChannel := TSFInt32.Create(Self, true, 'diffuseTextureChannel', 0);
   FdDiffuseTextureChannel.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdDiffuseTextureChannel);

  FFdShininess := TSFFloat.Create(Self, true, 'shininess', TPhongMaterialInfo.DefaultShininess);
   FdShininess.ChangesAlways := [chMaterial2];
  AddField(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdShininessTexture := TSFNode.Create(Self, true, 'shininessTexture', [TAbstractTextureNode]);
   FdShininessTexture.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdShininessTexture);

  FFdShininessTextureChannel := TSFInt32.Create(Self, true, 'shininessTextureChannel', 0);
   FdShininessTextureChannel.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdShininessTextureChannel);

  FFdSpecularColor := TSFColor.Create(Self, true, 'specularColor', TPhongMaterialInfo.DefaultSpecularColor);
   FdSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdSpecularTexture := TSFNode.Create(Self, true, 'specularTexture', [TAbstractTextureNode]);
   FdSpecularTexture.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdSpecularTexture);

  FFdSpecularTextureChannel := TSFInt32.Create(Self, true, 'specularTextureChannel', 0);
   FdSpecularTextureChannel.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdSpecularTextureChannel);

  FFdTransparency := TSFFloat.Create(Self, true, 'transparency', TMaterialInfo.DefaultTransparency);
   FdTransparency.ChangesAlways := [chMaterial2, chAlphaChannel];
  AddField(FFdTransparency);
  { X3D specification comment: [0,1] }

  {$ifndef CASTLE_SLIM_NODES}
  FFdFogImmune := TSFBool.Create(Self, true, 'fogImmune', false);
   FdFogImmune.ChangesAlways := [chMaterial2];
  AddField(FFdFogImmune);
  {$endif}

  FFdMirror := TSFFloat.Create(Self, true, 'mirror', 0.0);
   FdMirror.ChangesAlways := [chMaterial2];
  AddField(FFdMirror);

  {$ifndef CASTLE_SLIM_NODES}
  FFdReflSpecular := TMFColor.Create(Self, true, 'reflSpecular', []);
   FdReflSpecular.ChangesAlways := [chMaterial2];
  AddField(FFdReflSpecular);

  FFdReflDiffuse := TMFColor.Create(Self, true, 'reflDiffuse', []);
   FdReflDiffuse.ChangesAlways := [chMaterial2];
  AddField(FFdReflDiffuse);

  FFdTransSpecular := TMFColor.Create(Self, true, 'transSpecular', []);
   FdTransSpecular.ChangesAlways := [chMaterial2];
  AddField(FFdTransSpecular);

  FFdTransDiffuse := TMFColor.Create(Self, true, 'transDiffuse', []);
   FdTransDiffuse.ChangesAlways := [chMaterial2];
  AddField(FFdTransDiffuse);

  FFdReflSpecularExp := TSFFloat.Create(Self, true, 'reflSpecularExp', TPhongMaterialInfo.DefaultReflSpecularExp);
   FdReflSpecularExp.ChangesAlways := [chMaterial2];
  AddField(FFdReflSpecularExp);

  FFdTransSpecularExp := TSFFloat.Create(Self, true, 'transSpecularExp', TPhongMaterialInfo.DefaultTransSpecularExp);
   FdTransSpecularExp.ChangesAlways := [chMaterial2];
  AddField(FFdTransSpecularExp);
  {$endif}
end;

destructor TMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

class function TMaterialNode.ClassX3DType: string;
begin
  Result := 'Material';
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
    Result := A.List^[0];
end;

function TMaterialNode.TMyMaterialInfo.ReflDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[0];
end;

function TMaterialNode.TMyMaterialInfo.TransSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[0];
end;

function TMaterialNode.TMyMaterialInfo.TransDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[0];
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

constructor TPhysicalMaterialNode.Create(const AX3DName: string; const ABaseUrl: string);
begin
  inherited;
end;

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

procedure TShapeNode.CreateNode;
begin
  inherited;
end;

class function TShapeNode.ClassX3DType: string;
begin
  Result := 'Shape';
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

procedure TTwoSidedMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, true, 'ambientIntensity', 0.2);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackAmbientIntensity := TSFFloat.Create(Self, true, 'backAmbientIntensity', 0.2);
   FdBackAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdBackAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackDiffuseColor := TSFColor.Create(Self, true, 'backDiffuseColor', Vector3(0.8, 0.8, 0.8));
   FdBackDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdBackEmissiveColor := TSFColor.Create(Self, true, 'backEmissiveColor', Vector3(0, 0, 0));
   FdBackEmissiveColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdBackShininess := TSFFloat.Create(Self, true, 'backShininess', 0.2);
   FdBackShininess.ChangesAlways := [chMaterial2];
  AddField(FFdBackShininess);
  { X3D specification comment: [0,1] }

  FFdBackSpecularColor := TSFColor.Create(Self, true, 'backSpecularColor', Vector3(0, 0, 0));
   FdBackSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackSpecularColor);
  { X3D specification comment: [0,1] }

  FFdBackTransparency := TSFFloat.Create(Self, true, 'backTransparency', 0);
   FdBackTransparency.ChangesAlways := [chMaterial2, chAlphaChannel];
  AddField(FFdBackTransparency);
  { X3D specification comment: [0,1] }

  FFdDiffuseColor := TSFColor.Create(Self, true, 'diffuseColor', Vector3(0.8, 0.8, 0.8));
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdEmissiveColor := TSFColor.Create(Self, true, 'emissiveColor', Vector3(0, 0, 0));
   FdEmissiveColor.ChangesAlways := [chMaterial2];
  AddField(FFdEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdShininess := TSFFloat.Create(Self, true, 'shininess', 0.2);
   FdShininess.ChangesAlways := [chMaterial2];
  AddField(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdSeparateBackColor := TSFBool.Create(Self, true, 'separateBackColor', false);
   FdSeparateBackColor.ChangesAlways := [chMaterial2];
  AddField(FFdSeparateBackColor);

  FFdSpecularColor := TSFColor.Create(Self, true, 'specularColor', Vector3(0, 0, 0));
   FdSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, true, 'transparency', 0);
   FdTransparency.ChangesAlways := [chMaterial2, chAlphaChannel];
  AddField(FFdTransparency);
  { X3D specification comment: [0,1] }
end;

destructor TTwoSidedMaterialNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

class function TTwoSidedMaterialNode.ClassX3DType: string;
begin
  Result := 'TwoSidedMaterial';
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
