{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TKambiOctreePropertiesNode = class;

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
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dmaterialnode.inc}
  end;

  TAppearanceNode = class;
  TAbstractTextureTransformNode = class;
  TMaterialNode = class;
  TComposedShaderNode = class;
  TLinePropertiesNode = class;
  TCommonSurfaceShaderNode = class;

  TShading = (shDefault, shPhong, shWireframe);

  { Base node type for all Shape nodes. }
  TAbstractShapeNode = class(TAbstractChildNode, IAbstractBoundedObject)
  private
    function GetGeometry: TAbstractX3DGeometryNode;
    procedure SetGeometry(const Value: TAbstractX3DGeometryNode);
    function GetAppearance: TAppearanceNode;
    procedure SetAppearance(const Value: TAppearanceNode);
    function GetMaterial: TMaterialNode;
    procedure SetMaterial(const Value: TMaterialNode);
    function GetTexture: TAbstractTextureNode;
    procedure SetTexture(const Value: TAbstractTextureNode);
    function GetTextureTransform: TAbstractTextureTransformNode;
    procedure SetTextureTransform(const Value: TAbstractTextureTransformNode);
    function GetLineProperties: TLinePropertiesNode;
    procedure SetLineProperties(const Value: TLinePropertiesNode);
    function GetShading: TShading;
    procedure SetShading(const Value: TShading);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    type
      TSFShading = class(TSFStringEnum)
      strict private
        WireframeChanged: boolean;
      public
        class function ExposedEventsFieldClass: TX3DFieldClass; override;
        procedure ExposedEventReceive(Event: TX3DEvent; NewValue: TX3DField; const Time: TX3DTime); override;
        function ExecuteChanges: TX3DChanges; override;
      end;

    procedure CreateNode; override;

    private FFdAppearance: TSFNode;
    public property FdAppearance: TSFNode read FFdAppearance;

    private FFdGeometry: TSFNode;
    public property FdGeometry: TSFNode read FFdGeometry;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    { Whether to render this shape.
      See http://castle-engine.sourceforge.net/x3d_implementation_shape_extensions.php }
    private FFdRender: TSFBool;
    public property FdRender: TSFBool read FFdRender;

    private FFdShading: TSFShading;
    public property FdShading: TSFShading read FFdShading;
    property Shading: TShading read GetShading write SetShading;

    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;

    { Geometry of this shape. }
    property Geometry: TAbstractX3DGeometryNode read GetGeometry write SetGeometry;

    { Apperance of this shape.

      This is a comfortable shortcut for FdAppearance.Value,
      checking whether it's class is appropriate. }
    property Appearance: TAppearanceNode read GetAppearance write SetAppearance;

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

    { Get / set the (simple, one-sided) material of this shape.
      This is a shortcut for accessing "material" field of Apperance,
      that is @code(TAppearanceNode(FdAppearance.Value).FdMaterial.Value),
      checking whether nodes are set to appropriate types along the way.

      When setting this material to something non-nil, we make sure
      to also set Appearance to something non-nil. }
    property Material: TMaterialNode read GetMaterial write SetMaterial;

    { Line properties of this shape. This is a clean, type-safe way
      for accessing Appearance.lineProperties field of this shape. }
    property LineProperties: TLinePropertiesNode
      read GetLineProperties write SetLineProperties;

    private FFdOctreeTriangles: TSFNode;
    public property FdOctreeTriangles: TSFNode read FFdOctreeTriangles;

    { Find a suitable CommonSurfaceShader to be used when rendering this shape,
      or @nil. }
    function CommonSurfaceShader: TCommonSurfaceShaderNode;

    {$I auto_generated_node_helpers/x3dnodes_x3dshapenode.inc}
  end;

  TMFNodeShaders = class(TMFNode)
  protected
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
  private
    function GetTexture: TAbstractTextureNode;
    procedure SetTexture(const Value: TAbstractTextureNode);
    function GetMaterial: TMaterialNode;
    procedure SetMaterial(const Value: TMaterialNode);
    function GetNormalMap: TAbstractTexture2DNode;
    procedure SetNormalMap(const Value: TAbstractTexture2DNode);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFillProperties: TSFNode;
    public property FdFillProperties: TSFNode read FFdFillProperties;

    private FFdLineProperties: TSFNode;
    public property FdLineProperties: TSFNode read FFdLineProperties;

    private FFdMaterial: TSFNode;
    public property FdMaterial: TSFNode read FFdMaterial;

    private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

    private FFdTexture: TSFNode;
    public property FdTexture: TSFNode read FFdTexture;

    private FFdTextureTransform: TSFNode;
    public property FdTextureTransform: TSFNode read FFdTextureTransform;

    private FFdReceiveShadows: TMFNode;
    public property FdReceiveShadows: TMFNode read FFdReceiveShadows;

    private FFdShadowCaster: TSFBool;
    public property FdShadowCaster: TSFBool read FFdShadowCaster;

    private FFdEffects: TMFNode;
    public property FdEffects: TMFNode read FFdEffects;

    private FFdNormalMap: TSFNode;
    public property FdNormalMap: TSFNode read FFdNormalMap;

    private FFdHeightMap: TSFNode;
    public property FdHeightMap: TSFNode read FFdHeightMap;

    private FFdHeightMapScale: TSFFloat;
    public property FdHeightMapScale: TSFFloat read FFdHeightMapScale;

    { blendMode property.

      This is modelled after InstantReality extension on [http://www.instantreality.org/].
      See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
      and [http://www.instantreality.org/documentation/nodetype/BlendMode/].
      It allows you to use BlendMode nodes, which I consider very useful. }
    private FFdBlendMode: TSFNode;
    public property FdBlendMode: TSFNode read FFdBlendMode;

    { The texture of this appearance.

      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property Texture: TAbstractTextureNode read GetTexture write SetTexture;

    { The normal map of this appearance.

      See https://castle-engine.sourceforge.io/x3d_implementation_texturing_extensions.php#section_ext_bump_mapping .
      This is a deprecated method of using bump mapping, a better method is to
      use CommonSurfaceShader:
      https://castle-engine.sourceforge.io/x3d_implementation_texturing_extensions.php#section_ext_common_surface_shader .

      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property NormalMap: TAbstractTexture2DNode read GetNormalMap write SetNormalMap;

    { The material of this appearance.
      This only sets the simple, one-sided material node.

      This is a comfortable property for getting and setting the appropriate
      X3D field, checking class types along the way and setting the value
      through X3D events if necessary. }
    property Material: TMaterialNode read GetMaterial write SetMaterial;

    { Find a suitable CommonSurfaceShader to be used when rendering this shape,
      or @nil. }
    function CommonSurfaceShader: TCommonSurfaceShaderNode;

    { Returns an effective diffuse (and alpha) texture node
      that should be used for this appearance, or @nil.
      This texture may come from @link(Texture) or from @link(CommonSurfaceShader). }
    function DiffuseAlphaTexture: TAbstractTextureNode;

    function MaterialProperty: TMaterialProperty;

    {$I auto_generated_node_helpers/x3dnodes_appearance.inc}
  end;

  { Additional visual properties to be applied to all polygonal areas. }
  TFillPropertiesNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFilled: TSFBool;
    public property FdFilled: TSFBool read FFdFilled;

    private FFdHatchColor: TSFColor;
    public property FdHatchColor: TSFColor read FFdHatchColor;

    private FFdHatched: TSFBool;
    public property FdHatched: TSFBool read FFdHatched;

    private FFdHatchStyle: TSFInt32;
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
    class function URNMatching(const URN: string): boolean; override;

    private FFdApplied: TSFBool;
    public property FdApplied: TSFBool read FFdApplied;

    private FFdLinetype: TSFInt32;
    public property FdLinetype: TSFInt32 read FFdLinetype;

    { Line type (pattern). }
    property LineType: TLineType read GetLineType write SetLineType default ltSolid;

    private FFdLinewidthScaleFactor: TSFFloat;
    public property FdLinewidthScaleFactor: TSFFloat read FFdLinewidthScaleFactor;

    {$I auto_generated_node_helpers/x3dnodes_lineproperties.inc}
  end;

  { Material information, that abstracts various ways to define material properties.
    This hides differences between VRML 1.0 Material,
    X3D and VRML 2.0 Material and CommonSurfaceShader. }
  TMaterialInfo = class
  protected
    { Calculate physical material properties using standard VRML material
      fields. See [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_material_phong_brdf_fields].

      These should be used by descendants implementation,
      for example CalculateReflSpecular should be used by descendant
      ReflSpecular when material node doesn't specify any value
      in FdReflSpecular field.

      @groupBegin }
    procedure CalculateReflSpecular(out V: TVector3);
    procedure CalculateReflDiffuse(out V: TVector3);
    procedure CalculateTransSpecular(out V: TVector3);
    procedure CalculateTransDiffuse(out V: TVector3);
    { @groupEnd }
  public
    const
      { Default material parameters.

        They luckily match between all the material-like nodes:
        @unorderedList(
          @itemSpacing Compact
          @item X3D and VRML 2.0: TMaterialNode
          @item VRML 1.0: TMaterialNode_1
          @item CommonSurfaceShader: TCommonSurfaceShaderNode
        )

        The AmbientColor is an exception, the effective ambientColor for X3D is
        @code(ambientIntensity * diffuseColor = (0.2 * 0.8, 0.2 * 0.8, 0.2 * 0.8)).
      }
      DefaultAmbientColor: TVector3 = (Data: (0.2, 0.2, 0.2));
      DefaultAmbientIntensity = 0.2;
      DefaultDiffuseColor: TVector3 = (Data: (0.8, 0.8, 0.8));
      DefaultSpecularColor: TVector3 = (Data: (0, 0, 0));
      DefaultEmissiveColor: TVector3 = (Data: (0, 0, 0));
      DefaultShininess = 0.2;
      DefaultShininessExp = DefaultShininess * 128;
      DefaultTransparency = 0.0;
      DefaultReflectionColor: TVector3 = (Data: (0, 0, 0));
      DefaultTransmissionColor: TVector3 = (Data: (0, 0, 0));
      DefaultReflSpecularExp = 1000000;
      DefaultTransSpecularExp = 1000000;

    function AmbientColor: TVector3; virtual; abstract;
    function DiffuseColor: TVector3; virtual; abstract;
    function SpecularColor: TVector3; virtual; abstract;
    function EmissiveColor: TVector3; virtual; abstract;
    function Shininess: Single; virtual; abstract;
    function ShininessExp: Single;
    function ReflectionColor: TVector3; virtual; abstract;
    function TransmissionColor: TVector3; virtual;
    function Transparency: Single; virtual; abstract;

    function ReflSpecular: TVector3; virtual; abstract;
    function ReflDiffuse: TVector3; virtual; abstract;
    function TransSpecular: TVector3; virtual; abstract;
    function TransDiffuse: TVector3; virtual; abstract;

    function ReflSpecularExp: Single; virtual; abstract;
    function TransSpecularExp: Single; virtual; abstract;
  end;

  TX3DMaterialInfoAbstract = TMaterialInfo deprecated 'use TMaterialInfo';

  { Surface material properties for associated geometry nodes,
    used by the lighting equations during rendering. }
  TMaterialNode = class(TAbstractMaterialNode)
  strict private
    type
      TX3DMaterialInfo = class(TMaterialInfo)
      private
        FNode: TMaterialNode;
      public
        constructor Create(Node: TMaterialNode);

        function AmbientColor: TVector3; override;
        function DiffuseColor: TVector3; override;
        function SpecularColor: TVector3; override;
        function EmissiveColor: TVector3; override;
        function Shininess: Single; override;
        function ReflectionColor: TVector3; override;
        function Transparency: Single; override;

        function ReflSpecular: TVector3; override;
        function ReflDiffuse: TVector3; override;
        function TransSpecular: TVector3; override;
        function TransDiffuse: TVector3; override;

        function ReflSpecularExp: Single; override;
        function TransSpecularExp: Single; override;
      end;
    var
      FMaterialInfo: TX3DMaterialInfo;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    private FFdEmissiveColor: TSFColor;
    public property FdEmissiveColor: TSFColor read FFdEmissiveColor;

    private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    private FFdFogImmune: TSFBool;
    { Make the object not affected by fog.
      This is a Castle Game Engine extension, not present in X3D standard. }
    public property FdFogImmune: TSFBool read FFdFogImmune;

    private FFdMirror: TSFFloat;
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

    { Material information based on this node.
      It is automatically updated when properties of this material change.
      Do not free it yourself, it will be automatically freed when
      this node is freed. }
    function MaterialInfo: TMaterialInfo;

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
    procedure ForcePureEmissive;

    private FFdReflSpecular: TMFColor;
    public property FdReflSpecular: TMFColor read FFdReflSpecular;

    private FFdReflDiffuse: TMFColor;
    public property FdReflDiffuse: TMFColor read FFdReflDiffuse;

    private FFdTransSpecular: TMFColor;
    public property FdTransSpecular: TMFColor read FFdTransSpecular;

    private FFdTransDiffuse: TMFColor;
    public property FdTransDiffuse: TMFColor read FFdTransDiffuse;

    private FFdReflSpecularExp: TSFFloat;
    public property FdReflSpecularExp: TSFFloat read FFdReflSpecularExp;

    private FFdTransSpecularExp: TSFFloat;
    public property FdTransSpecularExp: TSFFloat read FFdTransSpecularExp;

    {$I auto_generated_node_helpers/x3dnodes_material.inc}
  end;
  TMaterialNode_2 = TMaterialNode;

  { Shape is a rendered object in the world, with an appearance and geometry. }
  TShapeNode = class(TAbstractShapeNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_shape.inc}
  end;

  { Material properties that can effect both the front and back side
    of a polygon individually.
    @bold(Not implemented yet.) }
  TTwoSidedMaterialNode = class(TAbstractMaterialNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdBackAmbientIntensity: TSFFloat;
    public property FdBackAmbientIntensity: TSFFloat read FFdBackAmbientIntensity;

    private FFdBackDiffuseColor: TSFColor;
    public property FdBackDiffuseColor: TSFColor read FFdBackDiffuseColor;

    private FFdBackEmissiveColor: TSFColor;
    public property FdBackEmissiveColor: TSFColor read FFdBackEmissiveColor;

    private FFdBackShininess: TSFFloat;
    public property FdBackShininess: TSFFloat read FFdBackShininess;

    private FFdBackSpecularColor: TSFColor;
    public property FdBackSpecularColor: TSFColor read FFdBackSpecularColor;

    private FFdBackTransparency: TSFFloat;
    public property FdBackTransparency: TSFFloat read FFdBackTransparency;

    private FFdDiffuseColor: TSFColor;
    public property FdDiffuseColor: TSFColor read FFdDiffuseColor;

    private FFdEmissiveColor: TSFColor;
    public property FdEmissiveColor: TSFColor read FFdEmissiveColor;

    private FFdShininess: TSFFloat;
    public property FdShininess: TSFFloat read FFdShininess;

    private FFdSeparateBackColor: TSFBool;
    public property FdSeparateBackColor: TSFBool read FFdSeparateBackColor;

    private FFdSpecularColor: TSFColor;
    public property FdSpecularColor: TSFColor read FFdSpecularColor;

    private FFdTransparency: TSFFloat;
    public property FdTransparency: TSFFloat read FFdTransparency;

    {$I auto_generated_node_helpers/x3dnodes_twosidedmaterial.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractMaterialNode.CreateNode;
begin
  inherited;

  { This is not actually specified anywhere (X3D XML encoding spec
    doesn't specify containerField for abstract X3DXxxNode classes)
    but it seems most sensible. }

  DefaultContainerField := 'material';
end;

{ TSFShading ----------------------------------------------------------------- }

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

{ TAbstractShapeNode --------------------------------------------------------- }

procedure TAbstractShapeNode.CreateNode;
const
  ShadingNames: array [TShading] of string = ('DEFAULT', 'PHONG', 'WIREFRAME');
begin
  inherited;

  FFdAppearance := TSFNode.Create(Self, 'appearance', [TAbstractAppearanceNode]);
   FdAppearance.ChangesAlways := [chEverything];
  AddField(FFdAppearance);

  FFdGeometry := TSFNode.Create(Self, 'geometry', [TAbstractX3DGeometryNode]);
   FdGeometry.ChangesAlways := [chEverything];
  AddField(FFdGeometry);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3(0, 0, 0));
   FdBboxCenter.Exposed := false;
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3(-1, -1, -1));
   FdBboxSize.Exposed := false;
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  FFdRender := TSFBool.Create(Self, 'render', true);
  AddField(FFdRender);

  FFdShading := TSFShading.Create(Self, 'shading', ShadingNames, Ord(shDefault));
   FdShading.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdShading);

  FFdOctreeTriangles := TSFNode.Create(Self, 'octreeTriangles', [TKambiOctreePropertiesNode]);
   FdOctreeTriangles.Exposed := false;
   FdOctreeTriangles.ChangesAlways := [chEverything];
  AddField(FFdOctreeTriangles);
end;

function TAbstractShapeNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;

  { According to VRML spec, when geometry is NULL then object is not
    drawn so appearance doesn't matter. }

  if FdGeometry.CurrentChildAllowed and
     (FdGeometry.Value <> nil) then
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

function TAbstractShapeNode.GetGeometry: TAbstractX3DGeometryNode;
begin
  if FdGeometry.Value is TAbstractX3DGeometryNode then
    Result := TAbstractX3DGeometryNode(FdGeometry.Value)
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetGeometry(const Value: TAbstractX3DGeometryNode);
begin
  FdGeometry.Send(Value);
end;

function TAbstractShapeNode.GetAppearance: TAppearanceNode;
begin
  if FdAppearance.Value is TAppearanceNode then
    Result := TAppearanceNode(FdAppearance.Value)
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetAppearance(const Value: TAppearanceNode);
begin
  FdAppearance.Send(Value);
end;

function TAbstractShapeNode.GetMaterial: TMaterialNode;
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
    Result := App.CommonSurfaceShader
  else
    Result := nil;
end;

procedure TAbstractShapeNode.SetMaterial(const Value: TMaterialNode);
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

function TAbstractShapeNode.GetShading: TShading;
begin
  Result := TShading(FdShading.EnumValue);
end;

procedure TAbstractShapeNode.SetShading(const Value: TShading);
begin
  FdShading.SendEnumValue(Ord(Value));
end;

procedure TAppearanceNode.CreateNode;
begin
  inherited;

  FFdFillProperties := TSFNode.Create(Self, 'fillProperties', [TFillPropertiesNode]);
   FdFillProperties.ChangesAlways := [chEverything];
  AddField(FFdFillProperties);

  FFdLineProperties := TSFNode.Create(Self, 'lineProperties', [TLinePropertiesNode]);
   FdLineProperties.ChangesAlways := [chEverything];
  AddField(FFdLineProperties);

  FFdMaterial := TSFNode.Create(Self, 'material', [TAbstractMaterialNode]);
   FdMaterial.ChangesAlways := [chEverything];
  AddField(FFdMaterial);

  FFdShaders := TMFNodeShaders.Create(Self, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  AddField(FFdShaders);

  FFdTexture := TSFNode.Create(Self, 'texture', [TAbstractTextureNode]);
   FdTexture.ChangesAlways := [chEverything];
  AddField(FFdTexture);

  FFdTextureTransform := TSFNode.Create(Self, 'textureTransform', [TAbstractTextureTransformNode]);
   FdTextureTransform.ChangesAlways := [chEverything];
  AddField(FFdTextureTransform);

  FFdReceiveShadows := TMFNode.Create(Self, 'receiveShadows', [TAbstractLightNode]);
   FdReceiveShadows.Exposed := false;
   FdReceiveShadows.ChangesAlways := [chShadowMaps];
  AddField(FFdReceiveShadows);

  FFdShadowCaster := TSFBool.Create(Self, 'shadowCaster', true);
   FdShadowCaster.ChangesAlways := [chShadowCasters];
  AddField(FFdShadowCaster);

  FFdNormalMap := TSFNode.Create(Self, 'normalMap', [TAbstractTextureNode]);
   FdNormalMap.ChangesAlways := [chEverything];
  AddField(FFdNormalMap);

  FFdHeightMap := TSFNode.Create(Self, 'heightMap', [TAbstractTextureNode]);
   FdHeightMap.ChangesAlways := [chEverything];
  AddField(FFdHeightMap);

  FFdHeightMapScale := TSFFloat.Create(Self, 'heightMapScale', DefaultHeightMapScale);
   FdHeightMapScale.ChangesAlways := [chEverything];
  AddField(FFdHeightMapScale);

  FFdBlendMode := TSFNode.Create(Self, 'blendMode', [TBlendModeNode]);
   FdBlendMode.ChangesAlways := [chEverything];
  AddField(FFdBlendMode);

  FFdEffects := TMFNode.Create(Self, 'effects', [TEffectNode]);
   FdEffects.Exposed := false;
   FdEffects.ChangesAlways := [chEverything];
  AddField(FFdEffects);

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

class function TAppearanceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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
    if not ((Result.FdLanguage.Value = '') or
            (Result.FdLanguage.Value = 'GLSL')) then
      Result := nil;
  end else
    Result := nil;
end;

function TAppearanceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FFdFillProperties.Enumerate(Func);
  if Result <> nil then Exit;

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

function TAppearanceNode.GetMaterial: TMaterialNode;
begin
  if FdMaterial.Value is TMaterialNode then
    Result := TMaterialNode(FdMaterial.Value)
  else
    Result := nil;
end;

procedure TAppearanceNode.SetMaterial(const Value: TMaterialNode);
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

function TAppearanceNode.DiffuseAlphaTexture: TAbstractTextureNode;
var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  Result := nil;
  SurfaceShader := CommonSurfaceShader;
  if SurfaceShader <> nil then
  begin
    if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
      Exit(SurfaceShader.MultiDiffuseAlphaTexture);
    if SurfaceShader.DiffuseTexture <> nil then
      Exit(SurfaceShader.DiffuseTexture);
  end else
    Result := Texture;
end;

function TAppearanceNode.MaterialProperty: TMaterialProperty;

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

  Tex := DiffuseAlphaTexture;
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

  MP := MaterialProperty;

  if (MP <> nil) and (MP.NormalMap <> '') and (FdNormalMap.Value = nil) then
  begin
    { use normalMap from MaterialProperty, if our normalMap field is empty now }
    NormalMapNode := TImageTextureNode.Create('', BaseUrl);
    NormalMapNode.FdUrl.Items.Add(MP.NormalMap);
    FdNormalMap.Value := NormalMapNode;
  end;

  if (MP <> nil) and (MP.AlphaChannel <> '') and (Texture <> nil) then
    Texture.FdAlphaChannel.Value := MP.AlphaChannel;
end;

procedure TFillPropertiesNode.CreateNode;
begin
  inherited;

  FFdFilled := TSFBool.Create(Self, 'filled', true);
  AddField(FFdFilled);

  FFdHatchColor := TSFColor.Create(Self, 'hatchColor', Vector3(1, 1, 1));
  AddField(FFdHatchColor);
  { X3D specification comment: [0,1] }

  FFdHatched := TSFBool.Create(Self, 'hatched', true);
  AddField(FFdHatched);

  FFdHatchStyle := TSFInt32.Create(Self, 'hatchStyle', 1);
  AddField(FFdHatchStyle);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'fillProperties';
end;

class function TFillPropertiesNode.ClassX3DType: string;
begin
  Result := 'FillProperties';
end;

class function TFillPropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TLinePropertiesNode.CreateNode;
begin
  inherited;

  FFdApplied := TSFBool.Create(Self, 'applied', true);
  AddField(FFdApplied);

  FFdLinetype := TSFInt32.Create(Self, 'linetype', 1);
  AddField(FFdLinetype);
  { X3D specification comment: [1,Inf) }

  FFdLinewidthScaleFactor := TSFFloat.Create(Self, 'linewidthScaleFactor', 0);
  AddField(FFdLinewidthScaleFactor);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'lineProperties';
end;

class function TLinePropertiesNode.ClassX3DType: string;
begin
  Result := 'LineProperties';
end;

class function TLinePropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
    else raise EInternalError.Create('TLinePropertiesNode.SetLineType:this Value not implemented');
  end;
end;

procedure TMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, 'ambientIntensity', TMaterialInfo.DefaultAmbientIntensity);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdDiffuseColor := TSFColor.Create(Self, 'diffuseColor', TMaterialInfo.DefaultDiffuseColor);
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdEmissiveColor := TSFColor.Create(Self, 'emissiveColor', TMaterialInfo.DefaultEmissiveColor);
   FdEmissiveColor.ChangesAlways := [chMaterial2];
  AddField(FFdEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdShininess := TSFFloat.Create(Self, 'shininess', TMaterialInfo.DefaultShininess);
   FdShininess.ChangesAlways := [chMaterial2];
  AddField(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdSpecularColor := TSFColor.Create(Self, 'specularColor', TMaterialInfo.DefaultSpecularColor);
   FdSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, 'transparency', TMaterialInfo.DefaultTransparency);
   FdTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  AddField(FFdTransparency);
  { X3D specification comment: [0,1] }

  FFdFogImmune := TSFBool.Create(Self, 'fogImmune', false);
   FdFogImmune.ChangesAlways := [chMaterial2];
  AddField(FFdFogImmune);

  FFdMirror := TSFFloat.Create(Self, 'mirror', 0.0);
   FdMirror.ChangesAlways := [chMaterial2];
  AddField(FFdMirror);

  FFdReflSpecular := TMFColor.Create(Self, 'reflSpecular', []);
   FdReflSpecular.ChangesAlways := [chMaterial2];
  AddField(FFdReflSpecular);

  FFdReflDiffuse := TMFColor.Create(Self, 'reflDiffuse', []);
   FdReflDiffuse.ChangesAlways := [chMaterial2];
  AddField(FFdReflDiffuse);

  FFdTransSpecular := TMFColor.Create(Self, 'transSpecular', []);
   FdTransSpecular.ChangesAlways := [chMaterial2];
  AddField(FFdTransSpecular);

  FFdTransDiffuse := TMFColor.Create(Self, 'transDiffuse', []);
   FdTransDiffuse.ChangesAlways := [chMaterial2];
  AddField(FFdTransDiffuse);

  FFdReflSpecularExp := TSFFloat.Create(Self, 'reflSpecularExp', TMaterialInfo.DefaultReflSpecularExp);
   FdReflSpecularExp.ChangesAlways := [chMaterial2];
  AddField(FFdReflSpecularExp);

  FFdTransSpecularExp := TSFFloat.Create(Self, 'transSpecularExp', TMaterialInfo.DefaultTransSpecularExp);
   FdTransSpecularExp.ChangesAlways := [chMaterial2];
  AddField(FFdTransSpecularExp);
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

class function TMaterialNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
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
    FMaterialInfo := TX3DMaterialInfo.Create(Self);
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

{ TMaterialInfo ---------------------------------------------------------- }

procedure TMaterialInfo.CalculateReflSpecular(out V: TVector3);
begin
  V := ReflectionColor;
end;

procedure TMaterialInfo.CalculateReflDiffuse(out V: TVector3);
begin
  V := DiffuseColor;
end;

procedure TMaterialInfo.CalculateTransSpecular(out V: TVector3);
begin
  V := TransmissionColor;
end;

procedure TMaterialInfo.CalculateTransDiffuse(out V: TVector3);
begin
  V := TVector3.MultiplyComponents(DiffuseColor, TransmissionColor);
end;

function TMaterialInfo.ShininessExp: Single;
begin
  Result := Clamped(Shininess * 128.0, 0.0, 128.0);
end;

function TMaterialInfo.TransmissionColor: TVector3;
begin
  { default TransmisionColor implementation }
  Result := Vector3(Transparency, Transparency, Transparency);
end;

{ TMaterialNode.TX3DMaterialInfo -------------------------------------------------------- }

constructor TMaterialNode.TX3DMaterialInfo.Create(Node: TMaterialNode);
begin
  inherited Create;
  FNode := Node;
end;

function TMaterialNode.TX3DMaterialInfo.AmbientColor: TVector3;
begin
  Result := FNode.FdDiffuseColor.Value * FNode.FdAmbientIntensity.Value;
end;

function TMaterialNode.TX3DMaterialInfo.DiffuseColor: TVector3;
begin
  Result := FNode.FdDiffuseColor.Value;
end;

function TMaterialNode.TX3DMaterialInfo.SpecularColor: TVector3;
begin
  Result := FNode.FdSpecularColor.Value;
end;

function TMaterialNode.TX3DMaterialInfo.EmissiveColor: TVector3;
begin
  Result := FNode.FdEmissiveColor.Value;
end;

function TMaterialNode.TX3DMaterialInfo.Shininess: Single;
begin
  Result := FNode.FdShininess.Value;
end;

function TMaterialNode.TX3DMaterialInfo.ReflectionColor: TVector3;
begin
  Result := Vector3(
    FNode.FdMirror.Value,
    FNode.FdMirror.Value,
    FNode.FdMirror.Value
  );
end;

function TMaterialNode.TX3DMaterialInfo.Transparency: Single;
begin
  Result := FNode.FdTransparency.Value;
end;

function TMaterialNode.TX3DMaterialInfo.ReflSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    CalculateReflSpecular(Result) else
    Result := A.L[0];
end;

function TMaterialNode.TX3DMaterialInfo.ReflDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    CalculateReflDiffuse(Result) else
    Result := A.L[0];
end;

function TMaterialNode.TX3DMaterialInfo.TransSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    CalculateTransSpecular(Result) else
    Result := A.L[0];
end;

function TMaterialNode.TX3DMaterialInfo.TransDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    CalculateTransDiffuse(Result) else
    Result := A.L[0];
end;

function TMaterialNode.TX3DMaterialInfo.ReflSpecularExp: Single;
begin
  Result := FNode.FdReflSpecularExp.Value;
end;

function TMaterialNode.TX3DMaterialInfo.TransSpecularExp: Single;
begin
  Result := FNode.FdTransSpecularExp.Value;
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

class function TShapeNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTwoSidedMaterialNode.CreateNode;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, 'ambientIntensity', 0.2);
   FdAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackAmbientIntensity := TSFFloat.Create(Self, 'backAmbientIntensity', 0.2);
   FdBackAmbientIntensity.ChangesAlways := [chMaterial2];
  AddField(FFdBackAmbientIntensity);
  { X3D specification comment: [0,1] }

  FFdBackDiffuseColor := TSFColor.Create(Self, 'backDiffuseColor', Vector3(0.8, 0.8, 0.8));
   FdBackDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdBackEmissiveColor := TSFColor.Create(Self, 'backEmissiveColor', Vector3(0, 0, 0));
   FdBackEmissiveColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdBackShininess := TSFFloat.Create(Self, 'backShininess', 0.2);
   FdBackShininess.ChangesAlways := [chMaterial2];
  AddField(FFdBackShininess);
  { X3D specification comment: [0,1] }

  FFdBackSpecularColor := TSFColor.Create(Self, 'backSpecularColor', Vector3(0, 0, 0));
   FdBackSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdBackSpecularColor);
  { X3D specification comment: [0,1] }

  FFdBackTransparency := TSFFloat.Create(Self, 'backTransparency', 0);
   FdBackTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  AddField(FFdBackTransparency);
  { X3D specification comment: [0,1] }

  FFdDiffuseColor := TSFColor.Create(Self, 'diffuseColor', Vector3(0.8, 0.8, 0.8));
   FdDiffuseColor.ChangesAlways := [chMaterial2];
  AddField(FFdDiffuseColor);
  { X3D specification comment: [0,1] }

  FFdEmissiveColor := TSFColor.Create(Self, 'emissiveColor', Vector3(0, 0, 0));
   FdEmissiveColor.ChangesAlways := [chMaterial2];
  AddField(FFdEmissiveColor);
  { X3D specification comment: [0,1] }

  FFdShininess := TSFFloat.Create(Self, 'shininess', 0.2);
   FdShininess.ChangesAlways := [chMaterial2];
  AddField(FFdShininess);
  { X3D specification comment: [0,1] }

  FFdSeparateBackColor := TSFBool.Create(Self, 'separateBackColor', false);
   FdSeparateBackColor.ChangesAlways := [chMaterial2];
  AddField(FFdSeparateBackColor);

  FFdSpecularColor := TSFColor.Create(Self, 'specularColor', Vector3(0, 0, 0));
   FdSpecularColor.ChangesAlways := [chMaterial2];
  AddField(FFdSpecularColor);
  { X3D specification comment: [0,1] }

  FFdTransparency := TSFFloat.Create(Self, 'transparency', 0);
   FdTransparency.ChangesAlways := [chMaterial2, chUseBlending];
  AddField(FFdTransparency);
  { X3D specification comment: [0,1] }
end;

class function TTwoSidedMaterialNode.ClassX3DType: string;
begin
  Result := 'TwoSidedMaterial';
end;

class function TTwoSidedMaterialNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterShapeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TAppearanceNode,
    TFillPropertiesNode,
    TLinePropertiesNode,
    TMaterialNode,
    TShapeNode,
    TTwoSidedMaterialNode
  ]);
end;

{$endif read_implementation}
