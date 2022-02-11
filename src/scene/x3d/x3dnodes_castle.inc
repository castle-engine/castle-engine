{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined specifically by Castle Game Engine.
  See [https://castle-engine.io/x3d_extensions.php]. } { }

{$ifdef read_interface}
  { Headlight configuration, allows to customize headlight look and shape.
    See https://castle-engine.io/x3d_implementation_navigation_extensions.php#section_ext_headlight . }
  TKambiHeadLightNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    strict private FFdAttenuation: TSFVec3f;
    public property FdAttenuation: TSFVec3f read FFdAttenuation;

    strict private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    strict private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    strict private FFdSpot: TSFBool;
    public property FdSpot: TSFBool read FFdSpot;

    strict private FFdSpotCutOffAngle: TSFFloat;
    public property FdSpotCutOffAngle: TSFFloat read FFdSpotCutOffAngle;

    strict private FFdSpotDropOffRate: TSFFloat;
    public property FdSpotDropOffRate: TSFFloat read FFdSpotDropOffRate;

    {$I auto_generated_node_helpers/x3dnodes_kambiheadlight.inc}
  end;

  { Text with 3D depth.
    @bold(The depth is not implemented now -- this is equivalent to @link(TTextNode) now.) }
  TText3DNode = class(TTextNode)
    {$I auto_generated_node_helpers/x3dnodes_text3d.inc}
  end;

  { Customize blending (partial transparency) mode.
    See https://castle-engine.io/x3d_extensions.php#section_ext_blending .

    This is an extension, not present in standard X3D.
    It is compatible with modelled after, and compatible with,
    and InstantReality extension -- see
    [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
    and [http://www.instantreality.org/documentation/nodetype/BlendMode/]. }
  TBlendModeNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdSrcFactor: TSFString;
    public property FdSrcFactor: TSFString read FFdSrcFactor;

    strict private FFdDestFactor: TSFString;
    public property FdDestFactor: TSFString read FFdDestFactor;

    strict private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    strict private FFdColorTransparency: TSFFloat;
    public property FdColorTransparency: TSFFloat read FFdColorTransparency;

    {$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
  end;

  { Appearance node with a special name @code(KambiAppearance).
    Useful if you want to use EXTERNPROTO to declare Castle Game Engine
    extensions the Appearance in a clean way. }
  TKambiAppearanceNode = class(TAppearanceNode)
    {$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
  end;

  { Embed a scene from another file, and replace some of it's nodes.
    See https://castle-engine.io/x3d_extensions.php#section_ext_kambi_inline . }
  TKambiInlineNode = class(TInlineNode)
  strict private
    { This is internal only for DoReplaceNodes, initialized in AfterLoadInlined. }
    ReplaceCount: Integer;
    procedure DoReplaceNodes(ParentNode: TX3DNode; var Node: TX3DNode);
  protected
    procedure AfterLoadInlined; override;
  public
    {$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
  end;

  TSceneOctreeProperties = (
    opRendering,
    opDynamicCollisions,
    opVisibleTriangles,
    opStaticCollisions);

  { Navigation information with some Castle Game Engine extensions.
    See https://castle-engine.io/x3d_implementation_navigation_extensions.php . }
  TKambiNavigationInfoNode = class(TNavigationInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdTimeOriginAtLoad: TSFBool;
    public property FdTimeOriginAtLoad: TSFBool read FFdTimeOriginAtLoad;

    strict private FFdOctreeRendering: TSFNode;
    public property FdOctreeRendering: TSFNode read FFdOctreeRendering;

    strict private FFdOctreeDynamicCollisions: TSFNode;
    public property FdOctreeDynamicCollisions: TSFNode read FFdOctreeDynamicCollisions;

    strict private FFdOctreeVisibleTriangles: TSFNode;
    public property FdOctreeVisibleTriangles: TSFNode read FFdOctreeVisibleTriangles;

    strict private FFdOctreeStaticCollisions: TSFNode;
    public property FdOctreeStaticCollisions: TSFNode read FFdOctreeStaticCollisions;

    strict private FFdHeadBobbing: TSFFloat;
    public property FdHeadBobbing: TSFFloat read FFdHeadBobbing;

    strict private FFdHeadBobbingTime: TSFFloat;
    public property FdHeadBobbingTime: TSFFloat read FFdHeadBobbingTime;

    strict private FFdHeadlightNode: TSFNode;
    public property FdHeadlightNode: TSFNode read FFdHeadlightNode;

    { Return value of appropriate FdOctreeXxx field.
      Returns @nil if the field doesn't have allowed (TKambiOctreePropertiesNode)
      value (or if the field is actually @nil, naturally). }
    function OctreeProperties(OP: TSceneOctreeProperties): TKambiOctreePropertiesNode;

    {$I auto_generated_node_helpers/x3dnodes_kambinavigationinfo.inc}
  end;

  TKambiOctreePropertiesNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdMaxDepth: TSFInt32;
    public property FdMaxDepth: TSFInt32 read FFdMaxDepth;

    strict private FFdLeafCapacity: TSFInt32;
    public property FdLeafCapacity: TSFInt32 read FFdLeafCapacity;

    { Override Limits fields, where our own fields have values <> -1. }
    procedure OverrideLimits(var Limits: TOctreeLimits);

    {$I auto_generated_node_helpers/x3dnodes_kambioctreeproperties.inc}
  end;

  TShadowMapCompareMode = (smCompareRLEqual, smCompareRGEqual, smNone);

  { Generate texture by rendering a depth (shadow) map.
    See https://castle-engine.io/x3d_extensions_shadow_maps.php }
  TGeneratedShadowMapNode = class(TAbstractSingleTextureNode)
  strict private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
    function GetCompareMode: TShadowMapCompareMode;
    procedure SetCompareMode(const Value: TShadowMapCompareMode);
    function GetUpdate: TTextureUpdate;
    procedure SetUpdate(const Value: TTextureUpdate);
  public
    const
      DefaultSize = 128;
      DefaultScale = 4.0;
      DefaultBias = 4.0;

    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassX3DType: string; override;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;

    strict private FFdUpdate: TSFTextureUpdate;
    public property FdUpdate: TSFTextureUpdate read FFdUpdate;

    { When to update the texture.
      See the specification of the "update" field of the GeneratedShadowMap
      on https://castle-engine.io/x3d_extensions_shadow_maps.php .

      You should usually use this property instead of accessing @link(FdUpdate),
      it will change FdUpdate.Value and call FdUpdate.Changed. }
    public property Update: TTextureUpdate read GetUpdate write SetUpdate;

    strict private FFdSize: TSFInt32;
    public property FdSize: TSFInt32 read FFdSize;

    strict private FFdLight: TSFNode;
    public property FdLight: TSFNode read FFdLight;

    strict private FFdScale: TSFFloat;
    public property FdScale: TSFFloat read FFdScale;

    strict private FFdBias: TSFFloat;
    public property FdBias: TSFFloat read FFdBias;

    strict private FFdCompareMode: TSFStringEnum;
    public property FdCompareMode: TSFStringEnum read FFdCompareMode;
    property CompareMode: TShadowMapCompareMode read GetCompareMode write SetCompareMode;

    {$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
  end;

  { Generate texture coordinates by projection.
    See https://castle-engine.io/x3d_extensions_shadow_maps.php }
  TProjectedTextureCoordinateNode = class(TAbstractSingleTextureCoordinateNode)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;

    strict private FFdProjector: TSFNode;
    public property FdProjector: TSFNode read FFdProjector;

    {$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
  end;

  { Screen-space shader effect.
    See https://castle-engine.io/x3d_extensions_screen_effects.php . }
  TScreenEffectNode = class(TAbstractChildNode)
  strict private
    FShader: TObject;
    FShaderLoaded: boolean;
    FStateForShaderPrepare: TX3DGraphTraverseState;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    strict private FFdShaders: TMFNode;
    public property FdShaders: TMFNode read FFdShaders;

    strict private FFdNeedsDepth: TSFBool;
    public property FdNeedsDepth: TSFBool read FFdNeedsDepth;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Shader and ShaderLoaded properties are internally managed by the renderer.
      Right now, this means TCastleScene.
      Always when ShaderLoaded = @false then Shader is @nil.
      When ShaderLoaded = @true, the Shader may be non-nil (if was enabled
      and loaded successfully) or not.
      @groupBegin }
    property Shader: TObject read FShader write FShader;
    property ShaderLoaded: boolean read FShaderLoaded write FShaderLoaded;
    property StateForShaderPrepare: TX3DGraphTraverseState
      read FStateForShaderPrepare;
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_screeneffect.inc}
  end;

  { Internal notes to MultiGeneratedTextureCoordinate:

    It's implemented as simple MultiTextureCoordinate descendant,
    this is conforming with the specification. (MultiTextureCoordinate
    allows a little more children, but this will be detected and ignored
    by renderer.)

    Also, this way fields like Sphere.texCoord may require just
    MultiTextureCoordinate, and so I have backward compatibility
    and also accept MultiGeneratedTextureCoordinate.

    Also, shadow maps processing can just use MultiTextureCoordinate
    (regardless if we're on primitives or not). }

  { Generated texture coordinates for multi-texturing.
    See [https://castle-engine.io/x3d_extensions.php#section_ext_tex_coord]. }
  TMultiGeneratedTextureCoordinateNode = class(TMultiTextureCoordinateNode)
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
  end;

  TEffectPartNode = class;

  { Shader effect, that can be composed with other effect or standard rendering.
    See https://castle-engine.io/compositing_shaders.php . }
  TEffectNode = class(TAbstractChildNode)
  strict private
    function GetLanguage: TShaderLanguage;
    procedure SetLanguage(const Value: TShaderLanguage);
  protected
    procedure GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdLanguage: TSFStringEnum;
    public property FdLanguage: TSFStringEnum read FFdLanguage;
    property Language: TShaderLanguage read GetLanguage write SetLanguage;

    strict private FFdParts: TMFNode;
    public property FdParts: TMFNode read FFdParts;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    {$I auto_generated_node_helpers/x3dnodes_effect.inc}
  end;

  { Part of a shader effect, used with @link(TEffectNode).
    See https://castle-engine.io/compositing_shaders.php . }
  TEffectPartNode = class(TAbstractNode)
  strict private
    ContentsCached: boolean;
    ContentsCache: string;
    procedure EventUrlReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    function GetShaderType: TShaderType;
    procedure SetShaderType(const Value: TShaderType);
    function GetContents: string;
    procedure SetContents(const Value: string);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdType: TSFStringEnum;
    public property FdType: TSFStringEnum read FFdType;

    { Shader type. }
    property ShaderType: TShaderType read GetShaderType write SetShaderType;

    strict private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { The shader code.

      Getting this automatically loads the shader code pointed by the URL field
      of this node. The shader code may be specified in an external file,
      or inline using "data URI", see https://castle-engine.io/manual_network.php .
      The shader code may also be specified using the XML "CDATA" in X3D XML encoding.
      Returns '' of no valid URLs are present
      (loading errors are reported to WritelnWarning).

      Setting this overrides the URL field, to specify only the given shader code. }
    property Contents: string read GetContents write SetContents;

    {$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
  end;

  { Procedural texture defined by shaders.
    See https://castle-engine.io/compositing_shaders.php . }
  TShaderTextureNode = class(TAbstractSingleTextureNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDefaultTexCoord: TSFString;
    public property FdDefaultTexCoord: TSFString read FFdDefaultTexCoord;

    {$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
  end;

  { Viewpoint for making reflections on flat objects.
    See https://castle-engine.io/x3d_extensions_mirror_plane.php . }
  TViewpointMirrorNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_viewpointmirror.inc}
  end;

  { Interpolate (animate) a 2D orientation,
    during which angle changes but axis remains constant. }
  TOrientationInterpolator2DNode = class(TAbstractInterpolatorNode)
  strict private
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    strict private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    { Event out } { }
    strict private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator2d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TKambiHeadLightNode -------------------------------------------------------- }

class function TKambiHeadLightNode.ClassX3DType: string;
begin
  Result := 'KambiHeadLight';
end;

procedure TKambiHeadLightNode.CreateNode;
const
  HeadLightDefaultAmbientIntensity = 0;
  HeadLightDefaultAttenuation: TVector3 = (X: 1; Y: 0; Z: 0);
  HeadLightDefaultColor: TVector3 = (X: 1; Y: 1; Z: 1);
  HeadLightDefaultIntensity = 1.0;
  HeadLightDefaultSpot = false;
  HeadLightDefaultSpotCutOffAngle = Pi / 4;
  HeadLightDefaultSpotDropOffRate = 0.0;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, true, 'ambientIntensity', HeadLightDefaultAmbientIntensity);
  AddField(FFdAmbientIntensity);

  FFdAttenuation := TSFVec3f.Create(Self, true, 'attenuation', HeadLightDefaultAttenuation);
  AddField(FFdAttenuation);

  FFdColor := TSFColor.Create(Self, true, 'color', HeadLightDefaultColor);
  AddField(FFdColor);

  FFdIntensity := TSFFloat.Create(Self, true, 'intensity', HeadLightDefaultIntensity);
  AddField(FFdIntensity);

  FFdSpot := TSFBool.Create(Self, true, 'spot', HeadLightDefaultSpot);
  AddField(FFdSpot);

  FFdSpotCutOffAngle := TSFFloat.Create(Self, true, 'spotCutOffAngle', HeadLightDefaultSpotCutOffAngle);
  AddField(FFdSpotCutOffAngle);

  FFdSpotDropOffRate := TSFFloat.Create(Self, true, 'spotDropOffRate', HeadLightDefaultSpotDropOffRate);
  AddField(FFdSpotDropOffRate);
end;

{ TBlendModeNode ------------------------------------------------------------- }

class function TBlendModeNode.ClassX3DType: string;
begin
  Result := 'BlendMode';
end;

procedure TBlendModeNode.CreateNode;
begin
  inherited;

  FFdSrcFactor := TSFString.Create(Self, true, 'srcFactor', 'src_alpha');
   FdSrcFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSrcFactor);

  FFdDestFactor := TSFString.Create(Self, true, 'destFactor', 'one_minus_src_alpha');
   FdDestFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDestFactor);

  FFdColor := TSFColor.Create(Self, true, 'color', Vector3(1, 1, 1));
   FdColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdColor);

  FFdColorTransparency := TSFFloat.Create(Self, true, 'colorTransparency', 0.0);
   FdColorTransparency.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdColorTransparency);

  DefaultContainerField := 'blendMode';
end;

procedure TKambiInlineNode.AfterLoadInlined;
var
  I: Integer;
begin
  ReplaceCount := Min(FdreplaceNames.Count, FdreplaceNodes.Count);
  if (FdreplaceNames.Count <> ReplaceCount) or
     (FdreplaceNodes.Count <> ReplaceCount) then
    WritelnWarning('VRML/X3D', 'replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successful loading, there's always exactly one child. }
  Assert(VRML1ChildrenCount = 1);

  { First, a special case when Children[0] node has the required name.
    Needs to be checked explicitly, EnumerateReplaceNodes cannot check this. }
  for I := 0 to ReplaceCount - 1 do
    { Ignore empty names, since VRML node cannot have empty name
      (it's only our implementation detail that we mark "noname" as empty name) }
    if (FdReplaceNames.Items[I] <> '') and
       (FdReplaceNames.Items[I] = VRML1Children[0].X3DName) then
    begin
      VRML1ChildRemove(0);
      VRML1ChildAdd(0, FdReplaceNodes[I]);
      Exit;
    end;

  VRML1Children[0].EnumerateReplaceChildren({$ifdef FPC}@{$endif} DoReplaceNodes);
end;

procedure TKambiInlineNode.DoReplaceNodes(ParentNode: TX3DNode;
  var Node: TX3DNode);
var
  I: Integer;
begin
  for I := 0 to ReplaceCount - 1 do
    if (FdReplaceNames.Items[I] <> '') and
       (FdReplaceNames.Items[I] = Node.X3DName) then
    begin
      Node := FdReplaceNodes[I];
      Exit;
    end;
end;

procedure TKambiNavigationInfoNode.CreateNode;
begin
  inherited;

  FFdTimeOriginAtLoad := TSFBool.Create(Self, false, 'timeOriginAtLoad', false);
  AddField(FFdTimeOriginAtLoad);

  FFdOctreeRendering := TSFNode.Create(Self, false, 'octreeRendering', [TKambiOctreePropertiesNode]);
  AddField(FFdOctreeRendering);

  FFdOctreeDynamicCollisions := TSFNode.Create(Self, false, 'octreeDynamicCollisions', [TKambiOctreePropertiesNode]);
  AddField(FFdOctreeDynamicCollisions);

  FFdOctreeVisibleTriangles := TSFNode.Create(Self, false, 'octreeVisibleTriangles', [TKambiOctreePropertiesNode]);
  AddField(FFdOctreeVisibleTriangles);

  FFdOctreeStaticCollisions := TSFNode.Create(Self, false, 'octreeStaticCollisions', [TKambiOctreePropertiesNode]);
  FFdOctreeStaticCollisions.AddAlternativeName('octreeCollidableTriangles', 0);
  AddField(FFdOctreeStaticCollisions);

  FFdHeadBobbing := TSFFloat.Create(Self, true, 'headBobbing', TCastleWalkNavigation.DefaultHeadBobbing, true);
   FdHeadBobbing.ChangeAlways := chNavigationInfo;
  AddField(FFdHeadBobbing);

  FFdHeadBobbingTime := TSFFloat.Create(Self, true, 'headBobbingTime', TCastleWalkNavigation.DefaultHeadBobbingTime, true);
   FdHeadBobbingTime.ChangeAlways := chNavigationInfo;
  AddField(FdHeadBobbingTime);

  FFdHeadlightNode := TSFNode.Create(Self, true, 'headlightNode', [TAbstractLightNode]);
  AddField(FFdHeadlightNode);
end;

class function TKambiNavigationInfoNode.ClassX3DType: string;
begin
  Result := 'KambiNavigationInfo';
end;

function TKambiNavigationInfoNode.OctreeProperties(
  OP: TSceneOctreeProperties): TKambiOctreePropertiesNode;
var
  N: TX3DNode;
begin
  case OP of
    opRendering        : N := FdOctreeRendering          .Value;
    opDynamicCollisions: N := FdOctreeDynamicCollisions  .Value;
    opVisibleTriangles : N := FdOctreeVisibleTriangles   .Value;
    opStaticCollisions : N := FdOctreeStaticCollisions.Value;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TKambiOctreePropertiesNode.OctreeProperties');
    {$endif}
  end;
  if N is TKambiOctreePropertiesNode then
    Result := TKambiOctreePropertiesNode(N) else
    Result := nil;
end;

procedure TKambiOctreePropertiesNode.CreateNode;
begin
  inherited;

  FFdMaxDepth := TSFInt32.Create(Self, true, 'maxDepth', -1);
   FdMaxDepth.ChangeAlways := chEverything;
  AddField(FFdMaxDepth);

  FFdLeafCapacity := TSFInt32.Create(Self, true, 'leafCapacity', -1);
   FdLeafCapacity.ChangeAlways := chEverything;
  AddField(FFdLeafCapacity);
end;

class function TKambiOctreePropertiesNode.ClassX3DType: string;
begin
  Result := 'KambiOctreeProperties';
end;

procedure TKambiOctreePropertiesNode.OverrideLimits(var Limits: TOctreeLimits);
begin
  if FdMaxDepth.Value >= 0 then
    Limits.MaxDepth := FdMaxDepth.Value else
  if FdMaxDepth.Value <> -1 then
    WritelnWarning('VRML/X3D', 'KambiOctreeProperties.maxDepth must be >= -1');

  if FdLeafCapacity.Value >= 0 then
    Limits.LeafCapacity := FdLeafCapacity.Value else
  if FdLeafCapacity.Value <> -1 then
    WritelnWarning('VRML/X3D', 'KambiOctreeProperties.leafCapacity must be >= -1');
end;

procedure TGeneratedShadowMapNode.CreateNode;
const
  CompareModeNames: array [TShadowMapCompareMode] of string =
  ('COMPARE_R_LEQUAL', 'COMPARE_R_GEQUAL', 'NONE');
begin
  inherited;

  FFdUpdate := TSFTextureUpdate.Create(Self, true, 'update', upNone);
  AddField(FFdUpdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdSize := TSFInt32.Create(Self, false, 'size', DefaultSize, { MustBeNonnegative } true);
   FdSize.ChangeAlways := chTextureRendererProperties;
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  { Note that light node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    GeneratedShadowMap. }
  FFdLight := TSFNode.Create(Self, true, 'light', [TAbstractLightNode]);
   FdLight.ChangeAlways := chGeneratedTextureUpdateNeeded;
   FdLight.WeakLink := true;
  AddField(FFdLight);

  FFdScale := TSFFloat.Create(Self, true, 'scale', DefaultScale, { MustBeNonnegative } true);
   FdScale.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdScale);

  FFdBias := TSFFloat.Create(Self, true, 'bias', DefaultBias, { MustBeNonnegative } true);
   FdBias.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdBias);

  FFdCompareMode := TSFStringEnum.Create(Self, true, 'compareMode', CompareModeNames, Ord(smCompareRLEqual));
   FdCompareMode.ChangeAlways := chTextureRendererProperties;
  AddField(FFdCompareMode);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FUpdate := FdUpdate;
end;

destructor TGeneratedShadowMapNode.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TGeneratedShadowMapNode.ClassX3DType: string;
begin
  Result := 'GeneratedShadowMap';
end;

function TGeneratedShadowMapNode.GetCompareMode: TShadowMapCompareMode;
begin
  Result := TShadowMapCompareMode(FdCompareMode.EnumValue);
end;

procedure TGeneratedShadowMapNode.SetCompareMode(const Value: TShadowMapCompareMode);
begin
  FdCompareMode.SendEnumValue(Ord(Value));
end;

function TGeneratedShadowMapNode.GetUpdate: TTextureUpdate;
begin
  Result := FdUpdate.Value;
end;

procedure TGeneratedShadowMapNode.SetUpdate(const Value: TTextureUpdate);
begin
  FdUpdate.Send(Value);
end;

procedure TProjectedTextureCoordinateNode.CreateNode;
begin
  inherited;

  { Note that projector node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    ProjectedTextureCoordinate. }
  FFdProjector := TSFNode.Create(Self, true, 'projector', [
    TSpotLightNode_1,
    TDirectionalLightNode_1,
    TSpotLightNode,
    TDirectionalLightNode,
    TAbstractViewpointNode
  ]);
   FFdprojector.ChangeAlways := chTextureCoordinate;
  AddField(FFdProjector);
end;

class function TProjectedTextureCoordinateNode.ClassX3DType: string;
begin
  Result := 'ProjectedTextureCoordinate';
end;

procedure TScreenEffectNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
   FdEnabled.ChangeAlways := chScreenEffectEnabled;
  AddField(FFdEnabled);

  FFdShaders := TMFNode.Create(Self, true, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangeAlways := chEverything;
  AddField(FFdShaders);

  FFdNeedsDepth := TSFBool.Create(Self, true, 'needsDepth', false);
   FdNeedsDepth.ChangeAlways := chRedisplay;
  AddField(FFdNeedsDepth);

  FStateForShaderPrepare := TX3DGraphTraverseState.Create;
end;

destructor TScreenEffectNode.Destroy;
begin
  FreeAndNil(FStateForShaderPrepare);
  inherited;
end;

class function TScreenEffectNode.ClassX3DType: string;
begin
  Result := 'ScreenEffect';
end;

function TScreenEffectNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FFdShaders.Enumerate(Func);
  if Result <> nil then Exit;
end;

class function TMultiGeneratedTextureCoordinateNode.ClassX3DType: string;
begin
  Result := 'MultiGeneratedTextureCoordinate';
end;

{ TEffectNode ---------------------------------------------------------------- }

procedure TEffectNode.CreateNode;
begin
  inherited;

  FFdLanguage := TSFStringEnum.Create(Self, false, 'language', ShaderLanguageNames, Ord(slDefault));
   FdLanguage.ChangeAlways := chEverything;
  AddField(FFdLanguage);

  FFdParts := TMFNode.Create(Self, false, 'parts', [TEffectPartNode]);
   FdParts.ChangeAlways := chEverything;
  AddField(FFdParts);

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
   FdEnabled.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnabled);

  HasInterfaceDeclarations := AllAccessTypes;
end;

procedure TEffectNode.GroupBeforeTraverse(const State: TX3DGraphTraverseState; var WasPointingDeviceSensor: Boolean);
begin
  inherited;
  if State.Effects = nil then
    State.Effects := TX3DNodeList.Create(false);
  State.Effects.Add(Self);
end;

class function TEffectNode.ClassX3DType: string;
begin
  Result := 'Effect';
end;

function TEffectNode.GetLanguage: TShaderLanguage;
begin
  Result := TShaderLanguage(FdLanguage.EnumValue);
end;

procedure TEffectNode.SetLanguage(const Value: TShaderLanguage);
begin
  FdLanguage.SendEnumValue(Ord(Value));
end;

constructor TEffectPartNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
end;

procedure TEffectPartNode.CreateNode;
begin
  inherited;

  FFdType := TSFStringEnum.Create(Self, false, 'type', ShaderTypeNameX3D, Ord(stVertex));
   FdType.ChangeAlways := chEverything;
  AddField(FFdType);

  FFdUrl := TMFString.Create(Self, true, 'url', []);
   FdUrl.ChangeAlways := chEverything;
   FdUrl.AddNotification({$ifdef FPC}@{$endif} EventUrlReceive);
  AddField(FFdUrl);

  DefaultContainerField := 'parts';
end;

class function TEffectPartNode.ClassX3DType: string;
begin
  Result := 'EffectPart';
end;

function TEffectPartNode.GetContents: string;
begin
  if not ContentsCached then
  begin
    ContentsCache := ShaderLoadContents(Self, FdUrl);
    ContentsCached := true;
  end;
  Result := ContentsCache;
end;

procedure TEffectPartNode.SetContents(const Value: string);
begin
  SetUrl(['data:text/plain,' + Value]);

  //Assert(Value = GetContents);
  // an optimized way to initialize ContentsCache
  ContentsCache := Value;
  ContentsCached := true;
end;

procedure TEffectPartNode.EventUrlReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  ContentsCached := false;
end;

function TEffectPartNode.GetShaderType: TShaderType;
begin
  Result := TShaderType(FdType.EnumValue);
end;

procedure TEffectPartNode.SetShaderType(const Value: TShaderType);
begin
  FdType.SendEnumValue(Ord(Value));
end;

procedure TShaderTextureNode.CreateNode;
begin
  inherited;

  FFdDefaultTexCoord := TSFString.Create(Self, false, 'defaultTexCoord', 'BOUNDS2D');
   FdDefaultTexCoord.ChangeAlways := chEverything;
  AddField(FFdDefaultTexCoord);
end;

class function TShaderTextureNode.ClassX3DType: string;
begin
  Result := 'ShaderTexture';
end;

{ TOrientationInterpolator2DNode ---------------------------------- }

procedure TOrientationInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, {$ifdef CASTLE_SLIM_NODES} false {$else} true {$endif}, 'keyValue', []);
  AddField(FFdKeyValue);

  FFdAxis := TSFVec3f.Create(Self, {$ifdef CASTLE_SLIM_NODES} false {$else} true {$endif}, 'axis', Vector3(0, 0, 1));
  AddField(FFdAxis);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification(
    {$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

class function TOrientationInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'OrientationInterpolator2D';
end;

procedure TOrientationInterpolator2DNode.EventSet_FractionReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);

  function KeyValue(Index: Integer): Single;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.List^[Index]
    else
      Result := 0;
  end;

var
  T: Single;
  OutputValueAngle: Single;
  KeyCount, Range: Integer;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    Exit;
  end else
  begin
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValueAngle := KeyValue(0)
    else
    if Range = KeyCount then
      OutputValueAngle := KeyValue(KeyCount - 1)
    else
      OutputValueAngle := AngleLerp(T, KeyValue(Range - 1), KeyValue(Range));
  end;
  EventValue_Changed.Send(Vector4(FdAxis.Value, OutputValueAngle), Time, Value.InternalPartialSend);
end;

procedure RegisterKambiNodes;
begin
  NodesManager.RegisterNodeClasses([
    TKambiHeadLightNode,
    TText3DNode,
    TBlendModeNode,
    TKambiAppearanceNode,
    TKambiInlineNode,
    TKambiNavigationInfoNode,
    TKambiOctreePropertiesNode,
    TGeneratedShadowMapNode,
    TProjectedTextureCoordinateNode,
    TScreenEffectNode,
    TMultiGeneratedTextureCoordinateNode,
    TEffectNode,
    TEffectPartNode,
    TShaderTextureNode,
    TViewpointMirrorNode,
    TOrientationInterpolator2DNode
  ]);
end;

{$endif read_implementation}
