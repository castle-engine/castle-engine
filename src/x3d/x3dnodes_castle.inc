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

{ Nodes that are defined specifically by Castle Game Engine.
  See [http://castle-engine.sourceforge.net/x3d_extensions.php]. } { }

{$ifdef read_interface}
  { Headlight configuration, allows to customize headlight look and shape.
    See http://castle-engine.sourceforge.net/x3d_implementation_navigation_extensions.php#section_ext_headlight . }
  TKambiHeadLightNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdAmbientIntensity: TSFFloat;
    public property FdAmbientIntensity: TSFFloat read FFdAmbientIntensity;

    private FFdAttenuation: TSFVec3f;
    public property FdAttenuation: TSFVec3f read FFdAttenuation;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdIntensity: TSFFloat;
    public property FdIntensity: TSFFloat read FFdIntensity;

    private FFdSpot: TSFBool;
    public property FdSpot: TSFBool read FFdSpot;

    private FFdSpotCutOffAngle: TSFFloat;
    public property FdSpotCutOffAngle: TSFFloat read FFdSpotCutOffAngle;

    private FFdSpotDropOffRate: TSFFloat;
    public property FdSpotDropOffRate: TSFFloat read FFdSpotDropOffRate;

    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_kambiheadlight.inc}
  end;

  { Text with 3D depth.
    @bold(The depth is not implemented now -- this is equivalent to @link(TTextNode) now.) }
  TText3DNode = class(TTextNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Fields added compared to normal Text node: }

    { }
    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_text3d.inc}
  end;

  { Customize blending (partial transparency) mode.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_blending .

    This is an extension, not present in standard X3D.
    It is compatible with modelled after, and compatible with,
    and InstantReality extension -- see
    [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
    and [http://www.instantreality.org/documentation/nodetype/BlendMode/]. }
  TBlendModeNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdSrcFactor: TSFString;
    public property FdSrcFactor: TSFString read FFdSrcFactor;

    private FFdDestFactor: TSFString;
    public property FdDestFactor: TSFString read FFdDestFactor;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdColorTransparency: TSFFloat;
    public property FdColorTransparency: TSFFloat read FFdColorTransparency;

    {$I auto_generated_node_helpers/x3dnodes_blendmode.inc}
  end;

  { Appearance node with a special name @code(KambiAppearance).
    Useful if you want to use EXTERNPROTO to declare Castle Game Engine
    extensions the Appearance in a clean way. }
  TKambiAppearanceNode = class(TAppearanceNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_kambiappearance.inc}
  end;

  { Embed a scene from another file, and replace some of it's nodes.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_kambi_inline . }
  TKambiInlineNode = class(TInlineNode)
  private
    { This is internal only for DoReplaceNodes, initialized in AfterLoadInlined. }
    ReplaceCount: Integer;
    procedure DoReplaceNodes(ParentNode: TX3DNode; var Node: TX3DNode);
  protected
    procedure AfterLoadInlined; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdReplaceNames: TMFString;
    public property FdReplaceNames: TMFString read FFdReplaceNames;

    private FFdReplaceNodes: TMFNode;
    public property FdReplaceNodes: TMFNode read FFdReplaceNodes;

    {$I auto_generated_node_helpers/x3dnodes_kambiinline.inc}
  end;

  TSceneOctreeProperties = (
    opRendering,
    opDynamicCollisions,
    opVisibleTriangles,
    opStaticCollisions);

  TKambiOctreePropertiesNode = class;

  { Navigation information with some Castle Game Engine extensions.
    See http://castle-engine.sourceforge.net/x3d_implementation_navigation_extensions.php . }
  TKambiNavigationInfoNode = class(TNavigationInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTimeOriginAtLoad: TSFBool;
    public property FdTimeOriginAtLoad: TSFBool read FFdTimeOriginAtLoad;

    private FFdOctreeRendering: TSFNode;
    public property FdOctreeRendering: TSFNode read FFdOctreeRendering;

    private FFdOctreeDynamicCollisions: TSFNode;
    public property FdOctreeDynamicCollisions: TSFNode read FFdOctreeDynamicCollisions;

    private FFdOctreeVisibleTriangles: TSFNode;
    public property FdOctreeVisibleTriangles: TSFNode read FFdOctreeVisibleTriangles;

    private FFdOctreeStaticCollisions: TSFNode;
    public property FdOctreeStaticCollisions: TSFNode read FFdOctreeStaticCollisions;

    private FFdHeadBobbing: TSFFloat;
    public property FdHeadBobbing: TSFFloat read FFdHeadBobbing;

    private FFdHeadBobbingTime: TSFFloat;
    public property FdHeadBobbingTime: TSFFloat read FFdHeadBobbingTime;

    private FFdHeadlightNode: TSFNode;
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
    class function URNMatching(const URN: string): boolean; override;

    private FFdMaxDepth: TSFInt32;
    public property FdMaxDepth: TSFInt32 read FFdMaxDepth;

    private FFdLeafCapacity: TSFInt32;
    public property FdLeafCapacity: TSFInt32 read FFdLeafCapacity;

    { Override Limits fields, where our own fields have values <> -1. }
    procedure OverrideLimits(var Limits: TOctreeLimits);

    {$I auto_generated_node_helpers/x3dnodes_kambioctreeproperties.inc}
  end;

  { Generate texture by rendering a depth (shadow) map.
    See http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php }
  TGeneratedShadowMapNode = class(TAbstractTextureNode)
  strict private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
    function GetLight: TAbstractLightNode;
    procedure SetLight(const Value: TAbstractLightNode);
  public
    const
      DefaultSize = 128;
      DefaultScale = 4.0;
      DefaultBias = 4.0;

    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;

    private FFdUpdate: TSFTextureUpdate;
    public property FdUpdate: TSFTextureUpdate read FFdUpdate;

    private FFdSize: TSFInt32;
    public property FdSize: TSFInt32 read FFdSize;

    private FFdLight: TSFNode;
    public property FdLight: TSFNode read FFdLight;

    private FFdScale: TSFFloat;
    public property FdScale: TSFFloat read FFdScale;

    private FFdBias: TSFFloat;
    public property FdBias: TSFFloat read FFdBias;

    private FFdCompareMode: TSFstring;
    public property FdCompareMode: TSFString read FFdCompareMode;

    property Light: TAbstractLightNode read GetLight write SetLight;

    {$I auto_generated_node_helpers/x3dnodes_generatedshadowmap.inc}
  end;

  { Generate texture coordinates by projection.
    See http://castle-engine.sourceforge.net/x3d_extensions_shadow_maps.php }
  TProjectedTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdProjector: TSFNode;
    public property FdProjector: TSFNode read FFdProjector;

    {$I auto_generated_node_helpers/x3dnodes_projectedtexturecoordinate.inc}
  end;

  { Screen-space shader effect.
    See http://castle-engine.sourceforge.net/x3d_extensions_screen_effects.php . }
  TScreenEffectNode = class(TAbstractChildNode)
  private
    FShader: TObject;
    FShaderLoaded: boolean;
    FStateForShaderPrepare: TX3DGraphTraverseState;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

    private FFdNeedsDepth: TSFBool;
    public property FdNeedsDepth: TSFBool read FFdNeedsDepth;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Shader and ShaderLoaded properties are internally manager by the renderer.
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
    See [http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_tex_coord]. }
  TMultiGeneratedTextureCoordinateNode = class(TMultiTextureCoordinateNode)
  public
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_multigeneratedtexturecoordinate.inc}
  end;

  { Shader effect, that can be composed with other effect or standard rendering.
    See http://castle-engine.sourceforge.net/compositing_shaders.php . }
  TEffectNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;

    private FFdParts: TMFNode;
    public property FdParts: TMFNode read FFdParts;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    {$I auto_generated_node_helpers/x3dnodes_effect.inc}
  end;

  { Part of a shader effect, used with @link(TEffectNode).
    See http://castle-engine.sourceforge.net/compositing_shaders.php . }
  TEffectPartNode = class(TAbstractNode, IAbstractUrlObject)
  private
    ContentsCached: boolean;
    ContentsCache: string;
    procedure EventUrlReceive(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
    function GetShaderType: TShaderType;
    procedure SetShaderType(const Value: TShaderType);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdType: TSFStringEnum;
    public property FdType: TSFStringEnum read FFdType;

    { Shader type. }
    property ShaderType: TShaderType read GetShaderType write SetShaderType;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Load shader code from node's URLs.
      In X3D XML encoding, also loads node's XML CDATA as a last resort.
      Will return '' if no valid URLs are present,
      loading errors are reported to WritelnWarning. }
    function Contents: string;

    {$I auto_generated_node_helpers/x3dnodes_effectpart.inc}
  end;

  { Procedural texture defined by shaders.
    See http://castle-engine.sourceforge.net/compositing_shaders.php . }
  TShaderTextureNode = class(TAbstractTextureNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDefaultTexCoord: TSFString;
    public property FdDefaultTexCoord: TSFString read FFdDefaultTexCoord;

    {$I auto_generated_node_helpers/x3dnodes_shadertexture.inc}
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
  HeadLightDefaultAttenuation: TVector3Single = (1, 0, 0);
  HeadLightDefaultColor: TVector3Single = (1, 1, 1);
  HeadLightDefaultIntensity = 1.0;
  HeadLightDefaultSpot = false;
  HeadLightDefaultSpotCutOffAngle = Pi / 4;
  HeadLightDefaultSpotDropOffRate = 0.0;
begin
  inherited;

  FFdAmbientIntensity := TSFFloat.Create(Self, 'ambientIntensity', HeadLightDefaultAmbientIntensity);
  AddField(FFdAmbientIntensity);

  FFdAttenuation := TSFVec3f.Create(Self, 'attenuation', HeadLightDefaultAttenuation);
  AddField(FFdAttenuation);

  FFdColor := TSFColor.Create(Self, 'color', HeadLightDefaultColor);
  AddField(FFdColor);

  FFdIntensity := TSFFloat.Create(Self, 'intensity', HeadLightDefaultIntensity);
  AddField(FFdIntensity);

  FFdSpot := TSFBool.Create(Self, 'spot', HeadLightDefaultSpot);
  AddField(FFdSpot);

  FFdSpotCutOffAngle := TSFFloat.Create(Self, 'spotCutOffAngle', HeadLightDefaultSpotCutOffAngle);
  AddField(FFdSpotCutOffAngle);

  FFdSpotDropOffRate := TSFFloat.Create(Self, 'spotDropOffRate', HeadLightDefaultSpotDropOffRate);
  AddField(FFdSpotDropOffRate);
end;

class function TKambiHeadLightNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    URNMatchingCastle(URN, ClassX3DType);
end;

class function TText3DNode.ClassX3DType: string;
begin
  Result := 'Text3D';
end;

class function TText3DNode.URNMatching(const URN: string): boolean;
begin
  Result :=
    { do not call (inherited URNMatching(URN)), since we don't match Text node rules }
    URNMatchingCastle(URN, ClassX3DType);
end;

procedure TText3DNode.CreateNode;
begin
  inherited;

  FFdDepth := TSFFloat.Create(Self, 'depth', 0.1, true);
   FdDepth.ChangesAlways := [chGeometry];
  AddField(FFdDepth);
end;

{ TBlendModeNode ------------------------------------------------------------- }

class function TBlendModeNode.ClassX3DType: string;
begin
  Result := 'BlendMode';
end;

class function TBlendModeNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    URNMatchingCastle(URN, ClassX3DType);
end;

procedure TBlendModeNode.CreateNode;
begin
  inherited;

  FFdSrcFactor := TSFString.Create(Self, 'srcFactor', 'src_alpha');
   FdSrcFactor.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdSrcFactor);

  FFdDestFactor := TSFString.Create(Self, 'destFactor', 'one_minus_src_alpha');
   FdDestFactor.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdDestFactor);

  FFdColor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   FdColor.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdColor);

  FFdColorTransparency := TSFFloat.Create(Self, 'colorTransparency', 0.0);
   FdColorTransparency.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdColorTransparency);

  DefaultContainerField := 'blendMode';
end;

class function TKambiAppearanceNode.ClassX3DType: string;
begin
  Result := 'KambiAppearance';
end;

class function TKambiAppearanceNode.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Appearance node. }
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

procedure TKambiAppearanceNode.CreateNode;
begin
  inherited;
end;

procedure TKambiInlineNode.CreateNode;
begin
  inherited;

  FFdReplaceNames := TMFString.Create(Self, 'replaceNames', []);
  AddField(FFdReplaceNames);

  FFdReplaceNodes := TMFNode.Create(Self, 'replaceNodes', [TX3DNode]);
  AddField(FFdReplaceNodes);
end;

class function TKambiInlineNode.ClassX3DType: string;
begin
  Result := 'KambiInline';
end;

class function TKambiInlineNode.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Inline node. }
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

procedure TKambiInlineNode.AfterLoadInlined;
var
  I: Integer;
begin
  ReplaceCount := Min(FdreplaceNames.Count, FdreplaceNodes.Count);
  if (FdreplaceNames.Count <> ReplaceCount) or
     (FdreplaceNodes.Count <> ReplaceCount) then
    WritelnWarning('VRML/X3D', 'replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successfull loading, there's always exactly one child. }
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

  VRML1Children[0].EnumerateReplaceChildren(@DoReplaceNodes);
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

  FFdTimeOriginAtLoad := TSFBool.Create(Self, 'timeOriginAtLoad', false);
  FFdTimeOriginAtLoad.Exposed := false;
  AddField(FFdTimeOriginAtLoad);

  FFdOctreeRendering := TSFNode.Create(Self, 'octreeRendering', [TKambiOctreePropertiesNode]);
  FFdOctreeRendering.Exposed := false;
  AddField(FFdOctreeRendering);

  FFdOctreeDynamicCollisions := TSFNode.Create(Self, 'octreeDynamicCollisions', [TKambiOctreePropertiesNode]);
  FFdOctreeDynamicCollisions.Exposed := false;
  AddField(FFdOctreeDynamicCollisions);

  FFdOctreeVisibleTriangles := TSFNode.Create(Self, 'octreeVisibleTriangles', [TKambiOctreePropertiesNode]);
  FFdOctreeVisibleTriangles.Exposed := false;
  AddField(FFdOctreeVisibleTriangles);

  FFdOctreeStaticCollisions := TSFNode.Create(Self, 'octreeStaticCollisions', [TKambiOctreePropertiesNode]);
  FFdOctreeStaticCollisions.Exposed := false;
  FFdOctreeStaticCollisions.AddAlternativeName('octreeCollidableTriangles', 0);
  AddField(FFdOctreeStaticCollisions);

  FFdHeadBobbing := TSFFloat.Create(Self, 'headBobbing', TWalkCamera.DefaultHeadBobbing, true);
   FdHeadBobbing.ChangesAlways := [chNavigationInfo];
  AddField(FFdHeadBobbing);

  FFdHeadBobbingTime := TSFFloat.Create(Self, 'headBobbingTime', TWalkCamera.DefaultHeadBobbingTime, true);
   FdHeadBobbingTime.ChangesAlways := [chNavigationInfo];
  AddField(FdHeadBobbingTime);

  FFdHeadlightNode := TSFNode.Create(Self, 'headlightNode', [TAbstractLightNode]);
  AddField(FFdHeadlightNode);
end;

class function TKambiNavigationInfoNode.ClassX3DType: string;
begin
  Result := 'KambiNavigationInfo';
end;

class function TKambiNavigationInfoNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
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
    else raise EInternalError.Create('TKambiOctreePropertiesNode.OctreeProperties');
  end;
  if N is TKambiOctreePropertiesNode then
    Result := TKambiOctreePropertiesNode(N) else
    Result := nil;
end;

procedure TKambiOctreePropertiesNode.CreateNode;
begin
  inherited;

  FFdMaxDepth := TSFInt32.Create(Self, 'maxDepth', -1);
   FdMaxDepth.ChangesAlways := [chEverything];
  AddField(FFdMaxDepth);

  FFdLeafCapacity := TSFInt32.Create(Self, 'leafCapacity', -1);
   FdLeafCapacity.ChangesAlways := [chEverything];
  AddField(FFdLeafCapacity);
end;

class function TKambiOctreePropertiesNode.ClassX3DType: string;
begin
  Result := 'KambiOctreeProperties';
end;

class function TKambiOctreePropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
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
begin
  inherited;

  FFdUpdate := TSFTextureUpdate.Create(Self, 'update', upNone);
  AddField(FFdUpdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdSize := TSFInt32.Create(Self, 'size', DefaultSize, { MustBeNonnegative } true);
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chTextureRendererProperties];
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  { Note that light node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    GeneratedShadowMap. }
  FFdLight := TSFNode.Create(Self, 'light', [TAbstractLightNode]);
   FdLight.ChangesAlways := [chGeneratedTextureUpdateNeeded];
   FdLight.WeakLink := true;
  AddField(FFdLight);

  FFdScale := TSFFloat.Create(Self, 'scale', DefaultScale, { MustBeNonnegative } true);
   FdScale.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  AddField(FFdScale);

  FFdBias := TSFFloat.Create(Self, 'bias', DefaultBias, { MustBeNonnegative } true);
   FdBias.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  AddField(FFdBias);

  FFdCompareMode := TSFString.Create(Self, 'compareMode', 'COMPARE_R_LEQUAL');
   FdCompareMode.ChangesAlways := [chTextureRendererProperties];
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

class function TGeneratedShadowMapNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

function TGeneratedShadowMapNode.GetLight: TAbstractLightNode;
begin
  if FdLight.Value is TAbstractLightNode then
    Result := TAbstractLightNode(FdLight.Value)
  else
    Result := nil;
end;

procedure TGeneratedShadowMapNode.SetLight(const Value: TAbstractLightNode);
begin
  FdLight.Send(Value);
end;

procedure TProjectedTextureCoordinateNode.CreateNode;
begin
  inherited;

  { Note that projector node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    ProjectedTextureCoordinate. }
  FFdProjector := TSFNode.Create(Self, 'projector',
    [TSpotLightNode_1, TDirectionalLightNode_1,
     TSpotLightNode  , TDirectionalLightNode  ,
     TAbstractX3DViewpointNode]);
   FFdprojector.ChangesAlways := [chTextureCoordinate];
  AddField(FFdProjector);
end;

class function TProjectedTextureCoordinateNode.ClassX3DType: string;
begin
  Result := 'ProjectedTextureCoordinate';
end;

class function TProjectedTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

procedure TScreenEffectNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
   FdEnabled.ChangesAlways := [chScreenEffectEnabled];
  AddField(FFdEnabled);

  FFdShaders := TMFNodeShaders.Create(Self, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  AddField(FFdShaders);

  FFdNeedsDepth := TSFBool.Create(Self, 'needsDepth', false);
   { no change needed, TCastleScene recalculates NeedsDepth every time
     when ScreenEffectsNeedDepth is called by scene manager. }
   FdNeedsDepth.ChangesAlways := [];
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

class function TScreenEffectNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
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

class function TMultiGeneratedTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

procedure TEffectNode.CreateNode;
begin
  inherited;

  FFdLanguage := TSFString.Create(Self, 'language', '');
   FdLanguage.Exposed := false;
   FdLanguage.ChangesAlways := [chEverything];
  AddField(FFdLanguage);

  FFdParts := TMFNode.Create(Self, 'parts', [TEffectPartNode]);
   FdParts.Exposed := false;
   FdParts.ChangesAlways := [chEverything];
  AddField(FFdParts);

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
   FdEnabled.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdEnabled);

  HasInterfaceDeclarations := AllAccessTypes;
end;

class function TEffectNode.ClassX3DType: string;
begin
  Result := 'Effect';
end;

class function TEffectNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

procedure TEffectPartNode.CreateNode;
begin
  inherited;

  FFdType := TSFStringEnum.Create(Self, 'type', ShaderTypeNameX3D, Ord(stVertex));
   FdType.Exposed := false;
   FdType.ChangesAlways := [chEverything];
  AddField(FFdType);

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.ChangesAlways := [chEverything];
   FdUrl.OnReceive.Add(@EventUrlReceive);
  AddField(FFdUrl);

  DefaultContainerField := 'parts';
end;

class function TEffectPartNode.ClassX3DType: string;
begin
  Result := 'EffectPart';
end;

class function TEffectPartNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
end;

function TEffectPartNode.Contents: string;
begin
  if not ContentsCached then
  begin
    ContentsCache := ShaderLoadContents(Self, FdUrl);
    ContentsCached := true;
  end;
  Result := ContentsCache;
end;

procedure TEffectPartNode.EventUrlReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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

  FFdDefaultTexCoord := TSFString.Create(Self, 'defaultTexCoord', 'BOUNDS2D');
   FdDefaultTexCoord.Exposed := false;
   FdDefaultTexCoord.ChangesAlways := [chEverything];
  AddField(FFdDefaultTexCoord);
end;

class function TShaderTextureNode.ClassX3DType: string;
begin
  Result := 'ShaderTexture';
end;

class function TShaderTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := URNMatchingCastle(URN, ClassX3DType);
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
    TShaderTextureNode
  ]);
end;

{$endif read_implementation}
