{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined specifically by Kambi VRML game engine.
  See [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php]. } { }

{$ifdef read_interface}
  TKambiTriangulationNode = class(TAbstractChildNode)
  private
    function GetDetailValue(Field: TSFLong; const GlobalValue, MinValue: Cardinal): Cardinal;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdQuadricSlices: TSFLong;
    public property FdQuadricSlices: TSFLong read FFdQuadricSlices;

    private FFdQuadricStacks: TSFLong;
    public property FdQuadricStacks: TSFLong read FFdQuadricStacks;

    private FFdRectDivisions: TSFLong;
    public property FdRectDivisions: TSFLong read FFdRectDivisions;

    { Return appropriate field value, or (when it's -1) return global value
      (from Detail_Xxx variables). In case of incorrect field value,
      they also take care of calling OnWarning and correcting this value.
      @groupBegin }
    function QuadricStacks: Cardinal;
    function QuadricSlices: Cardinal;
    function RectDivisions: Cardinal;
    { @groupEnd }

    class function URNMatching(const URN: string): boolean; override;
  end;

  TKambiHeadLightNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

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
  end;

  TText3DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdString: TMFString;
    public property FdString: TMFString read FFdString;

    private FFdFontStyle: TSFNode;
    public property FdFontStyle: TSFNode read FFdFontStyle;

    private FFdLength: TMFFloat;
    public property FdLength: TMFFloat read FFdLength;

    private FFdMaxExtent: TSFFloat;
    public property FdMaxExtent: TSFFloat read FFdMaxExtent;

    { Fields added compared to normal Text node: }

    { }
    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TFontStyleNode. }
    function FontStyle: TFontStyleNode;
  end;

  { BlendMode node.

    This is not standard X3D. It's modelled after Avalon extension on
    [http://www.instantreality.org/].
    See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
    and [http://www.instantreality.org/documentation/nodetype/BlendMode/].

    Actually, our version is just a subset of their node. }
  TBlendModeNode = class(TAbstractAppearanceChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdSrcFactor: TSFString;
    public property FdSrcFactor: TSFString read FFdSrcFactor;

    private FFdDestFactor: TSFString;
    public property FdDestFactor: TSFString read FFdDestFactor;

    private FFdColor: TSFColor;
    public property FdColor: TSFColor read FFdColor;

    private FFdColorTransparency: TSFFloat;
    public property FdColorTransparency: TSFFloat read FFdColorTransparency;
  end;

  TKambiAppearanceNode = class(TAppearanceNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TKambiInlineNode = class(TInlineNode)
  private
    { This is internal only for DoReplaceNodes, initialized in AfterLoadInlined. }
    ReplaceCount: Integer;
    procedure DoReplaceNodes(ParentNode: TX3DNode; var Node: TX3DNode);
  protected
    procedure AfterLoadInlined; override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdReplaceNames: TMFString;
    public property FdReplaceNames: TMFString read FFdReplaceNames;

    private FFdReplaceNodes: TMFNode;
    public property FdReplaceNodes: TMFNode read FFdReplaceNodes;
  end;

  TSceneOctreeProperties = (
    opRendering,
    opDynamicCollisions,
    opVisibleTriangles,
    opCollidableTriangles);

  TKambiOctreePropertiesNode = class;

  TKambiNavigationInfoNode = class(TNavigationInfoNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTimeOriginAtLoad: TSFBool;
    public property FdTimeOriginAtLoad: TSFBool read FFdTimeOriginAtLoad;

    private FFdOctreeRendering: TSFNode;
    public property FdOctreeRendering: TSFNode read FFdOctreeRendering;

    private FFdOctreeDynamicCollisions: TSFNode;
    public property FdOctreeDynamicCollisions: TSFNode read FFdOctreeDynamicCollisions;

    private FFdOctreeVisibleTriangles: TSFNode;
    public property FdOctreeVisibleTriangles: TSFNode read FFdOctreeVisibleTriangles;

    private FFdOctreeCollidableTriangles: TSFNode;
    public property FdOctreeCollidableTriangles: TSFNode read FFdOctreeCollidableTriangles;

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
  end;

  TKambiOctreePropertiesNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdMaxDepth: TSFInt32;
    public property FdMaxDepth: TSFInt32 read FFdMaxDepth;

    private FFdLeafCapacity: TSFInt32;
    public property FdLeafCapacity: TSFInt32 read FFdLeafCapacity;

    { Override Limits fields, where our own fields have values <> -1. }
    procedure OverrideLimits(var Limits: TOctreeLimits);
  end;

  TGeneratedShadowMapNode = class(TAbstractTextureNode)
  private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;

    private FFdUpdate: TSFString;
    public property FdUpdate: TSFString read FFdUpdate;

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

    function Light: TAbstractLightNode;
  end;

  TProjectedTextureCoordinateNode = class(TAbstractTextureCoordinateNode)
  public
    procedure CreateNode; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdProjector: TSFNode;
    public property FdProjector: TSFNode read FFdProjector;
  end;

  TScreenEffectNode = class(TAbstractChildNode)
  private
    FShader: TObject;
    FShaderLoaded: boolean;
    FStateForShaderPrepare: TVRMLGraphTraverseState;
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShaders: TMFNodeShaders;
    public property FdShaders: TMFNodeShaders read FFdShaders;

    private FFdNeedsDepth: TSFBool;
    public property FdNeedsDepth: TSFBool read FFdNeedsDepth;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Shader and ShaderLoaded properties are internally manager by the renderer.
      Right now, this means TVRMLGLScene.
      Always when ShaderLoaded = @false then Shader is @nil.
      When ShaderLoaded = @true, the Shader may be non-nil (if was enabled
      and loaded successfully) or not.
      @groupBegin }
    property Shader: TObject read FShader write FShader;
    property ShaderLoaded: boolean read FShaderLoaded write FShaderLoaded;
    property StateForShaderPrepare: TVRMLGraphTraverseState
      read FStateForShaderPrepare;
    { @groupEnd }
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

  { X3D MultiGeneratedTextureCoordinate node.
    See [http://castle-engine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord]. }
  TMultiGeneratedTextureCoordinateNode = class(TMultiTextureCoordinateNode)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TEffectNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;

    private FFdParts: TMFNode;
    public property FdParts: TMFNode read FFdParts;

    private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;
  end;

  TEffectPartNode = class(TAbstractNode, IAbstractUrlObject)
  private
    FUsedFullUrl: string;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdType: TSFStringShaderType;
    public property FdType: TSFStringShaderType read FFdType;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Load shader code from node's URLs.
      Will return '' if no valid URLs are present.
      Note that in XML encoding, URLs also contain XML CDATA.
      Sets UsedFullUrl. }
    function LoadContents: string;

    { Once the LoadContents is called, this is set to the URL that was
      used to load the shader contents. See also TShaderPartNode.UsedFullUrl. }
    property UsedFullUrl: string read FUsedFullUrl;
  end;

  TShaderTextureNode = class(TAbstractTextureNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDefaultTexCoord: TSFString;
    public property FdDefaultTexCoord: TSFString read FFdDefaultTexCoord;
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  TriangulationUseDef = -1;

procedure TKambiTriangulationNode.CreateNode;
begin
  inherited;

  FFdQuadricSlices := TSFLong.Create(Self, 'quadricSlices', TriangulationUseDef);
   FdQuadricSlices.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdQuadricSlices);

  FFdQuadricStacks := TSFLong.Create(Self, 'quadricStacks', TriangulationUseDef);
   FdQuadricStacks.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdQuadricStacks);

  FFdRectDivisions := TSFLong.Create(Self, 'rectDivisions', TriangulationUseDef);
   FdRectDivisions.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdRectDivisions);
end;

class function TKambiTriangulationNode.ClassNodeTypeName: string;
begin
  result := 'KambiTriangulation';
end;

function TKambiTriangulationNode.GetDetailValue(
  Field: TSFLong; const GlobalValue, MinValue: Cardinal): Cardinal;
begin
  if Field.Value = TriangulationUseDef then
    Result := GlobalValue else
  if Int64(Field.Value) < Int64(MinValue) then
  begin
    OnWarning(wtMajor, 'VRML/X3D', Format('Node "KambiTriangulation" field "%s" value is %d but must be >= %d (or = -1)',
      [Field.Name, Field.Value, MinValue]));
    Field.Value := TriangulationUseDef;
    Result := GlobalValue;
  end else
    Result := Field.Value;
end;

function TKambiTriangulationNode.QuadricSlices: Cardinal;
begin
  Result := GetDetailValue(FdQuadricSlices, Detail_QuadricSlices, MinQuadricSlices);
end;

function TKambiTriangulationNode.QuadricStacks: Cardinal;
begin
  Result := GetDetailValue(FdQuadricStacks, Detail_QuadricStacks, MinQuadricStacks);
end;

function TKambiTriangulationNode.RectDivisions: Cardinal;
begin
  Result := GetDetailValue(FdRectDivisions, Detail_RectDivisions, MinRectDivisions);
end;

class function TKambiTriangulationNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TKambiHeadLightNode.ClassNodeTypeName: string;
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
  Fields.Add(FFdAmbientIntensity);

  FFdAttenuation := TSFVec3f.Create(Self, 'attenuation', HeadLightDefaultAttenuation);
  Fields.Add(FFdAttenuation);

  FFdColor := TSFColor.Create(Self, 'color', HeadLightDefaultColor);
  Fields.Add(FFdColor);

  FFdIntensity := TSFFloat.Create(Self, 'intensity', HeadLightDefaultIntensity);
  Fields.Add(FFdIntensity);

  FFdSpot := TSFBool.Create(Self, 'spot', HeadLightDefaultSpot);
  Fields.Add(FFdSpot);

  FFdSpotCutOffAngle := TSFFloat.Create(Self, 'spotCutOffAngle', HeadLightDefaultSpotCutOffAngle);
  Fields.Add(FFdSpotCutOffAngle);

  FFdSpotDropOffRate := TSFFloat.Create(Self, 'spotDropOffRate', HeadLightDefaultSpotDropOffRate);
  Fields.Add(FFdSpotDropOffRate);
end;

class function TKambiHeadLightNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TText3DNode.ClassNodeTypeName: string;
begin
  Result := 'Text3D';
end;

class function TText3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TText3DNode.CreateNode;
begin
  inherited;

  FFdString := TMFString.Create(Self, 'string', []);
   FdString.ChangesAlways := [chGeometry];
  Fields.Add(FFdString);

  FFdFontStyle := TSFNode.Create(Self, 'fontStyle', [TFontStyleNode]);
   FdFontStyle.ChangesAlways := [chGeometry];
  Fields.Add(FFdFontStyle);

  FFdLength := TMFFloat.Create(Self, 'length', []);
   FdLength.ChangesAlways := [chGeometry];
  Fields.Add(FFdLength);

  FFdMaxExtent := TSFFloat.Create(Self, 'maxExtent', 0.0);
   FdMaxExtent.ChangesAlways := [chGeometry];
  Fields.Add(FFdMaxExtent);

  FFdDepth := TSFFloat.Create(Self, 'depth', 0.1, true);
   FdDepth.ChangesAlways := [chGeometry];
  Fields.Add(FFdDepth);

  FFdSolid := TSFBool.Create(Self, 'solid', true);
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);
end;

function TText3DNode.FontStyle: TFontStyleNode;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TFontStyleNode) then
    Result := TFontStyleNode(FdFontStyle.Value) else
    Result := nil;
end;

class function TBlendModeNode.ClassNodeTypeName: string;
begin
  Result := 'BlendMode';
end;

class function TBlendModeNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TBlendModeNode.CreateNode;
begin
  inherited;

  FFdSrcFactor := TSFString.Create(Self, 'srcFactor', 'src_alpha');
   FdSrcFactor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdSrcFactor);

  FFdDestFactor := TSFString.Create(Self, 'destFactor', 'one_minus_src_alpha');
   FdDestFactor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdDestFactor);

  FFdColor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   FdColor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdColor);

  FFdColorTransparency := TSFFloat.Create(Self, 'colorTransparency', 0.0);
   FdColorTransparency.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdColorTransparency);

  DefaultContainerField := 'blendMode';
end;

class function TKambiAppearanceNode.ClassNodeTypeName: string;
begin
  Result := 'KambiAppearance';
end;

class function TKambiAppearanceNode.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Appearance node. }
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TKambiAppearanceNode.CreateNode;
begin
  inherited;
end;

procedure TKambiInlineNode.CreateNode;
begin
  inherited;

  FFdReplaceNames := TMFString.Create(Self, 'replaceNames', []);
  Fields.Add(FFdReplaceNames);

  FFdReplaceNodes := TMFNode.Create(Self, 'replaceNodes', [TX3DNode]);
  Fields.Add(FFdReplaceNodes);
end;

class function TKambiInlineNode.ClassNodeTypeName: string;
begin
  Result := 'KambiInline';
end;

class function TKambiInlineNode.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Inline node. }
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TKambiInlineNode.AfterLoadInlined;
var
  I: Integer;
begin
  ReplaceCount := Min(FdreplaceNames.Count, FdreplaceNodes.Count);
  if (FdreplaceNames.Count <> ReplaceCount) or
     (FdreplaceNodes.Count <> ReplaceCount) then
    OnWarning(wtMajor, 'VRML/X3D', 'replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successfull loading, there's always exactly one child. }
  Assert(VRML1ChildrenCount = 1);

  { First, a special case when Children[0] node has the required name.
    Needs to be checked explicitly, EnumerateReplaceNodes cannot check this. }
  for I := 0 to ReplaceCount - 1 do
    { Ignore empty names, since VRML node cannot have empty name
      (it's only our implementation detail that we mark "noname" as empty name) }
    if (FdReplaceNames.Items[I] <> '') and
       (FdReplaceNames.Items[I] = VRML1Children[0].NodeName) then
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
       (FdReplaceNames.Items[I] = Node.NodeName) then
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
  Fields.Add(FFdTimeOriginAtLoad);

  FFdOctreeRendering := TSFNode.Create(Self, 'octreeRendering', [TKambiOctreePropertiesNode]);
  FFdOctreeRendering.Exposed := false;
  Fields.Add(FFdOctreeRendering);

  FFdOctreeDynamicCollisions := TSFNode.Create(Self, 'octreeDynamicCollisions', [TKambiOctreePropertiesNode]);
  FFdOctreeDynamicCollisions.Exposed := false;
  Fields.Add(FFdOctreeDynamicCollisions);

  FFdOctreeVisibleTriangles := TSFNode.Create(Self, 'octreeVisibleTriangles', [TKambiOctreePropertiesNode]);
  FFdOctreeVisibleTriangles.Exposed := false;
  Fields.Add(FFdOctreeVisibleTriangles);

  FFdOctreeCollidableTriangles := TSFNode.Create(Self, 'octreeCollidableTriangles', [TKambiOctreePropertiesNode]);
  FFdOctreeCollidableTriangles.Exposed := false;
  Fields.Add(FFdOctreeCollidableTriangles);

  FFdHeadBobbing := TSFFloat.Create(Self, 'headBobbing', DefaultHeadBobbing, true);
   FdHeadBobbing.ChangesAlways := [chNavigationInfo];
  Fields.Add(FFdHeadBobbing);

  FFdHeadBobbingTime := TSFFloat.Create(Self, 'headBobbingTime', DefaultHeadBobbingTime, true);
   FdHeadBobbingTime.ChangesAlways := [chNavigationInfo];
  Fields.Add(FdHeadBobbingTime);

  FFdHeadlightNode := TSFNode.Create(Self, 'headlightNode', [TAbstractLightNode]);
  Fields.Add(FFdHeadlightNode);
end;

class function TKambiNavigationInfoNode.ClassNodeTypeName: string;
begin
  Result := 'KambiNavigationInfo';
end;

class function TKambiNavigationInfoNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TKambiNavigationInfoNode.OctreeProperties(
  OP: TSceneOctreeProperties): TKambiOctreePropertiesNode;
var
  N: TX3DNode;
begin
  case OP of
    opRendering           : N := FdOctreeRendering          .Value;
    opDynamicCollisions   : N := FdOctreeDynamicCollisions  .Value;
    opVisibleTriangles    : N := FdOctreeVisibleTriangles   .Value;
    opCollidableTriangles : N := FdOctreeCollidableTriangles.Value;
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
  Fields.Add(FFdMaxDepth);

  FFdLeafCapacity := TSFInt32.Create(Self, 'leafCapacity', -1);
   FdLeafCapacity.ChangesAlways := [chEverything];
  Fields.Add(FFdLeafCapacity);
end;

class function TKambiOctreePropertiesNode.ClassNodeTypeName: string;
begin
  Result := 'KambiOctreeProperties';
end;

class function TKambiOctreePropertiesNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TKambiOctreePropertiesNode.OverrideLimits(var Limits: TOctreeLimits);
begin
  if FdMaxDepth.Value >= 0 then
    Limits.MaxDepth := FdMaxDepth.Value else
  if FdMaxDepth.Value <> -1 then
    OnWarning(wtMajor, 'VRML/X3D', 'KambiOctreeProperties.maxDepth must be >= -1');

  if FdLeafCapacity.Value >= 0 then
    Limits.LeafCapacity := FdLeafCapacity.Value else
  if FdLeafCapacity.Value <> -1 then
    OnWarning(wtMajor, 'VRML/X3D', 'KambiOctreeProperties.leafCapacity must be >= -1');
end;

procedure TGeneratedShadowMapNode.CreateNode;
begin
  inherited;

  FFdUpdate := TSFStringUpdate.Create(Self, 'update', 'NONE');
  Fields.Add(FFdUpdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdSize := TSFInt32.Create(Self, 'size', 128, { MustBeNonnegative } true);
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdSize);
  { X3D specification comment: (0,Inf) }

  { Note that light node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    GeneratedShadowMap. }
  FFdLight := TSFNode.Create(Self, 'light', [TAbstractLightNode]);
   FdLight.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdLight);

  FFdScale := TSFFloat.Create(Self, 'scale', DefaultShadowMapScale, { MustBeNonnegative } true);
   FdScale.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdScale);

  FFdBias := TSFFloat.Create(Self, 'bias', DefaultShadowMapBias, { MustBeNonnegative } true);
   FdBias.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdBias);

  FFdCompareMode := TSFString.Create(Self, 'compareMode', 'COMPARE_R_LEQUAL');
   FdCompareMode.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdCompareMode);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FdUpdate := FdUpdate;
end;

destructor TGeneratedShadowMapNode.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TGeneratedShadowMapNode.ClassNodeTypeName: string;
begin
  Result := 'GeneratedShadowMap';
end;

class function TGeneratedShadowMapNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TGeneratedShadowMapNode.Light: TAbstractLightNode;
begin
  if FdLight.Value is TAbstractLightNode then
    Result := TAbstractLightNode(FdLight.Value) else
    Result := nil;
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
  Fields.Add(FFdProjector);
end;

class function TProjectedTextureCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'ProjectedTextureCoordinate';
end;

class function TProjectedTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TScreenEffectNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
   FdEnabled.ChangesAlways := [chScreenEffectEnabled];
  Fields.Add(FFdEnabled);

  FFdShaders := TMFNodeShaders.Create(Self, 'shaders', [TAbstractShaderNode]);
   FdShaders.ChangesAlways := [chEverything];
  Fields.Add(FFdShaders);

  FFdNeedsDepth := TSFBool.Create(Self, 'needsDepth', false);
   { no change needed, TVRMLGLScene recalculates NeedsDepth every time
     when ScreenEffectsNeedDepth is called by scene manager. }
   FdNeedsDepth.ChangesAlways := [];
  Fields.Add(FFdNeedsDepth);

  FStateForShaderPrepare := TVRMLGraphTraverseState.Create;
end;

destructor TScreenEffectNode.Destroy;
begin
  FreeAndNil(FStateForShaderPrepare);
  inherited;
end;

class function TScreenEffectNode.ClassNodeTypeName: string;
begin
  Result := 'ScreenEffect';
end;

class function TScreenEffectNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TScreenEffectNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  inherited;
  FFdShaders.EnumerateValid(Func);
end;

class function TMultiGeneratedTextureCoordinateNode.ClassNodeTypeName: string;
begin
  Result := 'MultiGeneratedTextureCoordinate';
end;

class function TMultiGeneratedTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TEffectNode.CreateNode;
begin
  inherited;

  FFdLanguage := TSFString.Create(Self, 'language', '');
   FdLanguage.Exposed := false;
   FdLanguage.ChangesAlways := [chEverything];
  Fields.Add(FFdLanguage);

  FFdParts := TMFNode.Create(Self, 'parts', [TEffectPartNode]);
   FdParts.Exposed := false;
   FdParts.ChangesAlways := [chEverything];
  Fields.Add(FFdParts);

  FFdEnabled := TSFBool.Create(Self, 'enabled', true);
   FdEnabled.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdEnabled);

  HasInterfaceDeclarations := AllAccessTypes;
end;

class function TEffectNode.ClassNodeTypeName: string;
begin
  Result := 'Effect';
end;

class function TEffectNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TEffectPartNode.CreateNode;
begin
  inherited;

  FFdType := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
   FdType.Exposed := false;
   FdType.ChangesAlways := [chEverything];
  Fields.Add(FFdType);

  FFdUrl := TMFString.Create(Self, 'url', []);
   FdUrl.Exposed := false;
   FdUrl.ChangesAlways := [chEverything];
  Fields.Add(FFdUrl);

  DefaultContainerField := 'parts';
end;

class function TEffectPartNode.ClassNodeTypeName: string;
begin
  Result := 'EffectPart';
end;

class function TEffectPartNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TEffectPartNode.LoadContents: string;
begin
  Result := ShaderLoadContents(FUsedFullUrl, Self, FdUrl);
end;

procedure TShaderTextureNode.CreateNode;
begin
  inherited;

  FFdDefaultTexCoord := TSFString.Create(Self, 'defaultTexCoord', 'BOUNDS2D');
   FdDefaultTexCoord.Exposed := false;
   FdDefaultTexCoord.ChangesAlways := [chEverything];
  Fields.Add(FFdDefaultTexCoord);
end;

class function TShaderTextureNode.ClassNodeTypeName: string;
begin
  Result := 'ShaderTexture';
end;

class function TShaderTextureNode.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure RegisterKambiNodes;
begin
  NodesManager.RegisterNodeClasses([
    TKambiTriangulationNode,
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
