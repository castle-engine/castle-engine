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
  See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php]. } { }

{$ifdef read_interface}
  TNodeKambiTriangulation = class(TNodeX3DChildNode)
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdQuadricSlices: TSFLong;
    public property FdQuadricSlices: TSFLong read FFdQuadricSlices;

    private FFdQuadricStacks: TSFLong;
    public property FdQuadricStacks: TSFLong read FFdQuadricStacks;

    private FFdRectDivisions: TSFLong;
    public property FdRectDivisions: TSFLong read FFdRectDivisions;

    { zwracaja wartosc z odpowiedniego pola FdXxx lub,
      jesli ta wartosc jest -1, zwracaja Detail_Xxx.

      (Jesli ta wartosc jest nieprawidlowa to wywoluja VRMLWarning
      a potem "po cichu" zmieniaja ta wartosc na wartosc wzieta z Detail_Xxx.
      A wiec poprawiaja blednego VRMLa.) }
    function QuadricStacks: Cardinal;
    function QuadricSlices: Cardinal;
    function RectDivisions: Cardinal;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeKambiHeadLight = class(TNodeX3DChildNode)
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

  TNodeText3D = class(TNodeX3DGeometryNode)
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
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TNodeFontStyle. }
    function FontStyle: TNodeFontStyle;
  end;

  { BlendMode node.

    This is not standard X3D. It's modelled after Avalon extension on
    [http://www.instantreality.org/].
    See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
    and [http://www.instantreality.org/documentation/nodetype/BlendMode/].

    Actually, our version is just a subset of their node. }
  TNodeBlendMode = class(TNodeX3DAppearanceChildNode)
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

  TNodeKambiAppearance = class(TNodeAppearance)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeKambiInline = class(TNodeInline)
  private
    { This is internal only for DoReplaceNodes, initialized in AfterLoadInlined. }
    ReplaceCount: Integer;
    procedure DoReplaceNodes(ParentNode: TVRMLNode; var Node: TVRMLNode);
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

  TNodeKambiOctreeProperties = class;

  TNodeKambiNavigationInfo = class(TNodeNavigationInfo)
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
      Returns @nil if the field doesn't have allowed (TNodeKambiOctreeProperties)
      value (or if the field is actually @nil, naturally). }
    function OctreeProperties(OP: TSceneOctreeProperties): TNodeKambiOctreeProperties;
  end;

  TNodeKambiOctreeProperties = class(TNodeX3DNode)
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

  TNodeGeneratedShadowMap = class(TNodeX3DTextureNode)
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

    function Light: TNodeX3DLightNode;
  end;

  TNodeProjectedTextureCoordinate = class(TNodeX3DTextureCoordinateNode)
  public
    procedure CreateNode; override;

    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdProjector: TSFNode;
    public property FdProjector: TSFNode read FFdProjector;
  end;

  TNodeScreenEffect = class(TNodeX3DChildNode)
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
    See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_tex_coord]. }
  TNodeMultiGeneratedTextureCoordinate = class(TNodeMultiTextureCoordinate)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeEffect = class(TNodeX3DChildNode)
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

  TNodeEffectPart = class(TNodeX3DNode, INodeX3DUrlObject)
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
      used to load the shader contents. See also TNodeShaderPart.UsedFullUrl. }
    property UsedFullUrl: string read FUsedFullUrl;
  end;

  TNodeShaderTexture = class(TNodeX3DTextureNode)
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

procedure TNodeKambiTriangulation.CreateNode;
begin
  inherited;

  FFdquadricSlices := TSFLong.Create(Self, 'quadricSlices', TriangulationUseDef);
   FdquadricSlices.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdquadricSlices);

  FFdquadricStacks := TSFLong.Create(Self, 'quadricStacks', TriangulationUseDef);
   FdquadricStacks.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdquadricStacks);

  FFdrectDivisions := TSFLong.Create(Self, 'rectDivisions', TriangulationUseDef);
   FdrectDivisions.ChangesAlways := [chGeometryVRML1State];
  Fields.Add(FFdrectDivisions);
end;

class function TNodeKambiTriangulation.ClassNodeTypeName: string;
begin
  result := 'KambiTriangulation';
end;

{$define TRIANGULATION_DETAIL_FUNC:=
function TNodeKambiTriangulation.TRIANGULATION_DETAIL_FUNC_NAME: Cardinal;
begin
 if TRIANGULATION_DETAIL_FIELD.Value = TriangulationUseDef then
  result := TRIANGULATION_DETAIL_GLOBAL_VALUE else
 begin
  if Int64(TRIANGULATION_DETAIL_FIELD.Value) < Int64(TRIANGULATION_DETAIL_MIN) then
  begin
   VRMLWarning(vwSerious, Format('Node "KambiTriangulation" '+
     'field "%s" value is %d but must be >= %d (or = -1)',
     [TRIANGULATION_DETAIL_FIELD_STRING,
      TRIANGULATION_DETAIL_FIELD.Value,
      TRIANGULATION_DETAIL_MIN]));
   TRIANGULATION_DETAIL_FIELD.Value := TRIANGULATION_DETAIL_GLOBAL_VALUE;
  end;

  result := TRIANGULATION_DETAIL_FIELD.Value;
 end;
end;}

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricSlices}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricSlices}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricSlices'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricStacks}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricStacks}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricStacks'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := RectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD := FdRectDivisions}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_RectDivisions}
  {$define TRIANGULATION_DETAIL_MIN := MinRectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'rectDivisions'}
  TRIANGULATION_DETAIL_FUNC

{$undef TRIANGULATION_DETAIL_FUNC_NAME}
{$undef TRIANGULATION_DETAIL_FIELD}
{$undef TRIANGULATION_DETAIL_GLOBAL_VALUE}
{$undef TRIANGULATION_DETAIL_MIN}
{$undef TRIANGULATION_DETAIL_FIELD_STRING}
{$undef TRIANGULATION_DETAIL_FUNC}

class function TNodeKambiTriangulation.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeKambiHeadLight.ClassNodeTypeName: string;
begin
  Result := 'KambiHeadLight';
end;

procedure TNodeKambiHeadLight.CreateNode;
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

  FFdambientIntensity := TSFFloat.Create(Self, 'ambientIntensity', HeadLightDefaultAmbientIntensity);
  Fields.Add(FFdambientIntensity);

  FFdattenuation := TSFVec3f.Create(Self, 'attenuation', HeadLightDefaultAttenuation);
  Fields.Add(FFdattenuation);

  FFdcolor := TSFColor.Create(Self, 'color', HeadLightDefaultColor);
  Fields.Add(FFdcolor);

  FFdintensity := TSFFloat.Create(Self, 'intensity', HeadLightDefaultIntensity);
  Fields.Add(FFdintensity);

  FFdspot := TSFBool.Create(Self, 'spot', HeadLightDefaultSpot);
  Fields.Add(FFdspot);

  FFdspotCutOffAngle := TSFFloat.Create(Self, 'spotCutOffAngle', HeadLightDefaultSpotCutOffAngle);
  Fields.Add(FFdspotCutOffAngle);

  FFdspotDropOffRate := TSFFloat.Create(Self, 'spotDropOffRate', HeadLightDefaultSpotDropOffRate);
  Fields.Add(FFdspotDropOffRate);
end;

class function TNodeKambiHeadLight.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeText3D.ClassNodeTypeName: string;
begin
  Result := 'Text3D';
end;

class function TNodeText3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeText3D.CreateNode;
begin
  inherited;

  FFdstring := TMFString.Create(Self, 'string', []);
   Fdstring.ChangesAlways := [chGeometry];
  Fields.Add(FFdstring);

  FFdfontStyle := TSFNode.Create(Self, 'fontStyle', [TNodeFontStyle]);
   FdfontStyle.ChangesAlways := [chGeometry];
  Fields.Add(FFdfontStyle);

  FFdlength := TMFFloat.Create(Self, 'length', []);
   Fdlength.ChangesAlways := [chGeometry];
  Fields.Add(FFdlength);

  FFdmaxExtent := TSFFloat.Create(Self, 'maxExtent', 0.0);
   FdmaxExtent.ChangesAlways := [chGeometry];
  Fields.Add(FFdmaxExtent);

  FFddepth := TSFFloat.Create(Self, 'depth', 0.1, true);
   Fddepth.ChangesAlways := [chGeometry];
  Fields.Add(FFddepth);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
end;

function TNodeText3D.FontStyle: TNodeFontStyle;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TNodeFontStyle) then
    Result := TNodeFontStyle(FdFontStyle.Value) else
    Result := nil;
end;

class function TNodeBlendMode.ClassNodeTypeName: string;
begin
  Result := 'BlendMode';
end;

class function TNodeBlendMode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeBlendMode.CreateNode;
begin
  inherited;

  FFdsrcFactor := TSFString.Create(Self, 'srcFactor', 'src_alpha');
   FdsrcFactor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdsrcFactor);

  FFddestFactor := TSFString.Create(Self, 'destFactor', 'one_minus_src_alpha');
   FddestFactor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFddestFactor);

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
   Fdcolor.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdcolor);

  FFdcolorTransparency := TSFFloat.Create(Self, 'colorTransparency', 0.0);
   FdcolorTransparency.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdcolorTransparency);

  DefaultContainerField := 'blendMode';
end;

class function TNodeKambiAppearance.ClassNodeTypeName: string;
begin
  Result := 'KambiAppearance';
end;

class function TNodeKambiAppearance.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Appearance node. }
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeKambiAppearance.CreateNode;
begin
  inherited;
end;

procedure TNodeKambiInline.CreateNode;
begin
  inherited;

  FFdreplaceNames := TMFString.Create(Self, 'replaceNames', []);
  Fields.Add(FFdreplaceNames);

  FFdreplaceNodes := TMFNode.Create(Self, 'replaceNodes', [TVRMLNode]);
  Fields.Add(FFdreplaceNodes);
end;

class function TNodeKambiInline.ClassNodeTypeName: string;
begin
  Result := 'KambiInline';
end;

class function TNodeKambiInline.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Inline node. }
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeKambiInline.AfterLoadInlined;
var
  I: Integer;
begin
  ReplaceCount := Min(FdreplaceNames.Count, FdreplaceNodes.Count);
  if (FdreplaceNames.Count <> ReplaceCount) or
     (FdreplaceNodes.Count <> ReplaceCount) then
    VRMLWarning(vwSerious, 'replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successfull loading, there's always exactly one child. }
  Assert(VRML1ChildrenCount = 1);

  { First, a special case when Children[0] node has the required name.
    Needs to be checked explicitly, EnumerateReplaceNodes cannot check this. }
  for I := 0 to ReplaceCount - 1 do
    { Ignore empty names, since VRML node cannot have empty name
      (it's only our implementation detail that we mark "noname" as empty name) }
    if (FdReplaceNames.Items.Items[I] <> '') and
       (FdReplaceNames.Items.Items[I] = VRML1Children[0].NodeName) then
    begin
      VRML1ChildRemove(0);
      VRML1ChildAdd(0, FdReplaceNodes.Items[I]);
      Exit;
    end;

  VRML1Children[0].EnumerateReplaceChildren(@DoReplaceNodes);
end;

procedure TNodeKambiInline.DoReplaceNodes(ParentNode: TVRMLNode;
  var Node: TVRMLNode);
var
  I: Integer;
begin
  for I := 0 to ReplaceCount - 1 do
    if (FdReplaceNames.Items.Items[I] <> '') and
       (FdReplaceNames.Items.Items[I] = Node.NodeName) then
    begin
      Node := FdReplaceNodes.Items[I];
      Exit;
    end;
end;

procedure TNodeKambiNavigationInfo.CreateNode;
begin
  inherited;

  FFdtimeOriginAtLoad := TSFBool.Create(Self, 'timeOriginAtLoad', false);
  FFdtimeOriginAtLoad.Exposed := false;
  Fields.Add(FFdtimeOriginAtLoad);

  FFdoctreeRendering := TSFNode.Create(Self, 'octreeRendering', [TNodeKambiOctreeProperties]);
  FFdoctreeRendering.Exposed := false;
  Fields.Add(FFdoctreeRendering);

  FFdoctreeDynamicCollisions := TSFNode.Create(Self, 'octreeDynamicCollisions', [TNodeKambiOctreeProperties]);
  FFdoctreeDynamicCollisions.Exposed := false;
  Fields.Add(FFdoctreeDynamicCollisions);

  FFdoctreeVisibleTriangles := TSFNode.Create(Self, 'octreeVisibleTriangles', [TNodeKambiOctreeProperties]);
  FFdoctreeVisibleTriangles.Exposed := false;
  Fields.Add(FFdoctreeVisibleTriangles);

  FFdoctreeCollidableTriangles := TSFNode.Create(Self, 'octreeCollidableTriangles', [TNodeKambiOctreeProperties]);
  FFdoctreeCollidableTriangles.Exposed := false;
  Fields.Add(FFdoctreeCollidableTriangles);

  FFdHeadBobbing := TSFFloat.Create(Self, 'headBobbing', DefaultHeadBobbing, true);
   FdHeadBobbing.ChangesAlways := [chNavigationInfo];
  Fields.Add(FFdHeadBobbing);

  FFdHeadBobbingTime := TSFFloat.Create(Self, 'headBobbingTime', DefaultHeadBobbingTime, true);
   FdHeadBobbingTime.ChangesAlways := [chNavigationInfo];
  Fields.Add(FdHeadBobbingTime);

  FFdheadlightNode := TSFNode.Create(Self, 'headlightNode', [TNodeX3DLightNode]);
  Fields.Add(FFdheadlightNode);
end;

class function TNodeKambiNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'KambiNavigationInfo';
end;

class function TNodeKambiNavigationInfo.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeKambiNavigationInfo.OctreeProperties(
  OP: TSceneOctreeProperties): TNodeKambiOctreeProperties;
var
  N: TVRMLNode;
begin
  case OP of
    opRendering           : N := FdOctreeRendering          .Value;
    opDynamicCollisions   : N := FdOctreeDynamicCollisions  .Value;
    opVisibleTriangles    : N := FdOctreeVisibleTriangles   .Value;
    opCollidableTriangles : N := FdOctreeCollidableTriangles.Value;
    else raise EInternalError.Create('TNodeKambiOctreeProperties.OctreeProperties');
  end;
  if N is TNodeKambiOctreeProperties then
    Result := TNodeKambiOctreeProperties(N) else
    Result := nil;
end;

procedure TNodeKambiOctreeProperties.CreateNode;
begin
  inherited;

  FFdMaxDepth := TSFInt32.Create(Self, 'maxDepth', -1);
   FdMaxDepth.ChangesAlways := [chEverything];
  Fields.Add(FFdMaxDepth);

  FFdLeafCapacity := TSFInt32.Create(Self, 'leafCapacity', -1);
   FdLeafCapacity.ChangesAlways := [chEverything];
  Fields.Add(FFdLeafCapacity);
end;

class function TNodeKambiOctreeProperties.ClassNodeTypeName: string;
begin
  Result := 'KambiOctreeProperties';
end;

class function TNodeKambiOctreeProperties.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeKambiOctreeProperties.OverrideLimits(var Limits: TOctreeLimits);
begin
  if FdMaxDepth.Value >= 0 then
    Limits.MaxDepth := FdMaxDepth.Value else
  if FdMaxDepth.Value <> -1 then
    VRMLWarning(vwSerious, 'KambiOctreeProperties.maxDepth must be >= -1');

  if FdLeafCapacity.Value >= 0 then
    Limits.LeafCapacity := FdLeafCapacity.Value else
  if FdLeafCapacity.Value <> -1 then
    VRMLWarning(vwSerious, 'KambiOctreeProperties.leafCapacity must be >= -1');
end;

procedure TNodeGeneratedShadowMap.CreateNode;
begin
  inherited;

  FFdupdate := TSFStringUpdate.Create(Self, 'update', 'NONE');
  Fields.Add(FFdupdate);
  { X3D specification comment: ["NONE"|"NEXT_FRAME_ONLY"|"ALWAYS"] }

  FFdsize := TSFInt32.Create(Self, 'size', 128, { MustBeNonnegative } true);
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  { Note that light node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    GeneratedShadowMap. }
  FFdlight := TSFNode.Create(Self, 'light', [TNodeX3DLightNode]);
   Fdlight.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdlight);

  FFdscale := TSFFloat.Create(Self, 'scale', DefaultShadowMapScale, { MustBeNonnegative } true);
   Fdscale.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdscale);

  FFdbias := TSFFloat.Create(Self, 'bias', DefaultShadowMapBias, { MustBeNonnegative } true);
   Fdbias.ChangesAlways := [chGeneratedTextureUpdateNeeded];
  Fields.Add(FFdbias);

  FFdcompareMode := TSFString.Create(Self, 'compareMode', 'COMPARE_R_LEQUAL');
   FdcompareMode.ChangesAlways := [chTextureRendererProperties];
  Fields.Add(FFdcompareMode);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FdUpdate := FdUpdate;
end;

destructor TNodeGeneratedShadowMap.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TNodeGeneratedShadowMap.ClassNodeTypeName: string;
begin
  Result := 'GeneratedShadowMap';
end;

class function TNodeGeneratedShadowMap.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeGeneratedShadowMap.Light: TNodeX3DLightNode;
begin
  if FdLight.Value is TNodeX3DLightNode then
    Result := TNodeX3DLightNode(FdLight.Value) else
    Result := nil;
end;

procedure TNodeProjectedTextureCoordinate.CreateNode;
begin
  inherited;

  { Note that projector node is not enumerated as an active node
    for traversing (in DirectEnumerateActive), because the light doesn't
    shine here. We don't want
    to override it's transform with transformation of this
    ProjectedTextureCoordinate. }
  FFdprojector := TSFNode.Create(Self, 'projector',
    [TNodeSpotLight_1, TNodeDirectionalLight_1,
     TNodeSpotLight  , TNodeDirectionalLight  ,
     TNodeX3DViewpointNode]);
   FFdprojector.ChangesAlways := [chTextureCoordinate];
  Fields.Add(FFdprojector);
end;

class function TNodeProjectedTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'ProjectedTextureCoordinate';
end;

class function TNodeProjectedTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeScreenEffect.CreateNode;
begin
  inherited;

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
   Fdenabled.ChangesAlways := [chScreenEffectEnabled];
  Fields.Add(FFdenabled);

  FFdshaders := TMFNodeShaders.Create(Self, 'shaders', [TNodeX3DShaderNode]);
   Fdshaders.ChangesAlways := [chEverything];
  Fields.Add(FFdshaders);

  FFdneedsDepth := TSFBool.Create(Self, 'needsDepth', false);
   { no change needed, TVRMLGLScene recalculates NeedsDepth every time
     when ScreenEffectsNeedDepth is called by scene manager. }
   FdneedsDepth.ChangesAlways := [];
  Fields.Add(FFdneedsDepth);

  FStateForShaderPrepare := TVRMLGraphTraverseState.Create;
end;

destructor TNodeScreenEffect.Destroy;
begin
  FreeAndNil(FStateForShaderPrepare);
  inherited;
end;

class function TNodeScreenEffect.ClassNodeTypeName: string;
begin
  Result := 'ScreenEffect';
end;

class function TNodeScreenEffect.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeScreenEffect.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  inherited;
  FFdshaders.EnumerateValid(Func);
end;

class function TNodeMultiGeneratedTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'MultiGeneratedTextureCoordinate';
end;

class function TNodeMultiGeneratedTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeEffect.CreateNode;
begin
  inherited;

  FFdlanguage := TSFString.Create(Self, 'language', '');
   Fdlanguage.Exposed := false;
   Fdlanguage.ChangesAlways := [chEverything];
  Fields.Add(FFdlanguage);

  FFdParts := TMFNode.Create(Self, 'parts', [TNodeEffectPart]);
   FdParts.Exposed := false;
   FdParts.ChangesAlways := [chEverything];
  Fields.Add(FFdParts);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
   Fdenabled.ChangesAlways := [chVisibleNonGeometry];
  Fields.Add(FFdenabled);

  HasInterfaceDeclarations := AllAccessTypes;
end;

class function TNodeEffect.ClassNodeTypeName: string;
begin
  Result := 'Effect';
end;

class function TNodeEffect.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TNodeEffectPart.CreateNode;
begin
  inherited;

  FFdtype := TSFStringShaderType.Create(Self, 'type', 'VERTEX');
   Fdtype.Exposed := false;
   Fdtype.ChangesAlways := [chEverything];
  Fields.Add(FFdtype);

  FFdurl := TMFString.Create(Self, 'url', []);
   Fdurl.Exposed := false;
   Fdurl.ChangesAlways := [chEverything];
  Fields.Add(FFdurl);

  DefaultContainerField := 'parts';
end;

class function TNodeEffectPart.ClassNodeTypeName: string;
begin
  Result := 'EffectPart';
end;

class function TNodeEffectPart.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

function TNodeEffectPart.LoadContents: string;
begin
  Result := ShaderLoadContents(FUsedFullUrl, Self, FdUrl);
end;

procedure TNodeShaderTexture.CreateNode;
begin
  inherited;

  FFdDefaultTexCoord := TSFString.Create(Self, 'defaultTexCoord', 'BOUNDS2D');
   FdDefaultTexCoord.Exposed := false;
   FdDefaultTexCoord.ChangesAlways := [chEverything];
  Fields.Add(FFdDefaultTexCoord);
end;

class function TNodeShaderTexture.ClassNodeTypeName: string;
begin
  Result := 'ShaderTexture';
end;

class function TNodeShaderTexture.URNMatching(const URN: string): boolean;
begin
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure RegisterKambiNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeKambiTriangulation,
    TNodeKambiHeadLight,
    TNodeText3D,
    TNodeBlendMode,
    TNodeKambiAppearance,
    TNodeKambiInline,
    TNodeKambiNavigationInfo,
    TNodeKambiOctreeProperties,
    TNodeGeneratedShadowMap,
    TNodeProjectedTextureCoordinate,
    TNodeScreenEffect,
    TNodeMultiGeneratedTextureCoordinate,
    TNodeEffect,
    TNodeEffectPart,
    TNodeShaderTexture
  ]);
end;

{$endif read_implementation}
