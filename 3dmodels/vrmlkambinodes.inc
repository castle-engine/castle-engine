{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Nodes that are defined specifically by Kambi VRML game engine.
  See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php]. } { }

{$ifdef read_interface}
  TNodeKambiTriangulation = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdQuadricSlices: TSFLong;
    public property FdQuadricSlices: TSFLong read FFdQuadricSlices;

    private FFdQuadricStacks: TSFLong;
    public property FdQuadricStacks: TSFLong read FFdQuadricStacks;

    private FFdRectDivisions: TSFLong;
    public property FdRectDivisions: TSFLong read FFdRectDivisions;

    { zwracaja wartosc z odpowiedniego pola FdXxx lub,
      jesli ta wartosc jest -1, zwracaja Detail_Xxx.

      (Jesli ta wartosc jest nieprawidlowa to wywoluja VRMLNonFatalError
      a potem "po cichu" zmieniaja ta wartosc na wartosc wzieta z Detail_Xxx.
      A wiec poprawiaja blednego VRMLa.) }
    function QuadricStacks: Cardinal;
    function QuadricSlices: Cardinal;
    function RectDivisions: Cardinal;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeKambiHeadLight = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdstring: TMFString;
    public property Fdstring: TMFString read FFdstring;

    private FFdfontStyle: TSFNode;
    public property FdfontStyle: TSFNode read FFdfontStyle;

    private FFdlength: TMFFloat;
    public property Fdlength: TMFFloat read FFdlength;

    private FFdmaxExtent: TSFFloat;
    public property FdmaxExtent: TSFFloat read FFdmaxExtent;

    { Fields added compared to normal Text node: }

    { }
    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TNodeFontStyle_2. }
    function FontStyle: TNodeFontStyle_2;
  end;

  { BlendMode node.

    This is not standard X3D. It's modelled after Avalon extension on
    [http://www.instantreality.org/].
    See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
    and [http://www.instantreality.org/documentation/nodetype/BlendMode/].

    Actually, our version is just a subset of their node. }
  TNodeBlendMode = class(TNodeX3DAppearanceChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsrcFactor: TSFString;
    public property FdsrcFactor: TSFString read FFdsrcFactor;

    private FFddestFactor: TSFString;
    public property FddestFactor: TSFString read FFddestFactor;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdcolorTransparency: TSFFloat;
    public property FdcolorTransparency: TSFFloat read FFdcolorTransparency;
  end;

  TNodeKambiAppearance = class(TNodeAppearance)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdNormalMap: TSFNode;
    public property FdNormalMap: TSFNode read FFdNormalMap;

    private FFdHeightMap: TSFNode;
    public property FdHeightMap: TSFNode read FFdHeightMap;

    private FFdHeightMapScale: TSFFloat;
    public property FdHeightMapScale: TSFFloat read FFdHeightMapScale;

    { blendMode property.

      This is modelled after Avalon extension on [http://www.instantreality.org/].
      See [http://www.instantreality.org/documentation/nodetype/ManagedAppearance/]
      and [http://www.instantreality.org/documentation/nodetype/BlendMode/].
      It allows you to use BlendMode nodes, which I consider very useful. }
    private FFdBlendMode: TSFNode;
    public property FdBlendMode: TSFNode read FFdBlendMode;

    private FFdShadowCaster: TSFBool;
    public property FdShadowCaster: TSFBool read FFdShadowCaster;

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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdreplaceNames: TMFString;
    public property FdreplaceNames: TMFString read FFdreplaceNames;

    private FFdreplaceNodes: TMFNode;
    public property FdreplaceNodes: TMFNode read FFdreplaceNodes;
  end;

  TNodeKambiNavigationInfo = class(TNodeNavigationInfo)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtimeOriginAtLoad: TSFBool;
    public property FdtimeOriginAtLoad: TSFBool read FFdtimeOriginAtLoad;
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  TriangulationUseDef = -1;

constructor TNodeKambiTriangulation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdquadricSlices := TSFLong.Create(Self, 'quadricSlices', TriangulationUseDef);
  Fields.Add(FFdquadricSlices);

  FFdquadricStacks := TSFLong.Create(Self, 'quadricStacks', TriangulationUseDef);
  Fields.Add(FFdquadricStacks);

  FFdrectDivisions := TSFLong.Create(Self, 'rectDivisions', TriangulationUseDef);
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
   VRMLNonFatalError(Format('Node "KambiTriangulation" '+
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

constructor TNodeKambiHeadLight.Create(const ANodeName: string; const AWWWBasePath: string);
const
  HeadLightDefaultAmbientIntensity = 0;
  HeadLightDefaultAttenuation: TVector3Single = (1, 0, 0);
  HeadLightDefaultColor: TVector3Single = (1, 1, 1);
  HeadLightDefaultIntensity = 1.0;
  HeadLightDefaultSpot = false;
  HeadLightDefaultSpotCutOffAngle = 0.785398;
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

function TNodeKambiTriangulation.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
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

constructor TNodeText3D.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdstring := TMFString.Create(Self, 'string', []);
  Fields.Add(FFdstring);

  FFdfontStyle := TSFNode.Create(Self, 'fontStyle', [TNodeFontStyle_2]);
  Fields.Add(FFdfontStyle);

  FFdlength := TMFFloat.Create(Self, 'length', []);
  Fields.Add(FFdlength);

  FFdmaxExtent := TSFFloat.Create(Self, 'maxExtent', 0.0);
  Fields.Add(FFdmaxExtent);

  FFddepth := TSFFloat.Create(Self, 'depth', 0.1, true);
  Fields.Add(FFddepth);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
  Fields.Add(FFdsolid);
end;

function TNodeText3D.FontStyle: TNodeFontStyle_2;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TNodeFontStyle_2) then
    Result := TNodeFontStyle_2(FdFontStyle.Value) else
    Result := nil;
end;

function TNodeText3D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

constructor TNodeBlendMode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdsrcFactor := TSFString.Create(Self, 'srcFactor', 'src_alpha');
  Fields.Add(FFdsrcFactor);

  FFddestFactor := TSFString.Create(Self, 'destFactor', 'one_minus_src_alpha');
  Fields.Add(FFddestFactor);

  FFdcolor := TSFColor.Create(Self, 'color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);

  FFdcolorTransparency := TSFFloat.Create(Self, 'colorTransparency', 0.0);
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

constructor TNodeKambiAppearance.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdnormalMap := TSFNode.Create(Self, 'normalMap', [TNodeX3DTextureNode]);
  Fields.Add(FFdnormalMap);

  FFdheightMap := TSFNode.Create(Self, 'heightMap', [TNodeX3DTextureNode]);
  Fields.Add(FFdheightMap);

  FFdheightMapScale := TSFFloat.Create(Self, 'heightMapScale', DefaultHeightMapScale);
  Fields.Add(FFdheightMapScale);

  FFdblendMode := TSFNode.Create(Self, 'blendMode', [TNodeBlendMode]);
  Fields.Add(FFdblendMode);

  FFdShadowCaster := TSFBool.Create(Self, 'shadowCaster', true);
  Fields.Add(FFdShadowCaster);
end;

constructor TNodeKambiInline.Create(const ANodeName: string; const AWWWBasePath: string);
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
    VRMLNonFatalError('replaceNames and replaceNodes for KambiInline node should have the same number of items');

  { After successfull loading, there's always exactly one child. }
  Assert(ChildrenCount = 1);

  { First, a special case when Children[0] node has the required name.
    Needs to be checked explicitly, EnumerateReplaceNodes cannot check this. }
  for I := 0 to ReplaceCount - 1 do
    { Ignore empty names, since VRML node cannot have empty name
      (it's only our implementation detail that we mark "noname" as empty name) }
    if (FdReplaceNames.Items.Items[I] <> '') and
       (FdReplaceNames.Items.Items[I] = Children[0].NodeName) then
    begin
      RemoveChild(0);
      AddChild(0, FdReplaceNodes.Items[I]);
      Exit;
    end;

  Children[0].EnumerateReplaceChildren(@DoReplaceNodes);
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

constructor TNodeKambiNavigationInfo.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtimeOriginAtLoad := TSFBool.Create(Self, 'timeOriginAtLoad', false);
  Fields.Add(FFdtimeOriginAtLoad);
end;

class function TNodeKambiNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'KambiNavigationInfo';
end;

class function TNodeKambiNavigationInfo.URNMatching(const URN: string): boolean;
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
    TNodeKambiNavigationInfo
  ]);
end;

{$endif read_implementation}
