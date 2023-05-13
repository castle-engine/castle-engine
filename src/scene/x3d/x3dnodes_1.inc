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

{ VRML 1.0 specification nodes.

  Note: If a node is present in both VRML 1.0 and later VRML / X3D versions
  (VRML 2.0 aka VRML 97, X3D 3, X3D 4...)
  and it's implemented using the same class,
  then it is inside x3d_standard_xxx file, not here.
  In practice there are not so many such nodes: maybe only TNormalNode.

  This file is only for VRML 1.0 nodes, that have no purpose
  in later VRML / X3D versions.
  Most nodes in VRML 1.0 vs VRML 2.0 specifications are different
  enough that even when they have the same
  name (like Group) we implement them using different classes (like
  TGroupNode_1 and TGroupNode).

  Note that most VRML 1 nodes descend from TAbstractChildNode,
  this way we can use them inside VRML >= 2.0 group nodes and
  mix VRML 1.0 and greater versions.
} { }

{$ifdef read_interface}
  { Geometry node allowed only in VRML <= 1.0.

    In VRML 1.0 shape nodes are allowed pretty everywhere,
    while VRML 2.0 has different idea of how shapes are handled
    (they must be inside Shape node), so no shape node
    is suitable at the same time for VRML 1.0 and VRML 2.0. }
  TAbstractGeometryNode_1 = class(TAbstractGeometryNode)
  {$I auto_generated_node_helpers/x3dnodes_abstractvrml1geometry_1.inc}
  end;

  TAsciiTextNode_1 = class(TAbstractGeometryNode_1)
  strict private
    FFontTextureNode: TPixelTextureNode;
  public
    destructor Destroy; override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function FontTextureNode: TAbstractTexture2DNode; override;

    { Force recalculating the shape when font changed.
      For now, we don't detect font changes (e.g. when TFontStyleNode.CustomFont changed)
      automatically. }
    procedure FontChanged;

    function Justify: TX3DFontJustify;
  {$I auto_generated_node_helpers/x3dnodes_asciitext_1.inc}
  end;

  TConeNode_1 = class(TAbstractGeometryNode_1)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_cone_1.inc}
  end;

  TCubeNode_1 = class(TAbstractGeometryNode_1)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_cube_1.inc}
  end;

  TCylinderNode_1 = class(TAbstractGeometryNode_1)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_cylinder_1.inc}
  end;

  { Common base class for VRML 1.0 indexed nodes
    (IndexedFaceSet, IndexedTriangleMesh, IndexedLineSet). }
  TAbstractIndexedNode_1 = class(TAbstractGeometryNode_1)
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndexField: TMFLong; override;
  {$I auto_generated_node_helpers/x3dnodes_abstractvrml1indexed_1.inc}
  end;

  { Common base class for VRML 1.0 indexed polygon nodes
    (IndexedFaceSet and IndexedTriangleMesh). }
  TIndexedFacesOrTrianglesNode_1 = class(TAbstractIndexedNode_1)
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TIndexedFaceSetNode_1 = class(TIndexedFacesOrTrianglesNode_1)
  public
    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_indexedfaceset_1.inc}
  end;

  TIndexedLineSetNode_1 = class(TAbstractIndexedNode_1)
  public
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
    { Do we have enough normals information to render this node lit.
      VRML 1.0 specification explicitly mentions that IndexedLineSet is treated
      specially: it's unlit if there are not enough normals specified. }
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_indexedlineset_1.inc}
  end;

  TPointSetNode_1 = class(TAbstractGeometryNode_1)
  strict private
    CoordSubrange: TMFVec3f;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_pointset_1.inc}
  end;

  TSphereNode_1 = class(TAbstractGeometryNode_1)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_sphere_1.inc}
  end;

  TCoordinate3Node_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_coordinate3_1.inc}
  end;

  TFontStyleNode_1 = class(TAbstractChildNode)
  public
    function Family: TX3DFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function Font: TCastleFont;
  {$I auto_generated_node_helpers/x3dnodes_fontstyle_1.inc}
  end;

  TInfoNode_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_info_1.inc}
  end;

  TLODNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_lod_1.inc}
  end;

  TMaterialNode_1 = class(TAbstractChildNode)
  strict private
    type
      TMaterialInfo_1 = class(TPhongMaterialInfo)
      strict private
        FNode: TMaterialNode_1;
        FIndex: Integer;
      protected
        function GetDiffuseColor: TVector3; override;
        procedure SetDiffuseColor(const Value: TVector3); override;
        function GetSpecularColor: TVector3; override;
        procedure SetSpecularColor(const Value: TVector3); override;
      public
        constructor Create(ANode: TMaterialNode_1; const Index: Integer);

        function AmbientColor: TVector3; override;
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
      FMaterialInfo: array of TMaterialInfo_1;
  public
    destructor Destroy; override;

    { Only the emissive color is not black (zero).
      This detects a special case described in VRML 1.0 specification:
      when ambient, diffuse and specular are all empty (no values),
      then emissiveColor should be used at the final color and shape
      should be unlit.

      You should use the EmissiveColor4Single in this case. }
    function PureEmissive: boolean;

    { Force the material pure emissive (see @link(PureEmissive)) by setting
      other colors to black. }
    procedure ForcePureEmissive; deprecated 'use X3D with TUnlitMaterialNode';

    { Material information based on this node.
      It is automatically updated when properties of this material change.
      Do not free it yourself, it will be automatically freed when
      this node is freed. }
    function MaterialInfo(const AIndex: Integer = 0): TPhongMaterialInfo;
  {$I auto_generated_node_helpers/x3dnodes_material_1.inc}
  end;

  TMaterialBindingNode_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_materialbinding_1.inc}
  end;

  TNormalBindingNode_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_normalbinding_1.inc}
  end;

  TTexture2Node_1 = class(TAbstractTexture2DNode)
  strict protected
    { Texture is loaded from file or inlined.
      The priority has the filename, only if it's empty (or an exception
      occurs during file loading) then the inlined texture will be used.

      Note that in VRML 1.0 a node without any texture
      (that is, when IsTextureLoaded = true and still
      IsTextureImage = false) is also useful: it turns off using the previous
      texture. }
    procedure LoadTextureData(out CacheUsed: boolean); override;
  protected
    procedure ParseAfter(Reader: TX3DReaderNames); override;
  public
    function TextureDescription: string; override;
  {$I auto_generated_node_helpers/x3dnodes_texture2_1.inc}
  end;

  TTexture2TransformNode_1 = class(TAbstractChildNode)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    function TextureMatrixTransformation: TMatrix4;
  {$I auto_generated_node_helpers/x3dnodes_texture2transform_1.inc}
  end;

  TTextureCoordinate2Node_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_texturecoordinate2_1.inc}
  end;

  TShapeHintsNode_1 = class(TAbstractChildNode)
  protected
    function ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
      const APositionInParent: Integer): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_shapehints_1.inc}
  end;

  { Common base class for all VRML 1 nodes that modify the transformation. }
  TAbstractTransformationNode_1 = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_abstractvrml1transformation_1.inc}
  end;

  { VRML 1.0 MatrixTransform node. }
  TMatrixTransformNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_matrixtransform_1.inc}
  end;

  TRotationNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_rotation_1.inc}
  end;

  TScaleNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_scale_1.inc}
  end;

  TTransformNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_transform_1.inc}
  end;

  TTranslationNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  {$I auto_generated_node_helpers/x3dnodes_translation_1.inc}
  end;

  { Common base class for all cameras in VRML 1.0. }
  TAbstractCameraNode_1 = class(TAbstractViewpointNode)
  strict protected
    function PositionField: TSFVec3f; override;
  {$I auto_generated_node_helpers/x3dnodes_abstractvrml1camera_1.inc}
  end;

  TOrthographicCameraNode_1 = class(TAbstractCameraNode_1)
  public
    class function ProjectionType: TProjectionType; override;
  {$I auto_generated_node_helpers/x3dnodes_orthographiccamera_1.inc}
  end;

  TPerspectiveCameraNode_1 = class(TAbstractCameraNode_1)
  public
    class function ProjectionType: TProjectionType; override;
  {$I auto_generated_node_helpers/x3dnodes_perspectivecamera_1.inc}
  end;

  TDirectionalLightNode_1 = class(TAbstractDirectionalLightNode)
  public
    function Scope: TLightScope; override;
  {$I auto_generated_node_helpers/x3dnodes_directionallight_1.inc}
  end;

  TPointLightNode_1 = class(TAbstractPointLightNode)
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    function HasRadius: boolean; override;
    function Scope: TLightScope; override;
  {$I auto_generated_node_helpers/x3dnodes_pointlight_1.inc}
  end;

  TSpotLightNode_1 = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
    function  GetProjectionDirectionLocal: TVector3; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3); override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    { Spot exponent (based on dropOffRate).
      Not normalized (i.e. is a real exponent, in VRML 1.0 expresses
      in [0..1] range to mean exponents [0..128]).
      Clamp to correct range. }
    function SpotExponent: Single;

    { Spot cutoff angle (based on cutOffAngle).

      Expressed in degrees, clamped to correct range
      (see TSpotLightNode.SpotCutoffDeg for reasons).
      (since user can input any value in VRML, and also conversion
      radians -> degrees could accidentally raise value slightly > 90,
      so cutOffAngle = 1.5708 is in degrees 90.0002104591,
      which would cause OpenGL fixed-function error). }
    function SpotCutoffDeg: Single;

    function SpotCosCutoff: Single;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;

    function ProjectionMatrix: TMatrix4; override;
    function ModelviewMatrix: TMatrix4; override;
    function ModelviewRotationMatrix: TMatrix4; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
    function HasRadius: boolean; override;
    function Scope: TLightScope; override;
  {$I auto_generated_node_helpers/x3dnodes_spotlight_1.inc}
  end;

  TGroupNode_1 = class(TAbstractChildNode)
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_group_1.inc}
  end;

  { Base class for VRML 1.0 nodes that push / pop all attributes and matrices.
    It is used in implementation of VRML 1.0 Separator and WWWAnchor.
    Also WWWInline does the same work, when it's "separate" field is true. }
  TAbstractSeparatorNode_1 = class(TAbstractChildNode)
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_abstractvrml1separator_1.inc}
  end;

  TSeparatorNode_1 = class(TAbstractSeparatorNode_1)
  {$I auto_generated_node_helpers/x3dnodes_separator_1.inc}
  end;

  TSwitchNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_switch_1.inc}
  end;

  TTransformSeparatorNode_1 = class(TAbstractChildNode)
  strict private
    OriginalTransformation: TTransformation;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_transformseparator_1.inc}
  end;

  TWWWAnchorNode_1 = class(TAbstractSeparatorNode_1)
  {$I auto_generated_node_helpers/x3dnodes_wwwanchor_1.inc}
  end;

  { VRML 1.0 WWWInline node.

    Implemented as a descendant of VRML 2.0/X3D Inline node
    class. This way VRML 1.0 actually gets a couple of VRML 2.0/X3D extensions.
    The VRML 2.0/X3D field "url" is renamed here to VRML 1.0 field "name".
    (Note that this means that WWWInline.name is actually MFString,
    not just SFString like VRML 1.0 spec says.) }
  TWWWInlineNode_1 = class(TInlineNode)
  protected
    function SeparateGroup: boolean; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_wwwinline_1.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

const
  { Constants for @link(TAsciiTextNode_1.FdJustification).Value. }
  JUSTIFICATION_LEFT = 0;
  JUSTIFICATION_CENTER = 1;
  JUSTIFICATION_RIGHT = 2;

  { Arrays with TSFEnum (VRML 1 specific field) possible values.
    They have to be zero-based (we use High()+1 assuming it). }
  JustificationNames1: array [0..2] of String = ('LEFT', 'CENTER', 'RIGHT');
  FamilyNames1: array [0..2] of String = ('SERIF', 'SANS', 'TYPEWRITER');
  BindingNames1: array [0..7] of String = ('DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED');
  WrapNames1: array [0..1] of String = ('REPEAT', 'CLAMP');
  TextureModelNames1: array [0..0] of String = ('DECAL');
  MapNames1: array [0..1] of String = ('NONE', 'POINT');
  RenderCullingNames1: array [0..2] of String = ('ON', 'OFF', 'AUTO');
  FaceTypeNames1: array [0..1] of String = ('UNKNOWN_FACE_TYPE', 'CONVEX');
  ShapeTypeNames1: array [0..1] of String = ('UNKNOWN_SHAPE_TYPE', 'SOLID');
  VertexOrderingNames1: array [0..2] of String = ('UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE');

destructor TAsciiTextNode_1.Destroy;
begin
  if X3DCache <> nil then
    X3DCache.FreeFontTexture(FFontTextureNode);
  inherited;
end;

procedure TAsciiTextNode_1.FontChanged;
begin
  if X3DCache <> nil then
    X3DCache.FreeFontTexture(FFontTextureNode);
end;

function TAsciiTextNode_1.Justify: TX3DFontJustify;
begin
  case FdJustification.Value of
    JUSTIFICATION_LEFT  : Result := fjBegin;
    JUSTIFICATION_CENTER: Result := fjMiddle;
    JUSTIFICATION_RIGHT : Result := fjEnd;
    else
      { something default }
      Result := fjBegin;
  end;
end;

function TAsciiTextNode_1.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  FaceSet: TIndexedFaceSetNode_1;
  CoordNode: TCoordinate3Node_1;
  TexCoordNode: TTextureCoordinate2Node_1;
  I: Integer;
begin
  FaceSet := TIndexedFaceSetNode_1.Create(X3DName, BaseUrl);
  try
    { we have to modify State, so make a copy of it }
    State := TX3DGraphTraverseState.CreateCopy(State);

    CoordNode := TCoordinate3Node_1.Create('', BaseUrl);
    CoordNode.FdPoint.Items.Clear;
    State.VRML1State.SetOwnNode(vsCoordinate3, CoordNode);

    TexCoordNode := TTextureCoordinate2Node_1.Create('', BaseUrl);
    TexCoordNode.FdPoint.Items.Clear;
    State.VRML1State.SetOwnNode(vsTextureCoordinate2, TexCoordNode);

    TextProxy(Self,
      State.VRML1State.FontStyle.FdSize.Value, FdSpacing.Value, Justify, fjFirst,
      FdString.Items, -1, State.VRML1State.FontStyle.Font,
      false, nil, nil, nil,
      CoordNode.FdPoint, TexCoordNode.FdPoint, nil);

    { calculate FaceSet.FdCoordIndex, just include all quads from CoordNode }
    FaceSet.FdCoordIndex.Items.Clear;
    for I := 0 to CoordNode.FdPoint.Count - 1 do
    begin
      FaceSet.FdCoordIndex.Items.Add(I);
      if (I + 1) mod 4 = 0 then
        FaceSet.FdCoordIndex.Items.Add(-1);
    end;

    { For VRML 1.0, unfortunately textureCoordIndex must be set
      (even though it's exactly equivalent to coordIndex). }
    FaceSet.FdTextureCoordIndex.Items.Assign(FaceSet.FdCoordIndex.Items);

    Result := FaceSet;
  except FreeAndNil(FaceSet); raise end;

  if FFontTextureNode = nil then
    FFontTextureNode := X3DCache.LoadFontTexture(
      State.VRML1State.FontStyle.Font,
      State.VRML1State.FontStyle.Blending);
end;

function TAsciiTextNode_1.FontTextureNode: TAbstractTexture2DNode;
begin
  Result := FFontTextureNode;
end;

function TConeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

function TCubeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

function TCylinderNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

constructor TAbstractIndexedNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FdCoordIndex.SaveToStreamLineUptoNegative := true;
  FdTextureCoordIndex.SaveToStreamLineUptoNegative := true;
end;

function TAbstractIndexedNode_1.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  ACoord := State.VRML1State.Coordinate3.FdPoint;
end;

function TAbstractIndexedNode_1.CoordIndexField: TMFLong;
begin
  Result := FdCoordIndex;
end;

function TIndexedFaceSetNode_1.InternalTexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.VRML1State.TextureCoordinate2;
end;

function TIndexedLineSetNode_1.InternalTexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.VRML1State.TextureCoordinate2;
end;

{$warnings off} // using deprecated to keep supporting VRML 1
function TIndexedLineSetNode_1.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { This somewhat follows the logic of
    TAbstractNormalGenerator.NorImplementationFromVRML1Binding,
    answering "false" when NorImplementationFromVRML1Binding sets "niNone". }

  { for _INDEXED normal binding, check normalIndex non-empty }
  case State.VRML1State.NormalBinding.FdValue.Value of
    BIND_DEFAULT,
    BIND_PER_VERTEX_INDEXED,
    BIND_PER_PART_INDEXED,
    BIND_PER_FACE_INDEXED:
      Result := (FdNormalIndex.Count <> 0) and
                (FdNormalIndex.Items.L[0] >= 0);
    else
      Result := true;
  end;

  { check Normal.vector non-empty }
  if State.VRML1State.Normal.FdVector.Count = 0 then
    Result := false;
end;
{$warnings on}

constructor TPointSetNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  CoordSubrange := TMFVec3f.Create(Self, true, '', []);
end;

destructor TPointSetNode_1.Destroy;
begin
  FreeAndNil(CoordSubrange);
  inherited;
end;

function TPointSetNode_1.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;

  procedure CalculateRange(CoordsCount: Cardinal;
    out StartIndex, NumPoints: integer);
  begin
    startIndex := FdStartIndex.Value;
    numPoints := FdNumPoints.Value;
    if startIndex >= CoordsCount then
    begin
      startIndex := 0;
      numPoints := 0;
    end else
    begin
      if startIndex < 0 then
      begin
        if numPoints >= 0 then numPoints := numPoints + startIndex;
        startIndex := 0;
      end;

      {startIndex juz jest na pewno dobry, teraz ew. popraw numPoints}
      if numPoints >= 0 then
      begin
        if startIndex + numPoints > CoordsCount then
          numPoints := CoordsCount - startIndex;
      end else
        numPoints := CoordsCount - startIndex;
    end;
  end;

var
  FullCoord: TMFVec3f;
  StartIndex, NumPoints: integer;
begin
  Result := true;

  FullCoord := State.VRML1State.Coordinate3.FdPoint;
  CalculateRange(FullCoord.Count, StartIndex, NumPoints);

  if (StartIndex = 0) and (NumPoints = FullCoord.Count) then
    ACoord := FullCoord else
  begin
    { It's incredibly non-efficient to copy here, each time, coordinates
      contents. However, it's also the simplest correct implementation
      of the extremely-rare feature of startIndex / numPoints
      (VRML 1.0 is rare and outdated already!).

      As long as model stays static (and for strict VRML 1.0, it should
      always remain static), this will not be actually often called. }
    CoordSubrange.Items.Count := 0;
    CoordSubrange.Items.AddSubRange(FullCoord.Items, StartIndex, NumPoints);
    ACoord := CoordSubrange;
  end;
end;

function TPointSetNode_1.Lit(State: TX3DGraphTraverseState): boolean;
begin
  Result := false;
end;

function TSphereNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

function TFontStyleNode_1.Family: TX3DFontFamily;
begin
  Result := TX3DFontFamily(FdFamily.Value);
end;

{$warnings off}
function TFontStyleNode_1.Bold: boolean;
begin
  Result := FdStyle.Flags[FSSTYLE_BOLD];
end;

function TFontStyleNode_1.Italic: boolean;
begin
  Result := FdStyle.Flags[FSSTYLE_ITALIC];
end;
{$warnings on}

function TFontStyleNode_1.Font: TCastleFont;
begin
  Result := TFontStyleNode.GetDefaultFont(Family, Bold, Italic);
end;

constructor TLODNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

function TLODNode_1.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  { TODO: we should use here the LOD matching camera distance.
    For now, just like TLODNode.DirectEnumerateActive, we ignore the issue. }
  if VRML1ChildrenCount = 0 then
    raise EX3DError.Create('LOD node must have at least one child');

  Result := Func(Self, VRML1Children[0]);
  if Result <> nil then Exit;
end;

destructor TMaterialNode_1.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FMaterialInfo) - 1 do
    FreeAndNil(FMaterialInfo[I]);
  inherited;
end;

function TMaterialNode_1.PureEmissive: boolean;
begin
  Result := (FdAmbientColor.Count = 0) and
            (FdDiffuseColor.Count = 0) and
            (FdSpecularColor.Count = 0);
end;

procedure TMaterialNode_1.ForcePureEmissive;
begin
  FdAmbientColor .Count := 0; FdAmbientColor .Changed;
  FdDiffuseColor .Count := 0; FdDiffuseColor .Changed;
  FdSpecularColor.Count := 0; FdSpecularColor.Changed;
  Assert(PureEmissive);
end;

function TMaterialNode_1.MaterialInfo(const AIndex: Integer): TPhongMaterialInfo;
begin
  if AIndex >= Length(FMaterialInfo) then
    SetLength(FMaterialInfo, AIndex + 1);
  if FMaterialInfo[AIndex] = nil then
    FMaterialInfo[AIndex] := TMaterialInfo_1.Create(Self, AIndex);
  Result := FMaterialInfo[AIndex];
end;

{ TMaterialInfo_1 --------------------------------------------------------- }

constructor TMaterialNode_1.TMaterialInfo_1.Create(ANode: TMaterialNode_1; const Index: Integer);
begin
  inherited Create(ANode);
  FNode := ANode;
  FIndex := Index;
end;

function TMaterialNode_1.TMaterialInfo_1.AmbientColor: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdAmbientColor.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultAmbientColor
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.GetDiffuseColor: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdDiffuseColor.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultDiffuseColor
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

procedure TMaterialNode_1.TMaterialInfo_1.SetDiffuseColor(const Value: TVector3);
begin
  FNode.FdDiffuseColor.Send([Value]);
end;

function TMaterialNode_1.TMaterialInfo_1.GetSpecularColor: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdSpecularColor.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultSpecularColor
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

procedure TMaterialNode_1.TMaterialInfo_1.SetSpecularColor(const Value: TVector3);
begin
  FNode.FdSpecularColor.Send([Value]);
end;

function TMaterialNode_1.TMaterialInfo_1.EmissiveColor: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdEmissiveColor.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultEmissiveColor
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.Shininess: Single;
var
  A: TSingleList;
begin
  A := FNode.FdShininess.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultShininess
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflectionColor: TVector3;
var
  A: TSingleList;
  M: Single;
begin
  A := FNode.FdMirror.Items;
  if A.Count = 0 then
    M := 0.0 // TMaterialInfo.DefaultReflectionColor[0, 1 or 2]
  else
    M := A.L[Min(FIndex, A.Count - 1)];

  Result := Vector3(M, M, M);
end;

function TMaterialNode_1.TMaterialInfo_1.Transparency: Single;
var
  A: TSingleList;
begin
  A := FNode.FdTransparency.Items;
  if A.Count = 0 then
    Result := TMaterialInfo.DefaultTransparency
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflSpecularExp: Single;
var
  A: TSingleList;
begin
  A := FNode.FdReflSpecularExp.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransSpecularExp: Single;
var
  A: TSingleList;
begin
  A := FNode.FdTransSpecularExp.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

{ TTexture2Node_1 ------------------------------------------------------------ }

const
  { Constants for @link(TTexture2Node_1.FdWrapS).Value and @link(TTexture2Node_1.FdWrapT).Value. }
  TEXWRAP_REPEAT = 0;
  TEXWRAP_CLAMP = 1;

procedure TTexture2Node_1.ParseAfter(Reader: TX3DReaderNames);
begin
  inherited;

  { Implementation of VRML 1 node TTexture2Node_1
    just copies the FdWrapS/T values to X3D fields RepeatS/T at parsing.

    This makes code easier (the rest of code can just use
    TAbstractTexture2DNode.RepeatS/T).
    It also makes small unexpected issue -- if you would construct
    VRML 1 node TTexture2Node_1 by code,
    you actually should set RepeatS/T and not WrapS/T.
    But, as VRML 1.0 is deprecated and ancient format now, this is acceptable.
  }

  RepeatS := FdWrapS.Value = TEXWRAP_REPEAT;
  RepeatT := FdWrapT.Value = TEXWRAP_REPEAT;
end;

procedure TTexture2Node_1.LoadTextureData(out CacheUsed: boolean);
var
  FullUrl: string;
begin
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  { load from FdFilename }
  if FdFilename.Value <> '' then
  try
    FullUrl := PathFromBaseUrl(FdFilename.Value);
    FTextureImage := X3DCache.TextureImage_IncReference(
      FullUrl, FTextureComposite, FAlphaChannelData);
    FTextureUsedFullUrl := FullUrl;
    CacheUsed := true;
    Exit;
  except
    on E: Exception do
      { Remember that WritelnWarning *may* raise an exception. }
      WritelnWarning('Texture', Format(SLoadError,
        [E.ClassName, 'texture', URIDisplay(FullUrl), E.Message]));
  end;

  { Still not loaded (maybe FdFilename.Value is ''
    or loading image raised exception)? So try to use inlined texture. }
  if not FdImage.Value.IsEmpty then
    FTextureImage := FdImage.Value.MakeCopy;
end;

function TTexture2Node_1.TextureDescription: string;

  function InlinedDescr: string;
  begin
    Result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width,
        FdImage.Value.Height,
        BoolToStr(FdImage.Value.HasAlpha, true) ]);
  end;

begin
  if FdFilename.Value <> '' then
  begin
    Result := 'file "' +PathFromBaseUrl(FdFilename.Value) +'"';
    if not FdImage.Value.IsEmpty then
      Result := Result + (' (and '+InlinedDescr+')');
  end else
  if not FdImage.Value.IsEmpty then
    Result := InlinedDescr else
    Result := 'none';
end;

function TTexture2TransformNode_1.TextureMatrixTransformation: TMatrix4;
begin
  Result :=
    TranslationMatrix(Vector3(FdTranslation.Value + FdCenter.Value, 0)) *
    RotationMatrixRad(FdRotation.Value, Vector3(0, 0, 1)) *
    ScalingMatrix(Vector3(FdScaleFactor.Value[0], FdScaleFactor.Value[1], 1)) *
    TranslationMatrix(Vector3(-FdCenter.Value[0], -FdCenter.Value[1], 0));
end;

procedure TTexture2TransformNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.TextureTransform := StateStack.Top.TextureTransform *
    TextureMatrixTransformation;
end;

function TShapeHintsNode_1.ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
  const APositionInParent: Integer): boolean;
var
  Hints: TSFBitMask;
begin
  Result := inherited;

  if not Result then
  begin
    Result := (Lexer.Version.Major = 0) and
      (Lexer.Token = vtName) and
      (Lexer.TokenName = 'hints');
    if Result then
    begin
      Hints := TSFBitMask.Create(Self, true, 'hints',
        ['SOLID', 'ORDERED', 'CONVEX'], 'NONE', '',
        [ false,   true,      true]);
      try
        Lexer.NextToken;
        Hints.Parse(Lexer, Reader, false);
        {$warnings off} // using deprecated to keep supporting VRML 1
        if Hints.Flags[0] then
          FdShapeType.Value := SHTYPE_SOLID else
          FdShapeType.Value := SHTYPE_UNKNOWN;
        if Hints.Flags[1] then
          FdVertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE else
          FdVertexOrdering.Value := VERTORDER_UNKNOWN;
        if Hints.Flags[2] then
          FdFaceType.Value := FACETYPE_CONVEX else
          FdFaceType.Value := FACETYPE_UNKNOWN;
        {$warnings on}
      finally Hints.Free end;
    end;
  end;
end;

procedure TMatrixTransformNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.MultiplyMatrix(FdMatrix.Value);
end;

procedure TRotationNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(FdRotation.Value, Vector3(1, 1, 1), TVector3.Zero);
end;

procedure TScaleNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(TVector4.Zero, FdScaleFactor.Value, TVector3.Zero);
end;

procedure TTransformNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(
    FdCenter.Value,
    FdRotation.Value,
    FdScaleFactor.Value,
    FdScaleOrientation.Value,
    FdTranslation.Value);
end;

procedure TTranslationNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(TVector4.Zero, Vector3(1, 1, 1), FdTranslation.Value);
end;

function TAbstractCameraNode_1.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

class function TOrthographicCameraNode_1.ProjectionType: TProjectionType;
begin
  Result := ptOrthographic;
end;

class function TPerspectiveCameraNode_1.ProjectionType: TProjectionType;
begin
  Result := ptPerspective;
end;

function TDirectionalLightNode_1.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal else
    Result := lsLocalVRML1;
end;

constructor TPointLightNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3(0, 0, 1);
  FdLocation.DefaultValue := Vector3(0, 0, 1);
end;

function TPointLightNode_1.HasRadius: boolean;
begin
  Result := false;
end;

function TPointLightNode_1.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal else
    Result := lsLocalVRML1;
end;

constructor TSpotLightNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3(0, 0, 1);
  FdLocation.DefaultValue := Vector3(0, 0, 1);
end;

function TSpotLightNode_1.SpotExponent: Single;
begin
  Result := Clamped(FdDropOffRate.Value * 128.0, 0.0, 128.0);
end;

function TSpotLightNode_1.SpotCutoffDeg: Single;
begin
  Result := Min(90, RadToDeg(FdCutOffAngle.Value));
end;

function TSpotLightNode_1.SpotCosCutoff: Single;
begin
  Result := Cos(FdCutOffAngle.Value);
end;

procedure TSpotLightNode_1.UpdateLightInstance(var LightInstance: TLightInstance);
begin
  inherited;
  LightInstance.Direction :=
    LightInstance.Transform.MultDirection(FdDirection.Value).Normalize;
end;

function TSpotLightNode_1.ProjectionMatrix: TMatrix4;
var
  Angle, N, F: Single;
begin
  { If author didn't provide and CastleInternalShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  { Implementation just like for TSpotLightNode,
    except I was too lazy to add here projectionAngle. }
  Angle := 2 * FdCutOffAngle.Value;

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  Result := PerspectiveProjectionMatrixRad(Angle, 1, N, F);
end;

function TSpotLightNode_1.ModelviewMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := LookDirMatrix(Pos, Dir, Side, AUp);
end;

function TSpotLightNode_1.ModelviewRotationMatrix: TMatrix4;
var
  Pos, Dir, Side, AUp: TVector3;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := FastLookDirMatrix(Dir, AUp);
end;

function TSpotLightNode_1.GetProjectionLocationLocal: TVector3;
begin
  Result := FdLocation.Value;
end;

procedure TSpotLightNode_1.SetProjectionLocationLocal(const Value: TVector3);
begin
  FdLocation.Send(Value);
end;

function TSpotLightNode_1.GetProjectionDirectionLocal: TVector3;
begin
  Result := FdDirection.Value;
end;

procedure TSpotLightNode_1.SetProjectionDirectionLocal(const Value: TVector3);
begin
  FdDirection.Send(Value);
end;

function TSpotLightNode_1.HasRadius: boolean;
begin
  Result := false;
end;

procedure TSpotLightNode_1.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  { TODO: MaxDistance should be a little larger, as spot light rays
    are not parallel. }
  Box.DirectionDistances(ProjectionWorldLocation, ProjectionWorldDirection, MinDistance, MaxDistance);
end;

function TSpotLightNode_1.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal else
    Result := lsLocalVRML1;
end;

constructor TGroupNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

constructor TAbstractSeparatorNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

type
  TVRML1SeparatorEnumerator = class
    State: TX3DGraphTraverseState;
    function Enumerate(Node, Child: TX3DNode): Pointer;
  end;

function TVRML1SeparatorEnumerator.Enumerate(Node, Child: TX3DNode): Pointer;
begin
  Result := nil;
  if Child is TLocalFogNode then
    State.LocalFog := TLocalFogNode(Child);
end;

procedure TAbstractSeparatorNode_1.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
var
  Enumerator: TVRML1SeparatorEnumerator;
begin
  inherited;
  StateStack.Push;

  { Use TVRML1SeparatorEnumerator, to propagate LocalFog into children,
    just like VRML >= 2 grouping nodes. Otherwise LocalFog would
    never work in VRML 1.0. }
  Enumerator := TVRML1SeparatorEnumerator.Create;
  try
    Enumerator.State := StateStack.Top;
    DirectEnumerateActive({$ifdef FPC}@{$endif} Enumerator.Enumerate);
  finally FreeAndNil(Enumerator) end;
end;

procedure TAbstractSeparatorNode_1.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  StateStack.Pop;
  inherited;
end;

constructor TSwitchNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

function TSwitchNode_1.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := nil;

  if FdWhichChild.Value = -3 then
  begin
    { Enumerate all.
      Note : value -3 is already deprecated in VRML 1.0;
      but I support it, at least for now. }
    Result := inherited;
    if Result <> nil then Exit;
  end else
  begin
    { Jezeli whichChild jest nieprawidlowe to w rezultacie nie wejdziemy w
      zadne Child. Wpp. wejdziemy w jedno wyznaczone child. I o to chodzi.
      (note : value -1 is no special value; any value that doesn't specify
      valid child number and is not -3 instructs Switch to not enter
      into any child. This is conforming with the VRML 97 specification) }
    if Between(FdWhichChild.Value, 0, VRML1ChildrenCount - 1) then
    begin
      Result := Func(Self, VRML1Children[FdWhichChild.Value]);
      if Result <> nil then Exit;
    end;
  end;
end;

constructor TTransformSeparatorNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

procedure TTransformSeparatorNode_1.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;

  { We don't copy whole State.Top here, as changes to other properties of
    state should "leak out" from TransformSeparator node. }

  OriginalTransformation := StateStack.Top.Transformation;
end;

procedure TTransformSeparatorNode_1.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  StateStack.Top.Transformation := OriginalTransformation;
  inherited;
end;

constructor TWWWInlineNode_1.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;

  { change "url" field to "name", with default value being an empty string }
  FdUrl.AddAlternativeName('name', 1);
  FdUrl.Items.Add('');
  FdUrl.AssignDefaultValueFromValue;

  { bboxSize is (0, 0, 0) in VRML 1.0 (in VRML 2.0/X3D it's (-1, -1, -1)) }
  FdBboxSize.Value := Vector3(0, 0, 0);
  FdBboxSize.AssignDefaultValueFromValue;
end;

function TWWWInlineNode_1.SeparateGroup: boolean;
begin
  Result := FdSeparate.Value;
end;

procedure RegisterVRML1Nodes;
begin
  NodesManager.RegisterNodeClasses([
    TAsciiTextNode_1, TConeNode_1, TCubeNode_1, TCylinderNode_1,
    TIndexedFaceSetNode_1, TIndexedLineSetNode_1,
    TPointSetNode_1, TSphereNode_1,
    TCoordinate3Node_1, TFontStyleNode_1, TInfoNode_1, TLODNode_1, TMaterialNode_1,
    TMaterialBindingNode_1, TNormalBindingNode_1, TTexture2Node_1,
    TTexture2TransformNode_1,
    TTextureCoordinate2Node_1, TShapeHintsNode_1,
    TMatrixTransformNode_1, TRotationNode_1,
    TScaleNode_1, TTransformNode_1,
    TTranslationNode_1,
    TOrthographicCameraNode_1, TPerspectiveCameraNode_1,
    TDirectionalLightNode_1, TPointLightNode_1, TSpotLightNode_1,
    TGroupNode_1, TSeparatorNode_1, TSwitchNode_1, TTransformSeparatorNode_1,
    TWWWAnchorNode_1,
    TWWWInlineNode_1
  ]);
end;

{$endif read_implementation}
