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

{ VRML 1.0 specification nodes.

  If a node is present in both VRML 1.0
  and later (2.0 aka 97) specifications @bold(and) it's implemented using
  the same class in our engine, then it goes to vrml97* or x3d* file,
  not here. So this is for VRML 1.0-only nodes.

  (The above doesn't concern much nodes. Most nodes in VRML 1.0 and 97
  specifications are different enough that even when they have the same
  name (like Group) we implement them using different classes (like
  TGroupNode_1 and TGroupNode).)

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
  TAbstractGeometryNode_1 = class(TAbstractGeometryNode, IAbstractChildNode)
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TAsciiTextNode_1 = class(TAbstractGeometryNode_1)
  strict private
    FFontTextureNode: TPixelTextureNode;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    private FFdString: TMFString;
    public property FdString: TMFString read FFdString;

    private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    { Text justification.
      Use constants like JUSTIFICATION_LEFT and so on with this. }
    private FFdJustification: TSFEnum;
    public property FdJustification: TSFEnum read FFdJustification;

    private FFdWidth: TMFFloat;
    public property FdWidth: TMFFloat read FFdWidth;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function FontTextureNode: TAbstractTexture2DNode; override;

    function Justify: TX3DFontJustify;
  end;

  TConeNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    private FFdBottomRadius: TSFFloat;
    public property FdBottomRadius: TSFFloat read FFdBottomRadius;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TCubeNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdWidth: TSFFloat;
    public property FdWidth: TSFFloat read FFdWidth;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TCylinderNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  end;

  { Common base class for VRML 1.0 indexed nodes
    (IndexedFaceSet, IndexedTriangleMesh, IndexedLineSet). }
  TAbstractIndexedNode_1 = class(TAbstractGeometryNode_1)
  public
    procedure CreateNode; override;

    private FFdCoordIndex: TMFLong;
    public property FdCoordIndex: TMFLong read FFdCoordIndex;

    private FFdMaterialIndex: TMFLong;
    public property FdMaterialIndex: TMFLong read FFdMaterialIndex;

    private FFdNormalIndex: TMFLong;
    public property FdNormalIndex: TMFLong read FFdNormalIndex;

    private FFdTextureCoordIndex: TMFLong;
    public property FdTextureCoordIndex: TMFLong read FFdTextureCoordIndex;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndex: TMFLong; override;
  end;

  { Common base class for VRML 1.0 indexed polygon nodes
    (IndexedFaceSet and IndexedTriangleMesh). }
  TIndexedFacesOrTrianglesNode_1 = class(TAbstractIndexedNode_1)
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TIndexedFaceSetNode_1 = class(TIndexedFacesOrTrianglesNode_1)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;

    procedure CoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    private FFdRadianceTransfer: TMFVec3f;
    public property FdRadianceTransfer: TMFVec3f read FFdRadianceTransfer;

    function TexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
  end;

  TIndexedLineSetNode_1 = class(TAbstractIndexedNode_1)
    class function ClassX3DType: string; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function TexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;

    { Do we have enough normals information to render this node lit.
      VRML 1.0 specification explicitly mentions that IndexedLineSet is treated
      specially: it's unlit if there are not enough normals specified. }
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  end;

  TPointSetNode_1 = class(TAbstractGeometryNode_1)
  private
    CoordSubrange: TMFVec3f;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    private FFdStartIndex: TSFLong;
    public property FdStartIndex: TSFLong read FFdStartIndex;

    private FFdNumPoints: TSFLong;
    public property FdNumPoints: TSFLong read FFdNumPoints;

    function Coord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  end;

  TSphereNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TCoordinate3Node_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;
  end;

  TFontStyleNode_1 = class(TAbstractChildNode)
  strict private
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdSize: TSFFloat;
    public property FdSize: TSFFloat read FFdSize;

    private FFdFamily: TSFEnum;
    public property FdFamily: TSFEnum read FFdFamily;

    private FFdStyle: TSFBitMask;
    public property FdStyle: TSFBitMask read FFdStyle;

    private FFdBlending: TSFBool;
    public property FdBlending: TSFBool read FFdBlending;
    property Blending: boolean read GetBlending write SetBlending;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function Family: TX3DFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function Font: TTextureFontData;
  end;

  TInfoNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdString: TSFString;
    public property FdString: TSFString read FFdString;
  end;

  TLODNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdRange: TMFFloat;
    public property FdRange: TMFFloat read FFdRange;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TX3DMaterialInfo_1 = class;

  TMaterialNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdAmbientColor: TMFColor;
    public property FdAmbientColor: TMFColor read FFdAmbientColor;

    private FFdDiffuseColor: TMFColor;
    public property FdDiffuseColor: TMFColor read FFdDiffuseColor;

    private FFdSpecularColor: TMFColor;
    public property FdSpecularColor: TMFColor read FFdSpecularColor;

    private FFdEmissiveColor: TMFColor;
    public property FdEmissiveColor: TMFColor read FFdEmissiveColor;

    private FFdShininess: TMFFloat;
    public property FdShininess: TMFFloat read FFdShininess;

    private FFdTransparency: TMFFloat;
    public property FdTransparency: TMFFloat read FFdTransparency;

    { Fields used by ray-tracers. } { }
    private FFdMirror: TMFFloat;
    public property FdMirror: TMFFloat read FFdMirror;

    private FFdReflSpecular: TMFColor;
    public property FdReflSpecular: TMFColor read FFdReflSpecular;

    private FFdReflDiffuse: TMFColor;
    public property FdReflDiffuse: TMFColor read FFdReflDiffuse;

    private FFdTransSpecular: TMFColor;
    public property FdTransSpecular: TMFColor read FFdTransSpecular;

    private FFdTransDiffuse: TMFColor;
    public property FdTransDiffuse: TMFColor read FFdTransDiffuse;

    private FFdReflSpecularExp: TMFFloat;
    public property FdReflSpecularExp: TMFFloat read FFdReflSpecularExp;

    private FFdTransSpecularExp: TMFFloat;
    public property FdTransSpecularExp: TMFFloat read FFdTransSpecularExp;

    private FFdFogImmune: TSFBool;
    public property FdFogImmune: TSFBool read FFdFogImmune;

    { Easily extract VRML 1.0 material properties.
      These methods secure you from accessing non-existing material index
      (will return the last existing value, or default value if field is empty).

      Functions returning TVector4Single add Opacity at the last component.

      Transparency and Opacity are in [0 .. 1] range.
      Opacity = 1 - Transparency.

      ShininessExp is the @italic(not normalized) shininess exponent
      for Phong lighting equations. Normal VRML/X3D shininess field
      is "normalized", that is it has to be multiplied by 128 to get
      actual exponent for lighting equations.
      @groupBegin }
    function AmbientColor3Single(MatNum: integer): TVector3Single;
    function AmbientColor4Single(MatNum: integer): TVector4Single;
    function DiffuseColor3Single(MatNum: integer): TVector3Single;
    function DiffuseColor4Single(MatNum: integer): TVector4Single;
    function SpecularColor3Single(MatNum: integer): TVector3Single;
    function SpecularColor4Single(MatNum: integer): TVector4Single;
    function EmissiveColor3Single(MatNum: integer): TVector3Single;
    function EmissiveColor4Single(MatNum: integer): TVector4Single;
    function Transparency(MatNum: integer): Single;
    function Opacity(MatNum: integer): Single;
    function Shininess(MatNum: integer): Single;
    function ShininessExp(MatNum: integer): Single;

    function Mirror(MatNum: integer): Single;
    function ReflSpecularExp (MatNum: integer): Single;
    function TransSpecularExp(MatNum: integer): Single;
    { @groupEnd }

    { Only the emissive color is not black (zero).
      This detects a special case described in VRML 1.0 specification:
      when ambient, diffuse and specular are all empty (no values),
      then emissiveColor should be used at the final color and shape
      should be unlit.

      You should use the EmissiveColor4Single in this case. }
    function PureEmissive: boolean;

    { Force the material pure emissive (see @link(PureEmissive)) by setting
      other colors to black. }
    procedure ForcePureEmissive;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function MaterialInfo(const Index: Integer): TX3DMaterialInfo_1;
  end;

  TX3DMaterialInfo_1 = class(TX3DMaterialInfoAbstract)
  private
    FNode: TMaterialNode_1;
    FIndex: Integer;
  public
    constructor Create(Node: TMaterialNode_1; const Index: Integer);

    function DiffuseColor: TVector3Single; override;
    function Mirror: Single; override;
    function Transparency: Single; override;

    function ReflSpecular: TVector3Single; override;
    function ReflDiffuse: TVector3Single; override;
    function TransSpecular: TVector3Single; override;
    function TransDiffuse: TVector3Single; override;

    function ReflSpecularExp: Single; override;
    function TransSpecularExp: Single; override;
  end;

  TMaterialBindingNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;
  end;

  TNormalBindingNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;
  end;

  TTexture2Node_1 = class(TAbstractTexture2DNode)
  protected
    function GetRepeatS: boolean; override;
    function GetRepeatT: boolean; override;
    procedure SetRepeatS(const Value: boolean); override;
    procedure SetRepeatT(const Value: boolean); override;

    { Texture is loaded from file or inlined.
      The priority has the filename, only if it's empty (or an exception
      occurs during file loading) then the inlined texture will be used.

      Note that in VRML 1.0 a node without any texture
      (that is, when IsTextureLoaded = true and still
      IsTextureImage = false) is also useful: it turns off using the previous
      texture. }
    procedure LoadTextureData(out CacheUsed: boolean); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdFilename: TSFString;
    public property FdFilename: TSFString read FFdFilename;

    private FFdImage: TSFImage;
    public property FdImage: TSFImage read FFdImage;

    private FFdWrapS: TSFEnum;
    public property FdWrapS: TSFEnum read FFdWrapS;

    private FFdWrapT: TSFEnum;
    public property FdWrapT: TSFEnum read FFdWrapT;

    { Ignored fields, some unknown extensions to VRML 1.0 spec.
      Some models ([http://www-vrl.umich.edu/sel_prj/EECS498/]) use them.
      @groupBegin }
    private FFdModel: TSFEnum;
    public property FdModel: TSFEnum read FFdModel;

    private FFdBlendColor: TSFVec3f;
    public property FdBlendColor: TSFVec3f read FFdBlendColor;
    { @groupEnd }

    function TextureDescription: string; override;
  end;

  TTexture2TransformNode_1 = class(TAbstractChildNode)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    private FFdRotation: TSFFloat;
    public property FdRotation: TSFFloat read FFdRotation;

    private FFdScaleFactor: TSFVec2f;
    public property FdScaleFactor: TSFVec2f read FFdScaleFactor;

    private FFdCenter: TSFVec2f;
    public property FdCenter: TSFVec2f read FFdCenter;

    function TextureMatrixTransformation: TMatrix4Single;
  end;

  TTextureCoordinate2Node_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;
  end;

  TShapeHintsNode_1 = class(TAbstractChildNode)
  protected
    function ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
      const APositionInParent: Integer): boolean; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdVertexOrdering: TSFEnum;
    public property FdVertexOrdering: TSFEnum read FFdVertexOrdering;

    private FFdShapeType: TSFEnum;
    public property FdShapeType: TSFEnum read FFdShapeType;

    private FFdFaceType: TSFEnum;
    public property FdFaceType: TSFEnum read FFdFaceType;

    private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;
  end;

  { Common base class for all nodes that modify the modelview matrix.
    In descendants you only have to override the MatrixTransform method. }
  TAbstractTransformationNode_1 = class(TAbstractChildNode)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); virtual; abstract;
    function TransformScale: Single; virtual; abstract;
  end;

  { VRML 1.0 MatrixTransform node. }
  TMatrixTransformNode_1 = class(TAbstractTransformationNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdMatrix: TSFMatrix;
    public property FdMatrix: TSFMatrix read FFdMatrix;

    { Returns simply FdMatrix as transformation. Inverse is also calculated
      (if matrix is invertible at all, otherwise identity is returned). }
    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;

    { Return average scale for this FdMatrix.

      Note that this doesn't correctly extract scale from FdMatrix,
      as that is too difficcult. Insted it does simple extraction,
      which will work for identity, translation and scaling matrices
      (but e.g. will fail miserably (generate nonsense results) when
      looking at some rotation matrices).

      Ultimately, this is the reason why VRML 2.0 removed this node
      from specification: extracting some features from arbitrary given
      4x4 matrix is very difficult. }
    function TransformScale: Single; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TRotationNode_1 = class(TAbstractTransformationNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;
  end;

  TScaleNode_1 = class(TAbstractTransformationNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdScaleFactor: TSFVec3f;
    public property FdScaleFactor: TSFVec3f read FFdScaleFactor;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;
  end;

  TTransformNode_1 = class(TAbstractTransformationNode_1)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScaleFactor: TSFVec3f;
    public property FdScaleFactor: TSFVec3f read FFdScaleFactor;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TTranslationNode_1 = class(TAbstractTransformationNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function TransformScale: Single; override;
  end;

  { Common base class for all cameras in VRML 1.0. }
  TAbstractCameraNode_1 = class(TAbstractViewpointNode)
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    private FFdFocalDistance: TSFFloat;
    public property FdFocalDistance: TSFFloat read FFdFocalDistance;

    { Ignored fields, some unknown extensions to VRML 1.0 spec.
      Some models ([http://www-vrl.umich.edu/sel_prj/EECS498/]) use them.
      @groupBegin }
    private FFdNearDistance: TSFFloat;
    public property FdNearDistance: TSFFloat read FFdNearDistance;

    private FFdFarDistance: TSFFloat;
    public property FdFarDistance: TSFFloat read FFdFarDistance;
    { @groupEnd }
  end;

  TOrthographicCameraNode_1 = class(TAbstractCameraNode_1)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function ProjectionType: TProjectionType; override;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;
  end;

  TPerspectiveCameraNode_1 = class(TAbstractCameraNode_1)
  public
    procedure CreateNode; override;

    class function ClassX3DType: string; override;
    class function ProjectionType: TProjectionType; override;

    private FFdHeightAngle: TSFFloat;
    public property FdHeightAngle: TSFFloat read FFdHeightAngle;
  end;

  TDirectionalLightNode_1 = class(TAbstractDirectionalLightNode)
  public
    procedure CreateNode; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function Scope: TLightScope; override;
  end;

  TPointLightNode_1 = class(TAbstractPointLightNode)
  public
    procedure CreateNode; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function HasRadius: boolean; override;
    function Scope: TLightScope; override;
  end;

  TSpotLightNode_1 = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3Single; override;
    procedure SetProjectionLocationLocal(const Value: TVector3Single); override;
    function  GetProjectionDirectionLocal: TVector3Single; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3Single); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdDropOffRate: TSFFloat;
    public property FdDropOffRate: TSFFloat read FFdDropOffRate;

    private FFdCutOffAngle: TSFFloat;
    public property FdCutOffAngle: TSFFloat read FFdCutOffAngle;

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

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    procedure UpdateLightInstance(var LightInstance: TLightInstance); override;

    function ProjectionMatrix: TMatrix4Single; override;
    function ModelviewMatrix: TMatrix4Single; override;
    function ModelviewRotationMatrix: TMatrix4Single; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
    function HasRadius: boolean; override;
    function Scope: TLightScope; override;
  end;

  TGroupNode_1 = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  { Base class for VRML 1.0 nodes that push / pop all attributes and matrices.
    It is used in implementation of VRML 1.0 Separator and WWWAnchor.
    Also WWWInline does the same work, when it's "separate" field is true. }
  TAbstractSeparatorNode_1 = class(TAbstractChildNode)
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
  end;

  TSeparatorNode_1 = class(TAbstractSeparatorNode_1)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdRenderCulling: TSFEnum;
    public property FdRenderCulling: TSFEnum read FFdRenderCulling;
  end;

  TSwitchNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdWhichChild: TSFLong;
    public property FdWhichChild: TSFLong read FFdWhichChild;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TTransformSeparatorNode_1 = class(TAbstractChildNode)
  private
    OriginalTransform, OriginalInvertedTransform: TMatrix4Single;
    OriginalTransformScale: Single;
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
  end;

  TWWWAnchorNode_1 = class(TAbstractSeparatorNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdMap: TSFEnum;
    public property FdMap: TSFEnum read FFdMap;
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
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdSeparate: TSFBool;
    public property FdSeparate: TSFBool read FFdSeparate;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TAbstractGeometryNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TAsciiTextNode_1.CreateNode;
begin
  inherited;

  FFdString := TMFString.Create(Self, 'string', ['']);
   FdString.ChangesAlways := [chGeometry];
  AddField(FFdString);

  FFdSpacing := TSFFloat.Create(Self, 'spacing', 1);
   FdSpacing.ChangesAlways := [chGeometry];
  AddField(FFdSpacing);

  {$warnings off} // using deprecated knowingly, it should be internal
  FFdJustification := TSFEnum.Create(Self, 'justification', ['LEFT', 'CENTER', 'RIGHT'], JUSTIFICATION_LEFT);
  {$warnings on}
   FdJustification.ChangesAlways := [chGeometry];
  AddField(FFdJustification);

  FFdWidth := TMFFloat.Create(Self, 'width', [0]);
   FdWidth.ChangesAlways := [chGeometry];
  AddField(FFdWidth);
end;

destructor TAsciiTextNode_1.Destroy;
begin
  { actually FreeAndNil would also be OK, as this is never
    a child of another node. But for safety... }
  FreeIfUnusedAndNil(FFontTextureNode);
  inherited;
end;

class function TAsciiTextNode_1.ClassX3DType: string;
begin
  result := 'AsciiText';
end;

function TAsciiTextNode_1.Justify: TX3DFontJustify;
begin
  case FdJustification.Value of
    {$warnings off} // using deprecated knowingly, it should be internal
    JUSTIFICATION_LEFT  : Result := fjBegin;
    JUSTIFICATION_CENTER: Result := fjMiddle;
    JUSTIFICATION_RIGHT : Result := fjEnd;
    {$warnings on}
    else
      { something default }
      Result := fjBegin;
  end;
end;

function TAsciiTextNode_1.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
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
    State.SetLastNodes(vsCoordinate3, CoordNode, true);

    TexCoordNode := TTextureCoordinate2Node_1.Create('', BaseUrl);
    TexCoordNode.FdPoint.Items.Clear;
    State.SetLastNodes(vsTextureCoordinate2, TexCoordNode, true);

    TextProxy(Self,
      State.LastNodes.FontStyle.FdSize.Value, FdSpacing.Value, Justify, fjFirst,
      FdString.Items, State.LastNodes.FontStyle.Font,
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
    FFontTextureNode := CreateFontTextureNode(
      State.LastNodes.FontStyle.Font,
      State.LastNodes.FontStyle.Blending);
end;

function TAsciiTextNode_1.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

function TAsciiTextNode_1.FontTextureNode: TAbstractTexture2DNode;
begin
  Result := FFontTextureNode;
end;

procedure TConeNode_1.CreateNode;
begin
  inherited;

  FFdParts := TSFBitMask.Create(Self, 'parts', ['SIDES', 'BOTTOM'], 'NONE', 'ALL', [true, true]);
   FdParts.ChangesAlways := [chGeometry];
  AddField(FFdParts);

  FFdBottomRadius := TSFFloat.Create(Self, 'bottomRadius', 1, true);
   FdBottomRadius.ChangesAlways := [chGeometry];
  AddField(FFdBottomRadius);

  FFdHeight := TSFFloat.Create(Self, 'height', 2, true);
   FdHeight.ChangesAlways := [chGeometry];
  AddField(FFdHeight);
end;

class function TConeNode_1.ClassX3DType: string;
begin
  result := 'Cone';
end;

function TConeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCubeNode_1.CreateNode;
begin
  inherited;

  FFdWidth := TSFFloat.Create(Self, 'width', 2, true);
   FdWidth.ChangesAlways := [chGeometry];
  AddField(FFdWidth);

  FFdHeight := TSFFloat.Create(Self, 'height', 2, true);
   FdHeight.ChangesAlways := [chGeometry];
  AddField(FFdHeight);

  FFdDepth := TSFFloat.Create(Self, 'depth', 2, true);
   FdDepth.ChangesAlways := [chGeometry];
  AddField(FFdDepth);
end;

class function TCubeNode_1.ClassX3DType: string;
begin
  result := 'Cube';
end;

function TCubeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCylinderNode_1.CreateNode;
begin
  inherited;

  FFdParts := TSFBitMask.Create(Self, 'parts', ['SIDES', 'TOP', 'BOTTOM'], 'NONE', 'ALL', [true, true, true]);
   FdParts.ChangesAlways := [chGeometry];
  AddField(FFdParts);

  FFdRadius := TSFFloat.Create(Self, 'radius', 1, true);
   FdRadius.ChangesAlways := [chGeometry];
  AddField(FFdRadius);

  FFdHeight := TSFFloat.Create(Self, 'height', 2, true);
   FdHeight.ChangesAlways := [chGeometry];
  AddField(FFdHeight);
end;

class function TCylinderNode_1.ClassX3DType: string;
begin
  result := 'Cylinder';
end;

function TCylinderNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TAbstractIndexedNode_1.CreateNode;
begin
  inherited;

  FFdCoordIndex := TMFLong.Create(Self, 'coordIndex', [0]);
   FdCoordIndex.SaveToStreamLineUptoNegative := true;
   FdCoordIndex.ChangesAlways := [chGeometry];
  AddField(FFdCoordIndex);

  FFdMaterialIndex := TMFLong.Create(Self, 'materialIndex', [-1]);
   FdMaterialIndex.ChangesAlways := [chGeometry];
  AddField(FFdMaterialIndex);

  FFdNormalIndex := TMFLong.Create(Self, 'normalIndex', [-1]);
   FdNormalIndex.ChangesAlways := [chGeometry];
  AddField(FFdNormalIndex);

  FFdTextureCoordIndex := TMFLong.Create(Self, 'textureCoordIndex', [-1]);
   FdTextureCoordIndex.SaveToStreamLineUptoNegative := true;
   FdTextureCoordIndex.ChangesAlways := [chGeometry];
  AddField(FFdTextureCoordIndex);
end;

function TAbstractIndexedNode_1.Coord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  ACoord := State.LastNodes.Coordinate3.FdPoint;
end;

function TAbstractIndexedNode_1.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TIndexedFaceSetNode_1.CreateNode;
begin
  inherited;

  FFdRadianceTransfer := TMFVec3f.Create(Self, 'radianceTransfer', []);
   FdRadianceTransfer.ChangesAlways := [chGeometry];
  AddField(FFdRadianceTransfer);
end;

class function TIndexedFaceSetNode_1.ClassX3DType: string;
begin
  result := 'IndexedFaceSet';
end;

function TIndexedFaceSetNode_1.TexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

class function TIndexedLineSetNode_1.ClassX3DType: string;
begin
  result := 'IndexedLineSet';
end;

function TIndexedLineSetNode_1.TexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

function TIndexedLineSetNode_1.Lit(State: TX3DGraphTraverseState): boolean;
begin
  { This somewhat follows the logic of
    TAbstractNormalGenerator.NorImplementationFromVRML1Binding,
    answering "false" when NorImplementationFromVRML1Binding sets "niNone". }

  { for _INDEXED normal binding, check normalIndex non-empty }
  case State.LastNodes.NormalBinding.FdValue.Value of
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
  if State.LastNodes.Normal.FdVector.Count = 0 then
    Result := false;
end;

procedure TPointSetNode_1.CreateNode;
begin
  inherited;

  FFdStartIndex := TSFLong.Create(Self, 'startIndex', 0);
   FdStartIndex.ChangesAlways := [chGeometry];
  AddField(FFdStartIndex);

  FFdNumPoints := TSFLong.Create(Self, 'numPoints', -1);
   FdNumPoints.ChangesAlways := [chGeometry];
  AddField(FFdNumPoints);

  CoordSubrange := TMFVec3f.Create(Self, '', []);
end;

destructor TPointSetNode_1.Destroy;
begin
  FreeAndNil(CoordSubrange);
  inherited;
end;

class function TPointSetNode_1.ClassX3DType: string;
begin
  result := 'PointSet';
end;

function TPointSetNode_1.Coord(State: TX3DGraphTraverseState;
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

  FullCoord := State.LastNodes.Coordinate3.FdPoint;
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
    CoordSubrange.Items.AddListRange(FullCoord.Items, StartIndex, NumPoints);
    ACoord := CoordSubrange;
  end;
end;

function TPointSetNode_1.Lit(State: TX3DGraphTraverseState): boolean;
begin
  Result := false;
end;

procedure TSphereNode_1.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, 'radius', 1, true);
   FdRadius.ChangesAlways := [chGeometry];
  AddField(FFdRadius);
end;

class function TSphereNode_1.ClassX3DType: string;
begin
  result := 'Sphere';
end;

function TSphereNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCoordinate3Node_1.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3f.Create(Self, 'point', [Vector3Single(0, 0, 0)]);
   FdPoint.ChangesAlways := [chCoordinate];
  AddField(FFdPoint);
end;

class function TCoordinate3Node_1.ClassX3DType: string;
begin
  result := 'Coordinate3';
end;

procedure TFontStyleNode_1.CreateNode;
begin
  inherited;

  FFdSize := TSFFloat.Create(Self, 'size', 10, true);
   FdSize.ChangesAlways := [chGeometryVRML1State];
  AddField(FFdSize);

  {$warnings off}
  FFdFamily := TSFEnum.Create(Self, 'family', ['SERIF', 'SANS', 'TYPEWRITER'], FSFAMILY_SERIF);
  {$warnings on}
   FdFamily.ChangesAlways := [chGeometryVRML1State];
  AddField(FFdFamily);

  FFdStyle := TSFBitMask.Create(Self, 'style', ['BOLD', 'ITALIC'], 'NONE', '', [false, false]);
   FdStyle.ChangesAlways := [chGeometryVRML1State];
  AddField(FFdStyle);

  FFdBlending := TSFBool.Create(Self, 'blending', true);
   FdBlending.ChangesAlways := [chGeometryVRML1State];
  AddField(FFdBlending);
end;

class function TFontStyleNode_1.ClassX3DType: string;
begin
  result := 'FontStyle';
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

class function TFontStyleNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

function TFontStyleNode_1.Font: TTextureFontData;
var
  TemporaryFontStyle: TFontStyleNode;
begin
  TemporaryFontStyle := TFontStyleNode.Create('', BaseUrl);
  try
    TemporaryFontStyle.Family := Family;
    TemporaryFontStyle.Bold := Bold;
    TemporaryFontStyle.Italic := Italic;
    { get font for a default FontStyle node }
    Result := TemporaryFontStyle.Font;
  finally FreeAndNil(TemporaryFontStyle) end;
end;

function TFontStyleNode_1.GetBlending: boolean;
begin
  Result := FdBlending.Value;
end;

procedure TFontStyleNode_1.SetBlending(const Value: boolean);
begin
  FdBlending.Send(Value);
end;

procedure TInfoNode_1.CreateNode;
begin
  inherited;

  FFdString := TSFString.Create(Self, 'string', '<Undefined info>');
  AddField(FFdString);
end;

class function TInfoNode_1.ClassX3DType: string;
begin
  result := 'Info';
end;

procedure TLODNode_1.CreateNode;
begin
  inherited;

  FFdRange := TMFFloat.Create(Self, 'range',[]);
  AddField(FFdRange);

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  AddField(FFdCenter);

  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TLODNode_1.ClassX3DType: string;
begin
  result := 'LOD';
end;

function TLODNode_1.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  { TODO: we should use here the LOD matching camera distance.
    For now, just like TAbstractLODNode.DirectEnumerateActive, we ignore the issue. }
  if VRML1ChildrenCount = 0 then
    raise EX3DError.Create('LOD node must have at least one child');

  Result := Func(Self, VRML1Children[0]);
  if Result <> nil then Exit;
end;

class function TLODNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TMaterialNode_1.CreateNode;
begin
  inherited;

  FFdAmbientColor := TMFColor.Create(Self, 'ambientColor', [DefaultMaterial_1AmbientColor]);
   FdAmbientColor.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdAmbientColor);

  FFdDiffuseColor := TMFColor.Create(Self, 'diffuseColor', [DefaultMaterialDiffuseColor]);
   FdDiffuseColor.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdDiffuseColor);

  FFdSpecularColor := TMFColor.Create(Self, 'specularColor', [DefaultMaterialSpecularColor]);
   FdSpecularColor.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdSpecularColor);

  FFdEmissiveColor := TMFColor.Create(Self, 'emissiveColor', [DefaultMaterialEmissiveColor]);
   FdEmissiveColor.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdEmissiveColor);

  FFdShininess := TMFFloat.Create(Self, 'shininess', [DefaultMaterialShininess]);
   FdShininess.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdShininess);

  FFdTransparency := TMFFloat.Create(Self, 'transparency', [DefaultMaterialTransparency]);
   FdTransparency.ChangesAlways := [chVisibleVRML1State, chUseBlending];
  AddField(FFdTransparency);

  FFdMirror := TMFFloat.Create(Self, 'mirror', [DefaultMaterialMirror]);
   FdMirror.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdMirror);

  FFdReflSpecular := TMFColor.Create(Self, 'reflSpecular', []);
   FdReflSpecular.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdReflSpecular);

  FFdReflDiffuse := TMFColor.Create(Self, 'reflDiffuse', []);
   FdReflDiffuse.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdReflDiffuse);

  FFdTransSpecular := TMFColor.Create(Self, 'transSpecular', []);
   FdTransSpecular.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdTransSpecular);

  FFdTransDiffuse := TMFColor.Create(Self, 'transDiffuse', []);
   FdTransDiffuse.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdTransDiffuse);

  FFdReflSpecularExp := TMFFloat.Create(Self, 'reflSpecularExp', [DefaultMaterialReflSpecularExp]);
   FdReflSpecularExp.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdReflSpecularExp);

  FFdTransSpecularExp := TMFFloat.Create(Self, 'transSpecularExp', [DefaultMaterialTransSpecularExp]);
   FdTransSpecularExp.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdTransSpecularExp);

  FFdFogImmune := TSFBool.Create(Self, 'fogImmune', false);
   FdFogImmune.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdFogImmune);
end;

class function TMaterialNode_1.ClassX3DType: string;
begin
  result := 'Material';
end;

{ Functions below return MatNum Material property. If there doesn't exist
  enouch properties defined, thay return the last defined. This is useful :
  for example you can give one ambient, specular, emissive color and
  define multiple diffuseColors and then you can use multiple materials
  without defining multiple values for each field.

  VRML specification doesn't state clearly what to do when thare are not enouch
  material properties - there was an idea of "cycling" mechanism but was
  later deprecated. So I use the mechanism below - returning to the last defined
  property number. }

{$define MATERIAL_FUNCTION_3_SINGLE:=
function TMaterialNode_1.MATERIAL_FUNCTION_NAME_3(MatNum: integer): TVector3Single;
begin
  if MATERIAL_FUNCTION_FIELD.Count = 0 then
    result := MATERIAL_FUNCTION_DEFAULT else
    result := MATERIAL_FUNCTION_FIELD.Items.L[
      min(MatNum, MATERIAL_FUNCTION_FIELD.Count - 1)];
end;

function TMaterialNode_1.MATERIAL_FUNCTION_NAME_4(MatNum: integer): TVector4Single;
var result3: TVector3Single absolute result;
begin
  result3 := MATERIAL_FUNCTION_NAME_3(MatNum);
  result[3] := Opacity(MatNum);
end;
}

  {$define MATERIAL_FUNCTION_FIELD := FdAmbientColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterial_1AmbientColor}
  {$define MATERIAL_FUNCTION_NAME_3 := AmbientColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := AmbientColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdDiffuseColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialDiffuseColor}
  {$define MATERIAL_FUNCTION_NAME_3 := DiffuseColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := DiffuseColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdSpecularColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialSpecularColor}
  {$define MATERIAL_FUNCTION_NAME_3 := SpecularColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := SpecularColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

  {$define MATERIAL_FUNCTION_FIELD := FdEmissiveColor}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialEmissiveColor}
  {$define MATERIAL_FUNCTION_NAME_3 := EmissiveColor3Single}
  {$define MATERIAL_FUNCTION_NAME_4 := EmissiveColor4Single}
  MATERIAL_FUNCTION_3_SINGLE

{$undef MATERIAL_FUNCTION_3_SINGLE}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_DEFAULT}
{$undef MATERIAL_FUNCTION_NAME_3}
{$undef MATERIAL_FUNCTION_NAME_4}

{$define MATERIAL_FUNCTION_SINGLE:=
function TMaterialNode_1.MATERIAL_FUNCTION_NAME(MatNum: integer): Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_DEFAULT else
  result := MATERIAL_FUNCTION_FIELD.Items.L[
    min(MatNum, MATERIAL_FUNCTION_FIELD.Count-1)];
end;}

  {$define MATERIAL_FUNCTION_NAME := Transparency}
  {$define MATERIAL_FUNCTION_FIELD := FdTransparency}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialTransparency}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := Mirror}
  {$define MATERIAL_FUNCTION_FIELD := FdMirror}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialMirror}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := ReflSpecularExp}
  {$define MATERIAL_FUNCTION_FIELD := FdReflSpecularExp}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialReflSpecularExp}
  MATERIAL_FUNCTION_SINGLE

  {$define MATERIAL_FUNCTION_NAME := TransSpecularExp}
  {$define MATERIAL_FUNCTION_FIELD := FdTransSpecularExp}
  {$define MATERIAL_FUNCTION_DEFAULT := DefaultMaterialTransSpecularExp}
  MATERIAL_FUNCTION_SINGLE

{$undef MATERIAL_FUNCTION_NAME}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_DEFAULT}
{$undef MATERIAL_FUNCTION_SINGLE}

function TMaterialNode_1.Opacity(MatNum: integer): Single;
begin
  result := 1 - Transparency(MatNum);
end;

function TMaterialNode_1.Shininess(MatNum: integer): Single;
begin
  if FdShininess.Count = 0 then
    result := DefaultMaterialShininess else
    result := FdShininess.Items.L[min(MatNum, FdShininess.Count-1)];
end;

function TMaterialNode_1.ShininessExp(MatNum: integer): Single;
begin
  Result := Shininess(MatNum);

  { According to VRML specification, shininess must be within 0..1 range,
    and it maps uniformly to 0..128 range for OpenGL's exponent.
    That's fine. We do clamp to 0..128 if for whatever small floating
    point errors we'll get outside the range allowed by OpenGL,
    and to secure against incorrect negative shininess values in VRML.

    To encompass various incorrect VRML files we assume that
    shininess > 2 means that someone didn't grok the VRML spec,
    and stored actual exponent in VRML file.
    This unfortunately happens, see e.g. helix.wrl test. }

  if result > 2 then
    result := Clamped(result,         0.0, 128.0) else
    result := Clamped(result * 128.0, 0.0, 128.0);
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

class function TMaterialNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

function TMaterialNode_1.MaterialInfo(const Index: Integer): TX3DMaterialInfo_1;
begin
  Result := TX3DMaterialInfo_1.Create(Self, Index);
end;

constructor TX3DMaterialInfo_1.Create(Node: TMaterialNode_1; const Index: Integer);
begin
  inherited Create;
  FNode := Node;
  FIndex := Index;
end;

function TX3DMaterialInfo_1.DiffuseColor: TVector3Single;
begin
  Result := FNode.DiffuseColor3Single(FIndex);
end;

function TX3DMaterialInfo_1.Mirror: Single;
begin
  Result := FNode.Mirror(FIndex);
end;

function TX3DMaterialInfo_1.Transparency: Single;
begin
  Result := FNode.Transparency(FIndex);
end;

function TX3DMaterialInfo_1.ReflSpecular: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    CalculateReflSpecular(Result) else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TX3DMaterialInfo_1.ReflDiffuse: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    CalculateReflDiffuse(Result) else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TX3DMaterialInfo_1.TransSpecular: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    CalculateTransSpecular(Result) else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TX3DMaterialInfo_1.TransDiffuse: TVector3Single;
var
  A: TVector3SingleList;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    CalculateTransDiffuse(Result) else
    Result := A.L[Min(FIndex, A.Count - 1)];
end;

function TX3DMaterialInfo_1.ReflSpecularExp: Single;
begin
  Result := FNode.ReflSpecularExp(FIndex);
end;

function TX3DMaterialInfo_1.TransSpecularExp: Single;
begin
  Result := FNode.TransSpecularExp(FIndex);
end;

procedure TMaterialBindingNode_1.CreateNode;
begin
  inherited;

  FFdValue := TSFEnum.Create(Self, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 1);
   FdValue.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdValue);
end;

class function TMaterialBindingNode_1.ClassX3DType: string;
begin
  result := 'MaterialBinding';
end;

procedure TNormalBindingNode_1.CreateNode;
begin
  inherited;

  FFdValue := TSFEnum.Create(Self, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 0);
   FdValue.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdValue);
end;

class function TNormalBindingNode_1.ClassX3DType: string;
begin
  result := 'NormalBinding';
end;

procedure TTexture2Node_1.CreateNode;
begin
  inherited;

  FFdFilename := TSFString.Create(Self, 'filename', '');
   FdFilename.ChangesAlways := [chVisibleVRML1State, chTextureImage];
  AddField(FFdFilename);

  FFdImage := TSFImage.Create(Self, 'image', nil);
   FdImage.ChangesAlways := [chVisibleVRML1State, chTextureImage];
  AddField(FFdImage);

  {$warnings off} // using deprecated knowingly, it should be internal
  FFdWrapS := TSFEnum.Create(Self, 'wrapS', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
  {$warnings on}
   FdWrapS.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdWrapS);

  {$warnings off} // using deprecated knowingly, it should be internal
  FFdWrapT := TSFEnum.Create(Self, 'wrapT', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
  {$warnings on}
   FdWrapT.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdWrapT);

  FFdModel := TSFEnum.Create(Self, 'model', ['DECAL'], 0);
   FdModel.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdModel);

  FFdBlendColor := TSFVec3f.Create(Self, 'blendColor', Vector3Single(0, 0, 0));
   FdBlendColor.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdBlendColor);
end;

class function TTexture2Node_1.ClassX3DType: string;
begin
  result := 'Texture2';
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
    FTextureImage := X3DCache.TextureImage_IncReference(FullUrl, FTextureComposite,
      FAlphaChannelData);
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
    result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width,
        FdImage.Value.Height,
        BoolToStr(FdImage.Value.HasAlpha, true) ]);
  end;

begin
  if FdFilename.Value <> '' then
  begin
    result := 'file "' +PathFromBaseUrl(FdFilename.Value) +'"';
    if not FdImage.Value.IsEmpty then result += ' (and '+InlinedDescr+')';
  end else
  if not FdImage.Value.IsEmpty then
    result := InlinedDescr else
    result := 'none';
end;

{$warnings off} // using deprecated TEXWRAP_xxx knowingly, it should be internal

function TTexture2Node_1.GetRepeatS: boolean;
begin
  Result := FdWrapS.Value = TEXWRAP_REPEAT;
end;

function TTexture2Node_1.GetRepeatT: boolean;
begin
  Result := FdWrapT.Value = TEXWRAP_REPEAT;
end;

procedure TTexture2Node_1.SetRepeatS(const Value: boolean);
begin
  if Value then
    FdWrapS.Send(TEXWRAP_REPEAT)
  else
    FdWrapS.Send(TEXWRAP_CLAMP);
end;

procedure TTexture2Node_1.SetRepeatT(const Value: boolean);
begin
  if Value then
    FdWrapT.Send(TEXWRAP_REPEAT)
  else
    FdWrapT.Send(TEXWRAP_CLAMP);
end;

{$warnings on}

procedure TTexture2TransformNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, 'translation', Vector2Single(0, 0));
   FdTranslation.ChangesAlways := [chEverything];
  AddField(FFdTranslation);

  FFdRotation := TSFFloat.Create(Self, 'rotation', 0);
   FdRotation.ChangesAlways := [chEverything];
  AddField(FFdRotation);

  FFdScaleFactor := TSFVec2f.Create(Self, 'scaleFactor', Vector2Single(1, 1));
   FdScaleFactor.ChangesAlways := [chEverything];
  AddField(FFdScaleFactor);

  FFdCenter := TSFVec2f.Create(Self, 'center', Vector2Single(0, 0));
   FdCenter.ChangesAlways := [chEverything];
  AddField(FFdCenter);
end;

class function TTexture2TransformNode_1.ClassX3DType: string;
begin
  result := 'Texture2Transform';
end;

function TTexture2TransformNode_1.TextureMatrixTransformation: TMatrix4Single;
begin
  result := TranslationMatrix( Vector3Single(
    VectorAdd(FdTranslation.Value, FdCenter.Value) ));
  result := MatrixMult(result, RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  result := MatrixMult(result, ScalingMatrix(
    Vector3Single( FdScaleFactor.Value[0], FdScaleFactor.Value[1], 1 )));
  result := MatrixMult(result, TranslationMatrix(
    Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

procedure TTexture2TransformNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.TextureTransform := MatrixMult(StateStack.Top.TextureTransform,
    TextureMatrixTransformation);
end;

procedure TTextureCoordinate2Node_1.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, 'point', [Vector2Single(0, 0)]);
   FdPoint.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdPoint);
end;

class function TTextureCoordinate2Node_1.ClassX3DType: string;
begin
  result := 'TextureCoordinate2';
end;

procedure TShapeHintsNode_1.CreateNode;
begin
  inherited;

  FFdVertexOrdering := TSFEnum.Create(Self, 'vertexOrdering', ['UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE'], VERTORDER_UNKNOWN);
   FdVertexOrdering.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdVertexOrdering);

  FFdShapeType := TSFEnum.Create(Self, 'shapeType', ['UNKNOWN_SHAPE_TYPE', 'SOLID'], SHTYPE_UNKNOWN);
   FdShapeType.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdShapeType);

  FFdFaceType := TSFEnum.Create(Self, 'faceType', ['UNKNOWN_FACE_TYPE', 'CONVEX'], FACETYPE_CONVEX);
   FdFaceType.ChangesAlways := [chGeometryVRML1State];
  AddField(FFdFaceType);

  FFdCreaseAngle := TSFFloat.Create(Self, 'creaseAngle', DefaultVRML1CreaseAngle);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangesAlways := [chVisibleVRML1State];
  AddField(FFdCreaseAngle);
end;

class function TShapeHintsNode_1.ClassX3DType: string;
begin
  result := 'ShapeHints';
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
      Hints := TSFBitMask.Create(Self, 'hints',
        ['SOLID', 'ORDERED', 'CONVEX'], 'NONE', '',
        [ false,   true,      true]);
      try
        Lexer.NextToken;
        Hints.Parse(Lexer, Reader, false);
        if Hints.Flags[0] then
          FdShapeType.Value := SHTYPE_SOLID else
          FdShapeType.Value := SHTYPE_UNKNOWN;
        if Hints.Flags[1] then
          FdVertexOrdering.Value := VERTORDER_COUNTERCLOCKWISE else
          FdVertexOrdering.Value := VERTORDER_UNKNOWN;
        if Hints.Flags[2] then
          FdFaceType.Value := FACETYPE_CONVEX else
          FdFaceType.Value := FACETYPE_UNKNOWN;
      finally Hints.Free end;
    end;
  end;
end;

procedure TAbstractTransformationNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
var
  M, IM: TMatrix4Single;
begin
  inherited;

  Transformation(M, IM);
  StateStack.Top.Transform := MatrixMult(StateStack.Top.Transform, M);
  StateStack.Top.InvertedTransform := MatrixMult(IM, StateStack.Top.InvertedTransform);

  StateStack.Top.TransformScale *= TransformScale;
end;

procedure TMatrixTransformNode_1.CreateNode;
begin
  inherited;

  FFdMatrix := TSFMatrix.Create(Self, 'matrix', IdentityMatrix4Single);
   FdMatrix.ChangesAlways := [chEverything];
  AddField(FFdMatrix);
end;

class function TMatrixTransformNode_1.ClassX3DType: string;
begin
 result := 'MatrixTransform';
end;

procedure TMatrixTransformNode_1.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  Matrix := FdMatrix.Value;

  if not TryMatrixInverse(Matrix, InvertedMatrix) then
  begin
    if Log then
      WritelnLogMultiline('Matrix',
        'Cannot invert matrix:' + NL + MatrixToRawStr(Matrix, '  '));
    InvertedMatrix := IdentityMatrix4Single;
  end;
end;

function TMatrixTransformNode_1.TransformScale: Single;
begin
  Result := FdMatrix.TransformScale;
end;

class function TMatrixTransformNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TRotationNode_1.CreateNode;
begin
  inherited;

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chEverything];
  AddField(FFdRotation);
end;

class function TRotationNode_1.ClassX3DType: string;
begin
  result := 'Rotation';
end;

procedure TRotationNode_1.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  { We don't check here for FdRotation.Axis = zero, RotationMatricesRad
    will set both matrices for identity in such case. }
  RotationMatricesRad(FdRotation.RotationRad, FdRotation.Axis,
    Matrix, InvertedMatrix);
end;

function TRotationNode_1.TransformScale: Single;
begin
  Result := 1;
end;

procedure TScaleNode_1.CreateNode;
begin
  inherited;

  FFdScaleFactor := TSFVec3f.Create(Self, 'scaleFactor', Vector3Single(1, 1, 1));
   FdScaleFactor.ChangesAlways := [chEverything];
  AddField(FFdScaleFactor);
end;

class function TScaleNode_1.ClassX3DType: string;
begin
 result := 'Scale';
end;

procedure TScaleNode_1.Transformation(out Matrix, InvertedMatrix: TMatrix4Single);
begin
  ScalingMatrices(FdScaleFactor.Value, true, Matrix, InvertedMatrix);
end;

function TScaleNode_1.TransformScale: Single;
begin
  Result := Approximate3DScale(FdScaleFactor.Value);
end;

procedure TTransformNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chEverything];
  AddField(FFdTranslation);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
   FdRotation.ChangesAlways := [chEverything];
  AddField(FFdRotation);

  FFdScaleFactor := TSFVec3f.Create(Self, 'scaleFactor', Vector3Single(1, 1, 1));
   FdScaleFactor.ChangesAlways := [chEverything];
  AddField(FFdScaleFactor);

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
   FdScaleOrientation.ChangesAlways := [chEverything];
  AddField(FFdScaleOrientation);

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
   FdCenter.ChangesAlways := [chEverything];
  AddField(FFdCenter);
end;

class function TTransformNode_1.ClassX3DType: string;
begin
  result := 'Transform';
end;

procedure TTransformNode_1.Transformation(out Matrix, InvertedMatrix: TMatrix4Single);
var
  M, IM, MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  TranslationMatrices(VectorAdd(FdTranslation.Value, FdCenter.Value),
    Matrix, InvertedMatrix);

  { Note that even Axis = zero is OK, both M and IM will be identity in
    this case. }
  RotationMatricesRad(FdRotation.RotationRad, FdRotation.Axis, M, IM);
  Matrix := MatrixMult(Matrix, M);
  InvertedMatrix := MatrixMult(IM, InvertedMatrix);

  if (FdScaleFactor.Value[0] <> 1) or
     (FdScaleFactor.Value[1] <> 1) or
     (FdScaleFactor.Value[2] <> 1) then
  begin
    RotationMatricesRad(FdScaleOrientation.RotationRad, FdScaleOrientation.Axis,
      MRotateScaleOrient, IMRotateScaleOrient);

    Matrix := MatrixMult(Matrix, MRotateScaleOrient);
    InvertedMatrix := MatrixMult(IMRotateScaleOrient, InvertedMatrix);

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity. That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(FdScaleFactor.Value, true, M, IM);
    Matrix := MatrixMult(Matrix, M);
    InvertedMatrix := MatrixMult(IM, InvertedMatrix);

    Matrix := MatrixMult(Matrix, IMRotateScaleOrient);
    InvertedMatrix := MatrixMult(MRotateScaleOrient, InvertedMatrix);
  end;

  TranslationMatrices(VectorNegate(FdCenter.Value), M, IM);
  Matrix := MatrixMult(Matrix, M);
  InvertedMatrix := MatrixMult(IM, InvertedMatrix);
end;

function TTransformNode_1.TransformScale: Single;
begin
  Result := Approximate3DScale(FdScaleFactor.Value);
end;

class function TTransformNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TTranslationNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
   FdTranslation.ChangesAlways := [chEverything];
  AddField(FFdTranslation);
end;

class function TTranslationNode_1.ClassX3DType: string;
begin
  result := 'Translation';
end;

procedure TTranslationNode_1.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  TranslationMatrices(FdTranslation.Value, Matrix, InvertedMatrix);
end;

function TTranslationNode_1.TransformScale: Single;
begin
  Result := 1;
end;

procedure TAbstractCameraNode_1.CreateNode;
begin
  inherited;

  { Note that the default "position" value for VRML 1.0 is different
    than for VRML >= 2.0 (where is (0, 0, 10)). }
  FFdPosition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 1));
   FdPosition.ChangesAlways := [chViewpointVectors];
  AddField(FFdPosition);

  FFdFocalDistance := TSFFloat.Create(Self, 'focalDistance', 5, true);
  AddField(FFdFocalDistance);

  FFdNearDistance := TSFFloat.Create(Self, 'nearDistance', 0);
   FdNearDistance.ChangesAlways := [chViewpointProjection];
  AddField(FFdNearDistance);

  FFdFarDistance := TSFFloat.Create(Self, 'farDistance', 0);
   FdFarDistance.ChangesAlways := [chViewpointProjection];
  AddField(FFdFarDistance);
end;

function TAbstractCameraNode_1.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

procedure TOrthographicCameraNode_1.CreateNode;
begin
  inherited;

  FFdHeight := TSFFloat.Create(Self, 'height', 2, true);
  AddField(FFdHeight);
end;

class function TOrthographicCameraNode_1.ClassX3DType: string;
begin
  result := 'OrthographicCamera';
end;

class function TOrthographicCameraNode_1.ProjectionType: TProjectionType;
begin
  result := ptOrthographic;
end;

procedure TPerspectiveCameraNode_1.CreateNode;
begin
  inherited;

  FFdHeightAngle := TSFFloat.Create(Self, 'heightAngle', Pi / 4, true);
  AddField(FFdHeightAngle);
end;

class function TPerspectiveCameraNode_1.ClassX3DType: string;
begin
  result := 'PerspectiveCamera';
end;

class function TPerspectiveCameraNode_1.ProjectionType: TProjectionType;
begin
  result := ptPerspective;
end;

procedure TDirectionalLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;
end;

class function TDirectionalLightNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

function TDirectionalLightNode_1.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal else
    Result := lsLocalVRML1;
end;

procedure TPointLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3Single(0, 0, 1);
  FdLocation.DefaultValue := Vector3Single(0, 0, 1);
end;

class function TPointLightNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
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

procedure TSpotLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://castle-engine.sourceforge.net/x3d_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3Single(0, 0, 1);
  FdLocation.DefaultValue := Vector3Single(0, 0, 1);

  FFdDirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
   FdDirection.ChangesAlways := [chLightInstanceProperty, chLightLocationDirection];
  AddField(FFdDirection);

  FFdDropOffRate := TSFFloat.Create(Self, 'dropOffRate', 0);
   FdDropOffRate.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdDropOffRate);

  FFdCutOffAngle := TSFFloat.Create(Self, 'cutOffAngle', Pi / 4);
   FdCutOffAngle.ChangesAlways := [chVisibleNonGeometry];
  AddField(FFdCutOffAngle);
end;

class function TSpotLightNode_1.ClassX3DType: string;
begin
  result := 'SpotLight';
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

class function TSpotLightNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TSpotLightNode_1.UpdateLightInstance(var LightInstance: TLightInstance);
begin
  inherited;
  LightInstance.Direction := Normalized(MatrixMultDirection(
    LightInstance.Transform, FdDirection.Value));
end;

function TSpotLightNode_1.ProjectionMatrix: TMatrix4Single;
var
  Angle, N, F: Single;
begin
  { If author didn't provide and X3DShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  { Implementation just like for TSpotLightNode,
    except I was too lazy to add here projectionAngle. }
  Angle := 2 * FdCutOffAngle.Value;

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  Result := PerspectiveProjMatrixRad(Angle, 1, N, F);
end;

function TSpotLightNode_1.ModelviewMatrix: TMatrix4Single;
var
  Pos, Dir, Side, AUp: TVector3Single;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := LookDirMatrix(Pos, Dir, Side, AUp);
end;

function TSpotLightNode_1.ModelviewRotationMatrix: TMatrix4Single;
var
  Pos, Dir, Side, AUp: TVector3Single;
begin
  GetView(Pos, Dir, Side, AUp);
  Result := FastLookDirMatrix(Dir, AUp);
end;

function TSpotLightNode_1.GetProjectionLocationLocal: TVector3Single;
begin
  Result := FdLocation.Value;
end;

procedure TSpotLightNode_1.SetProjectionLocationLocal(const Value: TVector3Single);
begin
  FdLocation.Send(Value);
end;

function TSpotLightNode_1.GetProjectionDirectionLocal: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TSpotLightNode_1.SetProjectionDirectionLocal(const Value: TVector3Single);
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
  Box.DirectionDistances(ProjectionSceneLocation, ProjectionSceneDirection, MinDistance, MaxDistance);
end;

function TSpotLightNode_1.Scope: TLightScope;
begin
  if FdGlobal.Value then
    Result := lsGlobal else
    Result := lsLocalVRML1;
end;

procedure TGroupNode_1.CreateNode;
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TGroupNode_1.ClassX3DType: string;
begin
  result := 'Group';
end;

class function TGroupNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TAbstractSeparatorNode_1.CreateNode;
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
    DirectEnumerateActive(@Enumerator.Enumerate);
  finally FreeAndNil(Enumerator) end;
end;

procedure TAbstractSeparatorNode_1.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  StateStack.Pop;
  inherited;
end;

procedure TSeparatorNode_1.CreateNode;
begin
  inherited;

  FFdRenderCulling := TSFEnum.Create(Self, 'renderCulling', ['ON', 'OFF', 'AUTO'], 2);
  AddField(FFdRenderCulling);
end;

class function TSeparatorNode_1.ClassX3DType: string;
begin
  result := 'Separator';
end;

procedure TSwitchNode_1.CreateNode;
begin
  inherited;

  FFdWhichChild := TSFLong.Create(Self, 'whichChild', -1);
   FdWhichChild.ChangesAlways := [chEverything];
  AddField(FFdWhichChild);

  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TSwitchNode_1.ClassX3DType: string;
begin
  result := 'Switch';
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

class function TSwitchNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TTransformSeparatorNode_1.CreateNode;
begin
  inherited;
  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TTransformSeparatorNode_1.ClassX3DType: string;
begin
  result := 'TransformSeparator';
end;

procedure TTransformSeparatorNode_1.BeforeTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;

  { We don't copy whole State here, as changes to other properties of
    state should "leak out" from TransformSeparator node. }
  OriginalTransform := StateStack.Top.Transform;
  OriginalInvertedTransform := StateStack.Top.InvertedTransform;
  OriginalTransformScale := StateStack.Top.TransformScale;
end;

procedure TTransformSeparatorNode_1.AfterTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  StateStack.Top.Transform := OriginalTransform;
  StateStack.Top.InvertedTransform := OriginalInvertedTransform;
  StateStack.Top.TransformScale := OriginalTransformScale;
  inherited;
end;

procedure TWWWAnchorNode_1.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  AddField(FFdName);

  FFdDescription := TSFString.Create(Self, 'description', '');
  AddField(FFdDescription);

  FFdMap := TSFEnum.Create(Self, 'map', ['NONE', 'POINT'], 0);
  AddField(FFdMap);
end;

class function TWWWAnchorNode_1.ClassX3DType: string;
begin
  result := 'WWWAnchor';
end;

procedure TWWWInlineNode_1.CreateNode;
begin
  inherited;

  { change "url" field to "name", with default value being an empty string }
  FdUrl.AddAlternativeName('name', 1);
  FdUrl.Items.Add('');
  FdUrl.AssignDefaultValueFromValue;

  { bboxSize is (0, 0, 0) in VRMl 1.0 (in VRML 2.0/X3D it's (-1, -1, -1)) }
  FFdBboxSize.Value := Vector3Single(0, 0, 0);
  FFdBboxSize.AssignDefaultValueFromValue;

  FFdSeparate := TSFBool.Create(Self, 'separate', true);
   FdSeparate.ChangesAlways := [chEverything];
  AddField(FFdSeparate);
end;

class function TWWWInlineNode_1.ClassX3DType: string;
begin
  result := 'WWWInline';
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
