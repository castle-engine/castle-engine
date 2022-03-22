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

    strict private FFdString: TMFString;
    public property FdString: TMFString read FFdString;

    strict private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    { Text justification.
      Use constants like JUSTIFICATION_LEFT and so on with this. }
    strict private FFdJustification: TSFEnum;
    public property FdJustification: TSFEnum read FFdJustification;

    strict private FFdWidth: TMFFloat;
    public property FdWidth: TMFFloat read FFdWidth;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function FontTextureNode: TAbstractTexture2DNode; override;

    { Force recalculating the shape when font changed.
      For now, we don't detect font changes (when TFontStyleNode.OnFont
      returns something different) outselves. }
    procedure FontChanged;

    function Justify: TX3DFontJustify;
  end;

  TConeNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    strict private FFdBottomRadius: TSFFloat;
    public property FdBottomRadius: TSFFloat read FFdBottomRadius;

    strict private FFdHeight: TSFFloat;
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

    strict private FFdWidth: TSFFloat;
    public property FdWidth: TSFFloat read FFdWidth;

    strict private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    strict private FFdDepth: TSFFloat;
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

    strict private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdHeight: TSFFloat;
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

    strict private FFdCoordIndex: TMFLong;
    public property FdCoordIndex: TMFLong read FFdCoordIndex;

    strict private FFdMaterialIndex: TMFLong;
    public property FdMaterialIndex: TMFLong read FFdMaterialIndex;

    strict private FFdNormalIndex: TMFLong;
    public property FdNormalIndex: TMFLong read FFdNormalIndex;

    strict private FFdTextureCoordIndex: TMFLong;
    public property FdTextureCoordIndex: TMFLong read FFdTextureCoordIndex;

    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndexField: TMFLong; override;
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

    procedure InternalCoordPolygons(
      State: TX3DGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    strict private FFdRadianceTransfer: TMFVec3f;
    public property FdRadianceTransfer: TMFVec3f read FFdRadianceTransfer;

    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;
  end;

  TIndexedLineSetNode_1 = class(TAbstractIndexedNode_1)
    class function ClassX3DType: string; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    function InternalTexCoord(State: TX3DGraphTraverseState;
      out ATexCoord: TX3DNode): boolean; override;

    { Do we have enough normals information to render this node lit.
      VRML 1.0 specification explicitly mentions that IndexedLineSet is treated
      specially: it's unlit if there are not enough normals specified. }
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  end;

  TPointSetNode_1 = class(TAbstractGeometryNode_1)
  strict private
    CoordSubrange: TMFVec3f;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    strict private FFdStartIndex: TSFLong;
    public property FdStartIndex: TSFLong read FFdStartIndex;

    strict private FFdNumPoints: TSFLong;
    public property FdNumPoints: TSFLong read FFdNumPoints;

    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function Lit(State: TX3DGraphTraverseState): boolean; override;
  end;

  TSphereNode_1 = class(TAbstractGeometryNode_1)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRadius: TSFFloat;
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

    strict private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;
  end;

  TFontStyleNode_1 = class(TAbstractChildNode)
  strict private
    function GetBlending: boolean;
    procedure SetBlending(const Value: boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdSize: TSFFloat;
    public property FdSize: TSFFloat read FFdSize;

    strict private FFdFamily: TSFEnum;
    public property FdFamily: TSFEnum read FFdFamily;

    strict private FFdStyle: TSFBitMask;
    public property FdStyle: TSFBitMask read FFdStyle;

    strict private FFdBlending: TSFBool;
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

    strict private FFdString: TSFString;
    public property FdString: TSFString read FFdString;
  end;

  TLODNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRange: TMFFloat;
    public property FdRange: TMFFloat read FFdRange;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
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
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    strict private FFdAmbientColor: TMFColor;
    public property FdAmbientColor: TMFColor read FFdAmbientColor;

    strict private FFdDiffuseColor: TMFColor;
    public property FdDiffuseColor: TMFColor read FFdDiffuseColor;

    strict private FFdSpecularColor: TMFColor;
    public property FdSpecularColor: TMFColor read FFdSpecularColor;

    strict private FFdEmissiveColor: TMFColor;
    public property FdEmissiveColor: TMFColor read FFdEmissiveColor;

    strict private FFdShininess: TMFFloat;
    public property FdShininess: TMFFloat read FFdShininess;

    strict private FFdTransparency: TMFFloat;
    public property FdTransparency: TMFFloat read FFdTransparency;

    { Fields used by ray-tracers. } { }
    strict private FFdMirror: TMFFloat;
    public property FdMirror: TMFFloat read FFdMirror;

    strict private FFdReflSpecular: TMFColor;
    public property FdReflSpecular: TMFColor read FFdReflSpecular;

    strict private FFdReflDiffuse: TMFColor;
    public property FdReflDiffuse: TMFColor read FFdReflDiffuse;

    strict private FFdTransSpecular: TMFColor;
    public property FdTransSpecular: TMFColor read FFdTransSpecular;

    strict private FFdTransDiffuse: TMFColor;
    public property FdTransDiffuse: TMFColor read FFdTransDiffuse;

    strict private FFdReflSpecularExp: TMFFloat;
    public property FdReflSpecularExp: TMFFloat read FFdReflSpecularExp;

    strict private FFdTransSpecularExp: TMFFloat;
    public property FdTransSpecularExp: TMFFloat read FFdTransSpecularExp;

    strict private FFdFogImmune: TSFBool;
    public property FdFogImmune: TSFBool read FFdFogImmune;

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

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    { Material information based on this node.
      It is automatically updated when properties of this material change.
      Do not free it yourself, it will be automatically freed when
      this node is freed. }
    function MaterialInfo(const AIndex: Integer = 0): TPhongMaterialInfo;
  end;

  TMaterialBindingNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;
  end;

  TNormalBindingNode_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;
  end;

  TTexture2Node_1 = class(TAbstractTexture2DNode)
  strict protected
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

    strict private FFdFilename: TSFString;
    public property FdFilename: TSFString read FFdFilename;

    strict private FFdImage: TSFImage;
    public property FdImage: TSFImage read FFdImage;

    strict private FFdWrapS: TSFEnum;
    public property FdWrapS: TSFEnum read FFdWrapS;

    strict private FFdWrapT: TSFEnum;
    public property FdWrapT: TSFEnum read FFdWrapT;

    { Ignored fields, some unknown extensions to VRML 1.0 spec.
      Some models ([http://www-vrl.umich.edu/sel_prj/EECS498/]) use them.
      @groupBegin }
    strict private FFdModel: TSFEnum;
    public property FdModel: TSFEnum read FFdModel;

    strict private FFdBlendColor: TSFVec3f;
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

    strict private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    strict private FFdRotation: TSFFloat;
    public property FdRotation: TSFFloat read FFdRotation;

    strict private FFdScaleFactor: TSFVec2f;
    public property FdScaleFactor: TSFVec2f read FFdScaleFactor;

    strict private FFdCenter: TSFVec2f;
    public property FdCenter: TSFVec2f read FFdCenter;

    function TextureMatrixTransformation: TMatrix4;
  end;

  TTextureCoordinate2Node_1 = class(TAbstractChildNode)
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;
  end;

  TShapeHintsNode_1 = class(TAbstractChildNode)
  protected
    function ParseNodeBodyElement(Lexer: TX3DLexer; Reader: TX3DReaderNames;
      const APositionInParent: Integer): boolean; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdVertexOrdering: TSFEnum;
    public property FdVertexOrdering: TSFEnum read FFdVertexOrdering;

    strict private FFdShapeType: TSFEnum;
    public property FdShapeType: TSFEnum read FFdShapeType;

    strict private FFdFaceType: TSFEnum;
    public property FdFaceType: TSFEnum read FFdFaceType;

    strict private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;
  end;

  { Common base class for all VRML 1 nodes that modify the transformation. }
  TAbstractTransformationNode_1 = class(TAbstractChildNode)
  end;

  { VRML 1.0 MatrixTransform node. }
  TMatrixTransformNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdMatrix: TSFMatrix;
    public property FdMatrix: TSFMatrix read FFdMatrix;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TRotationNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;
  end;

  TScaleNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdScaleFactor: TSFVec3f;
    public property FdScaleFactor: TSFVec3f read FFdScaleFactor;
  end;

  TTransformNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    strict private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    strict private FFdScaleFactor: TSFVec3f;
    public property FdScaleFactor: TSFVec3f read FFdScaleFactor;

    strict private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    strict private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TTranslationNode_1 = class(TAbstractTransformationNode_1)
  protected
    procedure MiddleTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;
  end;

  { Common base class for all cameras in VRML 1.0. }
  TAbstractCameraNode_1 = class(TAbstractViewpointNode)
  strict protected
    function PositionField: TSFVec3f; override;
  public
    procedure CreateNode; override;

    strict private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    strict private FFdFocalDistance: TSFFloat;
    public property FdFocalDistance: TSFFloat read FFdFocalDistance;

    { Ignored fields, some unknown extensions to VRML 1.0 spec.
      Some models ([http://www-vrl.umich.edu/sel_prj/EECS498/]) use them.
      @groupBegin }
    strict private FFdNearDistance: TSFFloat;
    public property FdNearDistance: TSFFloat read FFdNearDistance;

    strict private FFdFarDistance: TSFFloat;
    public property FdFarDistance: TSFFloat read FFdFarDistance;
    { @groupEnd }
  end;

  TOrthographicCameraNode_1 = class(TAbstractCameraNode_1)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ProjectionType: TProjectionType; override;

    strict private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;
  end;

  TPerspectiveCameraNode_1 = class(TAbstractCameraNode_1)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ProjectionType: TProjectionType; override;

    strict private FFdHeightAngle: TSFFloat;
    public property FdHeightAngle: TSFFloat read FFdHeightAngle;
  end;

  TDirectionalLightNode_1 = class(TAbstractDirectionalLightNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function Scope: TLightScope; override;
  end;

  TPointLightNode_1 = class(TAbstractPointLightNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
    function HasRadius: boolean; override;
    function Scope: TLightScope; override;
  end;

  TSpotLightNode_1 = class(TAbstractPositionalLightNode)
  protected
    function  GetProjectionLocationLocal: TVector3; override;
    procedure SetProjectionLocationLocal(const Value: TVector3); override;
    function  GetProjectionDirectionLocal: TVector3; override;
    procedure SetProjectionDirectionLocal(const Value: TVector3); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    strict private FFdDropOffRate: TSFFloat;
    public property FdDropOffRate: TSFFloat read FFdDropOffRate;

    strict private FFdCutOffAngle: TSFFloat;
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

    function ProjectionMatrix: TMatrix4; override;
    function ModelviewMatrix: TMatrix4; override;
    function ModelviewRotationMatrix: TMatrix4; override;
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

    strict private FFdRenderCulling: TSFEnum;
    public property FdRenderCulling: TSFEnum read FFdRenderCulling;
  end;

  TSwitchNode_1 = class(TAbstractChildNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdWhichChild: TSFLong;
    public property FdWhichChild: TSFLong read FFdWhichChild;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;
  end;

  TTransformSeparatorNode_1 = class(TAbstractChildNode)
  strict private
    OriginalTransformation: TTransformation;
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

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    strict private FFdMap: TSFEnum;
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

    strict private FFdSeparate: TSFBool;
    public property FdSeparate: TSFBool read FFdSeparate;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TAbstractGeometryNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

const
  { Constants for @link(TAsciiTextNode_1.FdJustification).Value. }
  JUSTIFICATION_LEFT = 0;
  JUSTIFICATION_CENTER = 1;
  JUSTIFICATION_RIGHT = 2;

procedure TAsciiTextNode_1.CreateNode;
begin
  inherited;

  FFdString := TMFString.Create(Self, true, 'string', ['']);
   FdString.ChangeAlways := chGeometry;
  AddField(FFdString);

  FFdSpacing := TSFFloat.Create(Self, true, 'spacing', 1);
   FdSpacing.ChangeAlways := chGeometry;
  AddField(FFdSpacing);

  FFdJustification := TSFEnum.Create(Self, true, 'justification', ['LEFT', 'CENTER', 'RIGHT'], JUSTIFICATION_LEFT);
   FdJustification.ChangeAlways := chGeometry;
  AddField(FFdJustification);

  FFdWidth := TMFFloat.Create(Self, true, 'width', [0]);
   FdWidth.ChangeAlways := chGeometry;
  AddField(FFdWidth);
end;

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

class function TAsciiTextNode_1.ClassX3DType: string;
begin
  Result := 'AsciiText';
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

  FFdParts := TSFBitMask.Create(Self, true, 'parts', ['SIDES', 'BOTTOM'], 'NONE', 'ALL', [true, true]);
   FdParts.ChangeAlways := chGeometry;
  AddField(FFdParts);

  FFdBottomRadius := TSFFloat.Create(Self, true, 'bottomRadius', 1);
   FdBottomRadius.MustBeNonnegative := true;
   FdBottomRadius.ChangeAlways := chGeometry;
  AddField(FFdBottomRadius);

  FFdHeight := TSFFloat.Create(Self, true, 'height', 2);
   FdHeight.MustBeNonnegative := true;
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);
end;

class function TConeNode_1.ClassX3DType: string;
begin
  Result := 'Cone';
end;

function TConeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCubeNode_1.CreateNode;
begin
  inherited;

  FFdWidth := TSFFloat.Create(Self, true, 'width', 2);
   FdWidth.MustBeNonnegative := true;
   FdWidth.ChangeAlways := chGeometry;
  AddField(FFdWidth);

  FFdHeight := TSFFloat.Create(Self, true, 'height', 2);
   FdHeight.MustBeNonnegative := true;
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);

  FFdDepth := TSFFloat.Create(Self, true, 'depth', 2);
   FdDepth.MustBeNonnegative := true;
   FdDepth.ChangeAlways := chGeometry;
  AddField(FFdDepth);
end;

class function TCubeNode_1.ClassX3DType: string;
begin
  Result := 'Cube';
end;

function TCubeNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCylinderNode_1.CreateNode;
begin
  inherited;

  FFdParts := TSFBitMask.Create(Self, true, 'parts', ['SIDES', 'TOP', 'BOTTOM'], 'NONE', 'ALL', [true, true, true]);
   FdParts.ChangeAlways := chGeometry;
  AddField(FFdParts);

  FFdRadius := TSFFloat.Create(Self, true, 'radius', 1);
   FdRadius.MustBeNonnegative := true;
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);

  FFdHeight := TSFFloat.Create(Self, true, 'height', 2);
   FdHeight.MustBeNonnegative := true;
   FdHeight.ChangeAlways := chGeometry;
  AddField(FFdHeight);
end;

class function TCylinderNode_1.ClassX3DType: string;
begin
  Result := 'Cylinder';
end;

function TCylinderNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TAbstractIndexedNode_1.CreateNode;
begin
  inherited;

  FFdCoordIndex := TMFLong.Create(Self, true, 'coordIndex', [0]);
   FdCoordIndex.SaveToStreamLineUptoNegative := true;
   FdCoordIndex.ChangeAlways := chGeometry;
  AddField(FFdCoordIndex);

  FFdMaterialIndex := TMFLong.Create(Self, true, 'materialIndex', [-1]);
   FdMaterialIndex.ChangeAlways := chGeometry;
  AddField(FFdMaterialIndex);

  FFdNormalIndex := TMFLong.Create(Self, true, 'normalIndex', [-1]);
   FdNormalIndex.ChangeAlways := chGeometry;
  AddField(FFdNormalIndex);

  FFdTextureCoordIndex := TMFLong.Create(Self, true, 'textureCoordIndex', [-1]);
   FdTextureCoordIndex.SaveToStreamLineUptoNegative := true;
   FdTextureCoordIndex.ChangeAlways := chGeometry;
  AddField(FFdTextureCoordIndex);
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

procedure TIndexedFaceSetNode_1.CreateNode;
begin
  inherited;

  FFdRadianceTransfer := TMFVec3f.Create(Self, true, 'radianceTransfer', []);
   FdRadianceTransfer.ChangeAlways := chGeometry;
  AddField(FFdRadianceTransfer);
end;

class function TIndexedFaceSetNode_1.ClassX3DType: string;
begin
  Result := 'IndexedFaceSet';
end;

function TIndexedFaceSetNode_1.InternalTexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.VRML1State.TextureCoordinate2;
end;

class function TIndexedLineSetNode_1.ClassX3DType: string;
begin
  Result := 'IndexedLineSet';
end;

function TIndexedLineSetNode_1.InternalTexCoord(State: TX3DGraphTraverseState;
  out ATexCoord: TX3DNode): boolean;
begin
  Result := true;
  ATexCoord := State.VRML1State.TextureCoordinate2;
end;

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
                (FdNormalIndex.Items.List^[0] >= 0);
    else
      Result := true;
  end;

  { check Normal.vector non-empty }
  if State.VRML1State.Normal.FdVector.Count = 0 then
    Result := false;
end;

procedure TPointSetNode_1.CreateNode;
begin
  inherited;

  FFdStartIndex := TSFLong.Create(Self, true, 'startIndex', 0);
   FdStartIndex.ChangeAlways := chGeometry;
  AddField(FFdStartIndex);

  FFdNumPoints := TSFLong.Create(Self, true, 'numPoints', -1);
   FdNumPoints.ChangeAlways := chGeometry;
  AddField(FFdNumPoints);

  CoordSubrange := TMFVec3f.Create(Self, true, '', []);
end;

destructor TPointSetNode_1.Destroy;
begin
  FreeAndNil(CoordSubrange);
  inherited;
end;

class function TPointSetNode_1.ClassX3DType: string;
begin
  Result := 'PointSet';
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

procedure TSphereNode_1.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, true, 'radius', 1);
   FdRadius.MustBeNonnegative := true;
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
end;

class function TSphereNode_1.ClassX3DType: string;
begin
  Result := 'Sphere';
end;

function TSphereNode_1.AutoGenerate3DTexCoords: boolean;
begin
  Result := true;
end;

procedure TCoordinate3Node_1.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3f.Create(Self, true, 'point', [Vector3(0, 0, 0)]);
   FdPoint.ChangeAlways := chCoordinate;
  AddField(FFdPoint);
end;

class function TCoordinate3Node_1.ClassX3DType: string;
begin
  Result := 'Coordinate3';
end;

procedure TFontStyleNode_1.CreateNode;
begin
  inherited;

  FFdSize := TSFFloat.Create(Self, true, 'size', 10);
   FdSize.MustBeNonnegative := true;
   FdSize.ChangeAlways := chGeometryVRML1State;
  AddField(FFdSize);

  {$warnings off}
  FFdFamily := TSFEnum.Create(Self, true, 'family', ['SERIF', 'SANS', 'TYPEWRITER'], FSFAMILY_SERIF);
  {$warnings on}
   FdFamily.ChangeAlways := chGeometryVRML1State;
  AddField(FFdFamily);

  FFdStyle := TSFBitMask.Create(Self, true, 'style', ['BOLD', 'ITALIC'], 'NONE', '', [false, false]);
   FdStyle.ChangeAlways := chGeometryVRML1State;
  AddField(FFdStyle);

  FFdBlending := TSFBool.Create(Self, true, 'blending', true);
   FdBlending.ChangeAlways := chGeometryVRML1State;
  AddField(FFdBlending);
end;

class function TFontStyleNode_1.ClassX3DType: string;
begin
  Result := 'FontStyle';
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

  FFdString := TSFString.Create(Self, true, 'string', '<Undefined info>');
  AddField(FFdString);
end;

class function TInfoNode_1.ClassX3DType: string;
begin
  Result := 'Info';
end;

procedure TLODNode_1.CreateNode;
begin
  inherited;

  FFdRange := TMFFloat.Create(Self, true, 'range',[]);
  AddField(FFdRange);

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
  AddField(FFdCenter);

  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TLODNode_1.ClassX3DType: string;
begin
  Result := 'LOD';
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

class function TLODNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TMaterialNode_1.CreateNode;
begin
  inherited;

  FFdAmbientColor := TMFColor.Create(Self, true, 'ambientColor', [TPhongMaterialInfo.DefaultAmbientColor]);
   FdAmbientColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientColor);

  FFdDiffuseColor := TMFColor.Create(Self, true, 'diffuseColor', [TPhongMaterialInfo.DefaultDiffuseColor]);
   FdDiffuseColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseColor);

  FFdSpecularColor := TMFColor.Create(Self, true, 'specularColor', [TPhongMaterialInfo.DefaultSpecularColor]);
   FdSpecularColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularColor);

  FFdEmissiveColor := TMFColor.Create(Self, true, 'emissiveColor', [TPhongMaterialInfo.DefaultEmissiveColor]);
   FdEmissiveColor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveColor);

  FFdShininess := TMFFloat.Create(Self, true, 'shininess', [TPhongMaterialInfo.DefaultShininess]);
   FdShininess.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininess);

  FFdTransparency := TMFFloat.Create(Self, true, 'transparency', [TMaterialInfo.DefaultTransparency]);
   FdTransparency.ChangeAlways := chAlphaChannel;
  AddField(FFdTransparency);

  FFdMirror := TMFFloat.Create(Self, true, 'mirror', [0.0]);
   FdMirror.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdMirror);

  FFdReflSpecular := TMFColor.Create(Self, true, 'reflSpecular', []);
   FdReflSpecular.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflSpecular);

  FFdReflDiffuse := TMFColor.Create(Self, true, 'reflDiffuse', []);
   FdReflDiffuse.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflDiffuse);

  FFdTransSpecular := TMFColor.Create(Self, true, 'transSpecular', []);
   FdTransSpecular.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransSpecular);

  FFdTransDiffuse := TMFColor.Create(Self, true, 'transDiffuse', []);
   FdTransDiffuse.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransDiffuse);

  FFdReflSpecularExp := TMFFloat.Create(Self, true, 'reflSpecularExp', [TPhongMaterialInfo.DefaultReflSpecularExp]);
   FdReflSpecularExp.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflSpecularExp);

  FFdTransSpecularExp := TMFFloat.Create(Self, true, 'transSpecularExp', [TPhongMaterialInfo.DefaultTransSpecularExp]);
   FdTransSpecularExp.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransSpecularExp);

  FFdFogImmune := TSFBool.Create(Self, true, 'fogImmune', false);
   FdFogImmune.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdFogImmune);
end;

destructor TMaterialNode_1.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FMaterialInfo) - 1 do
    FreeAndNil(FMaterialInfo[I]);
  inherited;
end;

class function TMaterialNode_1.ClassX3DType: string;
begin
  Result := 'Material';
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
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.GetDiffuseColor: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdDiffuseColor.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultDiffuseColor
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
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
    Result := A.List^[Min(FIndex, A.Count - 1)];
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
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.Shininess: Single;
var
  A: TSingleList;
begin
  A := FNode.FdShininess.Items;
  if A.Count = 0 then
    Result := TPhongMaterialInfo.DefaultShininess
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
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
    M := A.List^[Min(FIndex, A.Count - 1)];

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
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransSpecular: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransDiffuse: TVector3;
var
  A: TVector3List;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.ReflSpecularExp: Single;
var
  A: TSingleList;
begin
  A := FNode.FdReflSpecularExp.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

function TMaterialNode_1.TMaterialInfo_1.TransSpecularExp: Single;
var
  A: TSingleList;
begin
  A := FNode.FdTransSpecularExp.Items;
  if A.Count = 0 then
    Result := inherited
  else
    Result := A.List^[Min(FIndex, A.Count - 1)];
end;

{ TMaterialBindingNode_1 ----------------------------------------------------- }

procedure TMaterialBindingNode_1.CreateNode;
begin
  inherited;

  FFdValue := TSFEnum.Create(Self, true, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 1);
   FdValue.ChangeAlways := chVisibleVRML1State;
  AddField(FFdValue);
end;

class function TMaterialBindingNode_1.ClassX3DType: string;
begin
  Result := 'MaterialBinding';
end;

procedure TNormalBindingNode_1.CreateNode;
begin
  inherited;

  FFdValue := TSFEnum.Create(Self, true, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 0);
   FdValue.ChangeAlways := chVisibleVRML1State;
  AddField(FFdValue);
end;

class function TNormalBindingNode_1.ClassX3DType: string;
begin
  Result := 'NormalBinding';
end;

const
  { Constants for @link(TTexture2Node_1.FdWrapS).Value and @link(TTexture2Node_1.FdWrapT).Value. }
  TEXWRAP_REPEAT = 0;
  TEXWRAP_CLAMP = 1;

procedure TTexture2Node_1.CreateNode;
begin
  inherited;

  FFdFilename := TSFString.Create(Self, true, 'filename', '');
   FdFilename.ChangeAlways := chTextureImage;
  AddField(FFdFilename);

  FFdImage := TSFImage.Create(Self, true, 'image', nil);
   FdImage.ChangeAlways := chTextureImage;
  AddField(FFdImage);

  FFdWrapS := TSFEnum.Create(Self, true, 'wrapS', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
   FdWrapS.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdWrapS);

  FFdWrapT := TSFEnum.Create(Self, true, 'wrapT', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
   FdWrapT.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdWrapT);

  FFdModel := TSFEnum.Create(Self, true, 'model', ['DECAL'], 0);
   FdModel.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdModel);

  FFdBlendColor := TSFVec3f.Create(Self, true, 'blendColor', Vector3(0, 0, 0));
   FdBlendColor.ChangeAlways := chTexturePropertiesNode;
  AddField(FFdBlendColor);
end;

class function TTexture2Node_1.ClassX3DType: string;
begin
  Result := 'Texture2';
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

procedure TTexture2TransformNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, true, 'translation', Vector2(0, 0));
   FdTranslation.ChangeAlways := chEverything;
  AddField(FFdTranslation);

  FFdRotation := TSFFloat.Create(Self, true, 'rotation', 0);
   FdRotation.ChangeAlways := chEverything;
  AddField(FFdRotation);

  FFdScaleFactor := TSFVec2f.Create(Self, true, 'scaleFactor', Vector2(1, 1));
   FdScaleFactor.ChangeAlways := chEverything;
  AddField(FFdScaleFactor);

  FFdCenter := TSFVec2f.Create(Self, true, 'center', Vector2(0, 0));
   FdCenter.ChangeAlways := chEverything;
  AddField(FFdCenter);
end;

class function TTexture2TransformNode_1.ClassX3DType: string;
begin
  Result := 'Texture2Transform';
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

procedure TTextureCoordinate2Node_1.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, true, 'point', [Vector2(0, 0)]);
   FdPoint.ChangeAlways := chVisibleVRML1State;
  AddField(FFdPoint);
end;

class function TTextureCoordinate2Node_1.ClassX3DType: string;
begin
  Result := 'TextureCoordinate2';
end;

procedure TShapeHintsNode_1.CreateNode;
begin
  inherited;

  FFdVertexOrdering := TSFEnum.Create(Self, true, 'vertexOrdering', ['UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE'], VERTORDER_UNKNOWN);
   FdVertexOrdering.ChangeAlways := chVisibleVRML1State;
  AddField(FFdVertexOrdering);

  FFdShapeType := TSFEnum.Create(Self, true, 'shapeType', ['UNKNOWN_SHAPE_TYPE', 'SOLID'], SHTYPE_UNKNOWN);
   FdShapeType.ChangeAlways := chVisibleVRML1State;
  AddField(FFdShapeType);

  FFdFaceType := TSFEnum.Create(Self, true, 'faceType', ['UNKNOWN_FACE_TYPE', 'CONVEX'], FACETYPE_CONVEX);
   FdFaceType.ChangeAlways := chGeometryVRML1State;
  AddField(FFdFaceType);

  FFdCreaseAngle := TSFFloat.Create(Self, true, 'creaseAngle', DefaultVRML1CreaseAngle);
   FdCreaseAngle.Angle := true;
   FdCreaseAngle.ChangeAlways := chVisibleVRML1State;
  AddField(FFdCreaseAngle);
end;

class function TShapeHintsNode_1.ClassX3DType: string;
begin
  Result := 'ShapeHints';
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

procedure TMatrixTransformNode_1.CreateNode;
begin
  inherited;

  FFdMatrix := TSFMatrix.Create(Self, true, 'matrix', TMatrix4.Identity);
   FdMatrix.ChangeAlways := chEverything;
  AddField(FFdMatrix);
end;

class function TMatrixTransformNode_1.ClassX3DType: string;
begin
 Result := 'MatrixTransform';
end;

procedure TMatrixTransformNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.MultiplyMatrix(FdMatrix.Value);
end;

class function TMatrixTransformNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TRotationNode_1.CreateNode;
begin
  inherited;

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chEverything;
  AddField(FFdRotation);
end;

class function TRotationNode_1.ClassX3DType: string;
begin
  Result := 'Rotation';
end;

procedure TRotationNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(FdRotation.Value, Vector3(1, 1, 1), TVector3.Zero);
end;

procedure TScaleNode_1.CreateNode;
begin
  inherited;

  FFdScaleFactor := TSFVec3f.Create(Self, true, 'scaleFactor', Vector3(1, 1, 1));
   FdScaleFactor.ChangeAlways := chEverything;
  AddField(FFdScaleFactor);
end;

class function TScaleNode_1.ClassX3DType: string;
begin
 Result := 'Scale';
end;

procedure TScaleNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(TVector4.Zero, FdScaleFactor.Value, TVector3.Zero);
end;

procedure TTransformNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chEverything;
  AddField(FFdTranslation);

  FFdRotation := TSFRotation.Create(Self, true, 'rotation', Vector3(0, 0, 1), 0);
   FdRotation.ChangeAlways := chEverything;
  AddField(FFdRotation);

  FFdScaleFactor := TSFVec3f.Create(Self, true, 'scaleFactor', Vector3(1, 1, 1));
   FdScaleFactor.ChangeAlways := chEverything;
  AddField(FFdScaleFactor);

  FFdScaleOrientation := TSFRotation.Create(Self, true, 'scaleOrientation', Vector3(0, 0, 1), 0);
   FdScaleOrientation.ChangeAlways := chEverything;
  AddField(FFdScaleOrientation);

  FFdCenter := TSFVec3f.Create(Self, true, 'center', Vector3(0, 0, 0));
   FdCenter.ChangeAlways := chEverything;
  AddField(FFdCenter);
end;

class function TTransformNode_1.ClassX3DType: string;
begin
  Result := 'Transform';
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

class function TTransformNode_1.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major <= 1;
end;

procedure TTranslationNode_1.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec3f.Create(Self, true, 'translation', Vector3(0, 0, 0));
   FdTranslation.ChangeAlways := chEverything;
  AddField(FFdTranslation);
end;

class function TTranslationNode_1.ClassX3DType: string;
begin
  Result := 'Translation';
end;

procedure TTranslationNode_1.MiddleTraverse(StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.Transformation.Multiply(TVector4.Zero, Vector3(1, 1, 1), FdTranslation.Value);
end;

procedure TAbstractCameraNode_1.CreateNode;
begin
  inherited;

  { Note that the default "position" value for VRML 1.0 is different
    than for VRML >= 2.0 (where is (0, 0, 10)). }
  FFdPosition := TSFVec3f.Create(Self, true, 'position', Vector3(0, 0, 1));
   FdPosition.ChangeAlways := chViewpointVectors;
  AddField(FFdPosition);

  FFdFocalDistance := TSFFloat.Create(Self, true, 'focalDistance', 5);
   FdFocalDistance.MustBeNonnegative := true;
  AddField(FFdFocalDistance);

  FFdNearDistance := TSFFloat.Create(Self, true, 'nearDistance', 0);
   FdNearDistance.ChangeAlways := chViewpointProjection;
  AddField(FFdNearDistance);

  FFdFarDistance := TSFFloat.Create(Self, true, 'farDistance', 0);
   FdFarDistance.ChangeAlways := chViewpointProjection;
  AddField(FFdFarDistance);
end;

function TAbstractCameraNode_1.PositionField: TSFVec3f;
begin
  Result := FdPosition;
end;

procedure TOrthographicCameraNode_1.CreateNode;
begin
  inherited;

  FFdHeight := TSFFloat.Create(Self, true, 'height', 2);
   FdHeight.MustBeNonnegative := true;
  AddField(FFdHeight);
end;

class function TOrthographicCameraNode_1.ClassX3DType: string;
begin
  Result := 'OrthographicCamera';
end;

class function TOrthographicCameraNode_1.ProjectionType: TProjectionType;
begin
  Result := ptOrthographic;
end;

procedure TPerspectiveCameraNode_1.CreateNode;
begin
  inherited;

  FFdHeightAngle := TSFFloat.Create(Self, true, 'heightAngle', Pi / 4);
   FdHeightAngle.MustBeNonnegative := true;
  AddField(FFdHeightAngle);
end;

class function TPerspectiveCameraNode_1.ClassX3DType: string;
begin
  Result := 'PerspectiveCamera';
end;

class function TPerspectiveCameraNode_1.ProjectionType: TProjectionType;
begin
  Result := ptPerspective;
end;

procedure TDirectionalLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See https://castle-engine.io/x3d_extensions.php#ext_light_attenuation }
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

class function TDirectionalLightNode_1.ClassX3DType: string;
begin
  Result := 'DirectionalLight';
end;

procedure TPointLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See https://castle-engine.io/x3d_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3(0, 0, 1);
  FdLocation.DefaultValue := Vector3(0, 0, 1);
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

class function TPointLightNode_1.ClassX3DType: string;
begin
  Result := 'PointLight';
end;

procedure TSpotLightNode_1.CreateNode;
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See https://castle-engine.io/x3d_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3(0, 0, 1);
  FdLocation.DefaultValue := Vector3(0, 0, 1);

  FFdDirection := TSFVec3f.Create(Self, true, 'direction', Vector3(0, 0, -1));
   FdDirection.ChangeAlways := chLightLocationDirection;
  AddField(FFdDirection);

  FFdDropOffRate := TSFFloat.Create(Self, true, 'dropOffRate', 0);
   FdDropOffRate.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDropOffRate);

  FFdCutOffAngle := TSFFloat.Create(Self, true, 'cutOffAngle', Pi / 4);
   FdCutOffAngle.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdCutOffAngle);
end;

class function TSpotLightNode_1.ClassX3DType: string;
begin
  Result := 'SpotLight';
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
  Result := 'Group';
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
    DirectEnumerateActive({$ifdef FPC}@{$endif} Enumerator.Enumerate);
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

  FFdRenderCulling := TSFEnum.Create(Self, true, 'renderCulling', ['ON', 'OFF', 'AUTO'], 2);
  AddField(FFdRenderCulling);
end;

class function TSeparatorNode_1.ClassX3DType: string;
begin
  Result := 'Separator';
end;

procedure TSwitchNode_1.CreateNode;
begin
  inherited;

  FFdWhichChild := TSFLong.Create(Self, true, 'whichChild', -1);
   FdWhichChild.ChangeAlways := chEverything;
  AddField(FFdWhichChild);

  VRML1ChildrenAllowed := true;
  VRML1ChildrenParsingAllowed := true;
end;

class function TSwitchNode_1.ClassX3DType: string;
begin
  Result := 'Switch';
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
  Result := 'TransformSeparator';
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

procedure TWWWAnchorNode_1.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  FFdMap := TSFEnum.Create(Self, true, 'map', ['NONE', 'POINT'], 0);
  AddField(FFdMap);
end;

class function TWWWAnchorNode_1.ClassX3DType: string;
begin
  Result := 'WWWAnchor';
end;

procedure TWWWInlineNode_1.CreateNode;
begin
  inherited;

  { change "url" field to "name", with default value being an empty string }
  FdUrl.AddAlternativeName('name', 1);
  FdUrl.Items.Add('');
  FdUrl.AssignDefaultValueFromValue;

  { bboxSize is (0, 0, 0) in VRMl 1.0 (in VRML 2.0/X3D it's (-1, -1, -1)) }
  FdBboxSize.Value := Vector3(0, 0, 0);
  FdBboxSize.AssignDefaultValueFromValue;

  FFdSeparate := TSFBool.Create(Self, true, 'separate', true);
   FdSeparate.ChangeAlways := chEverything;
  AddField(FFdSeparate);
end;

class function TWWWInlineNode_1.ClassX3DType: string;
begin
  Result := 'WWWInline';
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
