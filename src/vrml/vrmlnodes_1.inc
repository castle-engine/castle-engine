{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
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
  TNodeGroup_1 and TNodeGroup_2).)

  Note that most VRML 1 nodes descend from TNodeX3DChildNode,
  this way we can use them inside VRML >= 2.0 group nodes and
  mix VRML 1.0 and greater versions.
} { }

{$ifdef read_interface}
  { This is descendant of TVRMLGeometryNode that is allowed only in
    VRML <= 1.0.

    In VRML 1.0 shape nodes are allowed pretty everywhere,
    while VRML 2.0 has different idea of how shapes are handled
    (they must be inside Shape node), so no shape node
    is suitable at the same time for VRML 1.0 and VRML 2.0. }
  TVRMLGeometryNode_1 = class(TVRMLGeometryNode, INodeX3DChildNode)
  public
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeAsciiText_1 = class(TVRMLGeometryNode_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdString: TMFString;
    public property FdString: TMFString read FFdString;

    private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    { Use consts JUSTIFICATION_XXX (declared below in this unit) }
    private FFdJustification: TSFEnum;
    public property FdJustification: TSFEnum read FFdJustification;

    private FFdWidth: TMFFloat;
    public property FdWidth: TMFFloat read FFdWidth;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    function Justify: TVRMLFontJustify;
  end;

  TNodeCone_1 = class(TVRMLGeometryNode_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    private FFdBottomRadius: TSFFloat;
    public property FdBottomRadius: TSFFloat read FFdBottomRadius;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    function Proxy(State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  TNodeCube_1 = class(TVRMLGeometryNode_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdWidth: TSFFloat;
    public property FdWidth: TSFFloat read FFdWidth;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    private FFdDepth: TSFFloat;
    public property FdDepth: TSFFloat read FFdDepth;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCylinder_1 = class(TVRMLGeometryNode_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdParts: TSFBitMask;
    public property FdParts: TSFBitMask read FFdParts;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdHeight: TSFFloat;
    public property FdHeight: TSFFloat read FFdHeight;

    function Proxy(State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  { wspolny rodzic dla IndexedFaceSet, IndexedTriangleMesh, IndexedLineSet }
  TVRMLIndexedNode_1 = class(TVRMLGeometryNode_1)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdCoordIndex: TMFLong;
    public property FdCoordIndex: TMFLong read FFdCoordIndex;

    private FFdMaterialIndex: TMFLong;
    public property FdMaterialIndex: TMFLong read FFdMaterialIndex;

    private FFdNormalIndex: TMFLong;
    public property FdNormalIndex: TMFLong read FFdNormalIndex;

    private FFdTextureCoordIndex: TMFLong;
    public property FdTextureCoordIndex: TMFLong read FFdTextureCoordIndex;

    function Coord(State: TVRMLGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordIndex: TMFLong; override;
  end;

  { wspolny rodzic dla IndexedFaceSet i IndexedTriangleMesh }
  TNodeIndexed_Faces_Or_Triangles_1 = class(TVRMLIndexedNode_1)
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  TNodeIndexedFaceSet_1 = class(TNodeIndexed_Faces_Or_Triangles_1)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ClassNodeTypeName: string; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    private FFdRadianceTransfer: TMFVec3f;
    public property FdRadianceTransfer: TMFVec3f read FFdRadianceTransfer;

    function TexCoord(State: TVRMLGraphTraverseState;
      out ATexCoord: TVRMLNode): boolean; override;
  end;

  TNodeIndexedLineSet_1 = class(TVRMLIndexedNode_1)
    class function ClassNodeTypeName: string; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    function TexCoord(State: TVRMLGraphTraverseState;
      out ATexCoord: TVRMLNode): boolean; override;
  end;

  TNodePointSet_1 = class(TVRMLGeometryNode_1)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdStartIndex: TSFLong;
    public property FdStartIndex: TSFLong read FFdStartIndex;

    private FFdNumPoints: TSFLong;
    public property FdNumPoints: TSFLong read FFdNumPoints;

    {oblicz startIndex i numPoints na podstawie odpowiednich pol.
     Zwrocone numPoints jest na pewno > 0, przedzial startIndex..numPoints-1
     na pewno zawiera sie w przedziale 0..LastCoordinate3.FdPoint.Count-1.
     Ta proc. NIE poprawia wartosci na polach Fd* - bo byc moze w skryptach
     bedzie wygodniej zakladac ze nawet nieprawidlowe wartosci sa trwale. }
    procedure CalculateRange(LastCoordinate3: TNodeCoordinate3;
      out startIndex, numPoints: integer);

    function Coord(State: TVRMLGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeSphere_1 = class(TVRMLGeometryNode_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCoordinate3 = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdPoint: TMFVec3f;
    public property FdPoint: TMFVec3f read FFdPoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeFontStyle_1 = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdSize: TSFFloat;
    public property FdSize: TSFFloat read FFdSize;

    private FFdFamily: TSFEnum;
    public property FdFamily: TSFEnum read FFdFamily;

    private FFdStyle: TSFBitMask;
    public property FdStyle: TSFBitMask read FFdStyle;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function Family: TVRMLFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function TTF_Font: PTrueTypeFont;
  end;

  TNodeInfo = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdString: TSFString;
    public property FdString: TSFString read FFdString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeLOD_1 = class(TNodeX3DChildNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdRange: TMFFloat;
    public property FdRange: TMFFloat read FFdRange;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TVRMLMaterialInfo_1 = class;

  TNodeMaterial_1 = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

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

    { pola dodane przeze mnie na potrzeby raytracerow na RGK } { }
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

    { ponizej sa funkcje do latwego wyciagania materialu z node'a material.
      De facto powinienes ich uzywac zawsze gdy zalezy ci na wyciagnieciu
      wartosci materialu. Te funkcje uwzgledniaja fakt ze pola moga byc
      zapisane zle (niezgodnie ze specyfik. VRMLa albo zgodnie ale w jakis
      szczegolny przypadek wymagajacy opakowania) - np. ze moze nie byc
      podanej zadnej wartosci dla ambient a my chcemy kolor numer zero
      (w tym przypadku zwracany jest defaultowy kolor ambient materialu VRMLa)
      albo ze moze nie byc wystarczajacej ilosci kolorow (wtedy bierzemy
      ostatni kolor).

      Funkcje zwracajace TVector4Single zwracaja [TVector3Single, Opacity].

      Transparency i Opacity sa zwracane w zakresie 0..1, przy czym
      Opacity = 1 - Transparency.

      ShininessExp to @italic(nieznormalizowany) wykladnik odbicia zwierciadlanego
      (dla modelu osw. Phonga) (mimo ze w VRMLu powinnismy dostac
      znormalizowany, tzn. w zakresie 0..1 reprezentujacym 0..128;
      ale nie widze szczerze mowiac sensu takiej normalizacji, wartosc
      128 nie jest (chyba?) zadna szczegolna wartoscia, to tylko taka
      ustalona granica powyzej ktorej i tak wartosci juz w zasadzie nie maja
      sensu (cos^128 to juz jest BARDZO waska gorka).
      W sumie dla usera zakres 0..1 moze byc po prostu wygodniejszy do myslenia
      (chociaz moze byc tez mylacy, bo roznice miedzy 1 a 0.5 w takiej konwencji
      trudno jakos okreslic jako "dwa razy wieksza') ale dla wszystkiego co
      chcielibysmy robic programowo zdecydowanie bedziemy potrzebowali
      nieznormalizowanej wartosci. }
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

    { true oznacza ze ten material to specjalny przypadek o ktorym mowa w
      specyfikacji VRMLa : pola ambient, diffuse i specular maja dlugosc = 0
      a wiec nalezy uznac kolor Emissive za JUZ WYLICZONY ostateczny kolor.
      Powinienes wowczas odczytac tylko wartosc EmissiveColor i
      Transparency/Opacity (ujete w jednej prostej wartosci EmissiveColor4Single)
      i nie patrzec na inne wlasciwosci materialu. }
    function OnlyEmissiveMaterial: boolean;

    { true jesli wszystkie elementy pola FdTransparency sa ostro wieksze
      od zera  (od SingleEqualityEpsilon, tak naprawde).
      Pamietaj ze pole FdTransparency o Length = 0 jest zawsze traktowane
      jakby podano wartosc domyslna, tzn. [0], a wiec wtedy ta funkcja zwraca
      false (jest to sprzeczne z matematyczna (i intuicyjna) definicja
      kwalifikatora "all" ktora mowi ze "kazde zdanie kwalifikowane
      'dla kazdego' jest prawdziwe dla zbioru pustego") }
    function AllMaterialsTransparent: boolean;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function MaterialInfo(const Index: Integer): TVRMLMaterialInfo_1;
  end;

  TVRMLMaterialInfo_1 = class(TVRMLMaterialInfo)
  private
    FNode: TNodeMaterial_1;
    FIndex: Integer;
  public
    constructor Create(Node: TNodeMaterial_1; const Index: Integer);

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

  TNodeMaterialBinding = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeNormalBinding = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdValue: TSFEnum;
    public property FdValue: TSFEnum read FFdValue;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTexture2 = class(TVRMLTextureNode)
  protected
    { Texture in this class is loaded z pliku (pole filename)
      lub inlined (pole image). Pierwszenstwo ma tekstura z pliku,
      jesli filename = '' (lub wystapi jakis blad przy ladowaniu z filename
      ale VRMLWarning to zignoruje) to zostanie uzyta tekstura inline.

      BTW, in VRML 1.0 node z IsTextureLoaded = true i
      IsTextureImage = false tez ma swoje znaczenie:
      oznacza "wylacz aktywna teksture". }
    procedure LoadTextureData(out CacheUsed: boolean); override;
  public
    constructor Create(const ANodeName: string;
      const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdFilename: TSFString;
    public property FdFilename: TSFString read FFdFilename;

    private FFdImage: TSFImage;
    public property FdImage: TSFImage read FFdImage;

    private FFdWrapS: TSFEnum;
    public property FdWrapS: TSFEnum read FFdWrapS;

    private FFdWrapT: TSFEnum;
    public property FdWrapT: TSFEnum read FFdWrapT;

    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    private FFdModel: TSFEnum;
    public property FdModel: TSFEnum read FFdModel;

    private FFdBlendColor: TSFVec3f;
    public property FdBlendColor: TSFVec3f read FFdBlendColor;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodeTexture2Transform = class(TNodeX3DChildNode)
  protected
    procedure MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    private FFdRotation: TSFFloat;
    public property FdRotation: TSFFloat read FFdRotation;

    private FFdScaleFactor: TSFVec2f;
    public property FdScaleFactor: TSFVec2f read FFdScaleFactor;

    private FFdCenter: TSFVec2f;
    public property FdCenter: TSFVec2f read FFdCenter;

    function TextureMatrixTransformation: TMatrix4Single;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTextureCoordinate2 = class(TNodeX3DChildNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeShapeHints = class(TNodeX3DChildNode)
  protected
    function ParseNodeBodyElement(Lexer: TVRMLLexer; Names: TVRMLNames;
      const APositionInParent: Integer): boolean; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdVertexOrdering: TSFEnum;
    public property FdVertexOrdering: TSFEnum read FFdVertexOrdering;

    private FFdShapeType: TSFEnum;
    public property FdShapeType: TSFEnum read FFdShapeType;

    private FFdFaceType: TSFEnum;
    public property FdFaceType: TSFEnum read FFdFaceType;

    private FFdCreaseAngle: TSFFloat;
    public property FdCreaseAngle: TSFFloat read FFdCreaseAngle;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { TVRMLTransformationNode - wspolna klasa dla wszystkich node'ow ktorych
    jedynym celem jest zmodyfikowac aktualna macierz modelview.
    Wystarczy ze w kazdej z podklas napiszesz funkcje MatrixTransform: TMatrix4f
    no i oczywiscie zainicjujesz pola danego node'a. }
  TVRMLTransformationNode = class(TNodeX3DChildNode)
  protected
    procedure MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); virtual; abstract;
    function AverageScaleTransform: Single; virtual; abstract;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { This is VRML 1.0 MatrixTransform node. }
  TNodeMatrixTransform_1 = class(TVRMLTransformationNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

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
    function AverageScaleTransform: Single; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;
  end;

  TNodeRotation = class(TVRMLTransformationNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function AverageScaleTransform: Single; override;
  end;

  TNodeScale = class(TVRMLTransformationNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdScaleFactor: TSFVec3f;
    public property FdScaleFactor: TSFVec3f read FFdScaleFactor;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function AverageScaleTransform: Single; override;
  end;

  TNodeTransform_1 = class(TVRMLTransformationNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

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
    function AverageScaleTransform: Single; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;
  end;

  TNodeTranslation = class(TVRMLTransformationNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;

    procedure Transformation(out Matrix, InvertedMatrix: TMatrix4Single); override;
    function AverageScaleTransform: Single; override;
  end;

  { GeneralCamera - wspolna klasa dla wszystkich kamer VRML'a. }
  TVRMLCameraNode_1 = class(TVRMLViewpointNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdPosition: TSFVec3f;
    public property FdPosition: TSFVec3f read FFdPosition;

    private FFdFocalDistance: TSFFloat;
    public property FdFocalDistance: TSFFloat read FFdFocalDistance;

    private FFdHeightAngle: TSFFloat;
    public property FdHeightAngle: TSFFloat read FFdHeightAngle;

    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    private FFdNearDistance: TSFFloat;
    public property FdNearDistance: TSFFloat read FFdNearDistance;

    private FFdFarDistance: TSFFloat;
    public property FdFarDistance: TSFFloat read FFdFarDistance;

    function Position: TSFVec3f; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeOrthographicCamera = class(TVRMLCameraNode_1)
    class function ClassNodeTypeName: string; override;
    class function ProjectionType: TProjectionType; override;
  end;

  TNodePerspectiveCamera = class(TVRMLCameraNode_1)
    class function ClassNodeTypeName: string; override;
    class function ProjectionType: TProjectionType; override;
  end;

  TNodeDirectionalLight_1 = class(TVRMLDirectionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodePointLight_1 = class(TVRMLPointLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeSpotLight_1 = class(TVRMLPositionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdDirection: TSFVec3f;
    public property FdDirection: TSFVec3f read FFdDirection;

    private FFdDropOffRate: TSFFloat;
    public property FdDropOffRate: TSFFloat read FFdDropOffRate;

    private FFdCutOffAngle: TSFFloat;
    public property FdCutOffAngle: TSFFloat read FFdCutOffAngle;

    { nieznormalizowany wykladnik dla spot'a (na podstawie dropOffRate) }
    function SpotExp: Single;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    procedure UpdateActiveLight(var ActiveLight: TActiveLight); override;

    function ProjectionMatrix: TMatrix4Single; override;
    function ModelviewMatrix: TMatrix4Single; override;
    function ModelviewRotationMatrix: TMatrix4Single; override;
    function LocationLocal: TVector3Single; override;
    function DirectionLocal: TVector3Single; override;
    procedure Box3DDistances(const Box: TBox3D;
      out MinDistance, MaxDistance: Single); override;
  end;

  TNodeGroup_1 = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { A Group node that is added when VRML 1.0 file contains more than one root
    node.

    We represent this special "artifical" group as a TVRMLRootNode_1,
    @italic(that is a descendant of TNodeGroup_1 (not the other way around)).
    This way you can entirely forget about this issue and just process
    the VRML model as you like, and the only downside will be that you
    will actually work with a very slightly different model
    (with additional Group node) than what was encoded in the file.
    You can also test for (Node is TVRMLRootNode_1/2) and
    recognize this special case. }
  TVRMLRootNode_1 = class(TNodeGroup_1, IVRMLRootNode)
  private
    FForceVersion: boolean;
    FForceVersionMajor: Integer;
    FForceVersionMinor: Integer;
    FSaveAsRootNode: boolean;

    { To satisfy interface IVRMLRootNode }
    function GetSaveAsRootNode: boolean;
    procedure SetSaveAsRootNode(const Value: boolean);
  protected
    function DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { See IVRMLRootNode.SaveAsRootNode. }
    property SaveAsRootNode: boolean read FSaveAsRootNode write FSaveAsRootNode
      default true;

    (*Save TVRMLRootNode_* contents to stream.

      If SaveAsRootNode then TVRMLRootNode_* is saved to stream in a special way, so that actually
      only it's contents are written, without surrounding { } braces.
      This way, when saving, we "undo" the artificial wrapping in
      TVRMLRootNode_* that was done by ParseVRMLStatements and LoadVRMLClassic. *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);  override;

    { Set ForceVersion to @true to make SuggestedVRMLVersion of this class
      return suggest ForceVersionMajor, ForceVersionMinor with strong
      priority.

      @groupBegin }
    property ForceVersion: boolean read FForceVersion write FForceVersion default false;
    property ForceVersionMajor: Integer read FForceVersionMajor write FForceVersionMajor;
    property ForceVersionMinor: Integer read FForceVersionMinor write FForceVersionMinor;
    { @groupEnd }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { A general class that can ce used as a separator, something that
    pushes and pops all attribs and matrices.
    It is used in implementation of Separator and WWWAnchor.
    Also WWWInline does the same work, when it's "separate" field is true. }
  TVRMLSeparatorNode = class(TNodeX3DChildNode)
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeSeparator = class(TVRMLSeparatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdRenderCulling: TSFEnum;
    public property FdRenderCulling: TSFEnum read FFdRenderCulling;
  end;

  TNodeSwitch_1 = class(TNodeX3DChildNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdWhichChild: TSFLong;
    public property FdWhichChild: TSFLong read FFdWhichChild;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTransformSeparator = class(TNodeX3DChildNode)
  private
    OriginalTransform, OriginalInvertedTransform: TMatrix4Single;
    OriginalAverageScaleTransform: Single;
  protected
    procedure BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
    procedure AfterTraverse(StateStack: TVRMLGraphTraverseStateStack); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeWWWAnchor = class(TVRMLSeparatorNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    private FFdMap: TSFEnum;
    public property FdMap: TSFEnum read FFdMap;
  end;

  { VRML 1.0 WWWInline node.

    This is implemented as a descendant of VRML 2.0/X3D Inline node
    class. This way VRML 1.0 actually gets a couple of VRML 2.0/X3D extensions.
    The VRML 2.0/X3D field "url" is renamed here to VRML 1.0 field "name".
    (Note that this means that WWWInline.name is actually MFString,
    not just SFString like VRML 1.0 spec says.) }
  TNodeWWWInline = class(TNodeInline)
  protected
    function SeparateGroup: boolean; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    private FFdSeparate: TSFBool;
    public property FdSeparate: TSFBool read FFdSeparate;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TVRMLGeometryNode_1.ForVRMLVersion(
  const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TVRMLGeometryNode_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeAsciiText_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdstring := TMFString.Create(Self, 'string', ['']);
  Fields.Add(FFdstring);

  FFdspacing := TSFFloat.Create(Self, 'spacing', 1);
  Fields.Add(FFdspacing);

  FFdjustification := TSFEnum.Create(Self, 'justification', ['LEFT', 'CENTER', 'RIGHT'], JUSTIFICATION_LEFT);
  Fields.Add(FFdjustification);

  FFdwidth := TMFFloat.Create(Self, 'width', [0]);
  Fields.Add(FFdwidth);
end;

class function TNodeAsciiText_1.ClassNodeTypeName: string;
begin
  result := 'AsciiText';
end;

function TNodeAsciiText_1.Justify: TVRMLFontJustify;
begin
  Result := TVRMLFontJustify(FdJustification.Value);
end;

constructor TNodeCone_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdparts := TSFBitMask.Create(Self, 'parts', ['SIDES', 'BOTTOM'], 'NONE', 'ALL', [true, true]);
  Fields.Add(FFdparts);

  FFdbottomRadius := TSFFloat.Create(Self, 'bottomRadius', 1, true);
  Fields.Add(FFdbottomRadius);

  FFdheight := TSFFloat.Create(Self, 'height', 2, true);
  Fields.Add(FFdheight);
end;

class function TNodeCone_1.ClassNodeTypeName: string;
begin
  result := 'Cone';
end;

constructor TNodeCube_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdwidth := TSFFloat.Create(Self, 'width', 2, true);
  Fields.Add(FFdwidth);

  FFdheight := TSFFloat.Create(Self, 'height', 2, true);
  Fields.Add(FFdheight);

  FFddepth := TSFFloat.Create(Self, 'depth', 2, true);
  Fields.Add(FFddepth);
end;

class function TNodeCube_1.ClassNodeTypeName: string;
begin
  result := 'Cube';
end;

constructor TNodeCylinder_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdparts := TSFBitMask.Create(Self, 'parts', ['SIDES', 'TOP', 'BOTTOM'], 'NONE', 'ALL', [true, true, true]);
  Fields.Add(FFdparts);

  FFdradius := TSFFloat.Create(Self, 'radius', 1, true);
  Fields.Add(FFdradius);

  FFdheight := TSFFloat.Create(Self, 'height', 2, true);
  Fields.Add(FFdheight);
end;

class function TNodeCylinder_1.ClassNodeTypeName: string;
begin
  result := 'Cylinder';
end;

constructor TVRMLIndexedNode_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcoordIndex := TMFLong.Create(Self, 'coordIndex', [0]);
  FFdcoordIndex.SaveToStreamLineUptoNegative := true;
  Fields.Add(FFdcoordIndex);

  FFdmaterialIndex := TMFLong.Create(Self, 'materialIndex', [-1]);
  Fields.Add(FFdmaterialIndex);

  FFdnormalIndex := TMFLong.Create(Self, 'normalIndex', [-1]);
  Fields.Add(FFdnormalIndex);

  FFdtextureCoordIndex := TMFLong.Create(Self, 'textureCoordIndex', [-1]);
  FFdtextureCoordIndex.SaveToStreamLineUptoNegative := true;
  Fields.Add(FFdtextureCoordIndex);
end;

function TVRMLIndexedNode_1.Coord(State: TVRMLGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  ACoord := State.LastNodes.Coordinate3.FdPoint;
end;

function TVRMLIndexedNode_1.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

constructor TNodeIndexedFaceSet_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdRadianceTransfer := TMFVec3f.Create(Self, 'radianceTransfer', []);
  Fields.Add(FFdRadianceTransfer);
end;

class function TNodeIndexedFaceSet_1.ClassNodeTypeName: string;
begin
  result := 'IndexedFaceSet';
end;

function TNodeIndexedFaceSet_1.TexCoord(State: TVRMLGraphTraverseState;
  out ATexCoord: TVRMLNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

class function TNodeIndexedLineSet_1.ClassNodeTypeName: string;
begin
  result := 'IndexedLineSet';
end;

function TNodeIndexedLineSet_1.TexCoord(State: TVRMLGraphTraverseState;
  out ATexCoord: TVRMLNode): boolean;
begin
  Result := true;
  ATexCoord := State.LastNodes.TextureCoordinate2;
end;

constructor TNodePointSet_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdstartIndex := TSFLong.Create(Self, 'startIndex', 0);
  Fields.Add(FFdstartIndex);

  FFdnumPoints := TSFLong.Create(Self, 'numPoints', -1);
  Fields.Add(FFdnumPoints);
end;

class function TNodePointSet_1.ClassNodeTypeName: string;
begin
  result := 'PointSet';
end;

procedure TNodePointSet_1.CalculateRange(LastCoordinate3: TNodeCoordinate3;
  out startIndex, numPoints: integer);
begin
  startIndex := FdStartIndex.Value;
  numPoints := FdNumPoints.Value;
  if startIndex >= LastCoordinate3.FdPoint.Count then
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
      if startIndex+numPoints > LastCoordinate3.FdPoint.Count then
        numPoints := LastCoordinate3.FdPoint.Count - startIndex;
    end else
      numPoints := LastCoordinate3.FdPoint.Count - startIndex;
  end;
end;

function TNodePointSet_1.Coord(State: TVRMLGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  ACoord := State.LastNodes.Coordinate3.FdPoint;
end;

constructor TNodeSphere_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdradius := TSFFloat.Create(Self, 'radius', 1, true);
  Fields.Add(FFdradius);
end;

class function TNodeSphere_1.ClassNodeTypeName: string;
begin
  result := 'Sphere';
end;

constructor TNodeCoordinate3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec3f.Create(Self, 'point', [Vector3Single(0, 0, 0)]);
  Fields.Add(FFdpoint);
end;

class function TNodeCoordinate3.ClassNodeTypeName: string;
begin
  result := 'Coordinate3';
end;

function TNodeCoordinate3.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeFontStyle_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdsize := TSFFloat.Create(Self, 'size', 10, true);
  Fields.Add(FFdsize);

  FFdfamily := TSFEnum.Create(Self, 'family', ['SERIF', 'SANS', 'TYPEWRITER'], FSFAMILY_SERIF);
  Fields.Add(FFdfamily);

  FFdstyle := TSFBitMask.Create(Self, 'style', ['BOLD', 'ITALIC'], 'NONE', '', [false, false]);
  Fields.Add(FFdstyle);
end;

class function TNodeFontStyle_1.ClassNodeTypeName: string;
begin
  result := 'FontStyle';
end;

function TNodeFontStyle_1.TTF_Font: PTrueTypeFont;
begin
  Result := TTF_Font_Results[Family, Bold, Italic];
end;

function TNodeFontStyle_1.Family: TVRMLFontFamily;
begin
  Result := TVRMLFontFamily(FdFamily.Value);
end;

function TNodeFontStyle_1.Bold: boolean;
begin
  Result := FdStyle.Flags[FSSTYLE_BOLD];
end;

function TNodeFontStyle_1.Italic: boolean;
begin
  Result := FdStyle.Flags[FSSTYLE_ITALIC];
end;

class function TNodeFontStyle_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeFontStyle_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeInfo.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdstring := TSFString.Create(Self, 'string', '<Undefined info>');
  Fields.Add(FFdstring);
end;

class function TNodeInfo.ClassNodeTypeName: string;
begin
  result := 'Info';
end;

function TNodeInfo.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeLOD_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdrange := TMFFloat.Create(Self, 'range',[]);
  Fields.Add(FFdrange);

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);

  fParsingAllowedChildren := true;
  fAllowedChildren := true;
end;

class function TNodeLOD_1.ClassNodeTypeName: string;
begin
  result := 'LOD';
end;

procedure TNodeLOD_1.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { TODO: powinnismy tu uzywac odleglosci od viewera ? Problem.
    dla renderowania jest problem z wrzucaniem tego na display liste.
    dla boundingBoxa
      Wybrac ostatnie SubNode bo bedzie je nalatwiej obliczac ?
      Pierwsze, bo jest dokladne ? To ktore renderujemy ?
      W ostatnim przypadku, ladujemy z tym samym klopotem co RenderNKSpecific :
      zapamietywanie takiego BoundingBoxa nie jest poprawne.
  }
  if ChildrenCount = 0 then
    raise EVRMLError.Create('LOD node must have at least one child');

  Func(Self, Children[0]);
end;

class function TNodeLOD_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeLOD_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeMaterial_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdambientColor := TMFColor.Create(Self, 'ambientColor', [DefaultMaterial_1AmbientColor]);
  Fields.Add(FFdambientColor);

  FFddiffuseColor := TMFColor.Create(Self, 'diffuseColor', [DefaultMaterialDiffuseColor]);
  Fields.Add(FFddiffuseColor);

  FFdspecularColor := TMFColor.Create(Self, 'specularColor', [DefaultMaterialSpecularColor]);
  Fields.Add(FFdspecularColor);

  FFdemissiveColor := TMFColor.Create(Self, 'emissiveColor', [DefaultMaterialEmissiveColor]);
  Fields.Add(FFdemissiveColor);

  FFdshininess := TMFFloat.Create(Self, 'shininess', [DefaultMaterialShininess]);
  Fields.Add(FFdshininess);

  FFdtransparency := TMFFloat.Create(Self, 'transparency', [DefaultMaterialTransparency]);
  Fields.Add(FFdtransparency);

  FFdmirror := TMFFloat.Create(Self, 'mirror', [DefaultMaterialMirror]);
  Fields.Add(FFdmirror);

  FFdreflSpecular := TMFColor.Create(Self, 'reflSpecular', []);
  Fields.Add(FFdreflSpecular);

  FFdreflDiffuse := TMFColor.Create(Self, 'reflDiffuse', []);
  Fields.Add(FFdreflDiffuse);

  FFdtransSpecular := TMFColor.Create(Self, 'transSpecular', []);
  Fields.Add(FFdtransSpecular);

  FFdtransDiffuse := TMFColor.Create(Self, 'transDiffuse', []);
  Fields.Add(FFdtransDiffuse);

  FFdreflSpecularExp := TMFFloat.Create(Self, 'reflSpecularExp', [DefaultMaterialReflSpecularExp]);
  Fields.Add(FFdreflSpecularExp);

  FFdtransSpecularExp := TMFFloat.Create(Self, 'transSpecularExp', [DefaultMaterialTransSpecularExp]);
  Fields.Add(FFdtransSpecularExp);

  FFdfogImmune := TSFBool.Create(Self, 'fogImmune', false);
  Fields.Add(FFdfogImmune);
end;

class function TNodeMaterial_1.ClassNodeTypeName: string;
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
function TNodeMaterial_1.MATERIAL_FUNCTION_NAME_3(MatNum: integer): TVector3Single;
begin
  if MATERIAL_FUNCTION_FIELD.Count = 0 then
    result := MATERIAL_FUNCTION_DEFAULT else
    result := MATERIAL_FUNCTION_FIELD.Items.Items[
      min(MatNum, MATERIAL_FUNCTION_FIELD.Count - 1)];
end;

function TNodeMaterial_1.MATERIAL_FUNCTION_NAME_4(MatNum: integer): TVector4Single;
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
function TNodeMaterial_1.MATERIAL_FUNCTION_NAME(MatNum: integer): Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_DEFAULT else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[
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

function TNodeMaterial_1.Opacity(MatNum: integer): Single;
begin
  result := 1 - Transparency(MatNum);
end;

function TNodeMaterial_1.Shininess(MatNum: integer): Single;
begin
  if FdShininess.Count = 0 then
    result := DefaultMaterialShininess else
    result := FdShininess.Items.Items[min(MatNum, FdShininess.Count-1)];
end;

function TNodeMaterial_1.ShininessExp(MatNum: integer): Single;
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

function TNodeMaterial_1.OnlyEmissiveMaterial: boolean;
begin
  result:=(FdAmbientColor.Count = 0) and
          (FdDiffuseColor.Count = 0) and
          (FdSpecularColor.Count = 0);
end;

function TNodeMaterial_1.AllMaterialsTransparent: boolean;
var i: Integer;
begin
  if FdTransparency.Items.Length = 0 then
    result := DefaultMaterialTransparency > SingleEqualityEpsilon else
  begin
    for i := 0 to FdTransparency.Items.Length-1 do
      if FdTransparency.Items.Items[i] <= SingleEqualityEpsilon then Exit(false);
    result := true;
  end;
end;

class function TNodeMaterial_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeMaterial_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

function TNodeMaterial_1.MaterialInfo(const Index: Integer): TVRMLMaterialInfo_1;
begin
  Result := TVRMLMaterialInfo_1.Create(Self, Index);
end;

constructor TVRMLMaterialInfo_1.Create(Node: TNodeMaterial_1; const Index: Integer);
begin
  inherited Create;
  FNode := Node;
  FIndex := Index;
end;

function TVRMLMaterialInfo_1.DiffuseColor: TVector3Single;
begin
  Result := FNode.DiffuseColor3Single(FIndex);
end;

function TVRMLMaterialInfo_1.Mirror: Single;
begin
  Result := FNode.Mirror(FIndex);
end;

function TVRMLMaterialInfo_1.Transparency: Single;
begin
  Result := FNode.Transparency(FIndex);
end;

function TVRMLMaterialInfo_1.ReflSpecular: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdReflSpecular.Items;
  if A.Count = 0 then
    CalculateReflSpecular(Result) else
    Result := A.Items[Min(FIndex, A.Count - 1)];
end;

function TVRMLMaterialInfo_1.ReflDiffuse: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdReflDiffuse.Items;
  if A.Count = 0 then
    CalculateReflDiffuse(Result) else
    Result := A.Items[Min(FIndex, A.Count - 1)];
end;

function TVRMLMaterialInfo_1.TransSpecular: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdTransSpecular.Items;
  if A.Count = 0 then
    CalculateTransSpecular(Result) else
    Result := A.Items[Min(FIndex, A.Count - 1)];
end;

function TVRMLMaterialInfo_1.TransDiffuse: TVector3Single;
var
  A: TDynVector3SingleArray;
begin
  A := FNode.FdTransDiffuse.Items;
  if A.Count = 0 then
    CalculateTransDiffuse(Result) else
    Result := A.Items[Min(FIndex, A.Count - 1)];
end;

function TVRMLMaterialInfo_1.ReflSpecularExp: Single;
begin
  Result := FNode.ReflSpecularExp(FIndex);
end;

function TVRMLMaterialInfo_1.TransSpecularExp: Single;
begin
  Result := FNode.TransSpecularExp(FIndex);
end;

constructor TNodeMaterialBinding.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TSFEnum.Create(Self, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 1);
  Fields.Add(FFdvalue);
end;

class function TNodeMaterialBinding.ClassNodeTypeName: string;
begin
  result := 'MaterialBinding';
end;

function TNodeMaterialBinding.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeNormalBinding.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdvalue := TSFEnum.Create(Self, 'value', ['DEFAULT', 'OVERALL', 'PER_PART', 'PER_PART_INDEXED', 'PER_FACE', 'PER_FACE_INDEXED', 'PER_VERTEX', 'PER_VERTEX_INDEXED'], 0);
  Fields.Add(FFdvalue);
end;

class function TNodeNormalBinding.ClassNodeTypeName: string;
begin
  result := 'NormalBinding';
end;

function TNodeNormalBinding.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeTexture2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdfilename := TSFString.Create(Self, 'filename', '');
  Fields.Add(FFdfilename);

  FFdimage := TSFImage.Create(Self, 'image', nil);
  Fields.Add(FFdimage);

  FFdwrapS := TSFEnum.Create(Self, 'wrapS', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
  Fields.Add(FFdwrapS);

  FFdwrapT := TSFEnum.Create(Self, 'wrapT', ['REPEAT', 'CLAMP'], TEXWRAP_REPEAT);
  Fields.Add(FFdwrapT);

  FFdmodel := TSFEnum.Create(Self, 'model', ['DECAL'], 0);
  Fields.Add(FFdmodel);

  FFdblendColor := TSFVec3f.Create(Self, 'blendColor', Vector3Single(0, 0, 0));
  Fields.Add(FFdblendColor);
end;

class function TNodeTexture2.ClassNodeTypeName: string;
begin
  result := 'Texture2';
end;

procedure TNodeTexture2.LoadTextureData(out CacheUsed: boolean);
var
  FullUrl: string;
begin
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  { sprobuj zaladowac teksture z pliku FdFilename }
  if FdFilename.Value <> '' then
  try
    FullUrl := PathFromWWWBasePath(FdFilename.Value);
    FTextureImage := Cache.TextureImage_IncReference(FullUrl, FTextureDDS);
    FTextureUsedFullUrl := FullUrl;
    CacheUsed := true;
    Exit;
  except
    on E: Exception do
      { pamietajmy ze VRMLWarning moze spowodowac rzucenie wyjatku
        (chociaz nie musi) }
      VRMLWarning(vwIgnorable, 'Exception ' + E.ClassName +
        ' occured when trying to load '+
        'texture from filename "' + FullUrl + '" : ' + E.Message);
  end;

  { Still not loaded (so FdFilename.Value is '' or LoadImage raised exception) ?
    So try to use inlined texture. }
  if not FdImage.Value.IsNull then
    FTextureImage := FdImage.Value.MakeCopy;
end;

function TNodeTexture2.TextureDescription: string;

  function InlinedDescr: string;
  begin
    result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value.HasAlpha] ]);
  end;

begin
  if FdFilename.Value <> '' then
  begin
    result := 'file "' +PathFromWWWBasePath(FdFilename.Value) +'"';
    if not FdImage.Value.IsNull then result += ' (and '+InlinedDescr+')';
  end else
  if not FdImage.Value.IsNull then
    result := InlinedDescr else
    result := 'none';
end;

function TNodeTexture2.RepeatS: boolean;
begin
  Result := FdWrapS.Value = TEXWRAP_REPEAT;
end;

function TNodeTexture2.RepeatT: boolean;
begin
  Result := FdWrapT.Value = TEXWRAP_REPEAT;
end;

function TNodeTexture2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeTexture2Transform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdtranslation := TSFVec2f.Create(Self, 'translation', Vector2Single(0, 0));
  Fields.Add(FFdtranslation);

  FFdrotation := TSFFloat.Create(Self, 'rotation', 0);
  Fields.Add(FFdrotation);

  FFdscaleFactor := TSFVec2f.Create(Self, 'scaleFactor', Vector2Single(1, 1));
  Fields.Add(FFdscaleFactor);

  FFdcenter := TSFVec2f.Create(Self, 'center', Vector2Single(0, 0));
  Fields.Add(FFdcenter);
end;

class function TNodeTexture2Transform.ClassNodeTypeName: string;
begin
  result := 'Texture2Transform';
end;

function TNodeTexture2Transform.TextureMatrixTransformation: TMatrix4Single;
begin
  result := TranslationMatrix( Vector3Single(
    VectorAdd(FdTranslation.Value, FdCenter.Value) ));
  result := MatrixMult(result, RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  result := MatrixMult(result, ScalingMatrix(
    Vector3Single( FdScaleFactor.Value[0], FdScaleFactor.Value[1], 1 )));
  result := MatrixMult(result, TranslationMatrix(
    Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

procedure TNodeTexture2Transform.MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Top.TextureTransform := MatrixMult(StateStack.Top.TextureTransform,
    TextureMatrixTransformation);
end;

function TNodeTexture2Transform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeTextureCoordinate2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create(Self, 'point', [Vector2Single(0, 0)]);
  Fields.Add(FFdpoint);
end;

class function TNodeTextureCoordinate2.ClassNodeTypeName: string;
begin
  result := 'TextureCoordinate2';
end;

function TNodeTextureCoordinate2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeShapeHints.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvertexOrdering := TSFEnum.Create(Self, 'vertexOrdering', ['UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE'], VERTORDER_UNKNOWN);
  Fields.Add(FFdvertexOrdering);

  FFdshapeType := TSFEnum.Create(Self, 'shapeType', ['UNKNOWN_SHAPE_TYPE', 'SOLID'], SHTYPE_UNKNOWN);
  Fields.Add(FFdshapeType);

  FFdfaceType := TSFEnum.Create(Self, 'faceType', ['UNKNOWN_FACE_TYPE', 'CONVEX'], FACETYPE_CONVEX);
  Fields.Add(FFdfaceType);

  FFdcreaseAngle := TSFFloat.Create(Self, 'creaseAngle', DefaultVRML1CreaseAngle);
  Fields.Add(FFdcreaseAngle);
end;

class function TNodeShapeHints.ClassNodeTypeName: string;
begin
  result := 'ShapeHints';
end;

function TNodeShapeHints.ParseNodeBodyElement(Lexer: TVRMLLexer; Names: TVRMLNames;
  const APositionInParent: Integer): boolean;
var
  Hints: TSFBitMask;
begin
  Result := inherited;

  if not Result then
  begin
    Result := (Lexer.VRMLVerMajor = 0) and
      (Lexer.Token = vtName) and
      (Lexer.TokenName = 'hints');
    if Result then
    begin
      Hints := TSFBitMask.Create(Self, 'hints',
        ['SOLID', 'ORDERED', 'CONVEX'], 'NONE', '',
        [ false,   true,      true]);
      try
        Lexer.NextToken;
        Hints.Parse(Lexer, Names, false);
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

function TNodeShapeHints.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

procedure TVRMLTransformationNode.MiddleTraverse(StateStack: TVRMLGraphTraverseStateStack);
var
  M, IM: TMatrix4Single;
begin
  inherited;

  Transformation(M, IM);
  StateStack.Top.Transform := MatrixMult(StateStack.Top.Transform, M);
  StateStack.Top.InvertedTransform := MatrixMult(IM, StateStack.Top.InvertedTransform);

  StateStack.Top.AverageScaleTransform *= AverageScaleTransform;
end;

function TVRMLTransformationNode.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeMatrixTransform_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdmatrix := TSFMatrix.Create(Self, 'matrix', IdentityMatrix4Single);
  Fields.Add(FFdmatrix);
end;

class function TNodeMatrixTransform_1.ClassNodeTypeName: string;
begin
 result := 'MatrixTransform';
end;

procedure TNodeMatrixTransform_1.Transformation(
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

function TNodeMatrixTransform_1.AverageScaleTransform: Single;
begin
  Result := FdMatrix.AverageScaleTransform;
end;

class function TNodeMatrixTransform_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

constructor TNodeRotation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
end;

class function TNodeRotation.ClassNodeTypeName: string;
begin
  result := 'Rotation';
end;

procedure TNodeRotation.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  { We don't check here for FdRotation.Axis = zero, RotationMatricesRad
    will set both matrices for identity in such case. }
  RotationMatricesRad(FdRotation.RotationRad, FdRotation.Axis,
    Matrix, InvertedMatrix);
end;

function TNodeRotation.AverageScaleTransform: Single;
begin
  Result := 1;
end;

constructor TNodeScale.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdscaleFactor := TSFVec3f.Create(Self, 'scaleFactor', Vector3Single(1, 1, 1));
  Fields.Add(FFdscaleFactor);
end;

class function TNodeScale.ClassNodeTypeName: string;
begin
 result := 'Scale';
end;

procedure TNodeScale.Transformation(out Matrix, InvertedMatrix: TMatrix4Single);
begin
  ScalingMatrices(FdScaleFactor.Value, true, Matrix, InvertedMatrix);
end;

function TNodeScale.AverageScaleTransform: Single;
begin
  Result := ( FdScaleFactor.Value[0] +
              FdScaleFactor.Value[1] +
              FdScaleFactor.Value[2] ) / 3;
end;

constructor TNodeTransform_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);

  FFdscaleFactor := TSFVec3f.Create(Self, 'scaleFactor', Vector3Single(1, 1, 1));
  Fields.Add(FFdscaleFactor);

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
end;

class function TNodeTransform_1.ClassNodeTypeName: string;
begin
  result := 'Transform';
end;

procedure TNodeTransform_1.Transformation(out Matrix, InvertedMatrix: TMatrix4Single);
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

function TNodeTransform_1.AverageScaleTransform: Single;
begin
  Result := ( FdScaleFactor.Value[0] +
              FdScaleFactor.Value[1] +
              FdScaleFactor.Value[2] ) / 3;
end;

class function TNodeTransform_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

constructor TNodeTranslation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
end;

class function TNodeTranslation.ClassNodeTypeName: string;
begin
  result := 'Translation';
end;

procedure TNodeTranslation.Transformation(
  out Matrix, InvertedMatrix: TMatrix4Single);
begin
  TranslationMatrices(FdTranslation.Value, Matrix, InvertedMatrix);
end;

function TNodeTranslation.AverageScaleTransform: Single;
begin
  Result := 1;
end;

constructor TVRMLCameraNode_1.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { Note that the default "position" value for VRML 1.0 is different
    than for VRML >= 2.0 (where is (0, 0, 10)). }
  FFdposition := TSFVec3f.Create(Self, 'position', Vector3Single(0, 0, 1));
  Fields.Add(FFdposition);

  FFdfocalDistance := TSFFloat.Create(Self, 'focalDistance', 5, true);
  Fields.Add(FFdfocalDistance);

  FFdheightAngle := TSFFloat.Create(Self, 'heightAngle', Pi / 4, true);
  Fields.Add(FFdheightAngle);

  FFdnearDistance := TSFFloat.Create(Self, 'nearDistance', 0);
  Fields.Add(FFdnearDistance);

  FFdfarDistance := TSFFloat.Create(Self, 'farDistance', 0);
  Fields.Add(FFdfarDistance);
end;

function TVRMLCameraNode_1.Position: TSFVec3f;
begin
  Result := FdPosition;
end;

function TVRMLCameraNode_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

class function TNodeOrthographicCamera.ClassNodeTypeName: string;
begin
  result := 'OrthographicCamera';
end;

class function TNodeOrthographicCamera.ProjectionType: TProjectionType;
begin
  result := ptOrthographic;
end;

class function TNodePerspectiveCamera.ClassNodeTypeName: string;
begin
  result := 'PerspectiveCamera';
end;

class function TNodePerspectiveCamera.ProjectionType: TProjectionType;
begin
  result := ptPerspective;
end;

constructor TNodeDirectionalLight_1.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;
end;

class function TNodeDirectionalLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeDirectionalLight_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodePointLight_1.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3Single(0, 0, 1);
  FdLocation.DefaultValue := Vector3Single(0, 0, 1);
end;

class function TNodePointLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodePointLight_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeSpotLight_1.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { Default ambientIntensity value for VRML 1.0.
    See http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#ext_light_attenuation }
  FdAmbientIntensity.Value := -1;
  FdAmbientIntensity.DefaultValue := -1;

  { Default location value for VRML 1.0, was changed in VRML >= 2.0 }
  FdLocation.Value := Vector3Single(0, 0, 1);
  FdLocation.DefaultValue := Vector3Single(0, 0, 1);

  FFddirection := TSFVec3f.Create(Self, 'direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);
  FFddirection.ProcessedInActiveLight := true;

  FFddropOffRate := TSFFloat.Create(Self, 'dropOffRate', 0);
  Fields.Add(FFddropOffRate);

  FFdcutOffAngle := TSFFloat.Create(Self, 'cutOffAngle', Pi / 4);
  Fields.Add(FFdcutOffAngle);
end;

class function TNodeSpotLight_1.ClassNodeTypeName: string;
begin
  result := 'SpotLight';
end;

function TNodeSpotLight_1.SpotExp: Single;
begin
  result := FdDropOffRate.Value * 128.0;
end;

class function TNodeSpotLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

procedure TNodeSpotLight_1.UpdateActiveLight(
  var ActiveLight: TActiveLight);
begin
  inherited;
  ActiveLight.TransfNormDirection :=
    Normalized( MatrixMultDirection(ActiveLight.Transform, FdDirection.Value) );
end;

function TNodeSpotLight_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

function TNodeSpotLight_1.ProjectionMatrix: TMatrix4Single;
var
  Angle, N, F: Single;
begin
  { If author didn't provide and VRMLShadowMaps unit didn't calculate
    values for some fields, then use FallbackProjection* defaults here. }

  { Implementation just like for TNodeSpotLight_2,
    except I was too lazy to add here projectionAngle. }
  Angle := 2 * FdCutOffAngle.Value;

  N := FdProjectionNear.Value;
  if N = 0 then N := FallbackProjectionNear;

  F := FdProjectionFar.Value;
  if F = 0 then F := FallbackProjectionFar;

  Result := PerspectiveProjMatrixRad(Angle, 1, N, F);
end;

function TNodeSpotLight_1.ModelviewMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetCameraVectors(Pos, Dir, Up);
  Result := LookDirMatrix(Pos, Dir, Up);
end;

function TNodeSpotLight_1.ModelviewRotationMatrix: TMatrix4Single;
var
  Pos, Dir, Up: TVector3Single;
begin
  GetCameraVectors(Pos, Dir, Up);
  Result := LookDirMatrix(ZeroVector3Single, Dir, Up);
end;

function TNodeSpotLight_1.LocationLocal: TVector3Single;
begin
  Result := FdLocation.Value;
end;

function TNodeSpotLight_1.DirectionLocal: TVector3Single;
begin
  Result := FdDirection.Value;
end;

procedure TNodeSpotLight_1.Box3DDistances(const Box: TBox3D;
  out MinDistance, MaxDistance: Single);
begin
  { TODO: MaxDistance should be a little larger, as spot light rays
    are not parallel. }
  Box3DDirectionDistances(Box, Location, Direction, MinDistance, MaxDistance);
end;

constructor TNodeGroup_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  fParsingAllowedChildren := true;
  fAllowedChildren := true;
end;

class function TNodeGroup_1.ClassNodeTypeName: string;
begin
  result := 'Group';
end;

class function TNodeGroup_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeGroup_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TVRMLRootNode_1.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
  FSaveAsRootNode := true;
end;

function TVRMLRootNode_1.GetSaveAsRootNode: boolean;
begin
  Result := FSaveAsRootNode;
end;

procedure TVRMLRootNode_1.SetSaveAsRootNode(const Value: boolean);
begin
  FSaveAsRootNode := Value;
end;

procedure TVRMLRootNode_1.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties; NodeNames: TObject);
var
  I: integer;
  FileItems: TVRMLFileItemsList;
begin
  if SaveAsRootNode then
  begin
    { Special things for TVRMLRootNode_1 saving:
      NodeName is ignored (should be '').
      Fields are ignored (should never have any meaning). }

    FileItems := TVRMLFileItemsList.Create;
    try
      for I := 0 to Prototypes.Count - 1 do
        FileItems.Add(Prototypes[I]);

      for i := 0 to ChildrenCount - 1 do
        FileItems.Add(Children[i]);

      for I := 0 to Routes.Count - 1 do
        FileItems.Add(Routes[I]);

      for I := 0 to ImportsList.Count - 1 do
        FileItems.Add(ImportsList[I]);

      for I := 0 to ExportsList.Count - 1 do
        FileItems.Add(ExportsList[I]);

      FileItems.SaveToStream(SaveProperties, NodeNames);
    finally FreeAndNil(FileItems) end;
  end else
    inherited;
end;

function TVRMLRootNode_1.DeepCopyCore(CopyState: TVRMLNodeDeepCopyState): TVRMLNode;
var
  Res: TVRMLRootNode_1;
begin
  Result := inherited;

  Res := Result as TVRMLRootNode_1;

  Res.ForceVersion := ForceVersion;
  Res.ForceVersionMajor := ForceVersionMajor;
  Res.ForceVersionMinor := ForceVersionMinor;
end;

function TVRMLRootNode_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  if ForceVersion then
    AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
      true, ForceVersionMajor, ForceVersionMinor, 10 * 1000);
end;

constructor TVRMLSeparatorNode.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  fParsingAllowedChildren := true;
  fAllowedChildren := true;
end;

procedure TVRMLSeparatorNode.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;
  StateStack.Push;
end;

procedure TVRMLSeparatorNode.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  StateStack.Pop;
  inherited;
end;

function TVRMLSeparatorNode.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdrenderCulling := TSFEnum.Create(Self, 'renderCulling', ['ON', 'OFF', 'AUTO'], 2);
  Fields.Add(FFdrenderCulling);
end;

class function TNodeSeparator.ClassNodeTypeName: string;
begin
  result := 'Separator';
end;

constructor TNodeSwitch_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdwhichChild := TSFLong.Create(Self, 'whichChild', -1);
  Fields.Add(FFdwhichChild);
  fParsingAllowedChildren := true;
  fAllowedChildren := true;
end;

class function TNodeSwitch_1.ClassNodeTypeName: string;
begin
  result := 'Switch';
end;

procedure TNodeSwitch_1.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  if FdWhichChild.Value = -3 then
  begin
    { Enumerate all.
      Note : value -3 is already deprecated in VRML 1.0;
      but I support it, at least for now. }
    inherited;
  end else
  begin
    { Jezeli whichChild jest nieprawidlowe to w rezultacie nie wejdziemy w
      zadne Child. Wpp. wejdziemy w jedno wyznaczone child. I o to chodzi.
      (note : value -1 is no special value; any value that doesn't specify
      valid child number and is not -3 instructs Switch to not enter
      into any child. This is conformant with VRML 97 specification) }
    if Between(FdWhichChild.Value, 0, ChildrenCount - 1) then
      Func(Self, Children[FdWhichChild.Value]);
  end;
end;

class function TNodeSwitch_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeSwitch_1.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeTransformSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  fParsingAllowedChildren := true;
  fAllowedChildren := true;
end;

class function TNodeTransformSeparator.ClassNodeTypeName: string;
begin
  result := 'TransformSeparator';
end;

procedure TNodeTransformSeparator.BeforeTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  inherited;

  { We don't copy whole State here, as changes to other properties of
    TVRMLRenderState should "leak out" from TransformSeparator node. }
  OriginalTransform := StateStack.Top.Transform;
  OriginalInvertedTransform := StateStack.Top.InvertedTransform;
  OriginalAverageScaleTransform := StateStack.Top.AverageScaleTransform;
end;

procedure TNodeTransformSeparator.AfterTraverse(StateStack: TVRMLGraphTraverseStateStack);
begin
  StateStack.Top.Transform := OriginalTransform;
  StateStack.Top.InvertedTransform := OriginalInvertedTransform;
  StateStack.Top.AverageScaleTransform := OriginalAverageScaleTransform;
  inherited;
end;

function TNodeTransformSeparator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

constructor TNodeWWWAnchor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFddescription := TSFString.Create(Self, 'description', '');
  Fields.Add(FFddescription);

  FFdmap := TSFEnum.Create(Self, 'map', ['NONE', 'POINT'], 0);
  Fields.Add(FFdmap);
end;

class function TNodeWWWAnchor.ClassNodeTypeName: string;
begin
  result := 'WWWAnchor';
end;

constructor TNodeWWWInline.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { change "url" field to "name", with default value being an empty string }
  FdUrl.AddAlternativeName('name', 1);
  FdUrl.Items.Count := 1;
  FdUrl.Items[0] := '';
  FdUrl.AssignDefaultValueFromValue;

  { bboxSize is (0, 0, 0) in VRMl 1.0 (in VRML 2.0/X3D it's (-1, -1, -1)) }
  FFdbboxSize.Value := Vector3Single(0, 0, 0);
  FFdbboxSize.AssignDefaultValueFromValue;

  FFdseparate := TSFBool.Create(Self, 'separate', true);
  Fields.Add(FFdseparate);
end;

class function TNodeWWWInline.ClassNodeTypeName: string;
begin
  result := 'WWWInline';
end;

function TNodeWWWInline.SeparateGroup: boolean;
begin
  Result := FdSeparate.Value;
end;

function TNodeWWWInline.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);
end;

procedure RegisterVRML1Nodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeAsciiText_1, TNodeCone_1, TNodeCube_1, TNodeCylinder_1,
    TNodeIndexedFaceSet_1, TNodeIndexedLineSet_1,
    TNodePointSet_1, TNodeSphere_1,
    TNodeCoordinate3, TNodeFontStyle_1, TNodeInfo, TNodeLOD_1, TNodeMaterial_1,
    TNodeMaterialBinding, TNodeNormalBinding, TNodeTexture2,
    TNodeTexture2Transform,
    TNodeTextureCoordinate2, TNodeShapeHints,
    TNodeMatrixTransform_1, TNodeRotation,
    TNodeScale, TNodeTransform_1,
    TNodeTranslation,
    TNodeOrthographicCamera, TNodePerspectiveCamera,
    TNodeDirectionalLight_1, TNodePointLight_1, TNodeSpotLight_1,
    TNodeGroup_1, TNodeSeparator, TNodeSwitch_1, TNodeTransformSeparator,
    TNodeWWWAnchor,
    TNodeWWWInline
  ]);
end;

{$endif read_implementation}
