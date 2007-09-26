{
  Copyright 2002-2007 Michalis Kamburelis.

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

{ VRML 1.0 specification nodes.

  If a node is present in both VRML 1.0
  and later (2.0 aka 97) specifications @bold(and) it's implemented using
  the same class in our engine, then it goes to vrml97nodes.inc file,
  not here. So this is for VRML 1.0-only nodes.

  (The above doesn't concern much nodes. Most nodes in VRML 1.0 and 97
  specifications are different enough that even when they have the same
  name (like Group) we implement them using different classes (like
  TNodeGroup_1 and TNodeGroup_2).) } { }

{$ifdef read_interface}
  { This is descendant of TNodeGeneralShape that is allowed only in
    VRML <= 1.0.

    In VRML 1.0 shape nodes are allowed pretty everywhere,
    while VRML 2.0 has different idea of how shapes are handled
    (they must be inside Shape node), so no shape node
    is suitable at the same time for VRML 1.0 and VRML 2.0. }
  TNodeGeneralShape_1 = class(TNodeGeneralShape)
  public
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { Font justification that can be specified by FontStyle in
    justify/justification field. First three fields are equal
    (after casting by Ord) to JUSTIFICATION_* constants. }
  TVRMLFontJustify = (fjBegin, fjMiddle, fjEnd);

  TNodeAsciiText_1 = class(TNodeGeneralShape_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdString: TMFString index 0 read GetFieldAsMFString;
    property FdSpacing: TSFFloat index 1 read GetFieldAsSFFloat;
    { Use consts JUSTIFICATION_XXX (declared below in this unit) }
    property FdJustification: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdWidth: TMFFloat index 3 read GetFieldAsMFFloat;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    function Justify: TVRMLFontJustify;
  end;

  TNodeCone_1 = class(TNodeGeneralShape_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdParts: TSFBitMask index 0 read GetFieldAsSFBitMask;
    property FdBottomRadius: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 2 read GetFieldAsSFFloat;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCube_1 = class(TNodeGeneralShape_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdWidth: TSFFloat index 0 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdDepth: TSFFloat index 2 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCylinder_1 = class(TNodeGeneralShape_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdParts: TSFBitMask index 0 read GetFieldAsSFBitMask;
    property FdRadius: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdHeight: TSFFloat index 2 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  { wspolny rodzic dla IndexedFaceSet, IndexedTriangleMesh, IndexedLineSet }
  TNodeGeneralIndexed_1 = class(TNodeGeneralShape_1)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdCoordIndex: TMFLong index 0 read GetFieldAsMFLong;
    property FdMaterialIndex: TMFLong index 1 read GetFieldAsMFLong;
    property FdNormalIndex: TMFLong index 2 read GetFieldAsMFLong;
    property FdTextureCoordIndex: TMFLong index 3 read GetFieldAsMFLong;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  { wspolny rodzic dla IndexedFaceSet i IndexedTriangleMesh }
  TNodeIndexed_Faces_Or_Triangles_1 = class(TNodeGeneralIndexed_1)
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  TNodeIndexedFaceSet_1 = class(TNodeIndexed_Faces_Or_Triangles_1)
    class function ClassNodeTypeName: string; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeIndexedLineSet_1 = class(TNodeGeneralIndexed_1)
    class function ClassNodeTypeName: string; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodePointSet_1 = class(TNodeGeneralShape_1)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdStartIndex: TSFLong index 0 read GetFieldAsSFLong;
    property FdNumPoints: TSFLong index 1 read GetFieldAsSFLong;

    {oblicz startIndex i numPoints na podstawie odpowiednich pol.
     Zwrocone numPoints jest na pewno > 0, przedzial startIndex..numPoints-1
     na pewno zawiera sie w przedziale 0..LastCoordinate3.FdPoint.Count-1.
     Ta proc. NIE poprawia wartosci na polach Fd* - bo byc moze w skryptach
     bedzie wygodniej zakladac ze nawet nieprawidlowe wartosci sa trwale. }
    procedure CalculateRange(LastCoordinate3: TNodeCoordinate3;
      out startIndex, numPoints: integer);

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeSphere_1 = class(TNodeGeneralShape_1)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRadius: TSFFloat index 0 read GetFieldAsSFFloat;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCoordinate3 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdPoint: TMFVec3f index 0 read GetFieldAsMFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { Font family that can be specified by FontStyle node in family
    field. First three fields are equal (after casting by Ord) to
    three values of FSFAMILY_* constants. }
  TVRMLFontFamily = (ffSerif, ffSans, ffTypeWriter);

  TNodeFontStyle_1 = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdSize: TSFFloat index 0 read GetFieldAsSFFloat;
    property FdFamily: TSFEnum index 1 read GetFieldAsSFEnum;
    property FdStyle: TSFBitMask index 2 read GetFieldAsSFBitMask;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function Family: TVRMLFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function TTF_Font: PTrueTypeFont;
  end;

  TNodeInfo = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdString: TSFString index 0 read GetFieldAsSFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeLOD_1 = class(TVRMLNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdRange: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdCenter: TSFVec3f index 1 read GetFieldAsSFVec3f;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeMaterial_1 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdAmbientColor: TMFColor index 0 read GetFieldAsMFColor;
    property FdDiffuseColor: TMFColor index 1 read GetFieldAsMFColor;
    property FdSpecularColor: TMFColor index 2 read GetFieldAsMFColor;
    property FdEmissiveColor: TMFColor index 3 read GetFieldAsMFColor;
    property FdShininess: TMFFloat index 4 read GetFieldAsMFFloat;
    property FdTransparency: TMFFloat index 5 read GetFieldAsMFFloat;

    { pola dodane przeze mnie na potrzeby raytracerow na RGK }
    property FdMirror: TMFFloat index 6 read GetFieldAsMFFloat;
    property FdReflSpecular: TMFColor index 7 read GetFieldAsMFColor;
    property FdReflDiffuse: TMFColor index 8 read GetFieldAsMFColor;
    property FdTransSpecular: TMFColor index 9 read GetFieldAsMFColor;
    property FdTransDiffuse: TMFColor index 10 read GetFieldAsMFColor;
    property FdReflSpecularExp: TMFFloat index 11 read GetFieldAsMFFloat;
    property FdTransSpecularExp: TMFFloat index 12 read GetFieldAsMFFloat;

    property FdFogImmune: TSFBool index 13 read GetFieldAsSFBool;

    { ponizej sa funkcje do latwego wyciagania materialu z node'a material.
      De facto powinienes ich uzywac zawsze gdy zalezy ci na wyciagnieciu
      wartosci materialu. Te funkcje uwzgledniaja fakt ze pola moga byc
      zapisane zle (niezgodnie ze specyfik. VRMLa albo zgodnie ale w jakis
      szczegolny przypadek wymagajacy opakowania) - np. ze moze nie byc
      podanej zadnej wartosci dla ambient a my chcemy kolor numer zero
      (w tym przypadku zwracany jest defaultowy kolor ambient materialu VRMLa)
      albo ze moze nie byc wystarczajacej ilosci kolorow (wtedy bierzemy
      ostatni kolor).

      W przypadku czterech wartosci Trans/ReflSpecular/Diffuse
      te funkcje realizuja "wyliczanie" na podstawie innych pol materialu,
      tak jak opisalem w
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php].

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
    function ReflSpecular (MatNum: integer): TVector3Single;
    function ReflDiffuse  (MatNum: integer): TVector3Single;
    function TransSpecular(MatNum: integer): TVector3Single;
    function TransDiffuse (MatNum: integer): TVector3Single;
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
    function IsAllMaterialsTransparent: boolean;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeMaterialBinding = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdValue: TSFEnum index 0 read GetFieldAsSFEnum;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeNormalBinding = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdValue: TSFEnum index 0 read GetFieldAsSFEnum;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTexture2 = class(TNodeGeneralTexture)
  protected
    { Texture in this class is loaded z pliku (pole filename)
      lub inlined (pole image). Pierwszenstwo ma tekstura z pliku,
      jesli filename = '' (lub wystapi jakis blad przy ladowaniu z filename
      ale VRMLNonFatalError to zignoruje) to zostanie uzyta tekstura inline.
      Jezeli nie ma tekstury inline i nie ma prawidlowego filename to tekstura
      zostanie zaladowana jako ImageNone. To ostatnie stwierdzenie ma
      znaczenie: mowi ono ze IsTextureImage znaczy co innego niz
      IsTextureLoaded.

      BTW, in VRML 1.0 taki node z IsTextureLoaded = true i
      IsTextureImage = false tez ma swoje znaczenie:
      oznacza "wylacz aktywna teksture". }
    function LoadTextureImage(out CacheUsed: boolean): TImage; override;
  public
    constructor Create(const ANodeName: string;
      const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdFilename: TSFString index 0 read GetFieldAsSFString;
    property FdImage: TSFImage index 1 read GetFieldAsSFImage;
    property FdWrapS: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdWrapT: TSFEnum index 3 read GetFieldAsSFEnum;

    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    property FdModel: TSFEnum index 4 read GetFieldAsSFEnum;
    property FdBlendColor: TSFVec3f index 5 read GetFieldAsSFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodeTexture2Transform = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec2f index 0 read GetFieldAsSFVec2f;
    property FdRotation: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdScaleFactor: TSFVec2f index 2 read GetFieldAsSFVec2f;
    property FdCenter: TSFVec2f index 3 read GetFieldAsSFVec2f;
    function TextureMatrixTransformation: TMatrix4Single;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTextureCoordinate2 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdPoint: TMFVec2f index 0 read GetFieldAsMFVec2f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeShapeHints = class(TVRMLNode)
  private
    function ParseNodeBodyElement(Lexer: TVRMLLexer): boolean; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdVertexOrdering: TSFenum index 0 read GetFieldAsSFEnum;
    property FdShapeType: TSFEnum index 1 read GetFieldAsSFEnum;
    property FdFaceType: TSFEnum index 2 read GetFieldAsSFEnum;
    property FdCreaseAngle: TSFFloat index 3 read GetFieldAsSFFloat;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { TNodeGeneralTransformation - wspolna klasa dla wszystkich node'ow ktorych
    jedynym celem jest zmodyfikowac aktualna macierz modelview.
    Wystarczy ze w kazdej z podklas napiszesz funkcje MatrixTransform: TMatrix4f
    no i oczywiscie zainicjujesz pola danego node'a. }
  TNodeGeneralTransformation = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    function MatrixTransformation: TMatrix4Single; virtual; abstract;
    function AverageScaleTransform: Single; virtual; abstract;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeMatrixTransform = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdMatrix: TSFMatrix index 0 read GetFieldAsSFMatrix;

    function MatrixTransformation: TMatrix4Single; override;

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
  end;

  TNodeRotation = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRotation: TSFRotation index 0 read GetFieldAsSFRotation;

    function MatrixTransformation: TMatrix4Single; override;
    function AverageScaleTransform: Single; override;
  end;

  TNodeScale = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdScaleFactor: TSFVec3f index 0 read GetFieldAsSFVec3f;

    function MatrixTransformation: TMatrix4Single; override;
    function AverageScaleTransform: Single; override;
  end;

  TNodeTransform_1 = class(TNodeGeneralTransformation)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property FdRotation: TSFRotation index 1 read GetFieldAsSFRotation;
    property FdScaleFactor: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdScaleOrientation: TSFRotation index 3 read GetFieldAsSFRotation;
    property FdCenter: TSFVec3f index 4 read GetFieldAsSFVec3f;

    function MatrixTransformation: TMatrix4Single; override;
    function AverageScaleTransform: Single; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;
  end;

  TNodeTranslation = class(TNodeGeneralTransformation)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdTranslation: TSFVec3f index 0 read GetFieldAsSFVec3f;

    function MatrixTransformation: TMatrix4Single; override;
    function AverageScaleTransform: Single; override;
  end;

  TVRMLCameraKind = (ckOrthographic, ckPerspective);

  { A common class for both VRML 1.0 camera nodes and VRML 2.0 Viewpoint
    node. }
  TNodeGeneralViewpoint = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdPosition: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property FdOrientation: TSFRotation index 1 read GetFieldAsSFRotation;
    property FdDirection: TMFVec3f index 2 read GetFieldAsMFVec3f;
    property FdUp: TMFVec3f index 3 read GetFieldAsMFVec3f;
    property FdGravityUp: TSFVec3f index 4 read GetFieldAsSFVec3f;

    class function CameraKind: TVRMLCameraKind; virtual; abstract;

    { Calculate camera properties in the form of 3 vectors
      (position + direction + up) based on current field values of
      this node. Following VRML spec:

@preformatted(
  CamPos = FdPosition,
  CamDir = (0, 0, -1) rotated by FdOrientation,
  CamUp = (0, 1, 0) rotated by FdOrientation,
  GravityUp = (0, 1, 0) (not rotated by FdOrientation!),
  and everything is transformed by given CamTransform.
)
      (you should give here the actual VRML transformation at the point in file
      where camera is defined).

      Dodajemy do tego dodatki Kambiego :
      jesli FdDirection.Length > 0 to CamDirection nie jest liczone z
      FdOrientation ale jest brane wprost z FdDirection.Items[0].
      Podobnie dla FdUp.

      Zwraca zawsze znormalizowany CamDir i CamUp i GravityUp bo:
      @orderedList(
        @item(
          zeby zmusic cie do stosowania konsekwentnej zasady wyrazonej na
          poczatku VRMLNodes i nie pisania kodu ktory w jakis sposob bylby
          uzalezniony od podawanych tu CamDir. To dlatego ze
          pola FdOrientation / FdDirection / FdUp tego wezla NIE sluza do
          podawania czegokolwiek poza kierunkami, a wiec ich dlugosc jest
          niewazna. No i stosujac FdOrientation, a wiec uzywajac standardowego
          VRMLa 1.0, nie mozna nawet podac dlugosci FdDirection/FdUp innej
          niz 1.)
        @item(
          Normalizujemy tutaj bo w implementacji tej funkcji czesto wiemy
          ze nie trzeba normalizowac, np. gdy zwracamy standardowe dir/up
          kamery obrocone o orientation to nie potrzebujemy robic zadnej
          normalizacji bo wiemy ze wynik ma dlugosc 1. W ten sytuacji byloby
          nieoptymalne gdybys musial po wywolaniu tej procedury wywolac
          NormalizeTo1st(CamDir), bo przeciez czesto w ponizszej procedurze
          wiadomo ze nie trzea normalizowac (a wiec wywolanie NormalizeTo1st
          i wywolywany w jego srodku Sqrt sa zbedne).)
      )

      TODO: FocalDistance powinien tez byc tu zwracany (po przeliczeniu
      przez CamTransform) for TNodeGeneralVRML1Camera. }
    procedure GetCameraVectors(const CamTransform: TMatrix4Single;
      out CamPos, CamDir, CamUp, GravityUp: TVector3Single);
  end;

  TNodeGeneralViewpointClass = class of TNodeGeneralViewpoint;

  { GeneralCamera - wspolna klasa dla wszystkich kamer VRML'a. }
  TNodeGeneralVRML1Camera = class(TNodeGeneralViewpoint)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    property FdFocalDistance: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdHeightAngle: TSFFloat index 6 read GetFieldAsSFFloat;

    { Ignored fields -- they are not part of VRML 1.0 spec
      and I was not able to find any spec for them on the net.
      But some models ([http://www-vrl.umich.edu/sel_prj/EECS498/])
      use them. }
    property FdNearDistance: TSFFloat index 7 read GetFieldAsSFFloat;
    property FdFarDistance: TSFFloat index 8 read GetFieldAsSFFloat;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeOrthographicCamera = class(TNodeGeneralVRML1Camera)
    class function ClassNodeTypeName: string; override;
    class function CameraKind: TVRMLCameraKind; override;
  end;

  TNodePerspectiveCamera = class(TNodeGeneralVRML1Camera)
    class function ClassNodeTypeName: string; override;
    class function CameraKind: TVRMLCameraKind; override;
  end;

  TNodeGeneralLight = class(TVRMLNode)
  protected
    procedure MiddleTraverse(State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdOn: TSFBool index 0 read GetFieldAsSFBool;
    property FdIntensity: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdColor: TSFColor index 2 read GetFieldAsSFColor;
    property FdAmbientIntensity: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdKambiShadows: TSFBool index 4 read GetFieldAsSFBool;
    property FdKambiShadowsMain: TSFBool index 5 read GetFieldAsSFBool;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; virtual;
  end;

  TObjectsListItem_1 = TNodeGeneralLight;
  {$I objectslist_1.inc}
  TNodeGeneralLightsList = class(TObjectsList_1);

  TNodeGeneralDirectionalLight = class(TNodeGeneralLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdDirection: TSFVec3f index 6 read GetFieldAsSFVec3f;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; override;
  end;

  TNodeDirectionalLight_1 = class(TNodeGeneralDirectionalLight)
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeGeneralPositionalLight = class(TNodeGeneralLight)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property FdLocation: TSFVec3f index 6 read GetFieldAsSFVec3f;
    property FdAttenuation: TSFVec3f index 7 read GetFieldAsSFVec3f;

    { Attenuation obliczaja attenuation (tzn. wzorek 1/max( attenuation[0] + ...)
      zgodnie ze specyfik. VRMLa 97. Poniewaz obliczenie parametru DistanceToLight
      moze czasem wiazac sie ze spora strata czasu mozesz sprawdzic najpierw
      wartosc DistanceNeededForAttenuation --- jezeli jest false, to wartosc
      parametru DistanceToLight dla Attenutaion() nie ma znaczenia (mozesz
      podac cokolwiek).

      PAMIETAJ --- DistanceToLight powinien byc w lights coordinate system.
      TODO: raytracer nie realizuje teraz tego "PAMIETAJ" powyzej
      TODO: nie wiem czy OpenGL realizuje to "PAMIETAJ" powyzej
      w swoich swiatlach, check w OpenGL spec }
    function DistanceNeededForAttenuation: boolean;
    function Attenuation(const DistanceToLight: Single): Single; overload;
    function Attenuation(const DistanceToLight: Double): Double; overload;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; override;
  end;

  TNodeGeneralPointLight = class(TNodeGeneralPositionalLight)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
  end;

  TNodePointLight_1 = class(TNodeGeneralPointLight)
    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeSpotLight_1 = class(TNodeGeneralPositionalLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdDirection: TSFVec3f index 8 read GetFieldAsSFVec3f;
    property FdDropOffRate: TSFFloat index 9 read GetFieldAsSFFloat;
    property FdCutOffAngle: TSFFloat index 10 read GetFieldAsSFFloat;

    { nieznormalizowany wykladnik dla spot'a (na podstawie dropOffRate) }
    function SpotExp: Single;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; override;
  end;

  TNodeGroup_1 = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { A Group node that is added when VRML 1.0 file contains more than one root
    node. See comments at the beginning of this unit for more info. }
  TNodeGroupHidden_1 = class(TNodeGroup_1)
  public
    (*Save TNodeGroupHidden_* contents to stream.

      TNodeGroupHidden_* is saved to stream in a special way, so that actually
      only it's contents are written, without surrounding { } braces.
      This way, when saving, we "undo" the artificial wrapping in
      TNodeGroupHidden_* that was done by ParseVRMLStatements and ParseVRMLFile. *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);  override;
  end;

  { A general class that can ce used as a separator, something that
    pushes and pops all attribs and matrices.
    It is used in implementation of Separator and WWWAnchor.
    Also WWWInline does the same work, when it's "separate" field is true. }
  TNodeGeneralSeparator = class(TVRMLNode)
  private
    OriginalState: TVRMLGraphTraverseState;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeSeparator = class(TNodeGeneralSeparator)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdRenderCulling: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  TNodeSwitch_1 = class(TVRMLNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdWhichChild: TSFLong index 0 read GetFieldAsSFLong;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  TNodeTransformSeparator = class(TVRMLNode)
  private
    OriginalMatrix: TMatrix4Single;
    OriginalAverageScaleTransform: Single;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
  end;

  { TODO: node anchor dziala jak Separator, wartosci jego pol nie maja
    nigdzie zadnego znaczenia. Trzebaby to zaimplementowac, co wymaga
    1) TURLDataStream, patrz komentarz przy TNodeWWWInline
    2) mechaznimu picking - to juz mamy w view3dscene }
  TNodeWWWAnchor = class(TNodeGeneralSeparator)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdName: TSFString index 0 read GetFieldAsSFString;
    property FdDescription: TSFString index 0 read GetFieldAsSFString;
    property FdMap: TSFEnum index 0 read GetFieldAsSFEnum;
  end;

  INodeGeneralInline = interface
    { Call LoadInlined to load Inlined NOW. If Inlined is already loaded,
      than : if CanReload = true Inlined will be freed and loaded again,
      else (if CanReload = false) nothing will happen.

      LoadInlined(false) will be called automatically in BeforeTraverse. }
    procedure LoadInlined(CanReload: boolean);
  end;

  { gdy chcemy operowac na scenie juz po jej zaladowaniu, bezposrednio
    lub poprzez metode w rodzaju TVRMLNode.EnumerateNodes, jest istotne
    gdzie w hierarchii sceny znajduja sie Inlined nodes. Odpowiadam :
    sa one SubNode'ami WWWInline. Mozesz testowac ChildrenCount <> 0
    aby sprawdzic czy Inlined zostaly juz zaladowane. Mozesz
    zazadac ich natychmiastowego zaladowania uzywajac LoadInlined.

    TODO : naturalnie tylko FdName jako nazwa pliku na lokalnym
    systemie plikow jest obslugiwana chwilowo. W ogole, generalnie
    to fajnie byloby gdyby TVRMLScene.Create przyjmowalo URL a nie
    filename albo jeszcze lepiej gdyby miec strumien TURLDataStream
    ktory moze podawac dane identyfikowane przez URL...
    Moze nie w najblizszym czasie, ale zamierzam cos takiego kiedys
    zaimplementowac - szkielet (dla http) juz zrobilem w iswb. }
  TNodeWWWInline = class(TVRMLNode, INodeGeneralInline)
  private
    OriginalState: TVRMLGraphTraverseState;
    BeforeTraversePushedState: boolean;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  public
    procedure LoadInlined(CanReload: boolean);

    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdName: TSFString index 0 read GetFieldAsSFString;
    property FdBboxSize: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property FdBboxCenter: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdSeparate: TSFBool index 3 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;

    class function ChildrenSaveToStream: boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeGeneralShape_1.ForVRMLVersion(
  const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

constructor TNodeAsciiText_1.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..0]of string = ('');
      A2: array[0..2]of string = ('LEFT', 'CENTER', 'RIGHT');
      A3: array[0..0]of Single = (0);
begin
 inherited;
 Fields.Add(TMFString.Create('string', A1));
 Fields.Add(TSFFloat.Create('spacing', 1));
 Fields.Add(TSFEnum.Create('justification', A2, JUSTIFICATION_LEFT));
 Fields.Add(TMFFloat.Create('width', A3));
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
const A1: array[0..1]of string = ('SIDES', 'BOTTOM');
begin
 inherited;
 Fields.Add(TSFBitMask.Create('parts', A1, 'NONE', 'ALL', [true, true]));
 Fields.Add(TSFFloat.Create('bottomRadius', 1, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
end;

class function TNodeCone_1.ClassNodeTypeName: string;
begin
 result := 'Cone';
end;

constructor TNodeCube_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFFloat.Create('width', 2, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
 Fields.Add(TSFFloat.Create('depth', 2, true));
end;

class function TNodeCube_1.ClassNodeTypeName: string;
begin
 result := 'Cube';
end;

constructor TNodeCylinder_1.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..2]of string = ('SIDES', 'TOP', 'BOTTOM');
begin
 inherited;
 Fields.Add(TSFBitMask.Create('parts', A1, 'NONE', 'ALL', [true, true, true]));
 Fields.Add(TSFFloat.Create('radius', 1, true));
 Fields.Add(TSFFloat.Create('height', 2, true));
end;

class function TNodeCylinder_1.ClassNodeTypeName: string;
begin
 result := 'Cylinder';
end;

constructor TNodeGeneralIndexed_1.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..0]of Longint = (0);
      A2: array[0..0]of Longint = (-1);
begin
 inherited;
 Fields.Add(TMFLong.CreateMFLong('coordIndex', A1, true));
 Fields.Add(TMFLong.Create('materialIndex', A2));
 Fields.Add(TMFLong.Create('normalIndex', A2));
 Fields.Add(TMFLong.CreateMFLong('textureCoordIndex', A2, true));
end;

class function TNodeIndexedFaceSet_1.ClassNodeTypeName: string;
begin
 result := 'IndexedFaceSet';
end;

class function TNodeIndexedLineSet_1.ClassNodeTypeName: string;
begin
 result := 'IndexedLineSet';
end;

constructor TNodePointSet_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('startIndex', 0));
 Fields.Add(TSFLong.Create('numPoints', -1));
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
   if numPoints >= 0 then numPoints := numPoints+startIndex;
   startIndex := 0;
  end;

  {startIndex juz jest na pewno dobry, teraz ew. popraw numPoints}
  if numPoints >= 0 then
  begin
   if startIndex+numPoints > LastCoordinate3.FdPoint.Count then
    numPoints := LastCoordinate3.FdPoint.Count-startIndex;
  end else
   numPoints := LastCoordinate3.FdPoint.Count-startIndex;
 end;
end;

constructor TNodeSphere_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFFloat.Create('radius', 1, true));
end;

class function TNodeSphere_1.ClassNodeTypeName: string;
begin
 result := 'Sphere';
end;

constructor TNodeCoordinate3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFVec3f.Create('point', [Vector3Single(0, 0, 0)]));
end;

class function TNodeCoordinate3.ClassNodeTypeName: string;
begin
 result := 'Coordinate3';
end;

constructor TNodeFontStyle_1.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..2]of string = ('SERIF', 'SANS', 'TYPEWRITER');
      A2: array[0..1]of string = ('BOLD', 'ITALIC');
begin
 inherited;
 Fields.Add(TSFFloat.Create('size', 10, true));
 Fields.Add(TSFEnum.Create('family', A1, FSFAMILY_SERIF));
 Fields.Add(TSFBitMask.Create('style', A2, 'NONE', '', [false, false]));
end;

class function TNodeFontStyle_1.ClassNodeTypeName: string;
begin
 result := 'FontStyle';
end;

const
  TTF_Font_Results: array[TVRMLFontFamily, boolean, boolean]of PTrueTypeFont =
  (              {   [],                          [italic],                            [bold],                      [italic, bold] }
    {serif}      ( ((@TTF_BitstreamVeraSerif),   (@TTF_BitstreamVeraSerif_Italic)),    ((@TTF_BitstreamVeraSerif_Bold),    (@TTF_BitstreamVeraSerif_Bold_Italic)) ),
    {sans}       ( ((@TTF_BitstreamVeraSans),    (@TTF_BitstreamVeraSans_Italic)),     ((@TTF_BitstreamVeraSans_Bold),     (@TTF_BitstreamVeraSans_Bold_Italic)) ),
    {typewriter} ( ((@TTF_BitstreamVeraSansMono),(@TTF_BitstreamVeraSansMono_Italic)), ((@TTF_BitstreamVeraSansMono_Bold), (@TTF_BitstreamVeraSansMono_Bold_Italic)) )
  );

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

constructor TNodeInfo.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFString.Create('string', '<Undefined info>'));
end;

class function TNodeInfo.ClassNodeTypeName: string;
begin
 result := 'Info';
end;

constructor TNodeLOD_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFFloat.Create('range',[]));
 Fields.Add(TSFVec3f.Create('center', Vector3Single(0, 0, 0)));
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

constructor TNodeMaterial_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFColor.Create('ambientColor', [DefaultMaterial_1AmbientColor]));
 Fields.Add(TMFColor.Create('diffuseColor', [DefaultMaterialDiffuseColor]));
 Fields.Add(TMFColor.Create('specularColor', [DefaultMaterialSpecularColor]));
 Fields.Add(TMFColor.Create('emissiveColor', [DefaultMaterialEmissiveColor]));
 Fields.Add(TMFFloat.Create('shininess', [DefaultMaterialShininess]));
 Fields.Add(TMFFloat.Create('transparency', [DefaultMaterialTransparency]));

 Fields.Add(TMFFloat.Create('mirror', [DefaultMaterialMirror]));
 Fields.Add(TMFColor.Create('reflSpecular', []));
 Fields.Add(TMFColor.Create('reflDiffuse', []));
 Fields.Add(TMFColor.Create('transSpecular', []));
 Fields.Add(TMFColor.Create('transDiffuse', []));
 Fields.Add(TMFFloat.Create('reflSpecularExp', [DefaultMaterialReflSpecularExp]));
 Fields.Add(TMFFloat.Create('transSpecularExp', [DefaultMaterialTransSpecularExp]));

 Fields.Add(TSFBool.Create('fogImmune', false));
end;

class function TNodeMaterial_1.ClassNodeTypeName: string;
begin
 result := 'Material';
end;

{functions below return MatNum Material property. If there doesn't exist
   enouch properties defined, thay return the last defined. This is useful :
   for example you can give one ambient, specular, emissive color and
   define multiple diffuseColors and then you can use multiple materials
   without defining multiple values for each field.
 VRML specification doesn't state clearly what to do when thare are not enouch
   material properties - there were some idea of "cycling" mechanism but was
   later deprecated. So I use the mechanism below - returning to the last defined
   property number. }

{$define MATERIAL_FUNCTION_3_SINGLE:=
function TNodeMaterial_1.MATERIAL_FUNCTION_NAME_3(MatNum: integer): TVector3Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_DEFAULT else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[
    min(MatNum, MATERIAL_FUNCTION_FIELD.Count-1)];
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
 result := 1-Transparency(MatNum);
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

 {zgodnie ze specyfikacja VRML'a zakres 0..1 shininess VRML'a mapuje sie
  jednostajnie na caly zakres 0..128 OpenGL'a. Super. Robimy clamp
  na wszelki zas, na wypadek gdybysmy na skutek jakichs bledow nieco wyszli
  za zakres albo gdyby ktos zapisale zla wartosc w pliku (np. ujemna).

  A jezeli odczytana wartosc jest > 2 to zakladamy ze jakis
  kretyn zapisal nieznormalizowane wartosci Shininess w pliku
  (tacy sie zdarzaja, patrz helix.wrl).
  Pisze kretyn a mysle duzo gorzej bo jestem naprawde wkurzony - coraz
  wiecej musze w kodzie wprowadzac poprawek zeby odczytywac niepoprawnie
  zapisane VRMLe. A to rotacja wokol wektora (0, 0, 0) (program "Pioneer")
  a to nieznormalizowany Shininess... }
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

{ cztery funkcje ktore w razie braku wartosci zapisanych w polu (FdXxx.Count = 0)
  wyliczaja sobie kolor z innych wlasciwosci materialu. }
{$define MATERIAL_FUNCTION_CALC:=
function TNodeMaterial_1.MATERIAL_FUNCTION_NAME(MatNum: integer): TVector3Single;
begin
 if MATERIAL_FUNCTION_FIELD.Count = 0 then
  result := MATERIAL_FUNCTION_CALCULATE else
  result := MATERIAL_FUNCTION_FIELD.Items.Items[min(MatNum,
    MATERIAL_FUNCTION_FIELD.Count-1)]
end;}

  {$define MATERIAL_FUNCTION_NAME := ReflSpecular}
  {$define MATERIAL_FUNCTION_FIELD := FdReflSpecular}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    Vector3Single(Mirror(MatNum), Mirror(MatNum), Mirror(MatNum))}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := ReflDiffuse}
  {$define MATERIAL_FUNCTION_FIELD := FdReflDiffuse}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    DiffuseColor3Single(MatNum)}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := TransSpecular}
  {$define MATERIAL_FUNCTION_FIELD := FdTransSpecular}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    Vector3Single(Transparency(MatNum), Transparency(MatNum), Transparency(MatNum))}
  MATERIAL_FUNCTION_CALC

  {$define MATERIAL_FUNCTION_NAME := TransDiffuse}
  {$define MATERIAL_FUNCTION_FIELD := FdTransDiffuse}
  {$define MATERIAL_FUNCTION_CALCULATE:=
    VectorScale(DiffuseColor3Single(MatNum), Transparency(MatNum) )}
  MATERIAL_FUNCTION_CALC

{$undef MATERIAL_FUNCTION_CALC}
{$undef MATERIAL_FUNCTION_NAME}
{$undef MATERIAL_FUNCTION_FIELD}
{$undef MATERIAL_FUNCTION_CALCULATE}

function TNodeMaterial_1.IsAllMaterialsTransparent: boolean;
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

constructor TNodeMaterialBinding.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..7]of string = ('DEFAULT', 'OVERALL',
  'PER_PART', 'PER_PART_INDEXED',
  'PER_FACE', 'PER_FACE_INDEXED',
  'PER_VERTEX', 'PER_VERTEX_INDEXED');
begin
 inherited;
 Fields.Add(TSFEnum.Create('value', A1, 1));
end;

class function TNodeMaterialBinding.ClassNodeTypeName: string;
begin
 result := 'MaterialBinding';
end;

constructor TNodeNormalBinding.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..7]of string = ('DEFAULT', 'OVERALL',
  'PER_PART', 'PER_PART_INDEXED',
  'PER_FACE', 'PER_FACE_INDEXED',
  'PER_VERTEX', 'PER_VERTEX_INDEXED');
begin
 inherited;
 Fields.Add(TSFEnum.Create('value', A1, 0));
end;

class function TNodeNormalBinding.ClassNodeTypeName: string;
begin
 result := 'NormalBinding';
end;

constructor TNodeTexture2.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..1]of string = ('REPEAT', 'CLAMP');
begin
 inherited;
 Fields.Add(TSFString.Create('filename', ''));
 Fields.Add(TSFImage.Create('image', nil));
 Fields.Add(TSFEnum.Create('wrapS', A1, TEXWRAP_REPEAT));
 Fields.Add(TSFEnum.Create('wrapT', A1, TEXWRAP_REPEAT));
 Fields.Add(TSFEnum.Create('model', ['DECAL'], 0));
 Fields.Add(TSFVec3f.Create('blendColor', Vector3Single(0, 0, 0)));
end;

class function TNodeTexture2.ClassNodeTypeName: string;
begin
 result := 'Texture2';
end;

function TNodeTexture2.LoadTextureImage(out CacheUsed: boolean): TImage;
var
  FullUrl: string;
begin
  Result := nil;
  CacheUsed := false;

  { sprobuj zaladowac teksture z pliku FdFilename }
  if FdFilename.Value <> '' then
  try
    FullUrl := PathFromWWWBasePath(FdFilename.Value);
    Result := ImagesCache.LoadImage_IncReference(FullUrl);
    CacheUsed := true;
    Exit;
  except
    on E: Exception do
      { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
        (chociaz nie musi) }
      VRMLNonFatalError('Exception ' + E.ClassName +
        ' occured when trying to load '+
        'texture from filename "' + FullUrl + '" : ' + E.Message);
  end;

  { Still not loaded (so FdFilename.Value is '' or LoadImage raised exception) ?
    So try to use inlined texture. }
  if not FdImage.Value.IsNull then
    Result := FdImage.Value.MakeCopy;
end;

function TNodeTexture2.TextureDescription: string;

  function InlinedDescr: string;
  begin
    result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value is TAlphaImage] ]);
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

constructor TNodeTexture2Transform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec2f.Create('translation', Vector2Single(0, 0)));
 Fields.Add(TSFFloat.Create('rotation', 0));
 Fields.Add(TSFVec2f.Create('scaleFactor', Vector2Single(1, 1)));
 Fields.Add(TSFVec2f.Create('center', Vector2Single(0, 0)));
end;

class function TNodeTexture2Transform.ClassNodeTypeName: string;
begin
 result := 'Texture2Transform';
end;

function TNodeTexture2Transform.TextureMatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix( Vector3Single(
   VectorAdd(FdTranslation.Value, FdCenter.Value) ));
 result := MultMatrices(result, RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
 result := MultMatrices(result, ScalingMatrix(
   Vector3Single( FdScaleFactor.Value[0], FdScaleFactor.Value[1], 1 )));
 result := MultMatrices(result, TranslationMatrix(
   Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

procedure TNodeTexture2Transform.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
 inherited;
 State.CurrTextureMatrix := MultMatrices(State.CurrTextureMatrix,
   TextureMatrixTransformation);
end;

constructor TNodeTextureCoordinate2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TMFVec2f.Create('point', [Vector2Single(0, 0)]));
end;

class function TNodeTextureCoordinate2.ClassNodeTypeName: string;
begin
 result := 'TextureCoordinate2';
end;

constructor TNodeShapeHints.Create(const ANodeName: string;
  const AWWWBasePath: string);
const
  A1: array[0..2]of string = ('UNKNOWN_ORDERING', 'CLOCKWISE', 'COUNTERCLOCKWISE');
  A2: array[0..1]of string = ('UNKNOWN_SHAPE_TYPE', 'SOLID');
  A3: array[0..1]of string = ('UNKNOWN_FACE_TYPE', 'CONVEX');
begin
 inherited;
 Fields.Add(TSFEnum.Create('vertexOrdering', A1, VERTORDER_UNKNOWN));
 Fields.Add(TSFEnum.Create('shapeType', A2, SHTYPE_UNKNOWN));
 Fields.Add(TSFEnum.Create('faceType', A3, FACETYPE_CONVEX));
 Fields.Add(TSFFloat.Create('creaseAngle', 0.5));
end;

class function TNodeShapeHints.ClassNodeTypeName: string;
begin
 result := 'ShapeHints';
end;

function TNodeShapeHints.ParseNodeBodyElement(Lexer: TVRMLLexer): boolean;
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
      Hints := TSFBitMask.Create('hints',
        ['SOLID', 'ORDERED', 'CONVEX'], 'NONE', '',
        [ false,   true,      true]);
      try
        Lexer.NextToken;
        Hints.Parse(Lexer, false);
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

procedure TNodeGeneralTransformation.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
  inherited;
  State.CurrMatrix := MultMatrices(State.CurrMatrix, MatrixTransformation);
  State.AverageScaleTransform *= AverageScaleTransform;
end;

constructor TNodeMatrixTransform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFMatrix.Create('matrix', IdentityMatrix4Single));
end;

class function TNodeMatrixTransform.ClassNodeTypeName: string;
begin
 result := 'MatrixTransform';
end;

function TNodeMatrixTransform.MatrixTransformation: TMatrix4Single;
begin
  Result := FdMatrix.Matrix;
end;

function TNodeMatrixTransform.AverageScaleTransform: Single;
begin
  { This is a simple method of extracting average scaling factor from
    a matrix. Works OK for combination of identity, scaling,
    translation matrices.
    Fails awfully on rotation (and possibly many other) matrices. }
  Result := ( FdMatrix.Matrix[0, 0] +
              FdMatrix.Matrix[1, 1] +
              FdMatrix.Matrix[2, 2] ) / 3;
end;

constructor TNodeRotation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0));
end;

class function TNodeRotation.ClassNodeTypeName: string;
begin
 result := 'Rotation';
end;

function TNodeRotation.MatrixTransformation: TMatrix4Single;
begin
  { We check for dumb zero rotation vector, program "Pioneer" could
    write something like this to VRML. }
  if IsZeroVector(FdRotation.Axis) then
    result := IdentityMatrix4Single else
    { RotationMatrixRad, just like glRotate, has rotations defined in the same way
      (direction) as VRML, so it's all OK here. }
    result := RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis);
end;

function TNodeRotation.AverageScaleTransform: Single;
begin
  Result := 1;
end;

constructor TNodeScale.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('scaleFactor', Vector3Single(1, 1, 1)));
end;

class function TNodeScale.ClassNodeTypeName: string;
begin
 result := 'Scale';
end;

function TNodeScale.MatrixTransformation: TMatrix4Single;
begin
 result := ScalingMatrix(FdScaleFactor.Value);
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
 Fields.Add(TSFVec3f.Create('translation', Vector3Single(0, 0, 0)));
 Fields.Add(TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0));
 Fields.Add(TSFVec3f.Create('scaleFactor', Vector3Single(1, 1, 1)));
 Fields.Add(TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0));
 Fields.Add(TSFVec3f.Create('center', Vector3Single(0, 0, 0)));
end;

class function TNodeTransform_1.ClassNodeTypeName: string;
begin
 result := 'Transform';
end;

function TNodeTransform_1.MatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix(FdTranslation.Value);
 result := MultMatrices(result, TranslationMatrix(FdCenter.Value));
 if not IsZeroVector(FdRotation.Axis) then
  result := MultMatrices(result,
    RotationMatrixRad(FdRotation.RotationRad, FdRotation.Axis));
 result := MultMatrices(result,
   RotationMatrixRad(FdScaleOrientation.RotationRad, FdScaleOrientation.Axis));
 result := MultMatrices(result, ScalingMatrix(FdScaleFactor.Value));
 result := MultMatrices(result,
   RotationMatrixRad(-FdScaleOrientation.RotationRad, FdScaleOrientation.Axis));
 result := MultMatrices(result, TranslationMatrix(VectorNegate(FdCenter.Value)));
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
 Fields.Add(TSFVec3f.Create('translation', Vector3Single(0, 0, 0)));
end;

class function TNodeTranslation.ClassNodeTypeName: string;
begin
 result := 'Translation';
end;

function TNodeTranslation.MatrixTransformation: TMatrix4Single;
begin
 result := TranslationMatrix(FdTranslation.Value);
end;

function TNodeTranslation.AverageScaleTransform: Single;
begin
  Result := 1;
end;

constructor TNodeGeneralViewpoint.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFVec3f.Create('position', Vector3Single(0, 0, 1)));
  Fields.Add(TSFRotation.Create('orientation', Vector3Single(0, 0, 1), 0));
  Fields.Add(TMFVec3f.Create('direction', []));
  Fields.Add(TMFVec3f.Create('up', []));
  Fields.Add(TSFVec3f.Create('gravityUp', StdVRMLGravityUp));
end;

procedure TNodeGeneralViewpoint.GetCameraVectors(
  const CamTransform: TMatrix4Single;
  out CamPos, CamDir, CamUp, GravityUp: TVector3Single);
begin
  CamPos := FdPosition.Value;
  if FdDirection.Items.Length > 0 then
    CamDir := FdDirection.Items.Items[0] else
    CamDir := FdOrientation.RotatedPoint( StdVRMLCamDir );
  if FdUp.Items.Length > 0 then
    CamUp := FdUp.Items.Items[0] else
    CamUp := FdOrientation.RotatedPoint( StdVRMLCamUp );
  GravityUp := FdGravityUp.Value;

  { Niestety, macierz ponizej moze cos skalowac wiec nawet jesli powyzej
    uzylismy FdOrientation.RotatedPoint( StdVRMLCamDir/Up ) i wiemy ze CamDir/Up
    jest znormalizowane - to i tak musimy je tutaj znormalizowac.
    TODO: byloby dobrze uzyc tutaj czegos jak MultMatrixPointNoTranslationNoScale }
  CamPos := MultMatrixPoint(CamTransform, CamPos);
  CamDir := Normalized( MultMatrixPointNoTranslation(CamTransform, CamDir) );
  CamUp := Normalized( MultMatrixPointNoTranslation(CamTransform, CamUp) );
  GravityUp := Normalized( MultMatrixPointNoTranslation(CamTransform, GravityUp) );

  Assert(FloatsEqual(VectorLenSqr(CamDir), 1.0, 0.0001));
  Assert(FloatsEqual(VectorLenSqr(CamUp), 1.0, 0.0001));
end;

constructor TNodeGeneralVRML1Camera.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFFloat.Create('focalDistance', 5, true));
  Fields.Add(TSFFloat.Create('heightAngle', 0.785398, true));
  Fields.Add(TSFFloat.Create('nearDistance', 0));
  Fields.Add(TSFFloat.Create('farDistance', 0));
end;

class function TNodeOrthographicCamera.ClassNodeTypeName: string;
begin
 result := 'OrthographicCamera';
end;

class function TNodeOrthographicCamera.CameraKind: TVRMLCameraKind;
begin
 result := ckOrthographic;
end;

class function TNodePerspectiveCamera.ClassNodeTypeName: string;
begin
 result := 'PerspectiveCamera';
end;

class function TNodePerspectiveCamera.CameraKind: TVRMLCameraKind;
begin
 result := ckPerspective;
end;

constructor TNodeGeneralLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFBool.Create('on', true)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('intensity', 1)); Fields.Last.Exposed := true;
  Fields.Add(TSFColor.Create('color', Vector3Single(1, 1, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('ambientIntensity', -1)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('kambiShadows', false)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('kambiShadowsMain', false)); Fields.Last.Exposed := true;
end;

function TNodeGeneralLight.CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result.LightNode := Self;
  Result.Transform := State.CurrMatrix;
  Result.AverageScaleTransform := State.AverageScaleTransform;
end;

procedure TNodeGeneralLight.MiddleTraverse(State: TVRMLGraphTraverseState);
begin
  inherited;
  State.VRML1ActiveLights.AppendItem(CreateActiveLight(State));
end;

constructor TNodeGeneralPositionalLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFVec3f.Create('location', Vector3Single(0, 0, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('attenuation', Vector3Single(1, 0, 0))); Fields.Last.Exposed := true;
end;

function TNodeGeneralPositionalLight.DistanceNeededForAttenuation: boolean;
begin
  Result := (FdAttenuation.Value[1] > 0) or (FdAttenuation.Value[2] > 0);
end;

function TNodeGeneralPositionalLight.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result := inherited;

  Result.TransfLocation := MultMatrixPoint(Result.Transform,
    FdLocation.Value);
end;

{$define ATTENUATION_IMPLEMENTATION:=
begin
 (* moglibysmy tu nie badac czy DistanceNeededForAttenuation i zawsze
    robic wersje pelna (bo przeciez
      FdAttenuation.Value[1] * DistanceToLight +
      FdAttenuation.Value[2] * Sqr(DistanceToLight)
    i tak bedzie = 0 gdy FdAttenuation.Value[1] = FdAttenuation.Value[2] = 0.
    Ale wydaje mi sie ze tak jest szybciej - testowanie kosztuje nas
    troszke czasu ale mozemy sobie w ten sposob ocalic 2 x mnozenie i dodawanie. *)

 (* we check whether attenuation = (0, 0, 0). VRML 97 spec says that specifying
    (0, 0, 0) should be equal to specifying (1, 0, 0). (well, we avoid
    division by zero possibility this way so it's quite sensible, even
    if it wastes some time) *)
 if (FdAttenuation.Value[0] = 0) and
    (FdAttenuation.Value[1] = 0) and
    (FdAttenuation.Value[2] = 0) then result := 1;

 if DistanceNeededForAttenuation then
  result := 1/ KambiUtils.max(FdAttenuation.Value[0] +
                   FdAttenuation.Value[1] * DistanceToLight +
                   FdAttenuation.Value[2] * Sqr(DistanceToLight), Single(1.0)) else
  result := 1/ KambiUtils.max(FdAttenuation.Value[0], Single(1.0));
end;}

function TNodeGeneralPositionalLight.Attenuation(const DistanceToLight: Single): Single;
ATTENUATION_IMPLEMENTATION

function TNodeGeneralPositionalLight.Attenuation(const DistanceToLight: Double): Double;
ATTENUATION_IMPLEMENTATION

constructor TNodeGeneralDirectionalLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, -1))); Fields.Last.Exposed := true;
end;

class function TNodeGeneralDirectionalLight.ClassNodeTypeName: string;
begin
 result := 'DirectionalLight';
end;

function TNodeGeneralDirectionalLight.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result := inherited;
  Result.TransfNormDirection :=
    Normalized( MultMatrixPointNoTranslation(Result.Transform,
      FdDirection.Value) );
end;

class function TNodeDirectionalLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

constructor TNodeGeneralPointLight.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  { no new fields - this is just TNodeGeneralPositionalLight }
end;

class function TNodeGeneralPointLight.ClassNodeTypeName: string;
begin
  Result := 'PointLight';
end;

class function TNodePointLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

constructor TNodeSpotLight_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, -1)));
 Fields.Add(TSFFloat.Create('dropOffRate', 0));
 Fields.Add(TSFFloat.Create('cutOffAngle', 0.785398));
end;

class function TNodeSpotLight_1.ClassNodeTypeName: string;
begin
 result := 'SpotLight';
end;

function TNodeSpotLight_1.SpotExp: Single;
begin
 result := FdDropOffRate.Value*128.0;
end;

class function TNodeSpotLight_1.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor <= 1;
end;

function TNodeSpotLight_1.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result := inherited;
  Result.TransfNormDirection :=
    Normalized( MultMatrixPointNoTranslation(Result.Transform,
      FdDirection.Value) );
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

procedure TNodeGroupHidden_1.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
var
  I: integer;
begin
  { Special things for TNodeGroupHidden_1 saving:
    NodeName is ignored (should be '').
    Fields are ignored (should never have any meaning). }

  for I := 0 to Prototypes.Count - 1 do
    Prototypes[I].SaveToStream(SaveProperties);

  for i := 0 to ChildrenCount - 1 do
    Children[i].SaveToStream(SaveProperties);

  for I := 0 to Routes.Count - 1 do
    Routes[I].SaveToStream(SaveProperties);
end;

constructor TNodeGeneralSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 fParsingAllowedChildren := true;
 fAllowedChildren := true;
end;

procedure TNodeGeneralSeparator.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
 inherited;
 OriginalState := State;
 State := TVRMLGraphTraverseState.CreateCopy(OriginalState);
end;

procedure TNodeGeneralSeparator.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
 State.Free;
 State := OriginalState;
 inherited;
end;

constructor TNodeSeparator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFEnum.Create('renderCulling', ['ON', 'OFF', 'AUTO'], 2));
end;

class function TNodeSeparator.ClassNodeTypeName: string;
begin
 result := 'Separator';
end;

constructor TNodeSwitch_1.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('whichChild', -1));
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

procedure TNodeTransformSeparator.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;

  { nie robimy kopii calego State'a bo w TVRMLRenderState moga byc
    jeszcze inne informacje ktore powinny "przeciec" na zewnatrz
    TransformSeparator'a. }
  OriginalMatrix := State.CurrMatrix;
  OriginalAverageScaleTransform := State.AverageScaleTransform;
end;

procedure TNodeTransformSeparator.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
 State.CurrMatrix := OriginalMatrix;
 State.AverageScaleTransform := OriginalAverageScaleTransform;
 inherited;
end;

constructor TNodeWWWAnchor.Create(const ANodeName: string; const AWWWBasePath: string);
const A1: array[0..1]of string = ('NONE','POINT');
begin
 inherited;
 Fields.Add(TSFString.Create('name', ''));
 Fields.Add(TSFString.Create('description', ''));
 Fields.Add(TSFEnum.Create('map', A1, 0));
end;

class function TNodeWWWAnchor.ClassNodeTypeName: string;
begin
 result := 'WWWAnchor';
end;

constructor TNodeWWWInline.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFString.Create('name', ''));
 Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(0, 0, 0)));
 Fields.Add(TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0)));
 Fields.Add(TSFBool.Create('separate', true));

 fParsingAllowedChildren := false;
 fAllowedChildren := true;
end;

class function TNodeWWWInline.ClassNodeTypeName: string;
begin
 result := 'WWWInline';
end;

procedure TNodeWWWInline.LoadInlined(CanReload: boolean);
begin
 if ChildrenCount > 0 then
 begin
  if CanReload then RemoveAllChildren else exit;
 end;
 AddChild(LoadAsVRML(PathFromWWWBasePath(FdName.Value), false));
end;

procedure TNodeWWWInline.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;

  { We save here BeforeTraversePushedState, to be safe in case
    someone will change FdSeparate.Value between BeforeTraverse
    and AfterTraverse. }
  BeforeTraversePushedState := FdSeparate.Value;
  if BeforeTraversePushedState then
  begin
    OriginalState := State;
    State := TVRMLGraphTraverseState.CreateCopy(OriginalState);
  end;

  try
    LoadInlined(false);
  except
    if BeforeTraversePushedState then
    begin
      FreeAndNil(State);
      State := OriginalState;
    end;
    raise;
  end;
end;

procedure TNodeWWWInline.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
 if BeforeTraversePushedState then
 begin
   FreeAndNil(State);
   State := OriginalState;
 end;

 inherited;
end;

class function TNodeWWWInline.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;
{$endif read_implementation}
