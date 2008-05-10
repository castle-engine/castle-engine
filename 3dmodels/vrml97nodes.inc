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

{ This file defines all nodes from VRML 2.0 (aka 97) specification.
  Keep it alphabetically (aside from abstract classes
  that are only for easier implementation, like TNodeGeneralGrouping). } { }

{$ifdef read_interface}
  { This is a VRML 2.0 grouping node.
    This will push/pop full TVRMLGraphTraverseState in Before/AfterTraverse.
    It also propagates DirectionalLights in any child to all children
    in VRML2ActiceLights. }
  TNodeGeneralGrouping = class(TVRMLNode)
  private
    OriginalState: TVRMLGraphTraverseState;
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;
  end;

  TNodeX3DGroupingNode = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { eventIn      MFNode   addChildren } { }
    { eventIn      MFNode   removeChildren } { }
    property Fdchildren: TMFNode index 0 read GetFieldAsMFNode;
    property Fdmetadata: TSFNode index 1 read GetFieldAsSFNode;
    property FdbboxCenter: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdbboxSize: TSFVec3f index 3 read GetFieldAsSFVec3f;

    function ChildrenField: TMFNode; override;
  end;

  TNodeAnchor = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fddescription: TSFString index 4 read GetFieldAsSFString;
    property Fdparameter: TMFString index 5 read GetFieldAsMFString;
    property Fdurl: TMFString index 6 read GetFieldAsMFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeX3DShaderNode = class;
  TNodeComposedShader = class;

  TNodeAppearance = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdmaterial: TSFNode index 0 read GetFieldAsSFNode;
    property Fdtexture: TSFNode index 1 read GetFieldAsSFNode;
    property FdtextureTransform: TSFNode index 2 read GetFieldAsSFNode;

    property FdfillProperties: TSFNode index 3 read GetFieldAsSFNode;
    property FdlineProperties: TSFNode index 4 read GetFieldAsSFNode;
    property Fdmetadata: TSFNode index 5 read GetFieldAsSFNode;
    property Fdshaders: TMFNode index 6 read GetFieldAsMFNode;

    { This returns FdShaders.Items[Index], if it's a shader node suitable for GLSL
      shader. Otherwise returns @nil. This checks the class of
      FdShaders.Items[Index] and it's @code(language) field. }
    function GLSLShader(Index: Integer): TNodeComposedShader;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeAudioClip = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fddescription: TSFString index 0 read GetFieldAsSFString;
    property Fdloop: TSFBool index 1 read GetFieldAsSFBool;
    property Fdpitch: TSFFloat index 2 read GetFieldAsSFFloat;
    property FdstartTime: TSFTime index 3 read GetFieldAsSFTime;
    property FdstopTime: TSFTime index 4 read GetFieldAsSFTime;
    property Fdurl: TMFString index 5 read GetFieldAsMFString;
    { eventOut       SFTime   duration_changed } { }
    { eventOut       SFBool   isActive } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeBackground = class(TVRMLNode)
  private
    FBgImagesLoaded: boolean; { = false }

    { if not FBgImagesLoaded it should be always equal to BackgroundImagesNone
      (this is not important for interface of this class but we will use
      this in our implementation) }
    FBgImages: TBackgroundImages;

    { Non-nil only if FBgImages should be freed using
      UsedCache.LoadImage_DecReference. }
    UsedCache: TImagesCache;

    procedure SetBgImagesLoaded(const Value: boolean);

    FAllowedBgImagesClasses: TDynArrayImageClasses;
    function GetBgImages: TBackgroundImages;

    FImagesCache: TImagesCache;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;

    { eventIn      SFBool   set_bind } { }
    property FdGroundAngle: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdGroundColor: TMFColor index 1 read GetFieldAsMFColor;
    property FdBackUrl: TMFString index 2 read GetFieldAsMFString;
    property FdBottomUrl: TMFString index 3 read GetFieldAsMFString;
    property FdFrontUrl: TMFString index 4 read GetFieldAsMFString;
    property FdLeftUrl: TMFString index 5 read GetFieldAsMFString;
    property FdRightUrl: TMFString index 6 read GetFieldAsMFString;
    property FdTopUrl: TMFString index 7 read GetFieldAsMFString;
    property FdSkyAngle: TMFFloat index 8 read GetFieldAsMFFloat; {  [0, Pi] }
    property FdSkyColor: TMFColor index 9 read GetFieldAsMFColor; {  [0, 1] }
    { eventOut     SFBool   isBound } { }

    procedure Parse(Lexer: TVRMLLexer); override;

    { Pierwsze uzycie BgImages (albo pierwsze uzycie BgImages po Parse)
      automatycznie zaladuje obrazki z URLi
      BackUrl/BottomUrl itd. Obrazki dla ktorych zaden osiagalny URL nie byl
      podany zostana ustawione na ImageNone.
      Obrazki beda mialy Kind wsrod AllowedBgImagesKinds.

      Obrazki beda zawsze odpowiadaly URLom w polach Fd*Url - dlatego po
      zrobieniu Parse na tym obiekcie nastepne wywolanie BgImages bedzie
      ladowalo je od nowa. }
    property BgImages: TBackgroundImages read GetBgImages;

    { Czy obrazki juz sa zaladowane ? W zasadzie ta funkcja nigdy nie powinna
      ci byc potrzebna skoro cale ladowanie / zwalnianie obrazkow jest
      robione automatycznie. Ale moze ci przydac : ona mowi czy najblizsze
      wywolanie BgImages wywola ReloadBgImages czy nie.

      You can set this to @true to force loading of images now, and you
      can set to this to @false to force release of background images now.
      In normal circumstances, you don't need it --- BgImages are automatically
      loaded on first use, and are automatically freed when necessary.

      If you're sure you want to reload the background images (e.g. when
      you suspect that images on the disk changed), you may force reload
      by setting this to @false and then to @true again. }
    property BgImagesLoaded: boolean read FBgImagesLoaded
      write SetBgImagesLoaded;

    { Wszystkie obrazki na BgImages (ktore nie sa nil) maja zawsze
      klase wsrod AllowedBgImagesClasses.

      Aby zapewnic ze to stwierdzenie zawsze zachodzi jezeli BgImages zostalo
      juz chociaz raz wywolane to zmiana AllowedBgImagesClasses spowoduje
      uniewaznienie BgImages (tzn. nastepne wywolanie BgImages bedzie
      musialo przeladowac obrazki od nowa) }
    property AllowedBgImagesClasses: TDynArrayImageClasses
      read FAllowedBgImagesClasses; { = [], so all image classes are allowed }
    procedure SetAllowedBgImagesClasses(const Value: array of TImageClass);

    { Set this to non-nil before background images are loaded, this way bacground
      images will be loaded from this cache.

      They will also be freed using this
      cache then (so remember that used cache must remain valid for the lifetime
      of this node, or you have to call BgImagesLoaded := false explicitly to
      break the connection from this node to the cache). }
    property ImagesCache: TImagesCache read FImagesCache write FImagesCache;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeBillboard = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdaxisOfRotation: TSFVec3f index 4 read GetFieldAsSFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeBox = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdsize: TSFVec3f index 0 read GetFieldAsSFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  { X3DSensorNode when it's a secondary ancestor.
    (When it's primary ancestor, I use class TNodeX3DSensorNode). }
  INodeX3DSensorNode = interface
  end;

  TNodeCollision = class(TNodeX3DGroupingNode, INodeX3DSensorNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdcollide: TSFBool index 4 read GetFieldAsSFBool;
    property Fdproxy: TSFNode index 5 read GetFieldAsSFNode;
    { eventOut     SFTime   collideTime } { }

    { This should actually come from ancestor INodeX3DSensorNode } { }
    property Fdenabled: TSFBool index 6 read GetFieldAsSFBool;
    { property FdisActive: TSFBool ([out]) } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeColor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcolor: TMFColor index 0 read GetFieldAsMFColor;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeColorInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFColor index 1 read GetFieldAsMFColor;
    { eventOut     SFColor value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCone_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdbottomRadius: TSFFloat index 0 read GetFieldAsSFFloat;
    property Fdheight: TSFFloat index 1 read GetFieldAsSFFloat;
    property Fdside: TSFBool index 2 read GetFieldAsSFBool;
    property Fdbottom: TSFBool index 3 read GetFieldAsSFBool;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeContour2D = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      MFNode  addChildren } { }
    { eventIn      MFNode  removeChildren } { }
    property Fdchildren: TMFNode index 0 read GetFieldAsMFNode;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinate = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdpoint: TMFVec3f index 0 read GetFieldAsMFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateDeformer = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      MFNode   addChildren } { }
    { eventIn      MFNode   removeChildren } { }
    property Fdchildren: TMFNode index 0 read GetFieldAsMFNode;
    property FdcontrolPoint: TMFVec3f index 1 read GetFieldAsMFVec3f;
    property FdinputCoord: TMFNode index 2 read GetFieldAsMFNode;
    property FdinputTransform: TMFNode index 3 read GetFieldAsMFNode;
    property FdoutputCoord: TMFNode index 4 read GetFieldAsMFNode;
    property Fdweight: TMFFloat index 5 read GetFieldAsMFFloat;
    property FdbboxCenter: TSFVec3f index 6 read GetFieldAsSFVec3f;
    property FdbboxSize: TSFVec3f index 7 read GetFieldAsSFVec3f;
    property FduDimension: TSFInt32 index 8 read GetFieldAsSFInt32;
    property FduKnot: TMFFloat index 9 read GetFieldAsMFFloat;
    property FduOrder: TSFInt32 index 10 read GetFieldAsSFInt32;
    property FdvDimension: TSFInt32 index 11 read GetFieldAsSFInt32;
    property FdvKnot: TMFFloat index 12 read GetFieldAsMFFloat;
    property FdvOrder: TSFInt32 index 13 read GetFieldAsSFInt32;
    property FdwDimension: TSFInt32 index 14 read GetFieldAsSFInt32;
    property FdwKnot: TMFFloat index 15 read GetFieldAsMFFloat;
    property FdwOrder: TSFInt32 index 16 read GetFieldAsSFInt32;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFVec3f index 1 read GetFieldAsMFVec3f;
    { eventOut     MFVec3f value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCylinder_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdbottom: TSFBool index 0 read GetFieldAsSFBool;
    property Fdheight: TSFFloat index 1 read GetFieldAsSFFloat;
    property Fdradius: TSFFloat index 2 read GetFieldAsSFFloat;
    property Fdside: TSFBool index 3 read GetFieldAsSFBool;
    property Fdtop: TSFBool index 4 read GetFieldAsSFBool;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCylinderSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FddiskAngle: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdmaxAngle: TSFFloat index 4 read GetFieldAsSFFloat;
    property FdminAngle: TSFFloat index 5 read GetFieldAsSFFloat;
    property Fdoffset: TSFFloat index 6 read GetFieldAsSFFloat;
    property FdaxisRotation: TSFRotation index 7 read GetFieldAsSFRotation;
    { eventOut     SFRotation rotation_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeDirectionalLight_2 = class(TNodeGeneralDirectionalLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeElevationGrid = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      MFFloat  set_height } { }
    property Fdcolor: TSFNode index 0 read GetFieldAsSFNode;
    property Fdnormal: TSFNode index 1 read GetFieldAsSFNode;
    property FdtexCoord: TSFNode index 2 read GetFieldAsSFNode;
    property Fdheight: TMFFloat index 3 read GetFieldAsMFFloat;
    property Fdccw: TSFBool index 4 read GetFieldAsSFBool;
    property FdcolorPerVertex: TSFBool index 5 read GetFieldAsSFBool;
    property FdcreaseAngle: TSFFloat index 6 read GetFieldAsSFFloat;
    property FdnormalPerVertex: TSFBool index 7 read GetFieldAsSFBool;
    property Fdsolid: TSFBool index 8 read GetFieldAsSFBool;
    property FdxDimension: TSFInt32 index 9 read GetFieldAsSFInt32;
    property FdxSpacing: TSFFloat index 10 read GetFieldAsSFFloat;
    property FdzDimension: TSFInt32 index 11 read GetFieldAsSFInt32;
    property FdzSpacing: TSFFloat index 12 read GetFieldAsSFFloat;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { This checks whether xDimension and zDimension are >= 2,
      xSpacing and zSpacing are > 0 and height has at least the
      required number of values. If this returns @false then
      it is understood that ElevationGrid is not rendered, doesn't
      have any vertices/triangles etc. }
    function IsNotEmpty: boolean;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeExtrusion = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn MFVec2f    set_crossSection } { }
    { eventIn MFRotation set_orientation } { }
    { eventIn MFVec2f    set_scale } { }
    { eventIn MFVec3f    set_spine } { }
    property FdbeginCap: TSFBool index 0 read GetFieldAsSFBool;
    property Fdccw: TSFBool index 1 read GetFieldAsSFBool;
    property Fdconvex: TSFBool index 2 read GetFieldAsSFBool;
    property FdcreaseAngle: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdcrossSection: TMFVec2f index 4 read GetFieldAsMFVec2f;
    property FdendCap: TSFBool index 5 read GetFieldAsSFBool;
    property Fdorientation: TMFRotation index 6 read GetFieldAsMFRotation;
    property Fdscale: TMFVec2f index 7 read GetFieldAsMFVec2f;
    property Fdsolid: TSFBool index 8 read GetFieldAsSFBool;
    property Fdspine: TMFVec3f index 9 read GetFieldAsMFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeFog = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcolor: TSFColor index 0 read GetFieldAsSFColor;
    property FdfogType: TSFString index 1 read GetFieldAsSFString;
    property FdvisibilityRange: TSFFloat index 2 read GetFieldAsSFFloat;
    property FdVolumetric: TSFBool index 3 read GetFieldAsSFBool;
    property FdVolumetricDirection: TSFVec3f index 4 read GetFieldAsSFVec3f;
    property FdVolumetricVisibilityStart: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdAlternative: TSFNode index 6 read GetFieldAsSFNode;
    { eventIn      SFBool   set_bind } { }
    { eventOut     SFBool   isBound } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Returns FdAlternative.Value already typecasted to TNodeFog.
      If FdAlternative.Value is not of TNodeFog class, returns nil
      (returns also nil when FdAlternative.Value is nil, obviously). }
    function Alternative: TNodeFog;
  end;

  TNodeFontStyle_2 = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdfamily: TMFString index 0 read GetFieldAsMFString;
    property Fdhorizontal: TSFBool index 1 read GetFieldAsSFBool;
    property Fdjustify: TMFString index 2 read GetFieldAsMFString;
    property Fdlanguage: TSFString index 3 read GetFieldAsSFString;
    property FdleftToRight: TSFBool index 4 read GetFieldAsSFBool;
    property Fdsize: TSFFloat index 5 read GetFieldAsSFFloat;
    property Fdspacing: TSFFloat index 6 read GetFieldAsSFFloat;
    property Fdstyle: TSFString index 7 read GetFieldAsSFString;
    property FdtopToBottom: TSFBool index 8 read GetFieldAsSFBool;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function Family: TVRMLFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function Justify: TVRMLFontJustify;
    function TTF_Font: PTrueTypeFont;

    class function DefaultSize: Single;
    class function DefaultSpacing: Single;
    class function DefaultFamily: TVRMLFontFamily;
    class function DefaultBold: boolean;
    class function DefaultItalic: boolean;
    class function DefaultJustify: TVRMLFontJustify;
    class function DefaultTTF_Font: PTrueTypeFont;

    class function ClassTTF_Font(AFamily: TVRMLFontFamily;
      const ABold, AItalic: boolean): PTrueTypeFont;
  end;

  TNodeGeoCoordinate = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdgeoOrigin: TSFNode index 0 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 1 read GetFieldAsMFString;
    property Fdpoint: TMFString index 2 read GetFieldAsMFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoElevationGrid = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn        MFFloat    set_height } { }
    { eventIn        SFFloat    set_yScale } { }
    property Fdcolor: TSFNode index 0 read GetFieldAsSFNode;
    property Fdnormal: TSFNode index 1 read GetFieldAsSFNode;
    property FdtexCoord: TSFNode index 2 read GetFieldAsSFNode;
    property Fdccw: TSFBool index 3 read GetFieldAsSFBool;
    property FdcolorPerVertex: TSFBool index 4 read GetFieldAsSFBool;
    property FdcreaseAngle: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdgeoOrigin: TSFNode index 6 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 7 read GetFieldAsMFString;
    property FdgeoGridOrigin: TSFString index 8 read GetFieldAsSFString;
    property Fdheight: TMFFloat index 9 read GetFieldAsMFFloat;
    property FdnormalPerVertex: TSFBool index 10 read GetFieldAsSFBool;
    property Fdsolid: TSFBool index 11 read GetFieldAsSFBool;
    property FdxDimension: TSFInt32 index 12 read GetFieldAsSFInt32;
    property FdxSpacing: TSFString index 13 read GetFieldAsSFString;
    property FdyScale: TSFFloat index 14 read GetFieldAsSFFloat;
    property FdzDimension: TSFInt32 index 15 read GetFieldAsSFInt32;
    property FdzSpacing: TSFString index 16 read GetFieldAsSFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoLocation = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdgeoCoords: TSFString index 4 read GetFieldAsSFString;
    property FdgeoOrigin: TSFNode index 5 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 6 read GetFieldAsMFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoLOD = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcenter: TSFString index 0 read GetFieldAsSFString;
    property Fdchild1Url: TMFString index 1 read GetFieldAsMFString;
    property Fdchild2Url: TMFString index 2 read GetFieldAsMFString;
    property Fdchild3Url: TMFString index 3 read GetFieldAsMFString;
    property Fdchild4Url: TMFString index 4 read GetFieldAsMFString;
    property FdgeoOrigin: TSFNode index 5 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 6 read GetFieldAsMFString;
    property Fdrange: TSFFloat index 7 read GetFieldAsSFFloat;
    property FdrootUrl: TMFString index 8 read GetFieldAsMFString;
    property FdrootNode: TMFNode index 9 read GetFieldAsMFNode;
    { eventOut   MFNode    children } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoMetadata = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fddata: TMFNode index 0 read GetFieldAsMFNode;
    property Fdsummary: TMFString index 1 read GetFieldAsMFString;
    property Fdurl: TMFString index 2 read GetFieldAsMFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoOrigin = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdgeoSystem: TMFString index 0 read GetFieldAsMFString;
    property FdgeoCoords: TSFString index 1 read GetFieldAsSFString;
    property FdrotateYUp: TSFBool index 2 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoPositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn   SFFloat   set_fraction } { }
    property FdgeoOrigin: TSFNode index 0 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 1 read GetFieldAsMFString;
    property Fdkey: TMFFloat index 2 read GetFieldAsMFFloat;
    property FdkeyValue: TMFString index 3 read GetFieldAsMFString;
    { eventOut  SFString  geovalue_changed } { }
    { eventOut  SFVec3f   value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoTouchSensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdenabled: TSFBool index 0 read GetFieldAsSFBool;
    property FdgeoOrigin: TSFNode index 1 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 2 read GetFieldAsMFString;
    { eventOut      SFVec3f   hitNormal_changed } { }
    { eventOut      SFVec3f   hitPoint_changed } { }
    { eventOut      SFVec2f   hitTexCoord_changed } { }
    { eventOut      SFString  hitGeoCoord_changed } { }
    { eventOut      SFBool    isActive } { }
    { eventOut      SFBool    isOver } { }
    { eventOut      SFTime    touchTime } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoViewpoint = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn        SFBool       set_bind } { }
    { eventIn        SFString     set_orientation } { }
    { eventIn        SFString     set_position } { }
    property FdfieldOfView: TSFFloat index 0 read GetFieldAsSFFloat;
    property Fdheadlight: TSFBool index 1 read GetFieldAsSFBool;
    property Fdjump: TSFBool index 2 read GetFieldAsSFBool;
    property FdnavType: TMFString index 3 read GetFieldAsMFString;
    property Fddescription: TSFString index 4 read GetFieldAsSFString;
    property FdgeoOrigin: TSFNode index 5 read GetFieldAsSFNode;
    property FdgeoSystem: TMFString index 6 read GetFieldAsMFString;
    property Fdorientation: TSFRotation index 7 read GetFieldAsSFRotation;
    property Fdposition: TSFString index 8 read GetFieldAsSFString;
    property FdspeedFactor: TSFFloat index 9 read GetFieldAsSFFloat;
    { eventOut       SFTime       bindTime } { }
    { eventOut       SFBool       isBound } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGroup_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGroupHidden_2 = class(TNodeGroup_2)
  public
    (*Save TNodeGroupHidden_* contents to stream.

      TNodeGroupHidden_* is saved to stream in a special way, so that actually
      only it's contents are written, without surrounding { } braces.
      This way, when saving, we "undo" the artificial wrapping in
      TNodeGroupHidden_* that was done by ParseVRMLStatements and ParseVRMLFile. *)
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);  override;
  end;

  TNodeImageTexture = class(TNodeGeneralTexture)
  protected
    function LoadTextureImage(out CacheUsed: boolean): TImage; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdurl: TMFString index 0 read GetFieldAsMFString;
    property FdrepeatS: TSFBool index 1 read GetFieldAsSFBool;
    property FdrepeatT: TSFBool index 2 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodeIndexedFaceSet_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn       MFInt32 set_colorIndex } { }
    { eventIn       MFInt32 set_coordIndex } { }
    { eventIn       MFInt32 set_normalIndex } { }
    { eventIn       MFInt32 set_texCoordIndex } { }
    property Fdcolor: TSFNode index 0 read GetFieldAsSFNode;
    property Fdcoord: TSFNode index 1 read GetFieldAsSFNode;
    property Fdnormal: TSFNode index 2 read GetFieldAsSFNode;
    property FdtexCoord: TSFNode index 3 read GetFieldAsSFNode;
    property Fdccw: TSFBool index 4 read GetFieldAsSFBool;
    property FdcolorIndex: TMFInt32 index 5 read GetFieldAsMFInt32;
    property FdcolorPerVertex: TSFBool index 6 read GetFieldAsSFBool;
    property Fdconvex: TSFBool index 7 read GetFieldAsSFBool;
    property FdcoordIndex: TMFInt32 index 8 read GetFieldAsMFInt32;
    property FdcreaseAngle: TSFFloat index 9 read GetFieldAsSFFloat;
    property FdnormalIndex: TMFInt32 index 10 read GetFieldAsMFInt32;
    property FdnormalPerVertex: TSFBool index 11 read GetFieldAsSFBool;
    property Fdsolid: TSFBool index 12 read GetFieldAsSFBool;
    property FdtexCoordIndex: TMFInt32 index 13 read GetFieldAsMFInt32;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeIndexedLineSet_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn       MFInt32 set_colorIndex } { }
    { eventIn       MFInt32 set_coordIndex } { }
    property Fdcolor: TSFNode index 0 read GetFieldAsSFNode;
    property Fdcoord: TSFNode index 1 read GetFieldAsSFNode;
    property FdcolorIndex: TMFInt32 index 2 read GetFieldAsMFInt32;
    property FdcolorPerVertex: TSFBool index 3 read GetFieldAsSFBool;
    property FdcoordIndex: TMFInt32 index 4 read GetFieldAsMFInt32;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeInline = class(TNodeGeneralGrouping, INodeGeneralInline)
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdurl: TMFString index 0 read GetFieldAsMFString;
    property FdbboxCenter: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property FdbboxSize: TSFVec3f index 2 read GetFieldAsSFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ChildrenSaveToStream: boolean; override;

    procedure LoadInlined(CanReload: boolean);
  end;

  TNodeInlineLoadControl = class(TNodeGeneralGrouping, INodeGeneralInline)
  protected
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdload: TSFBool index 0 read GetFieldAsSFBool;
    property Fdurl: TMFString index 1 read GetFieldAsMFString;
    property FdbboxCenter: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdbboxSize: TSFVec3f index 3 read GetFieldAsSFVec3f;
    { eventOut     MFNode    children } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function ChildrenSaveToStream: boolean; override;

    procedure LoadInlined(CanReload: boolean);
  end;

  TNodeLOD_2 = class(TNodeGeneralGrouping)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdlevel: TMFNode index 0 read GetFieldAsMFNode;
    property Fdcenter: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property Fdrange: TMFFloat index 2 read GetFieldAsMFFloat;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeMaterial_2 = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdambientIntensity: TSFFloat index 0 read GetFieldAsSFFloat;
    property FddiffuseColor: TSFColor index 1 read GetFieldAsSFColor;
    property FdemissiveColor: TSFColor index 2 read GetFieldAsSFColor;
    property Fdshininess: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdspecularColor: TSFColor index 4 read GetFieldAsSFColor;
    property Fdtransparency: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdFogImmune: TSFBool index 6 read GetFieldAsSFBool;
    property FdMirror: TSFFloat index 7 read GetFieldAsSFFloat;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Opacity is just a 1 - FdTransparency.Value.
      Defined for your comfort --- for
      OpenGL you will usually want to pass Opacity, not Transparency. }
    function Opacity: Single;

    { ShininessExp is just 128 * FdShininess.Value, this is the "real"
      exponent indicated by shininess field value.
      Defined for your comfort --- for any graphic library you will usually
      want to pass the "real" exponent given by this function, not just
      value of shininess field. }
    function ShininessExp: Single;
  end;

  TNodeMovieTexture = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdloop: TSFBool index 0 read GetFieldAsSFBool;
    property Fdspeed: TSFFloat index 1 read GetFieldAsSFFloat;
    property FdstartTime: TSFTime index 2 read GetFieldAsSFTime;
    property FdstopTime: TSFTime index 3 read GetFieldAsSFTime;
    property Fdurl: TMFString index 4 read GetFieldAsMFString;
    property FdrepeatS: TSFBool index 5 read GetFieldAsSFBool;
    property FdrepeatT: TSFBool index 6 read GetFieldAsSFBool;
    { eventOut     SFTime   duration_changed } { }
    { eventOut     SFBool   isActive } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNavigationInfo = class(TNodeX3DBindableNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdavatarSize: TMFFloat index 1 read GetFieldAsMFFloat;
    property Fdheadlight: TSFBool index 2 read GetFieldAsSFBool;
    property Fdspeed: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdtransitionTime: TSFTime index 4 read GetFieldAsSFTime;
    property FdtransitionType: TMFString index 5 read GetFieldAsMFString;
    property Fdtype: TMFString index 6 read GetFieldAsMFString;
    property FdvisibilityLimit: TSFFloat index 7 read GetFieldAsSFFloat;
    { SFBool   [out]    transitionComplete }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNormal = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdvector: TMFVec3f index 0 read GetFieldAsMFVec3f;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNormalInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFVec3f index 1 read GetFieldAsMFVec3f;
    { eventOut     MFVec3f value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsCurve = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdcontrolPoint: TMFVec3f index 0 read GetFieldAsMFVec3f;
    property Fdweight: TMFFloat index 1 read GetFieldAsMFFloat;
    property Fdtessellation: TSFInt32 index 2 read GetFieldAsSFInt32;
    property Fdknot: TMFFloat index 3 read GetFieldAsMFFloat;
    property Fdorder: TSFInt32 index 4 read GetFieldAsSFInt32;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsCurve2D = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdcontrolPoint: TMFVec2f index 0 read GetFieldAsMFVec2f;
    property Fdtessellation: TSFInt32 index 1 read GetFieldAsSFInt32;
    property Fdweight: TMFFloat index 2 read GetFieldAsMFFloat;
    property Fdknot: TMFFloat index 3 read GetFieldAsMFFloat;
    property Fdorder: TSFInt32 index 4 read GetFieldAsSFInt32;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsGroup = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdtessellationScale: TSFFloat index 4 read GetFieldAsSFFloat;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsPositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat  set_fraction } { }
    property Fddimension: TSFInt32 index 0 read GetFieldAsSFInt32;
    property FdkeyValue: TMFVec3f index 1 read GetFieldAsMFVec3f;
    property FdkeyWeight: TMFFloat index 2 read GetFieldAsMFFloat;
    property Fdknot: TMFFloat index 3 read GetFieldAsMFFloat;
    property Fdorder: TSFInt32 index 4 read GetFieldAsSFInt32;
    { eventOut     SFVec3f  value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdcontrolPoint: TMFVec3f index 0 read GetFieldAsMFVec3f;
    property FdtexCoord: TSFNode index 1 read GetFieldAsSFNode;
    property FduTessellation: TSFInt32 index 2 read GetFieldAsSFInt32;
    property FdvTessellation: TSFInt32 index 3 read GetFieldAsSFInt32;
    property Fdweight: TMFFloat index 4 read GetFieldAsMFFloat;
    property Fdccw: TSFBool index 5 read GetFieldAsSFBool;
    property Fdsolid: TSFBool index 6 read GetFieldAsSFBool;
    property FduDimension: TSFInt32 index 7 read GetFieldAsSFInt32;
    property FduKnot: TMFFloat index 8 read GetFieldAsMFFloat;
    property FduOrder: TSFInt32 index 9 read GetFieldAsSFInt32;
    property FdvDimension: TSFInt32 index 10 read GetFieldAsSFInt32;
    property FdvKnot: TMFFloat index 11 read GetFieldAsMFFloat;
    property FdvOrder: TSFInt32 index 12 read GetFieldAsSFInt32;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsTextureSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdcontrolPoint: TMFVec2f index 0 read GetFieldAsMFVec2f;
    property Fdweight: TMFFloat index 1 read GetFieldAsMFFloat;
    property FduDimension: TSFInt32 index 2 read GetFieldAsSFInt32;
    property FduKnot: TMFFloat index 3 read GetFieldAsMFFloat;
    property FduOrder: TSFInt32 index 4 read GetFieldAsSFInt32;
    property FdvDimension: TSFInt32 index 5 read GetFieldAsSFInt32;
    property FdvKnot: TMFFloat index 6 read GetFieldAsMFFloat;
    property FdvOrder: TSFInt32 index 7 read GetFieldAsSFInt32;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeOrientationInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat    set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFRotation index 1 read GetFieldAsMFRotation;
    { eventOut     SFRotation value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodePixelTexture = class(TNodeGeneralTexture)
  protected
    function LoadTextureImage(out CacheUsed: boolean): TImage; override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdimage: TSFImage index 0 read GetFieldAsSFImage;
    property FdrepeatS: TSFBool index 1 read GetFieldAsSFBool;
    property FdrepeatT: TSFBool index 2 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function TextureDescription: string; override;
    function RepeatS: boolean; override;
    function RepeatT: boolean; override;
  end;

  TNodePlaneSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdmaxPosition: TSFVec2f index 3 read GetFieldAsSFVec2f;
    property FdminPosition: TSFVec2f index 4 read GetFieldAsSFVec2f;
    property Fdoffset: TSFVec3f index 5 read GetFieldAsSFVec3f;
    property FdaxisRotation: TSFRotation index 6 read GetFieldAsSFRotation;
    { eventOut     SFVec3f translation_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodePointLight_2 = class(TNodeGeneralPointLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    property Fdradius: TSFFloat index 8 read GetFieldAsSFFloat;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; override;
  end;

  TNodePointSet_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcolor: TSFNode index 0 read GetFieldAsSFNode;
    property Fdcoord: TSFNode index 1 read GetFieldAsSFNode;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodePolyline2D = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdpoint: TMFVec2f index 0 read GetFieldAsMFVec2f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodePositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFVec3f index 1 read GetFieldAsMFVec3f;
    { eventOut     SFVec3f value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeProximitySensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcenter: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property Fdsize: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property Fdenabled: TSFBool index 2 read GetFieldAsSFBool;
    { eventOut     SFBool     isActive } { }
    { eventOut     SFVec3f    position_changed } { }
    { eventOut     SFRotation orientation_changed } { }
    { eventOut     SFTime     enterTime } { }
    { eventOut     SFTime     exitTime } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeScalarInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn      SFFloat set_fraction } { }
    property Fdkey: TMFFloat index 0 read GetFieldAsMFFloat;
    property FdkeyValue: TMFFloat index 1 read GetFieldAsMFFloat;
    { eventOut     SFFloat value_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TVRMLInterfaceDeclarationsList = class;

  TNodeScript = class(TVRMLNode)
  private
    FInterfaceDeclarations: TVRMLInterfaceDeclarationsList;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    property Fdurl: TMFString index 0 read GetFieldAsMFString;
    property FddirectOutput: TSFBool index 1 read GetFieldAsSFBool;
    property FdmustEvaluate: TSFBool index 2 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    property InterfaceDeclarations: TVRMLInterfaceDeclarationsList
      read FInterfaceDeclarations;

    function ParseNodeBodyElement(Lexer: TVRMLLexer): boolean; override;
    procedure SaveContentsToStream(SaveProperties: TVRMLSaveToStreamProperties);
      override;
  end;

  TNodeTextureTransform = class;

  TNodeShape = class(TVRMLNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdappearance: TSFNode index 0 read GetFieldAsSFNode;
    property Fdgeometry: TSFNode index 1 read GetFieldAsSFNode;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
    procedure AfterTraverse(var State: TVRMLGraphTraverseState); override;

    { This is a shortcut for FdAppearance.Value.
      If anything makes this impossible (Apperance field is NULL, or wrong class)
      then returns @nil. }
    function Appearance: TNodeAppearance;

    { This is a shortcut for FdAppearance.Value.FdTexture.Value.
      If anything makes this impossible (Apperance field is NULL,
      or Appearance.Texture field is NULL, or wrong node class is
      passed as Appearance or Texture node), then returns nil. }
    function Texture: TNodeGeneralTexture;

    { This is like @link(Texture), but it returns normalMap field
      of KambiAppearance. }
    function NormalMap: TNodeGeneralTexture;

    { This is like @link(Texture), but it returns heightMap field
      of KambiAppearance. }
    function HeightMap: TNodeGeneralTexture;

    function HeightMapScale: Single;

    { This is like @link(Texture), but it returns TextureTransform
      of Apperance. }
    function TextureTransform: TNodeTextureTransform;

    { This is like @link(Texture), but it returns Material
      of Apperance. }
    function Material: TNodeMaterial_2;
  end;

  TNodeSound = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fddirection: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property Fdintensity: TSFFloat index 1 read GetFieldAsSFFloat;
    property Fdlocation: TSFVec3f index 2 read GetFieldAsSFVec3f;
    property FdmaxBack: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdmaxFront: TSFFloat index 4 read GetFieldAsSFFloat;
    property FdminBack: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdminFront: TSFFloat index 6 read GetFieldAsSFFloat;
    property Fdpriority: TSFFloat index 7 read GetFieldAsSFFloat;
    property Fdsource: TSFNode index 8 read GetFieldAsSFNode;
    property Fdspatialize: TSFBool index 9 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSphere_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdradius: TSFFloat index 0 read GetFieldAsSFFloat;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeSphereSensor = class(TNodeX3DDragSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdoffset: TSFRotation index 3 read GetFieldAsSFRotation;
    { eventOut     SFRotation rotation_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSpotLight_2 = class(TNodeGeneralPositionalLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdbeamWidth: TSFFloat index 8 read GetFieldAsSFFloat;
    property FdcutOffAngle: TSFFloat index 9 read GetFieldAsSFFloat;
    property Fddirection: TSFVec3f index 10 read GetFieldAsSFVec3f;
    property Fdradius: TSFFloat index 11 read GetFieldAsSFFloat;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function CreateActiveLight(State: TVRMLGraphTraverseState): TActiveLight; override;
  end;

  TNodeSwitch_2 = class(TNodeGeneralGrouping)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdchoice: TMFNode index 0 read GetFieldAsMFNode;
    property FdwhichChoice: TSFInt32 index 1 read GetFieldAsSFInt32;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeText = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdstring: TMFString index 0 read GetFieldAsMFString;
    property FdfontStyle: TSFNode index 1 read GetFieldAsSFNode;
    property Fdlength: TMFFloat index 2 read GetFieldAsMFFloat;
    property FdmaxExtent: TSFFloat index 3 read GetFieldAsSFFloat;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TNodeFontStyle_2. }
    function FontStyle: TNodeFontStyle_2;
  end;

  TNodeTextureCoordinate = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdpoint: TMFVec2f index 0 read GetFieldAsMFVec2f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTextureTransform = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcenter: TSFVec2f index 0 read GetFieldAsSFVec2f;
    property Fdrotation: TSFFloat index 1 read GetFieldAsSFFloat;
    property Fdscale: TSFVec2f index 2 read GetFieldAsSFVec2f;
    property Fdtranslation: TSFVec2f index 3 read GetFieldAsSFVec2f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function Matrix: TMatrix4Single;
  end;

  TNodeTimeSensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdcycleInterval: TSFTime index 0 read GetFieldAsSFTime;
    property Fdenabled: TSFBool index 1 read GetFieldAsSFBool;
    property Fdloop: TSFBool index 2 read GetFieldAsSFBool;
    property FdstartTime: TSFTime index 3 read GetFieldAsSFTime;
    property FdstopTime: TSFTime index 4 read GetFieldAsSFTime;
    { eventOut     SFTime   cycleTime } { }
    { eventOut     SFFloat  fraction_changed } { }
    { eventOut     SFBool   isActive } { }
    { eventOut     SFTime   time } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventOut     SFVec3f hitNormal_changed } { }
    { eventOut     SFVec3f hitPoint_changed } { }
    { eventOut     SFVec2f hitTexCoord_changed } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTransform_2 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
    procedure BeforeTraverse(var State: TVRMLGraphTraverseState); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdcenter: TSFVec3f index 4 read GetFieldAsSFVec3f;
    property Fdrotation: TSFRotation index 5 read GetFieldAsSFRotation;
    property Fdscale: TSFVec3f index 6 read GetFieldAsSFVec3f;
    property FdscaleOrientation: TSFRotation index 7 read GetFieldAsSFRotation;
    property Fdtranslation: TSFVec3f index 8 read GetFieldAsSFVec3f;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTrimmedSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    { eventIn       MFNode   addTrimmingContour } { }
    { eventIn       MFNode   removeTrimmingContour } { }
    property FdtrimmingContour: TMFNode index 0 read GetFieldAsMFNode;
    property Fdsurface: TSFNode index 1 read GetFieldAsSFNode;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeViewpoint = class(TNodeGeneralViewpoint)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdfieldOfView: TSFFloat index 6 read GetFieldAsSFFloat;
    property Fdjump: TSFBool index 7 read GetFieldAsSFBool;
    property Fddescription: TSFString index 8 read GetFieldAsSFString;
    property FdcenterOfRotation: TSFVec3f index 9 read GetFieldAsSFVec3f;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    class function CameraKind: TVRMLCameraKind; override;

    { This calculates proper angle of view for typical rectangular
      display, based on given fieldOfView field value.
      Result is in radians (just like fieldOfView VRML field).

      If you want to calculate horizontal angle of view then
      pass as ThisToOtherSizeRatio your window's width / height.
      If you want to calculate vertical angle of view then
      pass as ThisToOtherSizeRatio your window's height / width.
      For this method it doesn't really matter which is horizontal
      and which is vertical, both are treated the same.

      This works following VRML spec. So the angle of view for
      smaller window size is set to fieldOfViee. The other angle
      can always be calculated by AdjustViewAngleRadToAspectRatio
      (this implements the same equation that is mentioned in VRML spec).
      The larger angle cannot be larger than Pi, and may force the
      smaller angle to be smaller than fieldOfView. }
    function AngleOfView(const ThisToOtherSizeRatio: Single): Single;

    { This is like AngleOfView, but it allows you to specify
      FieldOfView as a parameter. }
    class function ViewpointAngleOfView(
      FieldOfView: Single;
      const ThisToOtherSizeRatio: Single): Single;
  end;

  TNodeVisibilitySensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdcenter: TSFVec3f index 0 read GetFieldAsSFVec3f;
    property Fdenabled: TSFBool index 1 read GetFieldAsSFBool;
    property Fdsize: TSFVec3f index 2 read GetFieldAsSFVec3f;
    { eventOut     SFTime  enterTime } { }
    { eventOut     SFTime  exitTime } { }
    { eventOut     SFBool  isActive } { }

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeWorldInfo = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property Fdinfo: TMFString index 0 read GetFieldAsMFString;
    property Fdtitle: TSFString index 1 read GetFieldAsSFString;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
type
  TGeneralGroupingEnumerator = class
    Lights: TDynActiveLightArray;
    State: TVRMLGraphTraverseState;
    procedure EnumerateChildrenFunction(Node, Child: TVRMLNode);
  end;

  procedure TGeneralGroupingEnumerator.EnumerateChildrenFunction(
    Node, Child: TVRMLNode);
  begin
    if Child is TNodeGeneralDirectionalLight then
      { We do CreateActiveLight with State from TNodeGeneralGrouping node,
        while precisely we should rather use State from TNodeDirectionalLight
        traverse. But, fortunately, State of TNodeDirectionalLight doesn't
        change inside (TNodeDirectionalLight doesn't do anything inside
        BeforeTraverse), so this is the same thing. }
      State.VRML2ActiveLights.AppendItem(
        TNodeGeneralDirectionalLight(Child).CreateActiveLight(State));
  end;

procedure TNodeGeneralGrouping.BeforeTraverse(
  var State: TVRMLGraphTraverseState);
var
  Enumerator: TGeneralGroupingEnumerator;
begin
  inherited;
  OriginalState := State;
  State := TVRMLGraphTraverseState.CreateCopy(OriginalState);

  (*We append all directional lights to the current State.
    This is how we implement DirectionalLight scope according to VRML 2.0
    specification.

    This may seem wasteful to enumerate children twice
    (first we enumerate here, then actual Traverse enumerates,
    this time recursively, for the second time). But it turned out to
    be the fastest and simplest method of propagating DirectionalLight
    correctly...

    First approach was to add ParentGroup to TActiveLight
    and do it as post-processing step, i.e. in TVRMLFlatScene in
    UpdateVRML2ActiveLights take this ParentGroup and add light everywhere.
    But that was 1. even slower, since it must traverse ParentGroup once again
    for each directional light, and for each shape within this ParentGroup,
    it must find all it's occurences inside ShapeStates, and add light there
    2. it fails ugly in case of DEF / USE of shapes.

    See kambi_vrml_test_suite/vrml_2/directional_light_scope.wrl test. Imagine
    (simplified VRML below) :

      Group {
        DEF S Shape { .... some sphere .... }
        Group {
          USE S
          DirectionalLight { }
          USE S
        }
        USE S
      }

    What would happen here ? ParentGroup.Enumerate would find S *two* times.
    For each of these occurences, it would find *four* shape states with
    matching node value. So 1. all four spheres would be lighted (incorrect,
    only two spheres in the middle should be lighted) 2. all of them would
    be lighted two times by the same light... Fix for 1st problem would
    require us to record some list of parents within State (which would
    awfully slow down Traverse work, that already is too crowded). Fix for 2nd
    problem would require some intelligent avoiding of duplicates
    (set light only for first node, that is both matching and has the light
    not set yet).

    It would be quite more convoluted and much slower than
    simple, correct solution below. *)

  Enumerator := TGeneralGroupingEnumerator.Create;
  try
    Enumerator.State := State;
    DirectEnumerateActive(
      {$ifdef FPC_OBJFPC} @ {$endif} Enumerator.EnumerateChildrenFunction);
  finally FreeAndNil(Enumerator) end;
end;

procedure TNodeGeneralGrouping.AfterTraverse(
  var State: TVRMLGraphTraverseState);
begin
  FreeAndNil(State);
  State := OriginalState;
  inherited;
end;

constructor TNodeX3DGroupingNode.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('addChildren', TMFNode, true));
  Events.Add(TVRMLEvent.Create('removeChildren', TMFNode, true));

  Fields.Add(TMFNode.Create(Self, 'children', AllowedChildrenNodes));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFVec3f.Create('bboxCenter', ZeroVector3Single));
  Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1)));
end;

function TNodeX3DGroupingNode.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

class function TNodeAnchor.ClassNodeTypeName: string;
begin
  Result := 'Anchor';
end;

class function TNodeAnchor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeAnchor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFString.Create('description', '')); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('parameter', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

procedure TNodeAnchor.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

class function TNodeAppearance.ClassNodeTypeName: string;
begin
  Result := 'Appearance';
end;

class function TNodeAppearance.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeAppearance.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFNode.Create(Self, 'material',
    [TNodeMaterial_2]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'texture',
    [TNodeImageTexture, TNodeMovieTexture, TNodePixelTexture]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'textureTransform',
    [TNodeTextureTransform]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'fillProperties', [{TNodeFillProperties}]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'lineProperties', [{TNodeLineProperties}]));
  Fields.Last.Exposed := true;

  Fields.Add(TSFNode.Create(Self, 'metadata', [{TNodeX3DMetadataObject}]));
  Fields.Last.Exposed := true;

  Fields.Add(TMFNode.Create(Self, 'shaders', [
    TNodeComposedShader, TNodePackagedShader,  TNodeProgramShader]));
  Fields.Last.Exposed := true;

  { In edition 2 of X3D XML encoding, this is empty... but in earlier
    versions, this was "appearance" and this seems more sensible,
    Appearance node may only occur within Shape.appearance field
    so it should definitely have DefaultContainerField set. }
  DefaultContainerField := 'appearance';
end;

function TNodeAppearance.GLSLShader(Index: Integer): TNodeComposedShader;
begin
  if FdShaders.Items[Index] is TNodeComposedShader then
  begin
    Result := TNodeComposedShader(FdShaders.Items[Index]);
    if not ((Result.FdLanguage.Value = '') or
            (Result.FdLanguage.Value = 'GLSL')) then
      Result := nil;
  end else
    Result := nil;
end;

class function TNodeAudioClip.ClassNodeTypeName: string;
begin
  Result := 'AudioClip';
end;

class function TNodeAudioClip.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeAudioClip.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFString.Create('description', '')); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('loop', FALSE)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('pitch', 1.0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('startTime', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('stopTime', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('duration_changed', TSFTime, false));
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));

  DefaultContainerField := 'children';
end;

constructor TNodeBackground.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_bind', TSFBool, true));
  Fields.Add(TMFFloat.Create('groundAngle', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFColor.Create('groundColor', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('backUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('bottomUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('frontUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('leftUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('rightUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('topUrl', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('skyAngle', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFColor.Create('skyColor', [ZeroVector3Single])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('isBound', TSFBool, false));

  DefaultContainerField := 'children';

  ImageClassesAssign(FAllowedBgImagesClasses, []);
  FBgImagesLoaded := false;
  FBgImages := BackgroundImagesNone;
end;

class function TNodeBackground.ClassNodeTypeName: string;
begin
  Result := 'Background';
end;

class function TNodeBackground.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeBackground.SetBgImagesLoaded(const Value: boolean);

  procedure LoadImg(bs: TBackgroundSide; Urls: TMFString);
  var
    I: Integer;
    URL: string;
  begin
    FBgImages[bs] := nil;
    for i := 0 to Urls.Count-1 do
    begin
      try
        URL := PathFromWWWBasePath(Urls.Items.Items[i]);
        if ImagesCache <> nil then
        begin
          { TODO: this doesn't honour AllowedBgImagesClasses for now,
            ImagesCache.LoadImage_IncReference always loads with
            AllowedImageClasses = [TRGBImage, TAlphaImage] }
          FBgImages[bs] := ImagesCache.LoadImage_IncReference(URL);
        end else
          FBgImages[bs] := LoadImage(URL, AllowedBgImagesClasses, [], 0, 0);
        Break;
      except
        on E: Exception do
        begin
          VRMLNonFatalError('Exception ' + E.ClassName +
            ' occured when trying to load ' +
            'background image from URL "' + URL + '" : '+E.Message);
          { and silence exception }
        end;
      end;
    end;
  end;

begin
  if Value <> BgImagesLoaded then
  begin
    if Value then
    begin
      try
        LoadImg(bsBack, FdBackUrl);
        LoadImg(bsBottom, FdBottomUrl);
        LoadImg(bsFront, FdFrontUrl);
        LoadImg(bsLeft, FdLeftUrl);
        LoadImg(bsRight, FdRightUrl);
        LoadImg(bsTop, FdTopUrl);
      except
        { In case of trouble, free the images that were loaded.
          This may happen if VRMLNonFatalError raises exception and some
          image cannot be loaded. }
        BackgroundImagesFreeAll(FbgImages, ImagesCache);
        raise;
      end;
      UsedCache := ImagesCache;
    end else
    begin
      BackgroundImagesFreeAll(FbgImages, UsedCache);
      UsedCache := nil;
    end;

    FBgImagesLoaded := Value;
  end;
end;

procedure TNodeBackground.Parse(Lexer: TVRMLLexer);
begin
  inherited;
  { Force reload on next BgImages use, since our fields are different now }
  BgImagesLoaded := false;
end;

function TNodeBackground.GetBgImages: TBackgroundImages;
begin
  BgImagesLoaded := true;
  Result := FBgImages;
end;

procedure TNodeBackground.SetAllowedBgImagesClasses(const Value: array of TImageClass);
begin
  if not ImageClassesEqual(Value, AllowedBgImagesClasses) then
  begin
    ImageClassesAssign(FAllowedBgImagesClasses, Value);
    BgImagesLoaded := false;
  end;
end;

destructor TNodeBackground.Destroy;
begin
  BgImagesLoaded := false;
  inherited;
end;

class function TNodeBillboard.ClassNodeTypeName: string;
begin
  Result := 'Billboard';
end;

class function TNodeBillboard.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeBillboard.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec3f.Create('axisOfRotation', Vector3Single(0, 1, 0)));
  Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

procedure TNodeBillboard.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

class function TNodeBox.ClassNodeTypeName: string;
begin
  Result := 'Box';
end;

class function TNodeBox.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeBox.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFVec3f.Create('size', Vector3Single(2, 2, 2)));
end;

class function TNodeCollision.ClassNodeTypeName: string;
begin
  Result := 'Collision';
end;

class function TNodeCollision.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCollision.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFBool.Create('collide', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'proxy', AllowedChildrenNodes));

  Events.Add(TVRMLEvent.Create('collideTime', TSFTime, false));

  Fields.Add(TSFBool.Create('enabled', true));
  Fields.Last.Exposed := true;

  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));

  DefaultContainerField := 'children';
end;

procedure TNodeCollision.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

class function TNodeColor.ClassNodeTypeName: string;
begin
  Result := 'Color';
end;

class function TNodeColor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeColor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFColor.Create('color', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'color';
end;

class function TNodeColorInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ColorInterpolator';
end;

class function TNodeColorInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeColorInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFColor.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TSFColor, false));

  DefaultContainerField := 'children';
end;

class function TNodeCone_2.ClassNodeTypeName: string;
begin
  Result := 'Cone';
end;

class function TNodeCone_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCone_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFFloat.Create('bottomRadius', 1));
  Fields.Add(TSFFloat.Create('height', 2));
  Fields.Add(TSFBool.Create('side', TRUE));
  Fields.Add(TSFBool.Create('bottom', TRUE));
end;

class function TNodeCone_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeContour2D.ClassNodeTypeName: string;
begin
  Result := 'Contour2D';
end;

class function TNodeContour2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeContour2D.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('addChildren', TMFNode, true));
  Events.Add(TVRMLEvent.Create('removeChildren', TMFNode, true));
  Fields.Add(TMFNode.Create(Self, 'children',
    [TNodeNurbsCurve2D, TNodePolyline2D, TNodeContour2D]));
  Fields.Last.Exposed := true;

  DefaultContainerField := 'trimmingContour';
end;

class function TNodeCoordinate.ClassNodeTypeName: string;
begin
  Result := 'Coordinate';
end;

class function TNodeCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCoordinate.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFVec3f.Create('point', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'coord';
end;

class function TNodeCoordinateDeformer.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDeformer';
end;

class function TNodeCoordinateDeformer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateDeformer.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('addChildren', TMFNode, true));
  Events.Add(TVRMLEvent.Create('removeChildren', TMFNode, true));
  Fields.Add(TMFNode.Create(Self, 'children', AllowedChildrenNodes)); Fields.Last.Exposed := true;
  Fields.Add(TMFVec3f.Create('controlPoint', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFNode.Create(Self, 'inputCoord', [TNodeCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TMFNode.Create(Self, 'inputTransform', [TNodeTransform_2])); Fields.Last.Exposed := true;
  Fields.Add(TMFNode.Create(Self, 'outputCoord', [TNodeCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('weight', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('bboxCenter', ZeroVector3Single));
  Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1)));
  Fields.Add(TSFInt32.Create('uDimension', 0));
  Fields.Add(TMFFloat.Create('uKnot', []));
  Fields.Add(TSFInt32.Create('uOrder', 2));
  Fields.Add(TSFInt32.Create('vDimension', 0));
  Fields.Add(TMFFloat.Create('vKnot', []));
  Fields.Add(TSFInt32.Create('vOrder', 2));
  Fields.Add(TSFInt32.Create('wDimension', 0));
  Fields.Add(TMFFloat.Create('wKnot', []));
  Fields.Add(TSFInt32.Create('wOrder', 2));

  DefaultContainerField := 'children';
end;

function TNodeCoordinateDeformer.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

class function TNodeCoordinateInterpolator.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator';
end;

class function TNodeCoordinateInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFVec3f.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TMFVec3f, false));

  DefaultContainerField := 'children';
end;

class function TNodeCylinder_2.ClassNodeTypeName: string;
begin
  Result := 'Cylinder';
end;

class function TNodeCylinder_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCylinder_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFBool.Create('bottom', TRUE));
  Fields.Add(TSFFloat.Create('height', 2));
  Fields.Add(TSFFloat.Create('radius', 1));
  Fields.Add(TSFBool.Create('side', TRUE));
  Fields.Add(TSFBool.Create('top', TRUE));
end;

class function TNodeCylinder_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeCylinderSensor.ClassNodeTypeName: string;
begin
  Result := 'CylinderSensor';
end;

class function TNodeCylinderSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeCylinderSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFFloat.Create('diskAngle', Pi / 12)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('maxAngle', -1)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('minAngle', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('offset', 0)); Fields.Last.Exposed := true;

  Fields.Add(TSFRotation.Create('axisRotation', Vector3Single(0, 1, 0), 0));
  Fields.Last.Exposed := true;

  Events.Add(TVRMLEvent.Create('rotation_changed', TSFRotation, false));

  DefaultContainerField := 'children';
end;

constructor TNodeDirectionalLight_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { Default value of ambientIntensity for VRML 1.0 and 2.0 is different,
    see comments at ambientIntensity in implementation of TPointLight_2. }
  FdAmbientIntensity.Value := 0;
  FdAmbientIntensity.DefaultValue := 0;

  DefaultContainerField := 'children';
end;

class function TNodeDirectionalLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeDirectionalLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'ElevationGrid';
end;

class function TNodeElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeElevationGrid.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('set_height', TMFFloat, true));
  Fields.Add(TSFNode.Create(Self, 'color', [TNodeColor])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'normal', [TNodeNormal])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('height', []));
  Fields.Add(TSFBool.Create('ccw', TRUE));
  Fields.Add(TSFBool.Create('colorPerVertex', TRUE));
  Fields.Add(TSFFloat.Create('creaseAngle', 0));
  Fields.Add(TSFBool.Create('normalPerVertex', TRUE));
  Fields.Add(TSFBool.Create('solid', TRUE));
  Fields.Add(TSFInt32.Create('xDimension', 0));
  Fields.Add(TSFFloat.Create('xSpacing', 1.0));
  Fields.Add(TSFInt32.Create('zDimension', 0));
  Fields.Add(TSFFloat.Create('zSpacing', 1.0));
end;

function TNodeElevationGrid.IsNotEmpty: boolean;
begin
  Result :=
    (FdXDimension.Value >= 2) and
    (FdZDimension.Value >= 2) and
    { VRML spec says that xSpacing and ySpacing shall be > 0.
      So I understand that when they are = 0 (or < 0) nothing
      should be rendered. }
    (FdXSpacing.Value > 0) and
    (FdZSpacing.Value > 0) and
    (FdHeight.Count >= FdXDimension.Value * FdZDimension.Value);
end;

class function TNodeExtrusion.ClassNodeTypeName: string;
begin
  Result := 'Extrusion';
end;

class function TNodeExtrusion.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeExtrusion.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('set_crossSection', TMFVec2f, true));
  Events.Add(TVRMLEvent.Create('set_orientation', TMFRotation, true));
  Events.Add(TVRMLEvent.Create('set_scale', TMFVec2f, true));
  Events.Add(TVRMLEvent.Create('set_spine', TMFVec3f, true));
  Fields.Add(TSFBool.Create('beginCap', TRUE));
  Fields.Add(TSFBool.Create('ccw', TRUE));
  Fields.Add(TSFBool.Create('convex', TRUE));
  Fields.Add(TSFFloat.Create('creaseAngle', 0));
  Fields.Add(TMFVec2f.Create('crossSection', [ Vector2Single(1, 1), Vector2Single(1, -1), Vector2Single(-1, -1),  Vector2Single(-1, 1),  Vector2Single(1, 1) ]));
  Fields.Add(TSFBool.Create('endCap', TRUE));
  Fields.Add(TMFRotation.Create('orientation', [ Vector4Single(0, 0, 1, 0) ] ));
  Fields.Add(TMFVec2f.Create('scale', Vector2Single(1, 1)));
  Fields.Add(TSFBool.Create('solid', TRUE));
  Fields.Add(TMFVec3f.Create('spine', [ Vector3Single(0, 0, 0), Vector3Single(0, 1, 0) ]));
end;

class function TNodeFog.ClassNodeTypeName: string;
begin
  Result := 'Fog';
end;

class function TNodeFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeFog.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFColor.Create('color', Vector3Single(1, 1, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFString.Create('fogType', 'LINEAR')); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('visibilityRange', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('volumetric', false));
  Fields.Add(TSFVec3f.Create('volumetricDirection', Vector3Single(0, -1, 0)));
  Fields.Add(TSFFloat.Create('volumetricVisibilityStart', 0));
  Fields.Add(TSFNode.Create(Self, 'alternative', [TNodeFog]));
  Events.Add(TVRMLEvent.Create('set_bind', TSFBool, true));
  Events.Add(TVRMLEvent.Create('isBound', TSFBool, false));

  DefaultContainerField := 'children';
end;

function TNodeFog.Alternative: TNodeFog;
begin
  if (FdAlternative.Value <> nil) and (FdAlternative.Value is TNodeFog) then
    Result := TNodeFog(FdAlternative.Value) else
    Result := nil;
end;

class function TNodeFontStyle_2.ClassNodeTypeName: string;
begin
  Result := 'FontStyle';
end;

class function TNodeFontStyle_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeFontStyle_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('family', ['SERIF']));
  Fields.Add(TSFBool.Create('horizontal', TRUE));
  Fields.Add(TMFString.Create('justify', ['BEGIN']));
  Fields.Add(TSFString.Create('language', ''));
  Fields.Add(TSFBool.Create('leftToRight', TRUE));
  Fields.Add(TSFFloat.Create('size', DefaultSize));
  Fields.Add(TSFFloat.Create('spacing', DefaultSpacing));
  Fields.Add(TSFString.Create('style', 'PLAIN'));
  Fields.Add(TSFBool.Create('topToBottom', TRUE));

  DefaultContainerField := 'fontStyle';
end;

class function TNodeFontStyle_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeFontStyle_2.TTF_Font: PTrueTypeFont;
begin
  Result := TTF_Font_Results[Family, Bold, Italic];
end;

class function TNodeFontStyle_2.ClassTTF_Font(
  AFamily: TVRMLFontFamily; const ABold, AItalic: boolean): PTrueTypeFont;
begin
  Result := TTF_Font_Results[AFamily, ABold, AItalic];
end;

function TNodeFontStyle_2.Family: TVRMLFontFamily;
var
  I: Integer;
begin
  for I := 0 to FdFamily.Items.Count - 1 do
    if FdFamily.Items[I] = 'SERIF' then
      Exit(ffSerif) else
    if FdFamily.Items[I] = 'SANS' then
      Exit(ffSans) else
    if FdFamily.Items[I] = 'TYPEWRITER' then
      Exit(ffTypeWriter) else
      VRMLNonFatalError('Font family "' + FdFamily.Items[I] + '" not supported');

  { If no supported values on FdFamily.Items then fall back to serif }
  Result := ffSerif;
end;

const
  StyleBold = 'BOLD';
  StyleBoldItalic = 'BOLDITALIC';
  StyleItalic = 'ITALIC';
  StylePlain = 'PLAIN';

function TNodeFontStyle_2.Bold: boolean;
begin
  Result :=
    (FdStyle.Value = StyleBold) or
    (FdStyle.Value = StyleBoldItalic);

  { This is the end of calculating Result.
    But we would like to make a warning in case of invalid FdStyle
    value, so we do check below. }

  if not Result then
  begin
    if not (
      (FdStyle.Value = StyleItalic) or
      (FdStyle.Value = StylePlain) or
      (FdStyle.Value = '')) then
      VRMLNonFatalError('Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TNodeFontStyle_2.Italic: boolean;
begin
  Result :=
    (FdStyle.Value = StyleItalic) or
    (FdStyle.Value = StyleBoldItalic);

  { This is the end of calculating Result.
    But we would like to make a warning in case of invalid FdStyle
    value, so we do check below. }

  if not Result then
  begin
    if not (
      (FdStyle.Value = StyleBold) or
      (FdStyle.Value = StylePlain) or
      (FdStyle.Value = '')) then
      VRMLNonFatalError('Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TNodeFontStyle_2.Justify: TVRMLFontJustify;
begin
  if FdJustify.Items.Count = 0 then
    Result := fjBegin else
  begin
    if (FdJustify.Items[0] = 'BEGIN') or
       (FdJustify.Items[0] = 'FIRST') then
      Result := fjBegin else
    if FdJustify.Items[0] = 'MIDDLE' then
      Result := fjMiddle else
    if FdJustify.Items[0] = 'END' then
      Result := fjEnd else
    begin
      Result := fjBegin;
      VRMLNonFatalError('Font justify "' + FdJustify.Items[0] +
        '" not supported');
    end;
  end;
end;

class function TNodeFontStyle_2.DefaultSize: Single;
begin
  Result := 1;
end;

class function TNodeFontStyle_2.DefaultSpacing: Single;
begin
  Result := 1;
end;

class function TNodeFontStyle_2.DefaultFamily: TVRMLFontFamily;
begin
  Result := ffSerif;
end;

class function TNodeFontStyle_2.DefaultBold: boolean;
begin
  Result := false;
end;

class function TNodeFontStyle_2.DefaultItalic: boolean;
begin
  Result := false;
end;

class function TNodeFontStyle_2.DefaultJustify: TVRMLFontJustify;
begin
  Result := fjBegin;
end;

class function TNodeFontStyle_2.DefaultTTF_Font: PTrueTypeFont;
begin
  Result := TTF_Font_Results[DefaultFamily, DefaultBold, DefaultItalic];
end;

class function TNodeGeoCoordinate.ClassNodeTypeName: string;
begin
  Result := 'GeoCoordinate';
end;

class function TNodeGeoCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoCoordinate.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Fields.Add(TMFString.Create('point', []));

  DefaultContainerField := 'coord';
end;

class function TNodeGeoElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'GeoElevationGrid';
end;

class function TNodeGeoElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoElevationGrid.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('set_height', TMFFloat, true));
  Events.Add(TVRMLEvent.Create('set_yScale', TSFFloat, true));
  Fields.Add(TSFNode.Create(Self, 'color', [TNodeColor])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'normal', [TNodeNormal])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('ccw', TRUE));
  Fields.Add(TSFBool.Create('colorPerVertex', TRUE));
  Fields.Add(TSFFloat.Create('creaseAngle', 0));
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Fields.Add(TSFString.Create('geoGridOrigin', '0 0 0'));
  Fields.Add(TMFFloat.Create('height', []));
  Fields.Add(TSFBool.Create('normalPerVertex', TRUE));
  Fields.Add(TSFBool.Create('solid', TRUE));
  Fields.Add(TSFInt32.Create('xDimension', 0));
  Fields.Add(TSFString.Create('xSpacing', '1.0'));
  Fields.Add(TSFFloat.Create('yScale', 1.0));
  Fields.Add(TSFInt32.Create('zDimension', 0));
  Fields.Add(TSFString.Create('zSpacing', '1.0'));
end;

class function TNodeGeoLocation.ClassNodeTypeName: string;
begin
  Result := 'GeoLocation';
end;

class function TNodeGeoLocation.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoLocation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFString.Create('geoCoords', '')); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));

  DefaultContainerField := 'children';
end;

class function TNodeGeoLOD.ClassNodeTypeName: string;
begin
  Result := 'GeoLOD';
end;

class function TNodeGeoLOD.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoLOD.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFString.Create('center', ''));
  Fields.Add(TMFString.Create('child1Url', []));
  Fields.Add(TMFString.Create('child2Url', []));
  Fields.Add(TMFString.Create('child3Url', []));
  Fields.Add(TMFString.Create('child4Url', []));
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Fields.Add(TSFFloat.Create('range', 10));
  Fields.Add(TMFString.Create('rootUrl', []));
  Fields.Add(TMFNode.Create(Self, 'rootNode', AllowedChildrenNodes));
  Events.Add(TVRMLEvent.Create('children', TMFNode, false));

  DefaultContainerField := 'children';
end;

class function TNodeGeoMetadata.ClassNodeTypeName: string;
begin
  Result := 'GeoMetadata';
end;

class function TNodeGeoMetadata.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoMetadata.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFNode.Create(Self, 'data', AllowedChildrenNodes)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('summary', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

class function TNodeGeoOrigin.ClassNodeTypeName: string;
begin
  Result := 'GeoOrigin';
end;

class function TNodeGeoOrigin.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoOrigin.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('geoSystem', ['GD','WE'])); Fields.Last.Exposed := true;
  Fields.Add(TSFString.Create('geoCoords', '')); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('rotateYUp', FALSE));

  DefaultContainerField := 'geoOrigin';
end;

class function TNodeGeoPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'GeoPositionInterpolator';
end;

class function TNodeGeoPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoPositionInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Fields.Add(TMFFloat.Create('key', []));
  Fields.Add(TMFString.Create('keyValue', []));
  Events.Add(TVRMLEvent.Create('geovalue_changed', TSFString, false));
  Events.Add(TVRMLEvent.Create('value_changed', TSFVec3f, false));

  DefaultContainerField := 'children';
end;

class function TNodeGeoTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'GeoTouchSensor';
end;

class function TNodeGeoTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoTouchSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFBool.Create('enabled', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Events.Add(TVRMLEvent.Create('hitNormal_changed', TSFVec3f, false));
  Events.Add(TVRMLEvent.Create('hitPoint_changed', TSFVec3f, false));
  Events.Add(TVRMLEvent.Create('hitTexCoord_changed', TSFVec2f, false));
  Events.Add(TVRMLEvent.Create('hitGeoCoord_changed', TSFString, false));
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));
  Events.Add(TVRMLEvent.Create('isOver', TSFBool, false));
  Events.Add(TVRMLEvent.Create('touchTime', TSFTime, false));

  DefaultContainerField := 'children';
end;

class function TNodeGeoViewpoint.ClassNodeTypeName: string;
begin
  Result := 'GeoViewpoint';
end;

class function TNodeGeoViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGeoViewpoint.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_bind', TSFBool, true));
  Events.Add(TVRMLEvent.Create('set_orientation', TSFString, true));
  Events.Add(TVRMLEvent.Create('set_position', TSFString, true));
  Fields.Add(TSFFloat.Create('fieldOfView', 0.785398)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('headlight', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('jump', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('navType', ['EXAMINE','ANY'])); Fields.Last.Exposed := true;
  Fields.Add(TSFString.Create('description', ''));
  Fields.Add(TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]));
  Fields.Add(TMFString.Create('geoSystem', ['GD','WE']));
  Fields.Add(TSFRotation.Create('orientation', Vector3Single(0, 0, 1), 0));
  Fields.Add(TSFString.Create('position', '0 0 100000'));
  Fields.Add(TSFFloat.Create('speedFactor', 1.0));
  Events.Add(TVRMLEvent.Create('bindTime', TSFTime, false));
  Events.Add(TVRMLEvent.Create('isBound', TSFBool, false));

  DefaultContainerField := 'children';
end;

class function TNodeGroup_2.ClassNodeTypeName: string;
begin
  Result := 'Group';
end;

class function TNodeGroup_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeGroup_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeGroup_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeGroup_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

procedure TNodeGroupHidden_2.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
var
  I: integer;
begin
  { Special things for TNodeGroupHidden_2 saving:
    NodeName is ignored (should be '').
    Only field "children" is used, and in a special way (we write it's
    nodes, without writing field name "children [" "]").
    Other fields are ignored (should never have any meaning). }

  for I := 0 to Prototypes.Count - 1 do
    Prototypes[I].SaveToStream(SaveProperties);

  for i := 0 to FdChildren.Count - 1 do
    FdChildren.Items[I].SaveToStream(SaveProperties);

  for I := 0 to Routes.Count - 1 do
    Routes[I].SaveToStream(SaveProperties);
end;

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeImageTexture.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('repeatS', TRUE));
  Fields.Add(TSFBool.Create('repeatT', TRUE));

  DefaultContainerField := 'texture';
end;

function TNodeImageTexture.LoadTextureImage(out CacheUsed: boolean): TImage;
var
  I: Integer;
  FullUrl: string;
begin
  Result := nil;
  CacheUsed := false;
  FTextureUsedFullUrl := '';

  for I := 0 to FdUrl.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      Result := ImagesCache.LoadImage_IncReference(FullUrl);
      CacheUsed := true;
      FTextureUsedFullUrl := FullUrl;
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load ' +
          'texture from filename "' + FullUrl + '" : ' + E.Message);
    end;
  end;
end;

function TNodeImageTexture.TextureDescription: string;
begin
  if TextureUsedFullUrl <> '' then
    Result := 'file "' + TextureUsedFullUrl + '"' else
    Result := 'none';
end;

function TNodeImageTexture.RepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TNodeImageTexture.RepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

class function TNodeIndexedFaceSet_2.ClassNodeTypeName: string;
begin
  Result := 'IndexedFaceSet';
end;

class function TNodeIndexedFaceSet_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeIndexedFaceSet_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('set_colorIndex', TMFInt32, true));
  Events.Add(TVRMLEvent.Create('set_coordIndex', TMFInt32, true));
  Events.Add(TVRMLEvent.Create('set_normalIndex', TMFInt32, true));
  Events.Add(TVRMLEvent.Create('set_texCoordIndex', TMFInt32, true));
  Fields.Add(TSFNode.Create(Self, 'color', [TNodeColor])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'normal', [TNodeNormal])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('ccw', TRUE));
  Fields.Add(TMFInt32.Create('colorIndex', []));
  Fields.Add(TSFBool.Create('colorPerVertex', TRUE));
  Fields.Add(TSFBool.Create('convex', TRUE));
  Fields.Add(TMFInt32.CreateMFLong('coordIndex', [], true));
  Fields.Add(TSFFloat.Create('creaseAngle', 0));
  Fields.Add(TMFInt32.Create('normalIndex', []));
  Fields.Add(TSFBool.Create('normalPerVertex', TRUE));
  Fields.Add(TSFBool.Create('solid', TRUE));
  Fields.Add(TMFInt32.CreateMFLong('texCoordIndex', [], true));
end;

class function TNodeIndexedFaceSet_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeIndexedLineSet_2.ClassNodeTypeName: string;
begin
  Result := 'IndexedLineSet';
end;

class function TNodeIndexedLineSet_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeIndexedLineSet_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('set_colorIndex', TMFInt32, true));
  Events.Add(TVRMLEvent.Create('set_coordIndex', TMFInt32, true));
  Fields.Add(TSFNode.Create(Self, 'color', [TNodeColor])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate])); Fields.Last.Exposed := true;
  Fields.Add(TMFInt32.Create('colorIndex', []));
  Fields.Add(TSFBool.Create('colorPerVertex', TRUE));
  Fields.Add(TMFInt32.CreateMFLong('coordIndex', [], true));
end;

class function TNodeIndexedLineSet_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeInline.ClassNodeTypeName: string;
begin
  Result := 'Inline';
end;

class function TNodeInline.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeInline.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('bboxCenter', ZeroVector3Single));
  Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1)));

  DefaultContainerField := 'children';

  FParsingAllowedChildren := false;
  FAllowedChildren := true;
end;

procedure TNodeInline.LoadInlined(CanReload: boolean);
var
  I: Integer;
  FullUrl: string;
  NewNode: TVRMLNode;
begin
  if ChildrenCount > 0 then
  begin
    if CanReload then RemoveAllChildren else Exit;
  end;

  NewNode := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      NewNode := LoadAsVRML(PathFromWWWBasePath(FullUrl), false);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load '+
          'inline file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;

  if NewNode <> nil then
    AddChild(NewNode);
end;

procedure TNodeInline.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  LoadInlined(false);
end;

class function TNodeInline.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

class function TNodeInlineLoadControl.ClassNodeTypeName: string;
begin
  Result := 'InlineLoadControl';
end;

class function TNodeInlineLoadControl.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeInlineLoadControl.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFBool.Create('load', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('bboxCenter', ZeroVector3Single));
  Fields.Add(TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1)));
  Events.Add(TVRMLEvent.Create('children', TMFNode, false));

  FParsingAllowedChildren := false;
  FAllowedChildren := true;
end;

procedure TNodeInlineLoadControl.LoadInlined(CanReload: boolean);
var
  I: Integer;
  FullUrl: string;
  NewNode: TVRMLNode;
begin
  { TODO: InlineLoadControl should load it's contents to
    children MFNode, and we should make a way (analogous
    to TNodeInlineLoadControl.ChildrenSaveToStream)
    to say that "we don't want to save to stream "children" field".
    For now it's not really important (user doesn't see
    where it's loaded), but it will be later for scripts. }

  if ChildrenCount > 0 then
  begin
    if CanReload then RemoveAllChildren else Exit;
  end;

  if not FdLoad.Value then Exit;

  NewNode := nil;

  for I := 0 to FdUrl.Items.Count - 1 do
  begin
    FullUrl := PathFromWWWBasePath(FdUrl.Items[I]);
    try
      NewNode := LoadAsVRML(PathFromWWWBasePath(FullUrl), false);
      Break;
    except
      on E: Exception do
        { pamietajmy ze VRMLNonFatalError moze spowodowac rzucenie wyjatku
          (chociaz nie musi) }
        VRMLNonFatalError('Exception ' + E.ClassName +
          ' occured when trying to load '+
          'inline file from URL "' + FullUrl + ' : ' + E.Message);
    end;
  end;

  if NewNode <> nil then
    AddChild(NewNode);
end;

procedure TNodeInlineLoadControl.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  LoadInlined(false);
end;

class function TNodeInlineLoadControl.ChildrenSaveToStream: boolean;
begin
  Result := false;
end;

class function TNodeLOD_2.ClassNodeTypeName: string;
begin
  Result := 'LOD';
end;

class function TNodeLOD_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeLOD_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFNode.Create(Self, 'level', AllowedChildrenNodes)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('center', ZeroVector3Single));
  Fields.Add(TMFFloat.Create('range', []));

  DefaultContainerField := 'children';
end;

class function TNodeLOD_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor = 2;
end;

function TNodeLOD_2.ChildrenField: TMFNode;
begin
  Result := FdLevel;
end;

procedure TNodeLOD_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdLevel.Items.Count >= 1 then
    Func(Self, FdLevel.Items[0]);
end;

class function TNodeMaterial_2.ClassNodeTypeName: string;
begin
  Result := 'Material';
end;

class function TNodeMaterial_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeMaterial_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFFloat.Create('ambientIntensity', DefaultMaterial_2AmbientIntensity)); Fields.Last.Exposed := true;
  Fields.Add(TSFColor.Create('diffuseColor', DefaultMaterialDiffuseColor)); Fields.Last.Exposed := true;
  Fields.Add(TSFColor.Create('emissiveColor', DefaultMaterialEmissiveColor)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('shininess', DefaultMaterialShininess)); Fields.Last.Exposed := true;
  Fields.Add(TSFColor.Create('specularColor', DefaultMaterialSpecularColor)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('transparency', DefaultMaterialTransparency)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('fogImmune', false)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('mirror', DefaultMaterialMirror));

  DefaultContainerField := 'material';
end;

class function TNodeMaterial_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeMaterial_2.Opacity: Single;
begin
  Result := 1- FdTransparency.Value;
end;

function TNodeMaterial_2.ShininessExp: Single;
begin
  Result := Clamped(FdShininess.Value * 128.0, 0.0, 128.0);
end;

class function TNodeMovieTexture.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeMovieTexture.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFBool.Create('loop', FALSE)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('speed', 1.0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('startTime', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('stopTime', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('repeatS', TRUE));
  Fields.Add(TSFBool.Create('repeatT', TRUE));
  Events.Add(TVRMLEvent.Create('duration_changed', TSFTime, false));
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));

  DefaultContainerField := 'children';
end;

class function TNodeNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'NavigationInfo';
end;

class function TNodeNavigationInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNavigationInfo.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFFloat.Create('avatarSize', [0.25, 1.6, 0.75])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('headlight', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('speed', 1.0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('transitionTime', 1.0)); Fields.Last.Exposed := true;
  Fields.Add(TMFString.Create('transitionType',  ['LINEAR']));
  Fields.Add(TMFString.Create('type', ['WALK', 'ANY'])); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('visibilityLimit', 0.0)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('transitionComplete', TSFBool, false));

  DefaultContainerField := 'children';
end;

class function TNodeNormal.ClassNodeTypeName: string;
begin
  Result := 'Normal';
end;

class function TNodeNormal.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNormal.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFVec3f.Create('vector', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'normal';
end;

class function TNodeNormalInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NormalInterpolator';
end;

class function TNodeNormalInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNormalInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFVec3f.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TMFVec3f, false));

  DefaultContainerField := 'children';
end;

class function TNodeNurbsCurve.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

class function TNodeNurbsCurve.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsCurve.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFVec3f.Create('controlPoint', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('weight', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('tessellation', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('knot', []));
  Fields.Add(TSFInt32.Create('order', 3));
end;

class function TNodeNurbsCurve2D.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve2D';
end;

class function TNodeNurbsCurve2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsCurve2D.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFVec2f.Create('controlPoint', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('tessellation', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('weight', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('knot', []));
  Fields.Add(TSFInt32.Create('order', 3));
end;

class function TNodeNurbsGroup.ClassNodeTypeName: string;
begin
  Result := 'NurbsGroup';
end;

class function TNodeNurbsGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsGroup.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFFloat.Create('tessellationScale', 1.0)); Fields.Last.Exposed := true;
end;

class function TNodeNurbsPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNodeNurbsPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsPositionInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TSFInt32.Create('dimension', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFVec3f.Create('keyValue', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('keyWeight', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('knot', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('order', 4)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TSFVec3f, false));

  DefaultContainerField := 'children';
end;

class function TNodeNurbsSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurface';
end;

class function TNodeNurbsSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFVec3f.Create('controlPoint', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate, TNodeNurbsTextureSurface])); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('uTessellation', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('vTessellation', 0)); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('weight', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('ccw', TRUE));
  Fields.Add(TSFBool.Create('solid', TRUE));
  Fields.Add(TSFInt32.Create('uDimension', 0));
  Fields.Add(TMFFloat.Create('uKnot', []));
  Fields.Add(TSFInt32.Create('uOrder', 3));
  Fields.Add(TSFInt32.Create('vDimension', 0));
  Fields.Add(TMFFloat.Create('vKnot', []));
  Fields.Add(TSFInt32.Create('vOrder', 3));
end;

class function TNodeNurbsTextureSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureSurface';
end;

class function TNodeNurbsTextureSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeNurbsTextureSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFVec2f.Create('controlPoint', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('weight', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('uDimension', 0));
  Fields.Add(TMFFloat.Create('uKnot', []));
  Fields.Add(TSFInt32.Create('uOrder', 3));
  Fields.Add(TSFInt32.Create('vDimension', 0));
  Fields.Add(TMFFloat.Create('vKnot', []));
  Fields.Add(TSFInt32.Create('vOrder', 3));
end;

class function TNodeOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'OrientationInterpolator';
end;

class function TNodeOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeOrientationInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFRotation.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TSFRotation, false));

  DefaultContainerField := 'children';
end;

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodePixelTexture.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFImage.Create('image', nil)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('repeatS', TRUE));
  Fields.Add(TSFBool.Create('repeatT', TRUE));

  DefaultContainerField := 'texture';
end;

function TNodePixelTexture.LoadTextureImage(out CacheUsed: boolean): TImage;
begin
  Result := nil;
  CacheUsed := false;

  if not FdImage.Value.IsNull then
    Result := FdImage.Value.MakeCopy;
end;

function TNodePixelTexture.TextureDescription: string;
begin
  if not FdImage.Value.IsNull then
    result := Format('inlined (width = %d; height = %d; with alpha = %s)',
      [ FdImage.Value.Width, FdImage.Value.Height,
        BoolToStr[FdImage.Value is TAlphaImage] ]) else
    result := 'none';
end;

function TNodePixelTexture.RepeatS: boolean;
begin
  Result := FdRepeatS.Value;
end;

function TNodePixelTexture.RepeatT: boolean;
begin
  Result := FdRepeatT.Value;
end;

class function TNodePlaneSensor.ClassNodeTypeName: string;
begin
  Result := 'PlaneSensor';
end;

class function TNodePlaneSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodePlaneSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec2f.Create('maxPosition', Vector2Single(-1, -1))); Fields.Last.Exposed := true;
  Fields.Add(TSFVec2f.Create('minPosition', Vector2Single(0, 0))); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('offset', ZeroVector3Single)); Fields.Last.Exposed := true;

  Fields.Add(TSFRotation.Create('axisRotation', Vector3Single(0, 0, 1), 0));
  Fields.Last.Exposed := true;

  Events.Add(TVRMLEvent.Create('translation_changed', TSFVec3f, false));

  DefaultContainerField := 'children';
end;

constructor TNodePointLight_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { Previous fields initialized in TNodeGeneralPointLight and above.
    However the default values of "ambientIntensity" and "location" fields
    are different.
    - ambientIntensity in VRML 1.0 is my extension,
      and the default value differs from VRML 2.0 spec on purpose,
      to allow VRML 1.0-compat behavior when ambientIntensity is not specified.
    - location has just different default value between VRML 1.0 and 2.0
      specifications... Though VRML 2.0 indeed has more sensible default
      value, so that's another improvement in VRML 2.0. }
  FdAmbientIntensity.Value := 0;
  FdAmbientIntensity.DefaultValue := 0;
  FdLocation.Value := ZeroVector3Single;
  FdLocation.DefaultValue := ZeroVector3Single;

  Fields.Add(TSFFloat.Create('radius', 100)); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

class function TNodePointLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodePointLight_2.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result := inherited;

  { TODO: For non-uniform scale, this will simply use average scale.
    This is not fully correct, VRML spec doesn't clarify this
    but I guess that the intention was that the non-uniform scale will
    make radius non-uniform, i.e. light volume will not be a regular sphere
    but some 3d ellipsoid. Unfortunately this would require quite more
    work, UpdateVRML2ActiveLights would then have to check for collision
    between
      sphere transformed by matrix Transform
    and
      bounding box
    which I don't know how to do *easily*... }
  Result.TransfRadius := FdRadius.Value * Result.AverageScaleTransform;
end;

class function TNodePointLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodePointSet_2.ClassNodeTypeName: string;
begin
  Result := 'PointSet';
end;

class function TNodePointSet_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodePointSet_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFNode.Create(Self, 'color', [TNodeColor])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate])); Fields.Last.Exposed := true;
end;

class function TNodePointSet_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodePolyline2D.ClassNodeTypeName: string;
begin
  Result := 'Polyline2D';
end;

class function TNodePolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodePolyline2D.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFVec2f.Create('point', [])); Fields.Last.Exposed := true;
end;

class function TNodePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator';
end;

class function TNodePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodePositionInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFVec3f.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TSFVec3f, false));

  DefaultContainerField := 'children';
end;

class function TNodeProximitySensor.ClassNodeTypeName: string;
begin
  Result := 'ProximitySensor';
end;

class function TNodeProximitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeProximitySensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec3f.Create('center', ZeroVector3Single)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('size', ZeroVector3Single)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('enabled', TRUE)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));
  Events.Add(TVRMLEvent.Create('position_changed', TSFVec3f, false));
  Events.Add(TVRMLEvent.Create('orientation_changed', TSFRotation, false));
  Events.Add(TVRMLEvent.Create('enterTime', TSFTime, false));
  Events.Add(TVRMLEvent.Create('exitTime', TSFTime, false));

  DefaultContainerField := 'children';
end;

class function TNodeScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ScalarInterpolator';
end;

class function TNodeScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeScalarInterpolator.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('set_fraction', TSFFloat, true));
  Fields.Add(TMFFloat.Create('key', [])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('keyValue', [])); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('value_changed', TSFFloat, false));

  DefaultContainerField := 'children';
end;

class function TNodeScript.ClassNodeTypeName: string;
begin
  Result := 'Script';
end;

class function TNodeScript.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeScript.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('url', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('directOutput', FALSE));
  Fields.Add(TSFBool.Create('mustEvaluate', FALSE));

  DefaultContainerField := 'children';

  FInterfaceDeclarations := TVRMLInterfaceDeclarationsList.Create;
end;

destructor TNodeScript.Destroy;
begin
  FreeWithContentsAndNil(FInterfaceDeclarations);
  inherited;
end;

function TNodeScript.ParseNodeBodyElement(Lexer: TVRMLLexer): boolean;
var
  I: TVRMLInterfaceDeclaration;
begin
  Result := inherited;

  if not Result then
  begin
    Result := Lexer.TokenIsKeyword(RestrictedInterfaceDeclarationKeywords);
    if Result then
    begin
      I := TVRMLInterfaceDeclaration.Create;
      InterfaceDeclarations.Add(I);
      I.Parse(Lexer, true, true);
    end;
  end;
end;

procedure TNodeScript.SaveContentsToStream(
  SaveProperties: TVRMLSaveToStreamProperties);
var
  I: Integer;
begin
  for I := 0 to InterfaceDeclarations.Count - 1 do
    InterfaceDeclarations[I].SaveToStream(SaveProperties, true);

  inherited;
end;

class function TNodeShape.ClassNodeTypeName: string;
begin
  Result := 'Shape';
end;

class function TNodeShape.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeShape.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFNode.Create(Self, 'appearance', [TNodeAppearance, TNodeKambiAppearance])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'geometry', AllowedGeometryNodes)); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

procedure TNodeShape.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  if FdGeometry.Value <> nil then
  begin
    { According to VRML spec, when geometry is NULL then object is not
      drawn so appearance doesn't matter. }
    if FdAppearance.Value <> nil then
      Func(Self, FdAppearance.Value);
    Func(Self, FdGeometry.Value);
  end;
end;

procedure TNodeShape.BeforeTraverse(var State: TVRMLGraphTraverseState);
begin
  inherited;
  State.ParentShape := Self;
end;

procedure TNodeShape.AfterTraverse(var State: TVRMLGraphTraverseState);
begin
  State.ParentShape := nil;
  inherited;
end;

function TNodeShape.Appearance: TNodeAppearance;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
    Result := TNodeAppearance(FdAppearance.Value) else
    Result := nil;
end;

function TNodeShape.Texture: TNodeGeneralTexture;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdTexture.Value <> nil) and
       (A.FdTexture.Value is TNodeGeneralTexture) then
      Result := TNodeGeneralTexture(A.FdTexture.Value);
  end;
end;

function TNodeShape.NormalMap: TNodeGeneralTexture;
var
  A: TNodeKambiAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    if (A.FdNormalMap.Value <> nil) and
       (A.FdNormalMap.Value is TNodeGeneralTexture) then
      Result := TNodeGeneralTexture(A.FdNormalMap.Value);
  end;
end;

function TNodeShape.HeightMap: TNodeGeneralTexture;
var
  A: TNodeKambiAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    if (A.FdHeightMap.Value <> nil) and
       (A.FdHeightMap.Value is TNodeGeneralTexture) then
      Result := TNodeGeneralTexture(A.FdHeightMap.Value);
  end;
end;

function TNodeShape.HeightMapScale: Single;
var
  A: TNodeKambiAppearance;
begin
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeKambiAppearance) then
  begin
    A := TNodeKambiAppearance(FdAppearance.Value);
    Result := A.FdHeightMapScale.Value;
  end else
    Result := DefaultHeightMapScale;
end;

function TNodeShape.TextureTransform: TNodeTextureTransform;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdTextureTransform.Value <> nil) and
       (A.FdTextureTransform.Value is TNodeTextureTransform) then
      Result := TNodeTextureTransform(A.FdTextureTransform.Value);
  end;
end;

function TNodeShape.Material: TNodeMaterial_2;
var
  A: TNodeAppearance;
begin
  Result := nil;
  if (FdAppearance.Value <> nil) and
     (FdAppearance.Value is TNodeAppearance) then
  begin
    A := TNodeAppearance(FdAppearance.Value);
    if (A.FdMaterial.Value <> nil) and
       (A.FdMaterial.Value is TNodeMaterial_2) then
      Result := TNodeMaterial_2(A.FdMaterial.Value);
  end;
end;

class function TNodeSound.ClassNodeTypeName: string;
begin
  Result := 'Sound';
end;

class function TNodeSound.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeSound.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('intensity', 1)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('location', ZeroVector3Single)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('maxBack', 10)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('maxFront', 10)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('minBack', 1)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('minFront', 1)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('priority', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'source', [TNodeAudioClip, TNodeMovieTexture])); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('spatialize', TRUE));

  DefaultContainerField := 'children';
end;

class function TNodeSphere_2.ClassNodeTypeName: string;
begin
  Result := 'Sphere';
end;

class function TNodeSphere_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeSphere_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFFloat.Create('radius', 1));
end;

class function TNodeSphere_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

class function TNodeSphereSensor.ClassNodeTypeName: string;
begin
  Result := 'SphereSensor';
end;

class function TNodeSphereSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeSphereSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFRotation.Create('offset', Vector3Single(0, 1, 0), 0)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('rotation_changed', TSFRotation, false));

  DefaultContainerField := 'children';
end;

class function TNodeSpotLight_2.ClassNodeTypeName: string;
begin
  Result := 'SpotLight';
end;

class function TNodeSpotLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeSpotLight_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFFloat.Create('beamWidth', 1.570796)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('cutOffAngle', 0.785398)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('direction', Vector3Single(0, 0, -1))); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('radius', 100)); Fields.Last.Exposed := true;

  { Default value of ambientIntensity for VRML 1.0 and 2.0 is different,
    see comments at ambientIntensity in implementation of TPointLight_2.
    Same thing for location. }
  FdAmbientIntensity.Value := 0;
  FdAmbientIntensity.DefaultValue := 0;
  FdLocation.DefaultValue := ZeroVector3Single;
  FdLocation.Value := FdLocation.DefaultValue;

  DefaultContainerField := 'children';
end;

class function TNodeSpotLight_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeSpotLight_2.CreateActiveLight(
  State: TVRMLGraphTraverseState): TActiveLight;
begin
  Result := inherited;

  Result.TransfNormDirection :=
    Normalized( MultMatrixPointNoTranslation(Result.Transform,
      FdDirection.Value) );

  { TODO: For non-uniform scale, this is too easy,
    see TNodePointLight_2.CreateActiveLight for more comments. }
  Result.TransfRadius := FdRadius.Value * Result.AverageScaleTransform;
end;

class function TNodeSwitch_2.ClassNodeTypeName: string;
begin
  Result := 'Switch';
end;

class function TNodeSwitch_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeSwitch_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFNode.Create(Self, 'choice', AllowedChildrenNodes)); Fields.Last.Exposed := true;
  Fields.Add(TSFInt32.Create('whichChoice', -1)); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

class function TNodeSwitch_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor = 2;
end;

function TNodeSwitch_2.ChildrenField: TMFNode;
begin
  Result := FdChoice;
end;

procedure TNodeSwitch_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  if Between(FdWhichChoice.Value, 0, FdChoice.Count - 1) then
    Func(Self, FdChoice.Items[FdWhichChoice.Value]);
end;

class function TNodeText.ClassNodeTypeName: string;
begin
  Result := 'Text';
end;

class function TNodeText.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeText.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFString.Create('string', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'fontStyle', [TNodeFontStyle_2])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('length', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('maxExtent', 0.0)); Fields.Last.Exposed := true;
end;

function TNodeText.FontStyle: TNodeFontStyle_2;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TNodeFontStyle_2) then
    Result := TNodeFontStyle_2(FdFontStyle.Value) else
    Result := nil;
end;

class function TNodeTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'TextureCoordinate';
end;

class function TNodeTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTextureCoordinate.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFVec2f.Create('point', [])); Fields.Last.Exposed := true;

  DefaultContainerField := 'texCoord';
end;

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTextureTransform.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec2f.Create('center', Vector2Single(0, 0))); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('rotation', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec2f.Create('scale', Vector2Single(1, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFVec2f.Create('translation', Vector2Single(0, 0))); Fields.Last.Exposed := true;

  DefaultContainerField := 'textureTransform';
end;

function TNodeTextureTransform.Matrix: TMatrix4Single;
begin
  { Note: don't be fooled by a little confusing VRML 2.0 spec
    wording for TextureTransform, that suggests that VRML 2.0
    TextureTransform should multiply matrices in reversed
    order than VRML 1.0. I'm talking about the paragraph

       In matrix transformation notation, where Tc is the
       untransformed texture coordinate, Tc' is the transformed
       texture coordinate, C (center), T (translation),
       R (rotation), and S (scale) are the intermediate
       transformation matrices,

         Tc' = -C × S × R × C × T × Tc

    VRML TextureTransform node transforms texture *coordinates*.
    OpenGL texture matrix transforms texture *coordinates*.
    And above paragraph says about texture *coordinates*.
    So I'm either ultra-dumb or ultra-smart, but for me
    the above paragraph is wrong: it says precisely that I should load
    the *reversed* matrix of what I'm actually loading.
    If I would follow this, I would actually *not* be
    conforming to the rest of TextureTransform description in VRML spec.

    Am I the only one fooled by this ? No:
    [http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4320634]

    So what's the truth (i.e. the correct order, consistent
    with the whole rest of TextureTransform specification and with
    other implementations) ?
    Actually the order is the same for
    both VRML 1.0 and 2.0, confirmed by experience (reversed
    order just will not work correctly, e.g. TextureTransform
    with rotation and center 0.5 0.5 fields specified will
    not behave correctly) and other implementations
    (see [http://search.cpan.org/src/LUKKA/FreeWRL-0.14/VRMLFunc.xs]
    function TextureTransform_Rend). }

  Result :=
    TranslationMatrix( Vector3Single(
      FdTranslation.Value[0] + FdCenter.Value[0],
      FdTranslation.Value[1] + FdCenter.Value[1], 0));
  Result := MultMatrices(Result,
    RotationMatrixRad(FdRotation.Value, Vector3Single(0, 0, 1)));
  Result := MultMatrices(Result,
    ScalingMatrix(
      Vector3Single( FdScale.Value[0], FdScale.Value[1], 1 )));
  Result := MultMatrices(Result,
    TranslationMatrix(
      Vector3Single( -FdCenter.Value[0], -FdCenter.Value[1], 0 )));
end;

class function TNodeTimeSensor.ClassNodeTypeName: string;
begin
  Result := 'TimeSensor';
end;

class function TNodeTimeSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTimeSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFTime.Create('cycleInterval', 1)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('enabled', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('loop', FALSE)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('startTime', 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFTime.Create('stopTime', 0)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('cycleTime', TSFTime, false));
  Events.Add(TVRMLEvent.Create('fraction_changed', TSFFloat, false));
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));
  Events.Add(TVRMLEvent.Create('time', TSFTime, false));

  DefaultContainerField := 'children';
end;

class function TNodeTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'TouchSensor';
end;

class function TNodeTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTouchSensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Events.Add(TVRMLEvent.Create('hitNormal_changed', TSFVec3f, false));
  Events.Add(TVRMLEvent.Create('hitPoint_changed', TSFVec3f, false));
  Events.Add(TVRMLEvent.Create('hitTexCoord_changed', TSFVec2f, false));

  DefaultContainerField := 'children';
end;

class function TNodeTransform_2.ClassNodeTypeName: string;
begin
  Result := 'Transform';
end;

class function TNodeTransform_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTransform_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec3f.Create('center', ZeroVector3Single)); Fields.Last.Exposed := true;
  Fields.Add(TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('scale', Vector3Single(1, 1, 1))); Fields.Last.Exposed := true;
  Fields.Add(TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('translation', ZeroVector3Single)); Fields.Last.Exposed := true;

  DefaultContainerField := 'children';
end;

class function TNodeTransform_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure TNodeTransform_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

procedure TNodeTransform_2.BeforeTraverse(var State: TVRMLGraphTraverseState);
var
  M, IM, MRotateScaleOrient, IMRotateScaleOrient: TMatrix4Single;
begin
  inherited;

  { Inherited TNodeGeneralGrouping already saved State.Transform and such.

    To make InvertedTransform, we multiply inverted matrices in inverted order
    below. }

  TranslationMatrices(VectorAdd(FdTranslation.Value, FdCenter.Value), M, IM);
  State.Transform := MultMatrices(State.Transform, M);
  State.InvertedTransform := MultMatrices(IM, State.InvertedTransform);

  { Note that even Axis = zero is OK, both M and IM will be identity in
    this case. }
  RotationMatricesRad(FdRotation.RotationRad, FdRotation.Axis, M, IM);
  State.Transform := MultMatrices(State.Transform, M);
  State.InvertedTransform := MultMatrices(IM, State.InvertedTransform);

  if (FdScale.Value[0] <> 1) or
     (FdScale.Value[1] <> 1) or
     (FdScale.Value[2] <> 1) then
  begin
    RotationMatricesRad(FdScaleOrientation.RotationRad, FdScaleOrientation.Axis,
      MRotateScaleOrient, IMRotateScaleOrient);

    State.Transform := MultMatrices(State.Transform, MRotateScaleOrient);
    State.InvertedTransform := MultMatrices(IMRotateScaleOrient, State.InvertedTransform);

    { For scaling, we explicitly request that if ScalingFactor contains
      zero, IM will be forced to be identity. That's because VRML allows
      scaling factor to have 0 components (we need InvertedTransform only
      for special tricks). }

    ScalingMatrices(FdScale.Value, true, M, IM);
    State.Transform := MultMatrices(State.Transform, M);
    State.InvertedTransform := MultMatrices(IM, State.InvertedTransform);

    { That's right, we reuse MRotateScaleOrient and IMRotateScaleOrient
      matrices below. Since we want to reverse them now, so normal
      Transform is multiplied by IM and InvertedTransform is multiplied by M. }
    State.Transform := MultMatrices(State.Transform, IMRotateScaleOrient);
    State.InvertedTransform := MultMatrices(MRotateScaleOrient, State.InvertedTransform);
  end;

  TranslationMatrices(VectorNegate(FdCenter.Value), M, IM);
  State.Transform := MultMatrices(State.Transform, M);
  State.InvertedTransform := MultMatrices(IM, State.InvertedTransform);

  State.AverageScaleTransform *=
    (FdScale.Value[0] + FdScale.Value[1] + FdScale.Value[2]) / 3;
end;

class function TNodeTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'TrimmedSurface';
end;

class function TNodeTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeTrimmedSurface.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Events.Add(TVRMLEvent.Create('addTrimmingContour', TMFNode, true));
  Events.Add(TVRMLEvent.Create('removeTrimmingContour', TMFNode, true));
  Fields.Add(TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'surface', [TNodeNurbsSurface])); Fields.Last.Exposed := true;
end;

class function TNodeViewpoint.ClassNodeTypeName: string;
begin
  Result := 'Viewpoint';
end;

class function TNodeViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeViewpoint.Create(
  const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFFloat.Create('fieldOfView', DefaultViewpointFieldOfView));
  Fields.Last.Exposed := true;

  Fields.Add(TSFBool.Create('jump', TRUE));
  Fields.Last.Exposed := true;

  Fields.Add(TSFString.Create('description', ''));
  Fields.Last.Exposed := true;

  Fields.Add(TSFVec3f.Create('centerOfRotation', Vector3Single(0, 0, 0)));
  Fields.Last.Exposed := true;

  { Default value of position is different for Viewpoint than for VRML 1.0
    cameras (as set by TNodeGeneralViewpoint). }
  FdPosition.DefaultValue := Vector3Single(0, 0, 10);
  FdPosition.Value := FdPosition.DefaultValue;

  DefaultContainerField := 'children';
end;

class function TNodeViewpoint.CameraKind: TVRMLCameraKind;
begin
  Result := ckPerspective;
end;

function TNodeViewpoint.AngleOfView(
  const ThisToOtherSizeRatio: Single): Single;
begin
  Result := ViewpointAngleOfView(FdFieldOfView.Value, ThisToOtherSizeRatio);
end;

class function TNodeViewpoint.ViewpointAngleOfView(
  FieldOfView: Single;
  const ThisToOtherSizeRatio: Single): Single;
var
  OtherAngle: Single;
begin
  Clamp(FieldOfView, 0.01, Pi - 0.01);

  if ThisToOtherSizeRatio < 1 then
  begin
    { So the resulting angle is the smaller one. }
    Result := FieldOfView;
    OtherAngle :=
      AdjustViewAngleRadToAspectRatio(Result, 1 / ThisToOtherSizeRatio);
    if OtherAngle > Pi then
      Result := AdjustViewAngleRadToAspectRatio(Pi, ThisToOtherSizeRatio);
  end else
  begin
    { So the resulting angle is the larger one. }
    OtherAngle := FieldOfView;
    Result :=
      AdjustViewAngleRadToAspectRatio(OtherAngle, ThisToOtherSizeRatio);
    if Result > Pi then
      Result := Pi;
  end;
end;

class function TNodeVisibilitySensor.ClassNodeTypeName: string;
begin
  Result := 'VisibilitySensor';
end;

class function TNodeVisibilitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeVisibilitySensor.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TSFVec3f.Create('center', ZeroVector3Single)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('enabled', TRUE)); Fields.Last.Exposed := true;
  Fields.Add(TSFVec3f.Create('size', ZeroVector3Single)); Fields.Last.Exposed := true;
  Events.Add(TVRMLEvent.Create('enterTime', TSFTime, false));
  Events.Add(TVRMLEvent.Create('exitTime', TSFTime, false));
  Events.Add(TVRMLEvent.Create('isActive', TSFBool, false));

  DefaultContainerField := 'children';
end;

class function TNodeWorldInfo.ClassNodeTypeName: string;
begin
  Result := 'WorldInfo';
end;

class function TNodeWorldInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

constructor TNodeWorldInfo.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  Fields.Add(TMFString.Create('info', []));
  Fields.Add(TSFString.Create('title', ''));

  DefaultContainerField := 'children';
end;
{$endif read_implementation}
