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

  TNodeX3DGeometryNode = class(TNodeX3DNode)
  end;

  TNodeX3DGroupingNode = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdmetadata: TSFNode;
    public property Fdmetadata: TSFNode read FFdmetadata;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    function ChildrenField: TMFNode; override;
  end;

  TNodeAnchor = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdparameter: TMFString;
    public property Fdparameter: TMFString read FFdparameter;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

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

    private FFdmaterial: TSFNode;
    public property Fdmaterial: TSFNode read FFdmaterial;

    private FFdtexture: TSFNode;
    public property Fdtexture: TSFNode read FFdtexture;

    private FFdtextureTransform: TSFNode;
    public property FdtextureTransform: TSFNode read FFdtextureTransform;

    private FFdfillProperties: TSFNode;
    public property FdfillProperties: TSFNode read FFdfillProperties;

    private FFdlineProperties: TSFNode;
    public property FdlineProperties: TSFNode read FFdlineProperties;

    private FFdmetadata: TSFNode;
    public property Fdmetadata: TSFNode read FFdmetadata;

    private FFdshaders: TMFNode;
    public property Fdshaders: TMFNode read FFdshaders;

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
    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdpitch: TSFFloat;
    public property Fdpitch: TSFFloat read FFdpitch;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

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

    { Event: SFBool, in } { }
    private FEventset_bind: TVRMLEvent;
    public property Eventset_bind: TVRMLEvent read FEventset_bind;

    private FFdGroundAngle: TMFFloat;
    public property FdGroundAngle: TMFFloat read FFdGroundAngle;

    private FFdGroundColor: TMFColor;
    public property FdGroundColor: TMFColor read FFdGroundColor;

    private FFdBackUrl: TMFString;
    public property FdBackUrl: TMFString read FFdBackUrl;

    private FFdBottomUrl: TMFString;
    public property FdBottomUrl: TMFString read FFdBottomUrl;

    private FFdFrontUrl: TMFString;
    public property FdFrontUrl: TMFString read FFdFrontUrl;

    private FFdLeftUrl: TMFString;
    public property FdLeftUrl: TMFString read FFdLeftUrl;

    private FFdRightUrl: TMFString;
    public property FdRightUrl: TMFString read FFdRightUrl;

    private FFdTopUrl: TMFString;
    public property FdTopUrl: TMFString read FFdTopUrl;

    private FFdSkyAngle: TMFFloat;
    public property FdSkyAngle: TMFFloat read FFdSkyAngle;

    private FFdSkyColor: TMFColor;
    public property FdSkyColor: TMFColor read FFdSkyColor;

    { Event: SFBool, out } { }
    private FEventisBound: TVRMLEvent;
    public property EventisBound: TVRMLEvent read FEventisBound;

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

    private FFdaxisOfRotation: TSFVec3f;
    public property FdaxisOfRotation: TSFVec3f read FFdaxisOfRotation;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeBox = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

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

    private FFdcollide: TSFBool;
    public property Fdcollide: TSFBool read FFdcollide;

    private FFdproxy: TSFNode;
    public property Fdproxy: TSFNode read FFdproxy;

    { Event: SFTime, out } { }
    private FEventcollideTime: TVRMLEvent;
    public property EventcollideTime: TVRMLEvent read FEventcollideTime;

    { This should actually come from ancestor INodeX3DSensorNode } { }
    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeColor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcolor: TMFColor;
    public property Fdcolor: TMFColor read FFdcolor;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeColorInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFColor;
    public property FdkeyValue: TMFColor read FFdkeyValue;

    { Event: SFColor, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCone_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdbottomRadius: TSFFloat;
    public property FdbottomRadius: TSFFloat read FFdbottomRadius;

    private FFdheight: TSFFloat;
    public property Fdheight: TSFFloat read FFdheight;

    private FFdside: TSFBool;
    public property Fdside: TSFBool read FFdside;

    private FFdbottom: TSFBool;
    public property Fdbottom: TSFBool read FFdbottom;

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

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinate = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdpoint: TMFVec3f;
    public property Fdpoint: TMFVec3f read FFdpoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateDeformer = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdinputCoord: TMFNode;
    public property FdinputCoord: TMFNode read FFdinputCoord;

    private FFdinputTransform: TMFNode;
    public property FdinputTransform: TMFNode read FFdinputTransform;

    private FFdoutputCoord: TMFNode;
    public property FdoutputCoord: TMFNode read FFdoutputCoord;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    private FFdwDimension: TSFInt32;
    public property FdwDimension: TSFInt32 read FFdwDimension;

    private FFdwKnot: TMFFloat;
    public property FdwKnot: TMFFloat read FFdwKnot;

    private FFdwOrder: TSFInt32;
    public property FdwOrder: TSFInt32 read FFdwOrder;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCylinder_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdbottom: TSFBool;
    public property Fdbottom: TSFBool read FFdbottom;

    private FFdheight: TSFFloat;
    public property Fdheight: TSFFloat read FFdheight;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdside: TSFBool;
    public property Fdside: TSFBool read FFdside;

    private FFdtop: TSFBool;
    public property Fdtop: TSFBool read FFdtop;

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

    private FFddiskAngle: TSFFloat;
    public property FddiskAngle: TSFFloat read FFddiskAngle;

    private FFdmaxAngle: TSFFloat;
    public property FdmaxAngle: TSFFloat read FFdmaxAngle;

    private FFdminAngle: TSFFloat;
    public property FdminAngle: TSFFloat read FFdminAngle;

    private FFdoffset: TSFFloat;
    public property Fdoffset: TSFFloat read FFdoffset;

    private FFdaxisRotation: TSFRotation;
    public property FdaxisRotation: TSFRotation read FFdaxisRotation;

    { Event: SFRotation, out } { }
    private FEventrotation_changed: TVRMLEvent;
    public property Eventrotation_changed: TVRMLEvent read FEventrotation_changed;

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

    { Event: MFFloat, in } { }
    private FEventset_height: TVRMLEvent;
    public property Eventset_height: TVRMLEvent read FEventset_height;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdnormal: TSFNode;
    public property Fdnormal: TSFNode read FFdnormal;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFdheight: TMFFloat;
    public property Fdheight: TMFFloat read FFdheight;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdcreaseAngle: TSFFloat;
    public property FdcreaseAngle: TSFFloat read FFdcreaseAngle;

    private FFdnormalPerVertex: TSFBool;
    public property FdnormalPerVertex: TSFBool read FFdnormalPerVertex;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdxDimension: TSFInt32;
    public property FdxDimension: TSFInt32 read FFdxDimension;

    private FFdxSpacing: TSFFloat;
    public property FdxSpacing: TSFFloat read FFdxSpacing;

    private FFdzDimension: TSFInt32;
    public property FdzDimension: TSFInt32 read FFdzDimension;

    private FFdzSpacing: TSFFloat;
    public property FdzSpacing: TSFFloat read FFdzSpacing;

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

  TNodeExtrusion = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: MFVec2f, in } { }
    private FEventset_crossSection: TVRMLEvent;
    public property Eventset_crossSection: TVRMLEvent read FEventset_crossSection;

    { Event: MFRotation, in } { }
    private FEventset_orientation: TVRMLEvent;
    public property Eventset_orientation: TVRMLEvent read FEventset_orientation;

    { Event: MFVec2f, in } { }
    private FEventset_scale: TVRMLEvent;
    public property Eventset_scale: TVRMLEvent read FEventset_scale;

    { Event: MFVec3f, in } { }
    private FEventset_spine: TVRMLEvent;
    public property Eventset_spine: TVRMLEvent read FEventset_spine;

    private FFdbeginCap: TSFBool;
    public property FdbeginCap: TSFBool read FFdbeginCap;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdconvex: TSFBool;
    public property Fdconvex: TSFBool read FFdconvex;

    private FFdcreaseAngle: TSFFloat;
    public property FdcreaseAngle: TSFFloat read FFdcreaseAngle;

    private FFdcrossSection: TMFVec2f;
    public property FdcrossSection: TMFVec2f read FFdcrossSection;

    private FFdendCap: TSFBool;
    public property FdendCap: TSFBool read FFdendCap;

    private FFdorientation: TMFRotation;
    public property Fdorientation: TMFRotation read FFdorientation;

    private FFdscale: TMFVec2f;
    public property Fdscale: TMFVec2f read FFdscale;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdspine: TMFVec3f;
    public property Fdspine: TMFVec3f read FFdspine;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { For given Index, return appropriate FdCrossIndex item as 3D vertex.
      That is, uses FdCrossIndex values as X, Z of 3D vertex and sets Y = 0
      (that's how Extrusion is defined in VRML / X3D). }
    function CrossSection3D(Index: integer): TVector3Single;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeFog = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcolor: TSFColor;
    public property Fdcolor: TSFColor read FFdcolor;

    private FFdfogType: TSFString;
    public property FdfogType: TSFString read FFdfogType;

    private FFdvisibilityRange: TSFFloat;
    public property FdvisibilityRange: TSFFloat read FFdvisibilityRange;

    private FFdVolumetric: TSFBool;
    public property FdVolumetric: TSFBool read FFdVolumetric;

    private FFdVolumetricDirection: TSFVec3f;
    public property FdVolumetricDirection: TSFVec3f read FFdVolumetricDirection;

    private FFdVolumetricVisibilityStart: TSFFloat;
    public property FdVolumetricVisibilityStart: TSFFloat read FFdVolumetricVisibilityStart;

    private FFdAlternative: TSFNode;
    public property FdAlternative: TSFNode read FFdAlternative;

    { Event: SFBool, in } { }
    private FEventset_bind: TVRMLEvent;
    public property Eventset_bind: TVRMLEvent read FEventset_bind;

    { Event: SFBool, out } { }
    private FEventisBound: TVRMLEvent;
    public property EventisBound: TVRMLEvent read FEventisBound;

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
    private FFdfamily: TMFString;
    public property Fdfamily: TMFString read FFdfamily;

    private FFdhorizontal: TSFBool;
    public property Fdhorizontal: TSFBool read FFdhorizontal;

    private FFdjustify: TMFString;
    public property Fdjustify: TMFString read FFdjustify;

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;

    private FFdleftToRight: TSFBool;
    public property FdleftToRight: TSFBool read FFdleftToRight;

    private FFdsize: TSFFloat;
    public property Fdsize: TSFFloat read FFdsize;

    private FFdspacing: TSFFloat;
    public property Fdspacing: TSFFloat read FFdspacing;

    private FFdstyle: TSFString;
    public property Fdstyle: TSFString read FFdstyle;

    private FFdtopToBottom: TSFBool;
    public property FdtopToBottom: TSFBool read FFdtopToBottom;

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
    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdpoint: TMFString;
    public property Fdpoint: TMFString read FFdpoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoElevationGrid = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: MFFloat, in } { }
    private FEventset_height: TVRMLEvent;
    public property Eventset_height: TVRMLEvent read FEventset_height;

    { Event: SFFloat, in } { }
    private FEventset_yScale: TVRMLEvent;
    public property Eventset_yScale: TVRMLEvent read FEventset_yScale;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdnormal: TSFNode;
    public property Fdnormal: TSFNode read FFdnormal;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdcreaseAngle: TSFFloat;
    public property FdcreaseAngle: TSFFloat read FFdcreaseAngle;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdgeoGridOrigin: TSFString;
    public property FdgeoGridOrigin: TSFString read FFdgeoGridOrigin;

    private FFdheight: TMFFloat;
    public property Fdheight: TMFFloat read FFdheight;

    private FFdnormalPerVertex: TSFBool;
    public property FdnormalPerVertex: TSFBool read FFdnormalPerVertex;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdxDimension: TSFInt32;
    public property FdxDimension: TSFInt32 read FFdxDimension;

    private FFdxSpacing: TSFString;
    public property FdxSpacing: TSFString read FFdxSpacing;

    private FFdyScale: TSFFloat;
    public property FdyScale: TSFFloat read FFdyScale;

    private FFdzDimension: TSFInt32;
    public property FdzDimension: TSFInt32 read FFdzDimension;

    private FFdzSpacing: TSFString;
    public property FdzSpacing: TSFString read FFdzSpacing;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoLocation = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdgeoCoords: TSFString;
    public property FdgeoCoords: TSFString read FFdgeoCoords;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoLOD = class(TNodeGeneralGrouping)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcenter: TSFString;
    public property Fdcenter: TSFString read FFdcenter;

    private FFdchild1Url: TMFString;
    public property Fdchild1Url: TMFString read FFdchild1Url;

    private FFdchild2Url: TMFString;
    public property Fdchild2Url: TMFString read FFdchild2Url;

    private FFdchild3Url: TMFString;
    public property Fdchild3Url: TMFString read FFdchild3Url;

    private FFdchild4Url: TMFString;
    public property Fdchild4Url: TMFString read FFdchild4Url;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdrange: TSFFloat;
    public property Fdrange: TSFFloat read FFdrange;

    private FFdrootUrl: TMFString;
    public property FdrootUrl: TMFString read FFdrootUrl;

    private FFdrootNode: TMFNode;
    public property FdrootNode: TMFNode read FFdrootNode;

    { Event: MFNode, out } { }
    private FEventchildren: TVRMLEvent;
    public property Eventchildren: TVRMLEvent read FEventchildren;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoMetadata = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFddata: TMFNode;
    public property Fddata: TMFNode read FFddata;

    private FFdsummary: TMFString;
    public property Fdsummary: TMFString read FFdsummary;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoOrigin = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdgeoCoords: TSFString;
    public property FdgeoCoords: TSFString read FFdgeoCoords;

    private FFdrotateYUp: TSFBool;
    public property FdrotateYUp: TSFBool read FFdrotateYUp;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoPositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFString;
    public property FdkeyValue: TMFString read FFdkeyValue;

    { Event: SFString, out } { }
    private FEventgeovalue_changed: TVRMLEvent;
    public property Eventgeovalue_changed: TVRMLEvent read FEventgeovalue_changed;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoTouchSensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    { Event: SFVec3f, out } { }
    private FEventhitNormal_changed: TVRMLEvent;
    public property EventhitNormal_changed: TVRMLEvent read FEventhitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventhitPoint_changed: TVRMLEvent;
    public property EventhitPoint_changed: TVRMLEvent read FEventhitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventhitTexCoord_changed: TVRMLEvent;
    public property EventhitTexCoord_changed: TVRMLEvent read FEventhitTexCoord_changed;

    { Event: SFString, out } { }
    private FEventhitGeoCoord_changed: TVRMLEvent;
    public property EventhitGeoCoord_changed: TVRMLEvent read FEventhitGeoCoord_changed;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisOver: TVRMLEvent;
    public property EventisOver: TVRMLEvent read FEventisOver;

    { Event: SFTime, out } { }
    private FEventtouchTime: TVRMLEvent;
    public property EventtouchTime: TVRMLEvent read FEventtouchTime;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeGeoViewpoint = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFBool, in } { }
    private FEventset_bind: TVRMLEvent;
    public property Eventset_bind: TVRMLEvent read FEventset_bind;

    { Event: SFString, in } { }
    private FEventset_orientation: TVRMLEvent;
    public property Eventset_orientation: TVRMLEvent read FEventset_orientation;

    { Event: SFString, in } { }
    private FEventset_position: TVRMLEvent;
    public property Eventset_position: TVRMLEvent read FEventset_position;

    private FFdfieldOfView: TSFFloat;
    public property FdfieldOfView: TSFFloat read FFdfieldOfView;

    private FFdheadlight: TSFBool;
    public property Fdheadlight: TSFBool read FFdheadlight;

    private FFdjump: TSFBool;
    public property Fdjump: TSFBool read FFdjump;

    private FFdnavType: TMFString;
    public property FdnavType: TMFString read FFdnavType;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdgeoOrigin: TSFNode;
    public property FdgeoOrigin: TSFNode read FFdgeoOrigin;

    private FFdgeoSystem: TMFString;
    public property FdgeoSystem: TMFString read FFdgeoSystem;

    private FFdorientation: TSFRotation;
    public property Fdorientation: TSFRotation read FFdorientation;

    private FFdposition: TSFString;
    public property Fdposition: TSFString read FFdposition;

    private FFdspeedFactor: TSFFloat;
    public property FdspeedFactor: TSFFloat read FFdspeedFactor;

    { Event: SFTime, out } { }
    private FEventbindTime: TVRMLEvent;
    public property EventbindTime: TVRMLEvent read FEventbindTime;

    { Event: SFBool, out } { }
    private FEventisBound: TVRMLEvent;
    public property EventisBound: TVRMLEvent read FEventisBound;

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

  { A Group node that is added when VRML >= 2.0 file contains more
    than one node at the root level. (Or contains any prototypes,
    routes etc. at the root level, as they must all be stored inside
    some node's properties).

    See TNodeGroupHidden_1 for some more comments about why this
    descends from TNodeGroup_2. }
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

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

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

    { Event: MFInt32, in } { }
    private FEventset_colorIndex: TVRMLEvent;
    public property Eventset_colorIndex: TVRMLEvent read FEventset_colorIndex;

    { Event: MFInt32, in } { }
    private FEventset_coordIndex: TVRMLEvent;
    public property Eventset_coordIndex: TVRMLEvent read FEventset_coordIndex;

    { Event: MFInt32, in } { }
    private FEventset_normalIndex: TVRMLEvent;
    public property Eventset_normalIndex: TVRMLEvent read FEventset_normalIndex;

    { Event: MFInt32, in } { }
    private FEventset_texCoordIndex: TVRMLEvent;
    public property Eventset_texCoordIndex: TVRMLEvent read FEventset_texCoordIndex;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFdnormal: TSFNode;
    public property Fdnormal: TSFNode read FFdnormal;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdcolorIndex: TMFInt32;
    public property FdcolorIndex: TMFInt32 read FFdcolorIndex;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdconvex: TSFBool;
    public property Fdconvex: TSFBool read FFdconvex;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFdcreaseAngle: TSFFloat;
    public property FdcreaseAngle: TSFFloat read FFdcreaseAngle;

    private FFdnormalIndex: TMFInt32;
    public property FdnormalIndex: TMFInt32 read FFdnormalIndex;

    private FFdnormalPerVertex: TSFBool;
    public property FdnormalPerVertex: TSFBool read FFdnormalPerVertex;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdtexCoordIndex: TMFInt32;
    public property FdtexCoordIndex: TMFInt32 read FFdtexCoordIndex;

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

    { Event: MFInt32, in } { }
    private FEventset_colorIndex: TVRMLEvent;
    public property Eventset_colorIndex: TVRMLEvent read FEventset_colorIndex;

    { Event: MFInt32, in } { }
    private FEventset_coordIndex: TVRMLEvent;
    public property Eventset_coordIndex: TVRMLEvent read FEventset_coordIndex;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFdcolorIndex: TMFInt32;
    public property FdcolorIndex: TMFInt32 read FFdcolorIndex;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

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

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

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

    private FFdload: TSFBool;
    public property Fdload: TSFBool read FFdload;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    { Event: MFNode, out } { }
    private FEventchildren: TVRMLEvent;
    public property Eventchildren: TVRMLEvent read FEventchildren;

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

    private FFdlevel: TMFNode;
    public property Fdlevel: TMFNode read FFdlevel;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrange: TMFFloat;
    public property Fdrange: TMFFloat read FFdrange;

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

    private FFdambientIntensity: TSFFloat;
    public property FdambientIntensity: TSFFloat read FFdambientIntensity;

    private FFddiffuseColor: TSFColor;
    public property FddiffuseColor: TSFColor read FFddiffuseColor;

    private FFdemissiveColor: TSFColor;
    public property FdemissiveColor: TSFColor read FFdemissiveColor;

    private FFdshininess: TSFFloat;
    public property Fdshininess: TSFFloat read FFdshininess;

    private FFdspecularColor: TSFColor;
    public property FdspecularColor: TSFColor read FFdspecularColor;

    private FFdtransparency: TSFFloat;
    public property Fdtransparency: TSFFloat read FFdtransparency;

    private FFdFogImmune: TSFBool;
    public property FdFogImmune: TSFBool read FFdFogImmune;

    private FFdMirror: TSFFloat;
    public property FdMirror: TSFFloat read FFdMirror;

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

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

    { Event: SFTime, out } { }
    private FEventduration_changed: TVRMLEvent;
    public property Eventduration_changed: TVRMLEvent read FEventduration_changed;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNavigationInfo = class(TNodeX3DBindableNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdavatarSize: TMFFloat;
    public property FdavatarSize: TMFFloat read FFdavatarSize;

    private FFdheadlight: TSFBool;
    public property Fdheadlight: TSFBool read FFdheadlight;

    private FFdspeed: TSFFloat;
    public property Fdspeed: TSFFloat read FFdspeed;

    private FFdtransitionTime: TSFTime;
    public property FdtransitionTime: TSFTime read FFdtransitionTime;

    private FFdtransitionType: TMFString;
    public property FdtransitionType: TMFString read FFdtransitionType;

    private FFdtype: TMFString;
    public property Fdtype: TMFString read FFdtype;

    private FFdvisibilityLimit: TSFFloat;
    public property FdvisibilityLimit: TSFFloat read FFdvisibilityLimit;

    { Event: SFBool, out } { }
    private FEventtransitionComplete: TVRMLEvent;
    public property EventtransitionComplete: TVRMLEvent read FEventtransitionComplete;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNormal = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdvector: TMFVec3f;
    public property Fdvector: TMFVec3f read FFdvector;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNormalInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsCurve = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsCurve2D = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsGroup = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsPositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFddimension: TSFInt32;
    public property Fddimension: TSFInt32 read FFddimension;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    private FFdkeyWeight: TMFFloat;
    public property FdkeyWeight: TMFFloat read FFdkeyWeight;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsTextureSurface = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeOrientationInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFRotation;
    public property FdkeyValue: TMFRotation read FFdkeyValue;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

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

    private FFdimage: TSFImage;
    public property Fdimage: TSFImage read FFdimage;

    private FFdrepeatS: TSFBool;
    public property FdrepeatS: TSFBool read FFdrepeatS;

    private FFdrepeatT: TSFBool;
    public property FdrepeatT: TSFBool read FFdrepeatT;

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

    private FFdmaxPosition: TSFVec2f;
    public property FdmaxPosition: TSFVec2f read FFdmaxPosition;

    private FFdminPosition: TSFVec2f;
    public property FdminPosition: TSFVec2f read FFdminPosition;

    private FFdoffset: TSFVec3f;
    public property Fdoffset: TSFVec3f read FFdoffset;

    private FFdaxisRotation: TSFRotation;
    public property FdaxisRotation: TSFRotation read FFdaxisRotation;

    { Event: SFVec3f, out } { }
    private FEventtranslation_changed: TVRMLEvent;
    public property Eventtranslation_changed: TVRMLEvent read FEventtranslation_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodePointLight_2 = class(TNodeGeneralPointLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

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

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

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

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodePositionInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeProximitySensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;

    { Event: SFRotation, out } { }
    private FEventorientation_changed: TVRMLEvent;
    public property Eventorientation_changed: TVRMLEvent read FEventorientation_changed;

    { Event: SFTime, out } { }
    private FEvententerTime: TVRMLEvent;
    public property EvententerTime: TVRMLEvent read FEvententerTime;

    { Event: SFTime, out } { }
    private FEventexitTime: TVRMLEvent;
    public property EventexitTime: TVRMLEvent read FEventexitTime;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeScalarInterpolator = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    private FFdkeyValue: TMFFloat;
    public property FdkeyValue: TMFFloat read FFdkeyValue;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeScript = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ClassNodeTypeName: string; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

    private FFddirectOutput: TSFBool;
    public property FddirectOutput: TSFBool read FFddirectOutput;

    private FFdmustEvaluate: TSFBool;
    public property FdmustEvaluate: TSFBool read FFdmustEvaluate;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTextureTransform = class;

  TNodeShape = class(TVRMLNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    private FFdappearance: TSFNode;
    public property Fdappearance: TSFNode read FFdappearance;

    private FFdgeometry: TSFNode;
    public property Fdgeometry: TSFNode read FFdgeometry;

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

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdintensity: TSFFloat;
    public property Fdintensity: TSFFloat read FFdintensity;

    private FFdlocation: TSFVec3f;
    public property Fdlocation: TSFVec3f read FFdlocation;

    private FFdmaxBack: TSFFloat;
    public property FdmaxBack: TSFFloat read FFdmaxBack;

    private FFdmaxFront: TSFFloat;
    public property FdmaxFront: TSFFloat read FFdmaxFront;

    private FFdminBack: TSFFloat;
    public property FdminBack: TSFFloat read FFdminBack;

    private FFdminFront: TSFFloat;
    public property FdminFront: TSFFloat read FFdminFront;

    private FFdpriority: TSFFloat;
    public property Fdpriority: TSFFloat read FFdpriority;

    private FFdsource: TSFNode;
    public property Fdsource: TSFNode read FFdsource;

    private FFdspatialize: TSFBool;
    public property Fdspatialize: TSFBool read FFdspatialize;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSphere_2 = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

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

    private FFdoffset: TSFRotation;
    public property Fdoffset: TSFRotation read FFdoffset;

    { Event: SFRotation, out } { }
    private FEventrotation_changed: TVRMLEvent;
    public property Eventrotation_changed: TVRMLEvent read FEventrotation_changed;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSpotLight_2 = class(TNodeGeneralPositionalLight)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdbeamWidth: TSFFloat;
    public property FdbeamWidth: TSFFloat read FFdbeamWidth;

    private FFdcutOffAngle: TSFFloat;
    public property FdcutOffAngle: TSFFloat read FFdcutOffAngle;

    private FFddirection: TSFVec3f;
    public property Fddirection: TSFVec3f read FFddirection;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

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

    private FFdchoice: TMFNode;
    public property Fdchoice: TMFNode read FFdchoice;

    private FFdwhichChoice: TSFInt32;
    public property FdwhichChoice: TSFInt32 read FFdwhichChoice;

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

    private FFdstring: TMFString;
    public property Fdstring: TMFString read FFdstring;

    private FFdfontStyle: TSFNode;
    public property FdfontStyle: TSFNode read FFdfontStyle;

    private FFdlength: TMFFloat;
    public property Fdlength: TMFFloat read FFdlength;

    private FFdmaxExtent: TSFFloat;
    public property FdmaxExtent: TSFFloat read FFdmaxExtent;

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

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTextureTransform = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcenter: TSFVec2f;
    public property Fdcenter: TSFVec2f read FFdcenter;

    private FFdrotation: TSFFloat;
    public property Fdrotation: TSFFloat read FFdrotation;

    private FFdscale: TSFVec2f;
    public property Fdscale: TSFVec2f read FFdscale;

    private FFdtranslation: TSFVec2f;
    public property Fdtranslation: TSFVec2f read FFdtranslation;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function Matrix: TMatrix4Single;
  end;

  TNodeTimeSensor = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcycleInterval: TSFTime;
    public property FdcycleInterval: TSFTime read FFdcycleInterval;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    { Event: SFTime, out } { }
    private FEventcycleTime: TVRMLEvent;
    public property EventcycleTime: TVRMLEvent read FEventcycleTime;

    { Event: SFFloat, out } { }
    private FEventfraction_changed: TVRMLEvent;
    public property Eventfraction_changed: TVRMLEvent read FEventfraction_changed;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFTime, out } { }
    private FEventtime: TVRMLEvent;
    public property Eventtime: TVRMLEvent read FEventtime;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTouchSensor = class(TNodeX3DTouchSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    { Event: SFVec3f, out } { }
    private FEventhitNormal_changed: TVRMLEvent;
    public property EventhitNormal_changed: TVRMLEvent read FEventhitNormal_changed;

    { Event: SFVec3f, out } { }
    private FEventhitPoint_changed: TVRMLEvent;
    public property EventhitPoint_changed: TVRMLEvent read FEventhitPoint_changed;

    { Event: SFVec2f, out } { }
    private FEventhitTexCoord_changed: TVRMLEvent;
    public property EventhitTexCoord_changed: TVRMLEvent read FEventhitTexCoord_changed;

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

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

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

    { Event: MFNode, in } { }
    private FEventaddTrimmingContour: TVRMLEvent;
    public property EventaddTrimmingContour: TVRMLEvent read FEventaddTrimmingContour;

    { Event: MFNode, in } { }
    private FEventremoveTrimmingContour: TVRMLEvent;
    public property EventremoveTrimmingContour: TVRMLEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

    private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeViewpoint = class(TNodeGeneralViewpoint)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdfieldOfView: TSFFloat;
    public property FdfieldOfView: TSFFloat read FFdfieldOfView;

    private FFdjump: TSFBool;
    public property Fdjump: TSFBool read FFdjump;

    private FFddescription: TSFString;
    public property Fddescription: TSFString read FFddescription;

    private FFdcenterOfRotation: TSFVec3f;
    public property FdcenterOfRotation: TSFVec3f read FFdcenterOfRotation;

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

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    { Event: SFTime, out } { }
    private FEvententerTime: TVRMLEvent;
    public property EvententerTime: TVRMLEvent read FEvententerTime;

    { Event: SFTime, out } { }
    private FEventexitTime: TVRMLEvent;
    public property EventexitTime: TVRMLEvent read FEventexitTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeWorldInfo = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    private FFdinfo: TMFString;
    public property Fdinfo: TMFString read FFdinfo;

    private FFdtitle: TSFString;
    public property Fdtitle: TSFString read FFdtitle;

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

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', AllowedChildrenNodes);
  Fields.Add(FFdchildren);

  FFdmetadata := TSFNode.Create(Self, 'metadata', [TNodeX3DMetadataObject]);
  Fields.Add(FFdmetadata);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
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

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdparameter := TMFString.Create('parameter', []);
  Fields.Add(FFdparameter);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

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

  FFdmaterial := TSFNode.Create(Self, 'material', [TNodeMaterial_2]);
  Fields.Add(FFdmaterial);

  FFdtexture := TSFNode.Create(Self, 'texture', [TNodeImageTexture, TNodeMovieTexture, TNodePixelTexture]);
  Fields.Add(FFdtexture);

  FFdtextureTransform := TSFNode.Create(Self, 'textureTransform', [TNodeTextureTransform]);
  Fields.Add(FFdtextureTransform);

  FFdfillProperties := TSFNode.Create(Self, 'fillProperties', [{TNodeFillProperties}]);
  Fields.Add(FFdfillProperties);

  FFdlineProperties := TSFNode.Create(Self, 'lineProperties', [{TNodeLineProperties}]);
  Fields.Add(FFdlineProperties);

  FFdmetadata := TSFNode.Create(Self, 'metadata', [{TNodeX3DMetadataObject}]);
  Fields.Add(FFdmetadata);

  FFdshaders := TMFNode.Create(Self, 'shaders', [TNodeComposedShader, TNodePackagedShader,  TNodeProgramShader]);
  Fields.Add(FFdshaders);

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

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdloop := TSFBool.Create('loop', FALSE);
  Fields.Add(FFdloop);

  FFdpitch := TSFFloat.Create('pitch', 1.0);
  Fields.Add(FFdpitch);

  FFdstartTime := TSFTime.Create('startTime', 0);
  Fields.Add(FFdstartTime);

  FFdstopTime := TSFTime.Create('stopTime', 0);
  Fields.Add(FFdstopTime);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FEventduration_changed := TVRMLEvent.Create('duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  DefaultContainerField := 'children';
end;

constructor TNodeBackground.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FEventset_bind := TVRMLEvent.Create('set_bind', TSFBool, true);
  Events.Add(FEventset_bind);

  FFdgroundAngle := TMFFloat.Create('groundAngle', []);
  Fields.Add(FFdgroundAngle);

  FFdgroundColor := TMFColor.Create('groundColor', []);
  Fields.Add(FFdgroundColor);

  FFdbackUrl := TMFString.Create('backUrl', []);
  Fields.Add(FFdbackUrl);

  FFdbottomUrl := TMFString.Create('bottomUrl', []);
  Fields.Add(FFdbottomUrl);

  FFdfrontUrl := TMFString.Create('frontUrl', []);
  Fields.Add(FFdfrontUrl);

  FFdleftUrl := TMFString.Create('leftUrl', []);
  Fields.Add(FFdleftUrl);

  FFdrightUrl := TMFString.Create('rightUrl', []);
  Fields.Add(FFdrightUrl);

  FFdtopUrl := TMFString.Create('topUrl', []);
  Fields.Add(FFdtopUrl);

  FFdskyAngle := TMFFloat.Create('skyAngle', []);
  Fields.Add(FFdskyAngle);

  FFdskyColor := TMFColor.Create('skyColor', [ZeroVector3Single]);
  Fields.Add(FFdskyColor);

  FEventisBound := TVRMLEvent.Create('isBound', TSFBool, false);
  Events.Add(FEventisBound);

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

  FFdaxisOfRotation := TSFVec3f.Create('axisOfRotation', Vector3Single(0, 1, 0));
  Fields.Add(FFdaxisOfRotation);

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

  FFdsize := TSFVec3f.Create('size', Vector3Single(2, 2, 2));
  Fields.Add(FFdsize);
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

  FFdcollide := TSFBool.Create('collide', TRUE);
  Fields.Add(FFdcollide);

  FFdproxy := TSFNode.Create(Self, 'proxy', AllowedChildrenNodes);
  FFdproxy.Exposed := false;
  Fields.Add(FFdproxy);

  FEventcollideTime := TVRMLEvent.Create('collideTime', TSFTime, false);
  Events.Add(FEventcollideTime);

  FFdenabled := TSFBool.Create('enabled', true);
  Fields.Add(FFdenabled);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

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

  FFdcolor := TMFColor.Create('color', []);
  Fields.Add(FFdcolor);

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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFColor.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFColor, false);
  Events.Add(FEventvalue_changed);

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

  FFdbottomRadius := TSFFloat.Create('bottomRadius', 1);
  FFdbottomRadius.Exposed := false;
  Fields.Add(FFdbottomRadius);

  FFdheight := TSFFloat.Create('height', 2);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);

  FFdside := TSFBool.Create('side', true);
  FFdside.Exposed := false;
  Fields.Add(FFdside);

  FFdbottom := TSFBool.Create('bottom', true);
  FFdbottom.Exposed := false;
  Fields.Add(FFdbottom);
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

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeNurbsCurve2D, TNodePolyline2D, TNodeContour2D]);
  Fields.Add(FFdchildren);

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

  FFdpoint := TMFVec3f.Create('point', []);
  Fields.Add(FFdpoint);

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

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', AllowedChildrenNodes);
  Fields.Add(FFdchildren);

  FFdcontrolPoint := TMFVec3f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdinputCoord := TMFNode.Create(Self, 'inputCoord', [TNodeCoordinate]);
  Fields.Add(FFdinputCoord);

  FFdinputTransform := TMFNode.Create(Self, 'inputTransform', [TNodeTransform_2]);
  Fields.Add(FFdinputTransform);

  FFdoutputCoord := TMFNode.Create(Self, 'outputCoord', [TNodeCoordinate]);
  Fields.Add(FFdoutputCoord);

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create('uOrder', 2);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create('vOrder', 2);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);

  FFdwDimension := TSFInt32.Create('wDimension', 0);
  FFdwDimension.Exposed := false;
  Fields.Add(FFdwDimension);

  FFdwKnot := TMFFloat.Create('wKnot', []);
  FFdwKnot.Exposed := false;
  Fields.Add(FFdwKnot);

  FFdwOrder := TSFInt32.Create('wOrder', 2);
  FFdwOrder.Exposed := false;
  Fields.Add(FFdwOrder);

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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

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

  FFdbottom := TSFBool.Create('bottom', TRUE);
  FFdbottom.Exposed := false;
  Fields.Add(FFdbottom);

  FFdheight := TSFFloat.Create('height', 2);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);

  FFdside := TSFBool.Create('side', TRUE);
  FFdside.Exposed := false;
  Fields.Add(FFdside);

  FFdtop := TSFBool.Create('top', TRUE);
  FFdtop.Exposed := false;
  Fields.Add(FFdtop);
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

  FFddiskAngle := TSFFloat.Create('diskAngle', Pi / 12);
  Fields.Add(FFddiskAngle);

  FFdmaxAngle := TSFFloat.Create('maxAngle', -1);
  Fields.Add(FFdmaxAngle);

  FFdminAngle := TSFFloat.Create('minAngle', 0);
  Fields.Add(FFdminAngle);

  FFdoffset := TSFFloat.Create('offset', 0);
  Fields.Add(FFdoffset);

  FFdaxisRotation := TSFRotation.Create('axisRotation', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdaxisRotation);

  FEventrotation_changed := TVRMLEvent.Create('rotation_changed', TSFRotation, false);
  Events.Add(FEventrotation_changed);

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

  FEventset_height := TVRMLEvent.Create('set_height', TMFFloat, true);
  Events.Add(FEventset_height);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeColor]);
  Fields.Add(FFdcolor);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeNormal]);
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate]);
  Fields.Add(FFdtexCoord);

  FFdheight := TMFFloat.Create('height', []);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);

  FFdccw := TSFBool.Create('ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', TRUE);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);

  FFdnormalPerVertex := TSFBool.Create('normalPerVertex', TRUE);
  FFdnormalPerVertex.Exposed := false;
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create('solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdxDimension := TSFInt32.Create('xDimension', 0);
  FFdxDimension.Exposed := false;
  Fields.Add(FFdxDimension);

  FFdxSpacing := TSFFloat.Create('xSpacing', 1.0);
  FFdxSpacing.Exposed := false;
  Fields.Add(FFdxSpacing);

  FFdzDimension := TSFInt32.Create('zDimension', 0);
  FFdzDimension.Exposed := false;
  Fields.Add(FFdzDimension);

  FFdzSpacing := TSFFloat.Create('zSpacing', 1.0);
  FFdzSpacing.Exposed := false;
  Fields.Add(FFdzSpacing);
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

  FEventset_crossSection := TVRMLEvent.Create('set_crossSection', TMFVec2f, true);
  Events.Add(FEventset_crossSection);

  FEventset_orientation := TVRMLEvent.Create('set_orientation', TMFRotation, true);
  Events.Add(FEventset_orientation);

  FEventset_scale := TVRMLEvent.Create('set_scale', TMFVec2f, true);
  Events.Add(FEventset_scale);

  FEventset_spine := TVRMLEvent.Create('set_spine', TMFVec3f, true);
  Events.Add(FEventset_spine);

  FFdbeginCap := TSFBool.Create('beginCap', TRUE);
  FFdbeginCap.Exposed := false;
  Fields.Add(FFdbeginCap);

  FFdccw := TSFBool.Create('ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdconvex := TSFBool.Create('convex', TRUE);
  FFdconvex.Exposed := false;
  Fields.Add(FFdconvex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);

  FFdcrossSection := TMFVec2f.Create('crossSection', [ Vector2Single(1, 1), Vector2Single(1, -1), Vector2Single(-1, -1),  Vector2Single(-1, 1),  Vector2Single(1, 1) ]);
  FFdcrossSection.Exposed := false;
  Fields.Add(FFdcrossSection);

  FFdendCap := TSFBool.Create('endCap', TRUE);
  FFdendCap.Exposed := false;
  Fields.Add(FFdendCap);

  FFdorientation := TMFRotation.Create('orientation', [ Vector4Single(0, 0, 1, 0) ] );
  FFdorientation.Exposed := false;
  Fields.Add(FFdorientation);

  FFdscale := TMFVec2f.Create('scale', Vector2Single(1, 1));
  FFdscale.Exposed := false;
  Fields.Add(FFdscale);

  FFdsolid := TSFBool.Create('solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdspine := TMFVec3f.Create('spine', [ Vector3Single(0, 0, 0), Vector3Single(0, 1, 0) ]);
  FFdspine.Exposed := false;
  Fields.Add(FFdspine);
end;

function TNodeExtrusion.CrossSection3D(Index: integer): TVector3Single;
begin
  Result[0] := FdCrossSection.Items.Items[Index][0];
  Result[1] := 0;
  Result[2] := FdCrossSection.Items.Items[Index][1];
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

  FFdcolor := TSFColor.Create('color', Vector3Single(1, 1, 1));
  Fields.Add(FFdcolor);

  FFdfogType := TSFString.Create('fogType', 'LINEAR');
  Fields.Add(FFdfogType);

  FFdvisibilityRange := TSFFloat.Create('visibilityRange', 0);
  Fields.Add(FFdvisibilityRange);

  FFdvolumetric := TSFBool.Create('volumetric', false);
  Fields.Add(FFdvolumetric);

  FFdvolumetricDirection := TSFVec3f.Create('volumetricDirection', Vector3Single(0, -1, 0));
  Fields.Add(FFdvolumetricDirection);

  FFdvolumetricVisibilityStart := TSFFloat.Create('volumetricVisibilityStart', 0);
  Fields.Add(FFdvolumetricVisibilityStart);

  FFdalternative := TSFNode.Create(Self, 'alternative', [TNodeFog]);
  Fields.Add(FFdalternative);

  FEventset_bind := TVRMLEvent.Create('set_bind', TSFBool, true);
  Events.Add(FEventset_bind);

  FEventisBound := TVRMLEvent.Create('isBound', TSFBool, false);
  Events.Add(FEventisBound);

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

  FFdfamily := TMFString.Create('family', ['SERIF']);
  Fields.Add(FFdfamily);

  FFdhorizontal := TSFBool.Create('horizontal', TRUE);
  Fields.Add(FFdhorizontal);

  FFdjustify := TMFString.Create('justify', ['BEGIN']);
  Fields.Add(FFdjustify);

  FFdlanguage := TSFString.Create('language', '');
  Fields.Add(FFdlanguage);

  FFdleftToRight := TSFBool.Create('leftToRight', TRUE);
  Fields.Add(FFdleftToRight);

  FFdsize := TSFFloat.Create('size', DefaultSize);
  Fields.Add(FFdsize);

  FFdspacing := TSFFloat.Create('spacing', DefaultSpacing);
  Fields.Add(FFdspacing);

  FFdstyle := TSFString.Create('style', 'PLAIN');
  Fields.Add(FFdstyle);

  FFdtopToBottom := TSFBool.Create('topToBottom', TRUE);
  Fields.Add(FFdtopToBottom);

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

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FFdpoint := TMFString.Create('point', []);
  Fields.Add(FFdpoint);

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

  FEventset_height := TVRMLEvent.Create('set_height', TMFFloat, true);
  Events.Add(FEventset_height);

  FEventset_yScale := TVRMLEvent.Create('set_yScale', TSFFloat, true);
  Events.Add(FEventset_yScale);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeColor]);
  Fields.Add(FFdcolor);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeNormal]);
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate]);
  Fields.Add(FFdtexCoord);

  FFdccw := TSFBool.Create('ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', TRUE);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FFdgeoGridOrigin := TSFString.Create('geoGridOrigin', '0 0 0');
  FFdgeoGridOrigin.Exposed := false;
  Fields.Add(FFdgeoGridOrigin);

  FFdheight := TMFFloat.Create('height', []);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);

  FFdnormalPerVertex := TSFBool.Create('normalPerVertex', TRUE);
  FFdnormalPerVertex.Exposed := false;
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create('solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdxDimension := TSFInt32.Create('xDimension', 0);
  FFdxDimension.Exposed := false;
  Fields.Add(FFdxDimension);

  FFdxSpacing := TSFString.Create('xSpacing', '1.0');
  FFdxSpacing.Exposed := false;
  Fields.Add(FFdxSpacing);

  FFdyScale := TSFFloat.Create('yScale', 1.0);
  FFdyScale.Exposed := false;
  Fields.Add(FFdyScale);

  FFdzDimension := TSFInt32.Create('zDimension', 0);
  FFdzDimension.Exposed := false;
  Fields.Add(FFdzDimension);

  FFdzSpacing := TSFString.Create('zSpacing', '1.0');
  FFdzSpacing.Exposed := false;
  Fields.Add(FFdzSpacing);
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

  FFdgeoCoords := TSFString.Create('geoCoords', '');
  Fields.Add(FFdgeoCoords);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

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

  FFdcenter := TSFString.Create('center', '');
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);

  FFdchild1Url := TMFString.Create('child1Url', []);
  FFdchild1Url.Exposed := false;
  Fields.Add(FFdchild1Url);

  FFdchild2Url := TMFString.Create('child2Url', []);
  FFdchild2Url.Exposed := false;
  Fields.Add(FFdchild2Url);

  FFdchild3Url := TMFString.Create('child3Url', []);
  FFdchild3Url.Exposed := false;
  Fields.Add(FFdchild3Url);

  FFdchild4Url := TMFString.Create('child4Url', []);
  FFdchild4Url.Exposed := false;
  Fields.Add(FFdchild4Url);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FFdrange := TSFFloat.Create('range', 10);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);

  FFdrootUrl := TMFString.Create('rootUrl', []);
  FFdrootUrl.Exposed := false;
  Fields.Add(FFdrootUrl);

  FFdrootNode := TMFNode.Create(Self, 'rootNode', AllowedChildrenNodes);
  FFdrootNode.Exposed := false;
  Fields.Add(FFdrootNode);

  FEventchildren := TVRMLEvent.Create('children', TMFNode, false);
  Events.Add(FEventchildren);
  Fields.Last.Exposed := false;

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

  FFddata := TMFNode.Create(Self, 'data', AllowedChildrenNodes);
  Fields.Add(FFddata);

  FFdsummary := TMFString.Create('summary', []);
  Fields.Add(FFdsummary);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

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

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  Fields.Add(FFdgeoSystem);

  FFdgeoCoords := TSFString.Create('geoCoords', '');
  Fields.Add(FFdgeoCoords);

  FFdrotateYUp := TSFBool.Create('rotateYUp', FALSE);
  Fields.Add(FFdrotateYUp);

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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FFdkey := TMFFloat.Create('key', []);
  FFdkey.Exposed := false;
  Fields.Add(FFdkey);

  FFdkeyValue := TMFString.Create('keyValue', []);
  FFdkeyValue.Exposed := false;
  Fields.Add(FFdkeyValue);

  FEventgeovalue_changed := TVRMLEvent.Create('geovalue_changed', TSFString, false);
  Events.Add(FEventgeovalue_changed);
  Fields.Last.Exposed := false;

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);
  Fields.Last.Exposed := false;

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

  FFdenabled := TSFBool.Create('enabled', TRUE);
  Fields.Add(FFdenabled);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FEventhitNormal_changed := TVRMLEvent.Create('hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventhitNormal_changed);

  FEventhitPoint_changed := TVRMLEvent.Create('hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventhitPoint_changed);

  FEventhitTexCoord_changed := TVRMLEvent.Create('hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventhitTexCoord_changed);

  FEventhitGeoCoord_changed := TVRMLEvent.Create('hitGeoCoord_changed', TSFString, false);
  Events.Add(FEventhitGeoCoord_changed);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisOver := TVRMLEvent.Create('isOver', TSFBool, false);
  Events.Add(FEventisOver);

  FEventtouchTime := TVRMLEvent.Create('touchTime', TSFTime, false);
  Events.Add(FEventtouchTime);

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

  FEventset_bind := TVRMLEvent.Create('set_bind', TSFBool, true);
  Events.Add(FEventset_bind);

  FEventset_orientation := TVRMLEvent.Create('set_orientation', TSFString, true);
  Events.Add(FEventset_orientation);

  FEventset_position := TVRMLEvent.Create('set_position', TSFString, true);
  Events.Add(FEventset_position);

  FFdfieldOfView := TSFFloat.Create('fieldOfView', Pi / 4);
  Fields.Add(FFdfieldOfView);

  FFdheadlight := TSFBool.Create('headlight', TRUE);
  Fields.Add(FFdheadlight);

  FFdjump := TSFBool.Create('jump', TRUE);
  Fields.Add(FFdjump);

  FFdnavType := TMFString.Create('navType', ['EXAMINE','ANY']);
  Fields.Add(FFdnavType);

  FFddescription := TSFString.Create('description', '');
  FFddescription.Exposed := false;
  Fields.Add(FFddescription);

  FFdgeoOrigin := TSFNode.Create(Self, 'geoOrigin', [TNodeGeoOrigin]);
  FFdgeoOrigin.Exposed := false;
  Fields.Add(FFdgeoOrigin);

  FFdgeoSystem := TMFString.Create('geoSystem', ['GD','WE']);
  FFdgeoSystem.Exposed := false;
  Fields.Add(FFdgeoSystem);

  FFdorientation := TSFRotation.Create('orientation', Vector3Single(0, 0, 1), 0);
  FFdorientation.Exposed := false;
  Fields.Add(FFdorientation);

  FFdposition := TSFString.Create('position', '0 0 100000');
  FFdposition.Exposed := false;
  Fields.Add(FFdposition);

  FFdspeedFactor := TSFFloat.Create('speedFactor', 1.0);
  FFdspeedFactor.Exposed := false;
  Fields.Add(FFdspeedFactor);

  FEventbindTime := TVRMLEvent.Create('bindTime', TSFTime, false);
  Events.Add(FEventbindTime);

  FEventisBound := TVRMLEvent.Create('isBound', TSFBool, false);
  Events.Add(FEventisBound);

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

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdrepeatS := TSFBool.Create('repeatS', TRUE);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create('repeatT', TRUE);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

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

  FEventset_colorIndex := TVRMLEvent.Create('set_colorIndex', TMFInt32, true);
  Events.Add(FEventset_colorIndex);

  FEventset_coordIndex := TVRMLEvent.Create('set_coordIndex', TMFInt32, true);
  Events.Add(FEventset_coordIndex);

  FEventset_normalIndex := TVRMLEvent.Create('set_normalIndex', TMFInt32, true);
  Events.Add(FEventset_normalIndex);

  FEventset_texCoordIndex := TVRMLEvent.Create('set_texCoordIndex', TMFInt32, true);
  Events.Add(FEventset_texCoordIndex);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeColor]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate]);
  Fields.Add(FFdcoord);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeNormal]);
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate]);
  Fields.Add(FFdtexCoord);

  FFdccw := TSFBool.Create('ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdcolorIndex := TMFInt32.Create('colorIndex', []);
  FFdcolorIndex.Exposed := false;
  Fields.Add(FFdcolorIndex);

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', TRUE);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdconvex := TSFBool.Create('convex', TRUE);
  FFdconvex.Exposed := false;
  Fields.Add(FFdconvex);

  FFdcoordIndex := TMFInt32.CreateMFLong('coordIndex', [], true);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);

  FFdnormalIndex := TMFInt32.Create('normalIndex', []);
  FFdnormalIndex.Exposed := false;
  Fields.Add(FFdnormalIndex);

  FFdnormalPerVertex := TSFBool.Create('normalPerVertex', TRUE);
  FFdnormalPerVertex.Exposed := false;
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create('solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdtexCoordIndex := TMFInt32.CreateMFLong('texCoordIndex', [], true);
  FFdtexCoordIndex.Exposed := false;
  Fields.Add(FFdtexCoordIndex);
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

  FEventset_colorIndex := TVRMLEvent.Create('set_colorIndex', TMFInt32, true);
  Events.Add(FEventset_colorIndex);

  FEventset_coordIndex := TVRMLEvent.Create('set_coordIndex', TMFInt32, true);
  Events.Add(FEventset_coordIndex);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeColor]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate]);
  Fields.Add(FFdcoord);

  FFdcolorIndex := TMFInt32.Create('colorIndex', []);
  FFdcolorIndex.Exposed := false;
  Fields.Add(FFdcolorIndex);

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', TRUE);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdcoordIndex := TMFInt32.CreateMFLong('coordIndex', [], true);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
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

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

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

  FFdload := TSFBool.Create('load', TRUE);
  Fields.Add(FFdload);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

  FEventchildren := TVRMLEvent.Create('children', TMFNode, false);
  Events.Add(FEventchildren);

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

  FFdlevel := TMFNode.Create(Self, 'level', AllowedChildrenNodes);
  Fields.Add(FFdlevel);

  FFdcenter := TSFVec3f.Create('center', ZeroVector3Single);
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);

  FFdrange := TMFFloat.Create('range', []);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);

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

  FFdambientIntensity := TSFFloat.Create('ambientIntensity', DefaultMaterial_2AmbientIntensity);
  Fields.Add(FFdambientIntensity);

  FFddiffuseColor := TSFColor.Create('diffuseColor', DefaultMaterialDiffuseColor);
  Fields.Add(FFddiffuseColor);

  FFdemissiveColor := TSFColor.Create('emissiveColor', DefaultMaterialEmissiveColor);
  Fields.Add(FFdemissiveColor);

  FFdshininess := TSFFloat.Create('shininess', DefaultMaterialShininess);
  Fields.Add(FFdshininess);

  FFdspecularColor := TSFColor.Create('specularColor', DefaultMaterialSpecularColor);
  Fields.Add(FFdspecularColor);

  FFdtransparency := TSFFloat.Create('transparency', DefaultMaterialTransparency);
  Fields.Add(FFdtransparency);

  FFdfogImmune := TSFBool.Create('fogImmune', false);
  Fields.Add(FFdfogImmune);

  FFdmirror := TSFFloat.Create('mirror', DefaultMaterialMirror);
  Fields.Add(FFdmirror);

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

  FFdloop := TSFBool.Create('loop', FALSE);
  Fields.Add(FFdloop);

  FFdspeed := TSFFloat.Create('speed', 1.0);
  Fields.Add(FFdspeed);

  FFdstartTime := TSFTime.Create('startTime', 0);
  Fields.Add(FFdstartTime);

  FFdstopTime := TSFTime.Create('stopTime', 0);
  Fields.Add(FFdstopTime);

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFdrepeatS := TSFBool.Create('repeatS', TRUE);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create('repeatT', TRUE);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

  FEventduration_changed := TVRMLEvent.Create('duration_changed', TSFTime, false);
  Events.Add(FEventduration_changed);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

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

  FFdavatarSize := TMFFloat.Create('avatarSize', [0.25, 1.6, 0.75]);
  Fields.Add(FFdavatarSize);

  FFdheadlight := TSFBool.Create('headlight', TRUE);
  Fields.Add(FFdheadlight);

  FFdspeed := TSFFloat.Create('speed', 1.0);
  Fields.Add(FFdspeed);

  FFdtransitionTime := TSFTime.Create('transitionTime', 1.0);
  Fields.Add(FFdtransitionTime);

  FFdtransitionType := TMFString.Create('transitionType',  ['LINEAR']);
  Fields.Add(FFdtransitionType);

  FFdtype := TMFString.Create('type', ['WALK', 'ANY']);
  Fields.Add(FFdtype);

  FFdvisibilityLimit := TSFFloat.Create('visibilityLimit', 0.0);
  Fields.Add(FFdvisibilityLimit);

  FEventtransitionComplete := TVRMLEvent.Create('transitionComplete', TSFBool, false);
  Events.Add(FEventtransitionComplete);

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

  FFdvector := TMFVec3f.Create('vector', []);
  Fields.Add(FFdvector);

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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

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

  FFdcontrolPoint := TMFVec3f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);

  FFdtessellation := TSFInt32.Create('tessellation', 0);
  Fields.Add(FFdtessellation);

  FFdknot := TMFFloat.Create('knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create('order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
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

  FFdcontrolPoint := TMFVec2f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdtessellation := TSFInt32.Create('tessellation', 0);
  Fields.Add(FFdtessellation);

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);

  FFdknot := TMFFloat.Create('knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create('order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
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

  FFdtessellationScale := TSFFloat.Create('tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFddimension := TSFInt32.Create('dimension', 0);
  Fields.Add(FFddimension);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FFdkeyWeight := TMFFloat.Create('keyWeight', []);
  Fields.Add(FFdkeyWeight);

  FFdknot := TMFFloat.Create('knot', []);
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create('order', 4);
  Fields.Add(FFdorder);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

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

  FFdcontrolPoint := TMFVec3f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate, TNodeNurbsTextureSurface]);
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create('uTessellation', 0);
  Fields.Add(FFduTessellation);

  FFdvTessellation := TSFInt32.Create('vTessellation', 0);
  Fields.Add(FFdvTessellation);

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);

  FFdccw := TSFBool.Create('ccw', TRUE);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create('solid', TRUE);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create('uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create('vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
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

  FFdcontrolPoint := TMFVec2f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create('uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create('vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFRotation.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

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

  FFdimage := TSFImage.Create('image', nil);
  Fields.Add(FFdimage);

  FFdrepeatS := TSFBool.Create('repeatS', TRUE);
  FFdrepeatS.Exposed := false;
  Fields.Add(FFdrepeatS);

  FFdrepeatT := TSFBool.Create('repeatT', TRUE);
  FFdrepeatT.Exposed := false;
  Fields.Add(FFdrepeatT);

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

  FFdmaxPosition := TSFVec2f.Create('maxPosition', Vector2Single(-1, -1));
  Fields.Add(FFdmaxPosition);

  FFdminPosition := TSFVec2f.Create('minPosition', Vector2Single(0, 0));
  Fields.Add(FFdminPosition);

  FFdoffset := TSFVec3f.Create('offset', ZeroVector3Single);
  Fields.Add(FFdoffset);

  FFdaxisRotation := TSFRotation.Create('axisRotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdaxisRotation);

  FEventtranslation_changed := TVRMLEvent.Create('translation_changed', TSFVec3f, false);
  Events.Add(FEventtranslation_changed);

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

  FFdradius := TSFFloat.Create('radius', 100);
  Fields.Add(FFdradius);

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

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeColor]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [TNodeCoordinate, TNodeGeoCoordinate]);
  Fields.Add(FFdcoord);
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

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);
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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

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

  FFdcenter := TSFVec3f.Create('center', ZeroVector3Single);
  Fields.Add(FFdcenter);

  FFdsize := TSFVec3f.Create('size', ZeroVector3Single);
  Fields.Add(FFdsize);

  FFdenabled := TSFBool.Create('enabled', TRUE);
  Fields.Add(FFdenabled);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventposition_changed := TVRMLEvent.Create('position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  FEventorientation_changed := TVRMLEvent.Create('orientation_changed', TSFRotation, false);
  Events.Add(FEventorientation_changed);

  FEvententerTime := TVRMLEvent.Create('enterTime', TSFTime, false);
  Events.Add(FEvententerTime);

  FEventexitTime := TVRMLEvent.Create('exitTime', TSFTime, false);
  Events.Add(FEventexitTime);

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

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);

  FFdkeyValue := TMFFloat.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

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

  FFdurl := TMFString.Create('url', []);
  Fields.Add(FFdurl);

  FFddirectOutput := TSFBool.Create('directOutput', FALSE);
  FFddirectOutput.Exposed := false;
  Fields.Add(FFddirectOutput);

  FFdmustEvaluate := TSFBool.Create('mustEvaluate', FALSE);
  FFdmustEvaluate.Exposed := false;
  Fields.Add(FFdmustEvaluate);

  DefaultContainerField := 'children';

  { VRML 97 says this is RestrictedAccessTypes.
    But X3D, (both the general spec, and X3D classic encoding grammar,
    so it's not by accident) allows here all access types,
    including inputOutput. }
  HasInterfaceDeclarations := AllAccessTypes;
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

  FFdappearance := TSFNode.Create(Self, 'appearance', [TNodeAppearance, TNodeKambiAppearance]);
  Fields.Add(FFdappearance);

  FFdgeometry := TSFNode.Create(Self, 'geometry', AllowedGeometryNodes);
  Fields.Add(FFdgeometry);

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

  FFddirection := TSFVec3f.Create('direction', Vector3Single(0, 0, 1));
  Fields.Add(FFddirection);

  FFdintensity := TSFFloat.Create('intensity', 1);
  Fields.Add(FFdintensity);

  FFdlocation := TSFVec3f.Create('location', ZeroVector3Single);
  Fields.Add(FFdlocation);

  FFdmaxBack := TSFFloat.Create('maxBack', 10);
  Fields.Add(FFdmaxBack);

  FFdmaxFront := TSFFloat.Create('maxFront', 10);
  Fields.Add(FFdmaxFront);

  FFdminBack := TSFFloat.Create('minBack', 1);
  Fields.Add(FFdminBack);

  FFdminFront := TSFFloat.Create('minFront', 1);
  Fields.Add(FFdminFront);

  FFdpriority := TSFFloat.Create('priority', 0);
  Fields.Add(FFdpriority);

  FFdsource := TSFNode.Create(Self, 'source', [TNodeAudioClip, TNodeMovieTexture]);
  Fields.Add(FFdsource);

  FFdspatialize := TSFBool.Create('spatialize', TRUE);
  FFdspatialize.Exposed := false;
  Fields.Add(FFdspatialize);

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

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
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

  FFdoffset := TSFRotation.Create('offset', Vector3Single(0, 1, 0), 0);
  Fields.Add(FFdoffset);

  FEventrotation_changed := TVRMLEvent.Create('rotation_changed', TSFRotation, false);
  Events.Add(FEventrotation_changed);

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

  FFdbeamWidth := TSFFloat.Create('beamWidth', Pi / 2);
  Fields.Add(FFdbeamWidth);

  FFdcutOffAngle := TSFFloat.Create('cutOffAngle', Pi / 4);
  Fields.Add(FFdcutOffAngle);

  FFddirection := TSFVec3f.Create('direction', Vector3Single(0, 0, -1));
  Fields.Add(FFddirection);

  FFdradius := TSFFloat.Create('radius', 100);
  Fields.Add(FFdradius);

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

  FFdchoice := TMFNode.Create(Self, 'choice', AllowedChildrenNodes);
  Fields.Add(FFdchoice);

  FFdwhichChoice := TSFInt32.Create('whichChoice', -1);
  Fields.Add(FFdwhichChoice);

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

  FFdstring := TMFString.Create('string', []);
  Fields.Add(FFdstring);

  FFdfontStyle := TSFNode.Create(Self, 'fontStyle', [TNodeFontStyle_2]);
  Fields.Add(FFdfontStyle);

  FFdlength := TMFFloat.Create('length', []);
  Fields.Add(FFdlength);

  FFdmaxExtent := TSFFloat.Create('maxExtent', 0.0);
  Fields.Add(FFdmaxExtent);
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

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);

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

  FFdcenter := TSFVec2f.Create('center', Vector2Single(0, 0));
  Fields.Add(FFdcenter);

  FFdrotation := TSFFloat.Create('rotation', 0);
  Fields.Add(FFdrotation);

  FFdscale := TSFVec2f.Create('scale', Vector2Single(1, 1));
  Fields.Add(FFdscale);

  FFdtranslation := TSFVec2f.Create('translation', Vector2Single(0, 0));
  Fields.Add(FFdtranslation);

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

  FFdcycleInterval := TSFTime.Create('cycleInterval', 1);
  Fields.Add(FFdcycleInterval);

  FFdenabled := TSFBool.Create('enabled', TRUE);
  Fields.Add(FFdenabled);

  FFdloop := TSFBool.Create('loop', FALSE);
  Fields.Add(FFdloop);

  FFdstartTime := TSFTime.Create('startTime', 0);
  Fields.Add(FFdstartTime);

  FFdstopTime := TSFTime.Create('stopTime', 0);
  Fields.Add(FFdstopTime);

  FEventcycleTime := TVRMLEvent.Create('cycleTime', TSFTime, false);
  Events.Add(FEventcycleTime);

  FEventfraction_changed := TVRMLEvent.Create('fraction_changed', TSFFloat, false);
  Events.Add(FEventfraction_changed);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventtime := TVRMLEvent.Create('time', TSFTime, false);
  Events.Add(FEventtime);

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

  FEventhitNormal_changed := TVRMLEvent.Create('hitNormal_changed', TSFVec3f, false);
  Events.Add(FEventhitNormal_changed);

  FEventhitPoint_changed := TVRMLEvent.Create('hitPoint_changed', TSFVec3f, false);
  Events.Add(FEventhitPoint_changed);

  FEventhitTexCoord_changed := TVRMLEvent.Create('hitTexCoord_changed', TSFVec2f, false);
  Events.Add(FEventhitTexCoord_changed);

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

  FFdcenter := TSFVec3f.Create('center', ZeroVector3Single);
  Fields.Add(FFdcenter);

  FFdrotation := TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);

  FFdscale := TSFVec3f.Create('scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);

  FFdscaleOrientation := TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);

  FFdtranslation := TSFVec3f.Create('translation', ZeroVector3Single);
  Fields.Add(FFdtranslation);

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

  FEventaddTrimmingContour := TVRMLEvent.Create('addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create('removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
  Fields.Add(FFdtrimmingContour);

  FFdsurface := TSFNode.Create(Self, 'surface', [TNodeNurbsSurface]);
  Fields.Add(FFdsurface);
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

  FFdfieldOfView := TSFFloat.Create('fieldOfView', DefaultViewpointFieldOfView);
  Fields.Add(FFdfieldOfView);

  FFdjump := TSFBool.Create('jump', TRUE);
  Fields.Add(FFdjump);

  FFddescription := TSFString.Create('description', '');
  Fields.Add(FFddescription);

  FFdcenterOfRotation := TSFVec3f.Create('centerOfRotation', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfRotation);

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

  FFdcenter := TSFVec3f.Create('center', ZeroVector3Single);
  Fields.Add(FFdcenter);

  FFdenabled := TSFBool.Create('enabled', TRUE);
  Fields.Add(FFdenabled);

  FFdsize := TSFVec3f.Create('size', ZeroVector3Single);
  Fields.Add(FFdsize);

  FEvententerTime := TVRMLEvent.Create('enterTime', TSFTime, false);
  Events.Add(FEvententerTime);

  FEventexitTime := TVRMLEvent.Create('exitTime', TSFTime, false);
  Events.Add(FEventexitTime);

  FEventisActive := TVRMLEvent.Create('isActive', TSFBool, false);
  Events.Add(FEventisActive);

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

  FFdinfo := TMFString.Create('info', []);
  FFdinfo.Exposed := false;
  Fields.Add(FFdinfo);

  FFdtitle := TSFString.Create('title', '');
  FFdtitle.Exposed := false;
  Fields.Add(FFdtitle);

  DefaultContainerField := 'children';
end;
{$endif read_implementation}
