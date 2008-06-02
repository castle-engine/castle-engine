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
  that are only for easier implementation, like TVRMLGroupingNode). } { }

{$ifdef read_interface}
  TNodeX3DShaderNode = class;

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

  TNodeCoordinateDeformer = class(TVRMLGroupingNode)
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

  TNodeDirectionalLight_2 = class(TVRMLDirectionalLightNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
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

  TNodeGeoLOD = class(TVRMLGroupingNode)
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

  TNodeImageTexture = class(TVRMLTextureNode)
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

  TNodeInlineLoadControl = class(TVRMLGroupingNode, IVRMLInlineNode)
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

  TNodePixelTexture = class(TVRMLTextureNode)
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

  TNodePointLight_2 = class(TVRMLPointLightNode)
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

  TNodeSpotLight_2 = class(TVRMLPositionalLightNode)
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

  TNodeViewpoint = class(TVRMLViewpointNode)
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
{$endif read_interface}

{$ifdef read_implementation}

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

class function TNodeAudioClip.ClassNodeTypeName: string;
begin
  Result := 'AudioClip';
end;

class function TNodeAudioClip.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeAudioClip.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

function TNodeBackground.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
end;

constructor TNodeBillboard.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdaxisOfRotation := TSFVec3f.Create('axisOfRotation', Vector3Single(0, 1, 0));
  Fields.Add(FFdaxisOfRotation);

  DefaultContainerField := 'children';
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

procedure TNodeBillboard.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

function TNodeBillboard.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeCollision.ClassNodeTypeName: string;
begin
  Result := 'Collision';
end;

class function TNodeCollision.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeCollision.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
var
  I: Integer;
begin
  for I := 0 to FdChildren.Count - 1 do
    Func(Self, FdChildren.Items[I]);
end;

function TNodeCollision.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeColorInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ColorInterpolator';
end;

class function TNodeColorInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeColorInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeContour2D.ClassNodeTypeName: string;
begin
  Result := 'Contour2D';
end;

class function TNodeContour2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeContour2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeCoordinateDeformer.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDeformer';
end;

class function TNodeCoordinateDeformer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeCoordinateDeformer.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

function TNodeCoordinateDeformer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeCoordinateInterpolator.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator';
end;

class function TNodeCoordinateInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeCoordinateInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeCylinderSensor.ClassNodeTypeName: string;
begin
  Result := 'CylinderSensor';
end;

class function TNodeCylinderSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeCylinderSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

function TNodeDirectionalLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeFog.ClassNodeTypeName: string;
begin
  Result := 'Fog';
end;

class function TNodeFog.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeFog.Alternative: TNodeFog;
begin
  if (FdAlternative.Value <> nil) and (FdAlternative.Value is TNodeFog) then
    Result := TNodeFog(FdAlternative.Value) else
    Result := nil;
end;

function TNodeFog.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
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

class function TNodeGeoCoordinate.ClassNodeTypeName: string;
begin
  Result := 'GeoCoordinate';
end;

class function TNodeGeoCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'GeoElevationGrid';
end;

class function TNodeGeoElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoElevationGrid.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoLocation.ClassNodeTypeName: string;
begin
  Result := 'GeoLocation';
end;

class function TNodeGeoLocation.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoLocation.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoLOD.ClassNodeTypeName: string;
begin
  Result := 'GeoLOD';
end;

class function TNodeGeoLOD.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoLOD.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoMetadata.ClassNodeTypeName: string;
begin
  Result := 'GeoMetadata';
end;

class function TNodeGeoMetadata.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoMetadata.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoOrigin.ClassNodeTypeName: string;
begin
  Result := 'GeoOrigin';
end;

class function TNodeGeoOrigin.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoOrigin.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'GeoPositionInterpolator';
end;

class function TNodeGeoPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoPositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'GeoTouchSensor';
end;

class function TNodeGeoTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoTouchSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeGeoViewpoint.ClassNodeTypeName: string;
begin
  Result := 'GeoViewpoint';
end;

class function TNodeGeoViewpoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeGeoViewpoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeImageTexture.ClassNodeTypeName: string;
begin
  Result := 'ImageTexture';
end;

class function TNodeImageTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
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

function TNodeImageTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeInlineLoadControl.ClassNodeTypeName: string;
begin
  Result := 'InlineLoadControl';
end;

class function TNodeInlineLoadControl.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
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

function TNodeInlineLoadControl.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeMovieTexture.ClassNodeTypeName: string;
begin
  Result := 'MovieTexture';
end;

class function TNodeMovieTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeMovieTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNavigationInfo.ClassNodeTypeName: string;
begin
  Result := 'NavigationInfo';
end;

class function TNodeNavigationInfo.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNavigationInfo.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);
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

class function TNodeNormalInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NormalInterpolator';
end;

class function TNodeNormalInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNormalInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNurbsCurve.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

class function TNodeNurbsCurve.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsCurve.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNurbsCurve2D.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve2D';
end;

class function TNodeNurbsCurve2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsCurve2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeNurbsGroup.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdtessellationScale := TSFFloat.Create('tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
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

function TNodeNurbsGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNurbsPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNodeNurbsPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsPositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNurbsSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurface';
end;

class function TNodeNurbsSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeNurbsTextureSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureSurface';
end;

class function TNodeNurbsTextureSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsTextureSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'OrientationInterpolator';
end;

class function TNodeOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodePixelTexture.ClassNodeTypeName: string;
begin
  Result := 'PixelTexture';
end;

class function TNodePixelTexture.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
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

function TNodePixelTexture.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodePlaneSensor.ClassNodeTypeName: string;
begin
  Result := 'PlaneSensor';
end;

class function TNodePlaneSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodePlaneSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodePointLight_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  { Previous fields initialized in TVRMLPointLightNode and above.
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

function TNodePointLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator';
end;

class function TNodePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodePositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeProximitySensor.ClassNodeTypeName: string;
begin
  Result := 'ProximitySensor';
end;

class function TNodeProximitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeProximitySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ScalarInterpolator';
end;

class function TNodeScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeScalarInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeScript.ClassNodeTypeName: string;
begin
  Result := 'Script';
end;

class function TNodeScript.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeScript.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeSound.ClassNodeTypeName: string;
begin
  Result := 'Sound';
end;

class function TNodeSound.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeSound.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeSphereSensor.ClassNodeTypeName: string;
begin
  Result := 'SphereSensor';
end;

class function TNodeSphereSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeSphereSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeSpotLight_2.ClassNodeTypeName: string;
begin
  Result := 'SpotLight';
end;

class function TNodeSpotLight_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
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

function TNodeSpotLight_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

constructor TNodeTextureCoordinate.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);

  DefaultContainerField := 'texCoord';
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

function TNodeTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeTextureTransform.ClassNodeTypeName: string;
begin
  Result := 'TextureTransform';
end;

class function TNodeTextureTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
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

function TNodeTextureTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeTouchSensor.ClassNodeTypeName: string;
begin
  Result := 'TouchSensor';
end;

class function TNodeTouchSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeTouchSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'TrimmedSurface';
end;

class function TNodeTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeTrimmedSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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
    cameras (as set by TVRMLViewpointNode). }
  FdPosition.DefaultValue := Vector3Single(0, 0, 10);
  FdPosition.Value := FdPosition.DefaultValue;

  DefaultContainerField := 'children';
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

function TNodeViewpoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

class function TNodeVisibilitySensor.ClassNodeTypeName: string;
begin
  Result := 'VisibilitySensor';
end;

class function TNodeVisibilitySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeVisibilitySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;
{$endif read_implementation}
