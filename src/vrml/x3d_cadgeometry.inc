{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  INodeX3DProductStructureChildNode = interface(INodeX3DChildNode)
  ['{318249E0-BACF-4F16-9E49-3E912F24E0AA}']
    property Fdname: TSFString { read FFdname} { };
  end;

  TNodeX3DProductStructureChildNode = class(TNodeX3DChildNode,
    INodeX3DProductStructureChildNode)
  public
    procedure CreateNode; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  TNodeCADAssembly = class(TNodeX3DGroupingNode, INodeX3DProductStructureChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  TNodeCADFace = class(TNodeX3DProductStructureChildNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdshape: TSFNode;
    public property Fdshape: TSFNode read FFdshape;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeCADLayer = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdvisible: TMFBool;
    public property Fdvisible: TMFBool read FFdvisible;
  end;

  TNodeCADPart = class(TNodeX3DGroupingNode, INodeX3DProductStructureChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;
  end;

  TNodeIndexedQuadSet = class(TNodeX3DComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventset_index: TVRMLEvent;
    public property Eventset_index: TVRMLEvent read FEventset_index;

    private FFdindex: TMFInt32;
    public property Fdindex: TMFInt32 read FFdindex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeQuadSet = class(TNodeX3DComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DProductStructureChildNode.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  DefaultContainerField := 'children';
end;

procedure TNodeCADAssembly.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  DefaultContainerField := 'children';
end;

class function TNodeCADAssembly.ClassNodeTypeName: string;
begin
  Result := 'CADAssembly';
end;

function TNodeCADAssembly.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeCADAssembly.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCADFace.CreateNode;
begin
  inherited;

  FFdshape := TSFNode.Create(Self, 'shape', [TNodeX3DShapeNode, TVRMLLODNode]);
  Fields.Add(FFdshape);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf, Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0, Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeCADFace.ClassNodeTypeName: string;
begin
  Result := 'CADFace';
end;

function TNodeCADFace.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeCADFace.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCADLayer.CreateNode;
begin
  inherited;

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdvisible := TMFBool.Create(Self, 'visible', []);
  Fields.Add(FFdvisible);

  DefaultContainerField := 'children';
end;

class function TNodeCADLayer.ClassNodeTypeName: string;
begin
  Result := 'CADLayer';
end;

function TNodeCADLayer.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeCADLayer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCADPart.CreateNode;
begin
  inherited;

  FFdcenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdname := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeCADPart.ClassNodeTypeName: string;
begin
  Result := 'CADPart';
end;

function TNodeCADPart.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeCADPart.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeIndexedQuadSet.CreateNode;
begin
  inherited;

  FEventset_index := TVRMLEvent.Create(Self, 'set_index', TMFInt32, true);
  Events.Add(FEventset_index);

  FFdindex := TMFInt32.Create(Self, 'index', []);
   Fdindex.Exposed := false;
   Fdindex.ChangesAlways := [chGeometry];
  Fields.Add(FFdindex);
  { X3D specification comment: [0,Inf) }
end;

class function TNodeIndexedQuadSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedQuadSet';
end;

function TNodeIndexedQuadSet.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeIndexedQuadSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeIndexedQuadSet.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TNodeQuadSet.CreateNode;
begin
  inherited;
end;

class function TNodeQuadSet.ClassNodeTypeName: string;
begin
  Result := 'QuadSet';
end;

function TNodeQuadSet.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeQuadSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterCADGeometryNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCADAssembly,
    TNodeCADFace,
    TNodeCADLayer,
    TNodeCADPart,
    TNodeIndexedQuadSet,
    TNodeQuadSet
  ]);
end;

{$endif read_implementation}
