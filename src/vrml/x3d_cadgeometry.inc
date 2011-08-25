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
  IAbstractX3DProductStructureChildNode = interface(IAbstractX3DChildNode)
  ['{318249E0-BACF-4F16-9E49-3E912F24E0AA}']
    property FdName: TSFString { read FFdName} { };
  end;

  TAbstractProductStructureChildNode = class(TAbstractChildNode,
    IAbstractX3DProductStructureChildNode)
  public
    procedure CreateNode; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;
  end;

  TCADAssemblyNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;
  end;

  TCADFaceNode = class(TAbstractProductStructureChildNode, IAbstractX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdShape: TSFNode;
    public property FdShape: TSFNode read FFdShape;

    private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;
  end;

  TCADLayerNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdVisible: TMFBool;
    public property FdVisible: TMFBool read FFdVisible;
  end;

  TCADPartNode = class(TAbstractX3DGroupingNode, IAbstractX3DProductStructureChildNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdCenter: TSFVec3f;
    public property FdCenter: TSFVec3f read FFdCenter;

    private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    private FFdRotation: TSFRotation;
    public property FdRotation: TSFRotation read FFdRotation;

    private FFdScale: TSFVec3f;
    public property FdScale: TSFVec3f read FFdScale;

    private FFdScaleOrientation: TSFRotation;
    public property FdScaleOrientation: TSFRotation read FFdScaleOrientation;

    private FFdTranslation: TSFVec3f;
    public property FdTranslation: TSFVec3f read FFdTranslation;
  end;

  TIndexedQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventSet_index: TVRMLEvent;
    public property EventSet_index: TVRMLEvent read FEventSet_index;

    private FFdIndex: TMFInt32;
    public property FdIndex: TMFInt32 read FFdIndex;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TQuadSetNode = class(TAbstractComposedGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractProductStructureChildNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  DefaultContainerField := 'children';
end;

procedure TCADAssemblyNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  DefaultContainerField := 'children';
end;

class function TCADAssemblyNode.ClassNodeTypeName: string;
begin
  Result := 'CADAssembly';
end;

class function TCADAssemblyNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCADFaceNode.CreateNode;
begin
  inherited;

  FFdShape := TSFNode.Create(Self, 'shape', [TAbstractX3DShapeNode, TAbstractLODNode]);
  Fields.Add(FFdShape);

  FFdBboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdBboxCenter.Exposed := false;
  Fields.Add(FFdBboxCenter);
  { X3D specification comment: (-Inf, Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdBboxSize.Exposed := false;
  Fields.Add(FFdBboxSize);
  { X3D specification comment: [0, Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TCADFaceNode.ClassNodeTypeName: string;
begin
  Result := 'CADFace';
end;

class function TCADFaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCADLayerNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdVisible := TMFBool.Create(Self, 'visible', []);
  Fields.Add(FFdVisible);

  DefaultContainerField := 'children';
end;

class function TCADLayerNode.ClassNodeTypeName: string;
begin
  Result := 'CADLayer';
end;

class function TCADLayerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCADPartNode.CreateNode;
begin
  inherited;

  FFdCenter := TSFVec3f.Create(Self, 'center', Vector3Single(0, 0, 0));
  Fields.Add(FFdCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdName := TSFString.Create(Self, 'name', '');
  Fields.Add(FFdName);

  FFdRotation := TSFRotation.Create(Self, 'rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdRotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdScale := TSFVec3f.Create(Self, 'scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdScale);
  { X3D specification comment: (0,Inf) }

  FFdScaleOrientation := TSFRotation.Create(Self, 'scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdScaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdTranslation := TSFVec3f.Create(Self, 'translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdTranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TCADPartNode.ClassNodeTypeName: string;
begin
  Result := 'CADPart';
end;

class function TCADPartNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TIndexedQuadSetNode.CreateNode;
begin
  inherited;

  FEventSet_index := TVRMLEvent.Create(Self, 'set_index', TMFInt32, true);
  Events.Add(FEventSet_index);

  FFdIndex := TMFInt32.Create(Self, 'index', []);
   FdIndex.Exposed := false;
   FdIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdIndex);
  { X3D specification comment: [0,Inf) }
end;

class function TIndexedQuadSetNode.ClassNodeTypeName: string;
begin
  Result := 'IndexedQuadSet';
end;

class function TIndexedQuadSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TIndexedQuadSetNode.CoordIndex: TMFLong;
begin
  Result := FdIndex;
end;

procedure TQuadSetNode.CreateNode;
begin
  inherited;
end;

class function TQuadSetNode.ClassNodeTypeName: string;
begin
  Result := 'QuadSet';
end;

class function TQuadSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterCADGeometryNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCADAssemblyNode,
    TCADFaceNode,
    TCADLayerNode,
    TCADPartNode,
    TIndexedQuadSetNode,
    TQuadSetNode
  ]);
end;

{$endif read_implementation}
