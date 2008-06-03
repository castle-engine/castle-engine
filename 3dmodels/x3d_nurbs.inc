{
  Copyright 2008 Michalis Kamburelis.

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

{ NurbsCurve2D and Contour2D are shader with VRML 97 NURBS definition.
  The rest is, for now, completely separate from VRML 97 NURBS definition
  in vrml97nodes.inc. }

{$ifdef read_interface}
  TNodeX3DNurbsControlCurveNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdcontrolPoint: TMFVec2d;
    public property FdcontrolPoint: TMFVec2d read FFdcontrolPoint;
  end;

  TNodeX3DParametricGeometryNode = class(TNodeX3DGeometryNode)
  end;

  TNodeX3DNurbsSurfaceGeometryNode = class(TNodeX3DParametricGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFduClosed: TSFBool;
    public property FduClosed: TSFBool read FFduClosed;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvClosed: TSFBool;
    public property FdvClosed: TSFBool read FFdvClosed;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  { Contour2D.

    For VRML 2.0, this is TNodeX3DGeometryNode, since VRML 2.0 specification
    explicitly says it's valid geometry node. For X3D specification, this was only
    TNodeX3DNode. When implementing NURBS, I will have to either handle
    both VRML 2.0 and X3D cases at once, or just split this class into
    two separate TNodeContour2D_2 and TNodeContour2D_3. }
  TNodeContour2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;
  end;

  TNodeContourPolyline2D = class(TNodeX3DNurbsControlCurveNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateDouble = class(TNodeX3DCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3d;
    public property Fdpoint: TMFVec3d read FFdpoint;
  end;

  TNodeNurbsCurve_3 = class(TNodeX3DParametricGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;
  end;

  TNodeNurbsCurve2D = class(TNodeX3DNurbsControlCurveNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;
  end;

  TNodeNurbsOrientationInterpolator = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeNurbsPatchSurface = class(TNodeX3DNurbsSurfaceGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsPositionInterpolator_3 = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;
  end;

  TNodeNurbsSet = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddGeometry: TVRMLEvent;
    public property EventaddGeometry: TVRMLEvent read FEventaddGeometry;

    { Event: MFNode, in } { }
    private FEventremoveGeometry: TVRMLEvent;
    public property EventremoveGeometry: TVRMLEvent read FEventremoveGeometry;

    private FFdgeometry: TMFNode;
    public property Fdgeometry: TMFNode read FFdgeometry;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeNurbsSurfaceInterpolator = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;

    { Event: SFVec3f, out } { }
    private FEventnormal_changed: TVRMLEvent;
    public property Eventnormal_changed: TVRMLEvent read FEventnormal_changed;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  TNodeNurbsSweptSurface = class(TNodeX3DParametricGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcrossSectionCurve: TSFNode;
    public property FdcrossSectionCurve: TSFNode read FFdcrossSectionCurve;

    private FFdtrajectoryCurve: TSFNode;
    public property FdtrajectoryCurve: TSFNode read FFdtrajectoryCurve;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

  TNodeNurbsSwungSurface = class(TNodeX3DParametricGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdprofileCurve: TSFNode;
    public property FdprofileCurve: TSFNode read FFdprofileCurve;

    private FFdtrajectoryCurve: TSFNode;
    public property FdtrajectoryCurve: TSFNode read FFdtrajectoryCurve;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

  TNodeNurbsTextureCoordinate = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  TNodeNurbsTrimmedSurface = class(TNodeX3DNurbsSurfaceGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddTrimmingContour: TVRMLEvent;
    public property EventaddTrimmingContour: TVRMLEvent read FEventaddTrimmingContour;

    { Event: MFNode, in } { }
    private FEventremoveTrimmingContour: TVRMLEvent;
    public property EventremoveTrimmingContour: TVRMLEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DNurbsControlCurveNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec2d.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

constructor TNodeX3DNurbsSurfaceGeometryNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode, TNodeNurbsTextureCoordinate]);
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create('uTessellation', 0);
  Fields.Add(FFduTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdvTessellation := TSFInt32.Create('vTessellation', 0);
  Fields.Add(FFdvTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFduClosed := TSFBool.Create('uClosed', false);
  FFduClosed.Exposed := false;
  Fields.Add(FFduClosed);

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create('uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvClosed := TSFBool.Create('vClosed', false);
  FFdvClosed.Exposed := false;
  Fields.Add(FFdvClosed);

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create('vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }
end;

constructor TNodeContour2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeNurbsCurve2D, TNodeContourPolyline2D,
    { In VRML 97 these are allowed children too: }
    TNodePolyline2D, TNodeContour2D ]);
  Fields.Add(FFdchildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TNodeContour2D.ClassNodeTypeName: string;
begin
  Result := 'Contour2D';
end;

function TNodeContour2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeContour2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeContourPolyline2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeContourPolyline2D.ClassNodeTypeName: string;
begin
  Result := 'ContourPolyline2D';
end;

function TNodeContourPolyline2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeContourPolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateDouble.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec3d.Create('point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateDouble.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDouble';
end;

function TNodeCoordinateDouble.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCoordinateDouble.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsCurve_3.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdtessellation := TSFInt32.Create('tessellation', 0);
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create('closed', false);
  FFdclosed.Exposed := false;
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create('knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create('order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }
end;

class function TNodeNurbsCurve_3.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

function TNodeNurbsCurve_3.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsCurve_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsCurve_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

constructor TNodeNurbsCurve2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtessellation := TSFInt32.Create('tessellation', 0);
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create('closed', false);
  FFdclosed.Exposed := false;
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create('knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create('order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeNurbsCurve2D.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve2D';
end;

function TNodeNurbsCurve2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNurbsCurve2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdknot := TMFDouble.Create('knot', []);
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create('order', 3);
  Fields.Add(FFdorder);
  { X3D specification comment: (2,Inf) }

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeNurbsOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsOrientationInterpolator';
end;

function TNodeNurbsOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsPatchSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

class function TNodeNurbsPatchSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsPatchSurface';
end;

function TNodeNurbsPatchSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsPatchSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsPositionInterpolator_3.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdknot := TMFDouble.Create('knot', []);
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create('order', 3);
  Fields.Add(FFdorder);
  { X3D specification comment: (2,Inf) }

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeNurbsPositionInterpolator_3.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

function TNodeNurbsPositionInterpolator_3.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsPositionInterpolator_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsPositionInterpolator_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

constructor TNodeNurbsSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddGeometry := TVRMLEvent.Create('addGeometry', TMFNode, true);
  Events.Add(FEventaddGeometry);

  FEventremoveGeometry := TVRMLEvent.Create('removeGeometry', TMFNode, true);
  Events.Add(FEventremoveGeometry);

  FFdgeometry := TMFNode.Create(Self, 'geometry', [TNodeNurbsSurface]);
  Fields.Add(FFdgeometry);

  FFdtessellationScale := TSFFloat.Create('tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
  { X3D specification comment: (0,Inf) }

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeNurbsSet.ClassNodeTypeName: string;
begin
  Result := 'NurbsSet';
end;

function TNodeNurbsSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsSurfaceInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFVec2f, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFDouble.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventposition_changed := TVRMLEvent.Create('position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  FEventnormal_changed := TVRMLEvent.Create('normal_changed', TSFVec3f, false);
  Events.Add(FEventnormal_changed);

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create('uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create('vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeNurbsSurfaceInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurfaceInterpolator';
end;

function TNodeNurbsSurfaceInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsSurfaceInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsSweptSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcrossSectionCurve := TSFNode.Create(Self, 'crossSectionCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdcrossSectionCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeNurbsCurve_3]);
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create('ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSweptSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSweptSurface';
end;

function TNodeNurbsSweptSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsSweptSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsSwungSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdprofileCurve := TSFNode.Create(Self, 'profileCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdprofileCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create('ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSwungSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSwungSurface';
end;

function TNodeNurbsSwungSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsSwungSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec2f.Create('controlPoint', []);
  Fields.Add(FFdcontrolPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFFloat.Create('weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFduDimension := TSFInt32.Create('uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create('uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create('uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvDimension := TSFInt32.Create('vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create('vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create('vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeNurbsTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureCoordinate';
end;

function TNodeNurbsTextureCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsTrimmedSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddTrimmingContour := TVRMLEvent.Create('addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create('removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
  Fields.Add(FFdtrimmingContour);
end;

class function TNodeNurbsTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTrimmedSurface';
end;

function TNodeNurbsTrimmedSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterNURBSNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeContour2D,
    TNodeContourPolyline2D,
    TNodeCoordinateDouble,
    TNodeNurbsCurve_3,
    TNodeNurbsCurve2D,
    TNodeNurbsOrientationInterpolator,
    TNodeNurbsPatchSurface,
    TNodeNurbsPositionInterpolator_3,
    TNodeNurbsSet,
    TNodeNurbsSurfaceInterpolator,
    TNodeNurbsSweptSurface,
    TNodeNurbsSwungSurface,
    TNodeNurbsTextureCoordinate,
    TNodeNurbsTrimmedSurface
  ]);
end;

{$endif read_implementation}
