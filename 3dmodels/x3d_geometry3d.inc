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

{$ifdef read_interface}
  TNodeBox = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCone_2 = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbottom: TSFBool;
    public property Fdbottom: TSFBool read FFdbottom;

    private FFdbottomRadius: TSFFloat;
    public property FdbottomRadius: TSFFloat read FFdbottomRadius;

    private FFdheight: TSFFloat;
    public property Fdheight: TSFFloat read FFdheight;

    private FFdside: TSFBool;
    public property Fdside: TSFBool read FFdside;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeCylinder_2 = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdbottom: TSFBool;
    public property Fdbottom: TSFBool read FFdbottom;

    private FFdheight: TSFFloat;
    public property Fdheight: TSFFloat read FFdheight;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdside: TSFBool;
    public property Fdside: TSFBool read FFdside;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdtop: TSFBool;
    public property Fdtop: TSFBool read FFdtop;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeElevationGrid = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFFloat, in } { }
    private FEventset_height: TVRMLEvent;
    public property Eventset_height: TVRMLEvent read FEventset_height;

    private FFdattrib: TMFNode;
    public property Fdattrib: TMFNode read FFdattrib;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdfogCoord: TSFNode;
    public property FdfogCoord: TSFNode read FFdfogCoord;

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

    private FFdheight: TMFFloat;
    public property Fdheight: TMFFloat read FFdheight;

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

    { This checks whether xDimension and zDimension are >= 2,
      xSpacing and zSpacing are > 0 and height has at least the
      required number of values. If this returns @false then
      it is understood that ElevationGrid is not rendered, doesn't
      have any vertices/triangles etc. }
    function IsNotEmpty: boolean;

    { Returns color.point field, assuming that color is set and
      specifies Color node. Otherwise returns @nil. }
    function Color: TMFVec3f;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeExtrusion = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

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

    { For given Index, return appropriate FdCrossIndex item as 3D vertex.
      That is, uses FdCrossIndex values as X, Z of 3D vertex and sets Y = 0
      (that's how Extrusion is defined in VRML / X3D). }
    function CrossSection3D(Index: integer): TVector3Single;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeIndexedFaceSet_2 = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

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

    private FFdcolorIndex: TMFInt32;
    public property FdcolorIndex: TMFInt32 read FFdcolorIndex;

    private FFdconvex: TSFBool;
    public property Fdconvex: TSFBool read FFdconvex;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFdcreaseAngle: TSFFloat;
    public property FdcreaseAngle: TSFFloat read FFdcreaseAngle;

    private FFdnormalIndex: TMFInt32;
    public property FdnormalIndex: TMFInt32 read FFdnormalIndex;

    private FFdtexCoordIndex: TMFInt32;
    public property FdtexCoordIndex: TMFInt32 read FFdtexCoordIndex;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeSphere_2 = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeBox.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdsize := TSFVec3f.Create('size', Vector3Single(2, 2, 2));
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeBox.ClassNodeTypeName: string;
begin
  Result := 'Box';
end;

function TNodeBox.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeBox.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCone_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbottom := TSFBool.Create('bottom', true);
  FFdbottom.Exposed := false;
  Fields.Add(FFdbottom);

  FFdbottomRadius := TSFFloat.Create('bottomRadius', 1);
  FFdbottomRadius.Exposed := false;
  Fields.Add(FFdbottomRadius);
  { X3D specification comment: (0,Inf) }

  FFdheight := TSFFloat.Create('height', 2);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);
  { X3D specification comment: (0,Inf) }

  FFdside := TSFBool.Create('side', true);
  FFdside.Exposed := false;
  Fields.Add(FFdside);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeCone_2.ClassNodeTypeName: string;
begin
  Result := 'Cone';
end;

function TNodeCone_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCone_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCone_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

constructor TNodeCylinder_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdbottom := TSFBool.Create('bottom', true);
  FFdbottom.Exposed := false;
  Fields.Add(FFdbottom);

  FFdheight := TSFFloat.Create('height', 2);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);
  { X3D specification comment: (0,Inf) }

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdside := TSFBool.Create('side', true);
  FFdside.Exposed := false;
  Fields.Add(FFdside);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdtop := TSFBool.Create('top', true);
  FFdtop.Exposed := false;
  Fields.Add(FFdtop);
end;

class function TNodeCylinder_2.ClassNodeTypeName: string;
begin
  Result := 'Cylinder';
end;

function TNodeCylinder_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCylinder_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCylinder_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

constructor TNodeElevationGrid.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_height := TVRMLEvent.Create('set_height', TMFFloat, true);
  Events.Add(FEventset_height);

  FFdattrib := TMFNode.Create(Self, 'attrib', [TNodeX3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeX3DColorNode]);
  Fields.Add(FFdcolor);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [TNodeFogCoordinate]);
  Fields.Add(FFdfogCoord);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeX3DNormalNode]);
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
  Fields.Add(FFdtexCoord);

  FFdccw := TSFBool.Create('ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', true);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdheight := TMFFloat.Create('height', []);
  FFdheight.Exposed := false;
  Fields.Add(FFdheight);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalPerVertex := TSFBool.Create('normalPerVertex', true);
  FFdnormalPerVertex.Exposed := false;
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdxDimension := TSFInt32.Create('xDimension', 0);
  FFdxDimension.Exposed := false;
  Fields.Add(FFdxDimension);
  { X3D specification comment: [0,Inf) }

  FFdxSpacing := TSFFloat.Create('xSpacing', 1.0);
  FFdxSpacing.Exposed := false;
  Fields.Add(FFdxSpacing);
  { X3D specification comment: (0,Inf) }

  FFdzDimension := TSFInt32.Create('zDimension', 0);
  FFdzDimension.Exposed := false;
  Fields.Add(FFdzDimension);
  { X3D specification comment: [0,Inf) }

  FFdzSpacing := TSFFloat.Create('zSpacing', 1.0);
  FFdzSpacing.Exposed := false;
  Fields.Add(FFdzSpacing);
  { X3D specification comment: (0,Inf) }
end;

class function TNodeElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'ElevationGrid';
end;

function TNodeElevationGrid.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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

function TNodeElevationGrid.Color: TMFVec3f;
begin
  if (FdColor.Value <> nil) and
     (FdColor.Value is TNodeColor) then
    Result := TNodeColor(FdColor.Value).FdColor else
    Result := nil;
end;

constructor TNodeExtrusion.Create(const ANodeName: string;
  const AWWWBasePath: string);
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

  FFdbeginCap := TSFBool.Create('beginCap', true);
  FFdbeginCap.Exposed := false;
  Fields.Add(FFdbeginCap);

  FFdccw := TSFBool.Create('ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdconvex := TSFBool.Create('convex', true);
  FFdconvex.Exposed := false;
  Fields.Add(FFdconvex);

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdcrossSection := TMFVec2f.Create('crossSection', [ Vector2Single(1, 1), Vector2Single(1, -1),  Vector2Single(-1, -1), Vector2Single(-1, 1), Vector2Single(1, 1) ]);
  FFdcrossSection.Exposed := false;
  Fields.Add(FFdcrossSection);
  { X3D specification comment: (-Inf,Inf) }

  FFdendCap := TSFBool.Create('endCap', true);
  FFdendCap.Exposed := false;
  Fields.Add(FFdendCap);

  FFdorientation := TMFRotation.Create('orientation', [ Vector4Single(0, 0, 1, 0) ]);
  FFdorientation.Exposed := false;
  Fields.Add(FFdorientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TMFVec2f.Create('scale', Vector2Single(1, 1));
  FFdscale.Exposed := false;
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdspine := TMFVec3f.Create('spine', [ Vector3Single(0, 0, 0), Vector3Single(0, 1, 0) ]);
  FFdspine.Exposed := false;
  Fields.Add(FFdspine);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeExtrusion.ClassNodeTypeName: string;
begin
  Result := 'Extrusion';
end;

function TNodeExtrusion.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeExtrusion.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeExtrusion.CrossSection3D(Index: integer): TVector3Single;
begin
  Result[0] := FdCrossSection.Items.Items[Index][0];
  Result[1] := 0;
  Result[2] := FdCrossSection.Items.Items[Index][1];
end;

constructor TNodeIndexedFaceSet_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
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

  FFdcolorIndex := TMFInt32.Create('colorIndex', []);
  FFdcolorIndex.Exposed := false;
  Fields.Add(FFdcolorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdconvex := TSFBool.Create('convex', true);
  FFdconvex.Exposed := false;
  Fields.Add(FFdconvex);

  FFdcoordIndex := TMFInt32.Create('coordIndex', []);
  FFdcoordIndex.SaveToStreamLineUptoNegative := true;
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdcreaseAngle := TSFFloat.Create('creaseAngle', 0);
  FFdcreaseAngle.Exposed := false;
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdnormalIndex := TMFInt32.Create('normalIndex', []);
  FFdnormalIndex.Exposed := false;
  Fields.Add(FFdnormalIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdtexCoordIndex := TMFInt32.Create('texCoordIndex', []);
  FFdtexCoordIndex.SaveToStreamLineUptoNegative := true;
  FFdtexCoordIndex.Exposed := false;
  Fields.Add(FFdtexCoordIndex);
  { X3D specification comment: [-1,Inf) }
end;

class function TNodeIndexedFaceSet_2.ClassNodeTypeName: string;
begin
  Result := 'IndexedFaceSet';
end;

function TNodeIndexedFaceSet_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeIndexedFaceSet_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeIndexedFaceSet_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeIndexedFaceSet_2.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

constructor TNodeSphere_2.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeSphere_2.ClassNodeTypeName: string;
begin
  Result := 'Sphere';
end;

function TNodeSphere_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeSphere_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSphere_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

procedure RegisterGeometry3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBox,
    TNodeCone_2,
    TNodeCylinder_2,
    TNodeElevationGrid,
    TNodeExtrusion,
    TNodeIndexedFaceSet_2,
    TNodeSphere_2
  ]);
end;

{$endif read_implementation}
