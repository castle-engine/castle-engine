{
  Copyright 2002-2011 Michalis Kamburelis.

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
  TNodeBox = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec3f;
    public property Fdsize: TSFVec3f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TNodeCone_2 = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TNodeCylinder_2 = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;
    function TexCoordField: TSFNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;

  TNodeElevationGrid = class(TNodeX3DGeometryNode)
  private
    procedure EventSet_HeightReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

    function Color: TMFVec3f; override;
    function ColorRGBA: TMFColorRGBA; override;
    function FogCoord: TMFFloat; override;
    function Attrib: TMFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeExtrusion = class(TNodeX3DGeometryNode)
  private
    procedure Eventset_crossSectionReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_orientationReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_scaleReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_spineReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

    { Check is spine closed. This happens when "spine" field is non-empty
      and it's first and last points are equal. }
    function SpineClosed: boolean;

    { Check is crossSection closed. This happens when "crossSection"
      field is non-empty and it's first and last points are equal. }
    function CrossSectionClosed: boolean;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeIndexedFaceSet_2 = class(TNodeX3DComposedGeometryNode)
  private
    procedure Eventset_colorIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_coordIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_normalIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
    procedure Eventset_texCoordIndexReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    function CoordIndex: TMFLong; override;

    procedure CoordPolygons(
      State: TVRMLGraphTraverseState;
      PolygonHandler: TIndexedPolygonHandler); override;

    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeSphere_2 = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;

    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function AutoGenerate3DTexCoords: boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeBox.CreateNode;
begin
  inherited;

  FFdsize := TSFVec3f.Create(Self, 'size', Vector3Single(2, 2, 2));
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chGeometry];
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);
end;

class function TNodeBox.ClassNodeTypeName: string;
begin
  Result := 'Box';
end;

function TNodeBox.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeBox.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeBox.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeBox.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeCone_2.CreateNode;
begin
  inherited;

  FFdbottom := TSFBool.Create(Self, 'bottom', true);
   Fdbottom.Exposed := false;
   Fdbottom.ChangesAlways := [chGeometry];
  Fields.Add(FFdbottom);

  FFdbottomRadius := TSFFloat.Create(Self, 'bottomRadius', 1);
   FdbottomRadius.Exposed := false;
   FdbottomRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdbottomRadius);
  { X3D specification comment: (0,Inf) }

  FFdheight := TSFFloat.Create(Self, 'height', 2);
   Fdheight.Exposed := false;
   Fdheight.ChangesAlways := [chGeometry];
  Fields.Add(FFdheight);
  { X3D specification comment: (0,Inf) }

  FFdside := TSFBool.Create(Self, 'side', true);
   Fdside.Exposed := false;
   Fdside.ChangesAlways := [chGeometry];
  Fields.Add(FFdside);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);
end;

class function TNodeCone_2.ClassNodeTypeName: string;
begin
  Result := 'Cone';
end;

function TNodeCone_2.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeCone_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCone_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TNodeCone_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  inherited;
  FdtexCoord.EnumerateValid(Func);
end;

function TNodeCone_2.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeCone_2.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeCylinder_2.CreateNode;
begin
  inherited;

  FFdbottom := TSFBool.Create(Self, 'bottom', true);
   Fdbottom.Exposed := false;
   Fdbottom.ChangesAlways := [chGeometry];
  Fields.Add(FFdbottom);

  FFdheight := TSFFloat.Create(Self, 'height', 2);
   Fdheight.Exposed := false;
   Fdheight.ChangesAlways := [chGeometry];
  Fields.Add(FFdheight);
  { X3D specification comment: (0,Inf) }

  FFdradius := TSFFloat.Create(Self, 'radius', 1);
   Fdradius.Exposed := false;
   Fdradius.ChangesAlways := [chGeometry];
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdside := TSFBool.Create(Self, 'side', true);
   Fdside.Exposed := false;
   Fdside.ChangesAlways := [chGeometry];
  Fields.Add(FFdside);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdtop := TSFBool.Create(Self, 'top', true);
   Fdtop.Exposed := false;
   Fdtop.ChangesAlways := [chGeometry];
  Fields.Add(FFdtop);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);
end;

class function TNodeCylinder_2.ClassNodeTypeName: string;
begin
  Result := 'Cylinder';
end;

function TNodeCylinder_2.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeCylinder_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeCylinder_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

procedure TNodeCylinder_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  inherited;
  FdtexCoord.EnumerateValid(Func);
end;

function TNodeCylinder_2.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeCylinder_2.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

procedure TNodeElevationGrid.CreateNode;
begin
  inherited;

  FEventset_height := TVRMLEvent.Create(Self, 'set_height', TMFFloat, true);
  Events.Add(FEventset_height);
  Eventset_height.OnReceive.Add(@EventSet_HeightReceive);

  FFdattrib := TMFNode.Create(Self, 'attrib', [TNodeX3DVertexAttributeNode]);
   Fdattrib.ChangesAlways := [chGeometry];
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [TNodeX3DColorNode]);
   Fdcolor.ChangesAlways := [chGeometry];
  Fields.Add(FFdcolor);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [TNodeFogCoordinate]);
   FdfogCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdfogCoord);

  FFdnormal := TSFNode.Create(Self, 'normal', [TNodeX3DNormalNode]);
   Fdnormal.ChangesAlways := [chGeometry];
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdcolorPerVertex := TSFBool.Create(Self, 'colorPerVertex', true);
   FdcolorPerVertex.Exposed := false;
   FdcolorPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdcolorPerVertex);

  FFdcreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdcreaseAngle.Exposed := false;
   FdcreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdheight := TMFFloat.Create(Self, 'height', []);
   Fdheight.Exposed := false;
   Fdheight.ChangesAlways := [chGeometry];
  Fields.Add(FFdheight);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalPerVertex := TSFBool.Create(Self, 'normalPerVertex', true);
   FdnormalPerVertex.Exposed := false;
   FdnormalPerVertex.ChangesAlways := [chGeometry];
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdxDimension := TSFInt32.Create(Self, 'xDimension', 0);
   FdxDimension.Exposed := false;
   FdxDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdxDimension);
  { X3D specification comment: [0,Inf) }

  FFdxSpacing := TSFFloat.Create(Self, 'xSpacing', 1.0);
   FdxSpacing.Exposed := false;
   FdxSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdxSpacing);
  { X3D specification comment: (0,Inf) }

  FFdzDimension := TSFInt32.Create(Self, 'zDimension', 0);
   FdzDimension.Exposed := false;
   FdzDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdzDimension);
  { X3D specification comment: [0,Inf) }

  FFdzSpacing := TSFFloat.Create(Self, 'zSpacing', 1.0);
   FdzSpacing.Exposed := false;
   FdzSpacing.ChangesAlways := [chGeometry];
  Fields.Add(FFdzSpacing);
  { X3D specification comment: (0,Inf) }
end;

class function TNodeElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'ElevationGrid';
end;

function TNodeElevationGrid.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
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

function TNodeElevationGrid.ColorRGBA: TMFColorRGBA;
begin
  if (FdColor.Value <> nil) and
     (FdColor.Value is TNodeColorRGBA) then
    Result := TNodeColorRGBA(FdColor.Value).FdColor else
    Result := nil;
end;

function TNodeElevationGrid.FogCoord: TMFFloat;
begin
  if (FdFogCoord.Value <> nil) and
     (FdFogCoord.Value is TNodeFogCoordinate) then
    Result := TNodeFogCoordinate(FdFogCoord.Value).FdDepth else
    Result := nil;
end;

function TNodeElevationGrid.Attrib: TMFNode;
begin
  Result := FdAttrib;
end;

procedure TNodeElevationGrid.EventSet_HeightReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdHeight.Assign(Value);
  FdHeight.Changed;
end;

procedure TNodeExtrusion.CreateNode;
begin
  inherited;

  FEventset_crossSection := TVRMLEvent.Create(Self, 'set_crossSection', TMFVec2f, true);
  Events.Add(FEventset_crossSection);
  Eventset_crossSection.OnReceive.Add(@EventSet_crossSectionReceive);

  FEventset_orientation := TVRMLEvent.Create(Self, 'set_orientation', TMFRotation, true);
  Events.Add(FEventset_orientation);
  Eventset_orientation.OnReceive.Add(@EventSet_orientationReceive);

  FEventset_scale := TVRMLEvent.Create(Self, 'set_scale', TMFVec2f, true);
  Events.Add(FEventset_scale);
  Eventset_scale.OnReceive.Add(@EventSet_scaleReceive);

  FEventset_spine := TVRMLEvent.Create(Self, 'set_spine', TMFVec3f, true);
  Events.Add(FEventset_spine);
  Eventset_spine.OnReceive.Add(@EventSet_spineReceive);

  FFdbeginCap := TSFBool.Create(Self, 'beginCap', true);
   FdbeginCap.Exposed := false;
   FdbeginCap.ChangesAlways := [chGeometry];
  Fields.Add(FFdbeginCap);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdconvex := TSFBool.Create(Self, 'convex', true);
   Fdconvex.Exposed := false;
   Fdconvex.ChangesAlways := [chGeometry];
  Fields.Add(FFdconvex);

  FFdcreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdcreaseAngle.Exposed := false;
   FdcreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdcrossSection := TMFVec2f.Create(Self, 'crossSection', [ Vector2Single(1, 1), Vector2Single(1, -1),  Vector2Single(-1, -1), Vector2Single(-1, 1), Vector2Single(1, 1) ]);
   FdcrossSection.Exposed := false;
   FdcrossSection.ChangesAlways := [chGeometry];
  Fields.Add(FFdcrossSection);
  { X3D specification comment: (-Inf,Inf) }

  FFdendCap := TSFBool.Create(Self, 'endCap', true);
   FdendCap.Exposed := false;
   FdendCap.ChangesAlways := [chGeometry];
  Fields.Add(FFdendCap);

  FFdorientation := TMFRotation.Create(Self, 'orientation', [ Vector4Single(0, 0, 1, 0) ]);
   Fdorientation.Exposed := false;
   Fdorientation.ChangesAlways := [chGeometry];
  Fields.Add(FFdorientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TMFVec2f.Create(Self, 'scale', Vector2Single(1, 1));
   Fdscale.Exposed := false;
   Fdscale.ChangesAlways := [chGeometry];
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdspine := TMFVec3f.Create(Self, 'spine', [ Vector3Single(0, 0, 0), Vector3Single(0, 1, 0) ]);
   Fdspine.Exposed := false;
   Fdspine.ChangesAlways := [chGeometry];
  Fields.Add(FFdspine);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeExtrusion.ClassNodeTypeName: string;
begin
  Result := 'Extrusion';
end;

function TNodeExtrusion.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
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

function TNodeExtrusion.SpineClosed: boolean;
var
  SpinePoints: TDynVector3SingleArray;
begin
  SpinePoints := FdSpine.Items;
  Result := (SpinePoints.Count <> 0) and
    VectorsPerfectlyEqual(SpinePoints.Items[SpinePoints.High],
                          SpinePoints.Items[0]);
end;

function TNodeExtrusion.CrossSectionClosed: boolean;
var
  CrossSectionPoints: TDynVector2SingleArray;
begin
  CrossSectionPoints := FdCrossSection.Items;
  Result := (CrossSectionPoints.Count <> 0) and
    VectorsPerfectlyEqual(CrossSectionPoints.Items[CrossSectionPoints.High],
                          CrossSectionPoints.Items[0]);
end;

function TNodeExtrusion.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;

{ Some of the things that are non-optimal when you will render
  Extrusion using this Proxy instead of directly rendering node:

  - In specialized Extrusion rendering, you now that sides are
    composed of quads, and so you can use QUAD_STRIP to render them.
    This allows vertices sharing. IndexedFaceSet uses vertex arrays,
    that also provides sharing but may be slightly worse.

  - In specialized Extrusion rendering, you know that sides' quads
    are convex. There's never any need to triangulate them carefully.
    For IndexedFaceSet, when caps may be non-convex, you have to set
    convex field to false, so sides will be unnecessarily triangulated
    with the slower algorithm.

  You gain:

  - creaseAngle support, which is really the crucial point why
    I'm implementing this. I already have a specialized Extrusion
    rendering, but adding there creaseAngle support is a major pain.

    You would have to calculate all coordinates first, and then calculate
    their normal vectors using a specialized routine like CreateNormals
    that knows about Extrusion connectivity. This is painful to implement,
    and naive implementation would even suffer from a slowdown not present
    in Proxy approach: calculating all coordinates two times (before
    calculating normals, and later for actual rendering). Sure, smarter
    approach would remember these coordinates. Proxy approach provides
    this automatically, saving coordinates inside Coordinate node,
    for both normals calculation and rendering.

  - A little ease of implementation: 230 lines of Proxy approach
    versus 390 lines of TExtrusionRenderer. And remember that
    actually handling creaseAngle would complicate TExtrusionRenderer
    even more (that's why I didn't dare to do it...), and that
    Proxy approach could also be used to implement things like
    bounding box and triangulate eventually (although for now we use
    our optimized methods).
}

var
  IFS: TNodeIndexedFaceSet_2 absolute Result;
  Index: Integer;
  E: TVRMLExtrusion;

  procedure NextIndex(SpineIndex, CrossSectionIndex: Integer;
    SpecialTex: boolean; SpecialTexIndex: Integer);
  begin
    if SpecialTex then
      IFS.FdTexCoordIndex.Items.Items[Index] := SpecialTexIndex else
      IFS.FdTexCoordIndex.Items.Items[Index] :=
        SpineIndex * FdCrossSection.Count + CrossSectionIndex;

    { For closed spine and/or crossSection, specify the same indexes.
      For coordIndex (for texCoordIndex, this is not correct, texture
      may have a seam there).
      This will allow the correct smoothing between spine/crossSection
      edges when calculating normal vectors. }

   if (SpineIndex = FdSpine.Items.High) and
       E.BeginEndCapsMatching then
      SpineIndex := 0;

    if (CrossSectionIndex = FdCrossSection.Items.High) and
       E.CrossSectionClosed then
      CrossSectionIndex := 0;

    IFS.FdCoordIndex.Items.Items[Index] :=
      SpineIndex * FdCrossSection.Count + CrossSectionIndex;

    Inc(Index);
  end;

  procedure NextIndexFaceDelimiter;
  begin
    IFS.FdCoordIndex.Items.Items[Index] := -1;
    IFS.FdTexCoordIndex.Items.Items[Index] := -1;
    Inc(Index);
  end;

  function SideTexCoord(SpineIndex, CrossSectionIndex: Integer): TVector2Single;
  begin
    { Check ranges, to avoid dividing by zero (eventually the
      corresponding coord is set to a predictable zero :) ). }
    if FdCrossSection.Count > 1 then
      Result[0] := CrossSectionIndex / (FdCrossSection.Count - 1) else
      Result[0] := 0;
    if E.High > 0 then
      Result[1] := SpineIndex / E.High else
      Result[1] := 0;
  end;

var
  CoordNode: TNodeCoordinate;
  TexCoordNode: TNodeTextureCoordinate;
  Transform: TMatrix4Single;
  I, J, Len, SidesCount: Integer;
  LastY, LastZ: TVector3Single;
  TexCapsMin, TexCapsMax: TVector2Single;
  TexCapsTranslation, TexCapsScale: Single;
begin
  IFS := TNodeIndexedFaceSet_2.Create(NodeName, WWWBasePath);
  try
    E := TVRMLExtrusion.Create;
    try
      E.Node := Self;

      { calculate SidesCount = number of verts on sides, often used number }
      SidesCount := (E.High + 1) * FdCrossSection.Count;

      { Fill CoordNode }

      CoordNode := TNodeCoordinate.Create('', WWWBasePath);
      IFS.FdCoord.Value := CoordNode;

      CoordNode.FdPoint.Items.Length := SidesCount;

      for I := 0 to E.High do
      begin
        E.SpineTransformTo1st(I, LastY, LastZ, Transform);
        for J := 0 to FdCrossSection.Count - 1 do
        begin
          CoordNode.FdPoint.Items.Items[I * FdCrossSection.Count + J] :=
            MatrixMultPoint(Transform, CrossSection3D(J));
        end;
      end;

      { Fill TexCoordNode }

      TexCoordNode := TNodeTextureCoordinate.Create('', WWWBasePath);
      IFS.FdTexCoord.Value := TexCoordNode;

      Len := SidesCount;
      if FdBeginCap.Value or FdEndCap.Value then
        Len += FdCrossSection.Count;
      TexCoordNode.FdPoint.Items.Length := Len;

      for I := 0 to E.High do
        for J := 0 to FdCrossSection.Count - 1 do
          TexCoordNode.FdPoint.Items.Items[I * FdCrossSection.Count + J] :=
            SideTexCoord(I, J);

      { calculate TexCapsTranslation, TexCapsScale }
      if FdBeginCap.Value or FdEndCap.Value then
      begin
        TexCapsTranslation := 0;
        TexCapsScale := 1;
        if FdCrossSection.Items.MinMax(TexCapsMin, TexCapsMax) then
        begin
          if TexCapsMax[0] - TexCapsMin[0] >=
             TexCapsMax[1] - TexCapsMin[1] then
          begin
            if not Zero(TexCapsMax[0] - TexCapsMin[0]) then
            begin
              { scale such that U is along the X of cap.
                Coord calculation will go like
                  u := (x - TexCapsMin[0]) / (TexCapsMax[0] - TexCapsMin[0]) }
              TexCapsTranslation := -TexCapsMin[0];
              TexCapsScale := 1 /  (TexCapsMax[0] - TexCapsMin[0]);
            end;
          end else
          begin
            if not Zero(TexCapsMax[1] - TexCapsMin[1]) then
            begin
              TexCapsTranslation := -TexCapsMin[1];
              TexCapsScale := 1 /  (TexCapsMax[1] - TexCapsMin[1]);
            end;
          end;
        end;

        for I := 0 to FdCrossSection.Count - 1 do
        begin
          TexCoordNode.FdPoint.Items.Items[
            SidesCount + I] :=
            VectorScale(VectorAdd(
              FdCrossSection.Items.Items[I],
              Vector2Single(TexCapsTranslation, TexCapsTranslation)),
              TexCapsScale);
        end;
      end;

      { Fill CoordIndex and TexCoordIndex }

      Len := 5 * E.High * (FdCrossSection.Count - 1);
      if FdBeginCap.Value then
        Len += FdCrossSection.Count + 1;
      if FdEndCap  .Value then
        Len += FdCrossSection.Count + 1;
      IFS.FdCoordIndex.Items.Length := Len;
      IFS.FdTexCoordIndex.Items.Length := Len;

      Index := 0;

      for I := 1 to E.High do
      begin
        for J := 1 to FdCrossSection.Count - 1 do
        begin
          { The VRML / X3D specifications say that ordering of sides
            should be
              spine[0](crossSection[0], crossSection[1])
              spine[1](crossSection[1], crossSection[0])
            This is important, because we want faces to be ordered just
            like the specification requires. This makes using "ccw"
            and "solid" fields predictable for VRML author, since he
            knows which side will be CCW and which CW.

            So below we produce the same order. }

          NextIndex(I - 1, J - 1, false, 0);
          NextIndex(I - 1, J    , false, 0);
          NextIndex(I    , J    , false, 0);
          NextIndex(I    , J - 1, false, 0);
          NextIndexFaceDelimiter;
        end;
      end;

      if FdBeginCap.Value then
      begin
        { "downto" order, to match the ordering of sides, this makes
          things such a "solid TRUE" work Ok. }
        for J := FdCrossSection.Count - 1 downto 0 do
          NextIndex(0, J, true, SidesCount + J);
        NextIndexFaceDelimiter;
      end;

      if FdEndCap.Value then
      begin
        for J := 0 to FdCrossSection.Count - 1 do
          NextIndex(E.High, J, true, SidesCount + J);
        NextIndexFaceDelimiter;
      end;

      { Check that we actually filled all indexes we planned }
      Assert(Index = IFS.FdCoordIndex.Items.Count);

    finally FreeAndNil(E) end;

    IFS.FdSolid.Value := FdSolid.Value;
    IFS.FdCcw.Value := FdCcw.Value;
    IFS.FdCreaseAngle.Value := FdCreaseAngle.Value;
    IFS.FdConvex.Value := FdConvex.Value;
  except FreeAndNil(Result); raise end;
end;

function TNodeExtrusion.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TNodeExtrusion.EventSet_crossSectionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcrossSection.Assign(Value);
  FdcrossSection.Changed;
end;

procedure TNodeExtrusion.EventSet_orientationReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdorientation.Assign(Value);
  Fdorientation.Changed;
end;

procedure TNodeExtrusion.EventSet_scaleReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdscale.Assign(Value);
  Fdscale.Changed;
end;

procedure TNodeExtrusion.EventSet_spineReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  Fdspine.Assign(Value);
  Fdspine.Changed;
end;

procedure TNodeIndexedFaceSet_2.CreateNode;
begin
  inherited;

  FEventset_colorIndex := TVRMLEvent.Create(Self, 'set_colorIndex', TMFInt32, true);
  Events.Add(FEventset_colorIndex);
  Eventset_colorIndex.OnReceive.Add(@EventSet_colorIndexReceive);

  FEventset_coordIndex := TVRMLEvent.Create(Self, 'set_coordIndex', TMFInt32, true);
  Events.Add(FEventset_coordIndex);
  Eventset_coordIndex.OnReceive.Add(@EventSet_coordIndexReceive);

  FEventset_normalIndex := TVRMLEvent.Create(Self, 'set_normalIndex', TMFInt32, true);
  Events.Add(FEventset_normalIndex);
  Eventset_normalIndex.OnReceive.Add(@EventSet_normalIndexReceive);

  FEventset_texCoordIndex := TVRMLEvent.Create(Self, 'set_texCoordIndex', TMFInt32, true);
  Events.Add(FEventset_texCoordIndex);
  Eventset_texCoordIndex.OnReceive.Add(@EventSet_texCoordIndexReceive);

  FFdcolorIndex := TMFInt32.Create(Self, 'colorIndex', []);
   FdcolorIndex.SaveToStreamLineUptoNegative := true;
   FdcolorIndex.Exposed := false;
   FdcolorIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdcolorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdconvex := TSFBool.Create(Self, 'convex', true);
   Fdconvex.Exposed := false;
   Fdconvex.ChangesAlways := [chGeometry];
  Fields.Add(FFdconvex);

  FFdcoordIndex := TMFInt32.Create(Self, 'coordIndex', []);
   FdcoordIndex.SaveToStreamLineUptoNegative := true;
   FdcoordIndex.Exposed := false;
   FdcoordIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdcreaseAngle := TSFFloat.Create(Self, 'creaseAngle', 0);
   FdcreaseAngle.Exposed := false;
   FdcreaseAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdcreaseAngle);
  { X3D specification comment: [0,Inf) }

  FFdnormalIndex := TMFInt32.Create(Self, 'normalIndex', []);
   FdnormalIndex.SaveToStreamLineUptoNegative := true;
   FdnormalIndex.Exposed := false;
   FdnormalIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdnormalIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdtexCoordIndex := TMFInt32.Create(Self, 'texCoordIndex', []);
   FdtexCoordIndex.SaveToStreamLineUptoNegative := true;
   FdtexCoordIndex.Exposed := false;
   FdtexCoordIndex.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoordIndex);
  { X3D specification comment: [-1,Inf) }
end;

class function TNodeIndexedFaceSet_2.ClassNodeTypeName: string;
begin
  Result := 'IndexedFaceSet';
end;

function TNodeIndexedFaceSet_2.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeIndexedFaceSet_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeIndexedFaceSet_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TNodeIndexedFaceSet_2.CoordIndex: TMFLong;
begin
  Result := FdCoordIndex;
end;

procedure TNodeIndexedFaceSet_2.EventSet_colorIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcolorIndex.Assign(Value);
  FdcolorIndex.Changed;
end;

procedure TNodeIndexedFaceSet_2.EventSet_coordIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdcoordIndex.Assign(Value);
  FdcoordIndex.Changed;
end;

procedure TNodeIndexedFaceSet_2.EventSet_normalIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdnormalIndex.Assign(Value);
  FdnormalIndex.Changed;
end;

procedure TNodeIndexedFaceSet_2.EventSet_texCoordIndexReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  FdtexCoordIndex.Assign(Value);
  FdtexCoordIndex.Changed;
end;

procedure TNodeSphere_2.CreateNode;
begin
  inherited;

  FFdradius := TSFFloat.Create(Self, 'radius', 1);
   Fdradius.Exposed := false;
   Fdradius.ChangesAlways := [chGeometry];
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinateGenerator, TNodeProjectedTextureCoordinate, TNodeMultiTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);
end;

class function TNodeSphere_2.ClassNodeTypeName: string;
begin
  Result := 'Sphere';
end;

function TNodeSphere_2.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeSphere_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeSphere_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TNodeSphere_2.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TNodeSphere_2.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
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
