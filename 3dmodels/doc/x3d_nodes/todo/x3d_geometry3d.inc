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
  end;

  TNodeCone = class(TNodeX3DGeometryNode)
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
  end;

  TNodeCylinder = class(TNodeX3DGeometryNode)
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
  end;

  TNodeIndexedFaceSet = class(TNodeX3DComposedGeometryNode)
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
  end;

  TNodeSphere = class(TNodeX3DGeometryNode)
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

  DefaultContainerField := 'children';
end;

class function TNodeBox.ClassNodeTypeName: string;
begin
  Result := 'Box';
end;

function TNodeBox.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBox.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCone.Create(const ANodeName: string;
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

  DefaultContainerField := 'children';
end;

class function TNodeCone.ClassNodeTypeName: string;
begin
  Result := 'Cone';
end;

function TNodeCone.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCone.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCylinder.Create(const ANodeName: string;
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

  DefaultContainerField := 'children';
end;

class function TNodeCylinder.ClassNodeTypeName: string;
begin
  Result := 'Cylinder';
end;

function TNodeCylinder.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCylinder.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeElevationGrid.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_height := TVRMLEvent.Create('set_height', TMFFloat, true);
  Events.Add(FEventset_height);

  FFdattrib := TMFNode.Create(Self, 'attrib', [X3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [X3DColorNode]);
  Fields.Add(FFdcolor);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [FogCoordinate]);
  Fields.Add(FFdfogCoord);

  FFdnormal := TSFNode.Create(Self, 'normal', [X3DNormalNode]);
  Fields.Add(FFdnormal);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [X3DTextureCoordinateNode]);
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

  DefaultContainerField := 'children';
end;

class function TNodeElevationGrid.ClassNodeTypeName: string;
begin
  Result := 'ElevationGrid';
end;

function TNodeElevationGrid.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeElevationGrid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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

  FFdcrossSection := TMFVec2f.Create('crossSection', [1 1 1 -1 -1 -1 -1 1 1 1]);
  FFdcrossSection.Exposed := false;
  Fields.Add(FFdcrossSection);
  { X3D specification comment: (-Inf,Inf) }

  FFdendCap := TSFBool.Create('endCap', true);
  FFdendCap.Exposed := false;
  Fields.Add(FFdendCap);

  FFdorientation := TMFRotation.Create('orientation', Vector3Single(0, 0, 1), 0);
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

  FFdspine := TMFVec3f.Create('spine', [0 0 0 0 1 0]);
  FFdspine.Exposed := false;
  Fields.Add(FFdspine);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeExtrusion.ClassNodeTypeName: string;
begin
  Result := 'Extrusion';
end;

function TNodeExtrusion.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeExtrusion.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIndexedFaceSet.Create(const ANodeName: string;
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
  FFdtexCoordIndex.Exposed := false;
  Fields.Add(FFdtexCoordIndex);
  { X3D specification comment: [-1,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeIndexedFaceSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedFaceSet';
end;

function TNodeIndexedFaceSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIndexedFaceSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSphere.Create(const ANodeName: string;
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

  DefaultContainerField := 'children';
end;

class function TNodeSphere.ClassNodeTypeName: string;
begin
  Result := 'Sphere';
end;

function TNodeSphere.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSphere.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterGeometry3DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBox,
    TNodeCone,
    TNodeCylinder,
    TNodeElevationGrid,
    TNodeExtrusion,
    TNodeIndexedFaceSet,
    TNodeSphere
  ]);
end;

{$endif read_implementation}
