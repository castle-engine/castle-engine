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
  TNodeX3DColorNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DComposedGeometryNode = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdattrib: TMFNode;
    public property Fdattrib: TMFNode read FFdattrib;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

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

    private FFdnormalPerVertex: TSFBool;
    public property FdnormalPerVertex: TSFBool read FFdnormalPerVertex;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

  TNodeX3DCoordinateNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DGeometricPropertyNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DGeometryNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeX3DNormalNode = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeClipPlane = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    private FFdplane: TSFVec4f;
    public property Fdplane: TSFVec4f read FFdplane;
  end;

  TNodeColor = class(TNodeX3DColorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcolor: TMFColor;
    public property Fdcolor: TMFColor read FFdcolor;
  end;

  TNodeColorRGBA = class(TNodeX3DColorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcolor: TMFColorRGBA;
    public property Fdcolor: TMFColorRGBA read FFdcolor;
  end;

  TNodeCoordinate = class(TNodeX3DCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3f;
    public property Fdpoint: TMFVec3f read FFdpoint;
  end;

  TNodeIndexedLineSet = class(TNodeX3DGeometryNode)
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

    private FFdattrib: TMFNode;
    public property Fdattrib: TMFNode read FFdattrib;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFdfogCoord: TSFNode;
    public property FdfogCoord: TSFNode read FFdfogCoord;

    private FFdcolorIndex: TMFInt32;
    public property FdcolorIndex: TMFInt32 read FFdcolorIndex;

    private FFdcolorPerVertex: TSFBool;
    public property FdcolorPerVertex: TSFBool read FFdcolorPerVertex;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;
  end;

  TNodeIndexedTriangleFanSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventset_index: TVRMLEvent;
    public property Eventset_index: TVRMLEvent read FEventset_index;

    private FFdindex: TMFInt32;
    public property Fdindex: TMFInt32 read FFdindex;
  end;

  TNodeIndexedTriangleSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventset_index: TVRMLEvent;
    public property Eventset_index: TVRMLEvent read FEventset_index;

    private FFdindex: TMFInt32;
    public property Fdindex: TMFInt32 read FFdindex;
  end;

  TNodeIndexedTriangleStripSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFInt32, in } { }
    private FEventset_index: TVRMLEvent;
    public property Eventset_index: TVRMLEvent read FEventset_index;

    private FFdindex: TMFInt32;
    public property Fdindex: TMFInt32 read FFdindex;
  end;

  TNodeLineSet = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdattrib: TMFNode;
    public property Fdattrib: TMFNode read FFdattrib;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFdfogCoord: TSFNode;
    public property FdfogCoord: TSFNode read FFdfogCoord;

    private FFdvertexCount: TMFInt32;
    public property FdvertexCount: TMFInt32 read FFdvertexCount;
  end;

  TNodeNormal = class(TNodeX3DNormalNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvector: TMFVec3f;
    public property Fdvector: TMFVec3f read FFdvector;
  end;

  TNodePointSet = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdattrib: TMFNode;
    public property Fdattrib: TMFNode read FFdattrib;

    private FFdcolor: TSFNode;
    public property Fdcolor: TSFNode read FFdcolor;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFdfogCoord: TSFNode;
    public property FdfogCoord: TSFNode read FFdfogCoord;
  end;

  TNodeTriangleFanSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfanCount: TMFInt32;
    public property FdfanCount: TMFInt32 read FFdfanCount;
  end;

  TNodeTriangleSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeTriangleStripSet = class(TNodeX3DComposedGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdstripCount: TMFInt32;
    public property FdstripCount: TMFInt32 read FFdstripCount;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DColorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DComposedGeometryNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdattrib := TMFNode.Create(Self, 'attrib', [X3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [X3DColorNode]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [X3DCoordinateNode]);
  Fields.Add(FFdcoord);

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

  FFdnormalPerVertex := TSFBool.Create('normalPerVertex', true);
  FFdnormalPerVertex.Exposed := false;
  Fields.Add(FFdnormalPerVertex);

  FFdsolid := TSFBool.Create('solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  DefaultContainerField := 'children';
end;

constructor TNodeX3DCoordinateNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DGeometricPropertyNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DGeometryNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeX3DNormalNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeClipPlane.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdenabled := TSFBool.Create('enabled', true);
  Fields.Add(FFdenabled);

  FFdplane := TSFVec4f.Create('plane', Vector4Single(0, 1, 0, 0));
  Fields.Add(FFdplane);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeClipPlane.ClassNodeTypeName: string;
begin
  Result := 'ClipPlane';
end;

function TNodeClipPlane.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeClipPlane.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeColor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TMFColor.Create('color', [NULL]);
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeColor.ClassNodeTypeName: string;
begin
  Result := 'Color';
end;

function TNodeColor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeColor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeColorRGBA.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcolor := TMFColorRGBA.Create('color', [NULL]);
  Fields.Add(FFdcolor);
  { X3D specification comment: [0,1] }

  DefaultContainerField := 'children';
end;

class function TNodeColorRGBA.ClassNodeTypeName: string;
begin
  Result := 'ColorRGBA';
end;

function TNodeColorRGBA.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeColorRGBA.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec3f.Create('point', [NULL]);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeCoordinate.ClassNodeTypeName: string;
begin
  Result := 'Coordinate';
end;

function TNodeCoordinate.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIndexedLineSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_colorIndex := TVRMLEvent.Create('set_colorIndex', TMFInt32, true);
  Events.Add(FEventset_colorIndex);

  FEventset_coordIndex := TVRMLEvent.Create('set_coordIndex', TMFInt32, true);
  Events.Add(FEventset_coordIndex);

  FFdattrib := TMFNode.Create(Self, 'attrib', [X3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [X3DColorNode]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [X3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [FogCoordinate]);
  Fields.Add(FFdfogCoord);

  FFdcolorIndex := TMFInt32.Create('colorIndex', []);
  FFdcolorIndex.Exposed := false;
  Fields.Add(FFdcolorIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFdcolorPerVertex := TSFBool.Create('colorPerVertex', true);
  FFdcolorPerVertex.Exposed := false;
  Fields.Add(FFdcolorPerVertex);

  FFdcoordIndex := TMFInt32.Create('coordIndex', []);
  FFdcoordIndex.Exposed := false;
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  DefaultContainerField := 'children';
end;

class function TNodeIndexedLineSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedLineSet';
end;

function TNodeIndexedLineSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIndexedLineSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIndexedTriangleFanSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_index := TVRMLEvent.Create('set_index', TMFInt32, true);
  Events.Add(FEventset_index);

  FFdindex := TMFInt32.Create('index', []);
  FFdindex.Exposed := false;
  Fields.Add(FFdindex);
  { X3D specification comment: [0,Inf) or -1 }

  DefaultContainerField := 'children';
end;

class function TNodeIndexedTriangleFanSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedTriangleFanSet';
end;

function TNodeIndexedTriangleFanSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIndexedTriangleFanSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIndexedTriangleSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_index := TVRMLEvent.Create('set_index', TMFInt32, true);
  Events.Add(FEventset_index);

  FFdindex := TMFInt32.Create('index', []);
  FFdindex.Exposed := false;
  Fields.Add(FFdindex);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeIndexedTriangleSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedTriangleSet';
end;

function TNodeIndexedTriangleSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIndexedTriangleSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIndexedTriangleStripSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_index := TVRMLEvent.Create('set_index', TMFInt32, true);
  Events.Add(FEventset_index);

  FFdindex := TMFInt32.Create('index', []);
  FFdindex.Exposed := false;
  Fields.Add(FFdindex);
  { X3D specification comment: [0,Inf) or -1 }

  DefaultContainerField := 'children';
end;

class function TNodeIndexedTriangleStripSet.ClassNodeTypeName: string;
begin
  Result := 'IndexedTriangleStripSet';
end;

function TNodeIndexedTriangleStripSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIndexedTriangleStripSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLineSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdattrib := TMFNode.Create(Self, 'attrib', [X3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [X3DColorNode]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [X3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [FogCoordinate]);
  Fields.Add(FFdfogCoord);

  FFdvertexCount := TMFInt32.Create('vertexCount', []);
  Fields.Add(FFdvertexCount);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLineSet.ClassNodeTypeName: string;
begin
  Result := 'LineSet';
end;

function TNodeLineSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLineSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNormal.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvector := TMFVec3f.Create('vector', []);
  Fields.Add(FFdvector);
  { X3D specification comment: [-1,1] }

  DefaultContainerField := 'children';
end;

class function TNodeNormal.ClassNodeTypeName: string;
begin
  Result := 'Normal';
end;

function TNodeNormal.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNormal.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePointSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdattrib := TMFNode.Create(Self, 'attrib', [X3DVertexAttributeNode]);
  Fields.Add(FFdattrib);

  FFdcolor := TSFNode.Create(Self, 'color', [X3DColorNode]);
  Fields.Add(FFdcolor);

  FFdcoord := TSFNode.Create(Self, 'coord', [X3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFdfogCoord := TSFNode.Create(Self, 'fogCoord', [FogCoordinate]);
  Fields.Add(FFdfogCoord);

  DefaultContainerField := 'children';
end;

class function TNodePointSet.ClassNodeTypeName: string;
begin
  Result := 'PointSet';
end;

function TNodePointSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePointSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTriangleFanSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfanCount := TMFInt32.Create('fanCount', []);
  Fields.Add(FFdfanCount);
  { X3D specification comment: [3,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeTriangleFanSet.ClassNodeTypeName: string;
begin
  Result := 'TriangleFanSet';
end;

function TNodeTriangleFanSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTriangleFanSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTriangleSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeTriangleSet.ClassNodeTypeName: string;
begin
  Result := 'TriangleSet';
end;

function TNodeTriangleSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTriangleSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTriangleStripSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdstripCount := TMFInt32.Create('stripCount', []);
  Fields.Add(FFdstripCount);
  { X3D specification comment: [3,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeTriangleStripSet.ClassNodeTypeName: string;
begin
  Result := 'TriangleStripSet';
end;

function TNodeTriangleStripSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTriangleStripSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterRenderingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeClipPlane,
    TNodeColor,
    TNodeColorRGBA,
    TNodeCoordinate,
    TNodeIndexedLineSet,
    TNodeIndexedTriangleFanSet,
    TNodeIndexedTriangleSet,
    TNodeIndexedTriangleStripSet,
    TNodeLineSet,
    TNodeNormal,
    TNodePointSet,
    TNodeTriangleFanSet,
    TNodeTriangleSet,
    TNodeTriangleStripSet
  ]);
end;

{$endif read_implementation}
