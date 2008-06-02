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
  TNodeArc2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdendAngle: TSFFloat;
    public property FdendAngle: TSFFloat read FFdendAngle;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdstartAngle: TSFFloat;
    public property FdstartAngle: TSFFloat read FFdstartAngle;
  end;

  TNodeArcClose2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosureType: TSFString;
    public property FdclosureType: TSFString read FFdclosureType;

    private FFdendAngle: TSFFloat;
    public property FdendAngle: TSFFloat read FFdendAngle;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdstartAngle: TSFFloat;
    public property FdstartAngle: TSFFloat read FFdstartAngle;
  end;

  TNodeCircle2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;
  end;

  TNodeDisk2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdinnerRadius: TSFFloat;
    public property FdinnerRadius: TSFFloat read FFdinnerRadius;

    private FFdouterRadius: TSFFloat;
    public property FdouterRadius: TSFFloat read FFdouterRadius;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

  { Polyline2D node.

    The "lineSegments" field uses our TVRMLField.AddAlternativeName mechanism,
    and when reading/saving VRML 97 file it will be named "point"
    (as it was in VRML 97 amendment 1 specification).
    In X3D normal name, "lineSegments", will be used. }
  TNodePolyline2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdlineSegments: TMFVec2f;
    public property FdlineSegments: TMFVec2f read FFdlineSegments;
  end;

  TNodePolypoint2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;
  end;

  TNodeRectangle2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdsize: TSFVec2f;
    public property Fdsize: TSFVec2f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

  TNodeTriangleSet2D = class(TNodeX3DGeometryNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvertices: TMFVec2f;
    public property Fdvertices: TMFVec2f read FFdvertices;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeArc2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdendAngle := TSFFloat.Create('endAngle', Pi/2);
  FFdendAngle.Exposed := false;
  Fields.Add(FFdendAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdstartAngle := TSFFloat.Create('startAngle', 0);
  FFdstartAngle.Exposed := false;
  Fields.Add(FFdstartAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  DefaultContainerField := 'children';
end;

class function TNodeArc2D.ClassNodeTypeName: string;
begin
  Result := 'Arc2D';
end;

function TNodeArc2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeArc2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeArcClose2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosureType := TSFString.Create('closureType', 'PIE');
  FFdclosureType.Exposed := false;
  Fields.Add(FFdclosureType);
  { X3D specification comment: ["PIE"|"CHORD"] }

  FFdendAngle := TSFFloat.Create('endAngle', Pi/2);
  FFdendAngle.Exposed := false;
  Fields.Add(FFdendAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', false);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFdstartAngle := TSFFloat.Create('startAngle', 0);
  FFdstartAngle.Exposed := false;
  Fields.Add(FFdstartAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  DefaultContainerField := 'children';
end;

class function TNodeArcClose2D.ClassNodeTypeName: string;
begin
  Result := 'ArcClose2D';
end;

function TNodeArcClose2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeArcClose2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCircle2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdradius := TSFFloat.Create('radius', 1);
  FFdradius.Exposed := false;
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeCircle2D.ClassNodeTypeName: string;
begin
  Result := 'Circle2D';
end;

function TNodeCircle2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCircle2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeDisk2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdinnerRadius := TSFFloat.Create('innerRadius', 0);
  FFdinnerRadius.Exposed := false;
  Fields.Add(FFdinnerRadius);
  { X3D specification comment: [0,Inf) }

  FFdouterRadius := TSFFloat.Create('outerRadius', 1);
  FFdouterRadius.Exposed := false;
  Fields.Add(FFdouterRadius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', false);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  DefaultContainerField := 'children';
end;

class function TNodeDisk2D.ClassNodeTypeName: string;
begin
  Result := 'Disk2D';
end;

function TNodeDisk2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeDisk2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePolyline2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdlineSegments := TMFVec2f.Create('lineSegments', []);
  FFdlineSegments.Exposed := false;
  Fields.Add(FFdlineSegments);
  { X3D specification comment: (-Inf,Inf) }

  FFdlineSegments.AddAlternativeName('point', 2);

  DefaultContainerField := 'children';
end;

class function TNodePolyline2D.ClassNodeTypeName: string;
begin
  Result := 'Polyline2D';
end;

function TNodePolyline2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodePolypoint2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec2f.Create('point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodePolypoint2D.ClassNodeTypeName: string;
begin
  Result := 'Polypoint2D';
end;

function TNodePolypoint2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePolypoint2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeRectangle2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdsize := TSFVec2f.Create('size', Vector2Single(2, 2));
  FFdsize.Exposed := false;
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create('solid', false);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  DefaultContainerField := 'children';
end;

class function TNodeRectangle2D.ClassNodeTypeName: string;
begin
  Result := 'Rectangle2D';
end;

function TNodeRectangle2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeRectangle2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTriangleSet2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdvertices := TMFVec2f.Create('vertices', []);
  Fields.Add(FFdvertices);
  { X3D specification comment: (-Inf,Inf) }

  FFdsolid := TSFBool.Create('solid', false);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  DefaultContainerField := 'children';
end;

class function TNodeTriangleSet2D.ClassNodeTypeName: string;
begin
  Result := 'TriangleSet2D';
end;

function TNodeTriangleSet2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTriangleSet2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterGeometry2DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeArc2D,
    TNodeArcClose2D,
    TNodeCircle2D,
    TNodeDisk2D,
    TNodePolyline2D,
    TNodePolypoint2D,
    TNodeRectangle2D,
    TNodeTriangleSet2D
  ]);
end;

{$endif read_implementation}
