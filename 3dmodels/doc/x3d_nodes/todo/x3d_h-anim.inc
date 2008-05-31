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
  TNodeHAnimDisplacer = class(TNodeX3DGeometricPropertyNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcoordIndex: TMFInt32;
    public property FdcoordIndex: TMFInt32 read FFdcoordIndex;

    private FFddisplacements: TMFVec3f;
    public property Fddisplacements: TMFVec3f read FFddisplacements;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdweight: TSFFloat;
    public property Fdweight: TSFFloat read FFdweight;
  end;

  TNodeHAnimHumanoid = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdinfo: TMFString;
    public property Fdinfo: TMFString read FFdinfo;

    private FFdjoints: TMFNode;
    public property Fdjoints: TMFNode read FFdjoints;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdsegments: TMFNode;
    public property Fdsegments: TMFNode read FFdsegments;

    private FFdsites: TMFNode;
    public property Fdsites: TMFNode read FFdsites;

    private FFdskeleton: TMFNode;
    public property Fdskeleton: TMFNode read FFdskeleton;

    private FFdskin: TMFNode;
    public property Fdskin: TMFNode read FFdskin;

    private FFdskinCoord: TSFNode;
    public property FdskinCoord: TSFNode read FFdskinCoord;

    private FFdskinNormal: TSFNode;
    public property FdskinNormal: TSFNode read FFdskinNormal;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdversion: TSFString;
    public property Fdversion: TSFString read FFdversion;

    private FFdviewpoints: TMFNode;
    public property Fdviewpoints: TMFNode read FFdviewpoints;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeHAnimJoint = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFddisplacers: TMFNode;
    public property Fddisplacers: TMFNode read FFddisplacers;

    private FFdlimitOrientation: TSFRotation;
    public property FdlimitOrientation: TSFRotation read FFdlimitOrientation;

    private FFdllimit: TMFFloat;
    public property Fdllimit: TMFFloat read FFdllimit;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;

    private FFdrotation: TSFRotation;
    public property Fdrotation: TSFRotation read FFdrotation;

    private FFdscale: TSFVec3f;
    public property Fdscale: TSFVec3f read FFdscale;

    private FFdscaleOrientation: TSFRotation;
    public property FdscaleOrientation: TSFRotation read FFdscaleOrientation;

    private FFdskinCoordIndex: TMFInt32;
    public property FdskinCoordIndex: TMFInt32 read FFdskinCoordIndex;

    private FFdskinCoordWeight: TMFFloat;
    public property FdskinCoordWeight: TMFFloat read FFdskinCoordWeight;

    private FFdstiffness: TMFFloat;
    public property Fdstiffness: TMFFloat read FFdstiffness;

    private FFdtranslation: TSFVec3f;
    public property Fdtranslation: TSFVec3f read FFdtranslation;

    private FFdulimit: TMFFloat;
    public property Fdulimit: TMFFloat read FFdulimit;
  end;

  TNodeHAnimSegment = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenterOfMass: TSFVec3f;
    public property FdcenterOfMass: TSFVec3f read FFdcenterOfMass;

    private FFdcoord: TSFNode;
    public property Fdcoord: TSFNode read FFdcoord;

    private FFddisplacers: TMFNode;
    public property Fddisplacers: TMFNode read FFddisplacers;

    private FFdmass: TSFFloat;
    public property Fdmass: TSFFloat read FFdmass;

    private FFdmomentsOfInertia: TMFFloat;
    public property FdmomentsOfInertia: TMFFloat read FFdmomentsOfInertia;

    private FFdname: TSFString;
    public property Fdname: TSFString read FFdname;
  end;

  TNodeHAnimSite = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeHAnimDisplacer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcoordIndex := TMFInt32.Create('coordIndex', []);
  Fields.Add(FFdcoordIndex);
  { X3D specification comment: [0,Inf) or -1 }

  FFddisplacements := TMFVec3f.Create('displacements', []);
  Fields.Add(FFddisplacements);

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdweight := TSFFloat.Create('weight', 0.0);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimDisplacer.ClassNodeTypeName: string;
begin
  Result := 'HAnimDisplacer';
end;

function TNodeHAnimDisplacer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimDisplacer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeHAnimHumanoid.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdinfo := TMFString.Create('info', []);
  Fields.Add(FFdinfo);

  FFdjoints := TMFNode.Create(Self, 'joints', [HAnimJoint]);
  Fields.Add(FFdjoints);

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create('scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdsegments := TMFNode.Create(Self, 'segments', [HAnimSegment]);
  Fields.Add(FFdsegments);

  FFdsites := TMFNode.Create(Self, 'sites', [HAnimSite]);
  Fields.Add(FFdsites);

  FFdskeleton := TMFNode.Create(Self, 'skeleton', [HAnimJoint, HAnimSite]);
  Fields.Add(FFdskeleton);

  FFdskin := TMFNode.Create(Self, 'skin', [X3DChildNode]);
  Fields.Add(FFdskin);

  FFdskinCoord := TSFNode.Create(Self, 'skinCoord', [X3DCoordinateNode]);
  Fields.Add(FFdskinCoord);

  FFdskinNormal := TSFNode.Create(Self, 'skinNormal', [X3DNormalNode]);
  Fields.Add(FFdskinNormal);

  FFdtranslation := TSFVec3f.Create('translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdversion := TSFString.Create('version', '');
  Fields.Add(FFdversion);

  FFdviewpoints := TMFNode.Create(Self, 'viewpoints', [HAnimSite]);
  Fields.Add(FFdviewpoints);

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

class function TNodeHAnimHumanoid.ClassNodeTypeName: string;
begin
  Result := 'HAnimHumanoid';
end;

function TNodeHAnimHumanoid.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimHumanoid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeHAnimJoint.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFddisplacers := TMFNode.Create(Self, 'displacers', [HAnimDisplacer]);
  Fields.Add(FFddisplacers);

  FFdlimitOrientation := TSFRotation.Create('limitOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdlimitOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdllimit := TMFFloat.Create('llimit', []);
  Fields.Add(FFdllimit);
  { X3D specification comment: (-Inf,Inf) }

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create('scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdskinCoordIndex := TMFInt32.Create('skinCoordIndex', []);
  Fields.Add(FFdskinCoordIndex);

  FFdskinCoordWeight := TMFFloat.Create('skinCoordWeight', []);
  Fields.Add(FFdskinCoordWeight);

  FFdstiffness := TMFFloat.Create('stiffness', [0 0 0]);
  Fields.Add(FFdstiffness);
  { X3D specification comment: [0,1] }

  FFdtranslation := TSFVec3f.Create('translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  FFdulimit := TMFFloat.Create('ulimit', []);
  Fields.Add(FFdulimit);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimJoint.ClassNodeTypeName: string;
begin
  Result := 'HAnimJoint';
end;

function TNodeHAnimJoint.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeHAnimSegment.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenterOfMass := TSFVec3f.Create('centerOfMass', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenterOfMass);
  { X3D specification comment: (-Inf,Inf) }

  FFdcoord := TSFNode.Create(Self, 'coord', [X3DCoordinateNode]);
  Fields.Add(FFdcoord);

  FFddisplacers := TMFNode.Create(Self, 'displacers', [HAnimDisplacer]);
  Fields.Add(FFddisplacers);

  FFdmass := TSFFloat.Create('mass', 0);
  Fields.Add(FFdmass);
  { X3D specification comment: [0,Inf) }

  FFdmomentsOfInertia := TMFFloat.Create('momentsOfInertia', [0 0 0 0 0 0 0 0 0]);
  Fields.Add(FFdmomentsOfInertia);
  { X3D specification comment: [0,Inf) }

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSegment.ClassNodeTypeName: string;
begin
  Result := 'HAnimSegment';
end;

function TNodeHAnimSegment.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimSegment.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeHAnimSite.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdname := TSFString.Create('name', '');
  Fields.Add(FFdname);

  FFdrotation := TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdscale := TSFVec3f.Create('scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (0,Inf) }

  FFdscaleOrientation := TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  FFdtranslation := TSFVec3f.Create('translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf)|[-1,1] }

  DefaultContainerField := 'children';
end;

class function TNodeHAnimSite.ClassNodeTypeName: string;
begin
  Result := 'HAnimSite';
end;

function TNodeHAnimSite.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeHAnimSite.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterH-AnimNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeHAnimDisplacer,
    TNodeHAnimHumanoid,
    TNodeHAnimJoint,
    TNodeHAnimSegment,
    TNodeHAnimSite
  ]);
end;

{$endif read_implementation}
