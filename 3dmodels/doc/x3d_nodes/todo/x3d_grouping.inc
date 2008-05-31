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
  INodeX3DBoundedObject = interface(IVRMLNode)
    property FdbboxCenter: TSFVec3f { read GetFdbboxCenter }; { }
    property FdbboxSize: TSFVec3f { read GetFdbboxSize }; { }
  end;

  TNodeX3DGroupingNode = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeGroup = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeStaticGroup = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeSwitch = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdwhichChoice: TSFInt32;
    public property FdwhichChoice: TSFInt32 read FFdwhichChoice;
  end;

  TNodeTransform = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

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
constructor TNodeX3DGroupingNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [X3DChildNode]);
  Fields.Add(FFdchildren);

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

constructor TNodeGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeGroup.ClassNodeTypeName: string;
begin
  Result := 'Group';
end;

function TNodeGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeStaticGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdchildren := TMFNode.Create(Self, 'children', [X3DChildNode]);
  FFdchildren.Exposed := false;
  Fields.Add(FFdchildren);

  FFdbboxCenter := TSFVec3f.Create('bboxCenter', Vector3Single(0, 0, (-Inf,Inf)));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create('bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeStaticGroup.ClassNodeTypeName: string;
begin
  Result := 'StaticGroup';
end;

function TNodeStaticGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeStaticGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSwitch.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdwhichChoice := TSFInt32.Create('whichChoice', -1);
  Fields.Add(FFdwhichChoice);
  { X3D specification comment: [-1,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeSwitch.ClassNodeTypeName: string;
begin
  Result := 'Switch';
end;

function TNodeSwitch.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSwitch.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTransform.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', Vector3Single(0, 0, 0));
  Fields.Add(FFdcenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdrotation := TSFRotation.Create('rotation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdrotation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdscale := TSFVec3f.Create('scale', Vector3Single(1, 1, 1));
  Fields.Add(FFdscale);
  { X3D specification comment: (-Inf, Inf) }

  FFdscaleOrientation := TSFRotation.Create('scaleOrientation', Vector3Single(0, 0, 1), 0);
  Fields.Add(FFdscaleOrientation);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FFdtranslation := TSFVec3f.Create('translation', Vector3Single(0, 0, 0));
  Fields.Add(FFdtranslation);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeTransform.ClassNodeTypeName: string;
begin
  Result := 'Transform';
end;

function TNodeTransform.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTransform.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterGroupingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeGroup,
    TNodeStaticGroup,
    TNodeSwitch,
    TNodeTransform
  ]);
end;

{$endif read_implementation}
