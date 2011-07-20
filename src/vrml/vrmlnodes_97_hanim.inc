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

{ VRML 97 nodes for HAnim 1.0 and 200x specifications.

  For X3D there's different file, x3d_h-anim, as for X3D nodes are
  prefixed with "HAnim" before their name.

  VRML 97 nodes actually inherit from X3D nodes, so you should write
  your code to use X3D nodes (like TNodeHAnimDisplacer) and then VRML 97
  versions will also be handled automatically. }
{ }

{$ifdef read_interface}
  TNodeDisplacer = class(TNodeHAnimDisplacer)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeHumanoid = class(TNodeHAnimHumanoid)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeJoint = class(TNodeHAnimJoint)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSegment = class(TNodeHAnimSegment)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeSite = class(TNodeHAnimSite)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeDisplacer.ClassNodeTypeName: string;
begin
  Result := 'Displacer';
end;

class function TNodeDisplacer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeHumanoid.ClassNodeTypeName: string;
begin
  Result := 'Humanoid';
end;

class function TNodeHumanoid.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeJoint.ClassNodeTypeName: string;
begin
  Result := 'Joint';
end;

class function TNodeJoint.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeSegment.ClassNodeTypeName: string;
begin
  Result := 'Segment';
end;

class function TNodeSegment.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeSite.ClassNodeTypeName: string;
begin
  Result := 'Site';
end;

class function TNodeSite.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure RegisterVRML97HAnimNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeDisplacer,
    TNodeHumanoid,
    TNodeJoint,
    TNodeSegment,
    TNodeSite
  ]);
end;
{$endif read_implementation}