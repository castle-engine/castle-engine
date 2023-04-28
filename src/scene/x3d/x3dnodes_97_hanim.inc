{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML 97 nodes for HAnim 1.0 and 200x specifications.

  For X3D there's different file, x3d_h-anim, as for X3D nodes are
  prefixed with "HAnim" before their name.

  VRML 97 nodes actually inherit from X3D nodes, so you should write
  your code to use X3D nodes (like THAnimDisplacerNode) and then VRML 97
  versions will also be handled automatically. }
{ }

{$ifdef read_interface}
  TDisplacerNode = class(THAnimDisplacerNode)
  public
    class function ClassX3DType: string; override;
  end;

  THumanoidNode = class(THAnimHumanoidNode)
  public
    class function ClassX3DType: string; override;
  end;

  TJointNode = class(THAnimJointNode)
  public
    class function ClassX3DType: string; override;
  end;

  TSegmentNode = class(THAnimSegmentNode)
  public
    class function ClassX3DType: string; override;
  end;

  TSiteNode = class(THAnimSiteNode)
  public
    class function ClassX3DType: string; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TDisplacerNode.ClassX3DType: string;
begin
  Result := 'Displacer';
end;

class function THumanoidNode.ClassX3DType: string;
begin
  Result := 'Humanoid';
end;

class function TJointNode.ClassX3DType: string;
begin
  Result := 'Joint';
end;

class function TSegmentNode.ClassX3DType: string;
begin
  Result := 'Segment';
end;

class function TSiteNode.ClassX3DType: string;
begin
  Result := 'Site';
end;

procedure RegisterVRML97HAnimNodes;
begin
  NodesManager.RegisterNodeClasses([
    TDisplacerNode,
    THumanoidNode,
    TJointNode,
    TSegmentNode,
    TSiteNode
  ]);
end;
{$endif read_implementation}
