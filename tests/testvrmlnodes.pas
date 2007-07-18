{
  Copyright 2004-2005 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestVRMLNodes;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLNodes = class(TTestCase)
    procedure TestNodesManager;
  end;

implementation

uses VRMLNodes;

{ TNode* ------------------------------------------------------------ }

type
  TNodeSpecial = class(TVRMLNode)
    function NodeTypeName: string; override;
  end;

function TNodeSpecial.NodeTypeName: string;
begin
 result := 'OohImSoSpecial';
end;

type
  TNodeSomething = class(TVRMLNode)
    class function ClassNodeTypeName: string; override;
  end;

class function TNodeSomething.ClassNodeTypeName: string;
begin
 result := 'WellImNothingSpecial';
end;

{ ------------------------------------------------------------ }

procedure TTestVRMLNodes.TestNodesManager;
begin
 try
  { throw exception because TNodeSpecial.ClassNodeTypeName = '' }
  NodesManager.RegisterNodeClass(TNodeSpecial);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeSpecial); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { throw exception because TNodeFog is already registered }
  NodesManager.RegisterNodeClass(TNodeFog);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeFog); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { this should succeed }
  NodesManager.RegisterNodeClass(TNodeSomething);
 finally 
  NodesManager.UnRegisterNodeClass(TNodeSomething);
 end;
end;

initialization
 RegisterTest(TTestVRMLNodes);
end.
