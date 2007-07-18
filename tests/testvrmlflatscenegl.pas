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

unit TestVRMLFlatSceneGL;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLFlatSceneGL = class(TTestCase)
  published
    procedure TestVRMLFlatSceneGL;
  end;

implementation

uses VRMLNodes, VRMLFlatScene, VRMLFlatSceneGL, Boxes3d, VectorMath;

procedure TTestVRMLFlatSceneGL.TestVRMLFlatSceneGL;

  procedure EmptySceneAsserts(EmptyScene: TVRMLFlatSceneGL);
  var
    CamKind: TVRMLCameraKind;
    CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  begin
   Assert(EmptyScene.VerticesCount(false) = 0);
   Assert(EmptyScene.VerticesCount(true) = 0);
   Assert(EmptyScene.TrianglesCount(false) = 0);
   Assert(EmptyScene.TrianglesCount(true) = 0);

   Assert(IsEmptyBox3d(EmptyScene.BoundingBox));

   Assert(EmptyScene.ShapeStates.Count = 0);

   Assert(EmptyScene.GetViewpoint(CamKind, CamPos, CamDir, CamUp, GravityUp) = nil);

   Assert(EmptyScene.FogNode = nil);

   Assert(EmptyScene.Background = nil);
  end;

var EmptyScene: TVRMLFlatSceneGL;
begin
 EmptyScene := TVRMLFlatSceneGL.Create(nil, true, roSceneAsAWhole);
 try
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;

 EmptyScene := TVRMLFlatSceneGL.Create(TNodeGroup_1.Create('', ''), true,
   roSceneAsAWhole);
 try
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;
end;

initialization
 RegisterTest(TTestVRMLFlatSceneGL);
end.
