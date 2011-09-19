{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ }
unit TestVRMLGLScene;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLGLScene = class(TTestCase)
  published
    procedure TestVRMLGLScene;
  end;

implementation

uses VRMLNodes, VRMLScene, VRMLGLScene, Boxes3D, VectorMath;

procedure TTestVRMLGLScene.TestVRMLGLScene;

  procedure EmptySceneAsserts(EmptyScene: TVRMLGLScene);
  var
    CamProjection: TProjectionType;
    CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  begin
   Assert(EmptyScene.VerticesCount(false) = 0);
   Assert(EmptyScene.VerticesCount(true) = 0);
   Assert(EmptyScene.TrianglesCount(false) = 0);
   Assert(EmptyScene.TrianglesCount(true) = 0);

   Assert(EmptyScene.BoundingBox.IsEmpty);

   Assert(EmptyScene.ShapesActiveCount = 0);

   Assert(EmptyScene.GetViewpoint(CamProjection, CamPos, CamDir, CamUp, GravityUp) = nil);

   Assert(EmptyScene.FogNode = nil);

   Assert(EmptyScene.Background = nil);
  end;

var EmptyScene: TVRMLGLScene;
begin
 EmptyScene := TVRMLGLScene.Create(nil);
 try
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;

 EmptyScene := TVRMLGLScene.Create(nil);
 try
  EmptyScene.Load(TX3DRootNode.Create('', ''), true);
  EmptySceneAsserts(EmptyScene);
  EmptyScene.ChangedAll;
  EmptySceneAsserts(EmptyScene);
 finally FreeAndNil(EmptyScene) end;
end;

initialization
 RegisterTest(TTestVRMLGLScene);
end.
