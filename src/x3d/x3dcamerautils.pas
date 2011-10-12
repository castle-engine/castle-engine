{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specifically for VRML/X3D cameras.
  @seealso(Cameras @link(Cameras) unit has our general classes and utilities
    for camera handling.) }
unit X3DCameraUtils;

interface

uses CastleUtils, VectorMath, Boxes3D, X3DNodes;

type
  { Version of VRML/X3D camera definition. }
  TX3DCameraVersion = (cvVrml1_Inventor, cvVrml2_X3d);

const
  { Standard camera settings. These values are defined by VRML specification,
    so you really shouldn't change these constants ever.

    For VRML 1.0 spec of PerspectiveCamera node determines these values.
    For VRML 97 spec part "4.4.5 Standard units and coordinate system"
    and default values for Viewpoint determines these values.

    Note that DefaultVRMLCameraPosition is indexed by TX3DCameraVersion, since
    it's different for VRML 1.0 and 2.0.

    @groupBegin }
  DefaultVRMLCameraPosition: array [TX3DCameraVersion] of TVector3Single =
    ( (0, 0, 1), (0, 0, 10) );
  DefaultVRMLCameraDirection: TVector3Single = (0, 0, -1);
  DefaultVRMLCameraUp: TVector3Single = (0, 1, 0);
  DefaultVRMLGravityUp: TVector3Single = (0, 1, 0);
  { @groupEnd }

{ Constructs string with VRML node defining camera with given
  properties. }
function MakeVRMLCameraStr(const Version: TX3DCameraVersion;
  const Xml: boolean;
  const Position, Direction, Up, GravityUp: TVector3Single): string;

{ Constructs TX3DNode defining camera with given properties. }
function MakeVRMLCameraNode(const Version: TX3DCameraVersion;
  const WWWBasePath: string;
  const Position, Direction, Up, GravityUp: TVector3Single): TX3DNode;

{ Make camera node (like MakeVRMLCameraNode) that makes the whole box
  nicely visible (like CameraViewpointForWholeScene). }
function CameraNodeForWholeScene(const Version: TX3DCameraVersion;
  const WWWBasePath: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TX3DNode;

implementation

uses SysUtils, Cameras;

function MakeVRMLCameraStr(const Version: TX3DCameraVersion;
  const Xml: boolean;
  const Position, Direction, Up, GravityUp: TVector3Single): string;
const
  Comment: array [boolean] of string = (
    '# Camera settings "encoded" in the VRML/X3D declaration below :' +nl+
    '# direction %s' +nl+
    '# up %s' +nl+
    '# gravityUp %s' + nl,

    '<!-- Camera settings "encoded" in the X3D declaration below :' +nl+
    '  direction %s' +nl+
    '  up %s' +nl+
    '  gravityUp %s -->' + nl);

  UntransformedViewpoint: array [TX3DCameraVersion, boolean] of string = (
    ('PerspectiveCamera {' +nl+
     '  position %s' +nl+
     '  orientation %s' +nl+
     '}',

     '<PerspectiveCamera' +nl+
     '  position="%s"' +nl+
     '  orientation="%s"' +nl+
     '/>'),

    ('Viewpoint {' +nl+
     '  position %s' +nl+
     '  orientation %s' +nl+
     '}',

     '<Viewpoint' +nl+
     '  position="%s"' +nl+
     '  orientation="%s"' +nl+
     '/>')
  );
  TransformedViewpoint: array [TX3DCameraVersion, boolean] of string = (
    ('Separator {' +nl+
     '  Transform {' +nl+
     '    translation %s' +nl+
     '    rotation %s %s' +nl+
     '  }' +nl+
     '  PerspectiveCamera {' +nl+
     '    position 0 0 0 # camera position is expressed by translation' +nl+
     '    orientation %s' +nl+
     '  }' +nl+
     '}',

     '<Separator>' +nl+
     '  <Transform' +nl+
     '    translation="%s"' +nl+
     '    rotation="%s %s"' +nl+
     '  />' +nl+
     '  <!-- the camera position is already expressed by the translation above -->' +nl+
     '  <PerspectiveCamera' +nl+
     '    position="0 0 0"' +nl+
     '    orientation="%s"' +nl+
     '  />' +nl+
     '</Separator>'),

    ('Transform {' +nl+
     '  translation %s' +nl+
     '  rotation %s %s' +nl+
     '  children Viewpoint {' +nl+
     '    position 0 0 0 # camera position is expressed by translation' +nl+
     '    orientation %s' +nl+
     '  }' +nl+
     '}',

     '<Transform' +nl+
     '  translation="%s"' +nl+
     '  rotation="%s %s">' +nl+
     '  <!-- the camera position is already expressed by the translation above -->' +nl+
     '  <Viewpoint' +nl+
     '    position="0 0 0"' +nl+
     '    orientation="%s"' +nl+
     '  />' +nl+
     '</Transform>')
  );

var
  RotationVectorForGravity: TVector3Single;
  AngleForGravity: Single;
begin
  Result := Format(Comment[Xml],
    [ VectorToRawStr(Direction),
      VectorToRawStr(Up),
      VectorToRawStr(GravityUp) ]);

  RotationVectorForGravity := VectorProduct(DefaultVRMLGravityUp, GravityUp);
  if ZeroVector(RotationVectorForGravity) then
  begin
    { Then GravityUp is parallel to DefaultVRMLGravityUp, which means that it's
      just the same. So we can use untranslated Viewpoint node. }
    Result := Result +
      Format(
        UntransformedViewpoint[Version, Xml],
        [ VectorToRawStr(Position),
          VectorToRawStr( CamDirUp2Orient(Direction, Up) ) ]);
  end else
  begin
    { Then we must transform Viewpoint node, in such way that
      DefaultVRMLGravityUp affected by this transformation will give
      desired GravityUp. }
    AngleForGravity := AngleRadBetweenVectors(DefaultVRMLGravityUp, GravityUp);
    Result := Result +
      Format(
        TransformedViewpoint[Version, Xml],
        [ VectorToRawStr(Position),
          VectorToRawStr(RotationVectorForGravity),
          FloatToRawStr(AngleForGravity),
          { I want
            1. standard VRML dir/up vectors
            2. rotated by orientation
            3. rotated around RotationVectorForGravity
            will give MatrixWalker.Direction/Up.
            CamDirUp2Orient will calculate the orientation needed to
            achieve given up/dir vectors. So I have to pass there
            MatrixWalker.Direction/Up *already rotated negatively
            around RotationVectorForGravity*. }
          VectorToRawStr( CamDirUp2Orient(
            RotatePointAroundAxisRad(-AngleForGravity, Direction, RotationVectorForGravity),
            RotatePointAroundAxisRad(-AngleForGravity, Up       , RotationVectorForGravity)
            )) ]);
  end;
end;

function MakeVRMLCameraNode(const Version: TX3DCameraVersion;
  const WWWBasePath: string;
  const Position, Direction, Up, GravityUp: TVector3Single): TX3DNode;
var
  RotationVectorForGravity: TVector3Single;
  AngleForGravity: Single;
  ViewpointNode: TAbstractViewpointNode;
  Separator: TSeparatorNode_1;
  Transform_1: TTransformNode_1;
  Transform_2: TTransformNode;
  Rotation, Orientation: TVector4Single;
begin
  RotationVectorForGravity := VectorProduct(DefaultVRMLGravityUp, GravityUp);
  if ZeroVector(RotationVectorForGravity) then
  begin
    { Then GravityUp is parallel to DefaultVRMLGravityUp, which means that it's
      just the same. So we can use untranslated Viewpoint node. }
    case Version of
      cvVrml1_Inventor: ViewpointNode := TPerspectiveCameraNode_1.Create('', WWWBasePath);
      cvVrml2_X3d     : ViewpointNode := TViewpointNode.Create('', WWWBasePath);
      else raise EInternalError.Create('MakeVRMLCameraNode Version incorrect');
    end;
    ViewpointNode.Position.Value := Position;
    ViewpointNode.FdOrientation.Value := CamDirUp2Orient(Direction, Up);
    Result := ViewpointNode;
  end else
  begin
    { Then we must transform Viewpoint node, in such way that
      DefaultVRMLGravityUp affected by this transformation will give
      desired GravityUp. }
    AngleForGravity := AngleRadBetweenVectors(DefaultVRMLGravityUp, GravityUp);
    Rotation := Vector4Single(RotationVectorForGravity, AngleForGravity);
    { I want
      1. standard VRML dir/up vectors
      2. rotated by orientation
      3. rotated around RotationVectorForGravity
      will give MatrixWalker.Direction/Up.
      CamDirUp2Orient will calculate the orientation needed to
      achieve given up/dir vectors. So I have to pass there
      MatrixWalker.Direction/Up *already rotated negatively
      around RotationVectorForGravity*. }
    Orientation := CamDirUp2Orient(
      RotatePointAroundAxisRad(-AngleForGravity, Direction, RotationVectorForGravity),
      RotatePointAroundAxisRad(-AngleForGravity, Up       , RotationVectorForGravity));
    case Version of
      cvVrml1_Inventor:
        begin
          Transform_1 := TTransformNode_1.Create('', WWWBasePath);
          Transform_1.FdTranslation.Value := Position;
          Transform_1.FdRotation.Value := Rotation;

          ViewpointNode := TPerspectiveCameraNode_1.Create('', WWWBasePath);
          ViewpointNode.Position.Value := ZeroVector3Single;
          ViewpointNode.FdOrientation.Value := Orientation;

          Separator := TSeparatorNode_1.Create('', WWWBasePath);
          Separator.VRML1ChildAdd(Transform_1);
          Separator.VRML1ChildAdd(ViewpointNode);

          Result := Separator;
        end;

      cvVrml2_X3d:
        begin
          Transform_2 := TTransformNode.Create('', WWWBasePath);
          Transform_2.FdTranslation.Value := Position;
          Transform_2.FdRotation.Value := Rotation;

          ViewpointNode := TViewpointNode.Create('', WWWBasePath);
          ViewpointNode.Position.Value := ZeroVector3Single;
          ViewpointNode.FdOrientation.Value := Orientation;

          Transform_2.FdChildren.Add(ViewpointNode);

          Result := Transform_2;
        end;
      else raise EInternalError.Create('MakeVRMLCameraNode Version incorrect');
    end;
  end;
end;

function CameraNodeForWholeScene(const Version: TX3DCameraVersion;
  const WWWBasePath: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TX3DNode;
var
  Position, Direction, Up, GravityUp: TVector3Single;
begin
  CameraViewpointForWholeScene(Box, WantedDirection, WantedUp,
    WantedDirectionPositive, WantedUpPositive, Position, Direction, Up, GravityUp);
  Result := MakeVRMLCameraNode(Version, WWWBasePath,
    Position, Direction, Up, GravityUp);
end;

end.