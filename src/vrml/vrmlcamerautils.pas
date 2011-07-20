{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specifically for VRML cameras.
  @seealso(Cameras @link(Cameras) unit has our general classes and utilities
    for camera handling.) }
unit VRMLCameraUtils;

interface

uses KambiUtils, VectorMath, Boxes3D, VRMLNodes;

type
  { VRML major version for VRMLCameraUtils: either VRML 1.0 or 2.0.
    For Inventor you should treat it like VRML 1.0. }
  TVRMLCameraVersion = 1..2;

const
  { Standard camera settings. These values are defined by VRML specification,
    so you really shouldn't change these constants ever.

    For VRML 1.0 spec of PerspectiveCamera node determines these values.
    For VRML 97 spec part "4.4.5 Standard units and coordinate system"
    and default values for Viewpoint determines these values.

    Note that DefaultVRMLCameraPosition is indexed by TVRMLCameraVersion, since
    it's different for VRML 1.0 and 2.0.

    @groupBegin }
  DefaultVRMLCameraPosition: array [TVRMLCameraVersion] of TVector3Single =
    ( (0, 0, 1), (0, 0, 10) );
  DefaultVRMLCameraDirection: TVector3Single = (0, 0, -1);
  DefaultVRMLCameraUp: TVector3Single = (0, 1, 0);
  DefaultVRMLGravityUp: TVector3Single = (0, 1, 0);
  { @groupEnd }

{ Constructs string with VRML node defining camera with given
  properties. }
function MakeVRMLCameraStr(const Version: TVRMLCameraVersion;
  const Xml: boolean;
  const Position, Direction, Up, GravityUp: TVector3Single): string;

{ Constructs TVRMLNode defining camera with given properties. }
function MakeVRMLCameraNode(const Version: TVRMLCameraVersion;
  const WWWBasePath: string;
  const Position, Direction, Up, GravityUp: TVector3Single): TVRMLNode;

{ Make camera node (like MakeVRMLCameraNode) that makes the whole box
  nicely visible (like CameraViewpointForWholeScene). }
function CameraNodeForWholeScene(const Version: TVRMLCameraVersion;
  const WWWBasePath: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TVRMLNode;

implementation

uses SysUtils, Cameras;

function MakeVRMLCameraStr(const Version: TVRMLCameraVersion;
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

  UntransformedViewpoint: array [TVRMLCameraVersion, boolean] of string = (
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
  TransformedViewpoint: array [TVRMLCameraVersion, boolean] of string = (
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

function MakeVRMLCameraNode(const Version: TVRMLCameraVersion;
  const WWWBasePath: string;
  const Position, Direction, Up, GravityUp: TVector3Single): TVRMLNode;
var
  RotationVectorForGravity: TVector3Single;
  AngleForGravity: Single;
  ViewpointNode: TVRMLViewpointNode;
  Separator: TNodeSeparator;
  Transform_1: TNodeTransform_1;
  Transform_2: TNodeTransform;
  Rotation, Orientation: TVector4Single;
begin
  RotationVectorForGravity := VectorProduct(DefaultVRMLGravityUp, GravityUp);
  if ZeroVector(RotationVectorForGravity) then
  begin
    { Then GravityUp is parallel to DefaultVRMLGravityUp, which means that it's
      just the same. So we can use untranslated Viewpoint node. }
    case Version of
      1: ViewpointNode := TNodePerspectiveCamera.Create('', WWWBasePath);
      2: ViewpointNode := TNodeViewpoint.Create('', WWWBasePath);
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
      1: begin
           Transform_1 := TNodeTransform_1.Create('', WWWBasePath);
           Transform_1.FdTranslation.Value := Position;
           Transform_1.FdRotation.Value := Rotation;

           ViewpointNode := TNodePerspectiveCamera.Create('', WWWBasePath);
           ViewpointNode.Position.Value := ZeroVector3Single;
           ViewpointNode.FdOrientation.Value := Orientation;

           Separator := TNodeSeparator.Create('', WWWBasePath);
           Separator.VRML1ChildAdd(Transform_1);
           Separator.VRML1ChildAdd(ViewpointNode);

           Result := Separator;
         end;

      2: begin
           Transform_2 := TNodeTransform.Create('', WWWBasePath);
           Transform_2.FdTranslation.Value := Position;
           Transform_2.FdRotation.Value := Rotation;

           ViewpointNode := TNodeViewpoint.Create('', WWWBasePath);
           ViewpointNode.Position.Value := ZeroVector3Single;
           ViewpointNode.FdOrientation.Value := Orientation;

           Transform_2.FdChildren.Add(ViewpointNode);

           Result := Transform_2;
         end;
      else raise EInternalError.Create('MakeVRMLCameraNode Version incorrect');
    end;
  end;
end;

function CameraNodeForWholeScene(const Version: TVRMLCameraVersion;
  const WWWBasePath: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TVRMLNode;
var
  Position, Direction, Up, GravityUp: TVector3Single;
begin
  CameraViewpointForWholeScene(Box, WantedDirection, WantedUp,
    WantedDirectionPositive, WantedUpPositive, Position, Direction, Up, GravityUp);
  Result := MakeVRMLCameraNode(Version, WWWBasePath,
    Position, Direction, Up, GravityUp);
end;

end.