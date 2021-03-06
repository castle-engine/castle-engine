{ -*- buffer-read-only: t -*-

  Copyright 2015-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal .

  The documentation for properties should go to x3dnodes_documentation.txt . }

{$ifdef read_interface}

  strict private
    function GetCenterOfRotation: TVector3;
    procedure SetCenterOfRotation(const Value: TVector3);
    function GetDescription: String;
    procedure SetDescription(const Value: String);
    function GetJump: Boolean;
    procedure SetJump(const Value: Boolean);
    function GetOrientation: TVector4;
    procedure SetOrientation(const Value: TVector4);
    function GetRetainUserOffsets: Boolean;
    procedure SetRetainUserOffsets(const Value: Boolean);
    function GetCameraMatrixSendAlsoOnOffscreenRendering: Boolean;
    procedure SetCameraMatrixSendAlsoOnOffscreenRendering(const Value: Boolean);
    function GetGravityUp: TVector3;
    procedure SetGravityUp(const Value: TVector3);
    function GetGravityTransform: Boolean;
    procedure SetGravityTransform(const Value: Boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdCenterOfRotation: TSFVec3f;
    { X3D field "centerOfRotation". } { }
    public property FdCenterOfRotation: TSFVec3f read FFdCenterOfRotation;
    { X3D field "centerOfRotation". } { }
    property CenterOfRotation: TVector3 read GetCenterOfRotation write SetCenterOfRotation;

    strict private FFdDescription: TSFString;
    { X3D field "description". } { }
    public property FdDescription: TSFString read FFdDescription;
    { X3D field "description". } { }
    property Description: String read GetDescription write SetDescription;

    strict private FFdJump: TSFBool;
    { X3D field "jump". } { }
    public property FdJump: TSFBool read FFdJump;
    { X3D field "jump". } { }
    property Jump: Boolean read GetJump write SetJump;

    strict private FFdOrientation: TSFRotation;
    { X3D field "orientation". } { }
    public property FdOrientation: TSFRotation read FFdOrientation;
    { X3D field "orientation". } { }
    property Orientation: TVector4 read GetOrientation write SetOrientation;

    strict private FFdRetainUserOffsets: TSFBool;
    { X3D field "retainUserOffsets". } { }
    public property FdRetainUserOffsets: TSFBool read FFdRetainUserOffsets;
    { X3D field "retainUserOffsets". } { }
    property RetainUserOffsets: Boolean read GetRetainUserOffsets write SetRetainUserOffsets;

    strict private FEventCameraMatrix: TSFMatrix4fEvent;
    { X3D out event "cameraMatrix". } { }
    public property EventCameraMatrix: TSFMatrix4fEvent read FEventCameraMatrix;

    strict private FEventCameraInverseMatrix: TSFMatrix4fEvent;
    { X3D out event "cameraInverseMatrix". } { }
    public property EventCameraInverseMatrix: TSFMatrix4fEvent read FEventCameraInverseMatrix;

    strict private FEventCameraRotationMatrix: TSFMatrix3fEvent;
    { X3D out event "cameraRotationMatrix". } { }
    public property EventCameraRotationMatrix: TSFMatrix3fEvent read FEventCameraRotationMatrix;

    strict private FEventCameraRotationInverseMatrix: TSFMatrix3fEvent;
    { X3D out event "cameraRotationInverseMatrix". } { }
    public property EventCameraRotationInverseMatrix: TSFMatrix3fEvent read FEventCameraRotationInverseMatrix;

    strict private FFdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool;
    { X3D field "cameraMatrixSendAlsoOnOffscreenRendering". } { }
    public property FdCameraMatrixSendAlsoOnOffscreenRendering: TSFBool read FFdCameraMatrixSendAlsoOnOffscreenRendering;
    { X3D field "cameraMatrixSendAlsoOnOffscreenRendering". } { }
    property CameraMatrixSendAlsoOnOffscreenRendering: Boolean read GetCameraMatrixSendAlsoOnOffscreenRendering write SetCameraMatrixSendAlsoOnOffscreenRendering;

    strict private FFdDirection: TMFVec3f;
    { X3D field "direction". } { }
    public property FdDirection: TMFVec3f read FFdDirection;

    strict private FFdUp: TMFVec3f;
    { X3D field "up". } { }
    public property FdUp: TMFVec3f read FFdUp;

    strict private FFdGravityUp: TSFVec3f;
    { X3D field "gravityUp". } { }
    public property FdGravityUp: TSFVec3f read FFdGravityUp;
    { X3D field "gravityUp". } { }
    property GravityUp: TVector3 read GetGravityUp write SetGravityUp;

    strict private FFdGravityTransform: TSFBool;
    { X3D field "gravityTransform". } { }
    public property FdGravityTransform: TSFBool read FFdGravityTransform;
    { X3D field "gravityTransform". } { }
    property GravityTransform: Boolean read GetGravityTransform write SetGravityTransform;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractViewpointNode ----------------------------------------------- }

function TAbstractViewpointNode.GetCenterOfRotation: TVector3;
begin
  Result := FdCenterOfRotation.Value;
end;

procedure TAbstractViewpointNode.SetCenterOfRotation(const Value: TVector3);
begin
  FdCenterOfRotation.Send(Value);
end;

function TAbstractViewpointNode.GetDescription: String;
begin
  Result := FdDescription.Value;
end;

procedure TAbstractViewpointNode.SetDescription(const Value: String);
begin
  FdDescription.Send(Value);
end;

function TAbstractViewpointNode.GetJump: Boolean;
begin
  Result := FdJump.Value;
end;

procedure TAbstractViewpointNode.SetJump(const Value: Boolean);
begin
  FdJump.Send(Value);
end;

function TAbstractViewpointNode.GetOrientation: TVector4;
begin
  Result := FdOrientation.Value;
end;

procedure TAbstractViewpointNode.SetOrientation(const Value: TVector4);
begin
  FdOrientation.Send(Value);
end;

function TAbstractViewpointNode.GetRetainUserOffsets: Boolean;
begin
  Result := FdRetainUserOffsets.Value;
end;

procedure TAbstractViewpointNode.SetRetainUserOffsets(const Value: Boolean);
begin
  FdRetainUserOffsets.Send(Value);
end;

function TAbstractViewpointNode.GetCameraMatrixSendAlsoOnOffscreenRendering: Boolean;
begin
  Result := FdCameraMatrixSendAlsoOnOffscreenRendering.Value;
end;

procedure TAbstractViewpointNode.SetCameraMatrixSendAlsoOnOffscreenRendering(const Value: Boolean);
begin
  FdCameraMatrixSendAlsoOnOffscreenRendering.Send(Value);
end;

function TAbstractViewpointNode.GetGravityUp: TVector3;
begin
  Result := FdGravityUp.Value;
end;

procedure TAbstractViewpointNode.SetGravityUp(const Value: TVector3);
begin
  FdGravityUp.Send(Value);
end;

function TAbstractViewpointNode.GetGravityTransform: Boolean;
begin
  Result := FdGravityTransform.Value;
end;

procedure TAbstractViewpointNode.SetGravityTransform(const Value: Boolean);
begin
  FdGravityTransform.Send(Value);
end;

class function TAbstractViewpointNode.ClassX3DType: String;
begin
  Result := 'X3DViewpointNode';
end;

procedure TAbstractViewpointNode.CreateNode;
begin
  inherited;

  FFdCenterOfRotation := TSFVec3f.Create(Self, True, 'centerOfRotation', Vector3(0, 0, 0));
   FdCenterOfRotation.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdCenterOfRotation);

  FFdDescription := TSFString.Create(Self, True, 'description', '');
   FdDescription.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDescription);

  FFdJump := TSFBool.Create(Self, True, 'jump', true);
   FdJump.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdJump);

  FFdOrientation := TSFRotation.Create(Self, True, 'orientation', Vector3(0, 0, 1), 0);
   FdOrientation.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdOrientation);

  FFdRetainUserOffsets := TSFBool.Create(Self, True, 'retainUserOffsets', false);
   FdRetainUserOffsets.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdRetainUserOffsets);

  FEventCameraMatrix := TSFMatrix4fEvent.Create(Self, 'cameraMatrix', false);
  AddEvent(FEventCameraMatrix);

  FEventCameraInverseMatrix := TSFMatrix4fEvent.Create(Self, 'cameraInverseMatrix', false);
  AddEvent(FEventCameraInverseMatrix);

  FEventCameraRotationMatrix := TSFMatrix3fEvent.Create(Self, 'cameraRotationMatrix', false);
  AddEvent(FEventCameraRotationMatrix);

  FEventCameraRotationInverseMatrix := TSFMatrix3fEvent.Create(Self, 'cameraRotationInverseMatrix', false);
  AddEvent(FEventCameraRotationInverseMatrix);

  FFdCameraMatrixSendAlsoOnOffscreenRendering := TSFBool.Create(Self, True, 'cameraMatrixSendAlsoOnOffscreenRendering', false);
   FdCameraMatrixSendAlsoOnOffscreenRendering.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdCameraMatrixSendAlsoOnOffscreenRendering);

  FFdDirection := TMFVec3f.Create(Self, True, 'direction', []);
   FdDirection.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDirection);

  FFdUp := TMFVec3f.Create(Self, True, 'up', []);
   FdUp.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdUp);

  FFdGravityUp := TSFVec3f.Create(Self, True, 'gravityUp', Vector3(0, 1, 0));
   FdGravityUp.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdGravityUp);

  FFdGravityTransform := TSFBool.Create(Self, True, 'gravityTransform', true);
   FdGravityTransform.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdGravityTransform);
end;

{$endif read_implementation}
