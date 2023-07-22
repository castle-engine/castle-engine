{
  Copyright 2003-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specifically for X3D cameras.
  @seealso(CastleCameras For our general classes and utilities
    for camera handling.) }
unit X3DCameraUtils;

{$I castleconf.inc}

interface

uses CastleUtils, CastleVectors, CastleBoxes, X3DNodes, CastleProjection,
  CastleRectangles;

type
  { Version of VRML/X3D camera definition. }
  TX3DCameraVersion = (cvVrml1_Inventor, cvVrml2_X3d);

const
  { Standard camera settings given by VRML/X3D specifications.
    @groupBegin }
  DefaultX3DCameraPosition: array [TX3DCameraVersion] of TVector3 =
    ( (X: 0; Y: 0; Z: 1),
      (X: 0; Y: 0; Z: 10)
    );
  DefaultX3DCameraDirection: TVector3 = (X: 0; Y: 0; Z: -1);
  DefaultX3DCameraUp       : TVector3 = (X: 0; Y: 1; Z: 0);
  DefaultX3DGravityUp      : TVector3 = (X: 0; Y: 1; Z: 0);
  { @groupEnd }

  { Default camera position, direction, view in X3D. }
  DefaultX3DCameraView: TViewVectors = (
    Translation: (X: 0; Y: 0; Z: 10);
    Direction:   (X: 0; Y: 0; Z: -1);
    Up:          (X: 0; Y: 1; Z: 0)
  );

type
  { Utility class to set various viewpoint properties,
    and then generate given viewpoint node. }
  TMakeX3DViewpoint = class
  strict private
    FVersion: TX3DCameraVersion;
    FXml: Boolean;
    FPosition, FDirection, FUp, FGravityUp, FCenterOfRotation: TVector3;
    FPerspectiveFieldOfView: Single;
    FProjectionType: TProjectionType;
    FOrthographicFieldOfView: TFloatRectangle;
    FAutoCenterOfRotation: Boolean;
  public
    const
      { Matches X3D default TViewpointNode.FieldOfView.

        It also matches TCastlePerspective.DefaultFieldOfView,
        which is good -- our TCastleCamera and friends (like TCastlePerspective)
        is consistent with X3D in this regard. }
      DefaultPerspectiveFieldOfView = Pi / 4;

    { Version of VRML or X3D to generate (in ToNode or ToString). }
    property Version: TX3DCameraVersion read FVersion write FVersion default cvVrml2_X3d;
    { Whether to use XML encoding in ToString. }
    property Xml: Boolean read FXml write FXml default true;

    { Position of the camera.
      By default it is @link(DefaultX3DCameraPosition DefaultX3DCameraPosition[cvVrml2_X3d]). }
    property Position: TVector3 read FPosition write FPosition;
    { Direction of the camera.
      By default it is @link(DefaultX3DCameraDirection). }
    property Direction: TVector3 read FDirection write FDirection;
    { Up vector of the camera.
      By default it is @link(DefaultX3DCameraUp). }
    property Up: TVector3 read FUp write FUp;
    { Gravity up vector of the camera.
      By default it is @link(DefaultX3DGravityUp).}
    property GravityUp: TVector3 read FGravityUp write FGravityUp;

    { Center of rotation, as should be specified in viewpoint node.
      By default it is zero, just like @link(TAbstractViewpointNode.CenterOfRotation). }
    property CenterOfRotation: TVector3 read FCenterOfRotation write FCenterOfRotation;

    { Automatically calculate center of rotation based on scene bounding box.
      The default (@false) makes X3D default of @link(TAbstractViewpointNode.AutoCenterOfRotation). }
    property AutoCenterOfRotation: Boolean
      read FAutoCenterOfRotation write FAutoCenterOfRotation default false;

    { Perspective field of view, in radians. }
    property PerspectiveFieldOfView: Single read FPerspectiveFieldOfView write FPerspectiveFieldOfView
      {$ifdef FPC}default DefaultPerspectiveFieldOfView{$endif};

    { TODO: This does not support yet choice perspective/orthographic camera,
      we always generate perspective node.
      But we expose relevant properties and they will be used in the future. }
    property ProjectionType: TProjectionType
      read FProjectionType write FProjectionType default ptPerspective;
    property OrthographicFieldOfView: TFloatRectangle
      read FOrthographicFieldOfView write FOrthographicFieldOfView;

    { }
    constructor Create;

    { Construct string with VRML/X3D node defining Viewpoint with given
      properties. }
    function ToString: String; override;

    { Construct X3D nodes that define viewpoint (maybe with some transformation)
      with given properties.

      Overloaded version with ViewpointNode parameter returns
      the TAbstractViewpointNode descendant that is for sure somewhere within
      the returned node. But the returned node may be a transformation node. }
    function ToNode(out ViewpointNode: TAbstractViewpointNode): TAbstractChildNode; overload;
    function ToNode: TAbstractChildNode; overload;
  end;

function MakeCameraStr(const Version: TX3DCameraVersion;
  const Xml: boolean;
  const Position, Direction, Up, GravityUp: TVector3): string;
  deprecated 'use TMakeX3DViewpoint with its ToString method';
function MakeCameraNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Position, Direction, Up, GravityUp: TVector3): TAbstractChildNode; overload;
  deprecated 'use TMakeX3DViewpoint with its ToNode method';
function MakeCameraNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Position, Direction, Up, GravityUp: TVector3;
  out ViewpointNode: TAbstractViewpointNode): TAbstractChildNode; overload;
  deprecated 'use TMakeX3DViewpoint with its ToNode method';

{ Make camera node (like MakeCameraNode) that makes the whole box
  nicely visible (like CameraViewpointForWholeScene). }
function CameraNodeForWholeScene(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TAbstractChildNode;

function MakeCameraNavNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const NavigationType: string;
  const WalkSpeed, VisibilityLimit: Single; const AvatarSize: TVector3;
  const Headlight: boolean): TNavigationInfoNode;

implementation

uses SysUtils, Math,
  CastleTransform;

{ TMakeX3DViewpoint ---------------------------------------------------------- }

constructor TMakeX3DViewpoint.Create;
begin
  inherited;
  FVersion := cvVrml2_X3d;
  FXml := true;

  FPosition := DefaultX3DCameraPosition[cvVrml2_X3d];
  FDirection := DefaultX3DCameraDirection;
  FUp := DefaultX3DCameraUp;
  FGravityUp := DefaultX3DGravityUp;

  FProjectionType := ptPerspective;
  FPerspectiveFieldOfView := DefaultPerspectiveFieldOfView;
  FOrthographicFieldOfView := TFloatRectangle.Empty;
end;

function TMakeX3DViewpoint.ToString: String;
const
  Comment: array [boolean] of string = (
    '# Generated by %s.' +nl+
    '# Use view3dscene "Clipboard -> Print Current Camera..." to generate X3D code like below.' +nl+
    '# Camera settings "encoded" in the X3D declaration below :' +nl+
    '#   position %s' +nl+
    '#   direction %s' +nl+
    '#   up %s' +nl+
    '#   gravityUp %s' +nl,

    '<!-- Generated by %s.' +nl+
    '  Use view3dscene "Clipboard -> Print Current Camera..." to generate X3D code like below.' +nl+
    '  Camera settings "encoded" in the X3D declaration below :' +nl+
    '    position %s' +nl+
    '    direction %s' +nl+
    '    up %s' +nl+
    '    gravityUp %s -->' +nl);

  UntransformedViewpoint: array [TX3DCameraVersion, boolean] of string = (
    ('PerspectiveCamera {' +nl+
     '  position %s' +nl+
     '  orientation %s' +nl+
     '%s' + // ExtraFields
     '}',

     '<PerspectiveCamera' +nl+
     '  position="%s"' +nl+
     '  orientation="%s"' +nl+
     '%s' + // ExtraFields
     '/>'),

    ('Viewpoint {' +nl+
     '  position %s' +nl+
     '  orientation %s' +nl+
     '%s' + // ExtraFields
     '}',

     '<Viewpoint' +nl+
     '  position="%s"' +nl+
     '  orientation="%s"' +nl+
     '%s' + // ExtraFields
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
     '%s' + // ExtraFields
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
     '%s' + // ExtraFields
     '  />' +nl+
     '</Separator>'),

    ('Transform {' +nl+
     '  translation %s' +nl+
     '  rotation %s %s' +nl+
     '  children Viewpoint {' +nl+
     '    position 0 0 0 # camera position is expressed by translation' +nl+
     '    orientation %s' +nl+
     '%s' + // ExtraFields
     '  }' +nl+
     '}',

     '<Transform' +nl+
     '  translation="%s"' +nl+
     '  rotation="%s %s">' +nl+
     '  <!-- the camera position is already expressed by the translation above -->' +nl+
     '  <Viewpoint' +nl+
     '    position="0 0 0"' +nl+
     '    orientation="%s"' +nl+
     '%s' + // ExtraFields
     '  />' +nl+
     '</Transform>')
  );

var
  RotationVectorForGravity: TVector3;
  AngleForGravity: Single;
  { Workarounding FPC 3.1.1 internal error 200211262 in x3dcamerautils.pas }
  S1, S2, S3, S4: string;
  ExtraFields: String;
begin
  Result := Format(Comment[Xml], [ApplicationName,
    Position.ToString,
    Direction.ToString,
    Up.ToString,
    GravityUp.ToString ]);

  { add centerOfRotation and fieldOfView to ExtraFields }
  ExtraFields := '';
  if Version = cvVrml2_X3d then
  begin
    if not CenterOfRotation.IsPerfectlyZero then
    begin
      if Xml then
        ExtraFields := ExtraFields + Format('  centerOfRotation="%s"', [CenterOfRotation.ToRawString]) + NL
      else
        ExtraFields := ExtraFields + Format('  centerOfRotation %s', [CenterOfRotation.ToRawString]) + NL;
    end;
    if not SameValue(PerspectiveFieldOfView, DefaultPerspectiveFieldOfView, 0.001) then
    begin
      if Xml then
        ExtraFields := ExtraFields + FormatDot('  fieldOfView="%f"', [PerspectiveFieldOfView]) + NL
      else
        ExtraFields := ExtraFields + FormatDot('  fieldOfView %f', [PerspectiveFieldOfView]) + NL;
    end;
    { As this is non-standard CGE field, it's especially important to
      add it only when it's non-default. }
    if AutoCenterOfRotation then
    begin
      if Xml then
        ExtraFields := ExtraFields + '  autoCenterOfRotation="TRUE"' + NL
      else
        ExtraFields := ExtraFields + '  autoCenterOfRotation TRUE' + NL;
    end;
  end;

  RotationVectorForGravity := TVector3.CrossProduct(DefaultX3DGravityUp, GravityUp);
  if RotationVectorForGravity.IsZero then
  begin
    { Then GravityUp is parallel to DefaultX3DGravityUp, which means that it's
      just the same. So we can use untranslated Viewpoint node. }
    S1 := Position.ToRawString;
    S2 := OrientationFromDirectionUp(Direction, Up).ToRawString;
    Result := Result + Format(UntransformedViewpoint[Version, Xml], [S1, S2, ExtraFields]);
  end else
  begin
    { Then we must transform Viewpoint node, in such way that
      DefaultX3DGravityUp affected by this transformation will give
      desired GravityUp. }
    AngleForGravity := AngleRadBetweenVectors(DefaultX3DGravityUp, GravityUp);
    S1 := Position.ToRawString;
    S2 := RotationVectorForGravity.ToRawString;
    S3 := Format('%g', [AngleForGravity]);
    { We want such that
        1. standard VRML/X3D dir/up vectors
        2. rotated by orientation
        3. rotated around RotationVectorForGravity
      will give Camera.Direction/Up.
      OrientationFromDirectionUp will calculate the orientation needed to
      achieve given up/dir vectors. So I have to pass there
      MatrixWalker.Direction/Up *already rotated negatively
      around RotationVectorForGravity*. }
    S4 := OrientationFromDirectionUp(
            RotatePointAroundAxisRad(-AngleForGravity, Direction, RotationVectorForGravity),
            RotatePointAroundAxisRad(-AngleForGravity, Up       , RotationVectorForGravity)
          ).ToRawString;
    Result := Result + Format(TransformedViewpoint[Version, Xml], [S1, S2, S3, S4, ExtraFields]);
  end;
end;

function TMakeX3DViewpoint.ToNode(
  out ViewpointNode: TAbstractViewpointNode): TAbstractChildNode;
var
  RotationVectorForGravity: TVector3;
  AngleForGravity: Single;
  Separator: TSeparatorNode_1;
  Transform_1: TTransformNode_1;
  Transform_2: TTransformNode;
  Rotation, Orientation: TVector4;
begin
  RotationVectorForGravity := TVector3.CrossProduct(DefaultX3DGravityUp, GravityUp);
  if RotationVectorForGravity.IsZero then
  begin
    { Then GravityUp is parallel to DefaultX3DGravityUp, which means that it's
      just the same. So we can use untranslated Viewpoint node. }
    case Version of
      cvVrml1_Inventor: ViewpointNode := TPerspectiveCameraNode_1.Create;
      cvVrml2_X3d     : ViewpointNode := TViewpointNode.Create;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('MakeCameraNode Version incorrect');
      {$endif}
    end;
    ViewpointNode.Position := Position;
    ViewpointNode.Orientation := OrientationFromDirectionUp(Direction, Up);
    Result := ViewpointNode;
  end else
  begin
    { Then we must transform Viewpoint node, in such way that
      DefaultX3DGravityUp affected by this transformation will give
      desired GravityUp. }
    AngleForGravity := AngleRadBetweenVectors(DefaultX3DGravityUp, GravityUp);
    Rotation := Vector4(RotationVectorForGravity, AngleForGravity);
    { I want
      1. standard VRML/X3D dir/up vectors
      2. rotated by orientation
      3. rotated around RotationVectorForGravity
      will give MatrixWalker.Direction/Up.
      OrientationFromDirectionUp will calculate the orientation needed to
      achieve given up/dir vectors. So I have to pass there
      MatrixWalker.Direction/Up *already rotated negatively
      around RotationVectorForGravity*. }
    Orientation := OrientationFromDirectionUp(
      RotatePointAroundAxisRad(-AngleForGravity, Direction, RotationVectorForGravity),
      RotatePointAroundAxisRad(-AngleForGravity, Up       , RotationVectorForGravity));
    case Version of
      cvVrml1_Inventor:
        begin
          Transform_1 := TTransformNode_1.Create;
          Transform_1.FdTranslation.Value := Position;
          Transform_1.FdRotation.Value := Rotation;

          ViewpointNode := TPerspectiveCameraNode_1.Create;
          ViewpointNode.Position := TVector3.Zero;
          ViewpointNode.Orientation := Orientation;

          Separator := TSeparatorNode_1.Create;
          Separator.VRML1ChildAdd(Transform_1);
          Separator.VRML1ChildAdd(ViewpointNode);

          Result := Separator;
        end;

      cvVrml2_X3d:
        begin
          Transform_2 := TTransformNode.Create;
          Transform_2.Translation := Position;
          Transform_2.Rotation := Rotation;

          ViewpointNode := TViewpointNode.Create;
          ViewpointNode.Position := TVector3.Zero;
          ViewpointNode.Orientation := Orientation;

          Transform_2.AddChildren(ViewpointNode);

          Result := Transform_2;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('MakeCameraNode Version incorrect');
      {$endif}
    end;
  end;

  { add CenterOfRotation and FieldOfView to ViewpointNode }
  Assert(ViewpointNode <> nil);
  ViewpointNode.CenterOfRotation := CenterOfRotation;
  ViewpointNode.AutoCenterOfRotation := AutoCenterOfRotation;
  if ViewpointNode is TViewpointNode then
    TViewpointNode(ViewpointNode).FieldOfView := PerspectiveFieldOfView;
  if ViewpointNode is TPerspectiveCameraNode_1 then
    TPerspectiveCameraNode_1(ViewpointNode).HeightAngle := PerspectiveFieldOfView;
end;

function TMakeX3DViewpoint.ToNode: TAbstractChildNode;
var
  IgnoredViewpointNode: TAbstractViewpointNode;
begin
  Result := ToNode(IgnoredViewpointNode);
end;

{ routines ------------------------------------------------------------------ }

function MakeCameraStr(const Version: TX3DCameraVersion;
  const Xml: boolean;
  const Position, Direction, Up, GravityUp: TVector3): string;
var
  Make: TMakeX3DViewpoint;
begin
  Make := TMakeX3DViewpoint.Create;
  try
    Make.Version := Version;
    Make.Xml := Xml;
    Make.Position := Position;
    Make.Direction := Direction;
    Make.Up := Up;
    Make.GravityUp := GravityUp;
    Result := Make.ToString;
  finally FreeAndNil(Make) end;
end;

function MakeCameraNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Position, Direction, Up, GravityUp: TVector3;
  out ViewpointNode: TAbstractViewpointNode): TAbstractChildNode;
var
  Make: TMakeX3DViewpoint;
begin
  Make := TMakeX3DViewpoint.Create;
  try
    Make.Version := Version;
    Make.Position := Position;
    Make.Direction := Direction;
    Make.Up := Up;
    Make.GravityUp := GravityUp;
    Result := Make.ToNode(ViewpointNode);
    { BaseUrl on viewpoint node, or its transformation,
      is completely meaningless. Which is why TMakeX3DViewpoint
      doesn't even have BaseUrl property. }
    Result.BaseUrl := BaseUrl;
  finally FreeAndNil(Make) end;
end;

function MakeCameraNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Position, Direction, Up, GravityUp: TVector3): TAbstractChildNode;
var
  IgnoredViewpointNode: TAbstractViewpointNode;
begin
  {$warnings off} // using deprecated in deprecated
  Result := MakeCameraNode(Version, BaseUrl, Position, Direction, Up, GravityUp,
    IgnoredViewpointNode);
  {$warnings on}
end;

function CameraNodeForWholeScene(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const Box: TBox3D;
  const WantedDirection, WantedUp: Integer;
  const WantedDirectionPositive, WantedUpPositive: boolean): TAbstractChildNode;
var
  Position, Direction, Up, GravityUp: TVector3;
  ViewpointNode: TAbstractViewpointNode;
begin
  CameraViewpointForWholeScene(Box, WantedDirection, WantedUp,
    WantedDirectionPositive, WantedUpPositive, Position, Direction, Up, GravityUp);
  {$warnings off} // TODO: fix using deprecated, actually CameraNodeForWholeScene should be reworked to be method of TMakeX3DViewpoint
  Result := MakeCameraNode(Version, BaseUrl,
    Position, Direction, Up, GravityUp, ViewpointNode);

  // set useful CenterOfRotation or AutoCenterOfRotation
  // option 1:
  //ViewpointNode.AutoCenterOfRotation := false;
  //ViewpointNode.CenterOfRotation := Box.Center;
  // option 2:
  // Better if scene may dynamically change to something drastically different.
  ViewpointNode.AutoCenterOfRotation := true;
  {$warnings on}
end;

function MakeCameraNavNode(const Version: TX3DCameraVersion;
  const BaseUrl: string;
  const NavigationType: string;
  const WalkSpeed, VisibilityLimit: Single; const AvatarSize: TVector3;
  const Headlight: boolean): TNavigationInfoNode;
var
  NavigationNode: TNavigationInfoNode;
begin
  case Version of
    cvVrml2_X3d     : NavigationNode := TNavigationInfoNode.Create('', BaseUrl);
    else raise EInternalError.Create('MakeCameraNavNode Version incorrect');
  end;
  NavigationNode.FdType.Items.Clear;
  NavigationNode.FdType.Items.Add(NavigationType);
  NavigationNode.FdAvatarSize.Items.Clear;
  NavigationNode.FdAvatarSize.Items.AddRange([AvatarSize.X, AvatarSize.Y, AvatarSize.Z]);
  NavigationNode.FdHeadlight.Value := Headlight;
  NavigationNode.FdSpeed.Value := WalkSpeed;
  NavigationNode.FdVisibilityLimit.Value := VisibilityLimit;
  Result := NavigationNode;
end;

end.
