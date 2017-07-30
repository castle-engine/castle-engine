{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Current rendering camera (TRenderingCamera).) }
unit CastleRenderingCamera;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleUtils, CastleVectors, CastleFrustum, CastleCameras, X3DNodes;

type
  TRenderingCamera = class;

  TCameraChangedEvent = procedure (
    const RenderingCamera: TRenderingCamera;
    Viewpoint: TAbstractViewpointNode) of object;

  TCameraChangedEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TCameraChangedEvent>)
  public
    { This calls all functions (all Items). }
    procedure ExecuteAll(const RenderingCamera: TRenderingCamera;
      const Viewpoint: TAbstractViewpointNode);
  end;

  TRenderTarget = (
    { Normal rendering. }
    rtScreen,
    { Rendering color buffer contents to normal single 2D texture. }
    rfOffScreen,
    { Rendering color buffer contents to cube map texture. }
    rtCubeMapEnvironment,
    { Rendering depth buffer contents to shadow map texture. }
    rtShadowMap,
    { Rendering with a special VSM shader to capture shadow map texture
      (in the normal color buffer). }
    rtVarianceShadowMap);

  { Current camera used for rendering. }
  TRenderingCamera = class
  private
    FOnChanged: TCameraChangedEventList;
    FTarget: TRenderTarget;
  public
    constructor Create;
    destructor Destroy; override;
  public
    { Current camera matrix. Transforms from world space (normal 3D space)
      to camera space (camera space is the space where you're always
      standing on zero point, looking in -Z, and so on).

      This is needed for various things, like
      TextureCoordinateGenerator.mode = "WORLDSPACE*" or generating
      Viewpoint.camera[Inverse]Matrix event.

      Always after changing this, change also all other camera
      fields, and then call @link(Changed). }
    Matrix: TMatrix4;

    { Inverse of @link(Matrix).

      Always call InverseMatrixNeeded before using it,
      InverseMatrixNeeded will check InverseMatrixDone
      and eventually will calculate inverse and set InverseMatrixDone to
      @true. }
    InverseMatrix: TMatrix4;
    InverseMatrixDone: boolean;

    { Camera rotation matrix. That is, this is like @link(Matrix) but
      it doesn't move the camera, only rotates it.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    RotationMatrix: TMatrix4;

    { Inverse of RotationMatrix.

      Always call RotationInverseMatrixNeeded before using it,
      RotationInverseMatrixNeeded will check RotationInverseMatrixDone
      and eventually will calculate inverse and set RotationInverseMatrixDone to
      @true. }
    RotationInverseMatrix: TMatrix4;
    RotationInverseMatrixDone: boolean;

    Frustum: TFrustum;

    procedure InverseMatrixNeeded;
    procedure RotationInverseMatrixNeeded;

    { Camera rotation matrix, as a 3x3 matrix. }
    function RotationMatrix3: TMatrix3;
    function RotationInverseMatrix3: TMatrix3;

    { Set all properties (except Target) from TCamera instance in ACamera.
      See @link(FromMatrix) for comments about @link(Target) property
      and Viewpoint parameter. }
    procedure FromCameraObject(const ACamera: TCamera;
      const Viewpoint: TAbstractViewpointNode);

    { Set all properties (except Target) from explict matrices.
      ProjectionMatrix is needed to calculate frustum.

      Remember that @link(Target) must be already set correctly when calling
      this, registered OnChanged callbacks may read it.

      Viewpoint is only passed to the Changed and OnChanged.
      It should be non-nil to indicate that the view comes from non-standard
      (not currently bound) VRML/X3D Viewpoint node. }
    procedure FromMatrix(const AMatrix, ARotationMatrix,
      ProjectionMatrix: TMatrix4;
      const Viewpoint: TAbstractViewpointNode);

    property Target: TRenderTarget read FTarget write FTarget;

    { Always called after camera changed.
      This will call all registered OnChanged events.
      Remember that @link(Target) must be already set correctly when calling
      this, registered OnChanged callbacks may read it. }
    procedure Changed(const Viewpoint: TAbstractViewpointNode);

    property OnChanged: TCameraChangedEventList read FOnChanged;
  end;

var
  RenderingCamera: TRenderingCamera;

implementation

uses SysUtils, CastleLog;

{ TCameraChangedEventList ------------------------------------------------ }

procedure TCameraChangedEventList.ExecuteAll(
  const RenderingCamera: TRenderingCamera;
  const Viewpoint: TAbstractViewpointNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](RenderingCamera, Viewpoint);
end;

{ TRenderingCamera --------------------------------------------------------------- }

constructor TRenderingCamera.Create;
begin
  inherited;
  FOnChanged := TCameraChangedEventList.Create;
end;

destructor TRenderingCamera.Destroy;
begin
  FreeAndNil(FOnChanged);
  inherited;
end;

procedure TRenderingCamera.Changed(const Viewpoint: TAbstractViewpointNode);
begin
  FOnChanged.ExecuteAll(Self, Viewpoint);
end;

procedure TRenderingCamera.InverseMatrixNeeded;
begin
  if not InverseMatrixDone then
  begin
    if not Matrix.TryInverse(InverseMatrix) then
    begin
      InverseMatrix := TMatrix4.Identity;
      if Log then
        WritelnLogMultiline('Camera', 'Camera matrix cannot be inverted, conversions between world and camera space will not be done. Camera matrix is: ' +
          Matrix.ToRawString('  '));
    end;
    InverseMatrixDone := true;
  end;
end;

procedure TRenderingCamera.RotationInverseMatrixNeeded;
begin
  if not RotationInverseMatrixDone then
  begin
    if not RotationMatrix.TryInverse(RotationInverseMatrix) then
    begin
      RotationInverseMatrix := TMatrix4.Identity;
      if Log then
        WritelnLogMultiline('Camera', 'Camera rotation matrix cannot be inverted, conversions between world and camera space will not be done. Camera matrix is: ' +
          RotationMatrix.ToRawString('  '));
    end;
    RotationInverseMatrixDone := true;
  end;
end;

function TRenderingCamera.RotationMatrix3: TMatrix3;
begin
  Move(RotationMatrix.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Move(RotationMatrix.Data[1], Result.Data[1], SizeOf(Single) * 3);
  Move(RotationMatrix.Data[2], Result.Data[2], SizeOf(Single) * 3);
end;

function TRenderingCamera.RotationInverseMatrix3: TMatrix3;
begin
  Move(RotationInverseMatrix.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Move(RotationInverseMatrix.Data[1], Result.Data[1], SizeOf(Single) * 3);
  Move(RotationInverseMatrix.Data[2], Result.Data[2], SizeOf(Single) * 3);
end;

procedure TRenderingCamera.FromCameraObject(const ACamera: TCamera;
  const Viewpoint: TAbstractViewpointNode);
begin
  Matrix := ACamera.Matrix;
  InverseMatrixDone := false;
  RotationMatrix := ACamera.RotationMatrix;
  RotationInverseMatrixDone := false;
  Frustum := ACamera.Frustum;
  Changed(Viewpoint);
end;

procedure TRenderingCamera.FromMatrix(
  const AMatrix, ARotationMatrix, ProjectionMatrix: TMatrix4;
  const Viewpoint: TAbstractViewpointNode);
begin
  Matrix := AMatrix;
  InverseMatrixDone := false;
  RotationMatrix := ARotationMatrix;
  RotationInverseMatrixDone := false;
  Frustum.Init(ProjectionMatrix, AMatrix);
  Changed(Viewpoint);
end;

initialization
  RenderingCamera := TRenderingCamera.Create;
finalization
  FreeAndNil(RenderingCamera);
end.
