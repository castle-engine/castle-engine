{
  Copyright 2009-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Render state.) }
unit RenderStateUnit;

interface

uses KambiUtils, VectorMath, Frustum, Cameras;

{$define read_interface}

type
  TRenderState = class;

  TCameraChangedEvent = procedure (RenderState: TRenderState) of object;

  TDynArrayItem_1 = TCameraChangedEvent;
  PDynArrayItem_1 = ^TCameraChangedEvent;
  {$define DYNARRAY_1_IS_FUNCTION}
  {$define DYNARRAY_1_IS_FUNCTION_METHOD}
  {$I dynarray_1.inc}
  TDynCameraChangedEventArray = class(TDynArray_1)
  public
    { This calls all functions (all Items). }
    procedure ExecuteAll(RenderState: TRenderState);
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

  { Common knowledge about currently rendered 3D state.
    This is basically just a collection of global variables that
    for whatever reason have to be read/write in various distinct
    parts of the engine. If they don't fit elsewhere, they go here. }
  TRenderState = class
  private
    FOnCameraChanged: TDynCameraChangedEventArray;
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

      Always after changing this, change also all other Camera*
      fields, and then call CameraChanged. }
    CameraMatrix: TMatrix4Single;

    { Inverse of CameraMatrix.

      Always call CameraInverseMatrixNeeded before using it,
      CameraInverseMatrixNeeded will check CameraInverseMatrixDone
      and eventually will calculate inverse and set CameraInverseMatrixDone to
      @true. }
    CameraInverseMatrix: TMatrix4Single;
    CameraInverseMatrixDone: boolean;

    { Camera rotation matrix. That is, this is like CameraMatrix but
      it doesn't move the camera, only rotates it.

      It's guaranteed that this is actually only 3x3 matrix,
      the 4th row and 4th column are all zero except the lowest right item
      which is 1.0. }
    CameraRotationMatrix: TMatrix4Single;

    { Inverse of CameraRotationMatrix.

      Always call CameraRotationInverseMatrixNeeded before using it,
      CameraRotationInverseMatrixNeeded will check CameraRotationInverseMatrixDone
      and eventually will calculate inverse and set CameraRotationInverseMatrixDone to
      @true. }
    CameraRotationInverseMatrix: TMatrix4Single;
    CameraRotationInverseMatrixDone: boolean;

    CameraFrustum: TFrustum;

    procedure CameraInverseMatrixNeeded;
    procedure CameraRotationInverseMatrixNeeded;

    { Camera rotation matrix, as a 3x3 matrix. }
    function CameraRotationMatrix3: TMatrix3Single;
    function CameraRotationInverseMatrix3: TMatrix3Single;

    { Set all Camera* properties from TCamera instance ACamera.

      Remember that @link(Target) must be already set correctly when calling
      this, registered OnCameraChanged callbacks may read it. }
    procedure CameraFromCameraObject(ACamera: TCamera);

    { Set all Camera* properties from explict matrices.
      ProjectionMatrix is needed to calculate frustum.

      Remember that @link(Target) must be already set correctly when calling
      this, registered OnCameraChanged callbacks may read it. }
    procedure CameraFromMatrix(const ACameraMatrix, ACameraRotationMatrix,
      ProjectionMatrix: TMatrix4Single);

    property Target: TRenderTarget read FTarget write FTarget;

    { Always called after camera changed.
      This will call all registered OnCameraChanged events.
      Remember that @link(Target) must be already set correctly when calling
      this, registered OnCameraChanged callbacks may read it. }
    procedure CameraChanged;

    property OnCameraChanged: TDynCameraChangedEventArray read FOnCameraChanged;
  end;

var
  RenderState: TRenderState;

{$undef read_interface}

implementation

uses SysUtils, KambiLog;

{$define read_implementation}
{$I dynarray_1.inc}

{ TDynCameraChangedEventArray ------------------------------------------------ }

procedure TDynCameraChangedEventArray.ExecuteAll(RenderState: TRenderState);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
    Items[I](RenderState);
end;

{ TRenderState --------------------------------------------------------------- }

constructor TRenderState.Create;
begin
  inherited;
  FOnCameraChanged := TDynCameraChangedEventArray.Create;
end;

destructor TRenderState.Destroy;
begin
  FreeAndNil(FOnCameraChanged);
  inherited;
end;

procedure TRenderState.CameraChanged;
begin
  FOnCameraChanged.ExecuteAll(Self);
end;

procedure TRenderState.CameraInverseMatrixNeeded;
begin
  if not CameraInverseMatrixDone then
  begin
    if not TryMatrixInverse(CameraMatrix, CameraInverseMatrix) then
    begin
      CameraInverseMatrix := IdentityMatrix4Single;
      if Log then
        WritelnLogMultiline('Camera', 'Camera matrix cannot be inverted, convertions between world and camera space will not be done. Camera matrix is: ' +
          MatrixToRawStr(CameraMatrix, '  '));
    end;
    CameraInverseMatrixDone := true;
  end;
end;

procedure TRenderState.CameraRotationInverseMatrixNeeded;
begin
  if not CameraRotationInverseMatrixDone then
  begin
    if not TryMatrixInverse(CameraRotationMatrix, CameraRotationInverseMatrix) then
    begin
      CameraRotationInverseMatrix := IdentityMatrix4Single;
      if Log then
        WritelnLogMultiline('Camera', 'Camera rotation matrix cannot be inverted, convertions between world and camera space will not be done. Camera matrix is: ' +
          MatrixToRawStr(CameraRotationMatrix, '  '));
    end;
    CameraRotationInverseMatrixDone := true;
  end;
end;

function TRenderState.CameraRotationMatrix3: TMatrix3Single;
begin
  Move(CameraRotationMatrix[0], Result[0], SizeOf(Single) * 3);
  Move(CameraRotationMatrix[1], Result[1], SizeOf(Single) * 3);
  Move(CameraRotationMatrix[2], Result[2], SizeOf(Single) * 3);
end;

function TRenderState.CameraRotationInverseMatrix3: TMatrix3Single;
begin
  Move(CameraRotationInverseMatrix[0], Result[0], SizeOf(Single) * 3);
  Move(CameraRotationInverseMatrix[1], Result[1], SizeOf(Single) * 3);
  Move(CameraRotationInverseMatrix[2], Result[2], SizeOf(Single) * 3);
end;

procedure TRenderState.CameraFromCameraObject(ACamera: TCamera);
begin
  CameraMatrix := ACamera.Matrix;
  CameraInverseMatrixDone := false;
  CameraRotationMatrix := ACamera.RotationMatrix;
  CameraRotationInverseMatrixDone := false;
  CameraFrustum := ACamera.Frustum;
  CameraChanged;
end;

procedure TRenderState.CameraFromMatrix(const ACameraMatrix, ACameraRotationMatrix,
  ProjectionMatrix: TMatrix4Single);
begin
  CameraMatrix := ACameraMatrix;
  CameraInverseMatrixDone := false;
  CameraRotationMatrix := ACameraRotationMatrix;
  CameraRotationInverseMatrixDone := false;
  CameraFrustum.Init(ProjectionMatrix, ACameraMatrix);
  CameraChanged;
end;

initialization
  RenderState := TRenderState.Create;
finalization
  FreeAndNil(RenderState);
end.
