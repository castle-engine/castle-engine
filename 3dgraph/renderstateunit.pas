{
  Copyright 2009 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ @abstract(Render state.) }
unit RenderStateUnit;

interface

uses KambiUtils, VectorMath;

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

  { Common knowledge about currently rendered 3D state.
    This is basically just a collection of global variables that
    for whatever reason have to be read/write in various distinct
    parts of the engine. If they don't fit elsewhere, they go here. }
  TRenderState = class
  private
    FOnCameraChanged: TDynCameraChangedEventArray;
  public
    constructor Create;
    destructor Destroy; override;

    { Value > 0 means we're inside some stencil test (like for
      InShadow = @false pass of shadow volumes). }
    StencilTest: Cardinal;

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

    procedure CameraInverseMatrixNeeded;

    { Always called after camera changed.
      This will call all registered OnCameraChanged events. }
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

procedure TRenderState.CameraChanged;
begin
  FOnCameraChanged.ExecuteAll(Self);
end;

initialization
  RenderState := TRenderState.Create;
finalization
  FreeAndNil(RenderState);
end.
