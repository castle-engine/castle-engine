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

uses VectorMath;

type
  { Common knowledge about currently rendered 3D state.
    This is basically just a collection of global variables that
    for whatever reason have to be read/write in various distinct
    parts of the engine. If they don't fit elsewhere, they go here. }
  TRenderState = class
  public
    { Value > 0 means we're inside some stencil test (like for
      InShadow = @false pass of shadow volumes). }
    StencilTest: Cardinal;

    { Current camera matrix. Transforms from world space (normal 3D space)
      to camera space (camera space is the space where you're always
      standing on zero point, looking in -Z, and so on).

      This is needed for various things, like
      TextureCoordinateGenerator.mode = "WORLDSPACE*" or generating
      Viewpoint.camera[Inverse]Matrix event.

      Always after changing this, change also all other Camera*Matrix
      fields, and then call CameraMatrixChanged. }
    CameraMatrix: TMatrix4Single;

    { Inverse of CameraMatrix.

      Always call CameraInverseMatrixNeeded before using it,
      CameraInverseMatrixNeeded will check CameraInverseMatrixDone
      and eventually will calculate inverse and set CameraInverseMatrixDone to
      @true. }
    CameraInverseMatrix: TMatrix4Single;
    CameraInverseMatrixDone: boolean;

    procedure CameraInverseMatrixNeeded;

{}{This should update TVRMLScene registered.}
    procedure CameraMatrixChanged;
  end;

var
  RenderState: TRenderState;

implementation

uses SysUtils, KambiLog;

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

procedure TRenderState.CameraMatrixChanged;
begin
end;

initialization
  RenderState := TRenderState.Create;
finalization
  FreeAndNil(RenderState);
end.
