{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating shader code for "plane mirror" feature,
  https://castle-engine.io/x3d_extensions_mirror_plane.php . }

{$ifdef read_interface}

//type
  { Shader uniforms calculated by rendering using ViewpointMirror,
    used by GLSL code calculating texture coordinates in case of
    TextureCoordinateGenerator.mode = "MIRROR-PLANE". }
  TMirrorPlaneUniforms = class
    NormalizedPlane: TVector4;
    CameraPositionOnPlane, CameraSide, CameraUp: TVector3;
    FrustumDimensions: TFloatRectangle;
    procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram);
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TMirrorPlaneUniforms.SetDynamicUniforms(const AProgram: TX3DShaderProgram);
begin
  AProgram.SetUniform('castle_NormalizedPlane', NormalizedPlane);
  AProgram.SetUniform('castle_CameraPositionOnPlane', CameraPositionOnPlane);
  AProgram.SetUniform('castle_CameraSide', CameraSide);
  AProgram.SetUniform('castle_CameraUp', CameraUp);
  AProgram.SetUniform('castle_FrustumDimensions',
    Vector4(
      FrustumDimensions.Left,
      FrustumDimensions.Bottom,
      FrustumDimensions.Width,
      FrustumDimensions.Height));
end;

{$endif read_implementation}
