{ Shaders types. Independent from OpenGL and X3D. }
unit CastleShaders;

{$I castleconf.inc}

interface

type
  TShaderType = (stVertex, stGeometry, stFragment);

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

  ShaderTypeNameX3D: array [TShaderType] of string =
  ( 'VERTEX', 'GEOMETRY', 'FRAGMENT' );

implementation

end.
