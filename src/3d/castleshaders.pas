{ Shaders types. Independent from OpenGL and X3D. }
unit CastleShaders;

interface

type
  TShaderType = (stVertex, stGeometry, stFragment);

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

implementation

end.