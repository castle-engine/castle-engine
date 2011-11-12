{ Shaders types. Independent from OpenGL and X3D. }
unit Shaders;

interface

type
  TShaderType = (stVertex, stGeometry, stFragment);

const
  ShaderTypeName: array [TShaderType] of string =
  ( 'Vertex', 'Geometry', 'Fragment' );

implementation

end.