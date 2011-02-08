{
  Copyright 2010-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Setting up OpenGL shading (TVRMLShaderGenerator). }
unit VRMLShaderGenerator;

interface

uses GLShaders;

type
  { Create appropriate shader and at the same time set OpenGL parameters
    for fixed-function rendering. Once everything is set up,
    you can use the @link(ShaderProgram) to create and link a program
    (that you should then enable), or simply allow the fixed-function
    pipeline to work.

    This is used internally by TVRMLGLRenderer. It isn't supposed to be used
    directly by other code. }
  TVRMLShaderGenerator = class
  public
    function ShaderProgram: TGLSLProgram;
  end;

implementation

{ TODO: a way to turn off using fixed-function pipeline completely
  will be needed some day.

  TODO: caching shader programs, using the same program if all settings
  are the same, will be needed some day. TShapeCache is not a good place
  for this, as the conditions for two shapes to share arrays/vbos
  are smaller/different (for example, two different geometry nodes
  can definitely share the same shader).

  Maybe caching should be done in this unit, or maybe in TVRMLGLRenderer
  in some TShapeShaderCache or such.

  TODO: a way to turn on/off per-pixel shading should be available.

  TODO: some day, avoid using predefined OpenGL state variables.
  Use only shader uniforms. Right now, we allow some state to be assigned
  using direct normal OpenGL fixed-function functions in VRMLGLRenderer,
  and our shaders just use it.
}

function TVRMLShaderGenerator.ShaderProgram: TGLSLProgram;
begin
  Result := TGLSLProgram.Create;
  Result.AttachVertexShader({$I template.vs.inc});
  Result.AttachFragmentShader({$I template.fs.inc});
  Result.Link(true);
end;

end.
