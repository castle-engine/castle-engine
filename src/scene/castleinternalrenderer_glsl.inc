{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { GLSL program integrated with TRenderer.
    This class adds ability to bind textures to uniform variables
    of GLSL shader. }
  TX3DGLSLProgram = class(TX3DShaderProgram)
  public
    constructor Create;
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DGLSLProgram ----------------------------------------------------------- }

constructor TX3DGLSLProgram.Create;
begin
  inherited Create;
end;

function TX3DGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;

  { Bind texture node to a uniform value.
    Returns false if setting this texture node resulted in GLSL error
    (invalid name or type in shader code)
    and it should be removed from UniformsTextures. }
  function BindTextureUniform(TextureField: TX3DField): boolean;

    { If TextureNode <> @nil and is a texture node, prepare it.
      Returns bound texture number (or -1 if failed).
      You should set shader uniform value to this number. }
    function BindSingleTexture(TextureNode: TX3DNode): LongInt;
    begin
      Result := -1;
      if (TextureNode <> nil) and
         (TextureNode is TAbstractTextureNode) then
      begin
        if BoundTextureUnits < GLFeatures.MaxTextureUnits then
        begin
          if TTextureResources.Bind(
            TAbstractTextureNode(TextureNode), BoundTextureUnits) then
          begin
            Result := BoundTextureUnits;
            Inc(BoundTextureUnits);
          end else
            SetupUniforms := false;
        end else
          WritelnWarning('Renderer', 'Not enough free texture units to bind all textures for this GLSL shader');
      end;
    end;

  var
    J: Integer;
    BoundValue: Int32;
    UniformValue: TInt32List;
    AUniformMissing: TUniformMissing;
  begin
    AUniformMissing := UniformMissingFromNode(TextureField.ParentNode as TX3DNode);

    if TextureField is TSFNode then
    begin
      BoundValue := BindSingleTexture(TSFNode(TextureField).Value);
      if BoundValue <> -1 then
        SetUniform(TextureField.X3DName, BoundValue, AUniformMissing);
    end else
    begin
      Assert(TextureField is TMFNode);

      UniformValue := TInt32List.Create;
      try
        UniformValue.Count := TMFNode(TextureField).Count;
        for J := 0 to TMFNode(TextureField).Count - 1 do
        begin
          BoundValue := BindSingleTexture(TMFNode(TextureField)[J]);
          { We cannot just resign from setting the field entirely,
            and setting to -1 would likely produce wild OpenGL warnings
            (setting texture sampler to -1...).
            So on failure, assume safe 0. BindTexture already did
            necessary warnings for user. }
          if BoundValue = -1 then BoundValue := 0;
          UniformValue[J] := BoundValue;
        end;
        SetUniform(TextureField.X3DName, UniformValue, AUniformMissing);
      finally FreeAndNil(UniformValue) end;
    end;
    Result := true;
  end;

var
  I: Integer;
begin
  Result := inherited SetupUniforms(BoundTextureUnits);
  I := 0;
  while I < UniformsTextures.Count do
  begin
    if BindTextureUniform(UniformsTextures[I]) then
      Inc(I)
    else
      UniformsTextures.Delete(I);
  end;
end;

{$endif read_implementation}
