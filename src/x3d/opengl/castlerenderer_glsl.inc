{$ifdef read_interface}

  { GLSL program integrated with VRML/X3D renderer. Adds ability to bind
    VRML/X3D textures to uniform variables of GLSL shader. }
  TX3DGLSLProgram = class(TX3DShaderProgram)
  private
    Renderer: TGLRenderer;
  public
    constructor Create(ARenderer: TGLRenderer);
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DGLSLProgram ----------------------------------------------------------- }

constructor TX3DGLSLProgram.Create(ARenderer: TGLRenderer);
begin
  inherited Create;
  Renderer := ARenderer;
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
          if Renderer.GLTextureNodes.Bind(
            TAbstractTextureNode(TextureNode), BoundTextureUnits) then
          begin
            Result := BoundTextureUnits;
            Inc(BoundTextureUnits);
          end else
            SetupUniforms := false;
        end else
          WritelnWarning('VRML/X3D', 'Not enough free texture units to bind all textures for this GLSL shader');
      end;
    end;

  var
    J: Integer;
    BoundValue: LongInt;
    UniformValue: TLongIntList;
  begin
    try
      if TextureField is TSFNode then
      begin
        BoundValue := BindSingleTexture(TSFNode(TextureField).Value);
        if BoundValue <> -1 then
          SetUniform(TextureField.X3DName, BoundValue, true);
      end else
      begin
        Assert(TextureField is TMFNode);

        UniformValue := TLongIntList.Create;
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
          SetUniform(TextureField.X3DName, UniformValue, true);
        finally FreeAndNil(UniformValue) end;
      end;
      Result := true;
    except
      { We capture EGLSLUniformInvalid, converting it to WritelnWarning.
        This way we can set Result := false, which will prevent
        from wasting time and repeating the same warning on every frame. }
      on E: EGLSLUniformInvalid do
      begin
        WritelnWarning('VRML/X3D', E.Message);
        Result := false;
      end;
    end;
  end;

var
  I: Integer;
begin
  Result := inherited SetupUniforms(BoundTextureUnits);
  I := 0;
  while I < UniformsTextures.Count do
  begin
    if BindTextureUniform(UniformsTextures[I]) then
      Inc(I) else
      UniformsTextures.Delete(I);
  end;
end;

{$endif read_implementation}
