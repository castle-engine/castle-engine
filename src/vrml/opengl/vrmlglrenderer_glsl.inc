{$ifdef read_interface}

  { GLSL program integrated with VRML renderer. Adds ability to bind
    VRML textures to uniform variables of GLSL shader. }
  TVRMLGLSLProgram = class(TVRMLShaderProgram)
  private
    Renderer: TVRMLGLRenderer;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
    function SetupUniforms(var BoundTextureUnits: Cardinal): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVRMLGLSLProgram ----------------------------------------------------------- }

constructor TVRMLGLSLProgram.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited Create;
  Renderer := ARenderer;
end;

function TVRMLGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;

  { Bind texture node to a uniform value.
    Returns false if setting this texture node resulted in GLSL error
    (invalid name or type in shader code)
    and it should be removed from UniformsTextures. }
  function BindTextureUniform(TextureField: TVRMLField): boolean;

    { If TextureNode <> @nil and is a texture node, prepare it.
      Returns bound texture number (or -1 if failed).
      You should set shader uniform value to this number. }
    function BindSingleTexture(TextureNode: TVRMLNode): LongInt;
    begin
      Result := -1;
      if (TextureNode <> nil) and
         (TextureNode is TNodeX3DTextureNode) then
      begin
        if BoundTextureUnits < GLMaxTextureUnits then
        begin
          if Renderer.GLTextureNodes.Bind(
            TNodeX3DTextureNode(TextureNode), BoundTextureUnits) then
          begin
            Result := BoundTextureUnits;
            Inc(BoundTextureUnits);
          end else
            SetupUniforms := false;
        end else
          OnWarning(wtMinor, 'VRML/X3D', 'Not enough free texture units to bind all textures for this GLSL shader');
      end;
    end;

  var
    J: Integer;
    BoundValue: LongInt;
    UniformValue: TDynLongIntArray;
  begin
    try
      if TextureField is TSFNode then
      begin
        BoundValue := BindSingleTexture(TSFNode(TextureField).Value);
        if BoundValue <> -1 then
          SetUniform(TextureField.Name, BoundValue, true);
      end else
      begin
        Assert(TextureField is TMFNode);

        UniformValue := TDynLongIntArray.Create(TMFNode(TextureField).Count);
        try
          for J := 0 to TMFNode(TextureField).Count - 1 do
          begin
            BoundValue := BindSingleTexture(TMFNode(TextureField).Items[J]);
            { We cannot just resign from setting the field entirely,
              and setting to -1 would likely produce wild OpenGL warnings
              (setting texture sampler to -1...).
              So on failure, assume safe 0. BindTexture already did
              necessary warnings for user. }
            if BoundValue = -1 then BoundValue := 0;
            UniformValue[J] := BoundValue;
          end;
          SetUniform(TextureField.Name, UniformValue, true);
        finally FreeAndNil(UniformValue) end;
      end;
      Result := true;
    except
      { We capture EGLSLUniformInvalid, converting it to OnWarning.
        This way we can set Result := false, which will prevent
        from wasting time and repeating the same warning on every frame. }
      on E: EGLSLUniformInvalid do
      begin
        OnWarning(wtMinor, 'VRML/X3D', E.Message);
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
