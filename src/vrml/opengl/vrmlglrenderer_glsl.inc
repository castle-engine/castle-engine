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

  { Scan IDecls for SFNode and MFNode fields, bind texture nodes inside. }
  procedure BindShaderFields(IDecls: TVRMLInterfaceDeclarationsList);

    { If TextureNode <> @nil and is a texture node, prepare it.
      Returns bound texture number (or -1 if failed).
      You should set shader uniform value to this number. }
    function BindTexture(TextureNode: TVRMLNode): LongInt;
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
          VRMLWarning(vwIgnorable, 'Not enough free texture units to bind all textures for this GLSL shader');
      end;
    end;

  var
    I, J: Integer;
    UniformField: TVRMLField;
    BoundValue: LongInt;
    UniformValue: TDynLongIntArray;
  begin
    Assert(IDecls <> nil);
    for I := 0 to IDecls.Count - 1 do
    begin
      if IDecls.Items[I].UniformInvalid then Continue;

      try
        UniformField := IDecls.Items[I].Field;
        if UniformField <> nil then
        begin
          if UniformField is TSFNode then
          begin
            BoundValue := BindTexture(TSFNode(UniformField).Value);
            if BoundValue <> -1 then
              SetUniform(UniformField.Name, BoundValue, true);
          end else
          if UniformField is TMFNode then
          begin
            UniformValue := TDynLongIntArray.Create(TMFNode(UniformField).Count);
            try
              for J := 0 to TMFNode(UniformField).Count - 1 do
              begin
                BoundValue := BindTexture(TMFNode(UniformField).Items[J]);
                { We cannot just resign from setting the field entirely,
                  and setting to -1 would likely produce wild OpenGL warnings
                  (setting texture sampler to -1...).
                  So on failure, assume safe 0. BindTexture already did
                  necessary warnings for user. }
                if BoundValue = -1 then BoundValue := 0;
                UniformValue[J] := BoundValue;
              end;
              SetUniform(UniformField.Name, UniformValue, true);
            finally FreeAndNil(UniformValue) end;
          end;
        end;
      except
        { We capture EGLSLUniformInvalid, converting it to VRMLWarning.
          This way we can set UniformInvalid := true, which will prevent
          from wasting time and repeating the same warning on every frame). }
        on E: EGLSLUniformInvalid do
        begin
          VRMLWarning(vwIgnorable, E.Message);
          IDecls.Items[I].UniformInvalid := true;
        end;
      end;
    end;
  end;

var
  I: Integer;
begin
  Result := inherited SetupUniforms(BoundTextureUnits);
  for I := 0 to UniformsNodes.Count - 1 do
    BindShaderFields(UniformsNodes[I].InterfaceDeclarations);
end;

{$endif read_implementation}
