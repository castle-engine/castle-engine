{$ifdef read_interface}

private
  { If this State indicates a normal map, use it for bump mapping. }
  procedure BumpMappingEnable(State: TX3DGraphTraverseState;
    var ABoundTextureUnits: Cardinal; Shader: TShader);

{$endif read_interface}

{$ifdef read_implementation}
procedure TGLRenderer.BumpMappingEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits: Cardinal; Shader: TShader);

  { Return OpenGL texture initialized for given texture node.
    If Node is nil, or it's corresponding OpenGL resource is not initialized
    for whatever reason --- returns 0. }
  function GLTexture(Node: TAbstractTextureNode;
    out DisplacementTextureInAlpha: boolean): TGLuint;
  var
    GLNode: TGLTextureNode;
  begin
    Result := 0;
    if Node <> nil then
    begin
      GLNode := GLTextureNodes.TextureNode(Node);
      if GLNode <> nil then
      begin
        if GLNode is TGLImageTextureNode then
          Result := TGLImageTextureNode(GLNode).GLName
        else
        if GLNode is TGLMovieTextureNode then
          Result := TGLMovieTextureNode(GLNode).GLName;

        DisplacementTextureInAlpha := Node.AlphaChannelFinal <> acNone;
      end;
    end;
  end;

  procedure Enable(const NormalTexture: TAbstractTextureNode;
    const DisplacementFactor: Single);
  const
    { TODO: take it from CommonSurfaceShader }
    NormalMapTextureCoordinatesUnit = 0;
  var
    GLNormalTexture: TGLuint;
    { Does NormalTexture have alpha channel. }
    DisplacementTextureInAlpha: boolean;
  begin
    if ShapeBumpMappingAllowed and (BumpMapping <> bmNone) then
    begin
      GLNormalTexture := GLTexture(NormalTexture, DisplacementTextureInAlpha);
      if GLNormalTexture <> 0 then
      begin
        ShapeBumpMappingUsed := true;

        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLNormalTexture);
        Shader.EnableBumpMapping(BumpMapping, ABoundTextureUnits,
          NormalMapTextureCoordinatesUnit,
          DisplacementTextureInAlpha, DisplacementFactor);
        Inc(ABoundTextureUnits);
      end;
    end;
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
  Appearance: TAppearanceNode;
begin
  if State.ShapeNode <> nil then
  begin
    SurfaceShader := State.ShapeNode.CommonSurfaceShader;
    if SurfaceShader <> nil then
    begin
      Enable(SurfaceShader.NormalTexture, SurfaceShader.DisplacementFactor / 255.0)
    end else
    if State.ShapeNode.Appearance <> nil then
    begin
      Appearance := State.ShapeNode.Appearance;
      Enable(Appearance.NormalMap, Appearance.HeightMapScale);
    end;
  end;
end;

{$endif read_implementation}
