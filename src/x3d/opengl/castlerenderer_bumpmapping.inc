{$ifdef read_interface}

private
  { If this State indicates a normal map, use it for bump mapping. }
  procedure BumpMappingEnable(State: TX3DGraphTraverseState;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

{$endif read_interface}

{$ifdef read_implementation}
procedure TGLRenderer.BumpMappingEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { Return OpenGL texture initialized for given texture node.
    If Node is nil, or it's corresponding OpenGL resource is not initialized
    for whatever reason --- returns 0. }
  function GLTexture(Node: TAbstractTextureNode;
    out DisplacementTextureInAlpha: boolean): TGLTextureId;
  var
    GLNode: TGLTextureNode;
  begin
    Result := 0;
    if Node <> nil then
    begin
      GLNode := GLTextureNodes.TextureNode(Node);
      if GLNode is TGL2DTextureNode then // also checks is GLNode <> nil
      begin
        Result := TGL2DTextureNode(GLNode).GLName;
        DisplacementTextureInAlpha := Node.AlphaChannelFinal <> acNone;
      end;
    end;
  end;

  procedure Enable(const NormalTexture: TAbstractTextureNode;
    const NormalTextureCoordinatesId: Integer;
    const DisplacementFactor: Single);
  var
    GLNormalTexture: TGLTextureId;
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
          NormalTextureCoordinatesId, DisplacementTextureInAlpha, DisplacementFactor);
        Inc(ABoundTextureUnits);

        { There is no need to increase the TexCoordNeeded unconditionally.
          If the NormalTextureCoordinatesId = 0 and
          TexCoordNeeded = 1 then we're already satisfied, as then the normalmap
          will use the existing texture coordinates from e.g. diffuse texture. }
        MaxVar(ATexCoordsNeeded, NormalTextureCoordinatesId + 1);
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
      Enable(
        SurfaceShader.NormalTexture,
        SurfaceShader.NormalTextureCoordinatesId,
        SurfaceShader.DisplacementFactor / 255.0);
    end else
    if State.ShapeNode.Appearance <> nil then
    begin
      Appearance := State.ShapeNode.Appearance;
      Enable(Appearance.NormalMap, 0, Appearance.HeightMapScale);
    end;
  end;
end;

{$endif read_implementation}
