{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Textures that affect surface parameters, like normal maps and other
  textures defined by the CommonSurfaceShader. }

{$ifdef read_interface}

private
  { Return OpenGL 2D texture initialized for given texture node.
    If Node is nil, or it's corresponding OpenGL resource is not initialized
    for whatever reason --- returns 0. }
  function GLTexture2D(Node: TAbstractTextureNode;
    out TextureHasAlpha: boolean): TGLTextureId;
  function GLTexture2D(Node: TAbstractTextureNode): TGLTextureId;

  { If this State indicates a normal map, use it for bump mapping. }
  procedure BumpMappingEnable(State: TX3DGraphTraverseState;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { If this State indicates other surface texture, use it. }
  procedure SurfaceTexturesEnable(State: TX3DGraphTraverseState;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

{$endif read_interface}

{$ifdef read_implementation}

function TGLRenderer.GLTexture2D(Node: TAbstractTextureNode;
  out TextureHasAlpha: boolean): TGLTextureId;
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
      TextureHasAlpha := Node.AlphaChannelFinal <> acNone;
    end;
  end;
end;

function TGLRenderer.GLTexture2D(Node: TAbstractTextureNode): TGLTextureId;
var
  IgnoreTextureHasAlpha: boolean;
begin
  Result := GLTexture2D(Node, IgnoreTextureHasAlpha);
end;

procedure TGLRenderer.BumpMappingEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure Enable(const NormalTexture: TAbstractTextureNode;
    const NormalTextureCoordinatesId: Integer;
    const DisplacementFactor: Single);
  var
    GLNormalTexture: TGLTextureId;
    { Does NormalTexture have alpha channel. }
    DisplacementTextureInAlpha: boolean;
  begin
    if ShapeBumpMappingAllowed and
       (BumpMapping <> bmNone) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLNormalTexture := GLTexture2D(NormalTexture, DisplacementTextureInAlpha);
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

procedure TGLRenderer.SurfaceTexturesEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure SurfaceTextureEnable(const SurfaceTexture: TSurfaceTexture;
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const TextureChannelMask: string);
  var
    GLTextureId: TGLTextureId;
  begin
    if (Texture <> nil) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLTextureId := GLTexture2D(Texture);
      if GLTextureId <> 0 then
      begin
        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLTextureId);
        Shader.EnableSurfaceTexture(SurfaceTexture, ABoundTextureUnits,
          Max(0, TextureCoordinatesId), TextureChannelMask);
        Inc(ABoundTextureUnits);

        { There is no need to increase the TexCoordNeeded unconditionally.
          If the NormalTextureCoordinatesId = 0 and
          TexCoordNeeded = 1 then we're already satisfied, as then the normalmap
          will use the existing texture coordinates from e.g. diffuse texture. }
        MaxVar(ATexCoordsNeeded, TextureCoordinatesId + 1);
      end;
    end;
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  if State.ShapeNode <> nil then
  begin
    SurfaceShader := State.ShapeNode.CommonSurfaceShader;
    if SurfaceShader <> nil then
    begin
      SurfaceTextureEnable(stAmbient,
        SurfaceShader.AmbientTexture,
        SurfaceShader.AmbientTextureCoordinatesId,
        SurfaceShader.AmbientTextureChannelMask);
      SurfaceTextureEnable(stSpecular,
        SurfaceShader.SpecularTexture,
        SurfaceShader.SpecularTextureCoordinatesId,
        SurfaceShader.SpecularTextureChannelMask);
      SurfaceTextureEnable(stShininess,
        SurfaceShader.ShininessTexture,
        SurfaceShader.ShininessTextureCoordinatesId,
        SurfaceShader.ShininessTextureChannelMask);
    end;
  end;
end;

{$endif read_implementation}
