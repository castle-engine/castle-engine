{
  Copyright 2002-2018 Michalis Kamburelis.

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

  { Is this shape possibly use some texture that requires Phong shading.
    Decide before actually making any OpenGL operations. }
  class function ShapeMaybeUsesPhongSurfaceTexture(const Shape: TX3DRendererShape): boolean;

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

class function TGLRenderer.ShapeMaybeUsesPhongSurfaceTexture(const Shape: TX3DRendererShape): boolean;
var
  SurfaceShader: TCommonSurfaceShaderNode;
begin
  Result := false;
  if Shape.Node <> nil then
  begin
    SurfaceShader := Shape.Node.CommonSurfaceShader;
    if SurfaceShader <> nil then
    begin
      Result :=
        (SurfaceShader.AmbientTexture <> nil) or
        (SurfaceShader.EmissiveTexture <> nil) or
        (SurfaceShader.NormalTexture <> nil) or
        (SurfaceShader.ShininessTexture <> nil) or
        (SurfaceShader.SpecularTexture <> nil);
    end else
    if Shape.Node.Appearance <> nil then
    begin
      Result := Shape.Node.Appearance.NormalMap <> nil;
    end;
  end;
end;

procedure TGLRenderer.BumpMappingEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure Enable(const NormalTexture: TAbstractTextureNode;
    NormalTextureCoordinatesId: Integer;
    const ParallaxHeight: Single);
  var
    GLNormalTexture: TGLTextureId;
    { Does NormalTexture have alpha channel. }
    HasAlpha: boolean;
  begin
    if ShapeBumpMappingAllowed and
       (BumpMapping <> bmNone) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLNormalTexture := GLTexture2D(NormalTexture, HasAlpha);
      if GLNormalTexture <> 0 then
      begin
        // fix NormalTextureCoordinatesId if needed
        NormalTextureCoordinatesId := Max(0, NormalTextureCoordinatesId);

        ShapeBumpMappingUsed := true;
        ShapeBumpMappingTextureCoordinatesId := NormalTextureCoordinatesId;

        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLNormalTexture);
        Shader.EnableBumpMapping(BumpMapping, ABoundTextureUnits,
          NormalTextureCoordinatesId, HasAlpha, ParallaxHeight);
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
        SurfaceShader.NormalTextureParallaxHeight);
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
