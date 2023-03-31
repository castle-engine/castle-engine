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

{ Textures that affect surface parameters beyond standard MainTexture
  (diffuse for Phong, emissive for Unlit, base for PhysicalMaterial).
  Like normal maps, and various other
  textures defined by the CommonSurfaceShader and materials in X3D v4. }

{$ifdef read_interface}

private
  { Return OpenGL 2D texture initialized for given texture node.
    If Node is nil, or it's corresponding OpenGL resource is not initialized
    for whatever reason --- returns 0. }
  function GLTexture2D(Node: TAbstractTextureNode;
    out TextureHasAlpha: boolean): TGLTextureId; overload;
  function GLTexture2D(Node: TAbstractTextureNode): TGLTextureId; overload;

  { If this Shape uses a normal map, use it for bump mapping. }
  procedure BumpMappingEnable(const Shape: TX3DRendererShape;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { If this Shape uses other surface texture, use it. }
  procedure SurfaceTexturesEnable(const Shape: TX3DRendererShape;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { Does this shape possibly use some texture that requires Phong shading.
    Decide before actually making any OpenGL operations. }
  class function ShapeMaybeUsesSurfaceTexture(const Shape: TX3DRendererShape): boolean;

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

class function TGLRenderer.ShapeMaybeUsesSurfaceTexture(const Shape: TX3DRendererShape): boolean;
var
  SurfaceShader: TCommonSurfaceShaderNode;
  MatOne: TAbstractOneSidedMaterialNode;
  MatPhong: TMaterialNode;
  MatPhysical: TPhysicalMaterialNode;
begin
  Result := false;
  if Shape.Node <> nil then
  begin
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := Shape.Node.CommonSurfaceShader;
    {$warnings on}
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
      if Shape.Node.Appearance.NormalMap <> nil then
        Exit(true);

      if Shape.Node.Appearance.Material is TAbstractOneSidedMaterialNode then
      begin
        MatOne := TAbstractOneSidedMaterialNode(Shape.Node.Appearance.Material);
        if (MatOne.EmissiveTexture <> nil) or
           (MatOne.NormalTexture <> nil) then
          Exit(true);
      end;

      if Shape.Node.Appearance.Material is TMaterialNode then
      begin
        MatPhong := TMaterialNode(Shape.Node.Appearance.Material);
        if (MatPhong.AmbientTexture <> nil) or
           // (MatPhong.DiffuseTexture <> nil) or // this is OK for Gouraud shading
           (MatPhong.ShininessTexture <> nil) or
           (MatPhong.SpecularTexture <> nil) or
           (MatPhong.OcclusionTexture <> nil) then
          Exit(true);
      end;

      if Shape.Node.Appearance.Material is TPhysicalMaterialNode then
      begin
        MatPhysical := TPhysicalMaterialNode(Shape.Node.Appearance.Material);
        if // (MatPhysical.BaseTexture <> nil) or // this is OK for Gouraud shading? Actually PhysicalMaterial always requires Phong shading anyway.
           (MatPhysical.MetallicRoughnessTexture <> nil) or
           (MatPhysical.OcclusionTexture <> nil) then
          Exit(true);
      end;
    end;
  end;
end;

procedure TGLRenderer.BumpMappingEnable(const Shape: TX3DRendererShape;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure Enable(const NormalTexture: TAbstractTextureNode;
    NormalTextureCoordinatesId: Integer;
    const NormalMapScale, ParallaxHeight: Single);
  var
    GLNormalTexture: TGLTextureId;
    { Does NormalTexture have alpha channel. }
    HasAlpha: boolean;
  begin
    if Shape.BumpMappingAllowed and
       (BumpMapping <> bmNone) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLNormalTexture := GLTexture2D(NormalTexture, HasAlpha);
      if GLNormalTexture <> 0 then
      begin
        // fix NormalTextureCoordinatesId if needed
        NormalTextureCoordinatesId := Max(0, NormalTextureCoordinatesId);

        Shape.BumpMappingUsed := true;
        Shape.BumpMappingTextureCoordinatesId := NormalTextureCoordinatesId;

        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLNormalTexture);
        Shader.EnableBumpMapping(BumpMapping, ABoundTextureUnits,
          NormalTextureCoordinatesId, NormalMapScale, HasAlpha, ParallaxHeight);
        Inc(ABoundTextureUnits);

        { There is no need to increase the ATexCoordsNeeded unconditionally.
          If the NormalTextureCoordinatesId = 0 and
          ATexCoordsNeeded = 1 then we're already satisfied, as then the normalmap
          will use the existing texture coordinates from e.g. diffuse texture. }
        MaxVar(ATexCoordsNeeded, NormalTextureCoordinatesId + 1);
      end;
    end;
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
  Mat: TAbstractMaterialNode;
  MatOneSided: TAbstractOneSidedMaterialNode;
  Appearance: TAppearanceNode;
  TexCoordIndex: Integer;
begin
  if Shape.State.ShapeNode <> nil then
  begin
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := Shape.State.ShapeNode.CommonSurfaceShader;
    {$warnings on}
    if SurfaceShader <> nil then
    begin
      Enable(
        SurfaceShader.NormalTexture,
        SurfaceShader.NormalTextureCoordinatesId,
        1,
        SurfaceShader.NormalTextureParallaxHeight);
    end else
    if Shape.State.ShapeNode.Appearance <> nil then
    begin
      Appearance := Shape.State.ShapeNode.Appearance;
      Mat := Appearance.Material;
      if (Mat is TAbstractOneSidedMaterialNode) and
         (TAbstractOneSidedMaterialNode(Mat).NormalTexture <> nil) then
      begin
        MatOneSided := TAbstractOneSidedMaterialNode(Mat);
        Shape.Geometry.FindTextureMapping(MatOneSided.NormalTextureMapping, TexCoordIndex, true);
        Enable(MatOneSided.NormalTexture, TexCoordIndex, MatOneSided.NormalScale, RenderOptions.BumpMappingParallaxHeight);
      end else
      begin
        Enable(Appearance.NormalMap, 0, 1, Appearance.HeightMapScale);
      end;
    end;
  end;
end;

procedure TGLRenderer.SurfaceTexturesEnable(const Shape: TX3DRendererShape;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure RequireTexCoord(const TextureCoordinatesId: Integer);
  begin
    { Note that there is no need to increase the ATexCoordsNeeded unconditionally.
      If the NormalTextureCoordinatesId = 0 and
      ATexCoordsNeeded = 1 then we're already satisfied, as then the normalmap
      will use the existing texture coordinates from e.g. diffuse texture. }

    MaxVar(ATexCoordsNeeded, TextureCoordinatesId + 1);
  end;

  function MappingToIndex(const Mapping: String): Integer;
  begin
    Shape.Geometry.FindTextureMapping(Mapping, Result, true);
  end;

  procedure SurfaceTextureEnable(const SurfaceTexture: TSurfaceTexture;
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const UniformTextureName, PlugCode: String);
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
          Max(0, TextureCoordinatesId),
          UniformTextureName,
          PlugCode);
        Inc(ABoundTextureUnits);

        RequireTexCoord(TextureCoordinatesId);
      end;
    end;
  end;

  procedure EmissiveTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stEmissive,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_emissive',
      'void PLUG_material_emissive(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= castle_texture_color_to_linear(texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ');' + NL +
      '}');
  end;

  procedure AmbientTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stAmbient,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_ambient',
      'void PLUG_material_ambient(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= castle_texture_color_to_linear(texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ');' + NL +
      '}');
  end;

  procedure SpecularTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stSpecular,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_specular',
      'void PLUG_material_specular(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= castle_texture_color_to_linear(texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ');' + NL +
      '}');
  end;

  procedure ShininessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const ChannelMask: String = 'a');
  begin
    SurfaceTextureEnable(stShininess,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_shininess',
      'void PLUG_material_shininess(inout float parameter)' + NL +
      '{' + NL +
      '  parameter *= texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ';' + NL +
      '}');
  end;

  procedure MetallicRoughnessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stMetallicRoughness,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_metallic_roughness',
      'void PLUG_material_metallic_roughness(inout float metallic, inout float roughness)' + NL +
      '{' + NL +
      { Roughness is stored in the 'g' channel, metallic is stored in the 'b' channel.
        Read it to place metallic is x, roughness in y. }
      '  vec2 mr = texture2D({uniform_texture_name}, {coord_name}.st).bg;' + NL +
      '  metallic *= mr.x;' + NL +
      '  roughness *= mr.y;' + NL +
      '}');
  end;

  procedure OcclusionTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const Strength: Single);
  var
    USingle: TDynamicUniformSingle;
  begin
    SurfaceTextureEnable(stOcclusion,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_occlusion',
      'uniform float castle_MaterialOcclusionStrength;' + NL +
      'void PLUG_material_occlusion(inout vec4 color)' + NL +
      '{' + NL +
      '  float occlusion = texture2D({uniform_texture_name}, {coord_name}.st).r;' + NL +
      '  color.rgb = mix(color.rgb, color.rgb * occlusion, castle_MaterialOcclusionStrength);' + NL +
      '}');

    { This procedure is called even with Texture = nil.
      In this case SurfaceTextureEnable, so we also don't need to add this uniform. }
    if Texture <> nil then
    begin
      USingle := TDynamicUniformSingle.Create;
      USingle.Name := 'castle_MaterialOcclusionStrength';
      USingle.Value := Strength;
      Shader.DynamicUniforms.Add(USingle);
    end;
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
  MatPhong: TMaterialNode;
  MatPhysical: TPhysicalMaterialNode;
  MatUnlit: TUnlitMaterialNode;
begin
  if Shape.State.ShapeNode <> nil then
  begin
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := Shape.State.ShapeNode.CommonSurfaceShader;
    {$warnings on}
    if SurfaceShader <> nil then
    begin
      EmissiveTextureEnable(
        SurfaceShader.EmissiveTexture,
        SurfaceShader.EmissiveTextureCoordinatesId,
        SurfaceShader.EmissiveTextureChannelMask);
      AmbientTextureEnable(
        SurfaceShader.AmbientTexture,
        SurfaceShader.AmbientTextureCoordinatesId,
        SurfaceShader.AmbientTextureChannelMask);
      SpecularTextureEnable(
        SurfaceShader.SpecularTexture,
        SurfaceShader.SpecularTextureCoordinatesId,
        SurfaceShader.SpecularTextureChannelMask);
      ShininessTextureEnable(
        SurfaceShader.ShininessTexture,
        SurfaceShader.ShininessTextureCoordinatesId,
        SurfaceShader.ShininessTextureChannelMask);
      // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
      if SurfaceShader.DiffuseTexture <> nil then
        RequireTexCoord(SurfaceShader.DiffuseTextureCoordinatesId);
      if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
        RequireTexCoord(SurfaceShader.DiffuseTextureCoordinatesId); // TODO test X3DOM/InstantReality are they using DiffuseTextureCoordinatesId in this case
    end else
    begin
      if Shape.State.ShapeNode.Material is TMaterialNode then
      begin
        MatPhong := TMaterialNode(Shape.State.ShapeNode.Material);
        EmissiveTextureEnable(
          MatPhong.EmissiveTexture,
          MappingToIndex(MatPhong.EmissiveTextureMapping));
        AmbientTextureEnable(
          MatPhong.AmbientTexture,
          MappingToIndex(MatPhong.AmbientTextureMapping));
        SpecularTextureEnable(
          MatPhong.SpecularTexture,
          MappingToIndex(MatPhong.SpecularTextureMapping));
        ShininessTextureEnable(
          MatPhong.ShininessTexture,
          MappingToIndex(MatPhong.ShininessTextureMapping));
        OcclusionTextureEnable(
          MatPhong.OcclusionTexture,
          MappingToIndex(MatPhong.OcclusionTextureMapping),
          MatPhong.OcclusionStrength);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatPhong.DiffuseTexture <> nil then
          RequireTexCoord(MappingToIndex(MatPhong.DiffuseTextureMapping));
      end else
      if Shape.State.ShapeNode.Material is TPhysicalMaterialNode then
      begin
        MatPhysical := TPhysicalMaterialNode(Shape.State.ShapeNode.Material);
        EmissiveTextureEnable(
          MatPhysical.EmissiveTexture,
          MappingToIndex(MatPhysical.EmissiveTextureMapping));
        MetallicRoughnessTextureEnable(
          MatPhysical.MetallicRoughnessTexture,
          MappingToIndex(MatPhysical.MetallicRoughnessTextureMapping));
        OcclusionTextureEnable(
          MatPhysical.OcclusionTexture,
          MappingToIndex(MatPhysical.OcclusionTextureMapping),
          MatPhysical.OcclusionStrength);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatPhysical.BaseTexture <> nil then
          RequireTexCoord(MappingToIndex(MatPhysical.BaseTextureMapping));
      end else
      if Shape.State.ShapeNode.Material is TPhysicalMaterialNode then
      begin
        MatUnlit := TUnlitMaterialNode(Shape.State.ShapeNode.Material);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatUnlit.EmissiveTexture <> nil then
          RequireTexCoord(MappingToIndex(MatUnlit.EmissiveTextureMapping));
      end;
    end;
  end;
end;

{$endif read_implementation}
