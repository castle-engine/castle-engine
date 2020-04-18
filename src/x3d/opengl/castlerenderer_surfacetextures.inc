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
    out TextureHasAlpha: boolean): TGLTextureId;
  function GLTexture2D(Node: TAbstractTextureNode): TGLTextureId;

  { If this State indicates a normal map, use it for bump mapping. }
  procedure BumpMappingEnable(State: TX3DGraphTraverseState;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { If this State indicates other surface texture, use it. }
  procedure SurfaceTexturesEnable(State: TX3DGraphTraverseState;
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
           (MatPhong.SpecularTexture <> nil) then
          Exit(true);
      end;

      if Shape.Node.Appearance.Material is TPhysicalMaterialNode then
      begin
        MatPhysical := TPhysicalMaterialNode(Shape.Node.Appearance.Material);
        if // (MatPhysical.BaseTexture <> nil) or // this is OK for Gouraud shading? Actually PhysicalMaterial always requires Phong shading anyway.
           (MatPhysical.MetallicRoughnessTexture <> nil) then
          Exit(true);
      end;
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
  Mat: TAbstractMaterialNode;
  MatOneSided: TAbstractOneSidedMaterialNode;
  Appearance: TAppearanceNode;
begin
  if State.ShapeNode <> nil then
  begin
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := State.ShapeNode.CommonSurfaceShader;
    {$warnings on}
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
      Mat := Appearance.Material;
      if (Mat is TAbstractOneSidedMaterialNode) and
         (TAbstractOneSidedMaterialNode(Mat).NormalTexture <> nil) then
      begin
        MatOneSided := TAbstractOneSidedMaterialNode(Mat);
        Enable(MatOneSided.NormalTexture, MatOneSided.NormalTextureChannel, 0.0);
      end else
      begin
        Enable(Appearance.NormalMap, 0, Appearance.HeightMapScale);
      end;
    end;
  end;
end;

procedure TGLRenderer.SurfaceTexturesEnable(State: TX3DGraphTraverseState;
  var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  procedure RequireTexCoord(const TextureCoordinatesId: Integer);
  begin
    { Note that there is no need to increase the TexCoordNeeded unconditionally.
      If the NormalTextureCoordinatesId = 0 and
      TexCoordNeeded = 1 then we're already satisfied, as then the normalmap
      will use the existing texture coordinates from e.g. diffuse texture. }

    MaxVar(ATexCoordsNeeded, TextureCoordinatesId + 1);
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
    const TextureChannel: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stEmissive,
      Texture,
      TextureChannel,
      'castle_SurfaceTexture_emissive',
      'void PLUG_material_emissive(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ';' + NL +
      '}');
  end;

  procedure AmbientTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureChannel: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stAmbient,
      Texture,
      TextureChannel,
      'castle_SurfaceTexture_ambient',
      'void PLUG_material_ambient(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ';' + NL +
      '}');
  end;

  procedure SpecularTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureChannel: Integer;
    const ChannelMask: String = 'rgb');
  begin
    SurfaceTextureEnable(stSpecular,
      Texture,
      TextureChannel,
      'castle_SurfaceTexture_specular',
      'void PLUG_material_specular(inout vec3 parameter)' + NL +
      '{' + NL +
      '  parameter *= texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ';' + NL +
      '}');
  end;

  procedure ShininessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureChannel: Integer;
    const ChannelMask: String = 'r');
  begin
    SurfaceTextureEnable(stShininess,
      Texture,
      TextureChannel,
      'castle_SurfaceTexture_shininess',
      'void PLUG_material_shininess(inout float parameter)' + NL +
      '{' + NL +
      '  parameter *= texture2D({uniform_texture_name}, {coord_name}.st).' + ChannelMask + ';' + NL +
      '}');
  end;

  procedure MetallicRoughnessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureChannel: Integer);
  begin
    SurfaceTextureEnable(stMetallicRoughness,
      Texture,
      TextureChannel,
      'castle_SurfaceTexture_metallic_roughness',
      'void PLUG_material_metallic_roughness(inout float metallic, inout float roughness)' + NL +
      '{' + NL +
      { Roughness is stored in the 'g' channel, metallic is stored in the 'b' channel.
        Read it to place metallic is x, roughness in y. }
      '  vec2 sample = texture2D({uniform_texture_name}, {coord_name}.st).bg;' + NL +
      '  metallic *= sample.x;' + NL +
      '  roughness *= sample.y;' + NL +
      '}');
  end;

var
  SurfaceShader: TCommonSurfaceShaderNode;
  MatPhong: TMaterialNode;
  MatPhysical: TPhysicalMaterialNode;
  MatUnlit: TUnlitMaterialNode;
begin
  if State.ShapeNode <> nil then
  begin
    {$warnings off} // using deprecated to keep backward compatibility
    SurfaceShader := State.ShapeNode.CommonSurfaceShader;
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
      if State.ShapeNode.Material is TMaterialNode then
      begin
        MatPhong := TMaterialNode(State.ShapeNode.Material);
        EmissiveTextureEnable(
          MatPhong.EmissiveTexture,
          MatPhong.EmissiveTextureChannel);
        AmbientTextureEnable(
          MatPhong.AmbientTexture,
          MatPhong.AmbientTextureChannel);
        SpecularTextureEnable(
          MatPhong.SpecularTexture,
          MatPhong.SpecularTextureChannel);
        ShininessTextureEnable(
          MatPhong.ShininessTexture,
          MatPhong.ShininessTextureChannel);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatPhong.DiffuseTexture <> nil then
          RequireTexCoord(MatPhong.DiffuseTextureChannel);
      end else
      if State.ShapeNode.Material is TPhysicalMaterialNode then
      begin
        MatPhysical := TPhysicalMaterialNode(State.ShapeNode.Material);
        EmissiveTextureEnable(
          MatPhysical.EmissiveTexture,
          MatPhysical.EmissiveTextureChannel);
        MetallicRoughnessTextureEnable(
          MatPhysical.MetallicRoughnessTexture,
          MatPhysical.MetallicRoughnessTextureChannel);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatPhysical.BaseTexture <> nil then
          RequireTexCoord(MatPhysical.BaseTextureChannel);
      end else
      if State.ShapeNode.Material is TPhysicalMaterialNode then
      begin
        MatUnlit := TUnlitMaterialNode(State.ShapeNode.Material);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatUnlit.EmissiveTexture <> nil then
          RequireTexCoord(MatUnlit.EmissiveTextureChannel);
      end;
    end;
  end;
end;

{$endif read_implementation}
