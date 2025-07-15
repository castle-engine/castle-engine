{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2025 Michalis Kamburelis.

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
    const RenderOptions: TCastleRenderOptions;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { If this Shape uses other surface texture, use it. }
  procedure SurfaceTexturesEnable(const Shape: TX3DRendererShape;
    var ABoundTextureUnits, ATexCoordsNeeded: Cardinal; Shader: TShader);

  { Does this shape possibly use some texture that requires Phong shading.
    Decide before actually making any OpenGL operations. }
  class function ShapeTextureForcesPhongShading(const Shape: TX3DRendererShape): boolean;

{$endif read_interface}

{$ifdef read_implementation}

function TRenderer.GLTexture2D(Node: TAbstractTextureNode;
  out TextureHasAlpha: boolean): TGLTextureId;
var
  TextureRes: TTextureResource;
begin
  Result := GLObjectNone;
  if Node <> nil then
  begin
    TextureRes := TTextureResources.Get(Node);
    if TextureRes is T2DTextureResource then // also checks is TextureRes <> nil
    begin
      Result := T2DTextureResource(TextureRes).GLName;
      TextureHasAlpha := Node.AlphaChannelFinal <> acNone;
    end;
  end;
end;

function TRenderer.GLTexture2D(Node: TAbstractTextureNode): TGLTextureId;
var
  IgnoreTextureHasAlpha: boolean;
begin
  Result := GLTexture2D(Node, IgnoreTextureHasAlpha);
end;

class function TRenderer.ShapeTextureForcesPhongShading(const Shape: TX3DRendererShape): boolean;
var
  SurfaceShader: TCommonSurfaceShaderNode;
  MatOne: TAbstractOneSidedMaterialNode;
  MatPhong: TMaterialNode;
  MatPhysical: TPhysicalMaterialNode;
  App: TAppearanceNode;
begin
  Result := false;
  if Shape.Node <> nil then
  begin
    App := Shape.Node.Appearance;
    if App <> nil then
    begin
      SurfaceShader := App.InternalCommonSurfaceShader;
      if SurfaceShader <> nil then
      begin
        Result :=
          (SurfaceShader.AmbientTexture <> nil) or
          (SurfaceShader.EmissiveTexture <> nil) or
          (SurfaceShader.NormalTexture <> nil) or
          (SurfaceShader.ShininessTexture <> nil) or
          (SurfaceShader.SpecularTexture <> nil);
      end else
      if App <> nil then
      begin
        if App.NormalMap <> nil then
          Exit(true);

        if App.Material is TAbstractOneSidedMaterialNode then
        begin
          MatOne := TAbstractOneSidedMaterialNode(App.Material);
          if (MatOne.EmissiveTexture <> nil) or
            (MatOne.NormalTexture <> nil) then
            Exit(true);
        end;

        if App.Material is TMaterialNode then
        begin
          MatPhong := TMaterialNode(App.Material);
          if (MatPhong.AmbientTexture <> nil) or
            // (MatPhong.DiffuseTexture <> nil) or // this is OK for Gouraud shading
            (MatPhong.ShininessTexture <> nil) or
            (MatPhong.SpecularTexture <> nil) or
            (MatPhong.OcclusionTexture <> nil) then
            Exit(true);
        end;

        if App.Material is TPhysicalMaterialNode then
        begin
          MatPhysical := TPhysicalMaterialNode(App.Material);
          if // (MatPhysical.BaseTexture <> nil) or // this is OK for Gouraud shading? Actually PhysicalMaterial always requires Phong shading anyway.
            (MatPhysical.MetallicRoughnessTexture <> nil) or
            (MatPhysical.OcclusionTexture <> nil) then
            Exit(true);
        end;
      end;
    end;
  end;
end;

procedure TRenderer.BumpMappingEnable(const Shape: TX3DRendererShape;
  const RenderOptions: TCastleRenderOptions;
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
       (BumpMapping(RenderOptions) <> bmNone) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLNormalTexture := GLTexture2D(NormalTexture, HasAlpha);
      if GLNormalTexture <> GLObjectNone then
      begin
        // fix NormalTextureCoordinatesId if needed
        NormalTextureCoordinatesId := Max(0, NormalTextureCoordinatesId);

        Shape.BumpMappingUsed := true;
        Shape.BumpMappingTextureCoordinatesId := NormalTextureCoordinatesId;

        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLNormalTexture);
        Shader.EnableBumpMapping(BumpMapping(RenderOptions), ABoundTextureUnits,
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
  Appearance := Shape.State.Appearance;
  if Appearance <> nil then
  begin
    SurfaceShader := Appearance.InternalCommonSurfaceShader;
    if SurfaceShader <> nil then
    begin
      Enable(
        SurfaceShader.NormalTexture,
        SurfaceShader.NormalTextureCoordinatesId,
        1,
        SurfaceShader.NormalTextureParallaxHeight);
    end else
    begin
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

procedure TRenderer.SurfaceTexturesEnable(const Shape: TX3DRendererShape;
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

  { Enable a texture affecting specific material parameter.
    We call it "surface texture" for consistency with CommonSurfaceShader
    X3DOM node that originally introduced this idea to X3D. }
  procedure SurfaceTextureEnable(const SurfaceTexture: TSurfaceTexture;
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer;
    const UniformTextureName: String);
  var
    GLTextureId: TGLTextureId;
  begin
    if (Texture <> nil) and
       (ABoundTextureUnits < GLFeatures.MaxTextureUnits) then
    begin
      GLTextureId := GLTexture2D(Texture);
      if GLTextureId <> GLObjectNone then
      begin
        ActiveTexture(ABoundTextureUnits);
        glBindTexture(GL_TEXTURE_2D, GLTextureId);
        Shader.EnableSurfaceTexture(SurfaceTexture, ABoundTextureUnits,
          Max(0, TextureCoordinatesId),
          UniformTextureName);
        Inc(ABoundTextureUnits);

        RequireTexCoord(TextureCoordinatesId);
      end;
    end;
  end;

  procedure EmissiveTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stEmissive,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_emissive');
  end;

  procedure AmbientTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stAmbient,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_ambient');
  end;

  procedure SpecularTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stSpecular,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_specular');
  end;

  procedure ShininessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stShininess,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_shininess');
  end;

  procedure MetallicRoughnessTextureEnable(
    const Texture: TAbstractTextureNode;
    const TextureCoordinatesId: Integer);
  begin
    SurfaceTextureEnable(stMetallicRoughness,
      Texture,
      TextureCoordinatesId,
      'castle_SurfaceTexture_metallic_roughness');
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
      'castle_SurfaceTexture_occlusion');

    { This procedure is called even with Texture = nil.
      In this case SurfaceTextureEnable, does not actually enable the shader code,
      so we also don't need to add this uniform.
      When Texture <> nil, this uniform is used by PlugCode[stOcclusion]
      in TSurfaceTextureShader.GenerateCode. }
    if Texture <> nil then
    begin
      USingle := TDynamicUniformSingle.Create;
      USingle.Name := 'castle_MaterialOcclusionStrength';
      USingle.Value := Strength;
      Shader.DynamicUniforms.Add(USingle);
    end;
  end;

var
  App: TAppearanceNode;
  SurfaceShader: TCommonSurfaceShaderNode;
  Material: TAbstractMaterialNode;
  MatPhong: TMaterialNode;
  MatPhysical: TPhysicalMaterialNode;
  MatUnlit: TUnlitMaterialNode;
begin
  App := Shape.State.Appearance;
  if App <> nil then
  begin
    SurfaceShader := App.InternalCommonSurfaceShader;
    if SurfaceShader <> nil then
    begin
      { Do some additional checks and warnings for TCommonSurfaceShaderNode.
        We don't care about efficiency of deprecated TCommonSurfaceShaderNode,
        so we can do these checks without worry.

        We disabled handling of non-standard channel masks, as they didn't seem
        to have much use, and would complicate implemetation of XxxTextureEnable
        (used also by X3D 4 material nodes, which are much more common case).
        They required to construct ShaderCode at runtime,
        and it should affect the hash (or at least ChannelMask
        should be part of the hash) to avoid shaders with different masks
        having same hash.

        So not implementing non-standard channel masks seems like a good idea,
        to gain code simplicity and efficiency for the non-deprecated and
        common code path of X3D 4 nodes. }

      if (SurfaceShader.EmissiveTexture <> nil) and
         (SurfaceShader.EmissiveTextureChannelMask <> 'rgb') then
        WritelnWarning('CommonSurfaceShader', 'emissiveTextureChannelMask <> "rgb" not implemented');
      if (SurfaceShader.AmbientTexture <> nil) and
         (SurfaceShader.AmbientTextureChannelMask <> 'rgb') then
        WritelnWarning('CommonSurfaceShader', 'ambientTextureChannelMask <> "rgb" not implemented');
      if (SurfaceShader.SpecularTexture <> nil) and
         (SurfaceShader.SpecularTextureChannelMask <> 'rgb') then
        WritelnWarning('CommonSurfaceShader', 'specularTextureChannelMask <> "rgb" not implemented');
      if (SurfaceShader.ShininessTexture <> nil) and
         (SurfaceShader.ShininessTextureChannelMask <> 'a') then
        WritelnWarning('CommonSurfaceShader', 'shininessTextureChannelMask <> "a" not implemented');

      EmissiveTextureEnable(
        SurfaceShader.EmissiveTexture,
        SurfaceShader.EmissiveTextureCoordinatesId);
      AmbientTextureEnable(
        SurfaceShader.AmbientTexture,
        SurfaceShader.AmbientTextureCoordinatesId);
      SpecularTextureEnable(
        SurfaceShader.SpecularTexture,
        SurfaceShader.SpecularTextureCoordinatesId);
      ShininessTextureEnable(
        SurfaceShader.ShininessTexture,
        SurfaceShader.ShininessTextureCoordinatesId);
      // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
      if SurfaceShader.DiffuseTexture <> nil then
        RequireTexCoord(SurfaceShader.DiffuseTextureCoordinatesId);
      if SurfaceShader.MultiDiffuseAlphaTexture <> nil then
        RequireTexCoord(SurfaceShader.DiffuseTextureCoordinatesId); // TODO test X3DOM/InstantReality are they using DiffuseTextureCoordinatesId in this case
    end else
    begin
      Material := App.Material;
      if Material is TMaterialNode then // also checks Material <> nil
      begin
        MatPhong := TMaterialNode(Material);
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
      if Material is TPhysicalMaterialNode then // also checks Material <> nil
      begin
        MatPhysical := TPhysicalMaterialNode(Material);
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
      if Material is TUnlitMaterialNode then  // also checks Material <> nil
      begin
        MatUnlit := TUnlitMaterialNode(Material);
        // These textures will be rendered using the MainTexture mechanism, but they still need tex coords
        if MatUnlit.EmissiveTexture <> nil then
          RequireTexCoord(MappingToIndex(MatUnlit.EmissiveTextureMapping));
      end;
    end;
  end;
end;

{$endif read_implementation}
