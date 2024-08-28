{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generating shader code for bump mapping. }

{$ifdef read_interface}

//type
  { Generating shader code for bump mapping. }
  TBumpMappingShader = record
  strict private
    { Set by GenerateCode, so can be used by SetUniformsOnce,
      but *not* by SetDynamicUniforms. }
    HasBumpMappingUniform_NormalMapTextureUnit: Boolean;
    BumpMappingUniform_NormalMapTextureUnit: Integer;
    HasBumpMappingUniform_NormalMapScale: Boolean;
    BumpMappingUniform_NormalMapScale: Single;
    HasBumpMappingUniform_HeightMapScale: Boolean;
    BumpMappingUniform_HeightMapScale: Single;
  public
    BumpMapping: TBumpMapping;

    NormalMapTextureCoordinatesId: Cardinal;
    NormalMapTextureUnit: Cardinal;
    NormalMapScale: Single;

    HeightMapInAlpha: Boolean;
    HeightMapScale: Single;

    procedure Clear;
    procedure PrepareHash(var Hash: TShaderCodeHash);
    procedure GenerateCode(const Shader: TShader);
    procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TBumpMappingShader.Clear;
begin
  BumpMapping := Low(TBumpMapping);
  NormalMapTextureUnit := 0;
  NormalMapTextureCoordinatesId := 0;
  NormalMapScale := 1;
  HeightMapInAlpha := false;
  HeightMapScale := 0;
end;

procedure TBumpMappingShader.PrepareHash(var Hash: TShaderCodeHash);
begin
  if BumpMapping <> bmNone then
  begin
    Hash.AddInteger(
      47 * Ord(BumpMapping) +
      373 * NormalMapTextureUnit +
      379 * NormalMapTextureCoordinatesId +
      383 * Ord(HeightMapInAlpha)
    );
    Hash.AddFloat(HeightMapScale, 2203);
    Hash.AddFloat(NormalMapScale, 2281);
  end;
end;

procedure TBumpMappingShader.GenerateCode(const Shader: TShader);
var
  VertexShader, FragmentShader: String;
begin
  HasBumpMappingUniform_NormalMapTextureUnit := false;
  HasBumpMappingUniform_NormalMapScale := false;
  HasBumpMappingUniform_HeightMapScale := false;

  if BumpMapping = bmNone then Exit;

  // add basic bump mapping code
  VertexShader   := {$I bump_mapping.vs.inc};
  FragmentShader := {$I bump_mapping.fs.inc};
  StringReplaceAllVar(FragmentShader, '<NormalMapTextureCoordinatesId>',
    IntToStr(NormalMapTextureCoordinatesId));
  Shader.Plug(stVertex  , VertexShader);
  Shader.Plug(stFragment, FragmentShader);

  HasBumpMappingUniform_NormalMapTextureUnit := true;
  BumpMappingUniform_NormalMapTextureUnit := NormalMapTextureUnit;

  HasBumpMappingUniform_NormalMapScale := true;
  BumpMappingUniform_NormalMapScale := NormalMapScale;

  if HeightMapInAlpha and (BumpMapping >= bmParallax) then
  begin
    // add parallax bump mapping code, with a variant for "steep parallax"
    VertexShader   := {$I bump_mapping_parallax.vs.inc};
    FragmentShader := {$I bump_mapping_parallax.fs.inc};
    if BumpMapping >= bmSteepParallax then
      FragmentShader := '#define CASTLE_BUMP_MAPPING_PARALLAX_STEEP' + NL
        + FragmentShader;
    Shader.Plug(stVertex  , VertexShader);
    Shader.Plug(stFragment, FragmentShader);

    HasBumpMappingUniform_HeightMapScale := true;
    BumpMappingUniform_HeightMapScale := HeightMapScale;

    if (BumpMapping >= bmSteepParallaxShadowing) and (Shader.LightShaders.Count > 0) then
    begin
      // add steep parallax with self-shadowing bump mapping code
      VertexShader   := {$I bump_mapping_steep_parallax_shadowing.vs.inc};
      FragmentShader := {$I bump_mapping_steep_parallax_shadowing.fs.inc};
      if Shader.LightShaders[0].Node is TAbstractPositionalLightNode then
        VertexShader := '#define CASTLE_LIGHT0_POSITIONAL' + NL
          + VertexShader;
      Shader.Plug(stVertex  , VertexShader);
      Shader.Plug(stFragment, FragmentShader);
    end;
  end;
end;

procedure TBumpMappingShader.SetUniformsOnce(const AProgram: TX3DShaderProgram);
begin
  if HasBumpMappingUniform_NormalMapTextureUnit then
    AProgram.SetUniform('castle_normal_map',
      BumpMappingUniform_NormalMapTextureUnit);

  if HasBumpMappingUniform_NormalMapScale then
    AProgram.SetUniform('castle_normalScale',
      BumpMappingUniform_NormalMapScale);

  if HasBumpMappingUniform_HeightMapScale then
    AProgram.SetUniform('castle_parallax_bm_scale',
      BumpMappingUniform_HeightMapScale);
end;

{$endif read_implementation}
