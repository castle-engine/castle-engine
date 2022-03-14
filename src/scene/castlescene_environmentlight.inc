{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Environment light. }

{$ifdef read_interface}

type
  { Environment light in an environment (like a sky) defined as a cubemap that shines on everything.
    It is like a sky that lights everything.
    It is sometimes called image-based lighting.

    See @url(https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/lighting.html#EnvironmentLight
    X3D EnvironmentLight specification) for various details. }
  TCastleEnvironmentLight = class(TCastleAbstractLight)
  strict private
    { Same thing as FLightNode, but with proper type. }
    FEnvironmentLight: TEnvironmentLightNode;

    FDiffuseTextureCubemap: String;
    FDiffuseTextureNegativeZ: String;
    FDiffuseTexturePositiveZ: String;
    FDiffuseTexturePositiveY: String;
    FDiffuseTextureNegativeY: String;
    FDiffuseTextureNegativeX: String;
    FDiffuseTexturePositiveX: String;
    FSpecularTextureCubemap: String;
    FSpecularTextureNegativeZ: String;
    FSpecularTexturePositiveZ: String;
    FSpecularTexturePositiveY: String;
    FSpecularTextureNegativeY: String;
    FSpecularTextureNegativeX: String;
    FSpecularTexturePositiveX: String;

    procedure SetDiffuseTextureCubemap(const Value: String);
    procedure SetDiffuseTextureNegativeZ(const Value: String);
    procedure SetDiffuseTexturePositiveZ(const Value: String);
    procedure SetDiffuseTexturePositiveY(const Value: String);
    procedure SetDiffuseTextureNegativeY(const Value: String);
    procedure SetDiffuseTextureNegativeX(const Value: String);
    procedure SetDiffuseTexturePositiveX(const Value: String);
    procedure SetSpecularTextureCubemap(const Value: String);
    procedure SetSpecularTextureNegativeZ(const Value: String);
    procedure SetSpecularTexturePositiveZ(const Value: String);
    procedure SetSpecularTexturePositiveY(const Value: String);
    procedure SetSpecularTextureNegativeY(const Value: String);
    procedure SetSpecularTextureNegativeX(const Value: String);
    procedure SetSpecularTexturePositiveX(const Value: String);
    procedure UpdateTextures;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Diffuse texture defined as a cubemap.
      Use DDS cubemap texture here.
      If you set this, the other parameters to define diffuse cubemap as 6 separate images
      are ignored.
      TODO: Cubemaps in KTX should also be supported soon. }
    property DiffuseTextureCubemap: String read FDiffuseTextureCubemap write SetDiffuseTextureCubemap;

    { Diffuse -Z texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTextureNegativeZ: String read FDiffuseTextureNegativeZ write SetDiffuseTextureNegativeZ;

    { Diffuse +Z texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTexturePositiveZ: String read FDiffuseTexturePositiveZ write SetDiffuseTexturePositiveZ;

    { Diffuse +Y texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTexturePositiveY: String read FDiffuseTexturePositiveY write SetDiffuseTexturePositiveY;

    { Diffuse -Y texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTextureNegativeY: String read FDiffuseTextureNegativeY write SetDiffuseTextureNegativeY;

    { Diffuse -X texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTextureNegativeX: String read FDiffuseTextureNegativeX write SetDiffuseTextureNegativeX;

    { Diffuse +X texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to DiffuseTextureCubemap. }
    property DiffuseTexturePositiveX: String read FDiffuseTexturePositiveX write SetDiffuseTexturePositiveX;

    { Specular texture defined as a cubemap.
      Use DDS cubemap texture here.
      If you set this, the other parameters to define diffuse cubemap as 6 separate images
      are ignored.
      TODO: Cubemaps in KTX should also be supported soon. }
    property SpecularTextureCubemap: String read FSpecularTextureCubemap write SetSpecularTextureCubemap;

    { Specular -Z texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTextureNegativeZ: String read FSpecularTextureNegativeZ write SetSpecularTextureNegativeZ;

    { Specular +Z texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTexturePositiveZ: String read FSpecularTexturePositiveZ write SetSpecularTexturePositiveZ;

    { Specular +Y texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTexturePositiveY: String read FSpecularTexturePositiveY write SetSpecularTexturePositiveY;

    { Specular -Y texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTextureNegativeY: String read FSpecularTextureNegativeY write SetSpecularTextureNegativeY;

    { Specular -X texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTextureNegativeX: String read FSpecularTextureNegativeX write SetSpecularTextureNegativeX;

    { Specular +X texture.
      All 6 texture sides must be assigned in order for them to work.
      Alternatively, you can assign a KTX / DDS textures to SpecularTextureCubemap. }
    property SpecularTexturePositiveX: String read FSpecularTexturePositiveX write SetSpecularTexturePositiveX;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TCastleEnvironmentLight.Create(AOwner: TComponent);
begin
  inherited;

  { internal stuff create }
  FEnvironmentLight := TEnvironmentLightNode.Create;
  FEnvironmentLight.Global := true;
  LightNode := FEnvironmentLight;
  GizmoTextureUrl := InternalCastleDesignData + 'gizmos/light/light_environment.png';
end;

function TCastleEnvironmentLight.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'DiffuseTextureCubemap') or
     (PropertyName = 'DiffuseTextureNegativeZ') or
     (PropertyName = 'DiffuseTexturePositiveZ') or
     (PropertyName = 'DiffuseTexturePositiveY') or
     (PropertyName = 'DiffuseTextureNegativeY') or
     (PropertyName = 'DiffuseTextureNegativeX') or
     (PropertyName = 'DiffuseTexturePositiveX') or
     (PropertyName = 'SpecularTextureCubemap') or
     (PropertyName = 'SpecularTextureNegativeZ') or
     (PropertyName = 'SpecularTexturePositiveZ') or
     (PropertyName = 'SpecularTexturePositiveY') or
     (PropertyName = 'SpecularTextureNegativeY') or
     (PropertyName = 'SpecularTextureNegativeX') or
     (PropertyName = 'SpecularTexturePositiveX') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleEnvironmentLight.SetDiffuseTextureCubemap(const Value: String);
begin
  if FDiffuseTextureCubemap <> Value then
  begin
    FDiffuseTextureCubemap := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTextureNegativeZ(const Value: String);
begin
  if FDiffuseTextureNegativeZ <> Value then
  begin
    FDiffuseTextureNegativeZ := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTexturePositiveZ(const Value: String);
begin
  if FDiffuseTexturePositiveZ <> Value then
  begin
    FDiffuseTexturePositiveZ := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTexturePositiveY(const Value: String);
begin
  if FDiffuseTexturePositiveY <> Value then
  begin
    FDiffuseTexturePositiveY := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTextureNegativeY(const Value: String);
begin
  if FDiffuseTextureNegativeY <> Value then
  begin
    FDiffuseTextureNegativeY := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTextureNegativeX(const Value: String);
begin
  if FDiffuseTextureNegativeX <> Value then
  begin
    FDiffuseTextureNegativeX := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetDiffuseTexturePositiveX(const Value: String);
begin
  if FDiffuseTexturePositiveX <> Value then
  begin
    FDiffuseTexturePositiveX := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTextureCubemap(const Value: String);
begin
  if FSpecularTextureCubemap <> Value then
  begin
    FSpecularTextureCubemap := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTextureNegativeZ(const Value: String);
begin
  if FSpecularTextureNegativeZ <> Value then
  begin
    FSpecularTextureNegativeZ := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTexturePositiveZ(const Value: String);
begin
  if FSpecularTexturePositiveZ <> Value then
  begin
    FSpecularTexturePositiveZ := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTexturePositiveY(const Value: String);
begin
  if FSpecularTexturePositiveY <> Value then
  begin
    FSpecularTexturePositiveY := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTextureNegativeY(const Value: String);
begin
  if FSpecularTextureNegativeY <> Value then
  begin
    FSpecularTextureNegativeY := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTextureNegativeX(const Value: String);
begin
  if FSpecularTextureNegativeX <> Value then
  begin
    FSpecularTextureNegativeX := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.SetSpecularTexturePositiveX(const Value: String);
begin
  if FSpecularTexturePositiveX <> Value then
  begin
    FSpecularTexturePositiveX := Value;
    UpdateTextures;
  end;
end;

procedure TCastleEnvironmentLight.UpdateTextures;
begin
  // TODO
end;

{$endif read_implementation}
