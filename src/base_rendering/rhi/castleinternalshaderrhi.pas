{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Upgrade extension for CastleRendererInternalShader to emit Vulkan GLSL 450
  (UBOs, Push Constants, explicit stage locations) alongside OpenGL/OpenGLES. }
unit CastleInternalShaderRHI;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, StrUtils, Math, Generics.Collections,
  CastleVectors, CastleColors, CastleStringUtils, CastleLog,
  CastleInternalRHI;

const
  { Maximum bone matrices supported in Set 2 skinning UBO }
  CASTLE_MAX_BONE_MATRICES = 256;
  { Maximum lights supported in Set 0 Binding 1 lights UBO }
  CASTLE_MAX_LIGHTS = 32;

type
  { Standard CGE Vertex Attribute Locations (strictly fixed across all shaders) }
  TCastleVertexLocation = (
    vlPosition    = 0,
    vlNormal      = 1,
    vlTexCoord0   = 2,
    vlTexCoord1   = 3,
    vlColor       = 4,
    vlTangent     = 5,
    vlJoints0     = 6,
    vlWeights0    = 7
  );

  { CPU-side mirror of Set 0, Binding 0 (Per-Pass / Global Scene UBO - std140) }
  PCastleSceneUniformBlock = ^TCastleSceneUniformBlock;
  TCastleSceneUniformBlock = packed record
    ProjectionMatrix: TMatrix4;
    ViewMatrix: TMatrix4;
    ViewMatrixInverse: TMatrix4;
    CameraPosition: TVector4;       // xyz = World Eye Pos, w = Scene Time
    FogColor: TVector4;             // rgb = Fog Color, a = Fog Density
    FogParams: TVector4;            // x = Start, y = End, z = Type, w = Reserved
    LightCount: Cardinal;
    Padding0, Padding1, Padding2: Cardinal;
  end;

  { CPU-side mirror of a single light entry in CastleLightsBlock (std140) }
  PCastleLightData = ^TCastleLightData;
  TCastleLightData = packed record
    Position: TVector4;         // xyz = eye-space position/direction, w = light type (0=dir, 1=point, 2=spot)
    Color: TVector4;            // rgb = color * intensity, w = radius
    SpotDirection: TVector4;    // xyz = eye-space direction, w = spotCosCutoff
    SpotParams: TVector4;       // x = spotCutoff (rad), y = beamWidth (rad), z = spotExponent, w = reserved
    Attenuation: TVector4;      // xyz = constant, linear, quadratic factors, w = reserved
    AmbientColor: TVector4;     // rgb = ambient color * ambient intensity, w = shadow map index / enabled (1.0)
    ProjectorMatrix: TMatrix4;  // 64 bytes: eye-to-light-clip-space projector matrix for shadow mapping
  end; // 6 * 16 + 64 = 160 bytes (160 mod 16 = 0)

  { CPU-side mirror of Set 0, Binding 1 (Multi-Light UBO - std140) }
  PCastleLightsUniformBlock = ^TCastleLightsUniformBlock;
  TCastleLightsUniformBlock = packed record
  private
    function AddLightInternal(const APos, AColor, ASpotDir, ASpotParams, AAtten, AAmbient: TVector4): Integer;
  public
    Count: Cardinal;                                                // 4 bytes
    Padding0, Padding1, Padding2: Cardinal;                         // 12 bytes
    Lights: array [0 .. CASTLE_MAX_LIGHTS - 1] of TCastleLightData; // 32 * 160 = 5120 bytes
    procedure Clear;
    function AddDirectionalLight(const Direction, Color, AmbientColor: TVector3): Integer;
    function AddPointLight(const Position, Color, AmbientColor: TVector3;
      const Attenuation: TVector3; const Radius: Single): Integer;
    function AddSpotLight(const Position, Direction, Color, AmbientColor: TVector3;
      const Attenuation: TVector3; const Radius, SpotCosCutoff, SpotCutoff, BeamWidth, SpotExponent: Single): Integer;
    procedure SetProjectorMatrix(const Index: Integer; const Matrix: TMatrix4;
      const AShadowSlotIndex: Integer = 0);
  end; // Total = 5136 bytes (5136 mod 16 = 0)

  { Shading model selector for RHI forward/bindless material pipelines }
  TCastleShadingModelRHI = (
    smPBRMetallicRoughness = 0,
    smPhong                = 1,
    smUnlit                = 2
  );

  { CPU-side mirror of Set 1, Binding 0 (Per-Material UBO - std140) }
  PCastleMaterialUniformBlock = ^TCastleMaterialUniformBlock;
  TCastleMaterialUniformBlock = packed record
    BaseColor: TVector4;            // PBR BaseColor / Phong DiffuseColor + Opacity
    SpecularColor: TVector4;        // rgb = Specular/F0, a = Shininess (or < 0 for Unlit)
    EmissiveColor: TVector4;        // rgb = Emissive, a = Metallic
    PBRParams: TVector4;            // x = Roughness, y = NormalScale, z = OcclusionStrength, w = AlphaCutoff
    procedure InitDefaultPBR;
    procedure InitDefaultPhong;
    procedure InitDefaultUnlit;
  end;

  { CPU-side mirror of a Bindless Material Entry in Set 1, Binding 1 (std430 SSBO / std140 UBO).
    Stores material factors alongside 32-bit bindless texture array indices into Set 1, Binding 0
    (castle_BindlessTextures[]), enabling multi-material vkCmdDrawIndexedIndirect batches. }
  PCastleBindlessMaterialEntry = ^TCastleBindlessMaterialEntry;
  TCastleBindlessMaterialEntry = packed record
    BaseColor: TVector4;            // 16 bytes
    SpecularColor: TVector4;        // 16 bytes
    EmissiveColor: TVector4;        // 16 bytes
    PBRParams: TVector4;            // 16 bytes
    BaseColorTexIndex: Cardinal;    // 4 bytes (index into castle_BindlessTextures[])
    NormalTexIndex: Cardinal;       // 4 bytes
    MetallicRoughnessTexIndex: Cardinal; // 4 bytes
    EmissiveTexIndex: Cardinal;     // 4 bytes
  end; // Total = 80 bytes (80 mod 16 = 0)

  { CPU-side mirror of Per-Draw Push Constants (128 bytes max, guaranteed by Vulkan spec) }
  PCastlePushConstantsBlock = ^TCastlePushConstantsBlock;
  TCastlePushConstantsBlock = packed record
    ModelViewMatrix: TMatrix4;      // 64 bytes
    NormalMatrixRow0: TVector4;     // 16 bytes (std140 padded 3x3 normal matrix)
    NormalMatrixRow1: TVector4;     // 16 bytes
    NormalMatrixRow2: TVector4;     // 16 bytes
    ObjectParams: TVector4;         // 16 bytes (x = MaterialIndex, y = BoneOffset, zw = Custom)
  end; // Total = 128 bytes

  { CPU-side mirror of Set 2, Binding 0 (Per-Object Skinning UBO - std140) }
  PCastleSkinningUniformBlock = ^TCastleSkinningUniformBlock;
  TCastleSkinningUniformBlock = packed record
    JointMatrices: array [0 .. CASTLE_MAX_BONE_MATRICES - 1] of TMatrix4; // 256 * 64 = 16384 bytes
    JointCount: Cardinal;
    Padding0, Padding1, Padding2: Cardinal;
  end;

  { CPU-side mirror of CastleScreenPushConstants (32 bytes) for post-processing screen effects }
  PCastleScreenPushConstants = ^TCastleScreenPushConstants;
  TCastleScreenPushConstants = packed record
    ScreenSize: TVector2;    // x = screen_width, y = screen_height (8 bytes)
    ScreenPlanes: TVector2;  // x = near plane, y = far plane (8 bytes)
    ScreenCustom: TVector4;  // custom effect parameters (16 bytes)
  end; // Total = 32 bytes

  { CPU-side mirror of Set 0, Binding 0 for GPU Compute Frustum/Distance/Hi-Z Culling (std140) }
  PCastleGpuCullingUniformBlock = ^TCastleGpuCullingUniformBlock;
  TCastleGpuCullingUniformBlock = packed record
    FrustumPlanes: array[0..5] of TVector4; // 6 planes (A, B, C, D), 96 bytes
    CameraPosition: TVector4;               // xyz = camera world pos, w = max distance (0 = disabled), 16 bytes
    HiZParams: TVector4;                    // x = width, y = height, z = mipCount, w = enableHiZ, 16 bytes
    InstanceCount: Cardinal;                // 4 bytes
    EnableFrustumCulling: Cardinal;         // 4 bytes (1 = enabled, 0 = disabled)
    EnableDistanceCulling: Cardinal;        // 4 bytes (1 = enabled, 0 = disabled)
    CompactDrawStream: Cardinal;            // 4 bytes (1 = compact visible draws via atomicAdd, 0 = 1:1 slots)
  end; // Total = 144 bytes (144 mod 16 = 0)

  { CPU-side mirror of a single instance in Set 0, Binding 1 Input SSBO for GPU Compute Culling (std430) }
  PCastleGpuCullingInstance = ^TCastleGpuCullingInstance;
  TCastleGpuCullingInstance = packed record
    BoundingSphere: TVector4; // xyz = world-space center, w = radius (16 bytes)
    AABBMin: TVector4;        // xyz = world-space AABB min, w = reserved (16 bytes)
    AABBMax: TVector4;        // xyz = world-space AABB max, w = reserved (16 bytes)
    IndexCount: Cardinal;     // 4 bytes (for VkDrawIndexedIndirectCommand)
    FirstIndex: Cardinal;     // 4 bytes
    VertexOffset: Integer;    // 4 bytes
    FirstInstance: Cardinal;  // 4 bytes
    InstanceCount: Cardinal;  // 4 bytes (typically 1)
    MaterialIndex: Cardinal;  // 4 bytes (bindless material index)
    Padding0: Cardinal;       // 4 bytes
    Padding1: Cardinal;       // 4 bytes
  end; // Total = 80 bytes (80 mod 16 = 0)

  { Push constants for Hi-Z depth pyramid mipmap reduction compute shader (16 bytes) }
  PCastleHiZPushConstants = ^TCastleHiZPushConstants;
  TCastleHiZPushConstants = packed record
    DstMipWidth: Cardinal;
    DstMipHeight: Cardinal;
    SrcMipWidth: Cardinal;
    SrcMipHeight: Cardinal;
  end;

const
  CASTLE_MAX_CLUSTERED_LIGHTS   = 256;
  CASTLE_DEFAULT_CLUSTER_X      = 16;
  CASTLE_DEFAULT_CLUSTER_Y      = 8;
  CASTLE_DEFAULT_CLUSTER_Z      = 24;
  CASTLE_MAX_LIGHTS_PER_CLUSTER = 64;
  CASTLE_MAX_SHADOW_CASCADES    = 4;
  CASTLE_MAX_SHADOW_ATLAS_TILES = 64;

type
  { Light type in Clustered Forward+ light buffer }
  TCastleClusterLightType = (
    cltDirectional = 0,
    cltPoint       = 1,
    cltSpot        = 2
  );

  { GPU std430 clustered light entry (64 bytes, 16-byte aligned) }
  PCastleClusteredLight = ^TCastleClusteredLight;
  TCastleClusteredLight = packed record
    PositionAndRange: TVector4;    // xyz = view-space position, w = attenuation range/radius (16 bytes)
    DirectionAndSpotCos: TVector4; // xyz = view-space direction, w = outer spot cos cutoff (16 bytes)
    ColorAndIntensity: TVector4;   // rgb = linear light color, w = intensity multiplier (16 bytes)
    ShadowAndTypeParams: TVector4; // x = Ord(LightType), y = inner spot cos, z = shadow tile index (-1 if none), w = shadow bias (16 bytes)
  end; // Total = 64 bytes

  { GPU std430 per-cluster light grid entry (8 bytes) }
  PCastleClusterGridEntry = ^TCastleClusterGridEntry;
  TCastleClusterGridEntry = packed record
    Offset: Cardinal; // Start offset in flat LightIndexList SSBO
    Count: Cardinal;  // Number of lights intersecting this cluster (0..CASTLE_MAX_LIGHTS_PER_CLUSTER)
  end;

  { GPU std140 uniform block for 3D Clustered Forward+ light culling compute shader (112 bytes) }
  PCastleClusterCullingUniformBlock = ^TCastleClusterCullingUniformBlock;
  TCastleClusterCullingUniformBlock = packed record
    InverseProjectionMatrix: TMatrix4; // 64 bytes
    ScreenAndDepthParams: TVector4;    // 16 bytes (x = ScreenWidth, y = ScreenHeight, z = ZNear, w = ZFar)
    GridX: Cardinal;                   // 4 bytes
    GridY: Cardinal;                   // 4 bytes
    GridZ: Cardinal;                   // 4 bytes
    ActiveLightCount: Cardinal;        // 4 bytes
    LogDepthParams: TVector4;          // 16 bytes (x = GridZ / Ln(ZFar/ZNear), y = -GridZ*Ln(ZNear)/Ln(ZFar/ZNear), z = MaxLightsPerCluster, w = 0)
  end; // Total = 112 bytes (112 mod 16 = 0)

  { GPU std140 uniform block for 4-cascade Directional Cascaded Shadow Maps (336 bytes) }
  PCastleDirectionalCascadeShadowBlock = ^TCastleDirectionalCascadeShadowBlock;
  TCastleDirectionalCascadeShadowBlock = packed record
    CascadeViewProjMatrix: array[0..CASTLE_MAX_SHADOW_CASCADES - 1] of TMatrix4; // 256 bytes
    CascadeAtlasUVScaleOffset: array[0..CASTLE_MAX_SHADOW_CASCADES - 1] of TVector4; // 64 bytes (xy = scale, zw = offset)
    CascadeSplitDepths: TVector4; // 16 bytes (far view-space depth split d0..d3)
  end; // Total = 336 bytes (336 mod 16 = 0)

  { GPU std430 entry for positional (spot / point face) shadow atlas tile (80 bytes) }
  PCastlePositionalShadowTileEntry = ^TCastlePositionalShadowTileEntry;
  TCastlePositionalShadowTileEntry = packed record
    LightViewProjMatrix: TMatrix4; // 64 bytes
    AtlasUVScaleOffset: TVector4;  // 16 bytes (xy = UV scale, zw = UV offset in shadow atlas)
  end; // Total = 80 bytes

const
  CASTLE_BLOOM_MAX_MIP_LEVELS     = 6;
  CASTLE_LUMINANCE_HISTOGRAM_BINS = 256;

  { Phase 16.3: Default 3D camera-aligned Froxel Volumetric Fog grid dimensions }
  CASTLE_FROXEL_GRID_X = 160;
  CASTLE_FROXEL_GRID_Y = 90;
  CASTLE_FROXEL_GRID_Z = 64;

  { Phase 17.2 & 17.3: Maximum localized reflection probes and projected box decals }
  CASTLE_MAX_REFLECTION_PROBES = 16;
  CASTLE_MAX_PROJECTED_DECALS  = 64;

  { Phase 18.1: Default GPU compute particle pool capacity }
  CASTLE_DEFAULT_MAX_PARTICLES = 65536;

  { Phase 19.1 & 19.2: Default compute skinning vertex capacity and max CDLOD terrain quadtree nodes }
  CASTLE_DEFAULT_MAX_SKINNED_VERTICES = 16384;
  CASTLE_MAX_TERRAIN_CDLOD_NODES      = 256;

  { Phase 20.2: Default Sky-View LUT resolution for Rayleigh/Mie Atmospheric Scattering }
  CASTLE_DEFAULT_SKY_VIEW_LUT_WIDTH   = 192;
  CASTLE_DEFAULT_SKY_VIEW_LUT_HEIGHT  = 108;

  { Phase 21.2: Default GPU Compute Gerstner Ocean simulation texture resolution }
  CASTLE_DEFAULT_OCEAN_GRID_SIZE      = 128;

  { Phase 22.1 & 22.2: Default DDGI Octahedral Probe Grid dimensions and max GPU Meshlets }
  CASTLE_DEFAULT_DDGI_GRID_X          = 8;
  CASTLE_DEFAULT_DDGI_GRID_Y          = 4;
  CASTLE_DEFAULT_DDGI_GRID_Z          = 8;
  CASTLE_DEFAULT_DDGI_OCT_TEXELS      = 8;
  CASTLE_MAX_MESHLETS                 = 4096;

  { Phase 23.2: Virtual Shadow Map (VSM) Page Table & Physical Tile Pool dimensions }
  CASTLE_VSM_PAGE_TABLE_DIM           = 64;
  CASTLE_VSM_PHYSICAL_TILE_SIZE       = 128;
  CASTLE_VSM_PHYSICAL_TILES_PER_ROW   = 16;
  CASTLE_VSM_MAX_PHYSICAL_TILES       = 256;

  { Maximum custom user sampler uniforms per Effect / ComposedShader / ScreenEffect (Set 2, Bindings 2..9) }
  CASTLE_MAX_USER_EFFECT_SAMPLERS     = 8;

type
  { Reflected std140 member of CastleUserEffectBlock (Set 2, Binding 1) }
  TUserEffectUniformDesc = record
    Name: String;
    GLSLType: String;
    DeclSuffix: String;
    ByteOffset: PtrUInt;
    ByteSize: PtrUInt;
    ArrayCount: Cardinal;
  end;

  { Reflected custom sampler uniform at Set 2, Binding (2 + SlotIndex) }
  TUserEffectSamplerDesc = record
    Name: String;
    GLSLType: String;
    SlotIndex: Integer;
  end;

  { Reflected custom inter-stage varying assigned deterministic location }
  TUserEffectVaryingDesc = record
    Name: String;
    GLSLType: String;
    InterpQual: String;
    Location: Cardinal;
    LocationSlots: Cardinal;
  end;

  { Unified cross-stage reflection layout for an X3D Effect / ComposedShader / ScreenEffect }
  TUserEffectShaderLayout = record
    Uniforms: array of TUserEffectUniformDesc;
    Samplers: array of TUserEffectSamplerDesc;
    Varyings: array of TUserEffectVaryingDesc;
    TotalUBOSize: PtrUInt;
    function FindUniform(const AName: String; out ADesc: TUserEffectUniformDesc): Boolean;
    function FindSampler(const AName: String; out ADesc: TUserEffectSamplerDesc): Boolean;
    function FindVarying(const AName: String; out ADesc: TUserEffectVaryingDesc): Boolean;
  end;
  { HDR Tonemapping Operators (Phase 15.3) }
  TCastleTonemapOperator = (
    tmoLinear     = 0,
    tmoReinhard   = 1,
    tmoACESFilmic = 2,
    tmoAgX        = 3
  );

  { Push constants for 13-tap Karis downsample & 9-tap tent upsample bloom compute passes (32 bytes) }
  PCastleBloomPushConstants = ^TCastleBloomPushConstants;
  TCastleBloomPushConstants = packed record
    SrcTexelSize: TVector2;      // 8 bytes (1.0 / SrcWidth, 1.0 / SrcHeight)
    Threshold: Single;           // 4 bytes (Bloom brightness threshold, e.g. 1.0)
    Knee: Single;                // 4 bytes (Soft knee width, e.g. 0.5)
    FilterRadius: Single;        // 4 bytes (Tent filter radius in UV or texel units, e.g. 1.0)
    MipLevel: Cardinal;          // 4 bytes (Current bloom mip level 0..CASTLE_BLOOM_MAX_MIP_LEVELS-1)
    IsFirstDownsample: Cardinal; // 4 bytes (1 on Mip 0 -> applies Karis average + soft threshold, 0 on Mip > 0)
    DstPackedSize: Cardinal;     // 4 bytes ((DstHeight shl 16) or DstWidth)
  end; // Total = 32 bytes

  { Push constants / uniform parameters for 256-bin log-luminance histogram & eye adaptation (32 bytes) }
  PCastleAutoExposureParams = ^TCastleAutoExposureParams;
  TCastleAutoExposureParams = packed record
    MinLogLum: Single;        // 4 bytes (Minimum log2 luminance, e.g. -8.0)
    MaxLogLum: Single;        // 4 bytes (Maximum log2 luminance, e.g. +4.0)
    TimeDelta: Single;        // 4 bytes (Frame delta time in seconds)
    AdaptationSpeed: Single;  // 4 bytes (Temporal adaptation speed, e.g. 2.5)
    TargetMiddleGrey: Single; // 4 bytes (Key value / middle grey, e.g. 0.18)
    MinExposure: Single;      // 4 bytes (Clamp min exposure, e.g. 0.1)
    MaxExposure: Single;      // 4 bytes (Clamp max exposure, e.g. 10.0)
    TotalPixels: Cardinal;    // 4 bytes (Width * Height of HDR scene target)
  end; // Total = 32 bytes

  { Push constants for HDR + Bloom + Auto-Exposure + ACES/AgX Tonemapping composite pass (32 bytes) }
  PCastleHDRCompositePushConstants = ^TCastleHDRCompositePushConstants;
  TCastleHDRCompositePushConstants = packed record
    Exposure: Single;            // 4 bytes (Manual or adapted exposure multiplier)
    BloomIntensity: Single;      // 4 bytes (Bloom contribution weight, e.g. 0.08)
    Gamma: Single;               // 4 bytes (Display gamma, default 2.2)
    Saturation: Single;          // 4 bytes (Color grading saturation, default 1.0)
    Contrast: Single;            // 4 bytes (Color grading contrast, default 1.0)
    TonemapOperator: Cardinal;   // 4 bytes (Ord(TCastleTonemapOperator))
    EnableBloom: Cardinal;       // 4 bytes (1 = add bloom, 0 = bypass)
    EnableAutoExposure: Cardinal;// 4 bytes (1 = multiply by adapted exposure SSBO, 0 = manual Exposure)
  end; // Total = 32 bytes

  { Phase 16.1: Push constants for Compute Ground-Truth Ambient Occlusion (GTAO) & Bilateral Denoiser (48 bytes) }
  PCastleGTAOPushConstants = ^TCastleGTAOPushConstants;
  TCastleGTAOPushConstants = packed record
    InvResolution: TVector2;  // 8 bytes (1.0 / Width, 1.0 / Height)
    ProjScale: TVector2;      // 8 bytes (1.0 / Proj[0][0], 1.0 / Proj[1][1])
    ZNear: Single;            // 4 bytes (Camera near plane)
    ZFar: Single;             // 4 bytes (Camera far plane)
    EffectRadius: Single;     // 4 bytes (View-space AO hemisphere radius, e.g. 1.5)
    FalloffRange: Single;     // 4 bytes (Horizon thickness falloff range, e.g. 0.6)
    Power: Single;            // 4 bytes (AO contrast power exponent, e.g. 1.5)
    SliceCount: Cardinal;     // 4 bytes (Azimuthal slices per pixel, e.g. 3)
    StepsPerSlice: Cardinal;  // 4 bytes (March steps per slice side, e.g. 4)
    DenoisePass: Cardinal;    // 4 bytes (0 = GTAO horizon evaluation, 1 = 5x5 bilateral denoiser)
  end; // Total = 48 bytes

  { Phase 16.2: Push constants for Hierarchical-Z Screen-Space Reflections (Hi-Z SSR) (48 bytes) }
  PCastleSSRPushConstants = ^TCastleSSRPushConstants;
  TCastleSSRPushConstants = packed record
    InvResolution: TVector2;  // 8 bytes (1.0 / Width, 1.0 / Height)
    ZNear: Single;            // 4 bytes (Camera near plane)
    ZFar: Single;             // 4 bytes (Camera far plane)
    MaxDistance: Single;      // 4 bytes (Maximum view-space ray distance, e.g. 50.0)
    Thickness: Single;        // 4 bytes (Depth intersection thickness tolerance, e.g. 0.25)
    MaxRoughness: Single;     // 4 bytes (Roughness cutoff above which SSR fades out, e.g. 0.6)
    EdgeFadeWidth: Single;    // 4 bytes (Screen-edge vignette fade border, e.g. 0.12)
    MaxSteps: Cardinal;       // 4 bytes (Maximum Hi-Z cell march steps, e.g. 48)
    HiZMipCount: Cardinal;    // 4 bytes (Number of Hi-Z min-depth mip levels, e.g. 6)
    Intensity: Single;        // 4 bytes (Reflection intensity multiplier, e.g. 1.0)
    Reserved: Cardinal;       // 4 bytes (Padding to 16-byte alignment)
  end; // Total = 48 bytes

  { Phase 16.3: Uniform / push-constant parameters for 3D Froxel Volumetric Fog & Exponential Height Fog (64 bytes) }
  PCastleVolumetricFogParams = ^TCastleVolumetricFogParams;
  TCastleVolumetricFogParams = packed record
    FogAlbedoAndBaseDensity: TVector4; // 16 bytes (rgb = scattering albedo, w = base density rho_0)
    SunDirectionAndAnisotropy: TVector4; // 16 bytes (xyz = normalized sun dir, w = Henyey-Greenstein g in [-0.99..0.99])
    SunColorAndIntensity: TVector4;    // 16 bytes (rgb = sun color, w = sun scattering intensity)
    HeightFalloff: Single;             // 4 bytes (Exponential height falloff h)
    BaseHeight: Single;                // 4 bytes (Reference fog height y_0)
    ZNear: Single;                     // 4 bytes (Camera near plane)
    ZFar: Single;                      // 4 bytes (Camera far plane)
  end; // Total = 64 bytes

  { Phase 17.1: Push constants for Temporal Anti-Aliasing (TAA) resolve compute pass (32 bytes) }
  PCastleTAAPushConstants = ^TCastleTAAPushConstants;
  TCastleTAAPushConstants = packed record
    InvResolution: TVector2;      // 8 bytes (1.0 / Width, 1.0 / Height)
    JitterOffset: TVector2;       // 8 bytes (Current sub-pixel Halton camera jitter in UV/NDC)
    BlendFactor: Single;          // 4 bytes (Current frame weight, e.g. 0.08 -> 92% history)
    VarianceClipGamma: Single;    // 4 bytes (YCoCg 3x3 neighborhood variance clip gamma, e.g. 1.25)
    VelocityWeightScale: Single;  // 4 bytes (Disocclusion velocity boost scale, e.g. 40.0)
    FrameIndex: Cardinal;         // 4 bytes (Monotonic frame counter)
  end; // Total = 32 bytes

  { Phase 17.2: GPU std140/std430 entry for a localized Box-Projected Parallax Reflection Probe (64 bytes) }
  PCastleReflectionProbeEntry = ^TCastleReflectionProbeEntry;
  TCastleReflectionProbeEntry = packed record
    PositionAndRadius: TVector4;     // 16 bytes (xyz = world capture center, w = influence radius)
    BoxMinAndIntensity: TVector4;    // 16 bytes (xyz = world AABB min, w = intensity multiplier)
    BoxMaxAndCubemapIndex: TVector4; // 16 bytes (xyz = world AABB max, w = cubemap array / bindless index)
    BlendDistanceAndFlags: TVector4; // 16 bytes (x = inner transition falloff distance, y = box projection 1/0, z = mip count, w = priority)
  end; // Total = 64 bytes

  { Phase 17.3: GPU std140/std430 entry for an Oriented Bounding Box (OBB) Projected Decal (96 bytes) }
  PCastleProjectedDecalEntry = ^TCastleProjectedDecalEntry;
  TCastleProjectedDecalEntry = packed record
    WorldToDecalRow0: TVector4;        // 16 bytes (Row 0 of 4x3 World -> OBB unit box [-0.5..0.5]^3 transform)
    WorldToDecalRow1: TVector4;        // 16 bytes (Row 1 of 4x3 World -> OBB unit box [-0.5..0.5]^3 transform)
    WorldToDecalRow2: TVector4;        // 16 bytes (Row 2 of 4x3 World -> OBB unit box [-0.5..0.5]^3 transform)
    AlbedoTintAndOpacity: TVector4;    // 16 bytes (rgb = decal base color tint, a = opacity)
    DecalNormalAndRoughness: TVector4; // 16 bytes (xyz = world projection normal axis, w = roughness modulation)
    UVScaleOffset: TVector4;           // 16 bytes (xy = UV scale, zw = UV offset in decal atlas)
  end; // Total = 96 bytes

  { Phase 18.1: GPU std430 entry for a single simulated particle (48 bytes) }
  PCastleGPUParticle = ^TCastleGPUParticle;
  TCastleGPUParticle = packed record
    PositionAndAge: TVector4;     // 16 bytes (xyz = world position, w = current age in seconds)
    VelocityAndMaxLife: TVector4; // 16 bytes (xyz = world velocity, w = maximum lifetime in seconds)
    ColorAndSize: TVector4;       // 16 bytes (rgb = particle emissive/tint color, w = billboard radius)
  end; // Total = 48 bytes

  { Phase 18.1: Push constants for GPU Compute Particle Simulation & Alive-List Compaction (48 bytes) }
  PCastleParticleSimPushConstants = ^TCastleParticleSimPushConstants;
  TCastleParticleSimPushConstants = packed record
    GravityAndDeltaTime: TVector4; // 16 bytes (xyz = gravity vector, w = deltaTime)
    EmitterOriginAndDrag: TVector4;// 16 bytes (xyz = emitter world origin, w = linear air drag)
    GroundPlaneY: Single;          // 4 bytes (Ground collision plane Y)
    Restitution: Single;           // 4 bytes (Bounce restitution factor in [0..1])
    MaxParticles: Cardinal;        // 4 bytes (Total particle capacity)
    SpawnSeed: Cardinal;           // 4 bytes (Per-frame random hash seed)
  end; // Total = 48 bytes

  { Phase 18.3: Push constants for Screen-Space Raymarched Contact Shadows (48 bytes) }
  PCastleContactShadowsPushConstants = ^TCastleContactShadowsPushConstants;
  TCastleContactShadowsPushConstants = packed record
    InvResolution: TVector2; // 8 bytes (1.0 / Width, 1.0 / Height)
    LightDirView: TVector3;  // 12 bytes (Normalized view-space directional light vector towards light)
    RayLength: Single;       // 4 bytes (View-space contact shadow ray march length, e.g. 0.35)
    Thickness: Single;       // 4 bytes (Depth occluder thickness window, e.g. 0.05)
    ZNear: Single;           // 4 bytes (Camera near plane)
    ZFar: Single;            // 4 bytes (Camera far plane)
    StepCount: Cardinal;     // 4 bytes (Raymarch step count, e.g. 16)
    Contrast: Single;        // 4 bytes (Contact shadow contrast multiplier, e.g. 1.0)
    Reserved: Cardinal;      // 4 bytes (Padding to 16-byte alignment)
  end; // Total = 48 bytes

  { Phase 19.1: Source bind-pose vertex entry for GPU Compute Skeletal Skinning (64 bytes) }
  PCastleSkinningVertex = ^TCastleSkinningVertex;
  TCastleSkinningVertex = packed record
    PositionAndUVX: TVector4;             // 16 bytes (xyz = bind-pose position, w = u)
    NormalAndUVY: TVector4;               // 16 bytes (xyz = bind-pose normal, w = v)
    JointIndices: array[0..3] of Cardinal;// 16 bytes (4 bone matrix indices)
    JointWeights: TVector4;               // 16 bytes (4 bone blend weights)
  end; // Total = 64 bytes

  { Phase 19.1: Morph target displacement delta entry (32 bytes) }
  PCastleMorphTargetDelta = ^TCastleMorphTargetDelta;
  TCastleMorphTargetDelta = packed record
    DeltaPosition: TVector4; // 16 bytes (xyz = position displacement delta, w = 0)
    DeltaNormal: TVector4;   // 16 bytes (xyz = normal displacement delta, w = 0)
  end; // Total = 32 bytes

  { Phase 19.1: Push constants for GPU Compute Skeletal Skinning & Morph Target Blending (32 bytes) }
  PCastleComputeSkinningPushConstants = ^TCastleComputeSkinningPushConstants;
  TCastleComputeSkinningPushConstants = packed record
    MorphWeights: TVector4;       // 16 bytes (up to 4 active morph target blend weights)
    VertexCount: Cardinal;        // 4 bytes (number of vertices to skin)
    MorphTargetCount: Cardinal;   // 4 bytes (0..4 active morph targets)
    JointCount: Cardinal;         // 4 bytes (active bone matrix count)
    StorePreviousPos: Cardinal;   // 4 bytes (1 = copy previous skinned pos for TAA motion vectors)
  end; // Total = 32 bytes

  { Phase 19.2: GPU std430 entry for a Continuous Distance-Dependent LOD (CDLOD) Terrain quadtree patch (32 bytes) }
  PCastleTerrainCDLODNode = ^TCastleTerrainCDLODNode;
  TCastleTerrainCDLODNode = packed record
    NodeXZAndScale: TVector4;              // 16 bytes (x = world X origin, y = world Z origin, z = patch world size, w = LOD level)
    MorphStartEndAndHeightRange: TVector4; // 16 bytes (x = morph start dist, y = morph end dist, z = minY, w = maxY)
  end; // Total = 32 bytes

  { Phase 20.2: Push constants for Rayleigh / Mie Physical Sky & Aerial Perspective LUT compute pass (64 bytes) }
  PCastleAtmospherePushConstants = ^TCastleAtmospherePushConstants;
  TCastleAtmospherePushConstants = packed record
    SunDirectionAndIntensity: TVector4;    // 16 bytes (xyz = normalized sun dir, w = solar irradiance intensity)
    RayleighScatteringAndHeight: TVector4; // 16 bytes (rgb = Rayleigh sea-level scattering coeff, w = scale height H_R)
    MieScatteringAndHeight: TVector4;      // 16 bytes (rgb = Mie sea-level scattering coeff, w = scale height H_M)
    PlanetRadius: Single;                  // 4 bytes (Planet radius in meters, e.g. 6360000.0)
    AtmosphereRadius: Single;              // 4 bytes (Top-of-atmosphere radius in meters, e.g. 6420000.0)
    MieAnisotropyG: Single;                // 4 bytes (Cornette-Shanks Mie asymmetry g in [0..0.99], default 0.8)
    RaymarchSteps: Cardinal;               // 4 bytes (Single-scattering raymarch steps, default 16)
  end; // Total = 64 bytes

  { Phase 21.2: Push constants for GPU Compute Gerstner Wave Ocean Simulation & Foam (48 bytes) }
  PCastleOceanWavePushConstants = ^TCastleOceanWavePushConstants;
  TCastleOceanWavePushConstants = packed record
    WindDirectionAndSpeed: TVector4; // 16 bytes (xy = normalized wind dir, z = wind speed m/s, w = time seconds)
    WaveParams: TVector4;            // 16 bytes (x = base amplitude, y = base wavelength, z = choppiness Q, w = patch world size)
    GridSize: Cardinal;              // 4 bytes (Ocean texture resolution, default 128)
    OctaveCount: Cardinal;           // 4 bytes (Number of Gerstner wave octaves, default 4)
    FoamThreshold: Single;           // 4 bytes (Jacobian determinant threshold for whitecap crest foam, e.g. 0.65)
    Gravity: Single;                 // 4 bytes (Gravitational acceleration, default 9.81)
  end; // Total = 48 bytes

  { Phase 22.1: GPU std140/push-constant parameters for Dynamic Diffuse Global Illumination (DDGI) Octahedral Probe Grid (48 bytes) }
  PCastleDDGIGridParams = ^TCastleDDGIGridParams;
  TCastleDDGIGridParams = packed record
    GridOriginAndNormalBias: TVector4;  // 16 bytes (xyz = world-space probe [0,0,0] origin, w = normal bias in meters)
    GridSpacingAndViewBias: TVector4;   // 16 bytes (xyz = probe world spacing, w = view bias in meters)
    GridCountX: Cardinal;               // 4 bytes (probe count along X, default 8)
    GridCountY: Cardinal;               // 4 bytes (probe count along Y, default 4)
    GridCountZ: Cardinal;               // 4 bytes (probe count along Z, default 8)
    Hysteresis: Single;                 // 4 bytes (temporal irradiance hysteresis in [0..0.99], default 0.97)
  end; // Total = 48 bytes

  { Phase 22.2: GPU std430 descriptor for a 64-vertex / 124-triangle Meshlet cluster (48 bytes) }
  PCastleMeshletDescriptor = ^TCastleMeshletDescriptor;
  TCastleMeshletDescriptor = packed record
    BoundingSphere: TVector4;    // 16 bytes (xyz = meshlet sphere center, w = radius)
    ConeAxisAndCutoff: TVector4; // 16 bytes (xyz = normalized normal cone axis, w = sin(coneHalfAngle) backface cutoff)
    VertexOffset: Cardinal;      // 4 bytes (base vertex offset)
    VertexCount: Cardinal;       // 4 bytes (meshlet vertex count <= 64)
    TriangleOffset: Cardinal;    // 4 bytes (base primitive offset)
    TriangleCount: Cardinal;     // 4 bytes (meshlet triangle count <= 124)
  end; // Total = 48 bytes

  { Phase 24.1: GPU std430 descriptor for Continuous Instance LOD Selection (48 bytes) }
  PCastleLODInstanceDescriptor = ^TCastleLODInstanceDescriptor;
  TCastleLODInstanceDescriptor = packed record
    BoundingSphere: TVector4;      // 16 bytes (xyz = world sphere center, w = radius)
    LODScreenThresholds: TVector4; // 16 bytes (x = LOD0->1 coverage, y = LOD1->2, z = LOD2->3, w = cull coverage)
    MeshID: Cardinal;              // 4 bytes (base mesh asset ID)
    InstanceID: Cardinal;          // 4 bytes (instance index)
    TransitionBand: Single;        // 4 bytes (crossfade band width in screen coverage units, default 0.02)
    Flags: Cardinal;               // 4 bytes (reserved)
  end; // Total = 48 bytes

const
  CASTLE_MAX_STREAMED_TEXTURES = 512;

type
  { Phase 27.2: GPU std430 descriptor for a Nanite-style Hierarchical Cluster DAG Node (48 bytes) }
  PCastleClusterDAGNode = ^TCastleClusterDAGNode;
  TCastleClusterDAGNode = packed record
    SphereCenterAndRadius: TVector4;       // 16 bytes (xyz = world sphere center, w = radius)
    ParentCenterAndErrors: TVector4;       // 16 bytes (x = self world error, y = parent world error, z = parent dist offset, w = reserved)
    ClusterID: Cardinal;                   // 4 bytes (cluster index)
    VertexOffset: Cardinal;                // 4 bytes (base vertex offset)
    TriangleOffset: Cardinal;              // 4 bytes (base primitive offset)
    TriangleCount: Cardinal;               // 4 bytes (triangle count <= 124)
  end; // Total = 48 bytes

  { Phase 29.1: GPU std430 streaming reservoir for ReSTIR DI / GI (UE5 MegaLights, 32 bytes) }
  PCastleReSTIRReservoir = ^TCastleReSTIRReservoir;
  TCastleReSTIRReservoir = packed record
    SelectedLightID: Integer;     // 4 bytes (selected candidate light index, -1 if empty)
    WeightSum: Single;            // 4 bytes (running sum of resampling weights sum(w_i))
    TargetPDF: Single;            // 4 bytes (unshadowed target PDF p_hat(y) of SelectedLightID)
    SampleCountM: Cardinal;       // 4 bytes (number of candidates M seen by this reservoir)
    ContributionWeightW: Single;  // 4 bytes (unbiased RIS weight W = WeightSum / (TargetPDF * M))
    Reserved0: Cardinal;          // 4 bytes (std430 16/32-byte alignment padding)
    Reserved1: Cardinal;          // 4 bytes
    Reserved2: Cardinal;          // 4 bytes
  end; // Total = 32 bytes

  { Phase 111: Push constants for GTAO + Hi-Z SSR HDR lighting modulation compute pass (16 bytes) }
  PCastlePostProcessModulatePushConstants = ^TCastlePostProcessModulatePushConstants;
  TCastlePostProcessModulatePushConstants = packed record
    AOStrength: Single;   // 4 bytes (0 = no AO attenuation, 1 = full GTAO visibility)
    SSRStrength: Single;  // 4 bytes (additive screen-space reflection multiplier)
    DstWidth: Cardinal;   // 4 bytes (output HDR texture width in pixels)
    DstHeight: Cardinal;  // 4 bytes (output HDR texture height in pixels)
  end; // Total = 16 bytes

  { Generates RHI-compliant GLSL headers, UBO blocks, and varying layouts }
  TCastleShaderGeneratorRHI = class
  private
    FBackend: TRHIBackendType;
    function GenerateVersionAndLayoutMacros: String;
    function GenerateVersionAndLayoutMacrosForStage(const AStage: TRHIShaderStage): String;
    function GenerateStandardUniformBlocks(const AStage: TRHIShaderStage): String;
    function GenerateStageIO(const AStage: TRHIShaderStage): String;
  public
    constructor Create(const ABackend: TRHIBackendType);

    { Builds the complete preamble injected before CGE's internal GLSL templates }
    function GeneratePreamble(const AStage: TRHIShaderStage): String;

    { Builds a clean cross-backend compute shader preamble (#version 450 on Vulkan,
      #version 430 core on OpenGL, #version 310 es on OpenGLES) with unified
      CASTLE_UBO, CASTLE_SSBO, CASTLE_SAMPLER, CASTLE_IMAGE, and CASTLE_PUSH_CONSTANT
      macros plus layout(local_size_x, local_size_y, local_size_z) in; }
    function GenerateComputePreamble(const ALocalSizeX: Cardinal = 1;
      const ALocalSizeY: Cardinal = 1;
      const ALocalSizeZ: Cardinal = 1): String;

    { Builds a bindless texture/material GLSL 450 preamble using GL_EXT_nonuniform_qualifier,
      Set 1 Binding 0 unbounded/sized sampler2D array (castle_BindlessTextures[]), and
      Set 1 Binding 1 Material SSBO (CastleBindlessMaterials[]) indexed by push-constant
      or per-draw MaterialIndex for multi-material indirect rendering. }
    function GenerateBindlessPreamble(const AStage: TRHIShaderStage;
      const AMaxBindlessTextures: Cardinal = 4096): String;

    { Transforms legacy user X3D EffectPart uniform declarations into a std140 UBO
      at Set 2, Binding 1 and emits WritelnWarning diagnostics for deprecated raw
      OpenGL built-in uniforms/variables while auto-upgrading them to #version 450 }
    function UpgradeUserEffectUniforms(const UserEffectSource: String): String;

    { Extracts a unified cross-stage std140 UBO, sampler-slot, and varying-location
      layout across Vertex, Geometry, and Fragment Effect / ComposedShader parts so
      interface declarations can be packed by uniform name (independent of X3D vs GLSL
      declaration order) and custom varyings receive matching CASTLE_LOCATION indices. }
    function ExtractUserEffectLayout(
      const AVertSource, AFragSource: String;
      const AGeomSource: String = '';
      const AFirstVaryingLocation: Cardinal = 10): TUserEffectShaderLayout;

    { Upgrades a single stage's Effect / ComposedShader / ScreenEffect GLSL source
      against a unified cross-stage TUserEffectShaderLayout. }
    function UpgradeUserEffectStageSource(
      const AStage: TRHIShaderStage;
      const AStageSource: String;
      const ALayout: TUserEffectShaderLayout;
      const AFirstSamplerBinding: Cardinal = 2;
      const ASetIndex: Cardinal = 2): String;

    { Scans user Effect / EffectPart GLSL code for deprecated OpenGL built-in
      uniforms, attributes, outputs, and texture functions (e.g. gl_ModelViewProjectionMatrix,
      gl_ModelViewMatrix, gl_ProjectionMatrix, gl_NormalMatrix, gl_Vertex, gl_Normal,
      gl_MultiTexCoord0, gl_Color, gl_FragColor, texture2D, textureCube, attribute, varying),
      emits clear WritelnWarning diagnostics, and injects #version 450 compatibility aliases. }
    function UpgradeLegacyGLSLBuiltIns(const UserEffectSource: String;
      const EffectDebugName: String = 'Effect'): String; overload;
    function UpgradeLegacyGLSLBuiltIns(const UserEffectSource: String;
      const EffectDebugName: String; out AWarningsCount: Cardinal): String; overload;

    { Appends Vulkan clip-space depth adjustment (Z from [-1..1] to [0..1])
      at the end of the vertex shader main() }
    function FinalizeVertexShaderSource(const StitchedSource: String): String;

    { Appends CGE geometry shader helper implementations (geometryVertexSet,
      geometryVertexZero, geometryVertexAdd with optional PLUG_geometry_vertex_* calls)
      after the user's geometry shader code and ensures input/output layout qualifiers exist }
    function FinalizeGeometryShaderSource(
      const StitchedSource: String;
      const PlugSetCalls: String = '';
      const PlugZeroCalls: String = '';
      const PlugAddCalls: String = ''): String;

    { Strips or comments out legacy bare uniform declarations replaced by Set 0..2 UBOs and Push Constants }
    function SanitizeVulkanShaderSource(const Source: String): String;

    { Builds GLSL 450 preamble for post-processing screen effects (TCastleScreenEffects) }
    function GenerateScreenEffectPreamble(const AStage: TRHIShaderStage;
      const HasDepth: Boolean;
      const AIncludeVertexResources: Boolean = False): String;

    { Builds GLSL 450 preamble for custom 2D fragment shaders (TDrawableImage.CustomShader)
      paired with TCastleBatch2DRenderer (64-byte push constants + location 0/1 stage IO). }
    function GenerateBatch2DFragmentPreamble: String;

    { Generates a complete GLSL 450 compute shader for GPU-driven frustum & distance culling
      that populates VkDrawIndexedIndirectCommand entries and atomic visibility counters. }
    function GenerateComputeCullingShader(const AWorkgroupSizeX: Cardinal = 64): String;

    { Generates a complete GLSL 450 compute shader for Hierarchical Z (Hi-Z) depth pyramid
      mipmap reduction (conservative 2x2 max-depth reduction with odd-dimension edge handling). }
    function GenerateHiZDownsampleComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Generates glTF 2.0 / X3D PhysicalMaterial Cook-Torrance microfacet GGX BRDF functions:
      DistributionGGX, GeometrySmithGGXCorrelated, FresnelSchlick, FresnelSchlickRoughness,
      PerturbNormalCotangent (screen-space derivative TBN), PerturbNormalTBN,
      EvaluateDirectCookTorranceGGX, and EvaluateIBLCookTorranceGGX. }
    function GeneratePBRFunctions: String;

    { Generates GLSL 450 SSBO/UBO bindings and evaluation functions for 3D Clustered Forward+
      (256+ dynamic lights) and 4-cascade Directional CSM + Positional Shadow Atlas 3x3 PCF:
      ComputeClusterIndex3D, FlattenClusterIndex, SampleShadowAtlasPCF3x3,
      EvaluateDirectionalCascadeShadow, EvaluatePositionalAtlasShadow, and
      EvaluateClusteredForwardLights. }
    function GenerateClusteredForwardAndShadowFunctions: String;

    { Generates a complete GLSL 450 3D vertex shader supporting optional GPU linear-blend
      skinning (Set 2 CastleSkinningBlock) and tangent-space basis (castle_tangent_to_eye_space). }
    function GenerateDefault3DVertexShader(
      const HasSkinning: Boolean = False;
      const HasTangents: Boolean = False): String; overload;

    { Generates a complete GLSL 450 3D vertex shader declaring only the vertex attributes
      and Set 2 CastleSkinningBlock actually present on the shape, preventing Vulkan
      VUID-VkGraphicsPipelineCreateInfo-Input-07904 on unprovided attributes while
      aliasing absent attributes to safe defaults. }
    function GenerateDefault3DVertexShader(
      const HasSkinning, HasTangents, HasTexCoord1, HasColorPerVertex: Boolean): String; overload;

    { Generates the 3D vertex shader preamble with conditional vertex input attributes
      and conditional Set 2 CastleSkinningBlock. }
    function GeneratePreambleFor3DVertex(
      const AHasSkinning, AHasTangents, AHasTexCoord1, AHasColorPerVertex: Boolean): String;

    { Generates standard Set 0..1 (and optional Set 2 CastleSkinningBlock) uniform blocks. }
    function GenerateStandardUniformBlocksForStage(
      const AStage: TRHIShaderStage;
      const AIncludeSkinningUBO: Boolean): String;

    { Generates a complete GLSL 450 3D fragment shader supporting Cook-Torrance GGX PBR,
      Phong, and Unlit shading models, multi-texture sampling (BaseColor, Normal,
      MetallicRoughness, Emissive, Occlusion), normal mapping, alpha-cutoff discard,
      multi-light evaluation, Split-Sum IBL ambient lighting, optional Clustered Forward+
      (256+ lights), and optional 4-cascade CSM + Shadow Atlas PCF. }
    function GenerateDefault3DFragmentShader(
      const HasBaseColorTex: Boolean = False;
      const HasNormalTex: Boolean = False;
      const HasMetallicRoughnessTex: Boolean = False;
      const HasEmissiveTex: Boolean = False;
      const HasOcclusionTex: Boolean = False;
      const HasIBL: Boolean = False;
      const UseAlphaTest: Boolean = False;
      const HasClusteredForward: Boolean = False;
      const HasCascadedShadows: Boolean = False): String;

    { Generates a complete GLSL 450 compute shader that precomputes the 2D Split-Sum GGX
      BRDF Integration LUT (NdotV x Roughness -> Scale, Bias) using Van der Corput
      Hammersley quasi-Monte Carlo importance sampling. }
    function GenerateIBLBrdfLutComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Generates a complete GLSL 450 compute shader that subdivides the view frustum into
      a 3D exponential-depth cluster grid (e.g. 16x8x24) and bins up to 256 dynamic point,
      spot, and directional lights into per-cluster light index lists. }
    function GenerateClusteredLightCullingComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 15.2: Generates a GLSL 450 compute shader for 13-tap Jiménez dual-filtering
      bloom downsample with Karis average firefly suppression and soft-knee thresholding on Mip 0. }
    function GenerateBloomDownsampleComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 15.2: Generates a GLSL 450 compute shader for 9-tap 3x3 tent-filter bloom upsample. }
    function GenerateBloomUpsampleComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 15.2: Generates a GLSL 450 compute shader that builds a 256-bin log2 luminance
      histogram and updates temporal eye-adapted exposure. }
    function GenerateLuminanceHistogramComputeShader(
      const AWorkgroupSizeX: Cardinal = 16;
      const AWorkgroupSizeY: Cardinal = 16): String;

    { Phase 15.3: Generates GLSL 450 functions for Reinhard, ACES Filmic (Narkowicz RRT+ODT),
      and AgX (Sobotka inset + log2 sigmoid + outset) tonemapping, saturation/contrast grading,
      and IEC 61966-2-1 Linear-to-sRGB transfer. }
    function GenerateTonemappingAndColorGradingFunctions: String;

    { Phase 15.3: Generates a complete GLSL 450 fragment shader for the HDR Post-Processing
      Composite pass (combining HDR scene color, mip-chain bloom, adapted exposure, color grading,
      ACES/AgX tonemapping, and sRGB output). }
    function GenerateHDRCompositeFragmentShader: String;

    { Phase 16.1: Generates a GLSL 450 compute shader for Ground-Truth Ambient Occlusion (GTAO),
      reconstructing view-space positions and normals from depth and integrating multi-slice
      horizon angles with cosine-weighted analytical visibility. }
    function GenerateGTAOComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 16.1: Generates a GLSL 450 compute shader for 5x5 depth- and normal-aware
      cross-bilateral spatial denoising of the GTAO visibility buffer. }
    function GenerateGTAODenoiseComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 16.2: Generates a GLSL 450 compute shader for Hierarchical-Z Screen-Space Reflections
      (Hi-Z SSR), raymarching reflection rays against the min-depth Hi-Z mip pyramid with
      roughness attenuation, Schlick Fresnel weighting, and screen-edge vignette fade. }
    function GenerateHiZSSRComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 16.3: Generates a GLSL 450 3D compute shader that injects exponential height fog
      density, Henyey-Greenstein anisotropic sun scattering, and extinction into the
      3D camera-aligned froxel volume texture. }
    function GenerateVolumetricFogInjectComputeShader(
      const AWorkgroupSizeX: Cardinal = 4;
      const AWorkgroupSizeY: Cardinal = 4;
      const AWorkgroupSizeZ: Cardinal = 4): String;

    { Phase 16.3: Generates a GLSL 450 compute shader that raymarches front-to-back along
      each (x, y) froxel column using Beer-Lambert analytical step integration. }
    function GenerateVolumetricFogRaymarchComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 17.1: Generates a GLSL 450 compute shader for Temporal Anti-Aliasing (TAA) resolve
      with 3x3 neighborhood YCoCg variance clipping, 5-tap Catmull-Rom bicubic history resampling,
      and velocity-based disocclusion weighting. }
    function GenerateTAAResolveComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 17.2: Generates GLSL 450 functions and UBO/SSBO bindings for localized Reflection Probes
      with Box-Projected Parallax ray-AABB intersection (EvaluateBoxProjectedDirection) and
      distance-based trilinear probe blending (EvaluateProbeInfluenceWeight). }
    function GenerateBoxProjectedReflectionProbeFunctions: String;

    { Phase 17.3: Generates GLSL 450 functions and UBO/SSBO bindings for Oriented Bounding Box (OBB)
      Decal Projection (ProjectWorldPointToDecalUV, EvaluateDecalAngleAndBoxFade). }
    function GenerateClusteredDecalProjectionFunctions: String;

    { Phase 18.1: Generates a GLSL 450 compute shader for GPU particle simulation (semi-implicit
      Euler gravity/drag integration, ground-plane bounce collision, and atomic alive-list
      compaction populating VkDrawIndirectCommand). }
    function GenerateParticleSimulationComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 18.2: Generates GLSL 450 functions for McGuire & Bavoil Weighted Blended
      Order-Independent Transparency (EvaluateOITDepthWeight, AccumulateWeightedBlendedOIT,
      EvaluateOITComposite). }
    function GenerateWeightedBlendedOITFunctions: String;

    { Phase 18.2: Generates a complete GLSL 450 fragment shader for compositing Weighted Blended
      OIT accumulation (pfRGBA16_Float) and revealage (pfR16_Float) targets over the opaque HDR scene. }
    function GenerateWeightedBlendedOITCompositeFragmentShader: String;

    { Phase 18.3: Generates a GLSL 450 compute shader for Screen-Space Raymarched Contact Shadows
      along the primary directional light vector with depth-thickness validation and edge fade. }
    function GenerateContactShadowsComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 19.1: Generates a GLSL 450 compute shader for Skeletal Linear-Blend Skinning (LBS)
      combined with up to 4 Morph Target displacements and previous-frame vertex position output. }
    function GenerateComputeSkinningShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 19.2: Generates GLSL 450 vertex shader functions for Continuous Distance-Dependent
      LOD (CDLOD) terrain quadtree morphing (EvaluateCDLODMorphFactor, MorphCDLODGridVertex). }
    function GenerateTerrainCDLODFunctions: String;

    { Phase 19.3: Generates GLSL 450 PBR functions for Pre-Integrated Wrap-Diffuse Subsurface
      Scattering (EvaluateSubsurfaceWrapDiffuse) and Thin-Foliage Back-Lit Transmission
      (EvaluateFoliageTransmission). }
    function GenerateSubsurfaceAndTransmissionPBRFunctions: String;

    { Phase 19.3 CPU reference: Evaluates wrap-diffuse subsurface scattering with energy normalization. }
    class function EvaluateSubsurfaceWrapDiffuse(
      const ANdotL, AWrapFactor: Single;
      const AScatterColor: TVector3): TVector3; static;

    { Phase 19.3 CPU reference: Evaluates back-lit foliage/cloth translucent transmission. }
    class function EvaluateFoliageTransmission(
      const AViewDir, ALightDir, ASurfaceNormal, ATransmissionTint: TVector3;
      const AThickness, ADistortion, APower, AScale: Single): TVector3; static;

    { Phase 20.1: Generates GLSL 450 PBR functions for Multi-Layer Clear Coat
      (EvaluateClearCoatBRDF) and Stretched Tangent/Bitangent Anisotropic GGX
      Microfacet Distribution (EvaluateAnisotropicGGX_D). }
    function GenerateClearCoatAndAnisotropicPBRFunctions: String;

    { Phase 20.1 CPU reference: Evaluates secondary IOR=1.5 (F0=0.04) Clear Coat specular lobe
      and energy-conserving base-layer attenuation (1.0 - F_cc). }
    class procedure EvaluateClearCoatBRDF(
      const ACNdotH, ACNdotL, ACNdotV, AVdotH, AClearCoatFactor, AClearCoatRoughness: Single;
      out AClearCoatSpecular, ABaseLayerAttenuation: Single); static;

    { Phase 20.1 CPU reference: Evaluates anisotropic Trowbridge-Reitz GGX normal distribution D_aniso. }
    class function EvaluateAnisotropicGGX_D(
      const ANdotH, ATdotH, ABdotH, ARoughness, AAnisotropy: Single): Single; static;

    { Phase 20.2: Generates a GLSL 450 compute shader for baking the Rayleigh / Mie
      single-scattering physical sky-view LUT (rgba16f). }
    function GenerateAtmosphereSkyComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 20.2 CPU reference: Evaluates Rayleigh molecular phase function Phi_R(cosTheta). }
    class function EvaluateRayleighPhase(const ACosTheta: Single): Single; static;

    { Phase 20.2 CPU reference: Evaluates Cornette-Shanks aerosol Mie phase function Phi_M(cosTheta, g). }
    class function EvaluateCornetteShanksMiePhase(const ACosTheta, AG: Single): Single; static;

    { Phase 20.3: Generates GLSL 450 post-processing functions for Screen-Space Multi-Ghost
      Lens Flare and Radial Spectral Chromatic Aberration. }
    function GenerateLensFlareAndChromaticAberrationFunctions: String;

    { Phase 20.3 CPU reference: Computes R, G, B radial dispersion UV coordinates for chromatic aberration. }
    class procedure EvaluateChromaticAberrationOffsets(
      const AUV: TVector2; const AIntensity: Single;
      out ARedUV, AGreenUV, ABlueUV: TVector2); static;

    { Phase 20.3 CPU reference: Computes reflected ghost sample UV and radial window weight for lens flare ghosting. }
    class procedure EvaluateLensFlareGhostUV(
      const AUV: TVector2; const AGhostIndex: Integer; const AGhostSpacing: Single;
      out AGhostUV: TVector2; out ARadialWeight: Single); static;

    { Phase 21.1: Generates GLSL 450 PBR functions for Velvet/Cloth Estevez-Kulla Charlie
      Sheen (EvaluateCharlieSheen_D) and Thin-Film Spectral Iridescence (EvaluateThinFilmIridescence). }
    function GenerateClothSheenAndIridescencePBRFunctions: String;

    { Phase 21.1 CPU reference: Evaluates Estevez & Kulla (2017) "Charlie" microfiber sheen NDF. }
    class function EvaluateCharlieSheen_D(const ANdotH, ASheenRoughness: Single): Single; static;

    { Phase 21.1 CPU reference: Evaluates Airy thin-film optical path difference spectral interference. }
    class function EvaluateThinFilmIridescence(
      const ACosTheta1, AIridescenceIOR, AThicknessNanometers: Single): TVector3; static;

    { Phase 21.2: Generates a GLSL 450 compute shader for Multi-Octave Trochoidal Gerstner
      Ocean Wave Displacement, Analytical Normals & Jacobian Whitecap Foam. }
    function GenerateOceanDisplacementComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 21.3: Generates GLSL 450 post-processing functions for Thin-Lens Bokeh Depth of Field
      Circle of Confusion (EvaluateThinLensCoC) and Per-Pixel Velocity Motion Blur
      (EvaluateMotionBlurSampleWeight). }
    function GenerateMotionBlurAndBokehDoFFunctions: String;

    { Phase 21.3 CPU reference: Evaluates signed thin-lens Circle of Confusion (CoC) in pixels. }
    class function EvaluateThinLensCoC(
      const ALinearDepth, AFocusDistance, AFocalLength, AApertureFStop, AMaxCoCRadius: Single): Single; static;

    { Phase 21.3 CPU reference: Evaluates McGuire velocity-line motion blur sample weight with depth ordering. }
    class function EvaluateMotionBlurSampleWeight(
      const ASampleOffsetPixels, ASampleVelocityLengthPixels, ACenterDepth, ASampleDepth, ADepthSoftness: Single): Single; static;

    { Phase 22.1: Generates GLSL 450 functions for Dynamic Diffuse Global Illumination (DDGI)
      Octahedral Probe Encoding/Decoding and Chebyshev Visibility Trilinear Probe Weighting. }
    function GenerateDDGIProbeSamplingFunctions: String;

    { Phase 22.1 CPU reference: Encodes a normalized 3D direction into [-1, 1]^2 octahedral coordinates. }
    class function OctahedralEncodeDir(const ADir: TVector3): TVector2; static;

    { Phase 22.1 CPU reference: Decodes [-1, 1]^2 octahedral coordinates back to a normalized 3D direction. }
    class function OctahedralDecodeUV(const AOctUV: TVector2): TVector3; static;

    { Phase 22.1 CPU reference: Evaluates DDGI probe weight with wrap cosine and Chebyshev variance visibility. }
    class function EvaluateDDGIProbeWeight(
      const AWorldPos, ASurfaceNormal, AProbePos: TVector3;
      const AProbeMeanDist, AProbeVariance: Single): Single; static;

    { Phase 22.2: Generates a GLSL 450 compute shader for GPU Meshlet / Sub-Mesh Cluster
      Bounding-Sphere & Normal-Cone Backface Culling. }
    function GenerateMeshletCullingComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 22.2 CPU reference: Returns True if the meshlet normal cone is 100% backfacing to ACameraPosition. }
    class function EvaluateMeshletConeBackfaceCull(
      const AMeshletCenter, ACameraPosition, AConeAxis: TVector3;
      const AConeCutoff: Single): Boolean; static;

    { Phase 22.3: Generates GLSL 450 functions for AMD FidelityFX Contrast-Adaptive Sharpening
      (EvaluateCASFilter5Tap) and Edge-Adaptive Spatial Upscaling. }
    function GenerateCASSharpeningAndUpscaleFunctions: String;

    { Phase 22.3 CPU reference: Evaluates 5-tap Contrast-Adaptive Sharpening (CAS) without halo ringing. }
    class function EvaluateCASFilter5Tap(
      const ACenter, ANorth, ASouth, AEast, AWest: TVector3;
      const ASharpness: Single): TVector3; static;

    { Phase 23.1: Generates GLSL 450 functions for Hardware Inline Ray Query (VK_KHR_ray_query)
      and Software BVH Ray-AABB / Möller-Trumbore Ray-Triangle Intersection. }
    function GenerateInlineRayQueryFunctions(
      const AUseHardwareRayQueryExt: Boolean = True): String;

    { Phase 23.1 CPU reference: Evaluates slab Ray-AABB intersection within [ATMin, ATMax]. }
    class function EvaluateRayAABBIntersection(
      const ARayOrigin, ARayDir, ABoxMin, ABoxMax: TVector3;
      const ATMin, ATMax: Single;
      out AHitTNear, AHitTFar: Single): Boolean; static;

    { Phase 23.1 CPU reference: Evaluates Möller-Trumbore Ray-Triangle intersection within [ATMin, ATMax]. }
    class function EvaluateRayTriangleIntersection(
      const ARayOrigin, ARayDir, AV0, AV1, AV2: TVector3;
      const ATMin, ATMax: Single;
      out AHitT, ABaryU, ABaryV: Single): Boolean; static;

    { Phase 23.2: Generates GLSL 450 functions for Virtual Shadow Map (VSM) Page Table
      decoding and Virtual-to-Physical Tile Pool UV translation. }
    function GenerateVSMPageLookupFunctions: String;

    { Phase 23.3: Generates GLSL 450 functions for Separable Screen-Space Subsurface
      Scattering (SSSSS) Christensen-Burley Diffusion Profile and Bilateral Depth Weighting. }
    function GenerateSeparableSSSSFunctions: String;

    { Phase 23.3 CPU reference: Evaluates Christensen-Burley normalized diffusion profile R(r, d). }
    class function EvaluateBurleyDiffusionProfile(
      const ARadiusMillimeters: Single;
      const AScatterDistanceRGB: TVector3): TVector3; static;

    { Phase 23.3 CPU reference: Evaluates depth-aware bilateral SSSSS weight. }
    class function EvaluateBilateralSSSSWeight(
      const ASampleOffsetMillimeters, ADepthDiffMeters, AScatterDist, ADepthFalloffScale: Single): Single; static;

    { Phase 24.1: Generates a GLSL 450 compute shader for GPU-Driven Continuous Instance
      LOD Selection using projected vertical screen-height coverage and dither crossfade. }
    function GenerateLODSelectionComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 24.1 CPU reference: Evaluates normalized vertical screen-height coverage
      in [0..1] for a bounding sphere of ARadius at distance ADistance. }
    class function EvaluateProjectedScreenCoverage(
      const ADistance, ARadius, ACotHalfFovY: Single): Single; static;

    { Phase 24.1 CPU reference: Selects discrete LOD level (0..3, or -1 if culled)
      and computes continuous crossfade transition weight in [0..1]. }
    class function SelectDiscreteLODLevel(
      const AScreenCoverage: Single;
      const AThresholds: TVector4;
      out ACrossfadeWeight: Single;
      const ATransitionBand: Single = 0.02): Integer; static;

    { Phase 24.2: Generates GLSL 450 functions for Screen-Space Bent Normals and
      Lagarde/Jimenez Cone-Intersection Specular Occlusion. }
    function GenerateBentNormalsAndSpecularOcclusionFunctions: String;

    { Phase 24.2 CPU reference: Evaluates Lagarde/Jimenez Specular Occlusion from NdotV, AO, and Roughness. }
    class function EvaluateSpecularOcclusion(
      const ANdotV, AAmbientOcclusion, ARoughness: Single): Single; static;

    { Phase 24.2 CPU reference: Bends surface shading normal toward unoccluded bent horizon normal based on AO. }
    class function BendNormalTowardsUnoccludedDir(
      const ASurfaceNormal, AUnoccludedBentDir: TVector3;
      const AAmbientOcclusion: Single): TVector3; static;

    { Phase 24.3: Generates GLSL 450 functions for Trilinear 3D Color Grading LUT (sampler3D)
      half-texel coordinate mapping, Natural cos^4(theta) Vignette, and Luminance-Preserving Film Grain. }
    function GenerateColorGradingLUTAndFilmGrainFunctions: String;

    { Phase 24.3 CPU reference: Maps [0..1]^3 RGB color to half-texel-centered 3D LUT UVW coordinates. }
    class function Compute3DLUTSampleCoord(
      const AColorRGB: TVector3;
      const ALutDimension: Single = 32.0): TVector3; static;

    { Phase 24.3 CPU reference: Evaluates natural optical cos^4(theta) lens vignette falloff. }
    class function EvaluateNaturalCos4Vignette(
      const AUV: TVector2;
      const AIntensity: Single): Single; static;

    { Phase 24.3 CPU reference: Applies luminance-preserving spatio-temporal film grain. }
    class function EvaluateFilmGrainNoise(
      const AColor: TVector3;
      const AUV: TVector2;
      const ATimeSeconds, AIntensity: Single): TVector3; static;

    { Phase 25.1: Generates GLSL 450 functions for Horizon-Aligned Volumetric Cloud
      Raymarching (Dual-Lobe Henyey-Greenstein Phase & Schneider Beer-Powder Law). }
    function GenerateVolumetricCloudFunctions: String;

    { Phase 25.1 CPU reference: Evaluates Dual-Lobe Henyey-Greenstein scattering phase function. }
    class function EvaluateDualLobeHGPhase(
      const ACosTheta, AForwardG, ABackwardG, AForwardWeight: Single): Single; static;

    { Phase 25.1 CPU reference: Evaluates Schneider's Beer-Powder cloud transmittance & dark-edge effect. }
    class function EvaluateBeerPowderTransmittance(
      const AOpticalDepth, APowderStrength: Single): Single; static;

    { Phase 25.2: Generates GLSL 450 functions for GPU Texture Streaming Mipmap
      Residency Feedback (atomicMin desired mip recording from screen-space UV derivatives). }
    function GenerateTextureMipFeedbackFunctions: String;

    { Phase 25.2 CPU reference: Computes desired mipmap level from max texel footprint derivative. }
    class function ComputeDesiredMipLevel(
      const ATexelFootprintMaxDerivative: Single;
      const AMaxMipLevel: Cardinal;
      const AMipBias: Single = 0.0): Cardinal; static;

    { Phase 25.3: Generates a GLSL 450 compute shader that converts a 2:1 Equirectangular
      HDR panorama texture into a 6-face Cubemap (tdCube) texture. }
    function GenerateEquirectToCubemapComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 25.3 CPU reference: Maps a normalized 3D direction vector to 2:1 equirectangular UV in [0..1]^2. }
    class function DirectionToEquirectangularUV(const ADir: TVector3): TVector2; static;

    { Phase 25.3 CPU reference: Inverse-maps 2:1 equirectangular UV in [0..1]^2 to a normalized 3D direction. }
    class function EquirectangularUVToDirection(const AUV: TVector2): TVector3; static;

    { Phase 26.1: Generates GLSL 450 functions for Analytic Rectangular Area Lights
      via Linearly Transformed Cosines (LTC) & Baum/Lambert Solid-Angle Integration. }
    function GenerateAreaLightLTCFunctions: String;

    { Phase 26.1 CPU reference: Evaluates subtended solid angle in [0..2*Pi] of a 3D rectangular area light. }
    class function EvaluateRectAreaLightSolidAngle(
      const ASurfacePos, ARectCenter, ARectRight, ARectUp: TVector3;
      const AHalfWidth, AHalfHeight: Single): Single; static;

    { Phase 26.1 CPU reference: Evaluates solid-angle-widened effective roughness for area lights. }
    class function EvaluateLTCFilteredRoughness(
      const ABaseRoughness, ADistanceToAreaLight, AAreaExtentRadius: Single): Single; static;

    { Phase 26.2: Generates GLSL 450 functions for Seamless World-Space Triplanar
      Projection and Height-Modulated Multi-Layer Splat Blending. }
    function GenerateTriplanarAndHeightBlendFunctions: String;

    { Phase 26.2 CPU reference: Evaluates power-sharpened normalized triplanar blend weights (w.x + w.y + w.z = 1). }
    class function EvaluateTriplanarBlendWeights(
      const AWorldNormal: TVector3;
      const ASharpness: Single = 4.0): TVector3; static;

    { Phase 26.2 CPU reference: Evaluates height-map-modulated 4-layer splat blend weights. }
    class function EvaluateHeightModulatedBlendWeights(
      const ALayerWeights, ALayerHeights: TVector4;
      const AHeightContrast: Single = 0.2): TVector4; static;

    { Phase 26.3: Generates a GLSL 450 compute shader for GPU Shadow Caster Light-Ray
      Extrusion Culling against the receiver camera frustum AABB. }
    function GenerateShadowCasterExtrusionCullingComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 26.3 CPU reference: Tests whether a shadow-caster sphere extruded along
      ALightRayDir for [0..AMaxExtrusionDist] intersects [AReceiverBoxMin, AReceiverBoxMax]. }
    class function EvaluateShadowCasterExtrudedSphereIntersectsAABB(
      const ASphereCenter: TVector3;
      const ASphereRadius: Single;
      const ALightRayDir: TVector3;
      const AMaxExtrusionDist: Single;
      const AReceiverBoxMin, AReceiverBoxMax: TVector3): Boolean; static;

    { Phase 27.1: Generates GLSL 450 functions for a 64-Bit Visibility Buffer
      (Depth32 | InstanceID20 | PrimitiveID12) and Analytic Perspective-Correct Barycentrics. }
    function GenerateVisibilityBufferFunctions: String;

    { Phase 27.1 CPU reference: Packs Depth01 (32b), InstanceID (20b), and PrimitiveID (12b) into 64-bit payload. }
    class function PackVisibilityBufferPayload(
      const ADepth01: Single;
      const AInstanceID, APrimitiveID: Cardinal): QWord; static;

    { Phase 27.1 CPU reference: Unpacks Depth01, InstanceID, and PrimitiveID from 64-bit visibility payload. }
    class procedure UnpackVisibilityBufferPayload(
      const APayload: QWord;
      out AOutDepth01: Single;
      out AOutInstanceID, AOutPrimitiveID: Cardinal); static;

    { Phase 27.1 CPU reference: Evaluates perspective-correct 2D barycentric weights (lambda0 + lambda1 + lambda2 = 1). }
    class function EvaluatePerspectiveBarycentrics2D(
      const APixelNDC, AV0NDC, AV1NDC, AV2NDC: TVector2;
      const AInvW0, AInvW1, AInvW2: Single): TVector3; static;

    { Phase 27.2: Generates a GLSL 450 compute shader for Nanite-style Hierarchical
      Cluster DAG Cut Selection (ParentProjectedError > Threshold >= SelfProjectedError). }
    function GenerateClusterDAGCutComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 27.2 CPU reference: Evaluates projected screen-space geometric error in pixels. }
    class function EvaluateClusterProjectedErrorPixels(
      const ASphereCenter, ACameraPos: TVector3;
      const ASphereRadius, AWorldError, ACotHalfFovY, AViewportHeight: Single): Single; static;

    { Phase 27.2 CPU reference: Returns True iff this cluster node lies on the active crack-free DAG cut. }
    class function IsClusterDAGCutNode(
      const ASelfProjectedErrorPx, AParentProjectedErrorPx, APixelErrorThreshold: Single): Boolean; static;

    { Phase 27.3: Generates a GLSL 450 compute shader for Sub-Pixel Software Compute
      Micro-Polygon Rasterization using 2D edge functions and 64-bit atomic visibility writes. }
    function GenerateSoftwareMicroRasterComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 27.3 CPU reference: Evaluates 2D signed edge determinant (B.x-A.x)*(P.y-A.y) - (B.y-A.y)*(P.x-A.x). }
    class function EvaluateEdgeFunction2D(
      const AA, AB, AP: TVector2): Single; static;

    { Phase 27.3 CPU reference: Returns True when a front-facing triangle's 2D screen AABB fits within AMaxBoxPixels. }
    class function IsMicroTriangleCandidateForSoftwareRaster(
      const AV0Screen, AV1Screen, AV2Screen: TVector2;
      const AMaxBoxPixels: Single = 16.0): Boolean; static;

    { Phase 28.1: Generates GLSL 450 functions for Lumen-style Global Signed Distance Field (SDF)
      box evaluation, polynomial smooth union, 3D clipmap UVW mapping, and soft cone tracing. }
    function GenerateGlobalSDFTraceFunctions: String;

    { Phase 28.1 CPU reference: Exact signed distance from ASamplePos to an axis-aligned box. }
    class function EvaluateSDFBoxDistance(
      const ASamplePos, ABoxCenter, ABoxHalfExtents: TVector3): Single; static;

    { Phase 28.1 CPU reference: Polynomial smooth minimum (smin) of two SDF distances. }
    class function EvaluateSDFSmoothUnion(
      const AD1, AD2, AK: Single): Single; static;

    { Phase 28.1 CPU reference: Updates soft cone-traced visibility / sky occlusion along an SDF ray. }
    class function EvaluateSDFSoftShadowCone(
      const ACurrentVisibility, ASDFDistance, ARayDistance, AConeTanHalfAngle: Single): Single; static;

    { Phase 28.2: Generates GLSL 450 functions for Lumen-style 6-Axis Surface Cache Card
      dominant axis selection and 2D card atlas UV projection. }
    function GenerateSurfaceCacheCardFunctions: String;

    { Phase 28.2 CPU reference: Selects dominant card axis 0..5 (+X, -X, +Y, -Y, +Z, -Z) for AWorldNormal. }
    class function SelectDominantSurfaceCardAxis(
      const AWorldNormal: TVector3): Cardinal; static;

    { Phase 28.2 CPU reference: Projects local hit point onto card axis tangent plane and maps to atlas UV. }
    class function ComputeSurfaceCacheAtlasUV(
      const ALocalHitPos, ABoxHalfExtents: TVector3;
      const ACardAxis: Cardinal;
      const ACardAtlasOffsetUV, ACardAtlasScaleUV: TVector2): TVector2; static;

    { Phase 28.3: Generates GLSL 450 functions for Screen-Space Radiance Cache bilateral
      probe interpolation and cosine-weighted hemisphere importance sampling. }
    function GenerateScreenRadianceCacheFunctions: String;

    { Phase 28.3 CPU reference: Plane-distance and normal-aware bilateral weight for screen probe interpolation. }
    class function EvaluateScreenProbeBilateralWeight(
      const APixelWorldPos, APixelNormal, AProbeWorldPos, AProbeNormal: TVector3;
      const APlaneDistSensitivity: Single = 10.0): Single; static;

    { Phase 28.3 CPU reference: Cosine-weighted hemisphere sample (Z-up tangent space) with PDF = cos(theta)/pi. }
    class function CosineWeightedHemisphereSample(
      const AXi: TVector2;
      out AOutPdf: Single): TVector3; static;

    { Phase 29.1: Generates GLSL 450 functions and structs for ReSTIR Weighted Reservoir
      Sampling (WRS), Streaming Resampled Importance Sampling (RIS), and Temporal/Spatial
      Reservoir Combination (UE5 MegaLights architecture). }
    function GenerateReSTIRReservoirFunctions: String;

    { Phase 29.2: Generates a GLSL 450 compute shader for ReSTIR Spatio-Temporal Reservoir
      Resampling across temporal history and spatial neighbors with depth/normal edge rejection. }
    function GenerateReSTIRSpatialTemporalComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 29.1 CPU reference: Initializes an empty ReSTIR reservoir (SelectedLightID = -1). }
    class procedure InitReSTIRReservoir(out AReservoir: TCastleReSTIRReservoir); static;

    { Phase 29.1 CPU reference: Evaluates Rec.709 luminance target PDF p_hat(x) for unshadowed light contribution. }
    class function EvaluateLuminanceTargetPDF(
      const AUnshadowedContribution: TVector3): Single; static;

    { Phase 29.1 CPU reference: Streams a single candidate light sample into AReservoir using WRS/RIS. }
    class function UpdateReservoirSample(
      var AReservoir: TCastleReSTIRReservoir;
      const ACandidateLightID: Integer;
      const ATargetPDF, ASourcePDF, ARandom01: Single): Boolean; static;

    { Phase 29.1 CPU reference: Caps SampleCountM to AMaxHistoryM and computes unbiased ContributionWeightW. }
    class procedure FinalizeReservoirWeight(
      var AReservoir: TCastleReSTIRReservoir;
      const AMaxHistoryM: Cardinal = 20); static;

    { Phase 29.2 CPU reference: Returns True when a temporal/spatial neighbor passes depth & normal similarity thresholds. }
    class function EvaluateReSTIRNeighborSimilarity(
      const ACenterDepth: Single;
      const ACenterNormal: TVector3;
      const ANeighborDepth: Single;
      const ANeighborNormal: TVector3;
      const AMaxRelativeDepthDiff: Single = 0.1;
      const AMinNormalCos: Single = 0.9): Boolean; static;

    { Phase 29.2 CPU reference: Combines ANeighbor reservoir into ADst with M-capping and target PDF re-evaluation. }
    class function CombineReservoirs(
      var ADst: TCastleReSTIRReservoir;
      const ANeighbor: TCastleReSTIRReservoir;
      const ANeighborTargetPDFAtCenter: Single;
      const AMaxHistoryM: Cardinal;
      const ARandom01: Single): Boolean; static;

    { Phase 30.1: Generates a GLSL 450 compute shader for Temporal Super Resolution (TSR)
      sub-pixel history upscaling using Lanczos-2 / Blackman-Harris reconstruction kernels,
      parallax disocclusion confidence, and thin-feature anti-ghosting. }
    function GenerateTSRUpscaleComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Phase 30.1 CPU reference: Evaluates windowed Lanczos-2 sinc kernel L(x) = sinc(x)*sinc(x/2) for |x| < 2. }
    class function EvaluateLanczos2Weight(const AX: Single): Single; static;

    { Phase 30.1 CPU reference: Evaluates 4-term Blackman-Harris window for |x| < ARadius. }
    class function EvaluateBlackmanHarrisWeight(
      const AX: Single;
      const ARadius: Single = 1.0): Single; static;

    { Phase 30.2 CPU reference: Evaluates temporal history confidence [0..1] from relative depth mismatch. }
    class function EvaluateTSRDisocclusionConfidence(
      const ACurrentLinearDepth, AReprojectedLinearDepth: Single;
      const AMaxRelativeDepthError: Single = 0.05): Single; static;

    { Phase 30.2 CPU reference: Boosts current-frame responsiveness [0..1] on thin sub-pixel geometry (wires/foliage/hair). }
    class function EvaluateTSRThinGeometryBlend(
      const ABaseCurrentFrameBlend, ADepthCrossLaplacian, ALumaContrast: Single): Single; static;

    { Phase 31.1: Generates GLSL 450 functions for UE5 Substrate/Strata Energy-Conserving
      Layered Material Slabs (directional slab albedo & Beer-Lambert medium transmittance). }
    function GenerateSubstrateLayeredMaterialFunctions: String;

    { Phase 31.1 CPU reference: Evaluates directional slab reflectance albedo E(mu, roughness) in [0..1]. }
    class function EvaluateDirectionalSlabAlbedo(
      const AF0, ANdotV, ARoughness: Single): Single; static;

    { Phase 31.1 CPU reference: Energy-conserving vertical composition of a top coating/medium slab over a bottom substrate slab. }
    class function EvaluateLayeredSlabComposition(
      const ATopLobeRadiance, ABottomLobeRadiance, AExtinctionSigmaT: TVector3;
      const ATopF0, ATopRoughness, ANdotV, ANdotL, ASlabThickness: Single): TVector3; static;

    { Phase 31.2: Generates GLSL 450 functions for Marschner / Chiang Dual-Specular
      Hair & Fur Fiber BSDF (longitudinal R, TT, TRT cuticle-tilted lobes and Beer-Lambert absorption). }
    function GenerateHairMarschnerBSDF: String;

    { Phase 31.2 CPU reference: Normalized longitudinal Gaussian lobe M(theta_h - shift) for hair fibers. }
    class function EvaluateMarschnerLongitudinalM(
      const ASinThetaI, ASinThetaO, ACuticleShiftRad, ALongitudinalRoughnessBeta: Single): Single; static;

    { Phase 31.2 CPU reference: Evaluates combined Marschner R + TT + TRT hair fiber scattering RGB. }
    class function EvaluateHairDualSpecularBSDF(
      const AHairAbsorptionSigmaA: TVector3;
      const ASinThetaI, ASinThetaO, ACosPhiD, ACuticleTiltRad, APrimaryRoughnessBeta: Single): TVector3; static;

    { Phase 31.3: Generates GLSL 450 functions for Stochastic Discrete Microfacet Glints & Sparkles. }
    function GenerateStochasticGlintFunctions: String;

    { Phase 31.3 CPU reference: Evaluates discrete microfacet glint NDF amplification for metallic paint/snow/minerals. }
    class function EvaluateStochasticGlintNDF(
      const ANdotH, AMacroRoughness, AFlakeAlignment01, AGlintDensity, AGlintSharpness: Single): Single; static;

    { Phase 32.1: Generates a GLSL 450 Mesh Shader stage (GL_EXT_mesh_shader) emitting
      meshlet vertices and primitive triangle indices via SetMeshOutputsEXT. }
    function GenerateMeshShaderStageFunctions(
      const AMaxVertices: Cardinal = 64;
      const AMaxPrimitives: Cardinal = 124;
      const AWorkgroupSizeX: Cardinal = 32): String;

    { Phase 32.1 CPU reference: Computes task shader workgroup count for ATotalMeshletCount. }
    class function ComputeMeshletTaskGroupCount(
      const ATotalMeshletCount: Cardinal;
      const AMeshletsPerTaskGroup: Cardinal = 32): Cardinal; static;

    { Phase 32.2: Generates a GLSL 450 compute shader for VK_KHR_fragment_shading_rate
      tile-based Variable Rate Shading (1x1, 2x1, 1x2, 2x2, 4x4) driven by luma contrast & motion. }
    function GenerateVRSContrastComputeShader(
      const ATileSize: Cardinal = 16): String;

    { Phase 32.2 CPU reference: Selects optimal shading rate code (0=1x1, 1=2x1, 4=1x2, 5=2x2, 10=4x4). }
    class function SelectOptimalShadingRateTile(
      const ALumaContrastX, ALumaContrastY, AVelocityPixels: Single;
      const AHighContrastThreshold: Single = 0.20;
      const ALowContrastThreshold: Single = 0.04): Cardinal; static;

    { Phase 32.3: Generates GLSL 450 functions for Runtime Virtual Texturing (RVT)
      virtual-to-physical page translation with bilinear border padding. }
    function GenerateRVTLookupFunctions: String;

    { Phase 32.3 CPU reference: Translates in-page UV [0..1]^2 and physical tile (X,Y) into physical RVT atlas UV. }
    class function TranslateRVTPageToPhysicalUV(
      const AInPageUV: TVector2;
      const APhysicalTileX, APhysicalTileY, APhysicalTilesPerRow: Cardinal;
      const ABorderTexelFraction: Single = 0.03125): TVector2; static;

    { Phase 33.1: Generates GLSL 450 functions for Physically Based Single-Layer Water
      (Beer-Lambert RGB optical absorption, Snell screen-space refraction offset, and photon caustics). }
    function GenerateSingleLayerWaterFunctions: String;

    { Phase 33.1 CPU reference: Evaluates Beer-Lambert spectral water transmittance exp(-sigma_a * depth). }
    class function EvaluateWaterBeerLambertAbsorption(
      const AAbsorptionCoeffRGB: TVector3;
      const AOpticalDepthMeters: Single): TVector3; static;

    { Phase 33.1 CPU reference: Computes depth-faded Snell screen-space refraction UV offset. }
    class function ComputeWaterRefractionUVOffset(
      const AViewSpaceNormal: TVector3;
      const AWaterDepthMeters, AIORRatio, ARefractionStrength: Single): TVector2; static;

    { Phase 33.1 CPU reference: Evaluates animated dual-wave interference underwater photon caustics. }
    class function EvaluateWaterCausticsPattern(
      const AWorldXZ: TVector2;
      const ATimeSeconds, AWaterDepthMeters: Single): Single; static;

    { Phase 33.2: Generates GLSL 450 functions for Sparse Heterogeneous Volume Brick Atlas
      lookup and physically based Planckian Blackbody temperature-to-RGB emission. }
    function GenerateSparseVolumeBlackbodyFunctions: String;

    { Phase 33.2 CPU reference: Converts temperature in Kelvin (e.g. 1000K..10000K) to Planckian blackbody RGB radiance. }
    class function EvaluateBlackbodyEmissionRGB(
      const ATemperatureKelvin: Single;
      const AIntensity: Single = 1.0): TVector3; static;

    { Phase 34.1: Generates GLSL 450 functions for Spatio-Temporal Variance-Guided Filtering (SVGF)
      temporal luminance moment accumulation (E[L], E[L^2]) and variance estimation. }
    function GenerateSVGFVarianceMomentsFunctions: String;

    { Phase 34.1 CPU reference: Evaluates first and second luminance moments (L, L^2) from linear RGB radiance. }
    class function EvaluateLuminanceMoments(const ARadianceRGB: TVector3): TVector2; static;

    { Phase 34.1 CPU reference: Computes temporal variance max(0, M2 - M1^2) from exponentially blended moments. }
    class function ComputeTemporalVariance(const AMoments: TVector2): Single; static;

    { Phase 34.2: Generates GLSL 450 functions for SVGF Edge-Stopping A-Trous Wavelet
      Spatial Filtering (normal, depth-gradient, and variance-guided luminance weights)
      and Specular Ray Hit-Distance Contact Hardening. }
    function GenerateSVGFAttrousWaveletFunctions: String;

    { Phase 34.2 CPU reference: Evaluates combined SVGF edge-stopping weight w_z * w_n * w_l in [0..1]. }
    class function EvaluateSVGFEdgeStoppingWeight(
      const ACenterNormal, ASampleNormal: TVector3;
      const ACenterDepth, ASampleDepth, ADepthGradientDotOffset: Single;
      const ACenterLuma, ASampleLuma, AVariance: Single;
      const APhiDepth: Single = 1.0;
      const APhiNormal: Single = 128.0;
      const APhiLuma: Single = 4.0): Single; static;

    { Phase 34.3 CPU reference: Evaluates contact-hardening reflection kernel factor [0..1] from ray hit distance and roughness. }
    class function EvaluateHitDistanceBlurFactor(
      const ARayHitDistance, ACameraDistance, ARoughness: Single): Single; static;

    { Phase 35.1: Generates GLSL 460 Hardware Ray Tracing Pipeline stages (GL_EXT_ray_tracing:
      raygen, closesthit, miss) invoking traceRayEXT and barycentric hit interpolation. }
    function GenerateRayTracingPipelineStages: String;

    { Phase 35.1 CPU reference: Interpolates a normalized triangle vertex normal using ray-hit barycentrics (bary.xy). }
    class function InterpolateRayHitNormal(
      const AN0, AN1, AN2: TVector3;
      const AHitBarycentricsUV: TVector2): TVector3; static;

    { Phase 35.2 CPU reference: Aligns AValue upward to the next multiple of APowerOfTwoAlignment. }
    class function AlignUpSBTSize(
      const AValue, AAlignment: Cardinal): Cardinal; static;

    { Phase 36.1: Generates a GLSL 450 compute shader for Extended Position-Based Dynamics (XPBD)
      Cloth & Hair Strand Simulation (compliant distance constraints, aerodynamic wind, and SDF collision). }
    function GenerateXPBDClothComputeShader(
      const AWorkgroupSizeX: Cardinal = 64): String;

    { Phase 36.1 CPU reference: Evaluates compliant XPBD distance constraint position correction delta for P1. }
    class function EvaluateXPBDDistanceConstraintDelta(
      const AP1, AP2: TVector3;
      const AInvMass1, AInvMass2, ARestLength, AComplianceAlpha, ASubstepDt: Single): TVector3; static;

    { Phase 36.2 CPU reference: Evaluates aerodynamic normal wind force on a cloth/foliage triangle element. }
    class function EvaluateAerodynamicWindForce(
      const AWindVelocity, AElementVelocity, AElementNormal: TVector3;
      const AElementArea, ADragCoeff: Single;
      const AAirDensity: Single = 1.225): TVector3; static;

    { Phase 36.2 CPU reference: Projects a cloth/hair particle out of a colliding bounding sphere with margin. }
    class function ResolveSphereCollisionConstraint(
      const AParticlePos, ASphereCenter: TVector3;
      const ASphereRadius: Single;
      const ACollisionMargin: Single = 0.01): TVector3; static;

    { Phase 37.1 & 37.2: Generates GLSL 450 Multi-View Stereo VR preamble (GL_EXT_multiview / gl_ViewIndex),
      asymmetric OpenXR eye frustum projection helper, and gaze-tracked radial foveated VRS shading rate. }
    function GenerateMultiviewStereoPreamble: String;

    { Phase 37.1 CPU reference: Computes OpenXR asymmetric eye frustum horizontal projection (XScale, XOffset) from tangent half-angles. }
    class function ComputeAsymmetricEyeProjectionXScaleOffset(
      const ATanLeft, ATanRight: Single): TVector2; static;

    { Phase 37.2 CPU reference: Evaluates gaze-tracked radial foveated shading rate code
      (0 = 1x1 fovea, $5 = 2x2 parafovea, $A = 4x4 periphery) around AGazeCenterUV. }
    class function EvaluateFoveatedShadingRate(
      const AUV, AGazeCenterUV: TVector2;
      const AInnerFoveaRadius: Single = 0.18;
      const AMiddleParafoveaRadius: Single = 0.42): Cardinal; static;

    { Phase 37.3: Generates GLSL 450 HDR10 Rec.2020 Wide Color Gamut conversion and SMPTE ST.2084 (PQ)
      10,000-nit perceptual quantizer transfer functions. }
    function GenerateHDR10PQEncodingFunctions: String;

    { Phase 37.3 CPU reference: Converts linear Rec.709 / sRGB radiance to linear Rec.2020 wide color gamut. }
    class function EvaluateLinearRec709ToRec2020(
      const ALinearRec709: TVector3): TVector3; static;

    { Phase 37.3 CPU reference: Encodes linear luminance in nits [0..10000] with the SMPTE ST.2084 (PQ) transfer function [0..1]. }
    class function EncodeST2084PQ(
      const ALinearNits: TVector3): TVector3; static;

    { Phase 38.1: Generates GLSL 450 Dual-Quaternion Skinning (DQS) functions for volume-preserving joint blending. }
    function GenerateDualQuaternionSkinningFunctions: String;

    { Phase 38.1 CPU reference: Blends two dual quaternions with antipodality sign correction and normalizes the result. }
    class procedure BlendDualQuaternionAntipodal(
      const ADQReal0, ADQDual0, ADQReal1, ADQDual1: TVector4;
      const AW0, AW1: Single;
      out AOutReal, AOutDual: TVector4); static;

    { Phase 38.1 CPU reference: Transforms a 3D vertex position by a normalized dual quaternion (q_real, q_dual). }
    class function EvaluateDualQuaternionTransform(
      const ADQReal, ADQDual: TVector4;
      const APosition: TVector3): TVector3; static;

    { Phase 38.2: Generates GLSL 450 MegaGeometry Displaced Micro-Mesh barycentric tessellation and edge-snapping functions. }
    function GenerateDisplacedMicroMeshFunctions: String;

    { Phase 38.2 CPU reference: Evaluates a micro-mesh vertex position displaced along the barycentrically interpolated normal. }
    class function EvaluateMicroMeshDisplacedPosition(
      const AP0, AP1, AP2, AN0, AN1, AN2: TVector3;
      const ABaryUV: TVector2;
      const AHeightSample, ADisplacementScale: Single;
      const ADisplacementBias: Single = 0.0): TVector3; static;

    { Phase 38.2 CPU reference: Snaps barycentric (u, v) coordinates onto a uniform subdivision grid for crack-free edges. }
    class function SnapMicroMeshBarycentricToGrid(
      const ABaryUV: TVector2;
      const ASubdivisionSegments: Cardinal): TVector2; static;

    { Phase 39.1: Generates GLSL 450 Optical Flow SAD Block Matching & parabolic sub-pixel motion estimation functions. }
    function GenerateOpticalFlowBlockMatchingShader: String;

    { Phase 39.1 CPU reference: Computes Sum of Absolute Differences (SAD) between two 2x2 luma patches. }
    class function EvaluateOpticalFlowPatchSAD(
      const APatchCurr, APatchPrev: TVector4): Single; static;

    { Phase 39.1 CPU reference: Estimates parabolic sub-pixel motion offset [-0.5..0.5]^2 from 5-point SAD neighbors. }
    class function EstimateParabolicSubpixelOffset(
      const ASADLeft, ASADCenter, ASADRight, ASADDown, ASADUp: Single): TVector2; static;

    { Phase 39.2: Generates GLSL 450 Martin Roberts R2 low-discrepancy sequence and golden-ratio spatio-temporal blue-noise functions. }
    function GenerateSpatioTemporalBlueNoiseFunctions: String;

    { Phase 39.2 CPU reference: Evaluates the 2D quasi-random R2 sequence in [0..1)^2 for ASampleIndex. }
    class function EvaluateR2Sequence2D(
      const ASampleIndex: Cardinal): TVector2; static;

    { Phase 39.2 CPU reference: Rotates a spatial blue-noise value [0..1) temporally using the golden ratio conjugate. }
    class function EvaluateTemporalBlueNoiseJitter(
      const ABaseBlueNoise: Single;
      const AFrameIndex: Cardinal): Single; static;

    { Phase 40.1: Generates GLSL 460 Shader Execution Reordering (SER, GL_NV_shader_invocation_reorder)
      coherence hint packing functions to sort divergent ray-tracing threads by hit state, material, and direction octant. }
    function GenerateSERCoherenceHintFunctions: String;

    { Phase 40.1 CPU reference: Packs an 8-bit SER coherence hint key [0..255] from (IsHit, MaterialID[0..15], DirectionOctant[0..7]). }
    class function PackSERCoherenceHintKey(
      const AIsHit: Boolean;
      const AMaterialShadingModelID: Cardinal;
      const ARayDirection: TVector3): Cardinal; static;

    { Phase 40.2: Generates GLSL 450 Dual-Source Color Blending functions (location = 0, index = 0/1)
      for single-pass physically based colored/stained glass reflection and spectral transmission. }
    function GenerateDualSourceGlassTransmissionFunctions: String;

    { Phase 40.2 CPU reference: Evaluates dual-source glass outputs (Src0 additive specular, Src1 multiplicative colored transmittance). }
    class procedure EvaluateDualSourceGlassOutputs(
      const ASpecularLightingRGB, AGlassTintRGB: TVector3;
      const AFresnelF, AGlassOpacity: Single;
      out AOutSpecularAdd, AOutTransmittanceMul: TVector3); static;

    { Phase 41.1: Generates Vulkan 1.4 Dynamic Rendering Local Read (VK_KHR_dynamic_rendering_local_read)
      subpassInput preamble and tile-local G-Buffer deferred lighting evaluation functions. }
    function GenerateDynamicRenderingLocalReadPreamble: String;

    { Phase 41.1 CPU reference: Evaluates tile-local G-Buffer deferred lighting (diffuse + roughness-driven specular)
      without external VRAM bandwidth round-trips. }
    class function EvaluateTileLocalDeferredLighting(
      const AAlbedo, ANormalWS: TVector3;
      const ARoughness: Single;
      const ALightDirWS, AViewDirWS, ALightColor: TVector3): TVector3; static;

    { Phase 41.2: Generates GLSL 450 Subgroup Wave Intrinsics (GL_KHR_shader_subgroup_ballot / arithmetic)
      for lock-free wave-level ballot bit-counting and exclusive prefix-sum reductions. }
    function GenerateSubgroupWaveIntrinsicsFunctions: String;

    { Phase 41.2 CPU reference: Counts active threads (popcount) in a 64-bit subgroup ballot mask. }
    class function EvaluateSubgroupBallotBitCount(const ABallotMask: UInt64): Cardinal; static;

    { Phase 41.2 CPU reference: Computes the exclusive prefix sum (0, v0, v0+v1, v0+v1+v2) across 4 subgroup lanes. }
    class function EvaluateSubgroupExclusivePrefixSum4(
      const AValues: TVector4Cardinal): TVector4Cardinal; static;

    { Phase 42.1: Generates GLSL 450 Buffer Device Address (GL_EXT_buffer_reference2 / VK_KHR_buffer_device_address)
      64-bit GPU pointer definitions for bindless vertex/index/material buffer dereferencing. }
    function GenerateBufferDeviceAddressPreamble: String;

    { Phase 42.1 CPU reference: Aligns a 64-bit GPU VkDeviceAddress upward to AAlignment (buffer_reference_align). }
    class function AlignBufferDeviceAddress(
      const AAddress: UInt64;
      const AAlignment: Cardinal = 16): UInt64; static;

    { Phase 42.2: Generates GLSL 450 Conservative Dominant-Axis Voxelization and 3D Mipmapped Voxel Cone Tracing (VXGI) functions. }
    function GenerateVoxelConeTracingFunctions: String;

    { Phase 42.2 CPU reference: Selects the dominant projection axis (0=X, 1=Y, 2=Z) that maximizes projected triangle area. }
    class function SelectDominantVoxelProjectionAxis(
      const AFaceNormal: TVector3): Cardinal; static;

    { Phase 42.2 CPU reference: Evaluates voxel cone diameter (X) and 3D texture mip level (Y) at ADistanceAlongCone. }
    class function EvaluateVoxelConeStepAperture(
      const ADistanceAlongCone, AConeHalfAngleTan, ABaseVoxelSize: Single): TVector2; static;

    { Phase 43.1: Generates GLSL 450 Normal-Variance Specular Anti-Aliasing (Kaplanyan & Antonov / Toksvig)
      functions to filter sub-pixel normal derivatives into GGX roughness and eliminate distant specular aliasing. }
    function GenerateSpecularNormalVarianceAAFunctions: String;

    { Phase 43.1 CPU reference: Evaluates filtered GGX perceptual roughness in [0..1] from base roughness and screen-space normal derivatives dNdU, dNdV. }
    class function EvaluateSpecularAARoughness(
      const ABaseRoughness: Single;
      const ADNdU, ADNdV: TVector3;
      const AVarianceStrength: Single = 0.5;
      const AMaxVarianceThreshold: Single = 0.18): Single; static;

    { Phase 43.2: Generates GLSL 450 Rectilinear Shadow Map Warping functions to concentrate shadow map
      texel density near the camera focus region while keeping [0..1]^2 boundaries invariant. }
    function GenerateRectilinearShadowWarpFunctions: String;

    { Phase 43.2 CPU reference: Warps normalized shadow map UV [0..1]^2 using rectilinear warp strength AWarpStrength >= 0. }
    class function EvaluateRectilinearShadowWarpUV(
      const AShadowUV: TVector2;
      const AWarpStrength: Single): TVector2; static;

    { Phase 44.1: Generates GLSL 450 Multi-Channel Signed Distance Field (MSDF, Chlumsky median-of-three)
      evaluation functions for sharp corner-preserving vector shapes and 3D text at arbitrary magnification. }
    function GenerateMSDFTextFunctions: String;

    { Phase 44.1 CPU reference: Evaluates the median of three MSDF RGB channels. }
    class function EvaluateMSDFMedianOfThree(
      const AR, AG, AB: Single): Single; static;

    { Phase 44.1 CPU reference: Evaluates anti-aliased screen-space glyph opacity in [0..1] from MSDF RGB and screen pixel range. }
    class function EvaluateMSDFScreenPxOpacity(
      const AMSDFSampleRGB: TVector3;
      const AScreenPxRange: Single): Single; static;

    { Phase 44.2: Generates GLSL 450 Procedural Quadratic Bezier Grass Blade position and tangent evaluation
      functions for GPU-instanced foliage and meadows. }
    function GenerateProceduralGrassBladeFunctions: String;

    { Phase 44.2 CPU reference: Evaluates quadratic Bezier grass blade position B(t) and normalized tangent B'(t) for t in [0..1]. }
    class procedure EvaluateQuadraticBezierGrassBlade(
      const AP0, AP1, AP2: TVector3;
      const AT: Single;
      out AOutPosition, AOutTangent: TVector3); static;

    { Phase 45.1: Generates Vulkan 1.4 Push Descriptor (`VK_KHR_push_descriptor`) GLSL 450 set layout preamble
      for zero-allocation command-buffer descriptor updates. }
    function GeneratePushDescriptorPreamble(const ASetIndex: Cardinal = 1): String;

    { Phase 45.1 CPU reference: Computes the byte size required for a Vulkan 1.4 Host Image Copy (`VK_EXT_host_image_copy`)
      subresource transfer with aligned row pitch. }
    class function ComputeHostImageCopyByteSize(
      const AWidth, AHeight, ADepth, ABytesPerTexel: Cardinal;
      const ARowAlignmentBytes: Cardinal = 1): UInt64; static;

    { Phase 45.2: Generates GLSL 450 Spherical Gaussian (SG) lobe evaluation, inner product, and spherical integral
      functions for real-time anisotropic specular and diffuse probe lighting. }
    function GenerateSphericalGaussianFunctions: String;

    { Phase 45.2 CPU reference: Evaluates the exact product of two normalized Spherical Gaussian lobes. }
    class procedure EvaluateSphericalGaussianProduct(
      const AAxis1: TVector3; const ASharpness1: Single;
      const AAxis2: TVector3; const ASharpness2: Single;
      out AProductAxis: TVector3;
      out AProductSharpness, ALogAmplitudeScale: Single); static;

    { Phase 45.2 CPU reference: Evaluates the exact integral of a Spherical Gaussian lobe over the unit sphere S^2. }
    class function EvaluateSphericalGaussianIntegral(
      const AAmplitude: TVector3;
      const ASharpness: Single): TVector3; static;

    { Phase 46.1: Generates GLSL 450 3D Gaussian Splatting (3DGS) EWA 2D covariance conic projection
      and Mahalanobis opacity falloff evaluation functions. }
    function GenerateGaussianSplattingFunctions: String;

    { Phase 46.1 CPU reference: Invert a 2D symmetric covariance matrix (CovXX, CovXY, CovYY) with low-pass dilation
      into conic coefficients (ConicX, ConicY, ConicZ) and evaluate Gaussian splat opacity at PixelOffset. }
    class function EvaluateGaussianSplatOpacity(
      const ACov2D: TVector3;
      const APixelOffset: TVector2;
      const ABaseOpacity: Single): Single; static;

    { Phase 46.2 CPU reference: Evaluates degree-1 (L0 + L1) view-dependent Spherical Harmonics RGB color
      clamped to non-negative radiance for 3D Gaussian Splats. }
    class function EvaluateSphericalHarmonicsL1RGB(
      const ASH0, ASH1X, ASH1Y, ASH1Z: TVector3;
      const AViewDir: TVector3): TVector3; static;

    { Phase 47.1: Generates GLSL 450 Percentage-Closer Soft Shadows (PCSS) blocker search
      and contact-hardening penumbra width evaluation functions. }
    function GeneratePCSSSoftShadowFunctions: String;

    { Phase 47.1 CPU reference: Evaluates PCSS contact-hardening penumbra filter width
      (d_receiver - d_blocker) * lightWorldSize / d_blocker. }
    class function EvaluatePCSSPenumbraWidth(
      const AReceiverDepth, AAvgBlockerDepth, ALightSize: Single): Single; static;

    { Phase 47.2 CPU reference: Computes hardware-aligned byte offset into a Vulkan Descriptor Buffer (`VK_EXT_descriptor_buffer`). }
    class function ComputeDescriptorBufferAlignedOffset(
      const ASetIndex: Cardinal;
      const ASetByteSize: UInt64;
      const AOffsetAlignment: UInt64 = 64): UInt64; static;

    { Phase 48.1: Generates GLSL 450 Multi-Layer Metallic Flake & Clear-Coat Car-Paint BRDF functions. }
    function GenerateCarPaintFlakeFunctions: String;

    { Phase 48.1 CPU reference: Evaluates multi-layer automotive car-paint radiance combining
      view-dependent two-tone base coat, metallic flake sparkle lobe, and dielectric clear coat Fresnel. }
    class function EvaluateCarPaintMultiLayerBRDF(
      const ABaseColorNormal, ABaseColorGrazing, AFlakeColor: TVector3;
      const ANdotV, ANdotH, AFlakeNdotH, AFlakeExponent: Single): TVector3; static;

    { Phase 48.2: Generates GLSL 450 Screen-Space Normal Curvature & Ridge/Valley Cavity Shading functions. }
    function GenerateScreenSpaceCurvatureFunctions: String;

    { Phase 48.2 CPU reference: Evaluates screen-space ridge/valley cavity multiplier in [0..2]
      from horizontal and vertical view-normal derivatives. }
    class function EvaluateScreenSpaceCurvatureCavity(
      const ADNormalDx, ADNormalDy: TVector3;
      const ACurvatureScale: Single): Single; static;

    { Phase 49.1: Generates GLSL 450 Physically Based Camera Exposure (EV100) functions. }
    function GenerateCameraEV100ExposureFunctions: String;

    { Phase 49.1 CPU reference: Computes photographic EV100 from aperture f-stops (N),
      shutter time in seconds (t), and sensor ISO sensitivity (S):
      EV100 = log2((N^2 / t) * (100.0 / S)). }
    class function EvaluateEV100FromCameraSettings(
      const AApertureFStops, AShutterSeconds, AISO: Single): Single; static;

    { Phase 49.1 CPU reference: Computes ISO 12232 saturation-based linear exposure
      multiplier from EV100: 1.0 / (1.2 * 2^EV100). }
    class function EvaluateExposureMultiplierFromEV100(
      const AEV100: Single): Single; static;

    { Phase 49.2: Generates GLSL 450 AgX Filmic Log Encoding & Highlight Chroma Gamut Compression functions. }
    function GenerateAgXGamutCompressionFunctions: String;

    { Phase 49.2 CPU reference: Compresses out-of-gamut high-intensity saturated RGB colors
      toward their Rec.709 luminance achromatic axis to prevent hard channel clipping. }
    class function EvaluateChromaGamutCompression(
      const ALinearRGB: TVector3;
      const ACompressionStrength: Single): TVector3; static;

    { Phase 50.1: Generates GLSL 450 Analytical Capsule Soft Shadow & Ambient Occlusion functions. }
    function GenerateCapsuleOcclusionFunctions: String;

    { Phase 50.1 CPU reference: Evaluates analytical soft shadow visibility in [0..1]
      (1 = unoccluded, 0 = fully occluded) for a ray from AShadingPos along ALightDir
      against a capsule segment [ACapsuleA, ACapsuleB] with ACapsuleRadius and AConeTang. }
    class function EvaluateCapsuleSoftShadowOcclusion(
      const AShadingPos, ALightDir, ACapsuleA, ACapsuleB: TVector3;
      const ACapsuleRadius, AConeTang: Single): Single; static;

    { Phase 50.2: Generates GLSL 450 Epipolar Volumetric Light Shafts (Crepuscular God Rays) functions. }
    function GenerateEpipolarLightShaftFunctions: String;

    { Phase 50.2 CPU reference: Evaluates the 2D screen-space sample UV along an epipolar
      slice ray from ALightScreenUV to ASliceEndpointUV at normalized step ASliceFraction in [0..1]. }
    class function EvaluateEpipolarSliceRayStep(
      const ALightScreenUV, ASliceEndpointUV: TVector2;
      const ASliceFraction: Single): TVector2; static;

    { Phase 51.1: Generates GLSL 450 Cooperative Matrix / Tensor Core preamble (`GL_KHR_cooperative_matrix`). }
    function GenerateCooperativeMatrixPreamble: String;

    { Phase 51.1 CPU reference: Decodes an 8-bit E4M3 floating-point byte (1 sign bit,
      4 exponent bits with bias 7, 3 mantissa bits) into a 32-bit IEEE 754 Single. }
    class function EvaluateQuantizedFP8ToFloat(
      const AFP8Byte: Byte): Single; static;

    { Phase 51.2: Generates GLSL 450 Anisotropic Kuwahara Structure-Tensor functions. }
    function GenerateAnisotropicKuwaharaFunctions: String;

    { Phase 51.2 CPU reference: Computes the principal orientation angle in radians
      [-Pi/2..Pi/2] and normalized eigenvalue anisotropy in [0..1] from structure
      tensor components (AJxx, AJyy, AJxy). }
    class function EvaluateStructureTensorEigenOrientation(
      const AJxx, AJyy, AJxy: Single;
      out AAnisotropy: Single): Single; static;

    { Phase 52.1: Generates GLSL 450 Physically Based Eye Cornea Snell Refraction & Iris Parallax functions. }
    function GeneratePhysicalEyeRefractionFunctions: String;

    { Phase 52.1 CPU reference: Evaluates the 2D iris UV parallax offset caused by
      Snell refraction at the cornea surface (ACorneaIOR) across AAnteriorChamberDepth. }
    class function EvaluateEyeIrisRefractionUVOffset(
      const AViewDirTS, ACorneaNormalTS: TVector3;
      const ACorneaIOR, AAnteriorChamberDepth: Single): TVector2; static;

    { Phase 52.2: Generates GLSL 450 Six-Point Directional Lightmap Basis functions for volumetric smoke/clouds. }
    function GenerateSixPointLightmapFunctions: String;

    { Phase 52.2 CPU reference: Evaluates 6-axis directional lightmap irradiance
      (+X, -X, +Y in AMapPosXNegXPosY; -Y, +Z, -Z in AMapNegYPosZNegZ) for ALightDirTS. }
    class function EvaluateSixPointDirectionalBasis(
      const ALightDirTS, AMapPosXNegXPosY, AMapNegYPosZNegZ: TVector3): Single; static;

    { Phase 53.1: Generates GLSL 450 Secant-Refined Parallax Occlusion Mapping (POM) functions. }
    function GenerateParallaxOcclusionMappingFunctions: String;

    { Phase 53.1 CPU reference: Computes the secant-interpolated ray-heightfield
      intersection depth between two POM step layers. }
    class function EvaluateSecantRefinedPOMIntersection(
      const APrevLayerDepth, ACurrLayerDepth, APrevHeight, ACurrHeight: Single): Single; static;

    { Phase 53.2: Generates GLSL 450 Spectral Cauchy Chromatic Dispersion & Lens Flare functions. }
    function GenerateChromaticDispersionFlareFunctions: String;

    { Phase 53.2 CPU reference: Evaluates wavelength-dependent Cauchy refractive indices
      n(lambda) = A + B / lambda^2 for Red (0.65 um), Green (0.55 um), and Blue (0.45 um). }
    class function EvaluateCauchyRefractiveIndexRGB(
      const ACoeffA, ACoeffBMicronsSq: Single): TVector3; static;

    { Phase 58.1: Builds the 128-byte per-draw TCastlePushConstantsBlock for a shape,
      packing ModelViewMatrix, std140-padded 3x3 NormalMatrix rows, and ObjectParams:
      - ObjectParams.X: Bindless material index (ABindlessMaterialIndex)
      - ObjectParams.Y: Skinning mode (0.0 = static mesh, 1.0 = Linear Blend Skinning, 2.0 = Dual-Quaternion Skinning)
      - ObjectParams.Z: Specular Anti-Aliasing flag (1.0 = enabled, 0.0 = disabled)
      - ObjectParams.W: ShadowMapSystemMode ordinal (0 = PCF, 1 = PCSS, 2 = VSM) }
    class function BuildShapeRHIPushConstants(
      const AShapeModelView: TMatrix4;
      const ANormalMatrix: TMatrix3;
      const AHasSkinJoints, ADualQuaternionSkinning, ASpecularAntiAliasing: Boolean;
      const AShadowMapSystemModeOrd: Cardinal = 0;
      const ABindlessMaterialIndex: Cardinal = 0): TCastlePushConstantsBlock; static;

    { Phase 59.1: Unpacks ObjectParams from a TCastlePushConstantsBlock into
      ABindlessMaterialIndex, ASkinningMode (0 = static, 1 = LBS, 2 = Dual-Quaternion),
      ASpecularAntiAliasing, and AShadowMapSystemModeOrd (0 = PCF, 1 = PCSS, 2 = VSM),
      mirroring the GLSL 450 castle_Object* macros. }
    class procedure UnpackShapeRHIPushConstantsObjectParams(
      const APushConstants: TCastlePushConstantsBlock;
      out ABindlessMaterialIndex, ASkinningMode: Cardinal;
      out ASpecularAntiAliasing: Boolean;
      out AShadowMapSystemModeOrd: Cardinal); static;

    { Phase 111: Generates a procedural fullscreen-triangle GLSL 450 vertex shader
      for HDR Tonemapping & Bloom Composite (compatible with CGE negative-height VkViewport). }
    function GenerateHDRCompositeVertexShader: String;

    { Phase 111: Generates a GLSL 450 compute shader that modulates scene HDR radiance
      by denoised GTAO ambient occlusion and adds Hi-Z SSR reflections. }
    function GeneratePostProcessModulateComputeShader(
      const AWorkgroupSizeX: Cardinal = 8;
      const AWorkgroupSizeY: Cardinal = 8): String;

    { Computes the 64-bit FNV-1a hash of the final GLSL source for on-disk SPIR-V caching }
    class function ComputeSourceHash(const FinalGLSL: String): UInt64; static;
  end;

implementation

const
  NL = LineEnding;

{ TCastleLightsUniformBlock }

procedure TCastleLightsUniformBlock.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

function TCastleLightsUniformBlock.AddLightInternal(const APos, AColor, ASpotDir,
  ASpotParams, AAtten, AAmbient: TVector4): Integer;
begin
  if Count >= CASTLE_MAX_LIGHTS then Exit(-1);
  Result := Count;
  Lights[Result].Position := APos;
  Lights[Result].Color := AColor;
  Lights[Result].SpotDirection := ASpotDir;
  Lights[Result].SpotParams := ASpotParams;
  Lights[Result].Attenuation := AAtten;
  Lights[Result].AmbientColor := AAmbient;
  Lights[Result].ProjectorMatrix := TMatrix4.Identity;
  Inc(Count);
end;

function TCastleLightsUniformBlock.AddDirectionalLight(const Direction, Color,
  AmbientColor: TVector3): Integer;
begin
  Result := AddLightInternal(Vector4(Direction, 0.0), Vector4(Color, 0.0),
    Vector4(0, 0, -1, -1.0), Vector4(0, 0, 0, 0), Vector4(1, 0, 0, 0), Vector4(AmbientColor, 1.0));
end;

function TCastleLightsUniformBlock.AddPointLight(const Position, Color,
  AmbientColor: TVector3; const Attenuation: TVector3; const Radius: Single): Integer;
begin
  Result := AddLightInternal(Vector4(Position, 1.0), Vector4(Color, Radius),
    Vector4(0, 0, -1, -1.0), Vector4(0, 0, 0, 0), Vector4(Attenuation, 0.0), Vector4(AmbientColor, 1.0));
end;

function TCastleLightsUniformBlock.AddSpotLight(const Position, Direction, Color,
  AmbientColor: TVector3; const Attenuation: TVector3; const Radius, SpotCosCutoff,
  SpotCutoff, BeamWidth, SpotExponent: Single): Integer;
begin
  Result := AddLightInternal(Vector4(Position, 2.0), Vector4(Color, Radius),
    Vector4(Direction, SpotCosCutoff), Vector4(SpotCutoff, BeamWidth, SpotExponent, 0.0),
    Vector4(Attenuation, 0.0), Vector4(AmbientColor, 1.0));
end;

procedure TCastleLightsUniformBlock.SetProjectorMatrix(const Index: Integer;
  const Matrix: TMatrix4; const AShadowSlotIndex: Integer);
begin
  if (Index >= 0) and (Index < Integer(Count)) then
  begin
    Lights[Index].ProjectorMatrix := Matrix;
    Lights[Index].SpotParams.W := Single(EnsureRange(AShadowSlotIndex, 0, 3) + 1);
  end;
end;

{ TCastleMaterialUniformBlock }

procedure TCastleMaterialUniformBlock.InitDefaultPBR;
begin
  BaseColor := Vector4(1.0, 1.0, 1.0, 1.0);
  SpecularColor := Vector4(0.04, 0.04, 0.04, 0.0); // a = 0.0 -> PBR Metallic-Roughness
  EmissiveColor := Vector4(0.0, 0.0, 0.0, 0.0);    // a = 0.0 -> Metallic = 0
  PBRParams := Vector4(1.0, 1.0, 1.0, 0.0);        // Roughness=1, NormalScale=1, OcclusionStrength=1, AlphaCutoff=0
end;

procedure TCastleMaterialUniformBlock.InitDefaultPhong;
begin
  BaseColor := Vector4(0.8, 0.8, 0.8, 1.0);
  SpecularColor := Vector4(0.0, 0.0, 0.0, 25.6);   // a > 0.0 -> Phong shininess (0.2 * 128)
  EmissiveColor := Vector4(0.0, 0.0, 0.0, 0.0);
  PBRParams := Vector4(0.5, 1.0, 1.0, 0.0);
end;

procedure TCastleMaterialUniformBlock.InitDefaultUnlit;
begin
  BaseColor := Vector4(1.0, 1.0, 1.0, 1.0);
  SpecularColor := Vector4(0.0, 0.0, 0.0, -1.0);   // a < 0.0 -> Unlit shading model
  EmissiveColor := Vector4(0.0, 0.0, 0.0, 0.0);
  PBRParams := Vector4(1.0, 1.0, 1.0, 0.0);
end;

{ TCastleShaderGeneratorRHI }

constructor TCastleShaderGeneratorRHI.Create(const ABackend: TRHIBackendType);
begin
  inherited Create;
  FBackend := ABackend;
end;

function TCastleShaderGeneratorRHI.GenerateVersionAndLayoutMacros: String;
begin
  Result := GenerateVersionAndLayoutMacrosForStage(ssVertex);
end;

function TCastleShaderGeneratorRHI.GenerateVersionAndLayoutMacrosForStage(
  const AStage: TRHIShaderStage): String;

  function BuildOpenGLSlotMacros: String;
  var
    S, B: Integer;
  begin
    Result := '';
    for S := 0 to 3 do
      for B := 0 to 7 do
        Result := Result + Format('#define CASTLE_GL_SLOT_%d_%d %d', [S, B, S * 8 + B]) + NL;
    Result := Result +
      '#define CASTLE_GL_BIND2(SetIdx, BindIdx) CASTLE_GL_SLOT_ ## SetIdx ## _ ## BindIdx' + NL +
      '#define CASTLE_GL_BIND(SetIdx, BindIdx) CASTLE_GL_BIND2(SetIdx, BindIdx)' + NL;
  end;

begin
  case FBackend of
    rbVulkan:
      begin
        Result :=
          '#version 450' + NL;
        if AStage = ssFragment then
          Result := Result +
            'precision highp float;' + NL +
            'precision highp int;' + NL;
        Result := Result +
          '#define CASTLE_VULKAN 1' + NL +
          '#define CASTLE_UBO(SetIdx, BindIdx) layout(std140, set = SetIdx, binding = BindIdx)' + NL +
          '#define CASTLE_SSBO(SetIdx, BindIdx) layout(std430, set = SetIdx, binding = BindIdx)' + NL +
          '#define CASTLE_SAMPLER(SetIdx, BindIdx) layout(set = SetIdx, binding = BindIdx)' + NL +
          '#define CASTLE_DESCRIPTOR_SET(SetIdx, BindIdx) layout(set = SetIdx, binding = BindIdx)' + NL +
          '#define CASTLE_IMAGE(SetIdx, BindIdx, ImgFmt) layout(ImgFmt, set = SetIdx, binding = BindIdx)' + NL +
          '#define CASTLE_PUSH_CONSTANT layout(push_constant)' + NL +
          '#define CASTLE_LOCATION(LocIdx) layout(location = LocIdx)' + NL +
          '#define CASTLE_VERTEX_INDEX gl_VertexIndex' + NL +
          '#define CASTLE_INSTANCE_INDEX gl_InstanceIndex' + NL;
      end;

    rbOpenGL:
      begin
        if AStage = ssCompute then
          Result := '#version 430 core' + NL
        else
          Result :=
            '#version 330 core' + NL +
            '#extension GL_ARB_separate_shader_objects : enable' + NL +
            '#extension GL_ARB_shading_language_420pack : enable' + NL +
            '#extension GL_ARB_explicit_attrib_location : enable' + NL +
            '#extension GL_ARB_shader_storage_buffer_object : enable' + NL +
            '#extension GL_ARB_shader_image_load_store : enable' + NL;
        Result := Result +
          '#define CASTLE_OPENGL 1' + NL +
          BuildOpenGLSlotMacros +
          { Flattens (SetIdx, BindIdx) to literal integer slot SetIdx * 8 + BindIdx }
          '#define CASTLE_UBO(SetIdx, BindIdx) layout(std140, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
          '#define CASTLE_SSBO(SetIdx, BindIdx) layout(std430, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
          '#define CASTLE_SAMPLER(SetIdx, BindIdx) layout(binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
          '#define CASTLE_DESCRIPTOR_SET(SetIdx, BindIdx) layout(binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
          '#define CASTLE_IMAGE(SetIdx, BindIdx, ImgFmt) layout(ImgFmt, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
          { Maps Push Constants to reserved UBO slot 24 in OpenGL }
          '#define CASTLE_PUSH_CONSTANT layout(std140, binding = 24)' + NL +
          '#define CASTLE_LOCATION(LocIdx) layout(location = LocIdx)' + NL +
          '#define CASTLE_VERTEX_INDEX gl_VertexID' + NL +
          '#define CASTLE_INSTANCE_INDEX gl_InstanceID' + NL;
      end;

    rbOpenGLES:
      begin
        if AStage = ssCompute then
          Result :=
            '#version 310 es' + NL +
            'precision highp float;' + NL +
            'precision highp int;' + NL +
            '#define CASTLE_OPENGLES 1' + NL +
            BuildOpenGLSlotMacros +
            '#define CASTLE_UBO(SetIdx, BindIdx) layout(std140, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
            '#define CASTLE_SSBO(SetIdx, BindIdx) layout(std430, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
            '#define CASTLE_SAMPLER(SetIdx, BindIdx) layout(binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
            '#define CASTLE_DESCRIPTOR_SET(SetIdx, BindIdx) layout(binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
            '#define CASTLE_IMAGE(SetIdx, BindIdx, ImgFmt) layout(ImgFmt, binding = CASTLE_GL_BIND(SetIdx, BindIdx))' + NL +
            '#define CASTLE_PUSH_CONSTANT layout(std140, binding = 24)' + NL +
            '#define CASTLE_LOCATION(LocIdx) layout(location = LocIdx)' + NL +
            '#define CASTLE_VERTEX_INDEX gl_VertexID' + NL +
            '#define CASTLE_INSTANCE_INDEX gl_InstanceID' + NL
        else
          Result :=
            '#version 300 es' + NL +
            'precision highp float;' + NL +
            'precision highp int;' + NL +
            '#define CASTLE_OPENGLES 1' + NL +
            '#define CASTLE_UBO(SetIdx, BindIdx) layout(std140)' + NL +
            '#define CASTLE_SSBO(SetIdx, BindIdx) layout(std430)' + NL +
            '#define CASTLE_SAMPLER(SetIdx, BindIdx)' + NL +
            '#define CASTLE_DESCRIPTOR_SET(SetIdx, BindIdx)' + NL +
            '#define CASTLE_IMAGE(SetIdx, BindIdx, ImgFmt) layout(ImgFmt)' + NL +
            '#define CASTLE_PUSH_CONSTANT layout(std140)' + NL +
            '#define CASTLE_LOCATION(LocIdx) layout(location = LocIdx)' + NL +
            '#define CASTLE_VERTEX_INDEX gl_VertexID' + NL +
            '#define CASTLE_INSTANCE_INDEX gl_InstanceID' + NL;
      end;
  end;
end;

function TCastleShaderGeneratorRHI.GenerateStandardUniformBlocks(
  const AStage: TRHIShaderStage): String;
begin
  Result := GenerateStandardUniformBlocksForStage(AStage, True);
end;

function TCastleShaderGeneratorRHI.GenerateStandardUniformBlocksForStage(
  const AStage: TRHIShaderStage;
  const AIncludeSkinningUBO: Boolean): String;
var
  LightAliases, SkinningSection: String;
  I: Integer;
begin
  LightAliases := '';
  for I := 0 to CASTLE_MAX_LIGHTS - 1 do
  begin
    LightAliases := LightAliases +
      Format('#define castle_LightSource%dPosition CastleLights.u_Lights[%d].Position.xyz', [I, I]) + NL +
      Format('#define castle_LightSource%dColor CastleLights.u_Lights[%d].Color.rgb', [I, I]) + NL +
      Format('#define castle_LightSource%dRadius CastleLights.u_Lights[%d].Color.w', [I, I]) + NL +
      Format('#define castle_LightSource%dSpotDirection CastleLights.u_Lights[%d].SpotDirection.xyz', [I, I]) + NL +
      Format('#define castle_LightSource%dSpotCosCutoff CastleLights.u_Lights[%d].SpotDirection.w', [I, I]) + NL +
      Format('#define castle_LightSource%dSpotCutoff CastleLights.u_Lights[%d].SpotParams.x', [I, I]) + NL +
      Format('#define castle_LightSource%dBeamWidth CastleLights.u_Lights[%d].SpotParams.y', [I, I]) + NL +
      Format('#define castle_LightSource%dSpotExponent CastleLights.u_Lights[%d].SpotParams.z', [I, I]) + NL +
      Format('#define castle_LightSource%dAttenuation CastleLights.u_Lights[%d].Attenuation.xyz', [I, I]) + NL +
      Format('#define castle_LightSource%dAmbientColor CastleLights.u_Lights[%d].AmbientColor.rgb', [I, I]) + NL +
      Format('#define castle_projector_matrix_%d CastleLights.u_Lights[%d].ProjectorMatrix', [I, I]) + NL;
  end;

  if AIncludeSkinningUBO then
    SkinningSection :=
      '/* --- Set 2: Per-Object / Skinning Uniforms --- */' + NL +
      'CASTLE_UBO(2, 0) uniform CastleSkinningBlock {' + NL +
      '  mat4 u_JointMatrices[256];' + NL +
      '  uint u_JointCount;' + NL +
      '} CastleSkinning;' + NL +
      NL +
      '#define castle_JointMatrix CastleSkinning.u_JointMatrices' + NL +
      NL
  else
    SkinningSection := '';

  Result :=
    '/* --- Set 0, Binding 0: Global / Per-Pass Uniforms --- */' + NL +
    'CASTLE_UBO(0, 0) uniform CastleSceneBlock {' + NL +
    '  mat4 u_ProjectionMatrix;' + NL +
    '  mat4 u_ViewMatrix;' + NL +
    '  mat4 u_ViewMatrixInverse;' + NL +
    '  vec4 u_CameraPosition;' + NL +
    '  vec4 u_FogColor;' + NL +
    '  vec4 u_FogParams;' + NL +
    '  uint u_LightCount;' + NL +
    '} CastleScene;' + NL +
    NL +
    '/* Legacy CGE uniform aliases for Set 0 */' + NL +
    '#define castle_ProjectionMatrix CastleScene.u_ProjectionMatrix' + NL +
    '#define castle_ViewMatrix CastleScene.u_ViewMatrix' + NL +
    '#define castle_ViewMatrixInverse CastleScene.u_ViewMatrixInverse' + NL +
    '#define castle_CameraMatrix CastleScene.u_ViewMatrix' + NL +
    '#define castle_CameraInverseMatrix CastleScene.u_ViewMatrixInverse' + NL +
    '#define castle_CameraRotationMatrix mat3(CastleScene.u_ViewMatrix)' + NL +
    '#define castle_CameraRotationInverseMatrix mat3(CastleScene.u_ViewMatrixInverse)' + NL +
    '#define castle_WorldEyePosition CastleScene.u_CameraPosition.xyz' + NL +
    '#define castle_SceneTime CastleScene.u_CameraPosition.w' + NL +
    NL +
    '/* --- Set 0, Binding 1: Multi-Light Uniform Block --- */' + NL +
    'struct CastleLightData {' + NL +
    '  vec4 Position;' + NL +
    '  vec4 Color;' + NL +
    '  vec4 SpotDirection;' + NL +
    '  vec4 SpotParams;' + NL +
    '  vec4 Attenuation;' + NL +
    '  vec4 AmbientColor;' + NL +
    '  mat4 ProjectorMatrix;' + NL +
    '};' + NL +
    NL +
    'CASTLE_UBO(0, 1) uniform CastleLightsBlock {' + NL +
    '  uint u_LightCount;' + NL +
    '  uint u_Pad0;' + NL +
    '  uint u_Pad1;' + NL +
    '  uint u_Pad2;' + NL +
    '  CastleLightData u_Lights[32];' + NL +
    '} CastleLights;' + NL +
    NL +
    '#define castle_LightCount CastleLights.u_LightCount' + NL +
    LightAliases + NL +
    '/* --- Set 0, Bindings 2..5: Shadow Map Samplers --- */' + NL +
    'CASTLE_SAMPLER(0, 2) uniform sampler2DShadow castle_shadow_map_0;' + NL +
    'CASTLE_SAMPLER(0, 3) uniform sampler2DShadow castle_shadow_map_1;' + NL +
    'CASTLE_SAMPLER(0, 4) uniform sampler2DShadow castle_shadow_map_2;' + NL +
    'CASTLE_SAMPLER(0, 5) uniform sampler2DShadow castle_shadow_map_3;' + NL +
    NL +
    '/* --- Set 1: Per-Material Uniforms & Samplers --- */' + NL +
    'CASTLE_UBO(1, 0) uniform CastleMaterialBlock {' + NL +
    '  vec4 u_BaseColor;' + NL +
    '  vec4 u_SpecularColor;' + NL +
    '  vec4 u_EmissiveColor;' + NL +
    '  vec4 u_PBRParams;' + NL +
    '} CastleMaterial;' + NL +
    NL +
    '/* Legacy CGE uniform aliases for Set 1 */' + NL +
    '#define castle_MaterialDiffuseAlpha CastleMaterial.u_BaseColor' + NL +
    '#define castle_MaterialBaseColor CastleMaterial.u_BaseColor' + NL +
    '#define castle_MaterialSpecular CastleMaterial.u_SpecularColor.rgb' + NL +
    '#define castle_MaterialShininess CastleMaterial.u_SpecularColor.a' + NL +
    '#define castle_MaterialEmissive CastleMaterial.u_EmissiveColor.rgb' + NL +
    '#define castle_MaterialMetallic CastleMaterial.u_EmissiveColor.a' + NL +
    '#define castle_MaterialRoughness CastleMaterial.u_PBRParams.x' + NL +
    '#define castle_MaterialNormalScale CastleMaterial.u_PBRParams.y' + NL +
    '#define castle_MaterialOcclusionStrength CastleMaterial.u_PBRParams.z' + NL +
    '#define castle_AlphaCutoff CastleMaterial.u_PBRParams.w' + NL +
    NL +
    'CASTLE_SAMPLER(1, 1) uniform sampler2D castle_base_color_texture;' + NL +
    'CASTLE_SAMPLER(1, 2) uniform sampler2D castle_normal_texture;' + NL +
    'CASTLE_SAMPLER(1, 3) uniform sampler2D castle_metallic_roughness_texture;' + NL +
    'CASTLE_SAMPLER(1, 4) uniform sampler2D castle_emissive_texture;' + NL +
    'CASTLE_SAMPLER(1, 5) uniform sampler2D castle_occlusion_texture;' + NL +
    'CASTLE_SAMPLER(1, 6) uniform sampler2D castle_brdf_lut_texture;' + NL +
    'CASTLE_SAMPLER(1, 7) uniform samplerCube castle_env_specular_texture;' + NL +
    NL +
    SkinningSection +
    '/* --- Push Constants: Per-Draw Matrices --- */' + NL +
    'CASTLE_PUSH_CONSTANT uniform CastlePushConstants {' + NL +
    '  mat4 u_ModelViewMatrix;' + NL +
    '  vec4 u_NormalMatrixRow0;' + NL +
    '  vec4 u_NormalMatrixRow1;' + NL +
    '  vec4 u_NormalMatrixRow2;' + NL +
    '  vec4 u_ObjectParams;' + NL +
    '} CastleDraw;' + NL +
    NL +
    '#define castle_ModelViewMatrix CastleDraw.u_ModelViewMatrix' + NL +
    '#define castle_NormalMatrix mat3(CastleDraw.u_NormalMatrixRow0.xyz, CastleDraw.u_NormalMatrixRow1.xyz, CastleDraw.u_NormalMatrixRow2.xyz)' + NL +
    '#define castle_ObjectParams CastleDraw.u_ObjectParams' + NL +
    '#define castle_ObjectMaterialIndex uint(CastleDraw.u_ObjectParams.x + 0.5)' + NL +
    '#define castle_ObjectSkinningMode int(CastleDraw.u_ObjectParams.y + 0.5)' + NL +
    '#define castle_ObjectSpecularAAEnabled (CastleDraw.u_ObjectParams.z > 0.5)' + NL +
    '#define castle_ObjectShadowMapMode int(CastleDraw.u_ObjectParams.w + 0.5)' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateStageIO(const AStage: TRHIShaderStage): String;
begin
  case AStage of
    ssVertex:
      Result :=
        '/* --- Standard Vertex Attributes (Locations 0..7) --- */' + NL +
        'CASTLE_LOCATION(0) in vec4 castle_Vertex;' + NL +
        'CASTLE_LOCATION(1) in vec3 castle_Normal;' + NL +
        'CASTLE_LOCATION(2) in vec2 castle_MultiTexCoord0;' + NL +
        'CASTLE_LOCATION(3) in vec2 castle_MultiTexCoord1;' + NL +
        'CASTLE_LOCATION(4) in vec4 castle_ColorPerVertex;' + NL +
        'CASTLE_LOCATION(5) in vec4 castle_Tangent;' + NL +
        'CASTLE_LOCATION(6) in vec4 castle_Joints0;' + NL +
        'CASTLE_LOCATION(7) in vec4 castle_Weights0;' + NL +
        NL +
        '/* --- Vertex-to-Fragment Varyings --- */' + NL +
        'CASTLE_LOCATION(0) out vec4 castle_vertex_eye;' + NL +
        'CASTLE_LOCATION(1) out vec3 castle_normal_eye;' + NL +
        'CASTLE_LOCATION(2) out vec2 castle_tex_coord0;' + NL +
        'CASTLE_LOCATION(3) out vec2 castle_tex_coord1;' + NL +
        'CASTLE_LOCATION(4) out vec4 castle_color_per_vertex;' + NL +
        'CASTLE_LOCATION(5) out mat3 castle_tangent_to_eye_space;' + NL +
        '#define castle_TexCoord0 vec4(castle_tex_coord0, 0.0, 1.0)' + NL +
        '#define castle_TexCoord1 vec4(castle_tex_coord1, 0.0, 1.0)' + NL;

    ssFragment:
      Result :=
        '/* --- Fragment Inputs (Must match Vertex Outputs by Location) --- */' + NL +
        'CASTLE_LOCATION(0) in vec4 castle_vertex_eye;' + NL +
        'CASTLE_LOCATION(1) in vec3 castle_normal_eye;' + NL +
        'CASTLE_LOCATION(2) in vec2 castle_tex_coord0;' + NL +
        'CASTLE_LOCATION(3) in vec2 castle_tex_coord1;' + NL +
        'CASTLE_LOCATION(4) in vec4 castle_color_per_vertex;' + NL +
        'CASTLE_LOCATION(5) in mat3 castle_tangent_to_eye_space;' + NL +
        '#define castle_TexCoord0 vec4(castle_tex_coord0, 0.0, 1.0)' + NL +
        '#define castle_TexCoord1 vec4(castle_tex_coord1, 0.0, 1.0)' + NL +
        '#define castle_ColorPerVertex castle_color_per_vertex' + NL +
        NL +
        '/* --- CGE Color-Space Helper Functions (terrain.fs & Effect shaders) --- */' + NL +
        'vec4 castle_texture_color_to_linear(const in vec4 srgbIn) {' + NL +
        '#ifdef CASTLE_GAMMA_CORRECTION' + NL +
        '  return vec4(pow(max(srgbIn.rgb, vec3(0.0)), vec3(2.2)), srgbIn.a);' + NL +
        '#else' + NL +
        '  return srgbIn;' + NL +
        '#endif' + NL +
        '}' + NL +
        'vec3 castle_texture_color_to_linear(const in vec3 srgbIn) {' + NL +
        '#ifdef CASTLE_GAMMA_CORRECTION' + NL +
        '  return pow(max(srgbIn, vec3(0.0)), vec3(2.2));' + NL +
        '#else' + NL +
        '  return srgbIn;' + NL +
        '#endif' + NL +
        '}' + NL +
        'vec3 castle_linear_to_screen(const in vec3 linearIn) {' + NL +
        '#ifdef CASTLE_GAMMA_CORRECTION' + NL +
        '  return pow(max(linearIn, vec3(0.0)), vec3(1.0 / 2.2));' + NL +
        '#else' + NL +
        '  return linearIn;' + NL +
        '#endif' + NL +
        '}' + NL +
        NL +
        '/* --- Fragment Color Output --- */' + NL +
        'CASTLE_LOCATION(0) out vec4 castle_FragColor;' + NL;

    ssGeometry:
      Result :=
        '/* --- Geometry Inputs (Must match Vertex Outputs by Location) --- */' + NL +
        '#define CASTLE_GEOMETRY_INPUT_SIZE gl_in.length()' + NL +
        'CASTLE_LOCATION(0) in vec4 castle_vertex_eye[];' + NL +
        'CASTLE_LOCATION(1) in vec3 castle_normal_eye[];' + NL +
        'CASTLE_LOCATION(2) in vec2 castle_tex_coord0[];' + NL +
        'CASTLE_LOCATION(3) in vec2 castle_tex_coord1[];' + NL +
        'CASTLE_LOCATION(4) in vec4 castle_color_per_vertex[];' + NL +
        'CASTLE_LOCATION(5) in mat3 castle_tangent_to_eye_space[];' + NL +
        '#define castle_TexCoord0 castle_tex_coord0' + NL +
        '#define castle_TexCoord1 castle_tex_coord1' + NL +
        '#define castle_ColorPerVertex castle_color_per_vertex' + NL +
        NL +
        '/* --- Geometry Outputs (Must match Fragment Inputs by Location) --- */' + NL +
        'CASTLE_LOCATION(0) out vec4 castle_vertex_eye_geoshader;' + NL +
        'CASTLE_LOCATION(1) out vec3 castle_normal_eye_geoshader;' + NL +
        'CASTLE_LOCATION(2) out vec2 castle_tex_coord0_geoshader;' + NL +
        'CASTLE_LOCATION(3) out vec2 castle_tex_coord1_geoshader;' + NL +
        'CASTLE_LOCATION(4) out vec4 castle_color_per_vertex_geoshader;' + NL +
        'CASTLE_LOCATION(5) out mat3 castle_tangent_to_eye_space_geoshader;' + NL +
        '#define castle_TexCoord0_geoshader castle_tex_coord0_geoshader' + NL +
        '#define castle_TexCoord1_geoshader castle_tex_coord1_geoshader' + NL +
        '#define castle_ColorPerVertex_geoshader castle_color_per_vertex_geoshader' + NL +
        NL +
        '/* --- Geometry Shader Helper Forward Declarations --- */' + NL +
        'void geometryVertexSet(const int index);' + NL +
        'void geometryVertexZero();' + NL +
        'void geometryVertexAdd(const int index, const float scale);' + NL;
  else
    Result := '';
  end;
end;

function TCastleShaderGeneratorRHI.GeneratePreamble(const AStage: TRHIShaderStage): String;
begin
  if AStage = ssCompute then
    Result := GenerateVersionAndLayoutMacrosForStage(ssCompute) + NL
  else
    Result :=
      GenerateVersionAndLayoutMacrosForStage(AStage) + NL +
      GenerateStandardUniformBlocks(AStage) + NL +
      GenerateStageIO(AStage) + NL;
end;

function TCastleShaderGeneratorRHI.GeneratePreambleFor3DVertex(
  const AHasSkinning, AHasTangents, AHasTexCoord1, AHasColorPerVertex: Boolean): String;
var
  AttrIO: String;
begin
  AttrIO :=
    '/* --- Active Standard Vertex Attributes --- */' + NL +
    'CASTLE_LOCATION(0) in vec4 castle_Vertex;' + NL +
    'CASTLE_LOCATION(1) in vec3 castle_Normal;' + NL +
    'CASTLE_LOCATION(2) in vec2 castle_MultiTexCoord0;' + NL;

  if AHasTexCoord1 then
    AttrIO := AttrIO + 'CASTLE_LOCATION(3) in vec2 castle_MultiTexCoord1;' + NL
  else
    AttrIO := AttrIO + '#define castle_MultiTexCoord1 (castle_MultiTexCoord0.xy)' + NL;

  if AHasColorPerVertex then
    AttrIO := AttrIO + 'CASTLE_LOCATION(4) in vec4 castle_ColorPerVertex;' + NL
  else
    AttrIO := AttrIO + '#define castle_ColorPerVertex vec4(1.0)' + NL;

  if AHasTangents then
    AttrIO := AttrIO + 'CASTLE_LOCATION(5) in vec4 castle_Tangent;' + NL
  else
    AttrIO := AttrIO + '#define castle_Tangent vec4(1.0, 0.0, 0.0, 1.0)' + NL;

  if AHasSkinning then
    AttrIO := AttrIO +
      'CASTLE_LOCATION(6) in vec4 castle_Joints0;' + NL +
      'CASTLE_LOCATION(7) in vec4 castle_Weights0;' + NL;

  AttrIO := AttrIO + NL +
    '/* --- Vertex-to-Fragment Varyings --- */' + NL +
    'CASTLE_LOCATION(0) out vec4 castle_vertex_eye;' + NL +
    'CASTLE_LOCATION(1) out vec3 castle_normal_eye;' + NL +
    'CASTLE_LOCATION(2) out vec2 castle_tex_coord0;' + NL +
    'CASTLE_LOCATION(3) out vec2 castle_tex_coord1;' + NL +
    'CASTLE_LOCATION(4) out vec4 castle_color_per_vertex;' + NL +
    'CASTLE_LOCATION(5) out mat3 castle_tangent_to_eye_space;' + NL +
    '#define castle_TexCoord0 vec4(castle_tex_coord0, 0.0, 1.0)' + NL +
    '#define castle_TexCoord1 vec4(castle_tex_coord1, 0.0, 1.0)' + NL;

  Result :=
    GenerateVersionAndLayoutMacrosForStage(ssVertex) + NL +
    GenerateStandardUniformBlocksForStage(ssVertex, AHasSkinning) + NL +
    AttrIO + NL;
end;

function TCastleShaderGeneratorRHI.GenerateComputePreamble(const ALocalSizeX,
  ALocalSizeY, ALocalSizeZ: Cardinal): String;
begin
  Result :=
    GenerateVersionAndLayoutMacrosForStage(ssCompute) + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = %d) in;',
      [Max(Cardinal(1), ALocalSizeX),
       Max(Cardinal(1), ALocalSizeY),
       Max(Cardinal(1), ALocalSizeZ)]) + NL;
end;

function TCastleShaderGeneratorRHI.GenerateBindlessPreamble(const AStage: TRHIShaderStage;
  const AMaxBindlessTextures: Cardinal): String;
var
  ArrayDecl: String;
begin
  if FBackend <> rbVulkan then
    Exit(GeneratePreamble(AStage));

  if AMaxBindlessTextures > 0 then
    ArrayDecl := Format('CASTLE_SAMPLER(1, 0) uniform sampler2D castle_BindlessTextures[%d];', [AMaxBindlessTextures])
  else
    ArrayDecl := 'CASTLE_SAMPLER(1, 0) uniform sampler2D castle_BindlessTextures[];';

  Result :=
    GenerateVersionAndLayoutMacros +
    '#extension GL_EXT_nonuniform_qualifier : require' + NL +
    '#define CASTLE_BINDLESS_TEXTURES 1' + NL +
    NL +
    GenerateStandardUniformBlocks(AStage) + NL +
    '/* --- Bindless Texture Table & Multi-Material SSBO (VK_EXT_descriptor_indexing) --- */' + NL +
    ArrayDecl + NL +
    NL +
    'struct CastleBindlessMaterialEntry {' + NL +
    '  vec4 BaseColor;' + NL +
    '  vec4 SpecularColor;' + NL +
    '  vec4 EmissiveColor;' + NL +
    '  vec4 PBRParams;' + NL +
    '  uint BaseColorTexIndex;' + NL +
    '  uint NormalTexIndex;' + NL +
    '  uint MetallicRoughnessTexIndex;' + NL +
    '  uint EmissiveTexIndex;' + NL +
    '};' + NL +
    NL +
    'CASTLE_SSBO(1, 1) readonly buffer CastleBindlessMaterialsBlock {' + NL +
    '  CastleBindlessMaterialEntry u_Materials[];' + NL +
    '} CastleBindlessMaterials;' + NL +
    NL +
    '#define castle_SampleBindless2D(TexIdx, UV) texture(castle_BindlessTextures[nonuniformEXT(uint(TexIdx))], (UV))' + NL +
    '#define castle_GetMaterialEntry(MatIdx) CastleBindlessMaterials.u_Materials[uint(MatIdx)]' + NL +
    '#define castle_DrawMaterialIndex uint(CastleDraw.u_ObjectParams.x)' + NL +
    NL +
    GenerateStageIO(AStage) + NL;
end;

function IsGLSLIdentChar(const C: Char): Boolean; inline;
begin
  Result := (C in ['a'..'z', 'A'..'Z', '0'..'9', '_']);
end;

function ContainsGLSLIdentifier(const Source, Ident: String): Boolean;
var
  StartPos, MatchPos, EndIdx, IdentLen, SrcLen: Integer;
begin
  Result := False;
  IdentLen := Length(Ident);
  SrcLen := Length(Source);
  if (IdentLen = 0) or (SrcLen < IdentLen) then Exit;

  StartPos := 1;
  while StartPos <= SrcLen - IdentLen + 1 do
  begin
    MatchPos := PosEx(Ident, Source, StartPos);
    if MatchPos = 0 then Exit(False);

    EndIdx := MatchPos + IdentLen;
    if ((MatchPos = 1) or not IsGLSLIdentChar(Source[MatchPos - 1])) and
       ((EndIdx > SrcLen) or not IsGLSLIdentChar(Source[EndIdx])) then
      Exit(True);

    StartPos := MatchPos + 1;
  end;
end;

function TCastleShaderGeneratorRHI.UpgradeLegacyGLSLBuiltIns(
  const UserEffectSource: String; const EffectDebugName: String): String;
var
  UnusedWarningsCount: Cardinal;
begin
  Result := UpgradeLegacyGLSLBuiltIns(UserEffectSource, EffectDebugName, UnusedWarningsCount);
end;

function TCastleShaderGeneratorRHI.UpgradeLegacyGLSLBuiltIns(
  const UserEffectSource: String; const EffectDebugName: String;
  out AWarningsCount: Cardinal): String;
type
  TLegacyBuiltInMapping = record
    LegacySymbol: String;
    ReplacementExpr: String;
    Description: String;
  end;
const
  LegacyBuiltIns: array[0..11] of TLegacyBuiltInMapping = (
    (LegacySymbol: 'gl_ModelViewProjectionMatrix';
     ReplacementExpr: '(castle_ProjectionMatrix * castle_ModelViewMatrix)';
     Description: 'deprecated OpenGL built-in matrix uniform'),
    (LegacySymbol: 'gl_ModelViewMatrix';
     ReplacementExpr: 'castle_ModelViewMatrix';
     Description: 'deprecated OpenGL built-in matrix uniform'),
    (LegacySymbol: 'gl_ProjectionMatrix';
     ReplacementExpr: 'castle_ProjectionMatrix';
     Description: 'deprecated OpenGL built-in matrix uniform'),
    (LegacySymbol: 'gl_NormalMatrix';
     ReplacementExpr: 'castle_NormalMatrix';
     Description: 'deprecated OpenGL built-in normal matrix uniform'),
    (LegacySymbol: 'gl_Vertex';
     ReplacementExpr: 'castle_Vertex';
     Description: 'deprecated OpenGL built-in vertex attribute'),
    (LegacySymbol: 'gl_Normal';
     ReplacementExpr: 'castle_Normal';
     Description: 'deprecated OpenGL built-in normal attribute'),
    (LegacySymbol: 'gl_MultiTexCoord0';
     ReplacementExpr: 'vec4(castle_MultiTexCoord0, 0.0, 1.0)';
     Description: 'deprecated OpenGL built-in texture coordinate attribute'),
    (LegacySymbol: 'gl_MultiTexCoord1';
     ReplacementExpr: 'vec4(castle_MultiTexCoord1, 0.0, 1.0)';
     Description: 'deprecated OpenGL built-in texture coordinate attribute'),
    (LegacySymbol: 'gl_Color';
     ReplacementExpr: 'castle_ColorPerVertex';
     Description: 'deprecated OpenGL built-in vertex color attribute'),
    (LegacySymbol: 'gl_FragColor';
     ReplacementExpr: 'castle_FragColor';
     Description: 'deprecated OpenGL built-in fragment output variable'),
    (LegacySymbol: 'texture2D';
     ReplacementExpr: 'texture';
     Description: 'deprecated GLSL 1.20 texture lookup function'),
    (LegacySymbol: 'textureCube';
     ReplacementExpr: 'texture';
     Description: 'deprecated GLSL 1.20 cube texture lookup function')
  );
var
  CompatDefines, UpgradedLines: TStringBuilder;
  Lines: TStringList;
  Line, Trimmed: String;
  I: Integer;
begin
  AWarningsCount := 0;
  if FBackend <> rbVulkan then
    Exit(UserEffectSource);

  CompatDefines := TStringBuilder.Create;
  UpgradedLines := TStringBuilder.Create;
  Lines := TStringList.Create;
  try
    for I := Low(LegacyBuiltIns) to High(LegacyBuiltIns) do
    begin
      if ContainsGLSLIdentifier(UserEffectSource, LegacyBuiltIns[I].LegacySymbol) then
      begin
        Inc(AWarningsCount);
        WritelnWarning('Shader', Format(
          'Custom shader "%s" uses %s "%s" which is removed in Vulkan GLSL #version 450. ' +
          'Auto-upgrading to "%s"; please update your Effect shader to use Castle Game Engine equivalents.',
          [EffectDebugName, LegacyBuiltIns[I].Description,
           LegacyBuiltIns[I].LegacySymbol, LegacyBuiltIns[I].ReplacementExpr]));
        CompatDefines.AppendLine(Format('#ifndef %s', [LegacyBuiltIns[I].LegacySymbol]));
        CompatDefines.AppendLine(Format('#define %s %s',
          [LegacyBuiltIns[I].LegacySymbol, LegacyBuiltIns[I].ReplacementExpr]));
        CompatDefines.AppendLine('#endif');
      end;
    end;

    if ContainsGLSLIdentifier(UserEffectSource, 'gl_PositionIn') then
    begin
      Inc(AWarningsCount);
      WritelnWarning('Shader', Format(
        'Custom shader "%s" uses deprecated GLSL 1.50 geometry input "gl_PositionIn" which is removed in Vulkan GLSL #version 450. ' +
        'Auto-upgrading to "gl_in[...].gl_Position".',
        [EffectDebugName]));
    end;

    Lines.Text := UserEffectSource;
    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      while Pos('gl_PositionIn[', Trimmed) > 0 do
      begin
        I := Pos('gl_PositionIn[', Trimmed);
        Trimmed := Copy(Trimmed, 1, I - 1) + 'gl_in[' +
          StringReplace(Copy(Trimmed, I + Length('gl_PositionIn['), MaxInt), ']', '].gl_Position', []);
      end;
      if Pos('[CASTLE_GEOMETRY_INPUT_SIZE]', Trimmed) > 0 then
        Trimmed := StringReplace(Trimmed, '[CASTLE_GEOMETRY_INPUT_SIZE]', '[]', [rfReplaceAll]);
      if IsPrefix('attribute ', Trimmed, False) then
      begin
        Inc(AWarningsCount);
        WritelnWarning('Shader', Format(
          'Custom shader "%s" uses deprecated GLSL qualifier "attribute" ("%s") which is removed in #version 450. ' +
          'Auto-upgrading to "in".',
          [EffectDebugName, Trimmed]));
        UpgradedLines.AppendLine('in ' + Trim(SEnding(Trimmed, Length('attribute ') + 1)));
      end else
      if IsPrefix('varying in ', Trimmed, False) then
      begin
        Inc(AWarningsCount);
        WritelnWarning('Shader', Format(
          'Custom shader "%s" uses deprecated GLSL qualifier "varying in" ("%s") which is removed in #version 450. ' +
          'Auto-upgrading to "in".',
          [EffectDebugName, Trimmed]));
        UpgradedLines.AppendLine('in ' + Trim(SEnding(Trimmed, Length('varying in ') + 1)));
      end else
      if IsPrefix('varying out ', Trimmed, False) then
      begin
        Inc(AWarningsCount);
        WritelnWarning('Shader', Format(
          'Custom shader "%s" uses deprecated GLSL qualifier "varying out" ("%s") which is removed in #version 450. ' +
          'Auto-upgrading to "out".',
          [EffectDebugName, Trimmed]));
        UpgradedLines.AppendLine('out ' + Trim(SEnding(Trimmed, Length('varying out ') + 1)));
      end else
      if IsPrefix('varying ', Trimmed, False) then
      begin
        Inc(AWarningsCount);
        WritelnWarning('Shader', Format(
          'Custom shader "%s" uses deprecated GLSL qualifier "varying" ("%s") which is removed in #version 450. ' +
          'Auto-upgrading to "in".',
          [EffectDebugName, Trimmed]));
        UpgradedLines.AppendLine('in ' + Trim(SEnding(Trimmed, Length('varying ') + 1)));
      end else
      if Trimmed <> Trim(Line) then
        UpgradedLines.AppendLine(Trimmed)
      else
        UpgradedLines.AppendLine(Line);
    end;

    if CompatDefines.Length > 0 then
      Result :=
        '/* --- Auto-Generated #version 450 Legacy OpenGL Compatibility Aliases --- */' + NL +
        CompatDefines.ToString + NL +
        UpgradedLines.ToString
    else
      Result := UpgradedLines.ToString;
  finally
    FreeAndNil(Lines);
    FreeAndNil(UpgradedLines);
    FreeAndNil(CompatDefines);
  end;
end;

function TCastleShaderGeneratorRHI.UpgradeUserEffectUniforms(
  const UserEffectSource: String): String;
const
  PrecisionQualifiers: array[0..2] of String = ('lowp ', 'mediump ', 'highp ');
var
  Lines: TStringList;
  UBOMembers, AliasDefines, StrippedBody: TStringBuilder;
  UpgradedInput, Line, Trimmed, DeclBody, VarType, VarName, BaseName, Qual: String;
  SpacePos, SemiPos, BracketPos, CustomSamplerSlot: Integer;
begin
  if FBackend <> rbVulkan then
    Exit(UserEffectSource);

  UpgradedInput := SanitizeVulkanShaderSource(UpgradeLegacyGLSLBuiltIns(UserEffectSource, 'Effect'));

  Lines := TStringList.Create;
  UBOMembers := TStringBuilder.Create;
  AliasDefines := TStringBuilder.Create;
  StrippedBody := TStringBuilder.Create;
  try
    Lines.Text := UpgradedInput;
    CustomSamplerSlot := 2;

    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      if IsPrefix('uniform ', Trimmed, False) and (Pos('{', Trimmed) = 0) then
      begin
        DeclBody := Trim(SEnding(Trimmed, Length('uniform ') + 1));
        SemiPos := Pos(';', DeclBody);
        if SemiPos > 0 then
          DeclBody := Copy(DeclBody, 1, SemiPos - 1);

        for Qual in PrecisionQualifiers do
          if IsPrefix(Qual, DeclBody, False) then
          begin
            DeclBody := Trim(SEnding(DeclBody, Length(Qual) + 1));
            Break;
          end;

        SpacePos := Pos(' ', DeclBody);
        if SpacePos > 0 then
        begin
          VarType := Trim(Copy(DeclBody, 1, SpacePos - 1));
          VarName := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));

          if IsPrefix('sampler', VarType, False) then
          begin
            StrippedBody.AppendLine(Format('CASTLE_SAMPLER(2, %d) uniform %s %s;',
              [CustomSamplerSlot, VarType, VarName]));
            Inc(CustomSamplerSlot);
          end else
          begin
            BracketPos := Pos('[', VarName);
            if BracketPos > 0 then
              BaseName := Trim(Copy(VarName, 1, BracketPos - 1))
            else
              BaseName := VarName;
            UBOMembers.AppendLine(Format('  %s u_user_%s;', [VarType, VarName]));
            AliasDefines.AppendLine(Format('#undef %s', [BaseName]));
            AliasDefines.AppendLine(Format('#define %s CastleUserEffect.u_user_%s',
              [BaseName, BaseName]));
          end;
        end;
      end else
        StrippedBody.AppendLine(Line);
    end;

    if UBOMembers.Length > 0 then
      Result :=
        '/* --- Auto-Generated Set 2 UBO for User X3D Effect Node --- */' + NL +
        'CASTLE_UBO(2, 1) uniform CastleUserEffectBlock {' + NL +
        UBOMembers.ToString +
        '} CastleUserEffect;' + NL +
        AliasDefines.ToString + NL +
        StrippedBody.ToString
    else
      Result := StrippedBody.ToString;
  finally
    FreeAndNil(StrippedBody);
    FreeAndNil(AliasDefines);
    FreeAndNil(UBOMembers);
    FreeAndNil(Lines);
  end;
end;

function TUserEffectShaderLayout.FindUniform(const AName: String;
  out ADesc: TUserEffectUniformDesc): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Uniforms) do
    if Uniforms[I].Name = AName then
    begin
      ADesc := Uniforms[I];
      Exit(True);
    end;
  FillChar(ADesc, SizeOf(ADesc), 0);
  Result := False;
end;

function TUserEffectShaderLayout.FindSampler(const AName: String;
  out ADesc: TUserEffectSamplerDesc): Boolean;
var
  I: Integer;
begin
  for I := 0 to High(Samplers) do
    if Samplers[I].Name = AName then
    begin
      ADesc := Samplers[I];
      Exit(True);
    end;
  FillChar(ADesc, SizeOf(ADesc), 0);
  Result := False;
end;

function TUserEffectShaderLayout.FindVarying(const AName: String;
  out ADesc: TUserEffectVaryingDesc): Boolean;
var
  I: Integer;
  LookupName: String;
begin
  LookupName := AName;
  for I := 0 to High(Varyings) do
    if Varyings[I].Name = LookupName then
    begin
      ADesc := Varyings[I];
      Exit(True);
    end;
  if IsSuffix('_geoshader', LookupName, False) then
  begin
    LookupName := Copy(LookupName, 1, Length(LookupName) - Length('_geoshader'));
    for I := 0 to High(Varyings) do
      if Varyings[I].Name = LookupName then
      begin
        ADesc := Varyings[I];
        Exit(True);
      end;
  end;
  FillChar(ADesc, SizeOf(ADesc), 0);
  Result := False;
end;

function IsBuiltInCastleVaryingName(const AName: String): Boolean;
const
  BuiltInVaryings: array[0..17] of String = (
    'castle_vertex_eye',
    'castle_normal_eye',
    'castle_tex_coord0',
    'castle_tex_coord1',
    'castle_TexCoord0',
    'castle_TexCoord1',
    'castle_color_per_vertex',
    'castle_ColorPerVertex',
    'castle_tangent_to_eye_space',
    'castle_fog_frag_coord',
    'castle_tex_coord0_4d',
    'castle_FragColor',
    'castle_vertex_eye_geoshader',
    'castle_normal_eye_geoshader',
    'castle_tex_coord0_geoshader',
    'castle_tex_coord1_geoshader',
    'castle_color_per_vertex_geoshader',
    'castle_tangent_to_eye_space_geoshader'
  );
var
  S: String;
begin
  for S in BuiltInVaryings do
    if S = AName then
      Exit(True);
  Result := False;
end;

function IsBuiltInScreenEffectSamplerName(const AName: String): Boolean;
begin
  Result := (AName = 'screen') or (AName = 'screen_depth');
end;

function IsBuiltInCastleVertexAttrName(const AName: String): Boolean;
const
  BuiltInAttrs: array[0..12] of String = (
    'castle_Vertex',
    'castle_Normal',
    'castle_MultiTexCoord0',
    'castle_MultiTexCoord1',
    'castle_ColorPerVertex',
    'castle_Tangent',
    'castle_Joints0',
    'castle_Weights0',
    'castle_SkinJoints0',
    'castle_SkinWeights0',
    'castle_FogCoord',
    'vertex',
    'tex_coord'
  );
var
  S: String;
begin
  for S in BuiltInAttrs do
    if S = AName then
      Exit(True);
  Result := False;
end;

procedure ComputeStd140TypeLayout(const AGLSLType: String;
  const AArrayCount: Cardinal; out ABaseAlign, AByteSize: PtrUInt);
begin
  if AArrayCount > 0 then
  begin
    ABaseAlign := 16;
    if AGLSLType = 'mat4' then
      AByteSize := PtrUInt(AArrayCount) * 64
    else if AGLSLType = 'mat3' then
      AByteSize := PtrUInt(AArrayCount) * 48
    else if AGLSLType = 'mat2' then
      AByteSize := PtrUInt(AArrayCount) * 32
    else
      AByteSize := PtrUInt(AArrayCount) * 16;
    Exit;
  end;

  if (AGLSLType = 'float') or (AGLSLType = 'int') or
     (AGLSLType = 'uint') or (AGLSLType = 'bool') then
  begin
    ABaseAlign := 4;
    AByteSize := 4;
  end else
  if (AGLSLType = 'vec2') or (AGLSLType = 'ivec2') or
     (AGLSLType = 'uvec2') or (AGLSLType = 'bvec2') then
  begin
    ABaseAlign := 8;
    AByteSize := 8;
  end else
  if (AGLSLType = 'vec3') or (AGLSLType = 'ivec3') or
     (AGLSLType = 'uvec3') or (AGLSLType = 'bvec3') then
  begin
    ABaseAlign := 16;
    AByteSize := 12;
  end else
  if (AGLSLType = 'vec4') or (AGLSLType = 'ivec4') or
     (AGLSLType = 'uvec4') or (AGLSLType = 'bvec4') then
  begin
    ABaseAlign := 16;
    AByteSize := 16;
  end else
  if AGLSLType = 'mat2' then
  begin
    ABaseAlign := 16;
    AByteSize := 32;
  end else
  if AGLSLType = 'mat3' then
  begin
    ABaseAlign := 16;
    AByteSize := 48;
  end else
  if AGLSLType = 'mat4' then
  begin
    ABaseAlign := 16;
    AByteSize := 64;
  end else
  begin
    ABaseAlign := 16;
    AByteSize := 16;
  end;
end;

function ComputeVaryingLocationSlots(const AGLSLType: String): Cardinal;
begin
  if AGLSLType = 'mat4' then
    Result := 4
  else if AGLSLType = 'mat3' then
    Result := 3
  else if AGLSLType = 'mat2' then
    Result := 2
  else
    Result := 1;
end;

function StripGLSLLineCommentAndTrackState(const ALine: String;
  var AInBlockComment: Boolean; var ABraceDepth: Integer): String;
var
  I, Len: Integer;
  C, NextC: Char;
  InSingleLineComment: Boolean;
  SB: TStringBuilder;
begin
  Len := Length(ALine);
  if Len = 0 then Exit('');
  SB := TStringBuilder.Create(Len);
  try
    I := 1;
    InSingleLineComment := False;
    while I <= Len do
    begin
      C := ALine[I];
      if I < Len then NextC := ALine[I + 1] else NextC := #0;
      if AInBlockComment then
      begin
        if (C = '*') and (NextC = '/') then
        begin
          AInBlockComment := False;
          Inc(I, 2);
          Continue;
        end;
        Inc(I);
        Continue;
      end;
      if InSingleLineComment then
        Break;
      if (C = '/') and (NextC = '*') then
      begin
        AInBlockComment := True;
        Inc(I, 2);
        Continue;
      end;
      if (C = '/') and (NextC = '/') then
        Break;
      if C = '{' then
        Inc(ABraceDepth)
      else if (C = '}') and (ABraceDepth > 0) then
        Dec(ABraceDepth);
      SB.Append(C);
      Inc(I);
    end;
    Result := Trim(SB.ToString);
  finally
    FreeAndNil(SB);
  end;
end;

procedure StripGLSLStorageAndPrecisionQualifiers(var ADeclBody: String;
  out AInterpQual: String);
var
  Changed: Boolean;
begin
  AInterpQual := '';
  repeat
    Changed := False;
    ADeclBody := Trim(ADeclBody);
    if IsPrefix('centroid ', ADeclBody, False) then
    begin
      ADeclBody := Trim(SEnding(ADeclBody, Length('centroid ') + 1));
      Changed := True;
    end else
    if IsPrefix('flat ', ADeclBody, False) then
    begin
      AInterpQual := 'flat ';
      ADeclBody := Trim(SEnding(ADeclBody, Length('flat ') + 1));
      Changed := True;
    end else
    if IsPrefix('noperspective ', ADeclBody, False) then
    begin
      AInterpQual := 'noperspective ';
      ADeclBody := Trim(SEnding(ADeclBody, Length('noperspective ') + 1));
      Changed := True;
    end else
    if IsPrefix('smooth ', ADeclBody, False) then
    begin
      AInterpQual := '';
      ADeclBody := Trim(SEnding(ADeclBody, Length('smooth ') + 1));
      Changed := True;
    end else
    if IsPrefix('lowp ', ADeclBody, False) then
    begin
      ADeclBody := Trim(SEnding(ADeclBody, Length('lowp ') + 1));
      Changed := True;
    end else
    if IsPrefix('mediump ', ADeclBody, False) then
    begin
      ADeclBody := Trim(SEnding(ADeclBody, Length('mediump ') + 1));
      Changed := True;
    end else
    if IsPrefix('highp ', ADeclBody, False) then
    begin
      ADeclBody := Trim(SEnding(ADeclBody, Length('highp ') + 1));
      Changed := True;
    end;
  until not Changed;
end;

function TryMatchVaryingDeclarationPrefix(const ATrimmed: String;
  const AStage: TRHIShaderStage; out ADeclBody: String;
  out AExplicitDir: String): Boolean;
begin
  Result := False;
  ADeclBody := '';
  AExplicitDir := '';
  if (Pos('(', ATrimmed) > 0) or (Pos('{', ATrimmed) > 0) or
     (Pos(';', ATrimmed) = 0) or IsPrefix('#', ATrimmed, False) then
    Exit;

  if IsPrefix('varying in ', ATrimmed, False) then
  begin
    ADeclBody := Trim(SEnding(ATrimmed, Length('varying in ') + 1));
    AExplicitDir := 'in';
    Exit(True);
  end;
  if IsPrefix('varying out ', ATrimmed, False) then
  begin
    ADeclBody := Trim(SEnding(ATrimmed, Length('varying out ') + 1));
    AExplicitDir := 'out';
    Exit(True);
  end;
  if IsPrefix('varying ', ATrimmed, False) then
  begin
    ADeclBody := Trim(SEnding(ATrimmed, Length('varying ') + 1));
    Exit(True);
  end;
  if IsPrefix('flat out ', ATrimmed, False) or IsPrefix('noperspective out ', ATrimmed, False) or
     IsPrefix('smooth out ', ATrimmed, False) or
     ((AStage in [ssVertex, ssGeometry]) and IsPrefix('out ', ATrimmed, False)) then
  begin
    if IsPrefix('out ', ATrimmed, False) then
      ADeclBody := Trim(SEnding(ATrimmed, Length('out ') + 1))
    else if IsPrefix('flat out ', ATrimmed, False) then
      ADeclBody := 'flat ' + Trim(SEnding(ATrimmed, Length('flat out ') + 1))
    else if IsPrefix('noperspective out ', ATrimmed, False) then
      ADeclBody := 'noperspective ' + Trim(SEnding(ATrimmed, Length('noperspective out ') + 1))
    else
      ADeclBody := Trim(SEnding(ATrimmed, Length('smooth out ') + 1));
    AExplicitDir := 'out';
    Exit(True);
  end;
  if IsPrefix('flat in ', ATrimmed, False) or IsPrefix('noperspective in ', ATrimmed, False) or
     IsPrefix('smooth in ', ATrimmed, False) or
     ((AStage in [ssFragment, ssGeometry]) and IsPrefix('in ', ATrimmed, False)) then
  begin
    if IsPrefix('in ', ATrimmed, False) then
      ADeclBody := Trim(SEnding(ATrimmed, Length('in ') + 1))
    else if IsPrefix('flat in ', ATrimmed, False) then
      ADeclBody := 'flat ' + Trim(SEnding(ATrimmed, Length('flat in ') + 1))
    else if IsPrefix('noperspective in ', ATrimmed, False) then
      ADeclBody := 'noperspective ' + Trim(SEnding(ATrimmed, Length('noperspective in ') + 1))
    else
      ADeclBody := Trim(SEnding(ATrimmed, Length('smooth in ') + 1));
    AExplicitDir := 'in';
    Exit(True);
  end;
end;

function TCastleShaderGeneratorRHI.ExtractUserEffectLayout(
  const AVertSource, AFragSource: String;
  const AGeomSource: String;
  const AFirstVaryingLocation: Cardinal): TUserEffectShaderLayout;
var
  UBOCursor: PtrUInt;
  NextVaryingLoc: Cardinal;

  procedure ScanStage(const AStage: TRHIShaderStage; const ASource: String);
  var
    Sanitized, Line, CleanLine, DeclBody, ExplicitDir, InterpQual, VarType,
      Declarators, OneDecl, BaseName, SuffixStr, CountStr: String;
    Lines: TStringList;
    InBlockComment: Boolean;
    BraceDepth, DepthBefore, SemiPos, SpacePos, CommaPos, BracketPos, EndBracketPos: Integer;
    ArrCount: Cardinal;
    BaseAlign, ByteSize: PtrUInt;
    ExistingUniform: TUserEffectUniformDesc;
    ExistingSampler: TUserEffectSamplerDesc;
    ExistingVarying: TUserEffectVaryingDesc;
    NewIdx: Integer;
  begin
    if Trim(ASource) = '' then Exit;
    Sanitized := SanitizeVulkanShaderSource(ASource);
    Lines := TStringList.Create;
    try
      Lines.Text := Sanitized;
      InBlockComment := False;
      BraceDepth := 0;
      for Line in Lines do
      begin
        DepthBefore := BraceDepth;
        CleanLine := StripGLSLLineCommentAndTrackState(Line, InBlockComment, BraceDepth);
        if (CleanLine = '') or (DepthBefore <> 0) then
          Continue;

        if IsPrefix('uniform ', CleanLine, False) and (Pos('{', CleanLine) = 0) and
           (Pos('(', CleanLine) = 0) then
        begin
          DeclBody := Trim(SEnding(CleanLine, Length('uniform ') + 1));
          SemiPos := Pos(';', DeclBody);
          if SemiPos > 0 then
            DeclBody := Trim(Copy(DeclBody, 1, SemiPos - 1));
          StripGLSLStorageAndPrecisionQualifiers(DeclBody, InterpQual);
          SpacePos := Pos(' ', DeclBody);
          if SpacePos > 0 then
          begin
            VarType := Trim(Copy(DeclBody, 1, SpacePos - 1));
            Declarators := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
            while Declarators <> '' do
            begin
              CommaPos := Pos(',', Declarators);
              if CommaPos > 0 then
              begin
                OneDecl := Trim(Copy(Declarators, 1, CommaPos - 1));
                Declarators := Trim(Copy(Declarators, CommaPos + 1, MaxInt));
              end else
              begin
                OneDecl := Trim(Declarators);
                Declarators := '';
              end;
              if OneDecl = '' then Continue;
              BracketPos := Pos('[', OneDecl);
              ArrCount := 0;
              SuffixStr := '';
              if BracketPos > 0 then
              begin
                BaseName := Trim(Copy(OneDecl, 1, BracketPos - 1));
                SuffixStr := Trim(Copy(OneDecl, BracketPos, MaxInt));
                EndBracketPos := Pos(']', SuffixStr);
                if EndBracketPos > 1 then
                begin
                  CountStr := Trim(Copy(SuffixStr, 2, EndBracketPos - 2));
                  ArrCount := Max(1, StrToIntDef(CountStr, 1));
                end else
                  ArrCount := 1;
              end else
                BaseName := OneDecl;

              if (BaseName = '') or IsBuiltInScreenEffectSamplerName(BaseName) then
                Continue;

              if IsPrefix('sampler', VarType, False) then
              begin
                if (not Result.FindSampler(BaseName, ExistingSampler)) and
                   (Length(Result.Samplers) < CASTLE_MAX_USER_EFFECT_SAMPLERS) then
                begin
                  NewIdx := Length(Result.Samplers);
                  SetLength(Result.Samplers, NewIdx + 1);
                  Result.Samplers[NewIdx].Name := BaseName;
                  Result.Samplers[NewIdx].GLSLType := VarType;
                  Result.Samplers[NewIdx].SlotIndex := NewIdx;
                end;
              end else
              begin
                if not Result.FindUniform(BaseName, ExistingUniform) then
                begin
                  ComputeStd140TypeLayout(VarType, ArrCount, BaseAlign, ByteSize);
                  UBOCursor := (UBOCursor + BaseAlign - 1) and not (BaseAlign - 1);
                  NewIdx := Length(Result.Uniforms);
                  SetLength(Result.Uniforms, NewIdx + 1);
                  Result.Uniforms[NewIdx].Name := BaseName;
                  Result.Uniforms[NewIdx].GLSLType := VarType;
                  Result.Uniforms[NewIdx].DeclSuffix := SuffixStr;
                  Result.Uniforms[NewIdx].ByteOffset := UBOCursor;
                  Result.Uniforms[NewIdx].ByteSize := ByteSize;
                  Result.Uniforms[NewIdx].ArrayCount := ArrCount;
                  Inc(UBOCursor, ByteSize);
                end;
              end;
            end;
          end;
        end else
        if TryMatchVaryingDeclarationPrefix(CleanLine, AStage, DeclBody, ExplicitDir) then
        begin
          SemiPos := Pos(';', DeclBody);
          if SemiPos > 0 then
            DeclBody := Trim(Copy(DeclBody, 1, SemiPos - 1));
          StripGLSLStorageAndPrecisionQualifiers(DeclBody, InterpQual);
          SpacePos := Pos(' ', DeclBody);
          if SpacePos > 0 then
          begin
            VarType := Trim(Copy(DeclBody, 1, SpacePos - 1));
            Declarators := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
            while Declarators <> '' do
            begin
              CommaPos := Pos(',', Declarators);
              if CommaPos > 0 then
              begin
                OneDecl := Trim(Copy(Declarators, 1, CommaPos - 1));
                Declarators := Trim(Copy(Declarators, CommaPos + 1, MaxInt));
              end else
              begin
                OneDecl := Trim(Declarators);
                Declarators := '';
              end;
              BracketPos := Pos('[', OneDecl);
              if BracketPos > 0 then
                BaseName := Trim(Copy(OneDecl, 1, BracketPos - 1))
              else
                BaseName := OneDecl;
              if IsSuffix('_geoshader', BaseName, False) then
                BaseName := Copy(BaseName, 1, Length(BaseName) - Length('_geoshader'));
              if (BaseName = '') or IsBuiltInCastleVaryingName(BaseName) then
                Continue;
              if not Result.FindVarying(BaseName, ExistingVarying) then
              begin
                NewIdx := Length(Result.Varyings);
                SetLength(Result.Varyings, NewIdx + 1);
                Result.Varyings[NewIdx].Name := BaseName;
                Result.Varyings[NewIdx].GLSLType := VarType;
                Result.Varyings[NewIdx].InterpQual := InterpQual;
                Result.Varyings[NewIdx].Location := NextVaryingLoc;
                Result.Varyings[NewIdx].LocationSlots := ComputeVaryingLocationSlots(VarType);
                Inc(NextVaryingLoc, Result.Varyings[NewIdx].LocationSlots);
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(Lines);
    end;
  end;

begin
  FillChar(Result, SizeOf(Result), 0);
  UBOCursor := 0;
  NextVaryingLoc := AFirstVaryingLocation;
  ScanStage(ssVertex, AVertSource);
  ScanStage(ssGeometry, AGeomSource);
  ScanStage(ssFragment, AFragSource);
  if Length(Result.Uniforms) > 0 then
  begin
    Result.TotalUBOSize := (UBOCursor + 15) and not PtrUInt(15);
    if Result.TotalUBOSize = 0 then
      Result.TotalUBOSize := 16;
  end;
end;

function TCastleShaderGeneratorRHI.UpgradeUserEffectStageSource(
  const AStage: TRHIShaderStage;
  const AStageSource: String;
  const ALayout: TUserEffectShaderLayout;
  const AFirstSamplerBinding: Cardinal;
  const ASetIndex: Cardinal): String;
var
  Sanitized, Line, CleanLine, DeclBody, ExplicitDir, InterpQual, VarType,
    Declarators, OneDecl, BaseName, SuffixStr, DirKeyword, HeaderDecls: String;
  Lines: TStringList;
  UBOMembers, AliasDefines, SamplerDecls, BodyBuilder: TStringBuilder;
  InBlockComment: Boolean;
  BraceDepth, DepthBefore, SemiPos, SpacePos, CommaPos, BracketPos, I: Integer;
  VDesc: TUserEffectVaryingDesc;
begin
  if FBackend <> rbVulkan then
    Exit(AStageSource);
  if Trim(AStageSource) = '' then
    Exit('');

  Sanitized := SanitizeVulkanShaderSource(AStageSource);

  Lines := TStringList.Create;
  UBOMembers := TStringBuilder.Create;
  AliasDefines := TStringBuilder.Create;
  SamplerDecls := TStringBuilder.Create;
  BodyBuilder := TStringBuilder.Create;
  try
    for I := 0 to High(ALayout.Uniforms) do
    begin
      UBOMembers.AppendLine(Format('  %s u_user_%s%s;',
        [ALayout.Uniforms[I].GLSLType, ALayout.Uniforms[I].Name, ALayout.Uniforms[I].DeclSuffix]));
      AliasDefines.AppendLine(Format('#undef %s', [ALayout.Uniforms[I].Name]));
      AliasDefines.AppendLine(Format('#define %s CastleUserEffect.u_user_%s',
        [ALayout.Uniforms[I].Name, ALayout.Uniforms[I].Name]));
    end;

    for I := 0 to High(ALayout.Samplers) do
      SamplerDecls.AppendLine(Format('CASTLE_SAMPLER(%d, %d) uniform %s %s;',
        [ASetIndex, Integer(AFirstSamplerBinding) + ALayout.Samplers[I].SlotIndex,
         ALayout.Samplers[I].GLSLType, ALayout.Samplers[I].Name]));

    Lines.Text := Sanitized;
    InBlockComment := False;
    BraceDepth := 0;
    for Line in Lines do
    begin
      DepthBefore := BraceDepth;
      CleanLine := StripGLSLLineCommentAndTrackState(Line, InBlockComment, BraceDepth);
      if (CleanLine <> '') and (DepthBefore = 0) and
         IsPrefix('uniform ', CleanLine, False) and (Pos('{', CleanLine) = 0) and
         (Pos('(', CleanLine) = 0) then
      begin
        DeclBody := Trim(SEnding(CleanLine, Length('uniform ') + 1));
        SemiPos := Pos(';', DeclBody);
        if SemiPos > 0 then
          DeclBody := Trim(Copy(DeclBody, 1, SemiPos - 1));
        StripGLSLStorageAndPrecisionQualifiers(DeclBody, InterpQual);
        SpacePos := Pos(' ', DeclBody);
        if SpacePos > 0 then
        begin
          Declarators := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
          if IsBuiltInScreenEffectSamplerName(Declarators) then
            BodyBuilder.AppendLine('// [Vulkan RHI screen sampler]: ' + Line)
          else
            BodyBuilder.AppendLine('// [Vulkan RHI Set 2 uniform]: ' + Line);
        end else
          BodyBuilder.AppendLine(Line);
      end else
      if (CleanLine <> '') and (DepthBefore = 0) and
         TryMatchVaryingDeclarationPrefix(CleanLine, AStage, DeclBody, ExplicitDir) then
      begin
        SemiPos := Pos(';', DeclBody);
        if SemiPos > 0 then
          DeclBody := Trim(Copy(DeclBody, 1, SemiPos - 1));
        StripGLSLStorageAndPrecisionQualifiers(DeclBody, InterpQual);
        SpacePos := Pos(' ', DeclBody);
        if SpacePos > 0 then
        begin
          VarType := Trim(Copy(DeclBody, 1, SpacePos - 1));
          Declarators := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
          while Declarators <> '' do
          begin
            CommaPos := Pos(',', Declarators);
            if CommaPos > 0 then
            begin
              OneDecl := Trim(Copy(Declarators, 1, CommaPos - 1));
              Declarators := Trim(Copy(Declarators, CommaPos + 1, MaxInt));
            end else
            begin
              OneDecl := Trim(Declarators);
              Declarators := '';
            end;
            BracketPos := Pos('[', OneDecl);
            if BracketPos > 0 then
            begin
              BaseName := Trim(Copy(OneDecl, 1, BracketPos - 1));
              SuffixStr := Trim(Copy(OneDecl, BracketPos, MaxInt));
              if SuffixStr = '[CASTLE_GEOMETRY_INPUT_SIZE]' then
                SuffixStr := '[]';
            end else
            begin
              BaseName := OneDecl;
              SuffixStr := '';
            end;
            if IsBuiltInCastleVaryingName(BaseName) then
              BodyBuilder.AppendLine('// [Vulkan RHI built-in varying]: ' + BaseName)
            else if ALayout.FindVarying(BaseName, VDesc) then
            begin
              if ExplicitDir <> '' then
                DirKeyword := ExplicitDir
              else if AStage = ssVertex then
                DirKeyword := 'out'
              else
                DirKeyword := 'in';
              BodyBuilder.AppendLine(Format('CASTLE_LOCATION(%d) %s%s %s %s%s;',
                [VDesc.Location, VDesc.InterpQual, DirKeyword, VarType, BaseName, SuffixStr]));
            end else
              BodyBuilder.AppendLine(Line);
          end;
        end else
          BodyBuilder.AppendLine(Line);
      end else
      if (CleanLine <> '') and (DepthBefore = 0) and (AStage = ssVertex) and
         (Pos('(', CleanLine) = 0) and (Pos('{', CleanLine) = 0) and (Pos(';', CleanLine) > 0) and
         (IsPrefix('attribute ', CleanLine, False) or IsPrefix('in ', CleanLine, False)) then
      begin
        if IsPrefix('attribute ', CleanLine, False) then
          DeclBody := Trim(SEnding(CleanLine, Length('attribute ') + 1))
        else
          DeclBody := Trim(SEnding(CleanLine, Length('in ') + 1));
        SemiPos := Pos(';', DeclBody);
        if SemiPos > 0 then
          DeclBody := Trim(Copy(DeclBody, 1, SemiPos - 1));
        StripGLSLStorageAndPrecisionQualifiers(DeclBody, InterpQual);
        SpacePos := Pos(' ', DeclBody);
        if SpacePos > 0 then
        begin
          BaseName := Trim(Copy(DeclBody, SpacePos + 1, MaxInt));
          BracketPos := Pos('[', BaseName);
          if BracketPos > 0 then
            BaseName := Trim(Copy(BaseName, 1, BracketPos - 1));
          if IsBuiltInCastleVertexAttrName(BaseName) then
            BodyBuilder.AppendLine('// [Vulkan RHI built-in attribute]: ' + BaseName)
          else
            BodyBuilder.AppendLine(Line);
        end else
          BodyBuilder.AppendLine(Line);
      end else
        BodyBuilder.AppendLine(Line);
    end;

    HeaderDecls := '';
    if UBOMembers.Length > 0 then
      HeaderDecls := HeaderDecls +
        '/* --- Auto-Generated Set 2 UBO for User X3D Effect Node --- */' + NL +
        Format('CASTLE_UBO(%d, 1) uniform CastleUserEffectBlock {', [ASetIndex]) + NL +
        UBOMembers.ToString +
        '} CastleUserEffect;' + NL +
        AliasDefines.ToString + NL;
    if SamplerDecls.Length > 0 then
      HeaderDecls := HeaderDecls + SamplerDecls.ToString + NL;

    Result := HeaderDecls + UpgradeLegacyGLSLBuiltIns(BodyBuilder.ToString, 'Effect');
    { Isolate user EffectPart uniform macros (e.g. terrain.fs uniform vec4 metallic/roughness)
      from CGE's own local variables inside main(), while keeping macros active when AStageSource
      itself defines main() (ComposedShader / ScreenEffect). }
    if (Length(ALayout.Uniforms) > 0) and (Pos('main(', AStageSource) = 0) and
       (Pos('main (', AStageSource) = 0) then
    begin
      Result := Result + NL;
      for I := 0 to High(ALayout.Uniforms) do
        Result := Result + Format('#undef %s', [ALayout.Uniforms[I].Name]) + NL;
    end;
  finally
    FreeAndNil(BodyBuilder);
    FreeAndNil(SamplerDecls);
    FreeAndNil(AliasDefines);
    FreeAndNil(UBOMembers);
    FreeAndNil(Lines);
  end;
end;

function TCastleShaderGeneratorRHI.FinalizeVertexShaderSource(
  const StitchedSource: String): String;
var
  LastBracePos: Integer;
begin
  if FBackend <> rbVulkan then
    Exit(StitchedSource);

  (* Inject Vulkan clip-space depth conversion ([-1..1] -> [0..1]) before the final '}'
     of main() so CGE's OpenGL projection matrices work identically in Vulkan *)
  LastBracePos := RPos('}', StitchedSource);
  if LastBracePos > 0 then
    Result := Copy(StitchedSource, 1, LastBracePos - 1) + NL +
      '  /* Vulkan NDC depth adjustment [0..1] */' + NL +
      '  gl_Position.z = (gl_Position.z + gl_Position.w) * 0.5;' + NL +
      '}' + NL
  else
    Result := StitchedSource;
end;

function TCastleShaderGeneratorRHI.FinalizeGeometryShaderSource(
  const StitchedSource: String;
  const PlugSetCalls: String;
  const PlugZeroCalls: String;
  const PlugAddCalls: String): String;
var
  HasInputLayout, HasMain: Boolean;
begin
  Result := StitchedSource;
  if FBackend <> rbVulkan then
    Exit;

  HasInputLayout :=
    (Pos(') in;', Result) > 0) or
    (Pos(')in;', Result) > 0);
  HasMain := Pos('void main', Result) > 0;

  if not HasInputLayout then
  begin
    if Pos('void geometryVertexAdd(const int index, const float scale);', Result) > 0 then
      Result := StringReplace(
        Result,
        'void geometryVertexAdd(const int index, const float scale);',
        'void geometryVertexAdd(const int index, const float scale);' + NL +
        'layout(triangles) in;' + NL +
        'layout(triangle_strip, max_vertices = 3) out;',
        [])
    else if HasMain then
      Result := StringReplace(
        Result,
        'void main',
        'layout(triangles) in;' + NL +
        'layout(triangle_strip, max_vertices = 3) out;' + NL +
        'void main',
        [])
    else
      Result :=
        'layout(triangles) in;' + NL +
        'layout(triangle_strip, max_vertices = 3) out;' + NL +
        Result;
  end;

  if not HasMain then
    Result := Result + NL +
      'void main()' + NL +
      '{' + NL +
      '  for (int i = 0; i < 3; ++i) {' + NL +
      '    gl_Position = gl_in[i].gl_Position;' + NL +
      '    geometryVertexSet(i);' + NL +
      '    EmitVertex();' + NL +
      '  }' + NL +
      '  EndPrimitive();' + NL +
      '}' + NL;

  Result := Result + NL +
    '/* --- CGE Geometry Shader Varying Pass-Through & Blending Implementations --- */' + NL +
    'void geometryVertexSet(const int index)' + NL +
    '{' + NL +
    '  castle_vertex_eye_geoshader = castle_vertex_eye[index];' + NL +
    '  castle_normal_eye_geoshader = castle_normal_eye[index];' + NL +
    '  castle_tex_coord0_geoshader = castle_tex_coord0[index];' + NL +
    '  castle_tex_coord1_geoshader = castle_tex_coord1[index];' + NL +
    '  castle_color_per_vertex_geoshader = castle_color_per_vertex[index];' + NL +
    '  castle_tangent_to_eye_space_geoshader = castle_tangent_to_eye_space[index];' + NL;
  if PlugSetCalls <> '' then
    Result := Result + PlugSetCalls + NL;
  Result := Result +
    '}' + NL +
    NL +
    'void geometryVertexZero()' + NL +
    '{' + NL +
    '  castle_vertex_eye_geoshader = vec4(0.0);' + NL +
    '  castle_normal_eye_geoshader = vec3(0.0);' + NL +
    '  castle_tex_coord0_geoshader = vec2(0.0);' + NL +
    '  castle_tex_coord1_geoshader = vec2(0.0);' + NL +
    '  castle_color_per_vertex_geoshader = vec4(0.0);' + NL +
    '  castle_tangent_to_eye_space_geoshader = mat3(0.0);' + NL;
  if PlugZeroCalls <> '' then
    Result := Result + PlugZeroCalls + NL;
  Result := Result +
    '}' + NL +
    NL +
    'void geometryVertexAdd(const int index, const float scale)' + NL +
    '{' + NL +
    '  castle_vertex_eye_geoshader += castle_vertex_eye[index] * scale;' + NL +
    '  castle_normal_eye_geoshader += castle_normal_eye[index] * scale;' + NL +
    '  castle_tex_coord0_geoshader += castle_tex_coord0[index] * scale;' + NL +
    '  castle_tex_coord1_geoshader += castle_tex_coord1[index] * scale;' + NL +
    '  castle_color_per_vertex_geoshader += castle_color_per_vertex[index] * scale;' + NL +
    '  castle_tangent_to_eye_space_geoshader += castle_tangent_to_eye_space[index] * scale;' + NL;
  if PlugAddCalls <> '' then
    Result := Result + PlugAddCalls + NL;
  Result := Result +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.SanitizeVulkanShaderSource(
  const Source: String): String;
const
  BuiltInUniformTokens: array[0..13] of String = (
    'castle_LightSource', 'castle_projector_matrix', 'castle_ModelViewMatrix',
    'castle_ProjectionMatrix', 'castle_NormalMatrix', 'castle_JointMatrix',
    'castle_Material', 'castle_WorldEyePosition', 'castle_SceneTime', 'castle_Fog',
    'castle_CameraMatrix', 'castle_CameraInverseMatrix',
    'castle_CameraRotationMatrix', 'castle_CameraRotationInverseMatrix'
  );
var
  Lines: TStringList;
  ResultBuilder: TStringBuilder;
  Line, Trimmed, Token: String;
  IsBuiltIn: Boolean;
begin
  if FBackend <> rbVulkan then
    Exit(Source);

  Lines := TStringList.Create;
  ResultBuilder := TStringBuilder.Create;
  try
    Lines.Text := Source;
    for Line in Lines do
    begin
      Trimmed := Trim(Line);
      IsBuiltIn := False;
      if IsPrefix('uniform ', Trimmed, False) and (Pos('{', Trimmed) = 0) then
        for Token in BuiltInUniformTokens do
          if Pos(Token, Trimmed) > 0 then
          begin
            IsBuiltIn := True;
            Break;
          end;

      if IsBuiltIn then
        ResultBuilder.AppendLine('// [Vulkan RHI UBO]: ' + Line)
      else
      if (Pos('{', Trimmed) = 0) and IsSuffix(');', Trimmed, False) and
         ((Pos('castle_texture_color_to_linear(', Trimmed) > 0) or
          (Pos('castle_linear_to_screen(', Trimmed) > 0)) then
        ResultBuilder.AppendLine('// [Vulkan RHI built-in helper]: ' + Line)
      else
        ResultBuilder.AppendLine(Line);
    end;
    Result := ResultBuilder.ToString;
  finally
    FreeAndNil(ResultBuilder);
    FreeAndNil(Lines);
  end;
end;

function TCastleShaderGeneratorRHI.GenerateScreenEffectPreamble(
  const AStage: TRHIShaderStage; const HasDepth: Boolean;
  const AIncludeVertexResources: Boolean): String;
begin
  if FBackend <> rbVulkan then
    Exit('');

  Result :=
    '#version 450' + NL +
    '#extension GL_ARB_separate_shader_objects : enable' + NL +
    '#define CASTLE_UBO(SetIdx, BindIdx) layout(std140, set = SetIdx, binding = BindIdx)' + NL +
    '#define CASTLE_SAMPLER(SetIdx, BindIdx) layout(set = SetIdx, binding = BindIdx)' + NL +
    '#define CASTLE_DESCRIPTOR_SET(SetIdx, BindIdx) layout(set = SetIdx, binding = BindIdx)' + NL +
    '#define CASTLE_LOCATION(LocIdx) layout(location = LocIdx)' + NL;

  if (AStage = ssFragment) or AIncludeVertexResources then
  begin
    Result := Result +
      NL +
      '/* --- Screen Effect Push Constants --- */' + NL +
      'layout(push_constant) uniform CastleScreenPushConstants {' + NL +
      '  vec2 screen_size;' + NL +
      '  vec2 screen_planes;' + NL +
      '  vec4 screen_custom;' + NL +
      '} castle_Screen;' + NL +
      NL +
      '#define screen_width (castle_Screen.screen_size.x)' + NL +
      '#define screen_height (castle_Screen.screen_size.y)' + NL +
      '#define near (castle_Screen.screen_planes.x)' + NL +
      '#define far (castle_Screen.screen_planes.y)' + NL +
      NL +
      '/* --- Screen Effect Textures (Set 0) --- */' + NL +
      'layout(set = 0, binding = 0) uniform sampler2D screen;' + NL;

    if HasDepth then
      Result := Result +
        'layout(set = 0, binding = 1) uniform sampler2D screen_depth;' + NL;
  end;

  Result := Result +
    NL +
    '/* --- Compatibility Aliases --- */' + NL +
    '#define texture2D texture' + NL +
    '#define textureCube texture' + NL +
    NL;

  case AStage of
    ssVertex:
      Result := Result +
        '/* --- Vertex Shader Stage IO --- */' + NL +
        'layout(location = 0) in vec2 vertex;' + NL +
        'layout(location = 1) in vec2 tex_coord;' + NL +
        'layout(location = 0) out vec2 v_tex_coord;' + NL;
    ssFragment:
      Result := Result +
        '/* --- Fragment Shader Stage IO --- */' + NL +
        'layout(location = 0) in vec2 tex_coord;' + NL +
        'layout(location = 0) out vec4 castle_FragColor;' + NL +
        '#define gl_FragColor castle_FragColor' + NL +
        '#define screenf_01_position tex_coord' + NL;
  end;
end;

function TCastleShaderGeneratorRHI.GenerateBatch2DFragmentPreamble: String;
begin
  Result :=
    GenerateVersionAndLayoutMacrosForStage(ssFragment) + NL +
    '/* --- 2D Batch Push Constants (64 bytes, matches TCastle2DPushConstants) --- */' + NL +
    'CASTLE_PUSH_CONSTANT uniform Castle2DPushConstants {' + NL +
    '  mat4 u_ModelViewMatrix;' + NL +
    '} CastleDraw;' + NL +
    NL +
    '/* --- 2D Batch Fragment Inputs & Output (matches BuiltInBatch2DVertSPIRV) --- */' + NL +
    'CASTLE_LOCATION(0) in vec2 castle_tex_coord0;' + NL +
    'CASTLE_LOCATION(1) in vec4 castle_color_per_vertex;' + NL +
    'CASTLE_LOCATION(0) out vec4 castle_FragColor;' + NL +
    NL;
end;

function TCastleShaderGeneratorRHI.GenerateComputeCullingShader(
  const AWorkgroupSizeX: Cardinal): String;
var
  EffectiveLocalSizeX: Cardinal;
begin
  if AWorkgroupSizeX = 0 then
    EffectiveLocalSizeX := 64
  else
    EffectiveLocalSizeX := AWorkgroupSizeX;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [EffectiveLocalSizeX]) + NL +
    NL +
    'layout(std140, set = 0, binding = 0) uniform CastleGpuCullingUniforms {' + NL +
    '  vec4 u_FrustumPlanes[6];' + NL +
    '  vec4 u_CameraPosition;' + NL +
    '  vec4 u_HiZParams;' + NL +
    '  uint u_InstanceCount;' + NL +
    '  uint u_EnableFrustumCulling;' + NL +
    '  uint u_EnableDistanceCulling;' + NL +
    '  uint u_CompactDrawStream;' + NL +
    '} CastleCulling;' + NL +
    NL +
    'struct CastleGpuCullingInstance {' + NL +
    '  vec4 BoundingSphere;' + NL +
    '  vec4 AABBMin;' + NL +
    '  vec4 AABBMax;' + NL +
    '  uint IndexCount;' + NL +
    '  uint FirstIndex;' + NL +
    '  int  VertexOffset;' + NL +
    '  uint FirstInstance;' + NL +
    '  uint InstanceCount;' + NL +
    '  uint MaterialIndex;' + NL +
    '  uint Padding0;' + NL +
    '  uint Padding1;' + NL +
    '};' + NL +
    NL +
    'struct CastleDrawIndexedIndirectCommand {' + NL +
    '  uint indexCount;' + NL +
    '  uint instanceCount;' + NL +
    '  uint firstIndex;' + NL +
    '  int  vertexOffset;' + NL +
    '  uint firstInstance;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) readonly buffer CastleCullingInstancesBlock {' + NL +
    '  CastleGpuCullingInstance u_Instances[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 2) writeonly buffer CastleIndirectCommandsBlock {' + NL +
    '  CastleDrawIndexedIndirectCommand u_Commands[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 3) buffer CastleVisibilityOutputBlock {' + NL +
    '  uint u_VisibleCount;' + NL +
    '  uint u_VisibilityFlags[];' + NL +
    '};' + NL +
    NL +
    'bool IsSphereInsideFrustum(vec3 center, float radius) {' + NL +
    '  for (int i = 0; i < 6; ++i) {' + NL +
    '    vec4 plane = CastleCulling.u_FrustumPlanes[i];' + NL +
    '    if (dot(plane.xyz, center) + plane.w < -radius)' + NL +
    '      return false;' + NL +
    '  }' + NL +
    '  return true;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  if (idx >= CastleCulling.u_InstanceCount) return;' + NL +
    '  CastleGpuCullingInstance inst = u_Instances[idx];' + NL +
    '  bool visible = true;' + NL +
    '  if (CastleCulling.u_EnableFrustumCulling != 0u) {' + NL +
    '    visible = IsSphereInsideFrustum(inst.BoundingSphere.xyz, inst.BoundingSphere.w);' + NL +
    '  }' + NL +
    '  if (visible && (CastleCulling.u_EnableDistanceCulling != 0u) && (CastleCulling.u_CameraPosition.w > 0.0)) {' + NL +
    '    float dist = distance(inst.BoundingSphere.xyz, CastleCulling.u_CameraPosition.xyz) - inst.BoundingSphere.w;' + NL +
    '    if (dist > CastleCulling.u_CameraPosition.w)' + NL +
    '      visible = false;' + NL +
    '  }' + NL +
    '  u_VisibilityFlags[idx] = visible ? 1u : 0u;' + NL +
    '  if (CastleCulling.u_CompactDrawStream != 0u) {' + NL +
    '    if (visible) {' + NL +
    '      uint outSlot = atomicAdd(u_VisibleCount, 1u);' + NL +
    '      CastleDrawIndexedIndirectCommand cmd;' + NL +
    '      cmd.indexCount = inst.IndexCount;' + NL +
    '      cmd.instanceCount = inst.InstanceCount;' + NL +
    '      cmd.firstIndex = inst.FirstIndex;' + NL +
    '      cmd.vertexOffset = inst.VertexOffset;' + NL +
    '      cmd.firstInstance = inst.FirstInstance;' + NL +
    '      u_Commands[outSlot] = cmd;' + NL +
    '    }' + NL +
    '  } else {' + NL +
    '    if (visible) atomicAdd(u_VisibleCount, 1u);' + NL +
    '    CastleDrawIndexedIndirectCommand cmd;' + NL +
    '    cmd.indexCount = inst.IndexCount;' + NL +
    '    cmd.instanceCount = visible ? inst.InstanceCount : 0u;' + NL +
    '    cmd.firstIndex = inst.FirstIndex;' + NL +
    '    cmd.vertexOffset = inst.VertexOffset;' + NL +
    '    cmd.firstInstance = inst.FirstInstance;' + NL +
    '    u_Commands[idx] = cmd;' + NL +
    '  }' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateHiZDownsampleComputeShader(
  const AWorkgroupSizeX: Cardinal;
  const AWorkgroupSizeY: Cardinal): String;
var
  LocalSizeX, LocalSizeY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then
    LocalSizeX := 8
  else
    LocalSizeX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then
    LocalSizeY := 8
  else
    LocalSizeY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalSizeX, LocalSizeY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_SrcDepthMip;' + NL +
    'layout(set = 0, binding = 1, r32f) uniform writeonly image2D u_DstDepthMip;' + NL +
    NL +
    'layout(push_constant) uniform CastleHiZPushConstants {' + NL +
    '  uvec2 u_DstMipSize;' + NL +
    '  uvec2 u_SrcMipSize;' + NL +
    '} pc;' + NL +
    NL +
    'void main() {' + NL +
    '  uvec2 dstCoord = gl_GlobalInvocationID.xy;' + NL +
    '  if (dstCoord.x >= pc.u_DstMipSize.x || dstCoord.y >= pc.u_DstMipSize.y) return;' + NL +
    '  ivec2 srcBase = ivec2(dstCoord * 2u);' + NL +
    '  ivec2 maxSrc = ivec2(max(pc.u_SrcMipSize, uvec2(1u))) - ivec2(1);' + NL +
    '  float d00 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(0, 0), ivec2(0), maxSrc), 0).r;' + NL +
    '  float d10 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(1, 0), ivec2(0), maxSrc), 0).r;' + NL +
    '  float d01 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(0, 1), ivec2(0), maxSrc), 0).r;' + NL +
    '  float d11 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(1, 1), ivec2(0), maxSrc), 0).r;' + NL +
    '  float maxDepth = max(max(d00, d10), max(d01, d11));' + NL +
    '  if (((pc.u_SrcMipSize.x & 1u) != 0u) && (dstCoord.x + 1u == pc.u_DstMipSize.x)) {' + NL +
    '    float d20 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(2, 0), ivec2(0), maxSrc), 0).r;' + NL +
    '    float d21 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(2, 1), ivec2(0), maxSrc), 0).r;' + NL +
    '    maxDepth = max(maxDepth, max(d20, d21));' + NL +
    '  }' + NL +
    '  if (((pc.u_SrcMipSize.y & 1u) != 0u) && (dstCoord.y + 1u == pc.u_DstMipSize.y)) {' + NL +
    '    float d02 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(0, 2), ivec2(0), maxSrc), 0).r;' + NL +
    '    float d12 = texelFetch(u_SrcDepthMip, clamp(srcBase + ivec2(1, 2), ivec2(0), maxSrc), 0).r;' + NL +
    '    maxDepth = max(maxDepth, max(d02, d12));' + NL +
    '  }' + NL +
    '  imageStore(u_DstDepthMip, ivec2(dstCoord), vec4(maxDepth, 0.0, 0.0, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GeneratePBRFunctions: String;
begin
  Result :=
    '/* --- glTF 2.0 / X3D PhysicalMaterial Cook-Torrance Microfacet GGX BRDF --- */' + NL +
    'const float CASTLE_PI = 3.14159265358979323846;' + NL +
    NL +
    'float DistributionGGX(float NdotH, float roughness) {' + NL +
    '  float alpha = max(roughness * roughness, 0.002025);' + NL +
    '  float alpha2 = alpha * alpha;' + NL +
    '  float denom = (NdotH * NdotH) * (alpha2 - 1.0) + 1.0;' + NL +
    '  return alpha2 / max(CASTLE_PI * denom * denom, 1e-7);' + NL +
    '}' + NL +
    NL +
    'float GeometrySmithGGXCorrelated(float NdotV, float NdotL, float roughness) {' + NL +
    '  float alpha = max(roughness * roughness, 0.002025);' + NL +
    '  float alpha2 = alpha * alpha;' + NL +
    '  float ggxV = NdotL * sqrt(max(NdotV * NdotV * (1.0 - alpha2) + alpha2, 1e-7));' + NL +
    '  float ggxL = NdotV * sqrt(max(NdotL * NdotL * (1.0 - alpha2) + alpha2, 1e-7));' + NL +
    '  return 0.5 / max(ggxV + ggxL, 1e-5);' + NL +
    '}' + NL +
    NL +
    'vec3 FresnelSchlick(float cosTheta, vec3 F0) {' + NL +
    '  float clamped = clamp(1.0 - cosTheta, 0.0, 1.0);' + NL +
    '  float c2 = clamped * clamped;' + NL +
    '  return F0 + (1.0 - F0) * (c2 * c2 * clamped);' + NL +
    '}' + NL +
    NL +
    'vec3 FresnelSchlickRoughness(float cosTheta, vec3 F0, float roughness) {' + NL +
    '  float clamped = clamp(1.0 - cosTheta, 0.0, 1.0);' + NL +
    '  float c2 = clamped * clamped;' + NL +
    '  return F0 + (max(vec3(1.0 - roughness), F0) - F0) * (c2 * c2 * clamped);' + NL +
    '}' + NL +
    NL +
    '/* Screen-space derivative cotangent-frame TBN normal mapping (Christian Schuler) */' + NL +
    'vec3 PerturbNormalCotangent(vec3 N, vec3 V, vec2 texCoord, vec3 tangentNormal, float normalScale) {' + NL +
    '  vec3 dp1 = dFdx(-V);' + NL +
    '  vec3 dp2 = dFdy(-V);' + NL +
    '  vec2 duv1 = dFdx(texCoord);' + NL +
    '  vec2 duv2 = dFdy(texCoord);' + NL +
    '  vec3 dp2perp = cross(dp2, N);' + NL +
    '  vec3 dp1perp = cross(N, dp1);' + NL +
    '  vec3 T = dp2perp * duv1.x + dp1perp * duv2.x;' + NL +
    '  vec3 B = dp2perp * duv1.y + dp1perp * duv2.y;' + NL +
    '  float invmax = inversesqrt(max(dot(T, T), dot(B, B)) + 1e-7);' + NL +
    '  mat3 TBN = mat3(T * invmax, B * invmax, N);' + NL +
    '  vec3 scaledNormal = normalize(tangentNormal * vec3(normalScale, normalScale, 1.0));' + NL +
    '  return normalize(TBN * scaledNormal);' + NL +
    '}' + NL +
    NL +
    'vec3 PerturbNormalTBN(mat3 TBN, vec3 tangentNormal, float normalScale) {' + NL +
    '  vec3 scaledNormal = normalize(tangentNormal * vec3(normalScale, normalScale, 1.0));' + NL +
    '  return normalize(TBN * scaledNormal);' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateDirectCookTorranceGGX(vec3 N, vec3 V, vec3 L, vec3 lightRadiance,' + NL +
    '  vec3 albedo, float metallic, float roughness) {' + NL +
    '  vec3 H = normalize(V + L);' + NL +
    '  float NdotL = max(dot(N, L), 0.0);' + NL +
    '  float NdotV = max(dot(N, V), 1e-4);' + NL +
    '  float NdotH = max(dot(N, H), 0.0);' + NL +
    '  float HdotV = max(dot(H, V), 0.0);' + NL +
    '  if (NdotL <= 0.0) return vec3(0.0);' + NL +
    '  vec3 F0 = mix(vec3(0.04), albedo, metallic);' + NL +
    '  float D = DistributionGGX(NdotH, roughness);' + NL +
    '  float Vis = GeometrySmithGGXCorrelated(NdotV, NdotL, roughness);' + NL +
    '  vec3 F = FresnelSchlick(HdotV, F0);' + NL +
    '  vec3 specular = (D * Vis) * F;' + NL +
    '  vec3 kD = (vec3(1.0) - F) * (1.0 - metallic);' + NL +
    '  vec3 diffuse = kD * albedo / CASTLE_PI;' + NL +
    '  return (diffuse + specular) * lightRadiance * NdotL;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateIBLCookTorranceGGX(vec3 N, vec3 V, vec3 albedo, float metallic,' + NL +
    '  float roughness, float ao, vec3 diffuseIrradiance, vec3 prefilteredEnvColor, vec2 brdfLut) {' + NL +
    '  float NdotV = max(dot(N, V), 1e-4);' + NL +
    '  vec3 F0 = mix(vec3(0.04), albedo, metallic);' + NL +
    '  vec3 F = FresnelSchlickRoughness(NdotV, F0, roughness);' + NL +
    '  vec3 kD = (vec3(1.0) - F) * (1.0 - metallic);' + NL +
    '  vec3 diffuse = diffuseIrradiance * albedo * kD;' + NL +
    '  vec3 specular = prefilteredEnvColor * (F * brdfLut.x + brdfLut.y);' + NL +
    '  return (diffuse + specular) * ao;' + NL +
    '}' + NL +
    NL +
    'vec3 LINEARtoSRGB(vec3 color) {' + NL +
    '  return pow(max(color, vec3(0.0)), vec3(1.0 / 2.2));' + NL +
    '}' + NL +
    NL +
    'vec3 toneMapUncharted2Impl(vec3 color) {' + NL +
    '  const float A = 0.15;' + NL +
    '  const float B = 0.50;' + NL +
    '  const float C = 0.10;' + NL +
    '  const float D = 0.20;' + NL +
    '  const float E = 0.02;' + NL +
    '  const float F = 0.30;' + NL +
    '  return ((color * (A * color + C * B) + D * E) / (color * (A * color + B) + D * F)) - E / F;' + NL +
    '}' + NL +
    NL +
    'vec3 toneMapUncharted(vec3 color) {' + NL +
    '  const float W = 11.2;' + NL +
    '  color = toneMapUncharted2Impl(color * 2.0);' + NL +
    '  vec3 whiteScale = 1.0 / toneMapUncharted2Impl(vec3(W));' + NL +
    '  return color * whiteScale;' + NL +
    '}' + NL +
    NL +
    'vec3 toneMapHejlRichard(vec3 color) {' + NL +
    '  color = max(vec3(0.0), color - vec3(0.004));' + NL +
    '  return (color * (6.2 * color + 0.5)) / (color * (6.2 * color + 1.7) + 0.06);' + NL +
    '}' + NL +
    NL +
    'vec3 toneMapACES(vec3 color) {' + NL +
    '  const float A = 2.51;' + NL +
    '  const float B = 0.03;' + NL +
    '  const float C = 2.43;' + NL +
    '  const float D = 0.59;' + NL +
    '  const float E = 0.14;' + NL +
    '  return clamp((color * (A * color + B)) / (color * (C * color + D) + E), 0.0, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateDefault3DVertexShader(
  const HasSkinning: Boolean; const HasTangents: Boolean): String;
begin
  Result := GenerateDefault3DVertexShader(HasSkinning, HasTangents, True, True);
end;

function TCastleShaderGeneratorRHI.GenerateDefault3DVertexShader(
  const HasSkinning, HasTangents, HasTexCoord1, HasColorPerVertex: Boolean): String;
var
  PreambleText, Body: String;
begin
  if HasSkinning and HasTangents and HasTexCoord1 and HasColorPerVertex then
    PreambleText := GeneratePreamble(ssVertex)
  else
    PreambleText := GeneratePreambleFor3DVertex(HasSkinning, HasTangents, HasTexCoord1, HasColorPerVertex);

  Body :=
    PreambleText +
    'void main() {' + NL +
    '  vec4 localPos = castle_Vertex;' + NL +
    '  vec3 localNormal = castle_Normal;' + NL +
    '  vec3 localTangent = castle_Tangent.xyz;' + NL;

  if HasSkinning then
    Body := Body +
      '  if ((CastleSkinning.u_JointCount > 0u) && (castle_ObjectSkinningMode != 0)) {' + NL +
      '    mat4 skinMat =' + NL +
      '      castle_Weights0.x * CastleSkinning.u_JointMatrices[uint(castle_Joints0.x + 0.5)] +' + NL +
      '      castle_Weights0.y * CastleSkinning.u_JointMatrices[uint(castle_Joints0.y + 0.5)] +' + NL +
      '      castle_Weights0.z * CastleSkinning.u_JointMatrices[uint(castle_Joints0.z + 0.5)] +' + NL +
      '      castle_Weights0.w * CastleSkinning.u_JointMatrices[uint(castle_Joints0.w + 0.5)];' + NL +
      '    if (castle_ObjectSkinningMode == 2) {' + NL +
      '      /* Volume-preserving rotational orthonormalization (Dual-Quaternion mode) */' + NL +
      '      vec3 r0 = normalize(skinMat[0].xyz);' + NL +
      '      vec3 r1 = normalize(skinMat[1].xyz - dot(skinMat[1].xyz, r0) * r0);' + NL +
      '      vec3 r2 = cross(r0, r1);' + NL +
      '      mat3 skinRot = mat3(r0, r1, r2);' + NL +
      '      localPos = vec4(skinRot * localPos.xyz + skinMat[3].xyz, localPos.w);' + NL +
      '      localNormal = skinRot * localNormal;' + NL +
      '      localTangent = skinRot * localTangent;' + NL +
      '    } else {' + NL +
      '      localPos = skinMat * localPos;' + NL +
      '      localNormal = mat3(skinMat) * localNormal;' + NL +
      '      localTangent = mat3(skinMat) * localTangent;' + NL +
      '    }' + NL +
      '  }' + NL;

  Body := Body +
    '  castle_vertex_eye = castle_ModelViewMatrix * localPos;' + NL +
    '  castle_normal_eye = normalize(castle_NormalMatrix * localNormal);' + NL +
    '  castle_tex_coord0 = castle_MultiTexCoord0;' + NL +
    '  castle_tex_coord1 = castle_MultiTexCoord1;' + NL +
    '  castle_color_per_vertex = castle_ColorPerVertex;' + NL;

  if HasTangents then
    Body := Body +
      '  vec3 T = normalize(castle_NormalMatrix * localTangent);' + NL +
      '  vec3 N = castle_normal_eye;' + NL +
      '  T = normalize(T - dot(T, N) * N);' + NL +
      '  vec3 B = cross(N, T) * castle_Tangent.w;' + NL +
      '  castle_tangent_to_eye_space = mat3(T, B, N);' + NL
  else
    Body := Body +
      '  castle_tangent_to_eye_space = mat3(1.0);' + NL;

  Body := Body +
    '  gl_Position = castle_ProjectionMatrix * castle_vertex_eye;' + NL +
    '}' + NL;

  Result := FinalizeVertexShaderSource(Body);
end;

function TCastleShaderGeneratorRHI.GenerateClusteredForwardAndShadowFunctions: String;
begin
  Result :=
    '/* Set 0 / Set 1 Clustered Forward+ & Shadow Atlas Bindings */' + NL +
    'struct CastleClusteredLightData {' + NL +
    '  vec4 PositionAndRange;' + NL +
    '  vec4 DirectionAndSpotCos;' + NL +
    '  vec4 ColorAndIntensity;' + NL +
    '  vec4 ShadowAndTypeParams;' + NL +
    '};' + NL +
    NL +
    'struct CastleClusterGridEntryData {' + NL +
    '  uint Offset;' + NL +
    '  uint Count;' + NL +
    '};' + NL +
    NL +
    'struct CastlePositionalShadowTileData {' + NL +
    '  mat4 LightViewProjMatrix;' + NL +
    '  vec4 AtlasUVScaleOffset;' + NL +
    '};' + NL +
    NL +
    'layout(std140, set = 0, binding = 2) uniform CastleClusterCullingBlock {' + NL +
    '  mat4 u_InvProjectionMatrix;' + NL +
    '  vec4 u_ScreenAndDepthParams;' + NL +
    '  uvec4 u_ClusterGridDim;' + NL +
    '  vec4 u_LogDepthParams;' + NL +
    '} CastleClusterUniforms;' + NL +
    NL +
    'layout(std430, set = 0, binding = 3) readonly buffer CastleClusteredLightsBuffer {' + NL +
    '  CastleClusteredLightData u_ClusteredLights[];' + NL +
    '} CastleClusteredLightsSSBO;' + NL +
    NL +
    'layout(std430, set = 0, binding = 4) readonly buffer CastleClusterGridBuffer {' + NL +
    '  CastleClusterGridEntryData u_ClusterGrid[];' + NL +
    '} CastleClusterGridSSBO;' + NL +
    NL +
    'layout(std430, set = 0, binding = 5) readonly buffer CastleClusterLightIndexBuffer {' + NL +
    '  uint u_ClusterLightIndices[];' + NL +
    '} CastleClusterLightIndexSSBO;' + NL +
    NL +
    'layout(std140, set = 0, binding = 6) uniform CastleCascadeShadowBlock {' + NL +
    '  mat4 u_CascadeViewProjMatrix[4];' + NL +
    '  vec4 u_CascadeAtlasUVScaleOffset[4];' + NL +
    '  vec4 u_CascadeSplitDepths;' + NL +
    '} CastleCascadeShadows;' + NL +
    NL +
    'layout(std430, set = 0, binding = 7) readonly buffer CastlePositionalShadowTilesBuffer {' + NL +
    '  CastlePositionalShadowTileData u_ShadowTiles[];' + NL +
    '} CastlePositionalShadowTilesSSBO;' + NL +
    NL +
    'layout(set = 1, binding = 8) uniform sampler2D castle_shadow_atlas_texture;' + NL +
    NL +
    'uvec3 ComputeClusterIndex3D(vec2 fragCoord, float positiveViewDepth) {' + NL +
    '  vec2 screenSize = max(CastleClusterUniforms.u_ScreenAndDepthParams.xy, vec2(1.0));' + NL +
    '  float zNear = max(CastleClusterUniforms.u_ScreenAndDepthParams.z, 1e-3);' + NL +
    '  float zFar = max(CastleClusterUniforms.u_ScreenAndDepthParams.w, zNear + 1e-2);' + NL +
    '  float clampedZ = clamp(positiveViewDepth, zNear, zFar);' + NL +
    '  uint cx = min(uint((fragCoord.x / screenSize.x) * float(CastleClusterUniforms.u_ClusterGridDim.x)), CastleClusterUniforms.u_ClusterGridDim.x - 1u);' + NL +
    '  uint cy = min(uint((fragCoord.y / screenSize.y) * float(CastleClusterUniforms.u_ClusterGridDim.y)), CastleClusterUniforms.u_ClusterGridDim.y - 1u);' + NL +
    '  float sliceF = log(clampedZ / zNear) / max(log(zFar / zNear), 1e-5) * float(CastleClusterUniforms.u_ClusterGridDim.z);' + NL +
    '  uint cz = min(uint(max(sliceF, 0.0)), CastleClusterUniforms.u_ClusterGridDim.z - 1u);' + NL +
    '  return uvec3(cx, cy, cz);' + NL +
    '}' + NL +
    NL +
    'uint FlattenClusterIndex(uvec3 clusterCoord) {' + NL +
    '  return (clusterCoord.z * CastleClusterUniforms.u_ClusterGridDim.y + clusterCoord.y) * CastleClusterUniforms.u_ClusterGridDim.x + clusterCoord.x;' + NL +
    '}' + NL +
    NL +
    'float SampleShadowAtlasPCF3x3(vec2 atlasUV, float compareDepth, vec2 texelSize) {' + NL +
    '  float shadow = 0.0;' + NL +
    '  for (int y = -1; y <= 1; ++y) {' + NL +
    '    for (int x = -1; x <= 1; ++x) {' + NL +
    '      vec2 sampleUV = atlasUV + vec2(float(x), float(y)) * texelSize;' + NL +
    '      float closestDepth = texture(castle_shadow_atlas_texture, sampleUV).r;' + NL +
    '      shadow += (compareDepth <= closestDepth) ? 1.0 : 0.0;' + NL +
    '    }' + NL +
    '  }' + NL +
    '  return shadow * (1.0 / 9.0);' + NL +
    '}' + NL +
    NL +
    'float SampleShadowAtlasPCSS(vec2 atlasUV, float compareDepth, vec2 texelSize) {' + NL +
    '  float blockerSum = 0.0;' + NL +
    '  float blockerCount = 0.0;' + NL +
    '  for (int y = -1; y <= 1; ++y) {' + NL +
    '    for (int x = -1; x <= 1; ++x) {' + NL +
    '      vec2 searchUV = atlasUV + vec2(float(x), float(y)) * texelSize * 2.0;' + NL +
    '      float d = texture(castle_shadow_atlas_texture, searchUV).r;' + NL +
    '      if (d < compareDepth) {' + NL +
    '        blockerSum += d;' + NL +
    '        blockerCount += 1.0;' + NL +
    '      }' + NL +
    '    }' + NL +
    '  }' + NL +
    '  if (blockerCount < 0.5) return 1.0;' + NL +
    '  float avgBlockerDepth = max(blockerSum / blockerCount, 1e-4);' + NL +
    '  float penumbraScale = clamp((compareDepth - avgBlockerDepth) / avgBlockerDepth * 3.0, 1.0, 4.0);' + NL +
    '  return SampleShadowAtlasPCF3x3(atlasUV, compareDepth, texelSize * penumbraScale);' + NL +
    '}' + NL +
    NL +
    'float EvaluateDirectionalCascadeShadow(vec3 worldPos, float positiveViewDepth, float NdotL) {' + NL +
    '  int cascadeIdx = 3;' + NL +
    '  if (positiveViewDepth < CastleCascadeShadows.u_CascadeSplitDepths.x) cascadeIdx = 0;' + NL +
    '  else if (positiveViewDepth < CastleCascadeShadows.u_CascadeSplitDepths.y) cascadeIdx = 1;' + NL +
    '  else if (positiveViewDepth < CastleCascadeShadows.u_CascadeSplitDepths.z) cascadeIdx = 2;' + NL +
    '  vec4 lightClip = CastleCascadeShadows.u_CascadeViewProjMatrix[cascadeIdx] * vec4(worldPos, 1.0);' + NL +
    '  vec3 lightNDC = lightClip.xyz / max(lightClip.w, 1e-5);' + NL +
    '  vec2 uv01 = lightNDC.xy * 0.5 + 0.5;' + NL +
    '  if (uv01.x < 0.0 || uv01.x > 1.0 || uv01.y < 0.0 || uv01.y > 1.0 || lightNDC.z < 0.0 || lightNDC.z > 1.0) return 1.0;' + NL +
    '  vec4 scaleOffset = CastleCascadeShadows.u_CascadeAtlasUVScaleOffset[cascadeIdx];' + NL +
    '  vec2 atlasUV = uv01 * scaleOffset.xy + scaleOffset.zw;' + NL +
    '  float slopeBias = max(0.0025 * (1.0 - max(NdotL, 0.0)), 0.0005);' + NL +
    '  vec2 texelSize = vec2(1.0) / max(vec2(textureSize(castle_shadow_atlas_texture, 0)), vec2(1.0));' + NL +
    '  if (castle_ObjectShadowMapMode == 1)' + NL +
    '    return SampleShadowAtlasPCSS(atlasUV, lightNDC.z - slopeBias, texelSize);' + NL +
    '  return SampleShadowAtlasPCF3x3(atlasUV, lightNDC.z - slopeBias, texelSize);' + NL +
    '}' + NL +
    NL +
    'float EvaluatePositionalAtlasShadow(vec3 worldPos, int shadowTileIdx, float bias) {' + NL +
    '  if (shadowTileIdx < 0) return 1.0;' + NL +
    '  CastlePositionalShadowTileData tile = CastlePositionalShadowTilesSSBO.u_ShadowTiles[shadowTileIdx];' + NL +
    '  vec4 lightClip = tile.LightViewProjMatrix * vec4(worldPos, 1.0);' + NL +
    '  if (lightClip.w <= 1e-5) return 1.0;' + NL +
    '  vec3 lightNDC = lightClip.xyz / lightClip.w;' + NL +
    '  vec2 uv01 = lightNDC.xy * 0.5 + 0.5;' + NL +
    '  if (uv01.x < 0.0 || uv01.x > 1.0 || uv01.y < 0.0 || uv01.y > 1.0 || lightNDC.z < 0.0 || lightNDC.z > 1.0) return 1.0;' + NL +
    '  vec2 atlasUV = uv01 * tile.AtlasUVScaleOffset.xy + tile.AtlasUVScaleOffset.zw;' + NL +
    '  vec2 texelSize = vec2(1.0) / max(vec2(textureSize(castle_shadow_atlas_texture, 0)), vec2(1.0));' + NL +
    '  return SampleShadowAtlasPCF3x3(atlasUV, lightNDC.z - max(bias, 0.0005), texelSize);' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateClusteredForwardLights(vec2 fragCoord, vec3 eyePos, vec3 worldPos, vec3 N, vec3 V, vec3 baseColor, float metallic, float roughness, bool useShadows) {' + NL +
    '  float positiveViewDepth = max(-eyePos.z, 1e-3);' + NL +
    '  uvec3 clusterCoord = ComputeClusterIndex3D(fragCoord, positiveViewDepth);' + NL +
    '  uint clusterIdx = FlattenClusterIndex(clusterCoord);' + NL +
    '  CastleClusterGridEntryData entry = CastleClusterGridSSBO.u_ClusterGrid[clusterIdx];' + NL +
    '  vec3 totalRadiance = vec3(0.0);' + NL +
    '  uint count = min(entry.Count, 64u);' + NL +
    '  for (uint i = 0u; i < count; ++i) {' + NL +
    '    uint lightIdx = CastleClusterLightIndexSSBO.u_ClusterLightIndices[entry.Offset + i];' + NL +
    '    CastleClusteredLightData cl = CastleClusteredLightsSSBO.u_ClusteredLights[lightIdx];' + NL +
    '    int lightType = int(cl.ShadowAndTypeParams.x + 0.5);' + NL +
    '    vec3 L;' + NL +
    '    float attenuation = 1.0;' + NL +
    '    if (lightType == 0) {' + NL +
    '      L = normalize(-cl.DirectionAndSpotCos.xyz);' + NL +
    '    } else {' + NL +
    '      vec3 toLight = cl.PositionAndRange.xyz - eyePos;' + NL +
    '      float dist = length(toLight);' + NL +
    '      float range = max(cl.PositionAndRange.w, 1e-4);' + NL +
    '      if (dist >= range) continue;' + NL +
    '      L = toLight / max(dist, 1e-5);' + NL +
    '      float distRatio = dist / range;' + NL +
    '      float windowFactor = max(1.0 - pow(distRatio, 4.0), 0.0);' + NL +
    '      attenuation = (windowFactor * windowFactor) / (dist * dist + 1.0);' + NL +
    '      if (lightType == 2) {' + NL +
    '        float cosDir = dot(-L, normalize(cl.DirectionAndSpotCos.xyz));' + NL +
    '        float outerCos = cl.DirectionAndSpotCos.w;' + NL +
    '        float innerCos = max(cl.ShadowAndTypeParams.y, outerCos + 1e-4);' + NL +
    '        float spotFactor = clamp((cosDir - outerCos) / (innerCos - outerCos), 0.0, 1.0);' + NL +
    '        attenuation *= spotFactor * spotFactor;' + NL +
    '      }' + NL +
    '    }' + NL +
    '    if (attenuation <= 0.0) continue;' + NL +
    '    float shadowVis = 1.0;' + NL +
    '    if (useShadows) {' + NL +
    '      if (lightType == 0) {' + NL +
    '        shadowVis = EvaluateDirectionalCascadeShadow(worldPos, positiveViewDepth, max(dot(N, L), 0.0));' + NL +
    '      } else {' + NL +
    '        int tileIdx = int(floor(cl.ShadowAndTypeParams.z + 0.5));' + NL +
    '        shadowVis = EvaluatePositionalAtlasShadow(worldPos, tileIdx, cl.ShadowAndTypeParams.w);' + NL +
    '      }' + NL +
    '    }' + NL +
    '    vec3 radiance = cl.ColorAndIntensity.rgb * (cl.ColorAndIntensity.w * attenuation * shadowVis);' + NL +
    '    totalRadiance += EvaluateDirectCookTorranceGGX(N, V, L, radiance, baseColor, metallic, roughness);' + NL +
    '  }' + NL +
    '  return totalRadiance;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateDefault3DFragmentShader(
  const HasBaseColorTex: Boolean;
  const HasNormalTex: Boolean;
  const HasMetallicRoughnessTex: Boolean;
  const HasEmissiveTex: Boolean;
  const HasOcclusionTex: Boolean;
  const HasIBL: Boolean;
  const UseAlphaTest: Boolean;
  const HasClusteredForward: Boolean;
  const HasCascadedShadows: Boolean): String;
begin
  Result :=
    GeneratePreamble(ssFragment) +
    GeneratePBRFunctions + NL;

  if HasClusteredForward or HasCascadedShadows then
    Result := Result + GenerateClusteredForwardAndShadowFunctions + NL;

  Result := Result +
    'void main() {' + NL +
    '  vec4 baseColor = castle_MaterialBaseColor;' + NL;

  if HasBaseColorTex then
    Result := Result +
      '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + NL;

  if UseAlphaTest then
    Result := Result +
      '  if (baseColor.a < castle_AlphaCutoff) discard;' + NL;

  Result := Result +
    '  /* Unlit Material Early Out (SpecularColor.a < 0.0) */' + NL +
    '  if (castle_MaterialShininess < 0.0) {' + NL;
  if HasEmissiveTex then
    Result := Result +
      '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive * texture(castle_emissive_texture, castle_tex_coord0).rgb;' + NL
  else
    Result := Result +
      '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive;' + NL;
  Result := Result +
    '    castle_FragColor = vec4(unlitColor, baseColor.a);' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  float metallic = clamp(castle_MaterialMetallic, 0.0, 1.0);' + NL +
    '  float roughness = clamp(castle_MaterialRoughness, 0.04, 1.0);' + NL;

  if HasMetallicRoughnessTex then
    Result := Result +
      '  vec4 mrSample = texture(castle_metallic_roughness_texture, castle_tex_coord0);' + NL +
      '  roughness = clamp(roughness * mrSample.g, 0.04, 1.0);' + NL +
      '  metallic = clamp(metallic * mrSample.b, 0.0, 1.0);' + NL;

  Result := Result +
    '  float ao = 1.0;' + NL;
  if HasOcclusionTex then
    Result := Result +
      '  float occSample = texture(castle_occlusion_texture, castle_tex_coord0).r;' + NL +
      '  ao = mix(1.0, occSample, clamp(castle_MaterialOcclusionStrength, 0.0, 1.0));' + NL;

  Result := Result +
    '  vec3 N = normalize(castle_normal_eye);' + NL +
    '  vec3 V = normalize(-castle_vertex_eye.xyz);' + NL;

  if HasNormalTex then
    Result := Result +
      '  vec3 tangentNormal = texture(castle_normal_texture, castle_tex_coord0).xyz * 2.0 - 1.0;' + NL +
      '  N = PerturbNormalCotangent(N, -castle_vertex_eye.xyz, castle_tex_coord0, tangentNormal, castle_MaterialNormalScale);' + NL;

  Result := Result +
    '  if (castle_ObjectSpecularAAEnabled) {' + NL +
    '    vec3 dndu = dFdx(N);' + NL +
    '    vec3 dndv = dFdy(N);' + NL +
    '    float normalVar = 0.25 * (dot(dndu, dndu) + dot(dndv, dndv));' + NL +
    '    float kernelRoughness2 = min(2.0 * normalVar, 0.18);' + NL +
    '    roughness = clamp(sqrt(clamp(roughness * roughness + kernelRoughness2, 0.0, 1.0)), 0.04, 1.0);' + NL +
    '  }' + NL +
    '  vec3 Lo = vec3(0.0);' + NL;

  if HasClusteredForward then
  begin
    Result := Result +
      '  vec3 worldPos = (castle_ViewMatrixInverse * castle_vertex_eye).xyz;' + NL +
      Format('  Lo += EvaluateClusteredForwardLights(gl_FragCoord.xy, castle_vertex_eye.xyz, worldPos, N, V, baseColor.rgb, metallic, roughness, %s);',
        [IfThen(HasCascadedShadows, 'true', 'false')]) + NL;
  end else
  begin
    Result := Result +
      '  uint lightCount = min(castle_LightCount, 32u);' + NL +
      '  for (uint i = 0u; i < lightCount; ++i) {' + NL +
      '    CastleLightData light = CastleLights.u_Lights[i];' + NL +
      '    vec3 L;' + NL +
      '    float attenuation = 1.0;' + NL +
      '    if (light.Position.w == 0.0) {' + NL +
      '      L = normalize(-light.Position.xyz);' + NL +
      '    } else {' + NL +
      '      vec3 toLight = light.Position.xyz - castle_vertex_eye.xyz;' + NL +
      '      float dist = length(toLight);' + NL +
      '      L = toLight / max(dist, 1e-5);' + NL +
      '      float attenDenom = light.Attenuation.x + light.Attenuation.y * dist + light.Attenuation.z * dist * dist;' + NL +
      '      if (attenDenom > 0.0) attenuation = 1.0 / attenDenom;' + NL +
      '      if (light.Color.w > 0.0 && dist > light.Color.w) attenuation = 0.0;' + NL +
      '      if (light.Position.w > 1.5 && light.SpotDirection.w >= 0.0) {' + NL +
      '        float cosDir = dot(-L, normalize(light.SpotDirection.xyz));' + NL +
      '        if (cosDir < light.SpotDirection.w) {' + NL +
      '          attenuation = 0.0;' + NL +
      '        } else if (light.SpotParams.y > 0.0 && light.SpotParams.y < light.SpotParams.x) {' + NL +
      '          attenuation *= clamp((acos(clamp(cosDir, -1.0, 1.0)) - light.SpotParams.x) / (light.SpotParams.y - light.SpotParams.x), 0.0, 1.0);' + NL +
      '        } else if (light.SpotParams.z > 0.0) {' + NL +
      '          attenuation *= pow(max(cosDir, 0.0), light.SpotParams.z);' + NL +
      '        }' + NL +
      '      }' + NL +
      '    }' + NL +
      '    float shadowVis = 1.0;' + NL;
    if HasCascadedShadows then
      Result := Result +
        '    if (light.Position.w == 0.0) {' + NL +
        '      vec3 worldPos = (castle_ViewMatrixInverse * castle_vertex_eye).xyz;' + NL +
        '      shadowVis = EvaluateDirectionalCascadeShadow(worldPos, max(-castle_vertex_eye.z, 1e-3), max(dot(N, L), 0.0));' + NL +
        '    }' + NL;
    Result := Result +
      '    vec3 radiance = light.Color.rgb * (attenuation * shadowVis);' + NL +
      '    Lo += EvaluateDirectCookTorranceGGX(N, V, L, radiance, baseColor.rgb, metallic, roughness);' + NL +
      '    Lo += light.AmbientColor.rgb * baseColor.rgb * (ao * attenuation);' + NL +
      '  }' + NL;
  end;

  if HasIBL then
    Result := Result +
      '  float NdotV = max(dot(N, V), 1e-4);' + NL +
      '  vec3 R = reflect(-V, N);' + NL +
      '  vec3 worldR = mat3(castle_ViewMatrixInverse) * R;' + NL +
      '  vec3 prefilteredEnv = textureLod(castle_env_specular_texture, worldR, roughness * 4.0).rgb;' + NL +
      '  vec2 brdfLut = texture(castle_brdf_lut_texture, vec2(NdotV, roughness)).rg;' + NL +
      '  vec3 diffuseIrradiance = textureLod(castle_env_specular_texture, mat3(castle_ViewMatrixInverse) * N, 4.0).rgb;' + NL +
      '  Lo += EvaluateIBLCookTorranceGGX(N, V, baseColor.rgb, metallic, roughness, ao, diffuseIrradiance, prefilteredEnv, brdfLut);' + NL;

  Result := Result +
    '  vec3 emissive = castle_MaterialEmissive;' + NL;
  if HasEmissiveTex then
    Result := Result +
      '  emissive *= texture(castle_emissive_texture, castle_tex_coord0).rgb;' + NL;

  Result := Result +
    '  castle_FragColor = vec4(Lo + emissive, baseColor.a);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateIBLBrdfLutComputeShader(
  const AWorkgroupSizeX: Cardinal;
  const AWorkgroupSizeY: Cardinal): String;
var
  LocalSizeX, LocalSizeY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then
    LocalSizeX := 8
  else
    LocalSizeX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then
    LocalSizeY := 8
  else
    LocalSizeY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalSizeX, LocalSizeY]) + NL +
    NL +
    'layout(set = 0, binding = 0, rgba8) uniform writeonly image2D u_BrdfLutImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleIBLBrdfLutPushConstants {' + NL +
    '  uvec2 u_LutSize;' + NL +
    '  uint  u_SampleCount;' + NL +
    '  uint  u_Reserved;' + NL +
    '} pc;' + NL +
    NL +
    'const float CASTLE_PI = 3.14159265358979323846;' + NL +
    NL +
    'float RadicalInverse_VdC(uint bits) {' + NL +
    '  bits = (bits << 16u) | (bits >> 16u);' + NL +
    '  bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);' + NL +
    '  bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);' + NL +
    '  bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);' + NL +
    '  bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);' + NL +
    '  return float(bits) * 2.3283064365386963e-10;' + NL +
    '}' + NL +
    NL +
    'vec2 Hammersley(uint i, uint N) {' + NL +
    '  return vec2(float(i) / float(max(N, 1u)), RadicalInverse_VdC(i));' + NL +
    '}' + NL +
    NL +
    'vec3 ImportanceSampleGGX(vec2 Xi, vec3 N, float roughness) {' + NL +
    '  float a = roughness * roughness;' + NL +
    '  float phi = 2.0 * CASTLE_PI * Xi.x;' + NL +
    '  float cosTheta = sqrt((1.0 - Xi.y) / (1.0 + (a * a - 1.0) * Xi.y));' + NL +
    '  float sinTheta = sqrt(max(1.0 - cosTheta * cosTheta, 0.0));' + NL +
    '  vec3 H = vec3(cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta);' + NL +
    '  vec3 up = abs(N.z) < 0.999 ? vec3(0.0, 0.0, 1.0) : vec3(1.0, 0.0, 0.0);' + NL +
    '  vec3 tangent = normalize(cross(up, N));' + NL +
    '  vec3 bitangent = cross(N, tangent);' + NL +
    '  return normalize(tangent * H.x + bitangent * H.y + N * H.z);' + NL +
    '}' + NL +
    NL +
    'float GeometrySchlickGGX_IBL(float NdotV, float roughness) {' + NL +
    '  float a = roughness;' + NL +
    '  float k = (a * a) * 0.5;' + NL +
    '  return NdotV / max(NdotV * (1.0 - k) + k, 1e-5);' + NL +
    '}' + NL +
    NL +
    'float GeometrySmith_IBL(float NdotV, float NdotL, float roughness) {' + NL +
    '  return GeometrySchlickGGX_IBL(NdotV, roughness) * GeometrySchlickGGX_IBL(NdotL, roughness);' + NL +
    '}' + NL +
    NL +
    'vec2 IntegrateBRDF(float NdotV, float roughness, uint sampleCount) {' + NL +
    '  vec3 V = vec3(sqrt(max(1.0 - NdotV * NdotV, 0.0)), 0.0, NdotV);' + NL +
    '  float A = 0.0;' + NL +
    '  float B = 0.0;' + NL +
    '  vec3 N = vec3(0.0, 0.0, 1.0);' + NL +
    '  for (uint i = 0u; i < sampleCount; ++i) {' + NL +
    '    vec2 Xi = Hammersley(i, sampleCount);' + NL +
    '    vec3 H = ImportanceSampleGGX(Xi, N, roughness);' + NL +
    '    vec3 L = normalize(2.0 * dot(V, H) * H - V);' + NL +
    '    float NdotL = max(L.z, 0.0);' + NL +
    '    float NdotH = max(H.z, 0.0);' + NL +
    '    float VdotH = max(dot(V, H), 0.0);' + NL +
    '    if (NdotL > 0.0) {' + NL +
    '      float G = GeometrySmith_IBL(NdotV, NdotL, roughness);' + NL +
    '      float G_Vis = (G * VdotH) / max(NdotH * NdotV, 1e-5);' + NL +
    '      float Fc = pow(1.0 - VdotH, 5.0);' + NL +
    '      A += (1.0 - Fc) * G_Vis;' + NL +
    '      B += Fc * G_Vis;' + NL +
    '    }' + NL +
    '  }' + NL +
    '  return vec2(A, B) / float(max(sampleCount, 1u));' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uvec2 coord = gl_GlobalInvocationID.xy;' + NL +
    '  if (coord.x >= pc.u_LutSize.x || coord.y >= pc.u_LutSize.y) return;' + NL +
    '  float NdotV = (float(coord.x) + 0.5) / float(max(pc.u_LutSize.x, 1u));' + NL +
    '  float roughness = max((float(coord.y) + 0.5) / float(max(pc.u_LutSize.y, 1u)), 0.04);' + NL +
    '  vec2 res = IntegrateBRDF(NdotV, roughness, max(pc.u_SampleCount, 16u));' + NL +
    '  imageStore(u_BrdfLutImage, ivec2(coord), vec4(res.x, res.y, 0.0, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateClusteredLightCullingComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
var
  LocalSizeX: Cardinal;
begin
  if AWorkgroupSizeX = 0 then
    LocalSizeX := 64
  else
    LocalSizeX := AWorkgroupSizeX;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [LocalSizeX]) + NL +
    NL +
    'struct CastleClusteredLightData {' + NL +
    '  vec4 PositionAndRange;' + NL +
    '  vec4 DirectionAndSpotCos;' + NL +
    '  vec4 ColorAndIntensity;' + NL +
    '  vec4 ShadowAndTypeParams;' + NL +
    '};' + NL +
    NL +
    'struct CastleClusterGridEntryData {' + NL +
    '  uint Offset;' + NL +
    '  uint Count;' + NL +
    '};' + NL +
    NL +
    'layout(std140, set = 0, binding = 0) uniform CastleClusterCullingBlock {' + NL +
    '  mat4 u_InvProjectionMatrix;' + NL +
    '  vec4 u_ScreenAndDepthParams;' + NL +
    '  uvec4 u_ClusterGridDim;' + NL +
    '  vec4 u_LogDepthParams;' + NL +
    '} u_ClusterInfo;' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) readonly buffer CastleClusteredLightsBuffer {' + NL +
    '  CastleClusteredLightData u_Lights[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 2) writeonly buffer CastleClusterGridBuffer {' + NL +
    '  CastleClusterGridEntryData u_ClusterGrid[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 3) writeonly buffer CastleClusterLightIndexBuffer {' + NL +
    '  uint u_ClusterLightIndices[];' + NL +
    '};' + NL +
    NL +
    'vec3 ScreenUVToViewAtZ(vec2 uv, float viewZ) {' + NL +
    '  vec4 ndc = vec4(uv * 2.0 - 1.0, 1.0, 1.0);' + NL +
    '  vec4 viewRay = u_ClusterInfo.u_InvProjectionMatrix * ndc;' + NL +
    '  viewRay.xyz /= max(abs(viewRay.w), 1e-5);' + NL +
    '  float rayScale = viewZ / min(viewRay.z, -1e-5);' + NL +
    '  return viewRay.xyz * rayScale;' + NL +
    '}' + NL +
    NL +
    'bool SphereIntersectsAABB(vec3 center, float radius, vec3 aabbMin, vec3 aabbMax) {' + NL +
    '  vec3 closest = clamp(center, aabbMin, aabbMax);' + NL +
    '  vec3 diff = center - closest;' + NL +
    '  return dot(diff, diff) <= (radius * radius);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint clusterIdx = gl_GlobalInvocationID.x;' + NL +
    '  uint gridX = max(u_ClusterInfo.u_ClusterGridDim.x, 1u);' + NL +
    '  uint gridY = max(u_ClusterInfo.u_ClusterGridDim.y, 1u);' + NL +
    '  uint gridZ = max(u_ClusterInfo.u_ClusterGridDim.z, 1u);' + NL +
    '  uint totalClusters = gridX * gridY * gridZ;' + NL +
    '  if (clusterIdx >= totalClusters) return;' + NL +
    NL +
    '  uint cx = clusterIdx % gridX;' + NL +
    '  uint cy = (clusterIdx / gridX) % gridY;' + NL +
    '  uint cz = clusterIdx / (gridX * gridY);' + NL +
    NL +
    '  float zNear = max(u_ClusterInfo.u_ScreenAndDepthParams.z, 1e-3);' + NL +
    '  float zFar = max(u_ClusterInfo.u_ScreenAndDepthParams.w, zNear + 1e-2);' + NL +
    '  float sliceNear = -zNear * pow(zFar / zNear, float(cz) / float(gridZ));' + NL +
    '  float sliceFar  = -zNear * pow(zFar / zNear, float(cz + 1u) / float(gridZ));' + NL +
    NL +
    '  vec2 uvMin = vec2(float(cx) / float(gridX), float(cy) / float(gridY));' + NL +
    '  vec2 uvMax = vec2(float(cx + 1u) / float(gridX), float(cy + 1u) / float(gridY));' + NL +
    '  vec3 p0 = ScreenUVToViewAtZ(uvMin, sliceNear);' + NL +
    '  vec3 p1 = ScreenUVToViewAtZ(uvMax, sliceNear);' + NL +
    '  vec3 p2 = ScreenUVToViewAtZ(uvMin, sliceFar);' + NL +
    '  vec3 p3 = ScreenUVToViewAtZ(uvMax, sliceFar);' + NL +
    '  vec3 aabbMin = min(min(p0, p1), min(p2, p3));' + NL +
    '  vec3 aabbMax = max(max(p0, p1), max(p2, p3));' + NL +
    NL +
    '  uint maxPerCluster = max(uint(u_ClusterInfo.u_LogDepthParams.z), 64u);' + NL +
    '  uint baseOffset = clusterIdx * maxPerCluster;' + NL +
    '  uint visibleCount = 0u;' + NL +
    '  uint lightCount = min(u_ClusterInfo.u_ClusterGridDim.w, 256u);' + NL +
    NL +
    '  for (uint i = 0u; i < lightCount; ++i) {' + NL +
    '    if (visibleCount >= maxPerCluster) break;' + NL +
    '    CastleClusteredLightData light = u_Lights[i];' + NL +
    '    int lightType = int(light.ShadowAndTypeParams.x + 0.5);' + NL +
    '    bool intersects = (lightType == 0) || SphereIntersectsAABB(light.PositionAndRange.xyz, light.PositionAndRange.w, aabbMin, aabbMax);' + NL +
    '    if (intersects) {' + NL +
    '      u_ClusterLightIndices[baseOffset + visibleCount] = i;' + NL +
    '      visibleCount++;' + NL +
    '    }' + NL +
    '  }' + NL +
    NL +
    '  CastleClusterGridEntryData entry;' + NL +
    '  entry.Offset = baseOffset;' + NL +
    '  entry.Count = visibleCount;' + NL +
    '  u_ClusterGrid[clusterIdx] = entry;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateBloomDownsampleComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_SrcTexture;' + NL +
    'layout(set = 0, binding = 1, rgba16f) uniform writeonly image2D u_DstImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleBloomPushConstants {' + NL +
    '  vec2  u_SrcTexelSize;' + NL +
    '  float u_Threshold;' + NL +
    '  float u_Knee;' + NL +
    '  float u_FilterRadius;' + NL +
    '  uint  u_MipLevel;' + NL +
    '  uint  u_IsFirstDownsample;' + NL +
    '  uint  u_DstPackedSize;' + NL +
    '} pc;' + NL +
    NL +
    'float LuminanceRec709(vec3 c) {' + NL +
    '  return dot(c, vec3(0.2126, 0.7152, 0.0722));' + NL +
    '}' + NL +
    NL +
    'float KarisWeight(vec3 c) {' + NL +
    '  return 1.0 / (1.0 + LuminanceRec709(c));' + NL +
    '}' + NL +
    NL +
    'vec3 ApplySoftThreshold(vec3 c, float threshold, float knee) {' + NL +
    '  float brightness = max(max(c.r, c.g), c.b);' + NL +
    '  float soft = clamp(brightness - threshold + knee, 0.0, 2.0 * knee);' + NL +
    '  soft = (soft * soft) / max(4.0 * knee + 1e-5, 1e-5);' + NL +
    '  float contribution = max(soft, brightness - threshold) / max(brightness, 1e-5);' + NL +
    '  return c * max(contribution, 0.0);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uvec2 dstSize = uvec2(pc.u_DstPackedSize & 0xFFFFu, pc.u_DstPackedSize >> 16u);' + NL +
    '  uvec2 coord = gl_GlobalInvocationID.xy;' + NL +
    '  if (coord.x >= dstSize.x || coord.y >= dstSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(coord) + vec2(0.5)) / vec2(max(dstSize, uvec2(1u)));' + NL +
    '  vec2 d = pc.u_SrcTexelSize;' + NL +
    NL +
    '  /* 13-tap Jiménez Dual-Filtering Downsample Kernel:' + NL +
    '     a - b - c' + NL +
    '     - j - k -' + NL +
    '     d - e - f' + NL +
    '     - l - m -' + NL +
    '     g - h - i */' + NL +
    '  vec3 a = texture(u_SrcTexture, uv + vec2(-2.0, -2.0) * d).rgb;' + NL +
    '  vec3 b = texture(u_SrcTexture, uv + vec2( 0.0, -2.0) * d).rgb;' + NL +
    '  vec3 c = texture(u_SrcTexture, uv + vec2( 2.0, -2.0) * d).rgb;' + NL +
    '  vec3 d0 = texture(u_SrcTexture, uv + vec2(-2.0,  0.0) * d).rgb;' + NL +
    '  vec3 e = texture(u_SrcTexture, uv).rgb;' + NL +
    '  vec3 f = texture(u_SrcTexture, uv + vec2( 2.0,  0.0) * d).rgb;' + NL +
    '  vec3 g = texture(u_SrcTexture, uv + vec2(-2.0,  2.0) * d).rgb;' + NL +
    '  vec3 h = texture(u_SrcTexture, uv + vec2( 0.0,  2.0) * d).rgb;' + NL +
    '  vec3 i = texture(u_SrcTexture, uv + vec2( 2.0,  2.0) * d).rgb;' + NL +
    '  vec3 j = texture(u_SrcTexture, uv + vec2(-1.0, -1.0) * d).rgb;' + NL +
    '  vec3 k = texture(u_SrcTexture, uv + vec2( 1.0, -1.0) * d).rgb;' + NL +
    '  vec3 l = texture(u_SrcTexture, uv + vec2(-1.0,  1.0) * d).rgb;' + NL +
    '  vec3 m = texture(u_SrcTexture, uv + vec2( 1.0,  1.0) * d).rgb;' + NL +
    NL +
    '  vec3 downsample;' + NL +
    '  if (pc.u_IsFirstDownsample != 0u) {' + NL +
    '    vec3 g0 = (a + b + d0 + e) * 0.25;' + NL +
    '    vec3 g1 = (b + c + e + f) * 0.25;' + NL +
    '    vec3 g2 = (d0 + e + g + h) * 0.25;' + NL +
    '    vec3 g3 = (e + f + h + i) * 0.25;' + NL +
    '    vec3 g4 = (j + k + l + m) * 0.25;' + NL +
    '    float w0 = KarisWeight(g0);' + NL +
    '    float w1 = KarisWeight(g1);' + NL +
    '    float w2 = KarisWeight(g2);' + NL +
    '    float w3 = KarisWeight(g3);' + NL +
    '    float w4 = KarisWeight(g4);' + NL +
    '    downsample = (g0 * w0 + g1 * w1 + g2 * w2 + g3 * w3) * 0.125 / max(0.25 * (w0 + w1 + w2 + w3), 1e-5)' + NL +
    '               + (g4 * 0.5);' + NL +
    '    downsample = ApplySoftThreshold(downsample, pc.u_Threshold, pc.u_Knee);' + NL +
    '  } else {' + NL +
    '    downsample  = e * 0.125;' + NL +
    '    downsample += (a + c + g + i) * 0.03125;' + NL +
    '    downsample += (b + d0 + f + h) * 0.0625;' + NL +
    '    downsample += (j + k + l + m) * 0.125;' + NL +
    '  }' + NL +
    NL +
    '  imageStore(u_DstImage, ivec2(coord), vec4(max(downsample, vec3(0.0)), 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateBloomUpsampleComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_SrcLowMipTexture;' + NL +
    'layout(set = 0, binding = 1, rgba16f) uniform image2D u_DstHighMipImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleBloomPushConstants {' + NL +
    '  vec2  u_SrcTexelSize;' + NL +
    '  float u_Threshold;' + NL +
    '  float u_Knee;' + NL +
    '  float u_FilterRadius;' + NL +
    '  uint  u_MipLevel;' + NL +
    '  uint  u_IsFirstDownsample;' + NL +
    '  uint  u_DstPackedSize;' + NL +
    '} pc;' + NL +
    NL +
    'void main() {' + NL +
    '  uvec2 dstSize = uvec2(pc.u_DstPackedSize & 0xFFFFu, pc.u_DstPackedSize >> 16u);' + NL +
    '  uvec2 coord = gl_GlobalInvocationID.xy;' + NL +
    '  if (coord.x >= dstSize.x || coord.y >= dstSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(coord) + vec2(0.5)) / vec2(max(dstSize, uvec2(1u)));' + NL +
    '  vec2 d = pc.u_SrcTexelSize * max(pc.u_FilterRadius, 1e-4);' + NL +
    NL +
    '  /* 9-tap 3x3 Tent Filter Upsample Kernel ([1, 2, 1; 2, 4, 2; 1, 2, 1] / 16) */' + NL +
    '  vec3 a = texture(u_SrcLowMipTexture, uv + vec2(-d.x, -d.y)).rgb;' + NL +
    '  vec3 b = texture(u_SrcLowMipTexture, uv + vec2( 0.0, -d.y)).rgb;' + NL +
    '  vec3 c = texture(u_SrcLowMipTexture, uv + vec2( d.x, -d.y)).rgb;' + NL +
    '  vec3 d0 = texture(u_SrcLowMipTexture, uv + vec2(-d.x,  0.0)).rgb;' + NL +
    '  vec3 e = texture(u_SrcLowMipTexture, uv).rgb;' + NL +
    '  vec3 f = texture(u_SrcLowMipTexture, uv + vec2( d.x,  0.0)).rgb;' + NL +
    '  vec3 g = texture(u_SrcLowMipTexture, uv + vec2(-d.x,  d.y)).rgb;' + NL +
    '  vec3 h = texture(u_SrcLowMipTexture, uv + vec2( 0.0,  d.y)).rgb;' + NL +
    '  vec3 i = texture(u_SrcLowMipTexture, uv + vec2( d.x,  d.y)).rgb;' + NL +
    NL +
    '  vec3 upsample = (e * 4.0 + (b + d0 + f + h) * 2.0 + (a + c + g + i)) * (1.0 / 16.0);' + NL +
    '  vec3 prevMip = imageLoad(u_DstHighMipImage, ivec2(coord)).rgb;' + NL +
    '  imageStore(u_DstHighMipImage, ivec2(coord), vec4(prevMip + upsample, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateLuminanceHistogramComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 16 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 16 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_HDRSceneTexture;' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) buffer CastleLuminanceHistogramBuffer {' + NL +
    '  uint u_HistogramBins[256];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 2) buffer CastleAutoExposureStateBuffer {' + NL +
    '  float u_AdaptedLuminance;' + NL +
    '  float u_AdaptedExposure;' + NL +
    '  float u_TargetLuminance;' + NL +
    '  float u_Reserved;' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleAutoExposurePushConstants {' + NL +
    '  float u_MinLogLum;' + NL +
    '  float u_MaxLogLum;' + NL +
    '  float u_TimeDelta;' + NL +
    '  float u_AdaptationSpeed;' + NL +
    '  float u_TargetMiddleGrey;' + NL +
    '  float u_MinExposure;' + NL +
    '  float u_MaxExposure;' + NL +
    '  uint  u_TotalPixels;' + NL +
    '} pc;' + NL +
    NL +
    'shared uint s_LocalHistogram[256];' + NL +
    NL +
    'uint ColorToHistogramBin(vec3 hdrColor) {' + NL +
    '  float lum = dot(hdrColor, vec3(0.2126, 0.7152, 0.0722));' + NL +
    '  if (lum < 1e-4) return 0u;' + NL +
    '  float logLum = clamp((log2(lum) - pc.u_MinLogLum) / max(pc.u_MaxLogLum - pc.u_MinLogLum, 1e-4), 0.0, 1.0);' + NL +
    '  return uint(logLum * 254.0 + 1.0);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint localIdx = gl_LocalInvocationIndex;' + NL +
    '  if (localIdx < 256u) s_LocalHistogram[localIdx] = 0u;' + NL +
    '  barrier();' + NL +
    NL +
    '  ivec2 texSize = textureSize(u_HDRSceneTexture, 0);' + NL +
    '  ivec2 coord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (coord.x < texSize.x && coord.y < texSize.y) {' + NL +
    '    vec3 hdrColor = texelFetch(u_HDRSceneTexture, coord, 0).rgb;' + NL +
    '    uint bin = ColorToHistogramBin(hdrColor);' + NL +
    '    atomicAdd(s_LocalHistogram[bin], 1u);' + NL +
    '  }' + NL +
    '  barrier();' + NL +
    NL +
    '  if (localIdx < 256u && s_LocalHistogram[localIdx] > 0u) {' + NL +
    '    atomicAdd(u_HistogramBins[localIdx], s_LocalHistogram[localIdx]);' + NL +
    '  }' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateTonemappingAndColorGradingFunctions: String;
begin
  Result :=
    '/* Phase 15.3: HDR Tonemapping (Reinhard, ACES Filmic, AgX) & Color Grading */' + NL +
    'vec3 TonemapReinhard(vec3 hdr) {' + NL +
    '  return hdr / (hdr + vec3(1.0));' + NL +
    '}' + NL +
    NL +
    'vec3 TonemapACESFilmic(vec3 x) {' + NL +
    '  const float a = 2.51;' + NL +
    '  const float b = 0.03;' + NL +
    '  const float c = 2.43;' + NL +
    '  const float d = 0.59;' + NL +
    '  const float e = 0.14;' + NL +
    '  return clamp((x * (a * x + b)) / (x * (c * x + d) + e), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec3 AgXDefaultContrastApprox(vec3 x) {' + NL +
    '  vec3 x2 = x * x;' + NL +
    '  vec3 x4 = x2 * x2;' + NL +
    '  return + 15.5     * x4 * x2' + NL +
    '         - 40.14    * x4 * x' + NL +
    '         + 31.96    * x4' + NL +
    '         - 6.868    * x2 * x' + NL +
    '         + 0.4298   * x2' + NL +
    '         + 0.1191   * x' + NL +
    '         - 0.00232;' + NL +
    '}' + NL +
    NL +
    'vec3 TonemapAgX(vec3 hdr) {' + NL +
    '  const mat3 agxInsetMatrix = mat3(' + NL +
    '    0.842479062253094,  0.0423282422610123, 0.0423756549057051,' + NL +
    '    0.0784335999999992, 0.878468636469772,  0.0784336,' + NL +
    '    0.0792237451477643, 0.0791661274605434, 0.879142973793104);' + NL +
    '  const mat3 agxOutsetMatrix = mat3(' + NL +
    '     1.19687900512017,   -0.0528968517574562, -0.0529716355144438,' + NL +
    '    -0.0980208811401368,  1.15190312990417,   -0.0980434501171241,' + NL +
    '    -0.0990297440797205, -0.0989611768448433,  1.15107367264116);' + NL +
    '  const float minEv = -12.47393;' + NL +
    '  const float maxEv = 4.026069;' + NL +
    '  vec3 val = agxInsetMatrix * max(hdr, vec3(1e-6));' + NL +
    '  val = clamp(log2(val), minEv, maxEv);' + NL +
    '  val = (val - minEv) / (maxEv - minEv);' + NL +
    '  val = AgXDefaultContrastApprox(val);' + NL +
    '  val = agxOutsetMatrix * val;' + NL +
    '  return clamp(pow(max(val, vec3(0.0)), vec3(2.2)), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec3 ApplyColorGrading(vec3 color, float saturation, float contrast) {' + NL +
    '  float luma = dot(color, vec3(0.2126, 0.7152, 0.0722));' + NL +
    '  vec3 satColor = mix(vec3(luma), color, saturation);' + NL +
    '  return max((satColor - vec3(0.18)) * contrast + vec3(0.18), vec3(0.0));' + NL +
    '}' + NL +
    NL +
    'vec3 LinearToSRGB(vec3 linearColor) {' + NL +
    '  vec3 c = max(linearColor, vec3(0.0));' + NL +
    '  vec3 lo = c * 12.92;' + NL +
    '  vec3 hi = 1.055 * pow(c, vec3(1.0 / 2.4)) - 0.055;' + NL +
    '  return mix(lo, hi, step(vec3(0.0031308), c));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateHDRCompositeFragmentShader: String;
begin
  Result :=
    '#version 450' + NL +
    'layout(location = 0) in vec2 v_TexCoord;' + NL +
    'layout(location = 0) out vec4 out_FragColor;' + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_HDRSceneTexture;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_BloomTexture;' + NL +
    'layout(std430, set = 0, binding = 2) readonly buffer CastleAutoExposureStateBuffer {' + NL +
    '  float u_AdaptedLuminance;' + NL +
    '  float u_AdaptedExposure;' + NL +
    '  float u_TargetLuminance;' + NL +
    '  float u_Reserved;' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleHDRCompositePushConstants {' + NL +
    '  float u_Exposure;' + NL +
    '  float u_BloomIntensity;' + NL +
    '  float u_Gamma;' + NL +
    '  float u_Saturation;' + NL +
    '  float u_Contrast;' + NL +
    '  uint  u_TonemapOperator;' + NL +
    '  uint  u_EnableBloom;' + NL +
    '  uint  u_EnableAutoExposure;' + NL +
    '} pc;' + NL +
    NL +
    GenerateTonemappingAndColorGradingFunctions + NL +
    'void main() {' + NL +
    '  vec3 hdrColor = texture(u_HDRSceneTexture, v_TexCoord).rgb;' + NL +
    '  if (pc.u_EnableBloom != 0u) {' + NL +
    '    vec3 bloomColor = texture(u_BloomTexture, v_TexCoord).rgb;' + NL +
    '    hdrColor += bloomColor * pc.u_BloomIntensity;' + NL +
    '  }' + NL +
    NL +
    '  float exposure = pc.u_Exposure;' + NL +
    '  if (pc.u_EnableAutoExposure != 0u) {' + NL +
    '    exposure *= max(u_AdaptedExposure, 1e-4);' + NL +
    '  }' + NL +
    '  vec3 exposed = ApplyColorGrading(hdrColor * exposure, pc.u_Saturation, pc.u_Contrast);' + NL +
    NL +
    '  vec3 mapped;' + NL +
    '  if (pc.u_TonemapOperator == 1u) mapped = TonemapReinhard(exposed);' + NL +
    '  else if (pc.u_TonemapOperator == 2u) mapped = TonemapACESFilmic(exposed);' + NL +
    '  else if (pc.u_TonemapOperator == 3u) mapped = TonemapAgX(exposed);' + NL +
    '  else mapped = clamp(exposed, 0.0, 1.0);' + NL +
    NL +
    '  vec3 srgb = (abs(pc.u_Gamma - 2.2) < 1e-3) ? LinearToSRGB(mapped) : pow(max(mapped, vec3(0.0)), vec3(1.0 / max(pc.u_Gamma, 1e-3)));' + NL +
    '  out_FragColor = vec4(srgb, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateGTAOComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_DepthTexture;' + NL +
    'layout(set = 0, binding = 1, r16f) uniform writeonly image2D u_RawGTAOImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleGTAOPushConstants {' + NL +
    '  vec2  u_InvResolution;' + NL +
    '  vec2  u_ProjScale;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '  float u_EffectRadius;' + NL +
    '  float u_FalloffRange;' + NL +
    '  float u_Power;' + NL +
    '  uint  u_SliceCount;' + NL +
    '  uint  u_StepsPerSlice;' + NL +
    '  uint  u_DenoisePass;' + NL +
    '} pc;' + NL +
    NL +
    'const float PI = 3.14159265359;' + NL +
    'const float HALF_PI = 1.57079632679;' + NL +
    NL +
    'float LinearizeDepth(float ndcDepth) {' + NL +
    '  float zNear = max(pc.u_ZNear, 1e-3);' + NL +
    '  float zFar = max(pc.u_ZFar, zNear + 1.0);' + NL +
    '  return (zNear * zFar) / max(zFar - ndcDepth * (zFar - zNear), 1e-5);' + NL +
    '}' + NL +
    NL +
    'vec3 ReconstructViewPosition(vec2 uv, float linearZ) {' + NL +
    '  vec2 ndc = uv * 2.0 - 1.0;' + NL +
    '  return vec3(ndc * pc.u_ProjScale * linearZ, -linearZ);' + NL +
    '}' + NL +
    NL +
    'vec3 ReconstructViewNormalFromDepth(vec2 uv, vec3 centerPos) {' + NL +
    '  vec2 dx = vec2(pc.u_InvResolution.x, 0.0);' + NL +
    '  vec2 dy = vec2(0.0, pc.u_InvResolution.y);' + NL +
    '  vec3 pRight = ReconstructViewPosition(uv + dx, LinearizeDepth(textureLod(u_DepthTexture, uv + dx, 0.0).r)) - centerPos;' + NL +
    '  vec3 pLeft  = centerPos - ReconstructViewPosition(uv - dx, LinearizeDepth(textureLod(u_DepthTexture, uv - dx, 0.0).r));' + NL +
    '  vec3 pUp    = ReconstructViewPosition(uv + dy, LinearizeDepth(textureLod(u_DepthTexture, uv + dy, 0.0).r)) - centerPos;' + NL +
    '  vec3 pDown  = centerPos - ReconstructViewPosition(uv - dy, LinearizeDepth(textureLod(u_DepthTexture, uv - dy, 0.0).r));' + NL +
    '  vec3 dX = (abs(pRight.z) < abs(pLeft.z)) ? pRight : pLeft;' + NL +
    '  vec3 dY = (abs(pUp.z) < abs(pDown.z)) ? pUp : pDown;' + NL +
    '  return normalize(cross(dX, dY));' + NL +
    '}' + NL +
    NL +
    'float InterleavedGradientNoise(vec2 pixelCoord) {' + NL +
    '  return fract(52.9829189 * fract(dot(pixelCoord, vec2(0.06711056, 0.00583715))));' + NL +
    '}' + NL +
    NL +
    'float IntegrateGTAOHorizonSlice(float h1, float h2, float nAngle) {' + NL +
    '  float sinN = sin(nAngle);' + NL +
    '  float cosN = cos(nAngle);' + NL +
    '  float i1 = (-cos(2.0 * h1 - nAngle) + cosN + 2.0 * h1 * sinN) * 0.25;' + NL +
    '  float i2 = (-cos(2.0 * h2 - nAngle) + cosN + 2.0 * h2 * sinN) * 0.25;' + NL +
    '  return clamp(i1 + i2, 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 dstSize = imageSize(u_RawGTAOImage);' + NL +
    '  ivec2 coord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (coord.x >= dstSize.x || coord.y >= dstSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(coord) + 0.5) * pc.u_InvResolution;' + NL +
    '  float rawDepth = textureLod(u_DepthTexture, uv, 0.0).r;' + NL +
    '  if (rawDepth >= 0.9999) {' + NL +
    '    imageStore(u_RawGTAOImage, coord, vec4(1.0, 0.0, 0.0, 1.0));' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  float linearZ = LinearizeDepth(rawDepth);' + NL +
    '  vec3 viewPos = ReconstructViewPosition(uv, linearZ);' + NL +
    '  vec3 viewNormal = ReconstructViewNormalFromDepth(uv, viewPos);' + NL +
    '  vec3 viewDir = normalize(-viewPos);' + NL +
    NL +
    '  uint sliceCount = max(pc.u_SliceCount, 1u);' + NL +
    '  uint stepsPerSlice = max(pc.u_StepsPerSlice, 1u);' + NL +
    '  float noiseAngle = InterleavedGradientNoise(vec2(coord)) * PI;' + NL +
    '  float visibilitySum = 0.0;' + NL +
    NL +
    '  for (uint s = 0u; s < sliceCount; ++s) {' + NL +
    '    float phi = (float(s) / float(sliceCount)) * PI + noiseAngle;' + NL +
    '    vec2 omega = vec2(cos(phi), sin(phi));' + NL +
    '    vec2 maxCos = vec2(-1.0);' + NL +
    '    for (uint stepIdx = 1u; stepIdx <= stepsPerSlice; ++stepIdx) {' + NL +
    '      float stepFrac = float(stepIdx) / float(stepsPerSlice);' + NL +
    '      vec2 uvOffset = omega * stepFrac * (pc.u_EffectRadius / max(linearZ, 0.5)) * pc.u_InvResolution * 16.0;' + NL +
    '      vec3 sPos1 = ReconstructViewPosition(uv + uvOffset, LinearizeDepth(textureLod(u_DepthTexture, uv + uvOffset, 0.0).r)) - viewPos;' + NL +
    '      vec3 sPos2 = ReconstructViewPosition(uv - uvOffset, LinearizeDepth(textureLod(u_DepthTexture, uv - uvOffset, 0.0).r)) - viewPos;' + NL +
    '      float dist1 = length(sPos1);' + NL +
    '      float dist2 = length(sPos2);' + NL +
    '      float falloff1 = clamp(1.0 - dist1 / max(pc.u_EffectRadius * (1.0 + pc.u_FalloffRange), 1e-4), 0.0, 1.0);' + NL +
    '      float falloff2 = clamp(1.0 - dist2 / max(pc.u_EffectRadius * (1.0 + pc.u_FalloffRange), 1e-4), 0.0, 1.0);' + NL +
    '      maxCos.x = max(maxCos.x, mix(-1.0, dot(sPos1 / max(dist1, 1e-5), viewDir), falloff1));' + NL +
    '      maxCos.y = max(maxCos.y, mix(-1.0, dot(sPos2 / max(dist2, 1e-5), viewDir), falloff2));' + NL +
    '    }' + NL +
    '    float h1 = -acos(clamp(maxCos.x, -1.0, 1.0));' + NL +
    '    float h2 =  acos(clamp(maxCos.y, -1.0, 1.0));' + NL +
    '    float nAngle = acos(clamp(dot(viewNormal, viewDir), -1.0, 1.0)) - HALF_PI;' + NL +
    '    visibilitySum += IntegrateGTAOHorizonSlice(h1, h2, nAngle);' + NL +
    '  }' + NL +
    NL +
    '  float ao = pow(clamp(visibilitySum / float(sliceCount), 0.0, 1.0), max(pc.u_Power, 0.01));' + NL +
    '  imageStore(u_RawGTAOImage, coord, vec4(ao, 0.0, 0.0, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateGTAODenoiseComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_DepthTexture;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_RawGTAOTexture;' + NL +
    'layout(set = 0, binding = 2, r16f) uniform writeonly image2D u_DenoisedGTAOImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleGTAOPushConstants {' + NL +
    '  vec2  u_InvResolution;' + NL +
    '  vec2  u_ProjScale;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '  float u_EffectRadius;' + NL +
    '  float u_FalloffRange;' + NL +
    '  float u_Power;' + NL +
    '  uint  u_SliceCount;' + NL +
    '  uint  u_StepsPerSlice;' + NL +
    '  uint  u_DenoisePass;' + NL +
    '} pc;' + NL +
    NL +
    'float ComputeBilateralDepthWeight(float centerDepth, float sampleDepth, float sigmaDepth) {' + NL +
    '  float diff = abs(centerDepth - sampleDepth) / max(sigmaDepth, 1e-5);' + NL +
    '  return exp(-diff * diff);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 dstSize = imageSize(u_DenoisedGTAOImage);' + NL +
    '  ivec2 coord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (coord.x >= dstSize.x || coord.y >= dstSize.y) return;' + NL +
    NL +
    '  vec2 centerUV = (vec2(coord) + 0.5) * pc.u_InvResolution;' + NL +
    '  float centerDepth = textureLod(u_DepthTexture, centerUV, 0.0).r;' + NL +
    '  float aoSum = 0.0;' + NL +
    '  float weightSum = 0.0;' + NL +
    NL +
    '  for (int dy = -2; dy <= 2; ++dy) {' + NL +
    '    for (int dx = -2; dx <= 2; ++dx) {' + NL +
    '      vec2 sampleUV = centerUV + vec2(float(dx), float(dy)) * pc.u_InvResolution;' + NL +
    '      float sampleDepth = textureLod(u_DepthTexture, sampleUV, 0.0).r;' + NL +
    '      float sampleAO = textureLod(u_RawGTAOTexture, sampleUV, 0.0).r;' + NL +
    '      float spatialWeight = exp(-float(dx * dx + dy * dy) / 4.5);' + NL +
    '      float depthWeight = ComputeBilateralDepthWeight(centerDepth, sampleDepth, 0.015);' + NL +
    '      float w = spatialWeight * depthWeight;' + NL +
    '      aoSum += sampleAO * w;' + NL +
    '      weightSum += w;' + NL +
    '    }' + NL +
    '  }' + NL +
    NL +
    '  float denoisedAO = aoSum / max(weightSum, 1e-5);' + NL +
    '  imageStore(u_DenoisedGTAOImage, coord, vec4(denoisedAO, 0.0, 0.0, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateHiZSSRComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_HDRSceneColor;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_HiZPyramidTexture;' + NL +
    'layout(set = 0, binding = 2, rgba16f) uniform writeonly image2D u_SSRReflectionImage;' + NL +
    NL +
    'layout(push_constant) uniform CastleSSRPushConstants {' + NL +
    '  vec2  u_InvResolution;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '  float u_MaxDistance;' + NL +
    '  float u_Thickness;' + NL +
    '  float u_MaxRoughness;' + NL +
    '  float u_EdgeFadeWidth;' + NL +
    '  uint  u_MaxSteps;' + NL +
    '  uint  u_HiZMipCount;' + NL +
    '  float u_Intensity;' + NL +
    '  uint  u_Reserved;' + NL +
    '} pc;' + NL +
    NL +
    'float EvaluateScreenEdgeFade(vec2 hitUV, float fadeWidth) {' + NL +
    '  vec2 border = smoothstep(vec2(0.0), vec2(max(fadeWidth, 1e-4)), hitUV) *' + NL +
    '                smoothstep(vec2(0.0), vec2(max(fadeWidth, 1e-4)), vec2(1.0) - hitUV);' + NL +
    '  return clamp(border.x * border.y, 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'float EvaluateSSRReflectionWeight(float roughness, float maxRoughness, float cosTheta) {' + NL +
    '  if (roughness >= maxRoughness) return 0.0;' + NL +
    '  float roughWeight = 1.0 - smoothstep(0.0, max(maxRoughness, 1e-4), roughness);' + NL +
    '  float fresnel = mix(0.04, max(1.0 - roughness, 0.04), pow(clamp(1.0 - cosTheta, 0.0, 1.0), 5.0));' + NL +
    '  return clamp(roughWeight * fresnel, 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 dstSize = imageSize(u_SSRReflectionImage);' + NL +
    '  ivec2 coord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (coord.x >= dstSize.x || coord.y >= dstSize.y) return;' + NL +
    NL +
    '  vec2 startUV = (vec2(coord) + 0.5) * pc.u_InvResolution;' + NL +
    '  float startDepth = textureLod(u_HiZPyramidTexture, startUV, 0.0).r;' + NL +
    '  if (startDepth >= 0.9999) {' + NL +
    '    imageStore(u_SSRReflectionImage, coord, vec4(0.0));' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  vec2 rayDirUV = normalize(startUV - vec2(0.5, 0.5) + vec2(1e-4)) * pc.u_InvResolution * 2.0;' + NL +
    '  float rayStepZ = -0.002;' + NL +
    '  vec2 curUV = startUV;' + NL +
    '  float curZ = startDepth;' + NL +
    '  int mipLevel = 0;' + NL +
    '  int maxMip = int(max(pc.u_HiZMipCount, 1u)) - 1;' + NL +
    '  bool hit = false;' + NL +
    NL +
    '  for (uint stepIdx = 0u; stepIdx < pc.u_MaxSteps; ++stepIdx) {' + NL +
    '    float stepScale = float(1 << mipLevel);' + NL +
    '    curUV += rayDirUV * stepScale;' + NL +
    '    curZ  += rayStepZ * stepScale;' + NL +
    '    if (curUV.x < 0.0 || curUV.x > 1.0 || curUV.y < 0.0 || curUV.y > 1.0) break;' + NL +
    '    float hiZMinDepth = textureLod(u_HiZPyramidTexture, curUV, float(mipLevel)).r;' + NL +
    '    if (curZ >= hiZMinDepth) {' + NL +
    '      if (mipLevel == 0) {' + NL +
    '        if (curZ - hiZMinDepth <= pc.u_Thickness) { hit = true; break; }' + NL +
    '      } else {' + NL +
    '        mipLevel = max(0, mipLevel - 1);' + NL +
    '      }' + NL +
    '    } else {' + NL +
    '      mipLevel = min(maxMip, mipLevel + 1);' + NL +
    '    }' + NL +
    '  }' + NL +
    NL +
    '  if (!hit) {' + NL +
    '    imageStore(u_SSRReflectionImage, coord, vec4(0.0));' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  float edgeFade = EvaluateScreenEdgeFade(curUV, pc.u_EdgeFadeWidth);' + NL +
    '  float reflWeight = EvaluateSSRReflectionWeight(0.15, pc.u_MaxRoughness, 0.5) * edgeFade * pc.u_Intensity;' + NL +
    '  vec3 hitColor = textureLod(u_HDRSceneColor, curUV, 0.0).rgb;' + NL +
    '  imageStore(u_SSRReflectionImage, coord, vec4(hitColor * reflWeight, reflWeight));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateVolumetricFogInjectComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY, AWorkgroupSizeZ: Cardinal): String;
var
  LocalX, LocalY, LocalZ: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 4 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 4 else LocalY := AWorkgroupSizeY;
  if AWorkgroupSizeZ = 0 then LocalZ := 4 else LocalZ := AWorkgroupSizeZ;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = %d) in;', [LocalX, LocalY, LocalZ]) + NL +
    NL +
    'layout(set = 0, binding = 0, rgba16f) uniform writeonly image3D u_FroxelScatteringVolume;' + NL +
    NL +
    'layout(push_constant) uniform CastleVolumetricFogParams {' + NL +
    '  vec4  u_FogAlbedoAndBaseDensity;' + NL +
    '  vec4  u_SunDirectionAndAnisotropy;' + NL +
    '  vec4  u_SunColorAndIntensity;' + NL +
    '  float u_HeightFalloff;' + NL +
    '  float u_BaseHeight;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '} pc;' + NL +
    NL +
    'const float FOUR_PI = 12.56637061436;' + NL +
    NL +
    'float EvaluateHenyeyGreensteinPhase(float cosTheta, float g) {' + NL +
    '  float g2 = g * g;' + NL +
    '  float denom = max(1.0 + g2 - 2.0 * g * cosTheta, 1e-4);' + NL +
    '  return (1.0 - g2) / (FOUR_PI * denom * sqrt(denom));' + NL +
    '}' + NL +
    NL +
    'float EvaluateHeightFogDensity(float worldY, float baseDensity, float baseHeight, float heightFalloff) {' + NL +
    '  float dh = max(worldY - baseHeight, -50.0);' + NL +
    '  return max(0.0, baseDensity * exp(-heightFalloff * dh));' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec3 gridSize = imageSize(u_FroxelScatteringVolume);' + NL +
    '  ivec3 froxelCoord = ivec3(gl_GlobalInvocationID.xyz);' + NL +
    '  if (froxelCoord.x >= gridSize.x || froxelCoord.y >= gridSize.y || froxelCoord.z >= gridSize.z) return;' + NL +
    NL +
    '  vec3 uvw = (vec3(froxelCoord) + 0.5) / vec3(gridSize);' + NL +
    '  float zNear = max(pc.u_ZNear, 0.05);' + NL +
    '  float zFar  = max(pc.u_ZFar, zNear + 1.0);' + NL +
    '  float sliceDepth = zNear * pow(zFar / zNear, uvw.z);' + NL +
    '  float worldY = (uvw.y - 0.5) * sliceDepth;' + NL +
    NL +
    '  float extinction = EvaluateHeightFogDensity(worldY, pc.u_FogAlbedoAndBaseDensity.w, pc.u_BaseHeight, pc.u_HeightFalloff);' + NL +
    '  vec3 viewRayDir = normalize(vec3((uvw.xy * 2.0 - 1.0), -1.0));' + NL +
    '  float cosTheta = dot(viewRayDir, normalize(pc.u_SunDirectionAndAnisotropy.xyz));' + NL +
    '  float phase = EvaluateHenyeyGreensteinPhase(cosTheta, clamp(pc.u_SunDirectionAndAnisotropy.w, -0.99, 0.99));' + NL +
    NL +
    '  vec3 scatteringAlbedo = pc.u_FogAlbedoAndBaseDensity.rgb;' + NL +
    '  vec3 sunLight = pc.u_SunColorAndIntensity.rgb * pc.u_SunColorAndIntensity.w;' + NL +
    '  vec3 inScattered = scatteringAlbedo * extinction * (vec3(0.15) + sunLight * phase);' + NL +
    '  imageStore(u_FroxelScatteringVolume, froxelCoord, vec4(inScattered, extinction));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateVolumetricFogRaymarchComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler3D u_FroxelScatteringVolume;' + NL +
    'layout(set = 0, binding = 1, rgba16f) uniform writeonly image3D u_IntegratedFroxelLUT;' + NL +
    NL +
    'layout(push_constant) uniform CastleVolumetricFogParams {' + NL +
    '  vec4  u_FogAlbedoAndBaseDensity;' + NL +
    '  vec4  u_SunDirectionAndAnisotropy;' + NL +
    '  vec4  u_SunColorAndIntensity;' + NL +
    '  float u_HeightFalloff;' + NL +
    '  float u_BaseHeight;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '} pc;' + NL +
    NL +
    'void IntegrateFroxelSliceStep(vec3 sliceScattering, float sliceExtinction, float stepLength,' + NL +
    '  inout vec3 accumLight, inout float accumTransmittance) {' + NL +
    '  float stepTransmittance = exp(-max(sliceExtinction, 0.0) * max(stepLength, 0.0));' + NL +
    '  vec3 sliceIntegral = sliceScattering * (1.0 - stepTransmittance) / max(sliceExtinction, 1e-5);' + NL +
    '  accumLight += accumTransmittance * sliceIntegral;' + NL +
    '  accumTransmittance *= stepTransmittance;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec3 gridSize = imageSize(u_IntegratedFroxelLUT);' + NL +
    '  ivec2 xy = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (xy.x >= gridSize.x || xy.y >= gridSize.y) return;' + NL +
    NL +
    '  float zNear = max(pc.u_ZNear, 0.05);' + NL +
    '  float zFar  = max(pc.u_ZFar, zNear + 1.0);' + NL +
    '  vec3 accumLight = vec3(0.0);' + NL +
    '  float accumTransmittance = 1.0;' + NL +
    '  float prevDepth = zNear;' + NL +
    NL +
    '  for (int z = 0; z < gridSize.z; ++z) {' + NL +
    '    float nextDepth = zNear * pow(zFar / zNear, float(z + 1) / float(gridSize.z));' + NL +
    '    float stepLength = max(nextDepth - prevDepth, 1e-3);' + NL +
    '    prevDepth = nextDepth;' + NL +
    '    vec4 sampleVal = texelFetch(u_FroxelScatteringVolume, ivec3(xy, z), 0);' + NL +
    '    IntegrateFroxelSliceStep(sampleVal.rgb, sampleVal.a, stepLength, accumLight, accumTransmittance);' + NL +
    '    imageStore(u_IntegratedFroxelLUT, ivec3(xy, z), vec4(accumLight, accumTransmittance));' + NL +
    '  }' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateTAAResolveComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_CurrentHDRTexture;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_HistoryHDRTexture;' + NL +
    'layout(set = 0, binding = 2) uniform sampler2D u_VelocityTexture;' + NL +
    'layout(set = 0, binding = 3, rgba16f) uniform writeonly image2D u_ResolvedHDRTexture;' + NL +
    NL +
    'layout(push_constant) uniform CastleTAAPushConstants {' + NL +
    '  vec2  u_InvResolution;' + NL +
    '  vec2  u_JitterOffset;' + NL +
    '  float u_BlendFactor;' + NL +
    '  float u_VarianceClipGamma;' + NL +
    '  float u_VelocityWeightScale;' + NL +
    '  uint  u_FrameIndex;' + NL +
    '} pc;' + NL +
    NL +
    'vec3 RGBToYCoCg(vec3 rgb) {' + NL +
    '  float Y  = dot(rgb, vec3(0.25, 0.50, 0.25));' + NL +
    '  float Co = dot(rgb, vec3(0.50, 0.00, -0.50));' + NL +
    '  float Cg = dot(rgb, vec3(-0.25, 0.50, -0.25));' + NL +
    '  return vec3(Y, Co, Cg);' + NL +
    '}' + NL +
    NL +
    'vec3 YCoCgToRGB(vec3 ycocg) {' + NL +
    '  float tmp = ycocg.x - ycocg.z;' + NL +
    '  float r   = tmp + ycocg.y;' + NL +
    '  float g   = ycocg.x + ycocg.z;' + NL +
    '  float b   = tmp - ycocg.y;' + NL +
    '  return max(vec3(r, g, b), vec3(0.0));' + NL +
    '}' + NL +
    NL +
    'vec3 ClipHistoryYCoCg(vec3 historyYCoCg, vec3 meanYCoCg, vec3 stdDevYCoCg, float gamma) {' + NL +
    '  vec3 boxMin = meanYCoCg - stdDevYCoCg * max(gamma, 0.1);' + NL +
    '  vec3 boxMax = meanYCoCg + stdDevYCoCg * max(gamma, 0.1);' + NL +
    '  return clamp(historyYCoCg, boxMin, boxMax);' + NL +
    '}' + NL +
    NL +
    'vec3 SampleHistoryCatmullRom5Tap(sampler2D tex, vec2 uv, vec2 invRes) {' + NL +
    '  vec2 samplePos = uv / max(invRes, vec2(1e-6));' + NL +
    '  vec2 texPos1 = floor(samplePos - 0.5) + 0.5;' + NL +
    '  vec2 f = samplePos - texPos1;' + NL +
    '  vec2 w0 = f * (-0.5 + f * (1.0 - 0.5 * f));' + NL +
    '  vec2 w1 = 1.0 + f * f * (-2.5 + 1.5 * f);' + NL +
    '  vec2 w2 = f * (0.5 + f * (2.0 - 1.5 * f));' + NL +
    '  vec2 w3 = f * f * (-0.5 + 0.5 * f);' + NL +
    '  vec2 w12 = w1 + w2;' + NL +
    '  vec2 offset12 = w2 / max(w12, vec2(1e-5));' + NL +
    '  vec2 texPos0 = (texPos1 - 1.0) * invRes;' + NL +
    '  vec2 texPos3 = (texPos1 + 2.0) * invRes;' + NL +
    '  vec2 texPos12 = (texPos1 + offset12) * invRes;' + NL +
    '  vec3 result = vec3(0.0);' + NL +
    '  result += textureLod(tex, vec2(texPos12.x, texPos0.y), 0.0).rgb * (w12.x * w0.y);' + NL +
    '  result += textureLod(tex, vec2(texPos0.x,  texPos12.y), 0.0).rgb * (w0.x  * w12.y);' + NL +
    '  result += textureLod(tex, vec2(texPos12.x, texPos12.y), 0.0).rgb * (w12.x * w12.y);' + NL +
    '  result += textureLod(tex, vec2(texPos3.x,  texPos12.y), 0.0).rgb * (w3.x  * w12.y);' + NL +
    '  result += textureLod(tex, vec2(texPos12.x, texPos3.y), 0.0).rgb * (w12.x * w3.y);' + NL +
    '  float totalWeight = w12.x * w0.y + w0.x * w12.y + w12.x * w12.y + w3.x * w12.y + w12.x * w3.y;' + NL +
    '  return max(result / max(totalWeight, 1e-5), vec3(0.0));' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 pixelCoord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  ivec2 outSize = imageSize(u_ResolvedHDRTexture);' + NL +
    '  if (pixelCoord.x >= outSize.x || pixelCoord.y >= outSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(pixelCoord) + 0.5) * pc.u_InvResolution;' + NL +
    '  vec2 velocity = textureLod(u_VelocityTexture, uv, 0.0).rg;' + NL +
    '  vec2 historyUV = uv - velocity;' + NL +
    NL +
    '  vec3 m1 = vec3(0.0);' + NL +
    '  vec3 m2 = vec3(0.0);' + NL +
    '  vec3 centerYCoCg = vec3(0.0);' + NL +
    '  for (int dy = -1; dy <= 1; ++dy) {' + NL +
    '    for (int dx = -1; dx <= 1; ++dx) {' + NL +
    '      vec2 sampleUV = clamp(uv + vec2(float(dx), float(dy)) * pc.u_InvResolution, vec2(0.0), vec2(1.0));' + NL +
    '      vec3 sampleYCoCg = RGBToYCoCg(textureLod(u_CurrentHDRTexture, sampleUV, 0.0).rgb);' + NL +
    '      if (dx == 0 && dy == 0) centerYCoCg = sampleYCoCg;' + NL +
    '      m1 += sampleYCoCg;' + NL +
    '      m2 += sampleYCoCg * sampleYCoCg;' + NL +
    '    }' + NL +
    '  }' + NL +
    '  vec3 meanYCoCg = m1 / 9.0;' + NL +
    '  vec3 stdDevYCoCg = sqrt(max(m2 / 9.0 - meanYCoCg * meanYCoCg, vec3(0.0)));' + NL +
    NL +
    '  if (any(lessThan(historyUV, vec2(0.0))) || any(greaterThan(historyUV, vec2(1.0)))) {' + NL +
    '    imageStore(u_ResolvedHDRTexture, pixelCoord, vec4(YCoCgToRGB(centerYCoCg), 1.0));' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  vec3 historyRGB = SampleHistoryCatmullRom5Tap(u_HistoryHDRTexture, historyUV, pc.u_InvResolution);' + NL +
    '  vec3 clippedHistoryYCoCg = ClipHistoryYCoCg(RGBToYCoCg(historyRGB), meanYCoCg, stdDevYCoCg, pc.u_VarianceClipGamma);' + NL +
    '  float velMag = length(velocity / max(pc.u_InvResolution, vec2(1e-6)));' + NL +
    '  float blendWeight = clamp(pc.u_BlendFactor + velMag * pc.u_VelocityWeightScale * 0.001, 0.04, 0.50);' + NL +
    '  vec3 resolvedRGB = YCoCgToRGB(mix(clippedHistoryYCoCg, centerYCoCg, blendWeight));' + NL +
    '  imageStore(u_ResolvedHDRTexture, pixelCoord, vec4(resolvedRGB, 1.0));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateBoxProjectedReflectionProbeFunctions: String;
begin
  Result :=
    '// --- Localized Reflection Probes with Box-Projected Parallax Correction (Phase 17.2) ---' + NL +
    'struct CastleReflectionProbeEntry {' + NL +
    '  vec4 PositionAndRadius;' + NL +
    '  vec4 BoxMinAndIntensity;' + NL +
    '  vec4 BoxMaxAndCubemapIndex;' + NL +
    '  vec4 BlendDistanceAndFlags;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 8) readonly buffer CastleReflectionProbesBuffer {' + NL +
    '  uvec4 u_ProbeHeader;' + NL +
    '  CastleReflectionProbeEntry u_Probes[];' + NL +
    '} CastleReflectionProbes;' + NL +
    NL +
    'vec3 EvaluateBoxProjectedDirection(vec3 worldPos, vec3 worldReflectDir, vec3 probePos, vec3 boxMin, vec3 boxMax) {' + NL +
    '  vec3 safeDir = sign(worldReflectDir) * max(abs(worldReflectDir), vec3(1e-5));' + NL +
    '  vec3 tMax = (boxMax - worldPos) / safeDir;' + NL +
    '  vec3 tMin = (boxMin - worldPos) / safeDir;' + NL +
    '  vec3 tLargest = max(tMax, tMin);' + NL +
    '  float hitDist = min(min(tLargest.x, tLargest.y), tLargest.z);' + NL +
    '  if (hitDist <= 0.0) return normalize(worldReflectDir);' + NL +
    '  vec3 hitPoint = worldPos + worldReflectDir * hitDist;' + NL +
    '  return normalize(hitPoint - probePos);' + NL +
    '}' + NL +
    NL +
    'float EvaluateProbeInfluenceWeight(vec3 worldPos, vec3 boxMin, vec3 boxMax, float blendDistance) {' + NL +
    '  if (any(lessThan(worldPos, boxMin)) || any(greaterThan(worldPos, boxMax))) return 0.0;' + NL +
    '  vec3 distToMin = worldPos - boxMin;' + NL +
    '  vec3 distToMax = boxMax - worldPos;' + NL +
    '  vec3 minEdgeDist = min(distToMin, distToMax);' + NL +
    '  float closestWall = min(min(minEdgeDist.x, minEdgeDist.y), minEdgeDist.z);' + NL +
    '  return clamp(closestWall / max(blendDistance, 1e-4), 0.0, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateClusteredDecalProjectionFunctions: String;
begin
  Result :=
    '// --- Oriented Bounding Box (OBB) Decal Projection Functions (Phase 17.3) ---' + NL +
    'struct CastleProjectedDecalEntry {' + NL +
    '  vec4 WorldToDecalRow0;' + NL +
    '  vec4 WorldToDecalRow1;' + NL +
    '  vec4 WorldToDecalRow2;' + NL +
    '  vec4 AlbedoTintAndOpacity;' + NL +
    '  vec4 DecalNormalAndRoughness;' + NL +
    '  vec4 UVScaleOffset;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 9) readonly buffer CastleProjectedDecalsBuffer {' + NL +
    '  uvec4 u_DecalHeader;' + NL +
    '  CastleProjectedDecalEntry u_Decals[];' + NL +
    '} CastleProjectedDecals;' + NL +
    NL +
    'bool ProjectWorldPointToDecalUV(vec3 worldPos, CastleProjectedDecalEntry decal, out vec2 decalUV, out vec3 localPos) {' + NL +
    '  vec4 wp = vec4(worldPos, 1.0);' + NL +
    '  localPos = vec3(dot(decal.WorldToDecalRow0, wp), dot(decal.WorldToDecalRow1, wp), dot(decal.WorldToDecalRow2, wp));' + NL +
    '  if (any(greaterThan(abs(localPos), vec3(0.5)))) {' + NL +
    '    decalUV = vec2(0.0);' + NL +
    '    return false;' + NL +
    '  }' + NL +
    '  decalUV = (localPos.xy + 0.5) * decal.UVScaleOffset.xy + decal.UVScaleOffset.zw;' + NL +
    '  return true;' + NL +
    '}' + NL +
    NL +
    'float EvaluateDecalAngleAndBoxFade(vec3 localPos, vec3 surfaceNormal, vec3 decalForward, float angleFadeCos) {' + NL +
    '  float cosAngle = dot(normalize(surfaceNormal), normalize(decalForward));' + NL +
    '  float normalWeight = smoothstep(angleFadeCos, min(angleFadeCos + 0.25, 0.99), cosAngle);' + NL +
    '  float zFade = 1.0 - smoothstep(0.35, 0.50, abs(localPos.z));' + NL +
    '  return clamp(normalWeight * zFade, 0.0, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateParticleSimulationComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
var
  LocalX: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 64 else LocalX := AWorkgroupSizeX;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [LocalX]) + NL +
    NL +
    'struct CastleGPUParticle {' + NL +
    '  vec4 PositionAndAge;' + NL +
    '  vec4 VelocityAndMaxLife;' + NL +
    '  vec4 ColorAndSize;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 0) buffer CastleParticlePoolBuffer {' + NL +
    '  CastleGPUParticle u_Particles[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) writeonly buffer CastleAliveParticleIndexBuffer {' + NL +
    '  uint u_AliveParticleIndices[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 2) buffer CastleParticleIndirectDrawBuffer {' + NL +
    '  uint vertexCount;' + NL +
    '  uint instanceCount;' + NL +
    '  uint firstVertex;' + NL +
    '  uint firstInstance;' + NL +
    '} u_DrawCmd;' + NL +
    NL +
    'layout(push_constant) uniform CastleParticleSimPushConstants {' + NL +
    '  vec4  u_GravityAndDeltaTime;' + NL +
    '  vec4  u_EmitterOriginAndDrag;' + NL +
    '  float u_GroundPlaneY;' + NL +
    '  float u_Restitution;' + NL +
    '  uint  u_MaxParticles;' + NL +
    '  uint  u_SpawnSeed;' + NL +
    '} pc;' + NL +
    NL +
    'bool SimulateParticleStep(inout CastleGPUParticle p, vec3 gravity, float drag, float groundY, float restitution, float dt) {' + NL +
    '  float age = p.PositionAndAge.w + dt;' + NL +
    '  float maxLife = p.VelocityAndMaxLife.w;' + NL +
    '  if (maxLife <= 0.0 || age >= maxLife) {' + NL +
    '    p.PositionAndAge.w = age;' + NL +
    '    return false;' + NL +
    '  }' + NL +
    '  vec3 vel = (p.VelocityAndMaxLife.xyz + gravity * dt) * max(1.0 - drag * dt, 0.0);' + NL +
    '  vec3 pos = p.PositionAndAge.xyz + vel * dt;' + NL +
    '  if (pos.y < groundY && vel.y < 0.0) {' + NL +
    '    pos.y = groundY;' + NL +
    '    vel.y = -vel.y * clamp(restitution, 0.0, 1.0);' + NL +
    '  }' + NL +
    '  p.PositionAndAge = vec4(pos, age);' + NL +
    '  p.VelocityAndMaxLife.xyz = vel;' + NL +
    '  return true;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  if (idx == 0u) {' + NL +
    '    u_DrawCmd.vertexCount = 6u;' + NL +
    '    u_DrawCmd.firstVertex = 0u;' + NL +
    '    u_DrawCmd.firstInstance = 0u;' + NL +
    '  }' + NL +
    '  if (idx >= pc.u_MaxParticles) return;' + NL +
    '  CastleGPUParticle p = u_Particles[idx];' + NL +
    '  if (SimulateParticleStep(p, pc.u_GravityAndDeltaTime.xyz, pc.u_EmitterOriginAndDrag.w,' + NL +
    '      pc.u_GroundPlaneY, pc.u_Restitution, pc.u_GravityAndDeltaTime.w)) {' + NL +
    '    uint aliveSlot = atomicAdd(u_DrawCmd.instanceCount, 1u);' + NL +
    '    u_AliveParticleIndices[aliveSlot] = idx;' + NL +
    '  }' + NL +
    '  u_Particles[idx] = p;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateWeightedBlendedOITFunctions: String;
begin
  Result :=
    '// --- McGuire & Bavoil Weighted Blended Order-Independent Transparency (Phase 18.2) ---' + NL +
    'float EvaluateOITDepthWeight(float linearZ, float alpha, float weightScale) {' + NL +
    '  float normZ = clamp(linearZ / max(weightScale, 1.0), 0.0, 0.99);' + NL +
    '  float oneMinusZ = 1.0 - normZ;' + NL +
    '  float depthFactor = max(1e-2, 3000.0 * oneMinusZ * oneMinusZ * oneMinusZ);' + NL +
    '  return clamp(max(alpha, 1e-3) * depthFactor, 1e-2, 3000.0);' + NL +
    '}' + NL +
    NL +
    'void AccumulateWeightedBlendedOIT(vec4 premulColorAndAlpha, float linearZ, float weightScale,' + NL +
    '  out vec4 outAccum, out float outRevealage) {' + NL +
    '  float a = clamp(premulColorAndAlpha.a, 0.0, 1.0);' + NL +
    '  float w = EvaluateOITDepthWeight(linearZ, a, weightScale);' + NL +
    '  outAccum = vec4(premulColorAndAlpha.rgb * w, a * w);' + NL +
    '  outRevealage = a;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateOITComposite(vec4 accumRGBA, float revealage, vec3 opaqueRGB) {' + NL +
    '  float transmittance = clamp(revealage, 0.0, 1.0);' + NL +
    '  if (transmittance >= 0.9999) return opaqueRGB;' + NL +
    '  vec3 avgTransparentColor = accumRGBA.rgb / max(accumRGBA.a, 1e-5);' + NL +
    '  return avgTransparentColor * (1.0 - transmittance) + opaqueRGB * transmittance;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateWeightedBlendedOITCompositeFragmentShader: String;
begin
  Result :=
    '#version 450' + NL +
    'layout(location = 0) in vec2 v_TexCoord;' + NL +
    'layout(location = 0) out vec4 out_FragColor;' + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_OpaqueHDRTexture;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_OITAccumTexture;' + NL +
    'layout(set = 0, binding = 2) uniform sampler2D u_OITRevealageTexture;' + NL +
    NL +
    GenerateWeightedBlendedOITFunctions + NL +
    'void main() {' + NL +
    '  vec3 opaqueRGB = texture(u_OpaqueHDRTexture, v_TexCoord).rgb;' + NL +
    '  vec4 accumRGBA = texture(u_OITAccumTexture, v_TexCoord);' + NL +
    '  float revealage = texture(u_OITRevealageTexture, v_TexCoord).r;' + NL +
    '  vec3 composited = EvaluateOITComposite(accumRGBA, revealage, opaqueRGB);' + NL +
    '  out_FragColor = vec4(composited, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateContactShadowsComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
var
  LocalX, LocalY: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 8 else LocalX := AWorkgroupSizeX;
  if AWorkgroupSizeY = 0 then LocalY := 8 else LocalY := AWorkgroupSizeY;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [LocalX, LocalY]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_DepthTexture;' + NL +
    'layout(set = 0, binding = 1, r16f) uniform writeonly image2D u_ContactShadowMask;' + NL +
    NL +
    'layout(push_constant) uniform CastleContactShadowsPushConstants {' + NL +
    '  vec2  u_InvResolution;' + NL +
    '  vec3  u_LightDirView;' + NL +
    '  float u_RayLength;' + NL +
    '  float u_Thickness;' + NL +
    '  float u_ZNear;' + NL +
    '  float u_ZFar;' + NL +
    '  uint  u_StepCount;' + NL +
    '  float u_Contrast;' + NL +
    '  uint  u_Reserved;' + NL +
    '} pc;' + NL +
    NL +
    'float LinearizeContactDepth(float d, float zNear, float zFar) {' + NL +
    '  return (zNear * zFar) / max(zFar - d * (zFar - zNear), 1e-5);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 pixelCoord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  ivec2 outSize = imageSize(u_ContactShadowMask);' + NL +
    '  if (pixelCoord.x >= outSize.x || pixelCoord.y >= outSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(pixelCoord) + 0.5) * pc.u_InvResolution;' + NL +
    '  float centerRawDepth = textureLod(u_DepthTexture, uv, 0.0).r;' + NL +
    '  if (centerRawDepth >= 0.9999) {' + NL +
    '    imageStore(u_ContactShadowMask, pixelCoord, vec4(1.0));' + NL +
    '    return;' + NL +
    '  }' + NL +
    NL +
    '  float startLinearZ = LinearizeContactDepth(centerRawDepth, pc.u_ZNear, pc.u_ZFar);' + NL +
    '  uint steps = max(pc.u_StepCount, 4u);' + NL +
    '  vec2 uvStep = (pc.u_LightDirView.xy * pc.u_RayLength * 0.25) / float(steps);' + NL +
    '  float zStep = (-pc.u_LightDirView.z * pc.u_RayLength) / float(steps);' + NL +
    '  float shadowOcclusion = 0.0;' + NL +
    NL +
    '  for (uint i = 1u; i <= steps; ++i) {' + NL +
    '    float t = float(i) / float(steps);' + NL +
    '    vec2 sampleUV = uv + uvStep * float(i);' + NL +
    '    if (any(lessThan(sampleUV, vec2(0.0))) || any(greaterThan(sampleUV, vec2(1.0)))) break;' + NL +
    '    float rayLinearZ = startLinearZ + zStep * float(i);' + NL +
    '    float sceneLinearZ = LinearizeContactDepth(textureLod(u_DepthTexture, sampleUV, 0.0).r, pc.u_ZNear, pc.u_ZFar);' + NL +
    '    float depthDelta = rayLinearZ - sceneLinearZ;' + NL +
    '    if (depthDelta > 0.001 && depthDelta < pc.u_Thickness) {' + NL +
    '      shadowOcclusion = max(shadowOcclusion, (1.0 - t * t) * clamp(pc.u_Contrast, 0.0, 1.0));' + NL +
    '      break;' + NL +
    '    }' + NL +
    '  }' + NL +
    '  imageStore(u_ContactShadowMask, pixelCoord, vec4(clamp(1.0 - shadowOcclusion, 0.0, 1.0)));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateComputeSkinningShader(
  const AWorkgroupSizeX: Cardinal): String;
var
  LocalX: Cardinal;
begin
  if AWorkgroupSizeX = 0 then LocalX := 64 else LocalX := AWorkgroupSizeX;

  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [LocalX]) + NL +
    NL +
    'struct CastleSkinningVertex {' + NL +
    '  vec4  PositionAndUVX;' + NL +
    '  vec4  NormalAndUVY;' + NL +
    '  uvec4 JointIndices;' + NL +
    '  vec4  JointWeights;' + NL +
    '};' + NL +
    NL +
    'struct CastleMorphTargetDelta {' + NL +
    '  vec4 DeltaPosition;' + NL +
    '  vec4 DeltaNormal;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 0) readonly buffer CastleSourceVertexBuffer {' + NL +
    '  CastleSkinningVertex u_SourceVertices[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) readonly buffer CastleMorphDeltaBuffer {' + NL +
    '  CastleMorphTargetDelta u_MorphDeltas[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 2) readonly buffer CastleJointMatrixBuffer {' + NL +
    '  mat4 u_JointMatrices[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 3) buffer CastleSkinnedVertexOutputBuffer {' + NL +
    '  CastleSkinningVertex u_SkinnedVertices[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 4) writeonly buffer CastlePreviousPositionBuffer {' + NL +
    '  vec4 u_PreviousPositions[];' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleComputeSkinningPushConstants {' + NL +
    '  vec4 u_MorphWeights;' + NL +
    '  uint u_VertexCount;' + NL +
    '  uint u_MorphTargetCount;' + NL +
    '  uint u_JointCount;' + NL +
    '  uint u_StorePreviousPos;' + NL +
    '} pc;' + NL +
    NL +
    'void main() {' + NL +
    '  uint vid = gl_GlobalInvocationID.x;' + NL +
    '  if (vid >= pc.u_VertexCount) return;' + NL +
    NL +
    '  if (pc.u_StorePreviousPos != 0u) {' + NL +
    '    u_PreviousPositions[vid] = vec4(u_SkinnedVertices[vid].PositionAndUVX.xyz, 1.0);' + NL +
    '  }' + NL +
    NL +
    '  CastleSkinningVertex src = u_SourceVertices[vid];' + NL +
    '  vec3 morphedPos = src.PositionAndUVX.xyz;' + NL +
    '  vec3 morphedNorm = src.NormalAndUVY.xyz;' + NL +
    NL +
    '  uint morphCount = min(pc.u_MorphTargetCount, 4u);' + NL +
    '  for (uint m = 0u; m < morphCount; ++m) {' + NL +
    '    float w = pc.u_MorphWeights[m];' + NL +
    '    if (abs(w) > 1e-5) {' + NL +
    '      CastleMorphTargetDelta delta = u_MorphDeltas[m * pc.u_VertexCount + vid];' + NL +
    '      morphedPos += delta.DeltaPosition.xyz * w;' + NL +
    '      morphedNorm += delta.DeltaNormal.xyz * w;' + NL +
    '    }' + NL +
    '  }' + NL +
    NL +
    '  vec3 skinnedPos = morphedPos;' + NL +
    '  vec3 skinnedNorm = morphedNorm;' + NL +
    '  float weightSum = src.JointWeights.x + src.JointWeights.y + src.JointWeights.z + src.JointWeights.w;' + NL +
    '  if (pc.u_JointCount > 0u && weightSum > 1e-5) {' + NL +
    '    mat4 skinMat =' + NL +
    '      u_JointMatrices[min(src.JointIndices.x, pc.u_JointCount - 1u)] * src.JointWeights.x +' + NL +
    '      u_JointMatrices[min(src.JointIndices.y, pc.u_JointCount - 1u)] * src.JointWeights.y +' + NL +
    '      u_JointMatrices[min(src.JointIndices.z, pc.u_JointCount - 1u)] * src.JointWeights.z +' + NL +
    '      u_JointMatrices[min(src.JointIndices.w, pc.u_JointCount - 1u)] * src.JointWeights.w;' + NL +
    '    skinnedPos = (skinMat * vec4(morphedPos, 1.0)).xyz;' + NL +
    '    skinnedNorm = mat3(skinMat) * morphedNorm;' + NL +
    '  }' + NL +
    '  float normLen = length(skinnedNorm);' + NL +
    '  if (normLen > 1e-5) skinnedNorm /= normLen;' + NL +
    NL +
    '  CastleSkinningVertex dst = src;' + NL +
    '  dst.PositionAndUVX.xyz = skinnedPos;' + NL +
    '  dst.NormalAndUVY.xyz = skinnedNorm;' + NL +
    '  u_SkinnedVertices[vid] = dst;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateTerrainCDLODFunctions: String;
begin
  Result :=
    '// --- GPU-Driven Continuous Distance-Dependent LOD (CDLOD) Terrain (Phase 19.2) ---' + NL +
    'struct CastleTerrainCDLODNode {' + NL +
    '  vec4 NodeXZAndScale;' + NL +
    '  vec4 MorphStartEndAndHeightRange;' + NL +
    '};' + NL +
    NL +
    'float EvaluateCDLODMorphFactor(float cameraDist, float lodNearDist, float lodFarDist, float morphStartRatio) {' + NL +
    '  float morphStart = mix(lodNearDist, lodFarDist, clamp(morphStartRatio, 0.1, 0.95));' + NL +
    '  return clamp((cameraDist - morphStart) / max(lodFarDist - morphStart, 1e-4), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec2 MorphCDLODGridVertex(vec2 gridUV, float patchResolution, float morphFactor) {' + NL +
    '  vec2 fracPart = fract(gridUV * patchResolution * 0.5) * 2.0 / max(patchResolution, 1.0);' + NL +
    '  return gridUV - fracPart * clamp(morphFactor, 0.0, 1.0);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateSubsurfaceAndTransmissionPBRFunctions: String;
begin
  Result :=
    '// --- Pre-Integrated Wrap-Diffuse Subsurface Scattering & Foliage Transmission (Phase 19.3) ---' + NL +
    'vec3 EvaluateSubsurfaceWrapDiffuse(float NdotL, float wrapFactor, vec3 scatterColor) {' + NL +
    '  float w = clamp(wrapFactor, 0.0, 1.0);' + NL +
    '  float wrapped = max(0.0, (NdotL + w) / (1.0 + w));' + NL +
    '  float normFactor = 1.0 / (1.0 + w);' + NL +
    '  float sssBand = smoothstep(0.0, max(w, 1e-4), wrapped) * (1.0 - max(0.0, NdotL));' + NL +
    '  return vec3(wrapped * normFactor) + scatterColor * sssBand;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateFoliageTransmission(vec3 viewDir, vec3 lightDir, vec3 surfaceNormal,' + NL +
    '  vec3 transmissionTint, float thickness, float distortion, float powerExp, float scale) {' + NL +
    '  vec3 transLightDir = normalize(lightDir + surfaceNormal * distortion);' + NL +
    '  float VdotBackL = clamp(dot(viewDir, -transLightDir), 0.0, 1.0);' + NL +
    '  float attenuation = exp(-max(0.0, thickness) * 2.0);' + NL +
    '  float phase = pow(VdotBackL, max(1.0, powerExp)) * max(0.0, scale) * attenuation;' + NL +
    '  return transmissionTint * phase;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSubsurfaceWrapDiffuse(
  const ANdotL, AWrapFactor: Single;
  const AScatterColor: TVector3): TVector3;
var
  W, Wrapped, NormFactor, T, SSSBand, BaseTerm: Single;
begin
  W := EnsureRange(AWrapFactor, 0.0, 1.0);
  Wrapped := Max(0.0, (ANdotL + W) / (1.0 + W));
  NormFactor := 1.0 / (1.0 + W);
  T := EnsureRange(Wrapped / Max(W, 1e-4), 0.0, 1.0);
  SSSBand := (T * T * (3.0 - 2.0 * T)) * (1.0 - Max(0.0, ANdotL));
  BaseTerm := Wrapped * NormFactor;
  Result := Vector3(BaseTerm, BaseTerm, BaseTerm) + AScatterColor * SSSBand;
end;

class function TCastleShaderGeneratorRHI.EvaluateFoliageTransmission(
  const AViewDir, ALightDir, ASurfaceNormal, ATransmissionTint: TVector3;
  const AThickness, ADistortion, APower, AScale: Single): TVector3;
var
  TransLightDir: TVector3;
  VdotBackL, Attenuation, Phase: Single;
begin
  TransLightDir := (ALightDir + ASurfaceNormal * ADistortion).Normalize;
  VdotBackL := EnsureRange(TVector3.DotProduct(AViewDir.Normalize, -TransLightDir), 0.0, 1.0);
  Attenuation := Exp(-Max(0.0, AThickness) * 2.0);
  Phase := Power(VdotBackL, Max(1.0, APower)) * Max(0.0, AScale) * Attenuation;
  Result := ATransmissionTint * Phase;
end;

function TCastleShaderGeneratorRHI.GenerateClearCoatAndAnisotropicPBRFunctions: String;
begin
  Result :=
    '// --- Multi-Layer Clear Coat & Anisotropic GGX Brushed-Metal PBR (Phase 20.1) ---' + NL +
    'void EvaluateClearCoatBRDF(float cNdotH, float cNdotL, float cNdotV, float VdotH,' + NL +
    '  float clearCoatFactor, float clearCoatRoughness,' + NL +
    '  out float outClearCoatSpecular, out float outBaseLayerAttenuation) {' + NL +
    '  float ccFactor = clamp(clearCoatFactor, 0.0, 1.0);' + NL +
    '  float r = clamp(clearCoatRoughness, 0.04, 1.0);' + NL +
    '  float alpha = max(0.002, r * r);' + NL +
    '  float alpha2 = alpha * alpha;' + NL +
    '  float nh = clamp(cNdotH, 0.0, 1.0);' + NL +
    '  float denom = nh * nh * (alpha2 - 1.0) + 1.0;' + NL +
    '  float D_cc = alpha2 / max(3.14159265359 * denom * denom, 1e-7);' + NL +
    '  float vh = clamp(VdotH, 0.0, 1.0);' + NL +
    '  float V_kelemen = 0.25 / max(vh * vh, 1e-4);' + NL +
    '  float f0 = 0.04;' + NL +
    '  float F_cc = (f0 + (1.0 - f0) * pow(1.0 - vh, 5.0)) * ccFactor;' + NL +
    '  outClearCoatSpecular = D_cc * V_kelemen * F_cc * clamp(cNdotL, 0.0, 1.0);' + NL +
    '  outBaseLayerAttenuation = 1.0 - F_cc;' + NL +
    '}' + NL +
    NL +
    'float EvaluateAnisotropicGGX_D(float NdotH, float TdotH, float BdotH, float roughness, float anisotropy) {' + NL +
    '  float r = clamp(roughness, 0.04, 1.0);' + NL +
    '  float alpha = max(0.002, r * r);' + NL +
    '  float aniso = clamp(anisotropy, -0.99, 0.99);' + NL +
    '  float alphaT = max(0.002, alpha * (1.0 + aniso));' + NL +
    '  float alphaB = max(0.002, alpha * (1.0 - aniso));' + NL +
    '  float d = (TdotH * TdotH) / (alphaT * alphaT) + (BdotH * BdotH) / (alphaB * alphaB) + (NdotH * NdotH);' + NL +
    '  return 1.0 / max(3.14159265359 * alphaT * alphaB * d * d, 1e-8);' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateClearCoatBRDF(
  const ACNdotH, ACNdotL, ACNdotV, AVdotH, AClearCoatFactor, AClearCoatRoughness: Single;
  out AClearCoatSpecular, ABaseLayerAttenuation: Single);
var
  CCFactor, R, Alpha, Alpha2, NH, Denom, D_cc, VH, V_Kelemen, F_cc: Single;
begin
  CCFactor := EnsureRange(AClearCoatFactor, 0.0, 1.0);
  R := EnsureRange(AClearCoatRoughness, 0.04, 1.0);
  Alpha := Max(0.002, R * R);
  Alpha2 := Alpha * Alpha;
  NH := EnsureRange(ACNdotH, 0.0, 1.0);
  Denom := NH * NH * (Alpha2 - 1.0) + 1.0;
  D_cc := Alpha2 / Max(Single(Pi) * Denom * Denom, 1e-7);
  VH := EnsureRange(AVdotH, 0.0, 1.0);
  V_Kelemen := 0.25 / Max(VH * VH, 1e-4);
  F_cc := (0.04 + 0.96 * Power(1.0 - VH, 5.0)) * CCFactor;
  AClearCoatSpecular := D_cc * V_Kelemen * F_cc * EnsureRange(ACNdotL, 0.0, 1.0);
  ABaseLayerAttenuation := 1.0 - F_cc;
end;

class function TCastleShaderGeneratorRHI.EvaluateAnisotropicGGX_D(
  const ANdotH, ATdotH, ABdotH, ARoughness, AAnisotropy: Single): Single;
var
  R, Alpha, Aniso, AlphaT, AlphaB, D: Single;
begin
  R := EnsureRange(ARoughness, 0.04, 1.0);
  Alpha := Max(0.002, R * R);
  Aniso := EnsureRange(AAnisotropy, -0.99, 0.99);
  AlphaT := Max(0.002, Alpha * (1.0 + Aniso));
  AlphaB := Max(0.002, Alpha * (1.0 - Aniso));
  D := (ATdotH * ATdotH) / (AlphaT * AlphaT) +
       (ABdotH * ABdotH) / (AlphaB * AlphaB) +
       (ANdotH * ANdotH);
  Result := 1.0 / Max(Single(Pi) * AlphaT * AlphaB * D * D, 1e-8);
end;

function TCastleShaderGeneratorRHI.GenerateAtmosphereSkyComputeShader(
  const AWorkgroupSizeX: Cardinal; const AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    GenerateVersionAndLayoutMacros +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [
      Max(Cardinal(1), AWorkgroupSizeX),
      Max(Cardinal(1), AWorkgroupSizeY)
    ]) + NL +
    NL +
    '// Set 0, Binding 0: Output Sky-View LUT (rgba16f)' + NL +
    'layout(rgba16f, set = 0, binding = 0) uniform writeonly image2D u_SkyViewLUT;' + NL +
    NL +
    'CASTLE_PUSH_CONSTANT uniform CastleAtmospherePushConstants {' + NL +
    '  vec4 u_SunDirectionAndIntensity;' + NL +
    '  vec4 u_RayleighScatteringAndHeight;' + NL +
    '  vec4 u_MieScatteringAndHeight;' + NL +
    '  float u_PlanetRadius;' + NL +
    '  float u_AtmosphereRadius;' + NL +
    '  float u_MieAnisotropyG;' + NL +
    '  uint u_RaymarchSteps;' + NL +
    '} castle_AtmospherePC;' + NL +
    NL +
    'float EvaluateRayleighPhase(float cosTheta) {' + NL +
    '  return (3.0 / (16.0 * 3.14159265359)) * (1.0 + cosTheta * cosTheta);' + NL +
    '}' + NL +
    NL +
    'float EvaluateCornetteShanksMiePhase(float cosTheta, float g) {' + NL +
    '  float g2 = g * g;' + NL +
    '  float num = 3.0 * (1.0 - g2) * (1.0 + cosTheta * cosTheta);' + NL +
    '  float denom = (8.0 * 3.14159265359) * (2.0 + g2) * pow(max(1.0 + g2 - 2.0 * g * cosTheta, 1e-4), 1.5);' + NL +
    '  return num / max(denom, 1e-7);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 lutSize = imageSize(u_SkyViewLUT);' + NL +
    '  ivec2 pixelCoord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  if (pixelCoord.x >= lutSize.x || pixelCoord.y >= lutSize.y) return;' + NL +
    NL +
    '  vec2 uv = (vec2(pixelCoord) + 0.5) / vec2(lutSize);' + NL +
    '  float azimuth = uv.x * 2.0 * 3.14159265359;' + NL +
    '  float elevation = (uv.y - 0.5) * 3.14159265359;' + NL +
    '  vec3 viewDir = normalize(vec3(cos(elevation) * cos(azimuth), sin(elevation), cos(elevation) * sin(azimuth)));' + NL +
    '  vec3 sunDir = normalize(castle_AtmospherePC.u_SunDirectionAndIntensity.xyz);' + NL +
    '  float cosTheta = dot(viewDir, sunDir);' + NL +
    NL +
    '  float phaseR = EvaluateRayleighPhase(cosTheta);' + NL +
    '  float phaseM = EvaluateCornetteShanksMiePhase(cosTheta, castle_AtmospherePC.u_MieAnisotropyG);' + NL +
    '  float atmThickness = max(1000.0, castle_AtmospherePC.u_AtmosphereRadius - castle_AtmospherePC.u_PlanetRadius);' + NL +
    '  uint steps = max(1u, castle_AtmospherePC.u_RaymarchSteps);' + NL +
    '  float stepLen = atmThickness / float(steps);' + NL +
    NL +
    '  vec3 opticalDepthR = vec3(0.0);' + NL +
    '  vec3 opticalDepthM = vec3(0.0);' + NL +
    '  vec3 inScattered = vec3(0.0);' + NL +
    '  for (uint i = 0u; i < steps; ++i) {' + NL +
    '    float h = (float(i) + 0.5) * stepLen * max(0.05, viewDir.y * 0.5 + 0.5);' + NL +
    '    float dR = exp(-h / max(1.0, castle_AtmospherePC.u_RayleighScatteringAndHeight.w)) * stepLen;' + NL +
    '    float dM = exp(-h / max(1.0, castle_AtmospherePC.u_MieScatteringAndHeight.w)) * stepLen;' + NL +
    '    opticalDepthR += castle_AtmospherePC.u_RayleighScatteringAndHeight.rgb * dR;' + NL +
    '    opticalDepthM += castle_AtmospherePC.u_MieScatteringAndHeight.rgb * dM;' + NL +
    '    vec3 transmittance = exp(-(opticalDepthR + opticalDepthM * 1.11));' + NL +
    '    inScattered += transmittance * (castle_AtmospherePC.u_RayleighScatteringAndHeight.rgb * dR * phaseR +' + NL +
    '      castle_AtmospherePC.u_MieScatteringAndHeight.rgb * dM * phaseM);' + NL +
    '  }' + NL +
    '  vec3 skyRadiance = inScattered * castle_AtmospherePC.u_SunDirectionAndIntensity.w;' + NL +
    '  imageStore(u_SkyViewLUT, pixelCoord, vec4(skyRadiance, 1.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateRayleighPhase(const ACosTheta: Single): Single;
begin
  Result := (3.0 / (16.0 * Single(Pi))) * (1.0 + ACosTheta * ACosTheta);
end;

class function TCastleShaderGeneratorRHI.EvaluateCornetteShanksMiePhase(
  const ACosTheta, AG: Single): Single;
var
  G, G2, Num, Denom: Single;
begin
  G := EnsureRange(AG, -0.99, 0.99);
  G2 := G * G;
  Num := 3.0 * (1.0 - G2) * (1.0 + ACosTheta * ACosTheta);
  Denom := (8.0 * Single(Pi)) * (2.0 + G2) * Power(Max(1.0 + G2 - 2.0 * G * ACosTheta, 1e-4), 1.5);
  Result := Num / Max(Denom, 1e-7);
end;

function TCastleShaderGeneratorRHI.GenerateLensFlareAndChromaticAberrationFunctions: String;
begin
  Result :=
    '// --- Screen-Space Multi-Ghost Lens Flare & Radial Chromatic Aberration (Phase 20.3) ---' + NL +
    'void EvaluateChromaticAberrationOffsets(vec2 uv, float intensity, out vec2 outRedUV, out vec2 outGreenUV, out vec2 outBlueUV) {' + NL +
    '  vec2 d = uv - vec2(0.5);' + NL +
    '  float r2 = dot(d, d);' + NL +
    '  vec2 offset = d * (intensity * (1.0 + r2));' + NL +
    '  outRedUV = uv - offset;' + NL +
    '  outGreenUV = uv;' + NL +
    '  outBlueUV = uv + offset;' + NL +
    '}' + NL +
    NL +
    'void EvaluateLensFlareGhostUV(vec2 uv, int ghostIndex, float ghostSpacing, out vec2 outGhostUV, out float outRadialWeight) {' + NL +
    '  vec2 flareAxis = vec2(0.5) - uv;' + NL +
    '  outGhostUV = uv + flareAxis * (float(max(1, ghostIndex)) * ghostSpacing);' + NL +
    '  float distFromCenter = length(outGhostUV - vec2(0.5)) / 0.70710678;' + NL +
    '  float w = clamp(1.0 - distFromCenter, 0.0, 1.0);' + NL +
    '  outRadialWeight = w * w * w * w;' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateChromaticAberrationOffsets(
  const AUV: TVector2; const AIntensity: Single;
  out ARedUV, AGreenUV, ABlueUV: TVector2);
var
  D, Offset: TVector2;
  R2: Single;
begin
  D := AUV - Vector2(0.5, 0.5);
  R2 := D.X * D.X + D.Y * D.Y;
  Offset := D * (AIntensity * (1.0 + R2));
  ARedUV := AUV - Offset;
  AGreenUV := AUV;
  ABlueUV := AUV + Offset;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateLensFlareGhostUV(
  const AUV: TVector2; const AGhostIndex: Integer; const AGhostSpacing: Single;
  out AGhostUV: TVector2; out ARadialWeight: Single);
var
  FlareAxis, Centered: TVector2;
  DistNorm, W: Single;
begin
  FlareAxis := Vector2(0.5, 0.5) - AUV;
  AGhostUV := AUV + FlareAxis * (Single(Max(1, AGhostIndex)) * AGhostSpacing);
  Centered := AGhostUV - Vector2(0.5, 0.5);
  DistNorm := Sqrt(Centered.X * Centered.X + Centered.Y * Centered.Y) / 0.70710678;
  W := EnsureRange(1.0 - DistNorm, 0.0, 1.0);
  ARadialWeight := W * W * W * W;
end;

function TCastleShaderGeneratorRHI.GenerateClothSheenAndIridescencePBRFunctions: String;
begin
  Result :=
    '// --- Velvet/Cloth Estevez-Kulla Charlie Sheen & Thin-Film Iridescence PBR (Phase 21.1) ---' + NL +
    'float EvaluateCharlieSheen_D(float NdotH, float sheenRoughness) {' + NL +
    '  float r = clamp(sheenRoughness, 0.04, 1.0);' + NL +
    '  float alpha = max(0.002, r * r);' + NL +
    '  float invAlpha = 1.0 / alpha;' + NL +
    '  float cos2h = clamp(NdotH * NdotH, 0.0, 1.0);' + NL +
    '  float sin2h = max(1e-5, 1.0 - cos2h);' + NL +
    '  return (2.0 + invAlpha) * pow(sin2h, invAlpha * 0.5) / (2.0 * 3.14159265359);' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateThinFilmIridescence(float cosTheta1, float iridescenceIOR, float thicknessNanometers) {' + NL +
    '  float nFilm = max(1.0, iridescenceIOR);' + NL +
    '  float sin2Theta1 = max(0.0, 1.0 - clamp(cosTheta1, 0.0, 1.0) * clamp(cosTheta1, 0.0, 1.0));' + NL +
    '  float sin2Theta2 = sin2Theta1 / (nFilm * nFilm);' + NL +
    '  float cosTheta2 = sqrt(max(0.0, 1.0 - sin2Theta2));' + NL +
    '  float opd = 2.0 * nFilm * max(0.0, thicknessNanometers) * cosTheta2;' + NL +
    '  vec3 wavelengths = vec3(650.0, 550.0, 450.0);' + NL +
    '  vec3 phase = (2.0 * 3.14159265359 * opd) / wavelengths + 3.14159265359;' + NL +
    '  return clamp(vec3(0.5) + 0.5 * cos(phase), 0.0, 1.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateCharlieSheen_D(
  const ANdotH, ASheenRoughness: Single): Single;
var
  R, Alpha, InvAlpha, Cos2H, Sin2H: Single;
begin
  R := EnsureRange(ASheenRoughness, 0.04, 1.0);
  Alpha := Max(0.002, R * R);
  InvAlpha := 1.0 / Alpha;
  Cos2H := EnsureRange(ANdotH * ANdotH, 0.0, 1.0);
  Sin2H := Max(1e-5, 1.0 - Cos2H);
  Result := (2.0 + InvAlpha) * Power(Sin2H, InvAlpha * 0.5) / (2.0 * Single(Pi));
end;

class function TCastleShaderGeneratorRHI.EvaluateThinFilmIridescence(
  const ACosTheta1, AIridescenceIOR, AThicknessNanometers: Single): TVector3;
var
  NFilm, Cos1, Sin2Theta1, Sin2Theta2, CosTheta2, OPD, TwoPiOPD: Single;
begin
  NFilm := Max(1.0, AIridescenceIOR);
  Cos1 := EnsureRange(ACosTheta1, 0.0, 1.0);
  Sin2Theta1 := Max(0.0, 1.0 - Cos1 * Cos1);
  Sin2Theta2 := Sin2Theta1 / (NFilm * NFilm);
  CosTheta2 := Sqrt(Max(0.0, 1.0 - Sin2Theta2));
  OPD := 2.0 * NFilm * Max(0.0, AThicknessNanometers) * CosTheta2;
  TwoPiOPD := 2.0 * Single(Pi) * OPD;
  Result := Vector3(
    EnsureRange(0.5 + 0.5 * Cos(TwoPiOPD / 650.0 + Single(Pi)), 0.0, 1.0),
    EnsureRange(0.5 + 0.5 * Cos(TwoPiOPD / 550.0 + Single(Pi)), 0.0, 1.0),
    EnsureRange(0.5 + 0.5 * Cos(TwoPiOPD / 450.0 + Single(Pi)), 0.0, 1.0));
end;

function TCastleShaderGeneratorRHI.GenerateOceanDisplacementComputeShader(
  const AWorkgroupSizeX: Cardinal; const AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    GenerateVersionAndLayoutMacros +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;', [
      Max(Cardinal(1), AWorkgroupSizeX),
      Max(Cardinal(1), AWorkgroupSizeY)
    ]) + NL +
    NL +
    '// Set 0, Binding 0: Ocean 3D Displacement (xyz) & Whitecap Crest Foam (w)' + NL +
    'layout(rgba16f, set = 0, binding = 0) uniform writeonly image2D u_OceanDisplacementAndFoam;' + NL +
    '// Set 0, Binding 1: Ocean Analytical Surface Normal (xyz) & Slope (w)' + NL +
    'layout(rgba16f, set = 0, binding = 1) uniform writeonly image2D u_OceanNormalAndSlope;' + NL +
    NL +
    'CASTLE_PUSH_CONSTANT uniform CastleOceanWavePushConstants {' + NL +
    '  vec4 u_WindDirectionAndSpeed;' + NL +
    '  vec4 u_WaveParams;' + NL +
    '  uint u_GridSize;' + NL +
    '  uint u_OctaveCount;' + NL +
    '  float u_FoamThreshold;' + NL +
    '  float u_Gravity;' + NL +
    '} castle_OceanPC;' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 coord = ivec2(gl_GlobalInvocationID.xy);' + NL +
    '  int gridN = int(max(1u, castle_OceanPC.u_GridSize));' + NL +
    '  if (coord.x >= gridN || coord.y >= gridN) return;' + NL +
    NL +
    '  vec2 uv = (vec2(coord) + 0.5) / float(gridN);' + NL +
    '  vec2 worldXZ = (uv - 0.5) * castle_OceanPC.u_WaveParams.w;' + NL +
    '  vec2 baseDir = normalize(castle_OceanPC.u_WindDirectionAndSpeed.xy + vec2(1e-5, 0.0));' + NL +
    '  float timeSec = castle_OceanPC.u_WindDirectionAndSpeed.w;' + NL +
    '  float amp = castle_OceanPC.u_WaveParams.x;' + NL +
    '  float wavelength = max(0.5, castle_OceanPC.u_WaveParams.y);' + NL +
    '  float choppiness = clamp(castle_OceanPC.u_WaveParams.z, 0.0, 1.0);' + NL +
    '  uint octaves = clamp(castle_OceanPC.u_OctaveCount, 1u, 8u);' + NL +
    NL +
    '  vec3 disp = vec3(0.0);' + NL +
    '  vec3 tangent = vec3(1.0, 0.0, 0.0);' + NL +
    '  vec3 bitangent = vec3(0.0, 0.0, 1.0);' + NL +
    '  float jacobian = 1.0;' + NL +
    '  for (uint i = 0u; i < octaves; ++i) {' + NL +
    '    float angle = float(i) * 0.42;' + NL +
    '    vec2 dir = normalize(vec2(baseDir.x * cos(angle) - baseDir.y * sin(angle),' + NL +
    '                              baseDir.x * sin(angle) + baseDir.y * cos(angle)));' + NL +
    '    float k = 6.28318530718 / wavelength;' + NL +
    '    float omega = sqrt(max(0.1, castle_OceanPC.u_Gravity) * k);' + NL +
    '    float phase = k * dot(dir, worldXZ) - omega * timeSec;' + NL +
    '    float c = cos(phase);' + NL +
    '    float s = sin(phase);' + NL +
    '    float q = choppiness / max(k * amp * float(octaves), 1e-4);' + NL +
    '    disp.x += q * amp * dir.x * c;' + NL +
    '    disp.y += amp * s;' + NL +
    '    disp.z += q * amp * dir.y * c;' + NL +
    '    tangent.y += k * amp * dir.x * c;' + NL +
    '    bitangent.y += k * amp * dir.y * c;' + NL +
    '    jacobian -= q * k * amp * s;' + NL +
    '    amp *= 0.5;' + NL +
    '    wavelength *= 0.5;' + NL +
    '  }' + NL +
    '  vec3 nrm = normalize(cross(bitangent, tangent));' + NL +
    '  float foam = clamp((castle_OceanPC.u_FoamThreshold - jacobian) / max(castle_OceanPC.u_FoamThreshold, 1e-4), 0.0, 1.0);' + NL +
    '  imageStore(u_OceanDisplacementAndFoam, coord, vec4(disp, foam));' + NL +
    '  imageStore(u_OceanNormalAndSlope, coord, vec4(nrm, 1.0 - nrm.y));' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateMotionBlurAndBokehDoFFunctions: String;
begin
  Result :=
    '// --- Per-Pixel Velocity Motion Blur & Thin-Lens Bokeh Depth of Field (Phase 21.3) ---' + NL +
    'float EvaluateThinLensCoC(float linearDepth, float focusDistance, float focalLength, float apertureFStop, float maxCoCRadius) {' + NL +
    '  float z = max(1e-3, linearDepth);' + NL +
    '  float zFocus = max(1e-3, focusDistance);' + NL +
    '  float f = clamp(focalLength, 1e-3, zFocus * 0.95);' + NL +
    '  float aperture = f / max(0.1, apertureFStop);' + NL +
    '  float cocRaw = aperture * (f * (z - zFocus)) / max(1e-4, z * (zFocus - f));' + NL +
    '  return clamp(cocRaw * maxCoCRadius * 1000.0, -maxCoCRadius, maxCoCRadius);' + NL +
    '}' + NL +
    NL +
    'float EvaluateMotionBlurSampleWeight(float sampleOffsetPixels, float sampleVelLenPixels,' + NL +
    '  float centerDepth, float sampleDepth, float depthSoftness) {' + NL +
    '  float velCoverage = clamp(1.0 - abs(sampleOffsetPixels) / max(0.5, sampleVelLenPixels), 0.0, 1.0);' + NL +
    '  float depthWeight = clamp(1.0 - max(0.0, sampleDepth - centerDepth) * max(0.0, depthSoftness), 0.0, 1.0);' + NL +
    '  return velCoverage * depthWeight;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateThinLensCoC(
  const ALinearDepth, AFocusDistance, AFocalLength, AApertureFStop, AMaxCoCRadius: Single): Single;
var
  Z, ZFocus, F, Aperture, CoCRaw: Single;
begin
  Z := Max(1e-3, ALinearDepth);
  ZFocus := Max(1e-3, AFocusDistance);
  F := EnsureRange(AFocalLength, 1e-3, ZFocus * 0.95);
  Aperture := F / Max(0.1, AApertureFStop);
  CoCRaw := Aperture * (F * (Z - ZFocus)) / Max(1e-4, Z * (ZFocus - F));
  Result := EnsureRange(CoCRaw * AMaxCoCRadius * 1000.0, -AMaxCoCRadius, AMaxCoCRadius);
end;

class function TCastleShaderGeneratorRHI.EvaluateMotionBlurSampleWeight(
  const ASampleOffsetPixels, ASampleVelocityLengthPixels, ACenterDepth, ASampleDepth, ADepthSoftness: Single): Single;
var
  VelCoverage, DepthWeight: Single;
begin
  VelCoverage := EnsureRange(1.0 - Abs(ASampleOffsetPixels) / Max(0.5, ASampleVelocityLengthPixels), 0.0, 1.0);
  DepthWeight := EnsureRange(1.0 - Max(0.0, ASampleDepth - ACenterDepth) * Max(0.0, ADepthSoftness), 0.0, 1.0);
  Result := VelCoverage * DepthWeight;
end;

function TCastleShaderGeneratorRHI.GenerateDDGIProbeSamplingFunctions: String;
begin
  Result :=
    '// --- Dynamic Diffuse Global Illumination (DDGI) Octahedral Probe Sampling (Phase 22.1) ---' + NL +
    'vec2 OctahedralSignNotZero(vec2 v) {' + NL +
    '  return vec2(v.x >= 0.0 ? 1.0 : -1.0, v.y >= 0.0 ? 1.0 : -1.0);' + NL +
    '}' + NL +
    NL +
    'vec2 OctahedralEncodeDir(vec3 dir) {' + NL +
    '  float l1Norm = max(1e-6, abs(dir.x) + abs(dir.y) + abs(dir.z));' + NL +
    '  vec2 p = dir.xy / l1Norm;' + NL +
    '  if (dir.z < 0.0) {' + NL +
    '    p = (1.0 - abs(p.yx)) * OctahedralSignNotZero(p.xy);' + NL +
    '  }' + NL +
    '  return clamp(p, -1.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec3 OctahedralDecodeUV(vec2 octUV) {' + NL +
    '  vec2 p = clamp(octUV, -1.0, 1.0);' + NL +
    '  vec3 v = vec3(p.xy, 1.0 - abs(p.x) - abs(p.y));' + NL +
    '  if (v.z < 0.0) {' + NL +
    '    v.xy = (1.0 - abs(v.yx)) * OctahedralSignNotZero(v.xy);' + NL +
    '  }' + NL +
    '  return normalize(v);' + NL +
    '}' + NL +
    NL +
    'float EvaluateDDGIProbeWeight(vec3 worldPos, vec3 surfaceNormal, vec3 probePos, float probeMeanDist, float probeVariance) {' + NL +
    '  vec3 toProbe = probePos - worldPos;' + NL +
    '  float distToProbe = length(toProbe);' + NL +
    '  vec3 dirToProbe = toProbe / max(1e-5, distToProbe);' + NL +
    '  float wrapCos = max(0.0001, ( dot(surfaceNormal, dirToProbe) + 1.0 ) * 0.5);' + NL +
    '  float weight = wrapCos * wrapCos + 0.02;' + NL +
    '  if (distToProbe > probeMeanDist) {' + NL +
    '    float v = max(1e-4, probeVariance);' + NL +
    '    float diff = distToProbe - probeMeanDist;' + NL +
    '    float chebyshev = v / (v + diff * diff);' + NL +
    '    weight *= max(0.0, chebyshev * chebyshev * chebyshev);' + NL +
    '  }' + NL +
    '  return max(1e-5, weight);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.OctahedralEncodeDir(const ADir: TVector3): TVector2;
var
  D: TVector3;
  L1, PX, PY, SignX, SignY: Single;
begin
  D := ADir.Normalize;
  L1 := Max(1e-6, Abs(D.X) + Abs(D.Y) + Abs(D.Z));
  PX := D.X / L1;
  PY := D.Y / L1;
  if D.Z < 0.0 then
  begin
    if PX >= 0.0 then SignX := 1.0 else SignX := -1.0;
    if PY >= 0.0 then SignY := 1.0 else SignY := -1.0;
    Result.X := (1.0 - Abs(PY)) * SignX;
    Result.Y := (1.0 - Abs(PX)) * SignY;
  end else
  begin
    Result.X := PX;
    Result.Y := PY;
  end;
  Result.X := EnsureRange(Result.X, -1.0, 1.0);
  Result.Y := EnsureRange(Result.Y, -1.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.OctahedralDecodeUV(const AOctUV: TVector2): TVector3;
var
  PX, PY, VZ, SignX, SignY: Single;
begin
  PX := EnsureRange(AOctUV.X, -1.0, 1.0);
  PY := EnsureRange(AOctUV.Y, -1.0, 1.0);
  VZ := 1.0 - Abs(PX) - Abs(PY);
  if VZ < 0.0 then
  begin
    if PX >= 0.0 then SignX := 1.0 else SignX := -1.0;
    if PY >= 0.0 then SignY := 1.0 else SignY := -1.0;
    Result.X := (1.0 - Abs(PY)) * SignX;
    Result.Y := (1.0 - Abs(PX)) * SignY;
    Result.Z := VZ;
  end else
    Result := Vector3(PX, PY, VZ);
  Result := Result.Normalize;
end;

class function TCastleShaderGeneratorRHI.EvaluateDDGIProbeWeight(
  const AWorldPos, ASurfaceNormal, AProbePos: TVector3;
  const AProbeMeanDist, AProbeVariance: Single): Single;
var
  ToProbe, DirToProbe: TVector3;
  DistToProbe, WrapCos, V, Diff, Chebyshev: Single;
begin
  ToProbe := AProbePos - AWorldPos;
  DistToProbe := ToProbe.Length;
  DirToProbe := ToProbe / Max(1e-5, DistToProbe);
  WrapCos := Max(0.0001, (TVector3.DotProduct(ASurfaceNormal.Normalize, DirToProbe) + 1.0) * 0.5);
  Result := WrapCos * WrapCos + 0.02;
  if DistToProbe > AProbeMeanDist then
  begin
    V := Max(1e-4, AProbeVariance);
    Diff := DistToProbe - AProbeMeanDist;
    Chebyshev := V / (V + Diff * Diff);
    Result := Result * Max(0.0, Chebyshev * Chebyshev * Chebyshev);
  end;
  Result := Max(1e-5, Result);
end;

function TCastleShaderGeneratorRHI.GenerateMeshletCullingComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    GenerateVersionAndLayoutMacros +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [
      Max(Cardinal(1), AWorkgroupSizeX)
    ]) + NL +
    NL +
    'struct CastleMeshletDescriptor {' + NL +
    '  vec4 BoundingSphere;' + NL +
    '  vec4 ConeAxisAndCutoff;' + NL +
    '  uint VertexOffset;' + NL +
    '  uint VertexCount;' + NL +
    '  uint TriangleOffset;' + NL +
    '  uint TriangleCount;' + NL +
    '};' + NL +
    NL +
    'CASTLE_SSBO(0, 0) readonly buffer CastleMeshletBuffer {' + NL +
    '  CastleMeshletDescriptor u_Meshlets[];' + NL +
    '};' + NL +
    NL +
    'CASTLE_SSBO(0, 1) writeonly buffer CastleVisibleMeshletIndices {' + NL +
    '  uint u_VisibleMeshletIndices[];' + NL +
    '};' + NL +
    NL +
    'CASTLE_SSBO(0, 2) buffer CastleVisibleMeshletCounter {' + NL +
    '  uint u_VisibleMeshletCount;' + NL +
    '};' + NL +
    NL +
    'CASTLE_PUSH_CONSTANT uniform CastleMeshletCullPushConstants {' + NL +
    '  vec4 u_CameraPosAndMaxDist;' + NL +
    '  uint u_MeshletCount;' + NL +
    '} castle_MeshletPC;' + NL +
    NL +
    'bool EvaluateMeshletConeBackfaceCull(vec3 meshletCenter, vec3 cameraPos, vec3 coneAxis, float coneCutoff) {' + NL +
    '  vec3 viewRay = normalize(meshletCenter - cameraPos);' + NL +
    '  return dot(viewRay, normalize(coneAxis)) >= coneCutoff;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  if (idx >= castle_MeshletPC.u_MeshletCount) return;' + NL +
    '  CastleMeshletDescriptor m = u_Meshlets[idx];' + NL +
    '  float dist = distance(m.BoundingSphere.xyz, castle_MeshletPC.u_CameraPosAndMaxDist.xyz);' + NL +
    '  if (dist - m.BoundingSphere.w > castle_MeshletPC.u_CameraPosAndMaxDist.w) return;' + NL +
    '  if (EvaluateMeshletConeBackfaceCull(m.BoundingSphere.xyz, castle_MeshletPC.u_CameraPosAndMaxDist.xyz,' + NL +
    '      m.ConeAxisAndCutoff.xyz, m.ConeAxisAndCutoff.w)) return;' + NL +
    '  uint outSlot = atomicAdd(u_VisibleMeshletCount, 1u);' + NL +
    '  u_VisibleMeshletIndices[outSlot] = idx;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateMeshletConeBackfaceCull(
  const AMeshletCenter, ACameraPosition, AConeAxis: TVector3;
  const AConeCutoff: Single): Boolean;
var
  ViewRay: TVector3;
begin
  ViewRay := (AMeshletCenter - ACameraPosition).Normalize;
  Result := TVector3.DotProduct(ViewRay, AConeAxis.Normalize) >= AConeCutoff;
end;

function TCastleShaderGeneratorRHI.GenerateCASSharpeningAndUpscaleFunctions: String;
begin
  Result :=
    '// --- AMD FidelityFX Contrast-Adaptive Sharpening (CAS) & Edge-Adaptive Upscaling (Phase 22.3) ---' + NL +
    'vec3 EvaluateCASFilter5Tap(vec3 center, vec3 north, vec3 south, vec3 east, vec3 west, float sharpness) {' + NL +
    '  vec3 minRGB = min(center, min(min(north, south), min(east, west)));' + NL +
    '  vec3 maxRGB = max(center, max(max(north, south), max(east, west)));' + NL +
    '  float amp = sqrt(clamp(min(minRGB.g, 1.0 - maxRGB.g) / max(maxRGB.g, 1e-4), 0.0, 1.0));' + NL +
    '  float peak = mix(-0.125, -0.2, clamp(sharpness, 0.0, 1.0));' + NL +
    '  float w = amp * peak;' + NL +
    '  return clamp((center + (north + south + east + west) * w) / max(1.0 + 4.0 * w, 1e-4), 0.0, 1.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateCASFilter5Tap(
  const ACenter, ANorth, ASouth, AEast, AWest: TVector3;
  const ASharpness: Single): TVector3;
var
  MinG, MaxG, Amp, Peak, W, InvDenom: Single;
begin
  MinG := Min(ACenter.Y, Min(Min(ANorth.Y, ASouth.Y), Min(AEast.Y, AWest.Y)));
  MaxG := Max(ACenter.Y, Max(Max(ANorth.Y, ASouth.Y), Max(AEast.Y, AWest.Y)));
  Amp := Sqrt(EnsureRange(Min(MinG, 1.0 - MaxG) / Max(MaxG, 1e-4), 0.0, 1.0));
  Peak := -0.125 + (-0.2 - (-0.125)) * EnsureRange(ASharpness, 0.0, 1.0);
  W := Amp * Peak;
  InvDenom := 1.0 / Max(1.0 + 4.0 * W, 1e-4);
  Result := Vector3(
    EnsureRange((ACenter.X + (ANorth.X + ASouth.X + AEast.X + AWest.X) * W) * InvDenom, 0.0, 1.0),
    EnsureRange((ACenter.Y + (ANorth.Y + ASouth.Y + AEast.Y + AWest.Y) * W) * InvDenom, 0.0, 1.0),
    EnsureRange((ACenter.Z + (ANorth.Z + ASouth.Z + AEast.Z + AWest.Z) * W) * InvDenom, 0.0, 1.0));
end;

function TCastleShaderGeneratorRHI.GenerateInlineRayQueryFunctions(
  const AUseHardwareRayQueryExt: Boolean): String;
var
  ExtHeader: String;
begin
  if AUseHardwareRayQueryExt then
    ExtHeader := '#extension GL_EXT_ray_query : enable' + NL
  else
    ExtHeader := '';

  Result :=
    '// --- Hardware Ray Query (VK_KHR_ray_query) & BVH Intersection (Phase 23.1) ---' + NL +
    ExtHeader +
    'bool EvaluateRayAABBIntersection(vec3 rayOrigin, vec3 rayDir, vec3 boxMin, vec3 boxMax,' + NL +
    '  float tMin, float tMax, out float outTNear, out float outTFar) {' + NL +
    '  vec3 invDir = 1.0 / max(abs(rayDir), vec3(1e-7)) * sign(rayDir + vec3(1e-12));' + NL +
    '  vec3 t0 = (boxMin - rayOrigin) * invDir;' + NL +
    '  vec3 t1 = (boxMax - rayOrigin) * invDir;' + NL +
    '  vec3 tSmaller = min(t0, t1);' + NL +
    '  vec3 tBigger  = max(t0, t1);' + NL +
    '  outTNear = max(tMin, max(tSmaller.x, max(tSmaller.y, tSmaller.z)));' + NL +
    '  outTFar  = min(tMax, min(tBigger.x, min(tBigger.y, tBigger.z)));' + NL +
    '  return outTFar >= outTNear;' + NL +
    '}' + NL +
    NL +
    'bool EvaluateRayTriangleIntersection(vec3 rayOrigin, vec3 rayDir, vec3 v0, vec3 v1, vec3 v2,' + NL +
    '  float tMin, float tMax, out float outT, out float outU, out float outV) {' + NL +
    '  vec3 e1 = v1 - v0;' + NL +
    '  vec3 e2 = v2 - v0;' + NL +
    '  vec3 pvec = cross(rayDir, e2);' + NL +
    '  float det = dot(e1, pvec);' + NL +
    '  if (abs(det) < 1e-7) return false;' + NL +
    '  float invDet = 1.0 / det;' + NL +
    '  vec3 tvec = rayOrigin - v0;' + NL +
    '  outU = dot(tvec, pvec) * invDet;' + NL +
    '  if (outU < 0.0 || outU > 1.0) return false;' + NL +
    '  vec3 qvec = cross(tvec, e1);' + NL +
    '  outV = dot(rayDir, qvec) * invDet;' + NL +
    '  if (outV < 0.0 || (outU + outV) > 1.0) return false;' + NL +
    '  outT = dot(e2, qvec) * invDet;' + NL +
    '  return (outT >= tMin && outT <= tMax);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateRayAABBIntersection(
  const ARayOrigin, ARayDir, ABoxMin, ABoxMax: TVector3;
  const ATMin, ATMax: Single;
  out AHitTNear, AHitTFar: Single): Boolean;

  function SafeInv(const V: Single): Single;
  begin
    if Abs(V) < 1e-7 then
    begin
      if V >= 0.0 then Result := 1e7 else Result := -1e7;
    end else
      Result := 1.0 / V;
  end;

var
  InvX, InvY, InvZ: Single;
  T0X, T1X, T0Y, T1Y, T0Z, T1Z: Single;
begin
  InvX := SafeInv(ARayDir.X);
  InvY := SafeInv(ARayDir.Y);
  InvZ := SafeInv(ARayDir.Z);

  T0X := (ABoxMin.X - ARayOrigin.X) * InvX;
  T1X := (ABoxMax.X - ARayOrigin.X) * InvX;
  T0Y := (ABoxMin.Y - ARayOrigin.Y) * InvY;
  T1Y := (ABoxMax.Y - ARayOrigin.Y) * InvY;
  T0Z := (ABoxMin.Z - ARayOrigin.Z) * InvZ;
  T1Z := (ABoxMax.Z - ARayOrigin.Z) * InvZ;

  AHitTNear := Max(ATMin, Max(Min(T0X, T1X), Max(Min(T0Y, T1Y), Min(T0Z, T1Z))));
  AHitTFar  := Min(ATMax, Min(Max(T0X, T1X), Min(Max(T0Y, T1Y), Max(T0Z, T1Z))));
  Result := AHitTFar >= AHitTNear;
end;

class function TCastleShaderGeneratorRHI.EvaluateRayTriangleIntersection(
  const ARayOrigin, ARayDir, AV0, AV1, AV2: TVector3;
  const ATMin, ATMax: Single;
  out AHitT, ABaryU, ABaryV: Single): Boolean;
var
  E1, E2, PVec, TVec, QVec: TVector3;
  Det, InvDet: Single;
begin
  AHitT := 0.0;
  ABaryU := 0.0;
  ABaryV := 0.0;
  E1 := AV1 - AV0;
  E2 := AV2 - AV0;
  PVec := TVector3.CrossProduct(ARayDir, E2);
  Det := TVector3.DotProduct(E1, PVec);
  if Abs(Det) < 1e-7 then
    Exit(False);
  InvDet := 1.0 / Det;
  TVec := ARayOrigin - AV0;
  ABaryU := TVector3.DotProduct(TVec, PVec) * InvDet;
  if (ABaryU < 0.0) or (ABaryU > 1.0) then
    Exit(False);
  QVec := TVector3.CrossProduct(TVec, E1);
  ABaryV := TVector3.DotProduct(ARayDir, QVec) * InvDet;
  if (ABaryV < 0.0) or (ABaryU + ABaryV > 1.0) then
    Exit(False);
  AHitT := TVector3.DotProduct(E2, QVec) * InvDet;
  Result := (AHitT >= ATMin) and (AHitT <= ATMax);
end;

function TCastleShaderGeneratorRHI.GenerateVSMPageLookupFunctions: String;
begin
  Result :=
    '// --- Virtual Shadow Map (VSM) Page Table & Physical Tile Pool Lookup (Phase 23.2) ---' + NL +
    'void DecodeVSMPageEntry(uint pageEntry, out bool outIsResident, out ivec2 outPhysicalTileCoord) {' + NL +
    '  outIsResident = (pageEntry & 1u) != 0u;' + NL +
    '  outPhysicalTileCoord = ivec2(int((pageEntry >> 8u) & 255u), int((pageEntry >> 16u) & 255u));' + NL +
    '}' + NL +
    NL +
    'vec2 ComputeVSMPhysicalAtlasUV(vec2 virtualUV, ivec2 physicalTileCoord, float pageTableDim, float physicalTilesPerRow) {' + NL +
    '  vec2 inPageUV = fract(clamp(virtualUV, 0.0, 0.999999) * max(1.0, pageTableDim));' + NL +
    '  return (vec2(physicalTileCoord) + inPageUV) / max(1.0, physicalTilesPerRow);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateSeparableSSSSFunctions: String;
begin
  Result :=
    '// --- Separable Screen-Space Subsurface Scattering (SSSSS) Burley Diffusion (Phase 23.3) ---' + NL +
    'vec3 EvaluateBurleyDiffusionProfile(float radiusMM, vec3 scatterDistanceRGB) {' + NL +
    '  float r = max(1e-3, abs(radiusMM));' + NL +
    '  vec3 d = max(vec3(1e-3), scatterDistanceRGB);' + NL +
    '  vec3 num = exp(-r / d) + exp(-r / (3.0 * d));' + NL +
    '  return num / (8.0 * 3.14159265359 * d * r);' + NL +
    '}' + NL +
    NL +
    'float EvaluateBilateralSSSSWeight(float sampleOffsetMM, float depthDiffMeters, float scatterDist, float depthFalloffScale) {' + NL +
    '  float r = max(1e-3, abs(sampleOffsetMM));' + NL +
    '  float d = max(1e-3, scatterDist);' + NL +
    '  float spatial = (exp(-r / d) + exp(-r / (3.0 * d))) / (8.0 * 3.14159265359 * d * r);' + NL +
    '  float dz = depthDiffMeters * max(0.0, depthFalloffScale);' + NL +
    '  float bilateral = exp(-(dz * dz));' + NL +
    '  return spatial * bilateral;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateBurleyDiffusionProfile(
  const ARadiusMillimeters: Single;
  const AScatterDistanceRGB: TVector3): TVector3;
var
  R, DX, DY, DZ, EightPiR: Single;
begin
  R := Max(1e-3, Abs(ARadiusMillimeters));
  DX := Max(1e-3, AScatterDistanceRGB.X);
  DY := Max(1e-3, AScatterDistanceRGB.Y);
  DZ := Max(1e-3, AScatterDistanceRGB.Z);
  EightPiR := 8.0 * Single(Pi) * R;
  Result := Vector3(
    (Exp(-R / DX) + Exp(-R / (3.0 * DX))) / (EightPiR * DX),
    (Exp(-R / DY) + Exp(-R / (3.0 * DY))) / (EightPiR * DY),
    (Exp(-R / DZ) + Exp(-R / (3.0 * DZ))) / (EightPiR * DZ));
end;

class function TCastleShaderGeneratorRHI.EvaluateBilateralSSSSWeight(
  const ASampleOffsetMillimeters, ADepthDiffMeters, AScatterDist, ADepthFalloffScale: Single): Single;
var
  R, D, Spatial, DZ, Bilateral: Single;
begin
  R := Max(1e-3, Abs(ASampleOffsetMillimeters));
  D := Max(1e-3, AScatterDist);
  Spatial := (Exp(-R / D) + Exp(-R / (3.0 * D))) / (8.0 * Single(Pi) * D * R);
  DZ := ADepthDiffMeters * Max(0.0, ADepthFalloffScale);
  Bilateral := Exp(-(DZ * DZ));
  Result := Spatial * Bilateral;
end;

function TCastleShaderGeneratorRHI.GenerateLODSelectionComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [Max(1, AWorkgroupSizeX)]) + NL +
    NL +
    'struct CastleLODInstanceDescriptor {' + NL +
    '  vec4 BoundingSphere;' + NL +
    '  vec4 LODScreenThresholds;' + NL +
    '  uint MeshID;' + NL +
    '  uint InstanceID;' + NL +
    '  float TransitionBand;' + NL +
    '  uint Flags;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 0) readonly buffer CastleLODInstancesBlock {' + NL +
    '  CastleLODInstanceDescriptor u_Instances[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) writeonly buffer CastleSelectedLODBlock {' + NL +
    '  ivec4 u_SelectedLODAndWeight[];' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleLODSelectionPush {' + NL +
    '  vec4 u_CameraPosAndCotHalfFovY;' + NL +
    '  uint u_InstanceCount;' + NL +
    '} PushConstants;' + NL +
    NL +
    'float EvaluateProjectedScreenCoverage(float dist, float radius, float cotHalfFovY) {' + NL +
    '  return clamp((2.0 * max(0.0, radius) * max(0.01, cotHalfFovY)) / max(1e-4, dist), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'int SelectDiscreteLODLevel(float coverage, vec4 thresholds, float band, out float outCrossfade) {' + NL +
    '  outCrossfade = 0.0;' + NL +
    '  if (coverage < thresholds.w) return -1;' + NL +
    '  float b = max(1e-4, band);' + NL +
    '  if (coverage >= thresholds.x) {' + NL +
    '    outCrossfade = clamp((thresholds.x + b - coverage) / b, 0.0, 1.0);' + NL +
    '    return 0;' + NL +
    '  }' + NL +
    '  if (coverage >= thresholds.y) {' + NL +
    '    outCrossfade = clamp((thresholds.y + b - coverage) / b, 0.0, 1.0);' + NL +
    '    return 1;' + NL +
    '  }' + NL +
    '  if (coverage >= thresholds.z) {' + NL +
    '    outCrossfade = clamp((thresholds.z + b - coverage) / b, 0.0, 1.0);' + NL +
    '    return 2;' + NL +
    '  }' + NL +
    '  return 3;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  if (idx >= PushConstants.u_InstanceCount) return;' + NL +
    '  CastleLODInstanceDescriptor inst = u_Instances[idx];' + NL +
    '  float dist = length(inst.BoundingSphere.xyz - PushConstants.u_CameraPosAndCotHalfFovY.xyz);' + NL +
    '  float coverage = EvaluateProjectedScreenCoverage(dist, inst.BoundingSphere.w, PushConstants.u_CameraPosAndCotHalfFovY.w);' + NL +
    '  float crossfade;' + NL +
    '  int lod = SelectDiscreteLODLevel(coverage, inst.LODScreenThresholds, inst.TransitionBand, crossfade);' + NL +
    '  u_SelectedLODAndWeight[idx] = ivec4(lod, int(inst.MeshID), int(inst.InstanceID), int(crossfade * 255.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateProjectedScreenCoverage(
  const ADistance, ARadius, ACotHalfFovY: Single): Single;
begin
  Result := EnsureRange(
    (2.0 * Max(0.0, ARadius) * Max(0.01, ACotHalfFovY)) / Max(1e-4, ADistance),
    0.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.SelectDiscreteLODLevel(
  const AScreenCoverage: Single;
  const AThresholds: TVector4;
  out ACrossfadeWeight: Single;
  const ATransitionBand: Single): Integer;
var
  Band: Single;
begin
  ACrossfadeWeight := 0.0;
  if AScreenCoverage < AThresholds.W then
    Exit(-1);
  Band := Max(1e-4, ATransitionBand);
  if AScreenCoverage >= AThresholds.X then
  begin
    ACrossfadeWeight := EnsureRange((AThresholds.X + Band - AScreenCoverage) / Band, 0.0, 1.0);
    Exit(0);
  end;
  if AScreenCoverage >= AThresholds.Y then
  begin
    ACrossfadeWeight := EnsureRange((AThresholds.Y + Band - AScreenCoverage) / Band, 0.0, 1.0);
    Exit(1);
  end;
  if AScreenCoverage >= AThresholds.Z then
  begin
    ACrossfadeWeight := EnsureRange((AThresholds.Z + Band - AScreenCoverage) / Band, 0.0, 1.0);
    Exit(2);
  end;
  Result := 3;
end;

function TCastleShaderGeneratorRHI.GenerateBentNormalsAndSpecularOcclusionFunctions: String;
begin
  Result :=
    '// --- Screen-Space Bent Normals & Lagarde/Jimenez Specular Occlusion (Phase 24.2) ---' + NL +
    'float EvaluateSpecularOcclusion(float NdotV, float ambientOcclusion, float roughness) {' + NL +
    '  float nv = clamp(NdotV, 0.0, 1.0);' + NL +
    '  float ao = clamp(ambientOcclusion, 0.0, 1.0);' + NL +
    '  float r  = clamp(roughness, 0.04, 1.0);' + NL +
    '  float exponent = exp2(-16.0 * r - 1.0);' + NL +
    '  return clamp(pow(max(1e-5, nv + ao), exponent) - 1.0 + ao, 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec3 BendNormalTowardsUnoccludedDir(vec3 surfaceNormal, vec3 unoccludedBentDir, float ambientOcclusion) {' + NL +
    '  float ao = clamp(ambientOcclusion, 0.0, 1.0);' + NL +
    '  float bendFactor = 1.0 - (1.0 - ao) * (1.0 - ao);' + NL +
    '  return normalize(mix(unoccludedBentDir, surfaceNormal, bendFactor));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSpecularOcclusion(
  const ANdotV, AAmbientOcclusion, ARoughness: Single): Single;
var
  NV, AO, R, Exponent: Single;
begin
  NV := EnsureRange(ANdotV, 0.0, 1.0);
  AO := EnsureRange(AAmbientOcclusion, 0.0, 1.0);
  R  := EnsureRange(ARoughness, 0.04, 1.0);
  Exponent := Power(2.0, -16.0 * R - 1.0);
  Result := EnsureRange(Power(Max(1e-5, NV + AO), Exponent) - 1.0 + AO, 0.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.BendNormalTowardsUnoccludedDir(
  const ASurfaceNormal, AUnoccludedBentDir: TVector3;
  const AAmbientOcclusion: Single): TVector3;
var
  AO, BendFactor: Single;
  Blended: TVector3;
begin
  AO := EnsureRange(AAmbientOcclusion, 0.0, 1.0);
  BendFactor := 1.0 - Sqr(1.0 - AO);
  Blended := TVector3.Lerp(BendFactor, AUnoccludedBentDir, ASurfaceNormal);
  if Blended.LengthSqr > 1e-8 then
    Result := Blended.Normalize
  else
    Result := ASurfaceNormal;
end;

function TCastleShaderGeneratorRHI.GenerateColorGradingLUTAndFilmGrainFunctions: String;
begin
  Result :=
    '// --- Trilinear 3D Color Grading LUT, Natural Cos^4 Vignette & Film Grain (Phase 24.3) ---' + NL +
    'vec3 Compute3DLUTSampleCoord(vec3 colorRGB, float lutDimension) {' + NL +
    '  float dim = max(2.0, lutDimension);' + NL +
    '  vec3 scale = vec3((dim - 1.0) / dim);' + NL +
    '  vec3 offset = vec3(0.5 / dim);' + NL +
    '  return clamp(colorRGB, 0.0, 1.0) * scale + offset;' + NL +
    '}' + NL +
    NL +
    'float EvaluateNaturalCos4Vignette(vec2 uv, float intensity) {' + NL +
    '  vec2 d = (uv - vec2(0.5)) * max(0.0, intensity);' + NL +
    '  float cos2Theta = 1.0 / (1.0 + dot(d, d));' + NL +
    '  return cos2Theta * cos2Theta;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateFilmGrainNoise(vec3 color, vec2 uv, float timeSeconds, float intensity) {' + NL +
    '  float seed = dot(uv, vec2(12.9898, 78.233)) + fract(timeSeconds) * 43.17;' + NL +
    '  float noise = fract(sin(seed) * 43758.5453) * 2.0 - 1.0;' + NL +
    '  float luma = dot(max(vec3(0.0), color), vec3(0.2126, 0.7152, 0.0722));' + NL +
    '  float response = 1.0 - sqrt(clamp(luma, 0.0, 1.0));' + NL +
    '  return max(vec3(0.0), color + vec3(noise * intensity * response));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.Compute3DLUTSampleCoord(
  const AColorRGB: TVector3;
  const ALutDimension: Single): TVector3;
var
  Dim, Scale, Offset: Single;
begin
  Dim := Max(2.0, ALutDimension);
  Scale := (Dim - 1.0) / Dim;
  Offset := 0.5 / Dim;
  Result := Vector3(
    EnsureRange(AColorRGB.X, 0.0, 1.0) * Scale + Offset,
    EnsureRange(AColorRGB.Y, 0.0, 1.0) * Scale + Offset,
    EnsureRange(AColorRGB.Z, 0.0, 1.0) * Scale + Offset);
end;

class function TCastleShaderGeneratorRHI.EvaluateNaturalCos4Vignette(
  const AUV: TVector2;
  const AIntensity: Single): Single;
var
  DX, DY, Cos2Theta: Single;
begin
  DX := (AUV.X - 0.5) * Max(0.0, AIntensity);
  DY := (AUV.Y - 0.5) * Max(0.0, AIntensity);
  Cos2Theta := 1.0 / (1.0 + DX * DX + DY * DY);
  Result := Cos2Theta * Cos2Theta;
end;

class function TCastleShaderGeneratorRHI.EvaluateFilmGrainNoise(
  const AColor: TVector3;
  const AUV: TVector2;
  const ATimeSeconds, AIntensity: Single): TVector3;
var
  Seed, Noise, Luma, Response, Delta: Single;
begin
  Seed := AUV.X * 12.9898 + AUV.Y * 78.233 + Frac(ATimeSeconds) * 43.17;
  Noise := Frac(Sin(Seed) * 43758.5453) * 2.0 - 1.0;
  Luma := Max(0.0, AColor.X) * 0.2126 + Max(0.0, AColor.Y) * 0.7152 + Max(0.0, AColor.Z) * 0.0722;
  Response := 1.0 - Sqrt(EnsureRange(Luma, 0.0, 1.0));
  Delta := Noise * AIntensity * Response;
  Result := Vector3(
    Max(0.0, AColor.X + Delta),
    Max(0.0, AColor.Y + Delta),
    Max(0.0, AColor.Z + Delta));
end;

function TCastleShaderGeneratorRHI.GenerateVolumetricCloudFunctions: String;
begin
  Result :=
    '// --- Volumetric Cloud Raymarching: Dual-Lobe Henyey-Greenstein & Beer-Powder Law (Phase 25.1) ---' + NL +
    'float EvaluateSingleHGPhase(float cosTheta, float g) {' + NL +
    '  float g2 = g * g;' + NL +
    '  float denom = max(1e-4, 1.0 + g2 - 2.0 * g * clamp(cosTheta, -1.0, 1.0));' + NL +
    '  return (1.0 - g2) / (4.0 * 3.14159265359 * denom * sqrt(denom));' + NL +
    '}' + NL +
    NL +
    'float EvaluateDualLobeHGPhase(float cosTheta, float forwardG, float backwardG, float forwardWeight) {' + NL +
    '  float w = clamp(forwardWeight, 0.0, 1.0);' + NL +
    '  return mix(EvaluateSingleHGPhase(cosTheta, backwardG), EvaluateSingleHGPhase(cosTheta, forwardG), w);' + NL +
    '}' + NL +
    NL +
    'float EvaluateBeerPowderTransmittance(float opticalDepth, float powderStrength) {' + NL +
    '  float tau = max(0.0, opticalDepth);' + NL +
    '  float beer = exp(-tau);' + NL +
    '  float powder = 1.0 - exp(-2.0 * tau);' + NL +
    '  return beer * mix(1.0, 2.0 * powder, clamp(powderStrength, 0.0, 1.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateDualLobeHGPhase(
  const ACosTheta, AForwardG, ABackwardG, AForwardWeight: Single): Single;

  function SingleHG(const CosT, G: Single): Single;
  var
    G2, Denom: Single;
  begin
    G2 := G * G;
    Denom := Max(1e-4, 1.0 + G2 - 2.0 * G * EnsureRange(CosT, -1.0, 1.0));
    Result := (1.0 - G2) / (4.0 * Single(Pi) * Denom * Sqrt(Denom));
  end;

var
  W: Single;
begin
  W := EnsureRange(AForwardWeight, 0.0, 1.0);
  Result := SingleHG(ACosTheta, ABackwardG) * (1.0 - W) + SingleHG(ACosTheta, AForwardG) * W;
end;

class function TCastleShaderGeneratorRHI.EvaluateBeerPowderTransmittance(
  const AOpticalDepth, APowderStrength: Single): Single;
var
  Tau, Beer, Powder, S: Single;
begin
  Tau := Max(0.0, AOpticalDepth);
  Beer := Exp(-Tau);
  Powder := 1.0 - Exp(-2.0 * Tau);
  S := EnsureRange(APowderStrength, 0.0, 1.0);
  Result := Beer * ((1.0 - S) + S * (2.0 * Powder));
end;

function TCastleShaderGeneratorRHI.GenerateTextureMipFeedbackFunctions: String;
begin
  Result :=
    '// --- GPU Texture Streaming Mipmap Residency Feedback (Phase 25.2) ---' + NL +
    'uint ComputeDesiredMipLevel(float texelFootprintMaxDeriv, uint maxMipLevel, float mipBias) {' + NL +
    '  float rho = max(1.0, texelFootprintMaxDeriv);' + NL +
    '  float lod = log2(rho) + mipBias;' + NL +
    '  return uint(clamp(int(floor(lod + 0.5)), 0, int(maxMipLevel)));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.ComputeDesiredMipLevel(
  const ATexelFootprintMaxDerivative: Single;
  const AMaxMipLevel: Cardinal;
  const AMipBias: Single): Cardinal;
var
  Rho, Lod: Single;
  ClampedInt: Integer;
begin
  Rho := Max(1.0, ATexelFootprintMaxDerivative);
  Lod := Log2(Rho) + AMipBias;
  ClampedInt := EnsureRange(Floor(Lod + 0.5), 0, Integer(AMaxMipLevel));
  Result := Cardinal(ClampedInt);
end;

function TCastleShaderGeneratorRHI.GenerateEquirectToCubemapComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;',
      [Max(1, AWorkgroupSizeX), Max(1, AWorkgroupSizeY)]) + NL +
    NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_EquirectPanorama;' + NL +
    'layout(rgba16f, set = 0, binding = 1) writeonly uniform image2DArray u_OutCubemapFaces;' + NL +
    NL +
    'vec2 DirectionToEquirectangularUV(vec3 dir) {' + NL +
    '  vec3 d = normalize(dir);' + NL +
    '  float u = atan(d.z, d.x) / (2.0 * 3.14159265359) + 0.5;' + NL +
    '  float v = asin(clamp(d.y, -1.0, 1.0)) / 3.14159265359 + 0.5;' + NL +
    '  return vec2(u, v);' + NL +
    '}' + NL +
    NL +
    'vec3 CubeFaceUVToDirection(uint faceIdx, vec2 uv) {' + NL +
    '  vec2 st = uv * 2.0 - 1.0;' + NL +
    '  if (faceIdx == 0u) return normalize(vec3( 1.0, -st.y, -st.x));' + NL +
    '  if (faceIdx == 1u) return normalize(vec3(-1.0, -st.y,  st.x));' + NL +
    '  if (faceIdx == 2u) return normalize(vec3( st.x,  1.0,  st.y));' + NL +
    '  if (faceIdx == 3u) return normalize(vec3( st.x, -1.0, -st.y));' + NL +
    '  if (faceIdx == 4u) return normalize(vec3( st.x, -st.y,  1.0));' + NL +
    '  return normalize(vec3(-st.x, -st.y, -1.0));' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  ivec2 faceSize = imageSize(u_OutCubemapFaces).xy;' + NL +
    '  ivec3 gid = ivec3(gl_GlobalInvocationID);' + NL +
    '  if (gid.x >= faceSize.x || gid.y >= faceSize.y || gid.z >= 6) return;' + NL +
    '  vec2 uv = (vec2(gid.xy) + vec2(0.5)) / vec2(faceSize);' + NL +
    '  vec3 dir = CubeFaceUVToDirection(uint(gid.z), uv);' + NL +
    '  vec2 eqUV = DirectionToEquirectangularUV(dir);' + NL +
    '  vec4 hdrColor = textureLod(u_EquirectPanorama, eqUV, 0.0);' + NL +
    '  imageStore(u_OutCubemapFaces, gid, hdrColor);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.DirectionToEquirectangularUV(
  const ADir: TVector3): TVector2;
var
  D: TVector3;
begin
  if ADir.LengthSqr > 1e-8 then
    D := ADir.Normalize
  else
    D := Vector3(1.0, 0.0, 0.0);
  Result := Vector2(
    ArcTan2(D.Z, D.X) / (2.0 * Single(Pi)) + 0.5,
    ArcSin(EnsureRange(D.Y, -1.0, 1.0)) / Single(Pi) + 0.5);
end;

class function TCastleShaderGeneratorRHI.EquirectangularUVToDirection(
  const AUV: TVector2): TVector3;
var
  Phi, Theta, CosTheta: Single;
begin
  Phi := (AUV.X - 0.5) * (2.0 * Single(Pi));
  Theta := (AUV.Y - 0.5) * Single(Pi);
  CosTheta := Cos(Theta);
  Result := Vector3(
    CosTheta * Cos(Phi),
    Sin(Theta),
    CosTheta * Sin(Phi));
end;

function TCastleShaderGeneratorRHI.GenerateAreaLightLTCFunctions: String;
begin
  Result :=
    '// --- Analytic Rectangular Area Lights: Solid-Angle & LTC Roughness (Phase 26.1) ---' + NL +
    'vec3 IntegrateEdgeVector(vec3 v1, vec3 v2) {' + NL +
    '  float x = dot(v1, v2);' + NL +
    '  float y = abs(x);' + NL +
    '  float a = 0.8543985 + (0.4965155 + 0.0145206 * y) * y;' + NL +
    '  float b = 3.4175940 + (4.1616724 + y) * y;' + NL +
    '  float v = a / b;' + NL +
    '  float thetaSintheta = (x > 0.0) ? v : 0.5 * inversesqrt(max(1.0 - x * x, 1e-7)) - v;' + NL +
    '  return cross(v1, v2) * thetaSintheta;' + NL +
    '}' + NL +
    NL +
    'float EvaluateRectAreaLightSolidAngle(vec3 surfacePos, vec3 rectCenter, vec3 rectRight, vec3 rectUp, float halfWidth, float halfHeight) {' + NL +
    '  vec3 ex = rectRight * halfWidth;' + NL +
    '  vec3 ey = rectUp * halfHeight;' + NL +
    '  vec3 p0 = normalize(rectCenter - ex - ey - surfacePos);' + NL +
    '  vec3 p1 = normalize(rectCenter + ex - ey - surfacePos);' + NL +
    '  vec3 p2 = normalize(rectCenter + ex + ey - surfacePos);' + NL +
    '  vec3 p3 = normalize(rectCenter - ex + ey - surfacePos);' + NL +
    '  vec3 sum = IntegrateEdgeVector(p0, p1) + IntegrateEdgeVector(p1, p2) + IntegrateEdgeVector(p2, p3) + IntegrateEdgeVector(p3, p0);' + NL +
    '  return length(sum);' + NL +
    '}' + NL +
    NL +
    'float EvaluateLTCFilteredRoughness(float baseRoughness, float distToLight, float areaExtentRadius) {' + NL +
    '  float r = clamp(baseRoughness, 0.04, 1.0);' + NL +
    '  float rad = max(0.0, areaExtentRadius);' + NL +
    '  float d = max(1e-3, distToLight);' + NL +
    '  return clamp(sqrt(r * r + rad / (rad + 2.0 * d)), 0.04, 1.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateRectAreaLightSolidAngle(
  const ASurfacePos, ARectCenter, ARectRight, ARectUp: TVector3;
  const AHalfWidth, AHalfHeight: Single): Single;

  function EdgeVector(const V1, V2: TVector3): TVector3;
  var
    CosAngle, Theta: Single;
    CrossVec: TVector3;
  begin
    CosAngle := EnsureRange(TVector3.DotProduct(V1, V2), -1.0, 1.0);
    Theta := ArcCos(CosAngle);
    CrossVec := TVector3.CrossProduct(V1, V2);
    if CrossVec.LengthSqr > 1e-10 then
      Result := CrossVec.Normalize * (0.5 * Theta)
    else
      Result := TVector3.Zero;
  end;

var
  EX, EY, P0, P1, P2, P3, SumVec: TVector3;
begin
  EX := ARectRight * AHalfWidth;
  EY := ARectUp * AHalfHeight;
  P0 := (ARectCenter - EX - EY - ASurfacePos).Normalize;
  P1 := (ARectCenter + EX - EY - ASurfacePos).Normalize;
  P2 := (ARectCenter + EX + EY - ASurfacePos).Normalize;
  P3 := (ARectCenter - EX + EY - ASurfacePos).Normalize;
  SumVec := EdgeVector(P0, P1) + EdgeVector(P1, P2) + EdgeVector(P2, P3) + EdgeVector(P3, P0);
  Result := SumVec.Length;
end;

class function TCastleShaderGeneratorRHI.EvaluateLTCFilteredRoughness(
  const ABaseRoughness, ADistanceToAreaLight, AAreaExtentRadius: Single): Single;
var
  R, Rad, D: Single;
begin
  R := EnsureRange(ABaseRoughness, 0.04, 1.0);
  Rad := Max(0.0, AAreaExtentRadius);
  D := Max(1e-3, ADistanceToAreaLight);
  Result := EnsureRange(Sqrt(R * R + Rad / (Rad + 2.0 * D)), 0.04, 1.0);
end;

function TCastleShaderGeneratorRHI.GenerateTriplanarAndHeightBlendFunctions: String;
begin
  Result :=
    '// --- Seamless World-Space Triplanar & Height-Modulated Splat Blending (Phase 26.2) ---' + NL +
    'vec3 EvaluateTriplanarBlendWeights(vec3 worldNormal, float sharpness) {' + NL +
    '  vec3 n = abs(normalize(worldNormal));' + NL +
    '  vec3 w = pow(max(n, vec3(1e-5)), vec3(max(1.0, sharpness)));' + NL +
    '  return w / max(1e-6, w.x + w.y + w.z);' + NL +
    '}' + NL +
    NL +
    'vec4 EvaluateHeightModulatedBlendWeights(vec4 layerWeights, vec4 layerHeights, float heightContrast) {' + NL +
    '  float c = max(1e-3, heightContrast);' + NL +
    '  vec4 h = layerWeights + layerHeights;' + NL +
    '  float maxH = max(max(h.x, h.y), max(h.z, h.w)) - c;' + NL +
    '  vec4 b = max(h - vec4(maxH), vec4(0.0));' + NL +
    '  return b / max(1e-6, b.x + b.y + b.z + b.w);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateTriplanarBlendWeights(
  const AWorldNormal: TVector3;
  const ASharpness: Single): TVector3;
var
  N: TVector3;
  P, WX, WY, WZ, SumW: Single;
begin
  if AWorldNormal.LengthSqr > 1e-8 then
    N := AWorldNormal.Normalize
  else
    N := Vector3(0.0, 1.0, 0.0);
  P := Max(1.0, ASharpness);
  WX := Power(Max(1e-5, Abs(N.X)), P);
  WY := Power(Max(1e-5, Abs(N.Y)), P);
  WZ := Power(Max(1e-5, Abs(N.Z)), P);
  SumW := Max(1e-6, WX + WY + WZ);
  Result := Vector3(WX / SumW, WY / SumW, WZ / SumW);
end;

class function TCastleShaderGeneratorRHI.EvaluateHeightModulatedBlendWeights(
  const ALayerWeights, ALayerHeights: TVector4;
  const AHeightContrast: Single): TVector4;
var
  C, H0, H1, H2, H3, MaxH, B0, B1, B2, B3, SumB: Single;
begin
  C := Max(1e-3, AHeightContrast);
  H0 := ALayerWeights.X + ALayerHeights.X;
  H1 := ALayerWeights.Y + ALayerHeights.Y;
  H2 := ALayerWeights.Z + ALayerHeights.Z;
  H3 := ALayerWeights.W + ALayerHeights.W;
  MaxH := Max(Max(H0, H1), Max(H2, H3)) - C;
  B0 := Max(0.0, H0 - MaxH);
  B1 := Max(0.0, H1 - MaxH);
  B2 := Max(0.0, H2 - MaxH);
  B3 := Max(0.0, H3 - MaxH);
  SumB := Max(1e-6, B0 + B1 + B2 + B3);
  Result := Vector4(B0 / SumB, B1 / SumB, B2 / SumB, B3 / SumB);
end;

function TCastleShaderGeneratorRHI.GenerateShadowCasterExtrusionCullingComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [Max(1, AWorkgroupSizeX)]) + NL +
    NL +
    'layout(std430, set = 0, binding = 0) readonly buffer CastleShadowCastersBlock {' + NL +
    '  vec4 u_CasterSpheres[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) writeonly buffer CastleVisibleShadowCastersBlock {' + NL +
    '  uint u_VisibleCasterFlags[];' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleShadowCasterCullPush {' + NL +
    '  vec4 u_LightRayDirAndMaxDist;' + NL +
    '  vec4 u_ReceiverBoxMin;' + NL +
    '  vec4 u_ReceiverBoxMax;' + NL +
    '} PushConstants;' + NL +
    NL +
    'bool EvaluateShadowCasterExtrudedSphereIntersectsAABB(vec3 sphereCenter, float sphereRadius,' + NL +
    '  vec3 lightRayDir, float maxExtrusionDist, vec3 receiverMin, vec3 receiverMax) {' + NL +
    '  vec3 expMin = receiverMin - vec3(max(0.0, sphereRadius));' + NL +
    '  vec3 expMax = receiverMax + vec3(max(0.0, sphereRadius));' + NL +
    '  vec3 invDir = 1.0 / max(abs(lightRayDir), vec3(1e-7)) * sign(lightRayDir + vec3(1e-12));' + NL +
    '  vec3 t0 = (expMin - sphereCenter) * invDir;' + NL +
    '  vec3 t1 = (expMax - sphereCenter) * invDir;' + NL +
    '  vec3 tSmaller = min(t0, t1);' + NL +
    '  vec3 tBigger  = max(t0, t1);' + NL +
    '  float tNear = max(0.0, max(tSmaller.x, max(tSmaller.y, tSmaller.z)));' + NL +
    '  float tFar  = min(max(0.0, maxExtrusionDist), min(tBigger.x, min(tBigger.y, tBigger.z)));' + NL +
    '  return tFar >= tNear;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  uint casterCount = uint(PushConstants.u_ReceiverBoxMin.w);' + NL +
    '  if (idx >= casterCount) return;' + NL +
    '  vec4 sphere = u_CasterSpheres[idx];' + NL +
    '  bool vis = EvaluateShadowCasterExtrudedSphereIntersectsAABB(sphere.xyz, sphere.w,' + NL +
    '    PushConstants.u_LightRayDirAndMaxDist.xyz, PushConstants.u_LightRayDirAndMaxDist.w,' + NL +
    '    PushConstants.u_ReceiverBoxMin.xyz, PushConstants.u_ReceiverBoxMax.xyz);' + NL +
    '  u_VisibleCasterFlags[idx] = vis ? 1u : 0u;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateShadowCasterExtrudedSphereIntersectsAABB(
  const ASphereCenter: TVector3;
  const ASphereRadius: Single;
  const ALightRayDir: TVector3;
  const AMaxExtrusionDist: Single;
  const AReceiverBoxMin, AReceiverBoxMax: TVector3): Boolean;
var
  ExpMin, ExpMax, RayDir: TVector3;
  R, TNear, TFar: Single;
begin
  R := Max(0.0, ASphereRadius);
  ExpMin := AReceiverBoxMin - Vector3(R, R, R);
  ExpMax := AReceiverBoxMax + Vector3(R, R, R);
  if ALightRayDir.LengthSqr > 1e-8 then
    RayDir := ALightRayDir.Normalize
  else
    RayDir := Vector3(0.0, -1.0, 0.0);
  Result := EvaluateRayAABBIntersection(
    ASphereCenter, RayDir, ExpMin, ExpMax, 0.0, Max(0.0, AMaxExtrusionDist), TNear, TFar);
end;

function TCastleShaderGeneratorRHI.GenerateVisibilityBufferFunctions: String;
begin
  Result :=
    '// --- 64-Bit Visibility Buffer & Perspective-Correct Barycentrics (Phase 27.1) ---' + NL +
    'uvec2 PackVisibilityBufferPayload(float depth01, uint instanceID, uint primitiveID) {' + NL +
    '  uint d32 = uint(clamp(depth01, 0.0, 1.0) * 4294967295.0 + 0.5);' + NL +
    '  uint id32 = ((instanceID & 0xFFFFFu) << 12u) | (primitiveID & 0xFFFu);' + NL +
    '  return uvec2(id32, d32);' + NL +
    '}' + NL +
    NL +
    'void UnpackVisibilityBufferPayload(uvec2 payload, out float outDepth01, out uint outInstanceID, out uint outPrimitiveID) {' + NL +
    '  outDepth01 = float(payload.y) / 4294967295.0;' + NL +
    '  outInstanceID = (payload.x >> 12u) & 0xFFFFFu;' + NL +
    '  outPrimitiveID = payload.x & 0xFFFu;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluatePerspectiveBarycentrics2D(vec2 pixelNDC, vec2 v0NDC, vec2 v1NDC, vec2 v2NDC, float invW0, float invW1, float invW2) {' + NL +
    '  vec2 e0 = v1NDC - v0NDC;' + NL +
    '  vec2 e1 = v2NDC - v0NDC;' + NL +
    '  vec2 ep = pixelNDC - v0NDC;' + NL +
    '  float det = e0.x * e1.y - e0.y * e1.x;' + NL +
    '  float invDet = (abs(det) > 1e-8) ? (1.0 / det) : 0.0;' + NL +
    '  float b1 = (ep.x * e1.y - ep.y * e1.x) * invDet;' + NL +
    '  float b2 = (e0.x * ep.y - e0.y * ep.x) * invDet;' + NL +
    '  float b0 = 1.0 - b1 - b2;' + NL +
    '  vec3 persp = vec3(b0 * invW0, b1 * invW1, b2 * invW2);' + NL +
    '  float sumP = persp.x + persp.y + persp.z;' + NL +
    '  return (abs(sumP) > 1e-8) ? (persp / sumP) : vec3(1.0, 0.0, 0.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.PackVisibilityBufferPayload(
  const ADepth01: Single;
  const AInstanceID, APrimitiveID: Cardinal): QWord;
var
  Depth32, Low32: Cardinal;
begin
  Depth32 := Round(EnsureRange(ADepth01, 0.0, 1.0) * 4294967295.0);
  Low32 := ((AInstanceID and $FFFFF) shl 12) or (APrimitiveID and $FFF);
  Result := (QWord(Depth32) shl 32) or QWord(Low32);
end;

class procedure TCastleShaderGeneratorRHI.UnpackVisibilityBufferPayload(
  const APayload: QWord;
  out AOutDepth01: Single;
  out AOutInstanceID, AOutPrimitiveID: Cardinal);
var
  Depth32, Low32: Cardinal;
begin
  Depth32 := Cardinal(APayload shr 32);
  Low32 := Cardinal(APayload and $FFFFFFFF);
  AOutDepth01 := Double(Depth32) / 4294967295.0;
  AOutInstanceID := (Low32 shr 12) and $FFFFF;
  AOutPrimitiveID := Low32 and $FFF;
end;

class function TCastleShaderGeneratorRHI.EvaluatePerspectiveBarycentrics2D(
  const APixelNDC, AV0NDC, AV1NDC, AV2NDC: TVector2;
  const AInvW0, AInvW1, AInvW2: Single): TVector3;
var
  E0, E1, EP: TVector2;
  Det, InvDet, B0, B1, B2, P0, P1, P2, SumP: Single;
begin
  E0 := AV1NDC - AV0NDC;
  E1 := AV2NDC - AV0NDC;
  EP := APixelNDC - AV0NDC;
  Det := E0.X * E1.Y - E0.Y * E1.X;
  if Abs(Det) <= 1e-8 then
    Exit(Vector3(1.0, 0.0, 0.0));
  InvDet := 1.0 / Det;
  B1 := (EP.X * E1.Y - EP.Y * E1.X) * InvDet;
  B2 := (E0.X * EP.Y - E0.Y * EP.X) * InvDet;
  B0 := 1.0 - B1 - B2;
  P0 := B0 * AInvW0;
  P1 := B1 * AInvW1;
  P2 := B2 * AInvW2;
  SumP := P0 + P1 + P2;
  if Abs(SumP) <= 1e-8 then
    Exit(Vector3(1.0, 0.0, 0.0));
  Result := Vector3(P0 / SumP, P1 / SumP, P2 / SumP);
end;

function TCastleShaderGeneratorRHI.GenerateClusterDAGCutComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [Max(1, AWorkgroupSizeX)]) + NL +
    NL +
    'struct CastleClusterDAGNode {' + NL +
    '  vec4 SphereCenterAndRadius;' + NL +
    '  vec4 ParentCenterAndErrors;' + NL +
    '  uint ClusterID;' + NL +
    '  uint VertexOffset;' + NL +
    '  uint TriangleOffset;' + NL +
    '  uint TriangleCount;' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 0) readonly buffer CastleClusterDAGNodesBlock {' + NL +
    '  CastleClusterDAGNode u_DAGNodes[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) writeonly buffer CastleSelectedDAGCutBlock {' + NL +
    '  uint u_SelectedCutClusters[];' + NL +
    '};' + NL +
    NL +
    'layout(push_constant) uniform CastleClusterDAGPush {' + NL +
    '  vec4 u_CameraPosAndCotHalfFovY;' + NL +
    '  vec4 u_ViewportHeightThresholdAndCount;' + NL +
    '} PushConstants;' + NL +
    NL +
    'float EvaluateClusterProjectedErrorPixels(vec3 sphereCenter, vec3 cameraPos, float sphereRadius,' + NL +
    '  float worldError, float cotHalfFovY, float viewportHeight) {' + NL +
    '  float dist = max(1e-3, length(sphereCenter - cameraPos) - max(0.0, sphereRadius));' + NL +
    '  float projScale = 0.5 * max(1.0, viewportHeight) * max(0.01, cotHalfFovY);' + NL +
    '  return (max(0.0, worldError) * projScale) / dist;' + NL +
    '}' + NL +
    NL +
    'bool IsClusterDAGCutNode(float selfProjectedErrorPx, float parentProjectedErrorPx, float pixelErrorThreshold) {' + NL +
    '  return (selfProjectedErrorPx <= pixelErrorThreshold) && (parentProjectedErrorPx > pixelErrorThreshold);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  uint idx = gl_GlobalInvocationID.x;' + NL +
    '  uint nodeCount = uint(PushConstants.u_ViewportHeightThresholdAndCount.z);' + NL +
    '  if (idx >= nodeCount) return;' + NL +
    '  CastleClusterDAGNode node = u_DAGNodes[idx];' + NL +
    '  float selfErrPx = EvaluateClusterProjectedErrorPixels(node.SphereCenterAndRadius.xyz,' + NL +
    '    PushConstants.u_CameraPosAndCotHalfFovY.xyz, node.SphereCenterAndRadius.w,' + NL +
    '    node.ParentCenterAndErrors.x, PushConstants.u_CameraPosAndCotHalfFovY.w,' + NL +
    '    PushConstants.u_ViewportHeightThresholdAndCount.x);' + NL +
    '  float parentErrPx = EvaluateClusterProjectedErrorPixels(node.SphereCenterAndRadius.xyz,' + NL +
    '    PushConstants.u_CameraPosAndCotHalfFovY.xyz, node.SphereCenterAndRadius.w,' + NL +
    '    node.ParentCenterAndErrors.y, PushConstants.u_CameraPosAndCotHalfFovY.w,' + NL +
    '    PushConstants.u_ViewportHeightThresholdAndCount.x);' + NL +
    '  bool onCut = IsClusterDAGCutNode(selfErrPx, parentErrPx, PushConstants.u_ViewportHeightThresholdAndCount.y);' + NL +
    '  u_SelectedCutClusters[idx] = onCut ? 1u : 0u;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateClusterProjectedErrorPixels(
  const ASphereCenter, ACameraPos: TVector3;
  const ASphereRadius, AWorldError, ACotHalfFovY, AViewportHeight: Single): Single;
var
  Dist, ProjScale: Single;
begin
  Dist := Max(1e-3, (ASphereCenter - ACameraPos).Length - Max(0.0, ASphereRadius));
  ProjScale := 0.5 * Max(1.0, AViewportHeight) * Max(0.01, ACotHalfFovY);
  Result := (Max(0.0, AWorldError) * ProjScale) / Dist;
end;

class function TCastleShaderGeneratorRHI.IsClusterDAGCutNode(
  const ASelfProjectedErrorPx, AParentProjectedErrorPx, APixelErrorThreshold: Single): Boolean;
begin
  Result := (ASelfProjectedErrorPx <= APixelErrorThreshold) and
            (AParentProjectedErrorPx > APixelErrorThreshold);
end;

function TCastleShaderGeneratorRHI.GenerateSoftwareMicroRasterComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [Max(1, AWorkgroupSizeX)]) + NL +
    NL +
    'float EvaluateEdgeFunction2D(vec2 a, vec2 b, vec2 p) {' + NL +
    '  return (b.x - a.x) * (p.y - a.y) - (b.y - a.y) * (p.x - a.x);' + NL +
    '}' + NL +
    NL +
    'bool IsMicroTriangleCandidateForSoftwareRaster(vec2 v0, vec2 v1, vec2 v2, float maxBoxPixels) {' + NL +
    '  float area2 = EvaluateEdgeFunction2D(v0, v1, v2);' + NL +
    '  if (area2 <= 0.0) return false;' + NL +
    '  vec2 minP = min(v0, min(v1, v2));' + NL +
    '  vec2 maxP = max(v0, max(v1, v2));' + NL +
    '  vec2 ext = maxP - minP;' + NL +
    '  return (ext.x <= maxBoxPixels) && (ext.y <= maxBoxPixels);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateEdgeFunction2D(
  const AA, AB, AP: TVector2): Single;
begin
  Result := (AB.X - AA.X) * (AP.Y - AA.Y) - (AB.Y - AA.Y) * (AP.X - AA.X);
end;

class function TCastleShaderGeneratorRHI.IsMicroTriangleCandidateForSoftwareRaster(
  const AV0Screen, AV1Screen, AV2Screen: TVector2;
  const AMaxBoxPixels: Single): Boolean;
var
  Area2, MinX, MinY, MaxX, MaxY: Single;
begin
  Area2 := EvaluateEdgeFunction2D(AV0Screen, AV1Screen, AV2Screen);
  if Area2 <= 0.0 then
    Exit(False);
  MinX := Min(AV0Screen.X, Min(AV1Screen.X, AV2Screen.X));
  MinY := Min(AV0Screen.Y, Min(AV1Screen.Y, AV2Screen.Y));
  MaxX := Max(AV0Screen.X, Max(AV1Screen.X, AV2Screen.X));
  MaxY := Max(AV0Screen.Y, Max(AV1Screen.Y, AV2Screen.Y));
  Result := ((MaxX - MinX) <= AMaxBoxPixels) and ((MaxY - MinY) <= AMaxBoxPixels);
end;

function TCastleShaderGeneratorRHI.GenerateGlobalSDFTraceFunctions: String;
begin
  Result :=
    '// Phase 28.1: Lumen-Style Global Signed Distance Field (SDF) Sphere Tracing & Cone Visibility' + NL +
    'float EvaluateSDFBoxDistance(vec3 samplePos, vec3 boxCenter, vec3 boxHalfExtents) {' + NL +
    '  vec3 q = abs(samplePos - boxCenter) - max(vec3(0.0), boxHalfExtents);' + NL +
    '  return length(max(q, vec3(0.0))) + min(max(q.x, max(q.y, q.z)), 0.0);' + NL +
    '}' + NL +
    NL +
    'float EvaluateSDFSmoothUnion(float d1, float d2, float k) {' + NL +
    '  float h = clamp(0.5 + 0.5 * (d2 - d1) / max(1e-5, k), 0.0, 1.0);' + NL +
    '  return mix(d2, d1, h) - k * h * (1.0 - h);' + NL +
    '}' + NL +
    NL +
    'vec3 ComputeGlobalSDFClipmapUVW(vec3 worldPos, vec3 clipmapCenter, float cascadeHalfExtent) {' + NL +
    '  return clamp((worldPos - clipmapCenter) / max(1e-4, 2.0 * cascadeHalfExtent) + vec3(0.5), vec3(0.0), vec3(1.0));' + NL +
    '}' + NL +
    NL +
    'float EvaluateSDFSoftShadowCone(float currentVisibility, float sdfDistance, float rayDistance, float coneTanHalfAngle) {' + NL +
    '  float coneRadius = max(1e-4, rayDistance * max(1e-3, coneTanHalfAngle));' + NL +
    '  float stepVis = clamp(sdfDistance / coneRadius, 0.0, 1.0);' + NL +
    '  return min(clamp(currentVisibility, 0.0, 1.0), stepVis);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSDFBoxDistance(
  const ASamplePos, ABoxCenter, ABoxHalfExtents: TVector3): Single;
var
  QX, QY, QZ, OutX, OutY, OutZ, InsideDist: Single;
begin
  QX := Abs(ASamplePos.X - ABoxCenter.X) - Max(0.0, ABoxHalfExtents.X);
  QY := Abs(ASamplePos.Y - ABoxCenter.Y) - Max(0.0, ABoxHalfExtents.Y);
  QZ := Abs(ASamplePos.Z - ABoxCenter.Z) - Max(0.0, ABoxHalfExtents.Z);
  OutX := Max(0.0, QX);
  OutY := Max(0.0, QY);
  OutZ := Max(0.0, QZ);
  InsideDist := Min(0.0, Max(QX, Max(QY, QZ)));
  Result := Sqrt(OutX * OutX + OutY * OutY + OutZ * OutZ) + InsideDist;
end;

class function TCastleShaderGeneratorRHI.EvaluateSDFSmoothUnion(
  const AD1, AD2, AK: Single): Single;
var
  H, SafeK: Single;
begin
  SafeK := Max(1e-5, AK);
  H := EnsureRange(0.5 + 0.5 * (AD2 - AD1) / SafeK, 0.0, 1.0);
  Result := AD2 * (1.0 - H) + AD1 * H - SafeK * H * (1.0 - H);
end;

class function TCastleShaderGeneratorRHI.EvaluateSDFSoftShadowCone(
  const ACurrentVisibility, ASDFDistance, ARayDistance, AConeTanHalfAngle: Single): Single;
var
  ConeRadius, StepVis: Single;
begin
  ConeRadius := Max(1e-4, ARayDistance * Max(1e-3, AConeTanHalfAngle));
  StepVis := EnsureRange(ASDFDistance / ConeRadius, 0.0, 1.0);
  Result := Min(EnsureRange(ACurrentVisibility, 0.0, 1.0), StepVis);
end;

function TCastleShaderGeneratorRHI.GenerateSurfaceCacheCardFunctions: String;
begin
  Result :=
    '// Phase 28.2: Lumen-Style 6-Axis Surface Cache Card Projection' + NL +
    'uint SelectDominantSurfaceCardAxis(vec3 worldNormal) {' + NL +
    '  vec3 an = abs(worldNormal);' + NL +
    '  if (an.x >= an.y && an.x >= an.z) return (worldNormal.x >= 0.0) ? 0u : 1u;' + NL +
    '  if (an.y >= an.x && an.y >= an.z) return (worldNormal.y >= 0.0) ? 2u : 3u;' + NL +
    '  return (worldNormal.z >= 0.0) ? 4u : 5u;' + NL +
    '}' + NL +
    NL +
    'vec2 ComputeSurfaceCacheAtlasUV(vec3 localHitPos, vec3 boxHalfExtents, uint cardAxis,' + NL +
    '  vec2 cardAtlasOffsetUV, vec2 cardAtlasScaleUV) {' + NL +
    '  vec3 normPos = clamp(localHitPos / max(vec3(1e-4), boxHalfExtents) * 0.5 + vec3(0.5), vec3(0.0), vec3(1.0));' + NL +
    '  vec2 faceUV;' + NL +
    '  if (cardAxis <= 1u) faceUV = normPos.yz;' + NL +
    '  else if (cardAxis <= 3u) faceUV = normPos.xz;' + NL +
    '  else faceUV = normPos.xy;' + NL +
    '  return cardAtlasOffsetUV + faceUV * cardAtlasScaleUV;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.SelectDominantSurfaceCardAxis(
  const AWorldNormal: TVector3): Cardinal;
var
  AX, AY, AZ: Single;
begin
  AX := Abs(AWorldNormal.X);
  AY := Abs(AWorldNormal.Y);
  AZ := Abs(AWorldNormal.Z);
  if (AX >= AY) and (AX >= AZ) then
  begin
    if AWorldNormal.X >= 0.0 then Exit(0) else Exit(1);
  end;
  if (AY >= AX) and (AY >= AZ) then
  begin
    if AWorldNormal.Y >= 0.0 then Exit(2) else Exit(3);
  end;
  if AWorldNormal.Z >= 0.0 then Result := 4 else Result := 5;
end;

class function TCastleShaderGeneratorRHI.ComputeSurfaceCacheAtlasUV(
  const ALocalHitPos, ABoxHalfExtents: TVector3;
  const ACardAxis: Cardinal;
  const ACardAtlasOffsetUV, ACardAtlasScaleUV: TVector2): TVector2;
var
  NX, NY, NZ: Single;
  FaceUV: TVector2;
begin
  NX := EnsureRange(ALocalHitPos.X / Max(1e-4, ABoxHalfExtents.X) * 0.5 + 0.5, 0.0, 1.0);
  NY := EnsureRange(ALocalHitPos.Y / Max(1e-4, ABoxHalfExtents.Y) * 0.5 + 0.5, 0.0, 1.0);
  NZ := EnsureRange(ALocalHitPos.Z / Max(1e-4, ABoxHalfExtents.Z) * 0.5 + 0.5, 0.0, 1.0);
  if ACardAxis <= 1 then
    FaceUV := Vector2(NY, NZ)
  else
  if ACardAxis <= 3 then
    FaceUV := Vector2(NX, NZ)
  else
    FaceUV := Vector2(NX, NY);
  Result := Vector2(
    ACardAtlasOffsetUV.X + FaceUV.X * ACardAtlasScaleUV.X,
    ACardAtlasOffsetUV.Y + FaceUV.Y * ACardAtlasScaleUV.Y);
end;

function TCastleShaderGeneratorRHI.GenerateScreenRadianceCacheFunctions: String;
begin
  Result :=
    '// Phase 28.3: Screen-Space Radiance Cache Probe Interpolation & Importance Sampling' + NL +
    'float EvaluateScreenProbeBilateralWeight(vec3 pixelWorldPos, vec3 pixelNormal,' + NL +
    '  vec3 probeWorldPos, vec3 probeNormal, float planeDistSensitivity) {' + NL +
    '  float planeDist = abs(dot(pixelWorldPos - probeWorldPos, pixelNormal));' + NL +
    '  float normalWeight = pow(max(0.0, dot(pixelNormal, probeNormal)), 8.0);' + NL +
    '  float depthWeight = exp(-planeDist * max(0.0, planeDistSensitivity));' + NL +
    '  return normalWeight * depthWeight;' + NL +
    '}' + NL +
    NL +
    'vec3 CosineWeightedHemisphereSample(vec2 xi, out float outPdf) {' + NL +
    '  float phi = 6.28318530718 * xi.x;' + NL +
    '  float cosTheta = sqrt(clamp(1.0 - xi.y, 0.0, 1.0));' + NL +
    '  float sinTheta = sqrt(clamp(xi.y, 0.0, 1.0));' + NL +
    '  outPdf = cosTheta * 0.31830988618;' + NL +
    '  return vec3(cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateScreenProbeBilateralWeight(
  const APixelWorldPos, APixelNormal, AProbeWorldPos, AProbeNormal: TVector3;
  const APlaneDistSensitivity: Single): Single;
var
  PlaneDist, NormalDot, NormalWeight, DepthWeight: Single;
begin
  PlaneDist := Abs(TVector3.DotProduct(APixelWorldPos - AProbeWorldPos, APixelNormal));
  NormalDot := Max(0.0, TVector3.DotProduct(APixelNormal, AProbeNormal));
  NormalWeight := Math.Power(NormalDot, 8.0);
  DepthWeight := Exp(-PlaneDist * Max(0.0, APlaneDistSensitivity));
  Result := NormalWeight * DepthWeight;
end;

class function TCastleShaderGeneratorRHI.CosineWeightedHemisphereSample(
  const AXi: TVector2;
  out AOutPdf: Single): TVector3;
var
  Phi, CosTheta, SinTheta, ClampedY: Single;
begin
  Phi := 2.0 * Pi * AXi.X;
  ClampedY := EnsureRange(AXi.Y, 0.0, 1.0);
  CosTheta := Sqrt(1.0 - ClampedY);
  SinTheta := Sqrt(ClampedY);
  AOutPdf := CosTheta / Pi;
  Result := Vector3(Cos(Phi) * SinTheta, Sin(Phi) * SinTheta, CosTheta);
end;

function TCastleShaderGeneratorRHI.GenerateReSTIRReservoirFunctions: String;
begin
  Result :=
    '// Phase 29: ReSTIR DI/GI Spatio-Temporal Reservoir Resampling (UE5 MegaLights Architecture)' + NL +
    'struct CastleReSTIRReservoir {' + NL +
    '  int SelectedLightID;' + NL +
    '  float WeightSum;' + NL +
    '  float TargetPDF;' + NL +
    '  uint SampleCountM;' + NL +
    '  float ContributionWeightW;' + NL +
    '  uint Reserved0;' + NL +
    '  uint Reserved1;' + NL +
    '  uint Reserved2;' + NL +
    '};' + NL +
    NL +
    'float EvaluateLuminanceTargetPDF(vec3 unshadowedContribution) {' + NL +
    '  return max(0.0, dot(max(vec3(0.0), unshadowedContribution), vec3(0.2126, 0.7152, 0.0722)));' + NL +
    '}' + NL +
    NL +
    'bool UpdateReservoirSample(inout CastleReSTIRReservoir r, int candidateLightID,' + NL +
    '  float targetPDF, float sourcePDF, float random01) {' + NL +
    '  float wi = max(0.0, targetPDF) / max(1e-6, sourcePDF);' + NL +
    '  r.WeightSum += wi;' + NL +
    '  r.SampleCountM += 1u;' + NL +
    '  if (wi > 0.0 && (random01 * r.WeightSum < wi)) {' + NL +
    '    r.SelectedLightID = candidateLightID;' + NL +
    '    r.TargetPDF = max(0.0, targetPDF);' + NL +
    '    return true;' + NL +
    '  }' + NL +
    '  return false;' + NL +
    '}' + NL +
    NL +
    'void FinalizeReservoirWeight(inout CastleReSTIRReservoir r, uint maxHistoryM) {' + NL +
    '  r.SampleCountM = min(r.SampleCountM, max(1u, maxHistoryM));' + NL +
    '  float denom = r.TargetPDF * float(r.SampleCountM);' + NL +
    '  r.ContributionWeightW = (denom > 1e-6 && r.SelectedLightID >= 0) ? (r.WeightSum / denom) : 0.0;' + NL +
    '}' + NL +
    NL +
    'bool EvaluateReSTIRNeighborSimilarity(float centerDepth, vec3 centerNormal,' + NL +
    '  float neighborDepth, vec3 neighborNormal, float maxRelativeDepthDiff, float minNormalCos) {' + NL +
    '  float relDepth = abs(centerDepth - neighborDepth) / max(1e-4, abs(centerDepth));' + NL +
    '  float nDot = dot(centerNormal, neighborNormal);' + NL +
    '  return (relDepth <= maxRelativeDepthDiff) && (nDot >= minNormalCos);' + NL +
    '}' + NL +
    NL +
    'bool CombineReservoirs(inout CastleReSTIRReservoir dst, CastleReSTIRReservoir neighbor,' + NL +
    '  float neighborTargetPDFAtCenter, uint maxHistoryM, float random01) {' + NL +
    '  if (neighbor.SampleCountM == 0u || neighbor.SelectedLightID < 0) return false;' + NL +
    '  uint cappedM = min(neighbor.SampleCountM, max(1u, maxHistoryM));' + NL +
    '  float wi = max(0.0, neighborTargetPDFAtCenter) * max(0.0, neighbor.ContributionWeightW) * float(cappedM);' + NL +
    '  dst.WeightSum += wi;' + NL +
    '  dst.SampleCountM += cappedM;' + NL +
    '  bool accepted = false;' + NL +
    '  if (wi > 0.0 && (random01 * dst.WeightSum < wi)) {' + NL +
    '    dst.SelectedLightID = neighbor.SelectedLightID;' + NL +
    '    dst.TargetPDF = max(0.0, neighborTargetPDFAtCenter);' + NL +
    '    accepted = true;' + NL +
    '  }' + NL +
    '  FinalizeReservoirWeight(dst, maxHistoryM * 2u);' + NL +
    '  return accepted;' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GenerateReSTIRSpatialTemporalComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;',
      [Max(1, AWorkgroupSizeX), Max(1, AWorkgroupSizeY)]) + NL +
    NL +
    GenerateReSTIRReservoirFunctions + NL +
    'layout(std430, set = 0, binding = 0) buffer CastleCurrentReservoirBlock {' + NL +
    '  CastleReSTIRReservoir u_CurrentReservoirs[];' + NL +
    '};' + NL +
    NL +
    'layout(std430, set = 0, binding = 1) readonly buffer CastleHistoryReservoirBlock {' + NL +
    '  CastleReSTIRReservoir u_HistoryReservoirs[];' + NL +
    '};' + NL +
    NL +
    'void main() {' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.InitReSTIRReservoir(out AReservoir: TCastleReSTIRReservoir);
begin
  FillChar(AReservoir, SizeOf(TCastleReSTIRReservoir), 0);
  AReservoir.SelectedLightID := -1;
end;

class function TCastleShaderGeneratorRHI.EvaluateLuminanceTargetPDF(
  const AUnshadowedContribution: TVector3): Single;
begin
  Result := Max(0.0,
    Max(0.0, AUnshadowedContribution.X) * 0.2126 +
    Max(0.0, AUnshadowedContribution.Y) * 0.7152 +
    Max(0.0, AUnshadowedContribution.Z) * 0.0722);
end;

class function TCastleShaderGeneratorRHI.UpdateReservoirSample(
  var AReservoir: TCastleReSTIRReservoir;
  const ACandidateLightID: Integer;
  const ATargetPDF, ASourcePDF, ARandom01: Single): Boolean;
var
  Wi: Single;
begin
  Wi := Max(0.0, ATargetPDF) / Max(1e-6, ASourcePDF);
  AReservoir.WeightSum := AReservoir.WeightSum + Wi;
  Inc(AReservoir.SampleCountM);
  if (Wi > 0.0) and (EnsureRange(ARandom01, 0.0, 0.999999) * AReservoir.WeightSum < Wi) then
  begin
    AReservoir.SelectedLightID := ACandidateLightID;
    AReservoir.TargetPDF := Max(0.0, ATargetPDF);
    Result := True;
  end else
    Result := False;
end;

class procedure TCastleShaderGeneratorRHI.FinalizeReservoirWeight(
  var AReservoir: TCastleReSTIRReservoir;
  const AMaxHistoryM: Cardinal);
var
  Denom: Single;
begin
  AReservoir.SampleCountM := Min(AReservoir.SampleCountM, Max(Cardinal(1), AMaxHistoryM));
  Denom := AReservoir.TargetPDF * Single(AReservoir.SampleCountM);
  if (Denom > 1e-6) and (AReservoir.SelectedLightID >= 0) then
    AReservoir.ContributionWeightW := AReservoir.WeightSum / Denom
  else
    AReservoir.ContributionWeightW := 0.0;
end;

class function TCastleShaderGeneratorRHI.EvaluateReSTIRNeighborSimilarity(
  const ACenterDepth: Single;
  const ACenterNormal: TVector3;
  const ANeighborDepth: Single;
  const ANeighborNormal: TVector3;
  const AMaxRelativeDepthDiff: Single;
  const AMinNormalCos: Single): Boolean;
var
  RelDepth, NDot: Single;
begin
  RelDepth := Abs(ACenterDepth - ANeighborDepth) / Max(1e-4, Abs(ACenterDepth));
  NDot := TVector3.DotProduct(ACenterNormal, ANeighborNormal);
  Result := (RelDepth <= AMaxRelativeDepthDiff) and (NDot >= AMinNormalCos);
end;

class function TCastleShaderGeneratorRHI.CombineReservoirs(
  var ADst: TCastleReSTIRReservoir;
  const ANeighbor: TCastleReSTIRReservoir;
  const ANeighborTargetPDFAtCenter: Single;
  const AMaxHistoryM: Cardinal;
  const ARandom01: Single): Boolean;
var
  CappedM: Cardinal;
  Wi: Single;
begin
  if (ANeighbor.SampleCountM = 0) or (ANeighbor.SelectedLightID < 0) then
    Exit(False);
  CappedM := Min(ANeighbor.SampleCountM, Max(Cardinal(1), AMaxHistoryM));
  Wi := Max(0.0, ANeighborTargetPDFAtCenter) * Max(0.0, ANeighbor.ContributionWeightW) * Single(CappedM);
  ADst.WeightSum := ADst.WeightSum + Wi;
  Inc(ADst.SampleCountM, CappedM);
  if (Wi > 0.0) and (EnsureRange(ARandom01, 0.0, 0.999999) * ADst.WeightSum < Wi) then
  begin
    ADst.SelectedLightID := ANeighbor.SelectedLightID;
    ADst.TargetPDF := Max(0.0, ANeighborTargetPDFAtCenter);
    Result := True;
  end else
    Result := False;
  FinalizeReservoirWeight(ADst, Max(Cardinal(1), AMaxHistoryM * 2));
end;

function TCastleShaderGeneratorRHI.GenerateTSRUpscaleComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;',
      [Max(1, AWorkgroupSizeX), Max(1, AWorkgroupSizeY)]) + NL +
    NL +
    '// Phase 30: Temporal Super Resolution (TSR) Sub-Pixel History Upscaling & Disocclusion Anti-Ghosting' + NL +
    'float EvaluateLanczos2Weight(float x) {' + NL +
    '  float ax = abs(x);' + NL +
    '  if (ax < 1e-5) return 1.0;' + NL +
    '  if (ax >= 2.0) return 0.0;' + NL +
    '  float px = 3.14159265359 * ax;' + NL +
    '  return (sin(px) * sin(0.5 * px)) / (0.5 * px * px);' + NL +
    '}' + NL +
    NL +
    'float EvaluateBlackmanHarrisWeight(float x, float radius) {' + NL +
    '  float ax = abs(x);' + NL +
    '  float r = max(1e-4, radius);' + NL +
    '  if (ax >= r) return 0.0;' + NL +
    '  float t = 0.5 * (1.0 + x / r);' + NL +
    '  float p = 6.28318530718 * t;' + NL +
    '  return max(0.0, 0.35875 - 0.48829 * cos(p) + 0.14128 * cos(2.0 * p) - 0.01168 * cos(3.0 * p));' + NL +
    '}' + NL +
    NL +
    'float EvaluateTSRDisocclusionConfidence(float currentLinearDepth, float reprojectedLinearDepth, float maxRelDepthError) {' + NL +
    '  float relErr = abs(currentLinearDepth - reprojectedLinearDepth) / max(1e-4, abs(currentLinearDepth));' + NL +
    '  return clamp(1.0 - relErr / max(1e-4, maxRelDepthError), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'float EvaluateTSRThinGeometryBlend(float baseCurrentBlend, float depthCrossLaplacian, float lumaContrast) {' + NL +
    '  float thinFactor = clamp(abs(depthCrossLaplacian) * max(0.0, lumaContrast) * 4.0, 0.0, 1.0);' + NL +
    '  return mix(clamp(baseCurrentBlend, 0.01, 1.0), 0.85, thinFactor);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateLanczos2Weight(const AX: Single): Single;
var
  AXAbs, PX: Double;
begin
  AXAbs := Abs(AX);
  if AXAbs < 1e-5 then
    Exit(1.0);
  if AXAbs >= 2.0 then
    Exit(0.0);
  PX := Pi * AXAbs;
  Result := (Sin(PX) * Sin(0.5 * PX)) / (0.5 * PX * PX);
end;

class function TCastleShaderGeneratorRHI.EvaluateBlackmanHarrisWeight(
  const AX: Single;
  const ARadius: Single): Single;
var
  AXAbs, SafeR, T, P: Double;
begin
  AXAbs := Abs(AX);
  SafeR := Max(1e-4, ARadius);
  if AXAbs >= SafeR then
    Exit(0.0);
  T := 0.5 * (1.0 + AX / SafeR);
  P := 2.0 * Pi * T;
  Result := Max(0.0, 0.35875 - 0.48829 * Cos(P) + 0.14128 * Cos(2.0 * P) - 0.01168 * Cos(3.0 * P));
end;

class function TCastleShaderGeneratorRHI.EvaluateTSRDisocclusionConfidence(
  const ACurrentLinearDepth, AReprojectedLinearDepth: Single;
  const AMaxRelativeDepthError: Single): Single;
var
  RelErr: Single;
begin
  RelErr := Abs(ACurrentLinearDepth - AReprojectedLinearDepth) / Max(1e-4, Abs(ACurrentLinearDepth));
  Result := EnsureRange(1.0 - RelErr / Max(1e-4, AMaxRelativeDepthError), 0.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.EvaluateTSRThinGeometryBlend(
  const ABaseCurrentFrameBlend, ADepthCrossLaplacian, ALumaContrast: Single): Single;
var
  BaseBlend, ThinFactor: Single;
begin
  BaseBlend := EnsureRange(ABaseCurrentFrameBlend, 0.01, 1.0);
  ThinFactor := EnsureRange(Abs(ADepthCrossLaplacian) * Max(0.0, ALumaContrast) * 4.0, 0.0, 1.0);
  Result := BaseBlend + (0.85 - BaseBlend) * ThinFactor;
end;

function TCastleShaderGeneratorRHI.GenerateSubstrateLayeredMaterialFunctions: String;
begin
  Result :=
    '// Phase 31.1: UE5 Substrate / Strata Energy-Conserving Layered Material Slabs' + NL +
    'float EvaluateDirectionalSlabAlbedo(float f0, float nDotV, float roughness) {' + NL +
    '  float mu = clamp(nDotV, 0.0, 1.0);' + NL +
    '  float r = clamp(roughness, 0.0, 1.0);' + NL +
    '  float f90 = max(f0, 1.0 - r);' + NL +
    '  return clamp(f0 + (f90 - f0) * pow(1.0 - mu, 5.0), 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateLayeredSlabComposition(vec3 topLobeRadiance, vec3 bottomLobeRadiance,' + NL +
    '  vec3 extinctionSigmaT, float topF0, float topRoughness, float nDotV, float nDotL, float slabThickness) {' + NL +
    '  float eo = EvaluateDirectionalSlabAlbedo(topF0, nDotV, topRoughness);' + NL +
    '  float ei = EvaluateDirectionalSlabAlbedo(topF0, nDotL, topRoughness);' + NL +
    '  float opticalPath = max(0.0, slabThickness) * (1.0 / max(0.1, nDotV) + 1.0 / max(0.1, nDotL));' + NL +
    '  vec3 mediumTransmittance = exp(-max(vec3(0.0), extinctionSigmaT) * opticalPath);' + NL +
    '  vec3 bottomWeight = (1.0 - eo) * (1.0 - ei) * mediumTransmittance;' + NL +
    '  return max(vec3(0.0), topLobeRadiance) + bottomWeight * max(vec3(0.0), bottomLobeRadiance);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateDirectionalSlabAlbedo(
  const AF0, ANdotV, ARoughness: Single): Single;
var
  Mu, R, F90: Single;
begin
  Mu := EnsureRange(ANdotV, 0.0, 1.0);
  R := EnsureRange(ARoughness, 0.0, 1.0);
  F90 := Max(AF0, 1.0 - R);
  Result := EnsureRange(AF0 + (F90 - AF0) * Math.Power(1.0 - Mu, 5.0), 0.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.EvaluateLayeredSlabComposition(
  const ATopLobeRadiance, ABottomLobeRadiance, AExtinctionSigmaT: TVector3;
  const ATopF0, ATopRoughness, ANdotV, ANdotL, ASlabThickness: Single): TVector3;
var
  Eo, Ei, OpticalPath, EnergyTransmit: Single;
  MediumT: TVector3;
begin
  Eo := EvaluateDirectionalSlabAlbedo(ATopF0, ANdotV, ATopRoughness);
  Ei := EvaluateDirectionalSlabAlbedo(ATopF0, ANdotL, ATopRoughness);
  OpticalPath := Max(0.0, ASlabThickness) * (1.0 / Max(0.1, ANdotV) + 1.0 / Max(0.1, ANdotL));
  MediumT := Vector3(
    Exp(-Max(0.0, AExtinctionSigmaT.X) * OpticalPath),
    Exp(-Max(0.0, AExtinctionSigmaT.Y) * OpticalPath),
    Exp(-Max(0.0, AExtinctionSigmaT.Z) * OpticalPath));
  EnergyTransmit := (1.0 - Eo) * (1.0 - Ei);
  Result := Vector3(
    Max(0.0, ATopLobeRadiance.X) + EnergyTransmit * MediumT.X * Max(0.0, ABottomLobeRadiance.X),
    Max(0.0, ATopLobeRadiance.Y) + EnergyTransmit * MediumT.Y * Max(0.0, ABottomLobeRadiance.Y),
    Max(0.0, ATopLobeRadiance.Z) + EnergyTransmit * MediumT.Z * Max(0.0, ABottomLobeRadiance.Z));
end;

function TCastleShaderGeneratorRHI.GenerateHairMarschnerBSDF: String;
begin
  Result :=
    '// Phase 31.2: Marschner / Chiang Dual-Specular Hair & Fur Fiber BSDF' + NL +
    'float EvaluateMarschnerLongitudinalM(float sinThetaI, float sinThetaO,' + NL +
    '  float cuticleShiftRad, float longitudinalRoughnessBeta) {' + NL +
    '  float thetaI = asin(clamp(sinThetaI, -1.0, 1.0));' + NL +
    '  float thetaO = asin(clamp(sinThetaO, -1.0, 1.0));' + NL +
    '  float thetaH = 0.5 * (thetaI + thetaO) - cuticleShiftRad;' + NL +
    '  float beta = max(0.02, longitudinalRoughnessBeta);' + NL +
    '  return (0.39894228 / beta) * exp(-0.5 * (thetaH * thetaH) / (beta * beta));' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateHairDualSpecularBSDF(vec3 hairAbsorptionSigmaA, float sinThetaI, float sinThetaO,' + NL +
    '  float cosPhiD, float cuticleTiltRad, float primaryRoughnessBeta) {' + NL +
    '  float beta = max(0.02, primaryRoughnessBeta);' + NL +
    '  float mR   = EvaluateMarschnerLongitudinalM(sinThetaI, sinThetaO, -2.0 * cuticleTiltRad, beta);' + NL +
    '  float mTT  = EvaluateMarschnerLongitudinalM(sinThetaI, sinThetaO,  1.0 * cuticleTiltRad, 0.75 * beta);' + NL +
    '  float mTRT = EvaluateMarschnerLongitudinalM(sinThetaI, sinThetaO,  4.0 * cuticleTiltRad, 1.50 * beta);' + NL +
    '  float nR   = 0.25 * max(0.0, 0.5 + 0.5 * cosPhiD);' + NL +
    '  float nTT  = 0.50 * pow(max(0.0, 0.5 - 0.5 * cosPhiD), 2.0);' + NL +
    '  float nTRT = 0.35 * max(0.0, 0.5 + 0.5 * cosPhiD);' + NL +
    '  vec3 transTT  = exp(-max(vec3(0.0), hairAbsorptionSigmaA) * 1.0);' + NL +
    '  vec3 transTRT = exp(-max(vec3(0.0), hairAbsorptionSigmaA) * 2.0);' + NL +
    '  return vec3(0.04 * mR * nR) + transTT * (mTT * nTT) + transTRT * (mTRT * nTRT);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateMarschnerLongitudinalM(
  const ASinThetaI, ASinThetaO, ACuticleShiftRad, ALongitudinalRoughnessBeta: Single): Single;
var
  ThetaI, ThetaO, ThetaH, Beta: Double;
begin
  ThetaI := ArcSin(EnsureRange(ASinThetaI, -1.0, 1.0));
  ThetaO := ArcSin(EnsureRange(ASinThetaO, -1.0, 1.0));
  ThetaH := 0.5 * (ThetaI + ThetaO) - ACuticleShiftRad;
  Beta := Max(0.02, ALongitudinalRoughnessBeta);
  Result := (0.3989422804014327 / Beta) * Exp(-0.5 * (ThetaH * ThetaH) / (Beta * Beta));
end;

class function TCastleShaderGeneratorRHI.EvaluateHairDualSpecularBSDF(
  const AHairAbsorptionSigmaA: TVector3;
  const ASinThetaI, ASinThetaO, ACosPhiD, ACuticleTiltRad, APrimaryRoughnessBeta: Single): TVector3;
var
  Beta, MR, MTT, MTRT, NR, NTT, NTRT, SpecR: Single;
  TransTT, TransTRT: TVector3;
begin
  Beta := Max(0.02, APrimaryRoughnessBeta);
  MR   := EvaluateMarschnerLongitudinalM(ASinThetaI, ASinThetaO, -2.0 * ACuticleTiltRad, Beta);
  MTT  := EvaluateMarschnerLongitudinalM(ASinThetaI, ASinThetaO,  1.0 * ACuticleTiltRad, 0.75 * Beta);
  MTRT := EvaluateMarschnerLongitudinalM(ASinThetaI, ASinThetaO,  4.0 * ACuticleTiltRad, 1.50 * Beta);
  NR   := 0.25 * Max(0.0, 0.5 + 0.5 * ACosPhiD);
  NTT  := 0.50 * Sqr(Max(0.0, 0.5 - 0.5 * ACosPhiD));
  NTRT := 0.35 * Max(0.0, 0.5 + 0.5 * ACosPhiD);
  TransTT := Vector3(
    Exp(-Max(0.0, AHairAbsorptionSigmaA.X) * 1.0),
    Exp(-Max(0.0, AHairAbsorptionSigmaA.Y) * 1.0),
    Exp(-Max(0.0, AHairAbsorptionSigmaA.Z) * 1.0));
  TransTRT := Vector3(
    Exp(-Max(0.0, AHairAbsorptionSigmaA.X) * 2.0),
    Exp(-Max(0.0, AHairAbsorptionSigmaA.Y) * 2.0),
    Exp(-Max(0.0, AHairAbsorptionSigmaA.Z) * 2.0));
  SpecR := 0.04 * MR * NR;
  Result := Vector3(
    SpecR + TransTT.X * (MTT * NTT) + TransTRT.X * (MTRT * NTRT),
    SpecR + TransTT.Y * (MTT * NTT) + TransTRT.Y * (MTRT * NTRT),
    SpecR + TransTT.Z * (MTT * NTT) + TransTRT.Z * (MTRT * NTRT));
end;

function TCastleShaderGeneratorRHI.GenerateStochasticGlintFunctions: String;
begin
  Result :=
    '// Phase 31.3: Stochastic Discrete Microfacet Glints & Sparkles' + NL +
    'float EvaluateStochasticGlintNDF(float nDotH, float macroRoughness,' + NL +
    '  float flakeAlignment01, float glintDensity, float glintSharpness) {' + NL +
    '  float a = max(0.02, macroRoughness * macroRoughness);' + NL +
    '  float a2 = a * a;' + NL +
    '  float nh = clamp(nDotH, 0.0, 1.0);' + NL +
    '  float denom = nh * nh * (a2 - 1.0) + 1.0;' + NL +
    '  float dMacro = a2 / max(1e-6, 3.14159265 * denom * denom);' + NL +
    '  float glintSpike = pow(clamp(flakeAlignment01, 0.0, 1.0), max(1.0, glintSharpness)) / max(0.01, glintDensity);' + NL +
    '  return dMacro * (1.0 + glintSpike);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateStochasticGlintNDF(
  const ANdotH, AMacroRoughness, AFlakeAlignment01, AGlintDensity, AGlintSharpness: Single): Single;
var
  A, A2, NH, Denom, DMacro, GlintSpike: Single;
begin
  A := Max(0.02, AMacroRoughness * AMacroRoughness);
  A2 := A * A;
  NH := EnsureRange(ANdotH, 0.0, 1.0);
  Denom := NH * NH * (A2 - 1.0) + 1.0;
  DMacro := A2 / Max(1e-6, Pi * Denom * Denom);
  GlintSpike := Math.Power(EnsureRange(AFlakeAlignment01, 0.0, 1.0), Max(1.0, AGlintSharpness)) / Max(0.01, AGlintDensity);
  Result := DMacro * (1.0 + GlintSpike);
end;

function TCastleShaderGeneratorRHI.GenerateMeshShaderStageFunctions(
  const AMaxVertices, AMaxPrimitives, AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    '#extension GL_EXT_mesh_shader : require' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;', [Max(1, AWorkgroupSizeX)]) + NL +
    Format('layout(triangles, max_vertices = %d, max_primitives = %d) out;',
      [Max(3, AMaxVertices), Max(1, AMaxPrimitives)]) + NL +
    NL +
    'void EmitMeshletOutputCounts(uint vertexCount, uint primitiveCount) {' + NL +
    '  SetMeshOutputsEXT(vertexCount, primitiveCount);' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '  EmitMeshletOutputCounts(0u, 0u);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.ComputeMeshletTaskGroupCount(
  const ATotalMeshletCount: Cardinal;
  const AMeshletsPerTaskGroup: Cardinal): Cardinal;
var
  GroupSize: Cardinal;
begin
  if ATotalMeshletCount = 0 then
    Exit(0);
  GroupSize := Max(Cardinal(1), AMeshletsPerTaskGroup);
  Result := (ATotalMeshletCount + GroupSize - 1) div GroupSize;
end;

function TCastleShaderGeneratorRHI.GenerateVRSContrastComputeShader(
  const ATileSize: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;',
      [Max(1, ATileSize), Max(1, ATileSize)]) + NL +
    NL +
    '// Phase 32.2: VK_KHR_fragment_shading_rate Tile-Based Variable Rate Shading Classifier' + NL +
    'uint SelectOptimalShadingRateTile(float lumaContrastX, float lumaContrastY,' + NL +
    '  float velocityPixels, float highContrastThreshold, float lowContrastThreshold) {' + NL +
    '  float maxContrast = max(lumaContrastX, lumaContrastY);' + NL +
    '  if (maxContrast >= highContrastThreshold && velocityPixels < 4.0) return 0u;' + NL +
    '  if (maxContrast < lowContrastThreshold && velocityPixels >= 8.0) return 10u;' + NL +
    '  if (maxContrast < lowContrastThreshold) return 5u;' + NL +
    '  if (lumaContrastX < lumaContrastY * 0.5) return 1u;' + NL +
    '  if (lumaContrastY < lumaContrastX * 0.5) return 4u;' + NL +
    '  return 5u;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.SelectOptimalShadingRateTile(
  const ALumaContrastX, ALumaContrastY, AVelocityPixels: Single;
  const AHighContrastThreshold: Single;
  const ALowContrastThreshold: Single): Cardinal;
var
  MaxContrast: Single;
begin
  MaxContrast := Max(ALumaContrastX, ALumaContrastY);
  if (MaxContrast >= AHighContrastThreshold) and (AVelocityPixels < 4.0) then
    Exit(0); // 1x1 full shading rate
  if (MaxContrast < ALowContrastThreshold) and (AVelocityPixels >= 8.0) then
    Exit(10); // 4x4 (1/16th rate for fast-moving low-contrast regions)
  if MaxContrast < ALowContrastThreshold then
    Exit(5); // 2x2 quarter rate
  if ALumaContrastX < ALumaContrastY * 0.5 then
    Exit(1); // 2x1 coarse X, fine Y
  if ALumaContrastY < ALumaContrastX * 0.5 then
    Exit(4); // 1x2 fine X, coarse Y
  Result := 5; // 2x2 default mid-contrast rate
end;

function TCastleShaderGeneratorRHI.GenerateRVTLookupFunctions: String;
begin
  Result :=
    '// Phase 32.3: Runtime Virtual Texturing (RVT) Page Table & Physical Tile Cache Lookup' + NL +
    'vec2 TranslateRVTPageToPhysicalUV(vec2 inPageUV, uint physicalTileX, uint physicalTileY,' + NL +
    '  uint physicalTilesPerRow, float borderTexelFraction) {' + NL +
    '  float invTiles = 1.0 / max(1.0, float(physicalTilesPerRow));' + NL +
    '  float innerScale = max(0.0, 1.0 - 2.0 * borderTexelFraction);' + NL +
    '  vec2 paddedUV = vec2(borderTexelFraction) + clamp(inPageUV, vec2(0.0), vec2(1.0)) * innerScale;' + NL +
    '  return (vec2(float(physicalTileX), float(physicalTileY)) + paddedUV) * invTiles;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.TranslateRVTPageToPhysicalUV(
  const AInPageUV: TVector2;
  const APhysicalTileX, APhysicalTileY, APhysicalTilesPerRow: Cardinal;
  const ABorderTexelFraction: Single): TVector2;
var
  InvTiles, InnerScale, PaddedU, PaddedV: Single;
begin
  InvTiles := 1.0 / Max(1.0, Single(APhysicalTilesPerRow));
  InnerScale := Max(0.0, 1.0 - 2.0 * ABorderTexelFraction);
  PaddedU := ABorderTexelFraction + EnsureRange(AInPageUV.X, 0.0, 1.0) * InnerScale;
  PaddedV := ABorderTexelFraction + EnsureRange(AInPageUV.Y, 0.0, 1.0) * InnerScale;
  Result := Vector2(
    (Single(APhysicalTileX) + PaddedU) * InvTiles,
    (Single(APhysicalTileY) + PaddedV) * InvTiles);
end;

function TCastleShaderGeneratorRHI.GenerateSingleLayerWaterFunctions: String;
begin
  Result :=
    '// Phase 33.1: Physically Based Single-Layer Water (Beer-Lambert Absorption, Snell Refraction & Caustics)' + NL +
    'vec3 EvaluateWaterBeerLambertAbsorption(vec3 absorptionCoeffRGB, float opticalDepthMeters) {' + NL +
    '  float d = max(0.0, opticalDepthMeters);' + NL +
    '  return exp(-max(absorptionCoeffRGB, vec3(0.0)) * d);' + NL +
    '}' + NL +
    NL +
    'vec2 ComputeWaterRefractionUVOffset(vec3 viewSpaceNormal, float waterDepthMeters,' + NL +
    '  float iorRatio, float refractionStrength) {' + NL +
    '  float shorelineFade = clamp(max(0.0, waterDepthMeters) * 0.5, 0.0, 1.0);' + NL +
    '  float etaFactor = max(0.0, 1.0 - iorRatio);' + NL +
    '  return viewSpaceNormal.xy * (etaFactor * refractionStrength * shorelineFade);' + NL +
    '}' + NL +
    NL +
    'float EvaluateWaterCausticsPattern(vec2 worldXZ, float timeSeconds, float waterDepthMeters) {' + NL +
    '  float d = max(0.0, waterDepthMeters);' + NL +
    '  float depthEnvelope = clamp(d * 2.0, 0.0, 1.0) * exp(-d * 0.15);' + NL +
    '  vec2 uv1 = worldXZ * 1.7 + vec2(timeSeconds * 0.8, -timeSeconds * 0.6);' + NL +
    '  vec2 uv2 = worldXZ * 2.3 + vec2(-timeSeconds * 0.7, timeSeconds * 0.9);' + NL +
    '  float wave1 = sin(uv1.x) * cos(uv1.y);' + NL +
    '  float wave2 = sin(uv2.x + 1.57) * cos(uv2.y - 0.78);' + NL +
    '  float focus = pow(max(0.0, 0.5 + 0.5 * (wave1 + wave2) * 0.5), 4.0);' + NL +
    '  return focus * depthEnvelope;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateWaterBeerLambertAbsorption(
  const AAbsorptionCoeffRGB: TVector3;
  const AOpticalDepthMeters: Single): TVector3;
var
  D: Single;
begin
  D := Max(0.0, AOpticalDepthMeters);
  Result := Vector3(
    Exp(-Max(0.0, AAbsorptionCoeffRGB.X) * D),
    Exp(-Max(0.0, AAbsorptionCoeffRGB.Y) * D),
    Exp(-Max(0.0, AAbsorptionCoeffRGB.Z) * D));
end;

class function TCastleShaderGeneratorRHI.ComputeWaterRefractionUVOffset(
  const AViewSpaceNormal: TVector3;
  const AWaterDepthMeters, AIORRatio, ARefractionStrength: Single): TVector2;
var
  ShorelineFade, EtaFactor, Scale: Single;
begin
  ShorelineFade := EnsureRange(Max(0.0, AWaterDepthMeters) * 0.5, 0.0, 1.0);
  EtaFactor := Max(0.0, 1.0 - AIORRatio);
  Scale := EtaFactor * ARefractionStrength * ShorelineFade;
  Result := Vector2(AViewSpaceNormal.X * Scale, AViewSpaceNormal.Y * Scale);
end;

class function TCastleShaderGeneratorRHI.EvaluateWaterCausticsPattern(
  const AWorldXZ: TVector2;
  const ATimeSeconds, AWaterDepthMeters: Single): Single;
var
  D, DepthEnvelope, Wave1, Wave2, Focus: Single;
  UV1, UV2: TVector2;
begin
  D := Max(0.0, AWaterDepthMeters);
  DepthEnvelope := EnsureRange(D * 2.0, 0.0, 1.0) * Exp(-D * 0.15);
  UV1 := Vector2(AWorldXZ.X * 1.7 + ATimeSeconds * 0.8, AWorldXZ.Y * 1.7 - ATimeSeconds * 0.6);
  UV2 := Vector2(AWorldXZ.X * 2.3 - ATimeSeconds * 0.7, AWorldXZ.Y * 2.3 + ATimeSeconds * 0.9);
  Wave1 := Sin(UV1.X) * Cos(UV1.Y);
  Wave2 := Sin(UV2.X + 1.57) * Cos(UV2.Y - 0.78);
  Focus := Power(Max(0.0, 0.5 + 0.25 * (Wave1 + Wave2)), 4.0);
  Result := Focus * DepthEnvelope;
end;

function TCastleShaderGeneratorRHI.GenerateSparseVolumeBlackbodyFunctions: String;
begin
  Result :=
    '// Phase 33.2: Sparse Brick-Atlas Heterogeneous Volumes & Planckian Blackbody Emission' + NL +
    'vec3 EvaluateBlackbodyEmissionRGB(float temperatureKelvin, float intensity) {' + NL +
    '  if (temperatureKelvin < 500.0) return vec3(0.0);' + NL +
    '  float t = clamp((temperatureKelvin - 800.0) / 5200.0, 0.0, 2.0);' + NL +
    '  float r = clamp(1.5 * t + 0.2, 0.0, 1.0);' + NL +
    '  float g = clamp(1.2 * t - 0.25, 0.0, 1.0);' + NL +
    '  float b = clamp(1.4 * t - 0.85, 0.0, 1.0);' + NL +
    '  return vec3(r, g, b) * max(0.0, intensity);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateBlackbodyEmissionRGB(
  const ATemperatureKelvin: Single;
  const AIntensity: Single): TVector3;
var
  T, R, G, B, SafeIntensity: Single;
begin
  if ATemperatureKelvin < 500.0 then
    Exit(TVector3.Zero);
  T := EnsureRange((ATemperatureKelvin - 800.0) / 5200.0, 0.0, 2.0);
  R := EnsureRange(1.5 * T + 0.2, 0.0, 1.0);
  G := EnsureRange(1.2 * T - 0.25, 0.0, 1.0);
  B := EnsureRange(1.4 * T - 0.85, 0.0, 1.0);
  SafeIntensity := Max(0.0, AIntensity);
  Result := Vector3(R * SafeIntensity, G * SafeIntensity, B * SafeIntensity);
end;

function TCastleShaderGeneratorRHI.GenerateSVGFVarianceMomentsFunctions: String;
begin
  Result :=
    '// Phase 34.1: Spatio-Temporal Variance-Guided Filtering (SVGF) Luminance Moments & Variance' + NL +
    'vec2 EvaluateLuminanceMoments(vec3 radianceRGB) {' + NL +
    '  float luma = dot(max(radianceRGB, vec3(0.0)), vec3(0.2126, 0.7152, 0.0722));' + NL +
    '  return vec2(luma, luma * luma);' + NL +
    '}' + NL +
    NL +
    'float ComputeTemporalVariance(vec2 moments) {' + NL +
    '  return max(0.0, moments.y - moments.x * moments.x);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateLuminanceMoments(
  const ARadianceRGB: TVector3): TVector2;
var
  Luma: Single;
begin
  Luma := Max(0.0, ARadianceRGB.X) * 0.2126 +
          Max(0.0, ARadianceRGB.Y) * 0.7152 +
          Max(0.0, ARadianceRGB.Z) * 0.0722;
  Result := Vector2(Luma, Luma * Luma);
end;

class function TCastleShaderGeneratorRHI.ComputeTemporalVariance(
  const AMoments: TVector2): Single;
begin
  Result := Max(0.0, AMoments.Y - Sqr(AMoments.X));
end;

function TCastleShaderGeneratorRHI.GenerateSVGFAttrousWaveletFunctions: String;
begin
  Result :=
    '// Phase 34.2 & 34.3: SVGF Edge-Stopping A-Trous Wavelet Filter & Specular Hit-Distance Contact Hardening' + NL +
    'float EvaluateSVGFEdgeStoppingWeight(vec3 centerNormal, vec3 sampleNormal,' + NL +
    '  float centerDepth, float sampleDepth, float depthGradientDotOffset,' + NL +
    '  float centerLuma, float sampleLuma, float variance,' + NL +
    '  float phiDepth, float phiNormal, float phiLuma) {' + NL +
    '  float wNormal = pow(clamp(dot(centerNormal, sampleNormal), 0.0, 1.0), max(1.0, phiNormal));' + NL +
    '  float depthDenom = max(1e-5, abs(depthGradientDotOffset) * max(1e-4, phiDepth) + 1e-4);' + NL +
    '  float wDepth = exp(-abs(centerDepth - sampleDepth) / depthDenom);' + NL +
    '  float lumaDenom = max(1e-5, phiLuma * sqrt(max(0.0, variance) + 1e-6));' + NL +
    '  float wLuma = exp(-abs(centerLuma - sampleLuma) / lumaDenom);' + NL +
    '  return clamp(wNormal * wDepth * wLuma, 0.0, 1.0);' + NL +
    '}' + NL +
    NL +
    'float EvaluateHitDistanceBlurFactor(float rayHitDistance, float cameraDistance, float roughness) {' + NL +
    '  float dHit = max(0.0, rayHitDistance);' + NL +
    '  float dCam = max(1e-4, cameraDistance);' + NL +
    '  float contactRatio = dHit / (dHit + dCam);' + NL +
    '  return clamp(contactRatio * clamp(roughness, 0.0, 1.0), 0.0, 1.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSVGFEdgeStoppingWeight(
  const ACenterNormal, ASampleNormal: TVector3;
  const ACenterDepth, ASampleDepth, ADepthGradientDotOffset: Single;
  const ACenterLuma, ASampleLuma, AVariance: Single;
  const APhiDepth: Single;
  const APhiNormal: Single;
  const APhiLuma: Single): Single;
var
  WNormal, DepthDenom, WDepth, LumaDenom, WLuma: Single;
begin
  WNormal := Power(EnsureRange(TVector3.DotProduct(ACenterNormal, ASampleNormal), 0.0, 1.0),
    Max(1.0, APhiNormal));
  DepthDenom := Max(1e-5, Abs(ADepthGradientDotOffset) * Max(1e-4, APhiDepth) + 1e-4);
  WDepth := Exp(-Abs(ACenterDepth - ASampleDepth) / DepthDenom);
  LumaDenom := Max(1e-5, APhiLuma * Sqrt(Max(0.0, AVariance) + 1e-6));
  WLuma := Exp(-Abs(ACenterLuma - ASampleLuma) / LumaDenom);
  Result := EnsureRange(WNormal * WDepth * WLuma, 0.0, 1.0);
end;

class function TCastleShaderGeneratorRHI.EvaluateHitDistanceBlurFactor(
  const ARayHitDistance, ACameraDistance, ARoughness: Single): Single;
var
  DHit, DCam, ContactRatio: Single;
begin
  DHit := Max(0.0, ARayHitDistance);
  DCam := Max(1e-4, ACameraDistance);
  ContactRatio := DHit / (DHit + DCam);
  Result := EnsureRange(ContactRatio * EnsureRange(ARoughness, 0.0, 1.0), 0.0, 1.0);
end;

function TCastleShaderGeneratorRHI.GenerateRayTracingPipelineStages: String;
begin
  Result :=
    '#version 460' + NL +
    '#extension GL_EXT_ray_tracing : require' + NL +
    NL +
    '// Phase 35.1: Hardware Ray Tracing Pipeline (VK_KHR_ray_tracing_pipeline)' + NL +
    'struct CastleRayPayload {' + NL +
    '  vec3 radiance;' + NL +
    '  float hitT;' + NL +
    '};' + NL +
    NL +
    'vec3 InterpolateRayHitNormal(vec3 n0, vec3 n1, vec3 n2, vec2 hitBaryUV) {' + NL +
    '  float w0 = 1.0 - hitBaryUV.x - hitBaryUV.y;' + NL +
    '  return normalize(n0 * w0 + n1 * hitBaryUV.x + n2 * hitBaryUV.y);' + NL +
    '}' + NL +
    NL +
    '// traceRayEXT(topLevelAS, rayFlags, cullMask, sbtRecordOffset, sbtRecordStride, missIndex, origin, tMin, dir, tMax, payloadLocation);' + NL;
end;

class function TCastleShaderGeneratorRHI.InterpolateRayHitNormal(
  const AN0, AN1, AN2: TVector3;
  const AHitBarycentricsUV: TVector2): TVector3;
var
  W0: Single;
begin
  W0 := 1.0 - AHitBarycentricsUV.X - AHitBarycentricsUV.Y;
  Result := (AN0 * W0) + (AN1 * AHitBarycentricsUV.X) + (AN2 * AHitBarycentricsUV.Y);
  if Result.LengthSqr > 1e-12 then
    Result := Result.Normalize
  else
    Result := Vector3(0, 0, 1);
end;

class function TCastleShaderGeneratorRHI.AlignUpSBTSize(
  const AValue, AAlignment: Cardinal): Cardinal;
var
  SafeAlign: Cardinal;
begin
  SafeAlign := Max(Cardinal(1), AAlignment);
  if AValue = 0 then
    Exit(0);
  Result := ((AValue + SafeAlign - 1) div SafeAlign) * SafeAlign;
end;

function TCastleShaderGeneratorRHI.GenerateXPBDClothComputeShader(
  const AWorkgroupSizeX: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = 1, local_size_z = 1) in;',
      [Max(1, AWorkgroupSizeX)]) + NL +
    NL +
    '// Phase 36.1 & 36.2: GPU XPBD Cloth/Hair Constraints, Aerodynamic Wind & Sphere SDF Collision' + NL +
    'vec3 EvaluateXPBDDistanceConstraintDelta(vec3 p1, vec3 p2, float invMass1, float invMass2,' + NL +
    '  float restLength, float complianceAlpha, float substepDt) {' + NL +
    '  vec3 diff = p1 - p2;' + NL +
    '  float dist = length(diff);' + NL +
    '  if (dist < 1e-6) return vec3(0.0);' + NL +
    '  float wSum = invMass1 + invMass2;' + NL +
    '  if (wSum <= 0.0) return vec3(0.0);' + NL +
    '  float dtSafe = max(1e-5, substepDt);' + NL +
    '  float alphaTilde = max(0.0, complianceAlpha) / (dtSafe * dtSafe);' + NL +
    '  float c = dist - restLength;' + NL +
    '  float dLambda = -c / (wSum + alphaTilde);' + NL +
    '  return (diff / dist) * (invMass1 * dLambda);' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateAerodynamicWindForce(vec3 windVel, vec3 elemVel, vec3 elemNormal,' + NL +
    '  float elemArea, float dragCoeff, float airDensity) {' + NL +
    '  vec3 relVel = windVel - elemVel;' + NL +
    '  float vDotN = dot(relVel, elemNormal);' + NL +
    '  float pressure = 0.5 * max(0.0, airDensity) * max(0.0, elemArea) * max(0.0, dragCoeff) * abs(vDotN) * vDotN;' + NL +
    '  return elemNormal * pressure;' + NL +
    '}' + NL +
    NL +
    'vec3 ResolveSphereCollisionConstraint(vec3 particlePos, vec3 sphereCenter, float sphereRadius, float margin) {' + NL +
    '  vec3 toParticle = particlePos - sphereCenter;' + NL +
    '  float dist = length(toParticle);' + NL +
    '  float minRadius = max(0.0, sphereRadius + margin);' + NL +
    '  if (dist >= minRadius) return particlePos;' + NL +
    '  vec3 dir = (dist > 1e-6) ? (toParticle / dist) : vec3(0.0, 1.0, 0.0);' + NL +
    '  return sphereCenter + dir * minRadius;' + NL +
    '}' + NL +
    NL +
    'void main() {' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateXPBDDistanceConstraintDelta(
  const AP1, AP2: TVector3;
  const AInvMass1, AInvMass2, ARestLength, AComplianceAlpha, ASubstepDt: Single): TVector3;
var
  Diff: TVector3;
  Dist, WSum, DtSafe, AlphaTilde, C, DLambda: Single;
begin
  Diff := AP1 - AP2;
  Dist := Diff.Length;
  if Dist < 1e-6 then
    Exit(TVector3.Zero);
  WSum := Max(0.0, AInvMass1) + Max(0.0, AInvMass2);
  if WSum <= 0.0 then
    Exit(TVector3.Zero);
  DtSafe := Max(1e-5, ASubstepDt);
  AlphaTilde := Max(0.0, AComplianceAlpha) / Sqr(DtSafe);
  C := Dist - ARestLength;
  DLambda := -C / (WSum + AlphaTilde);
  Result := Diff * ((Max(0.0, AInvMass1) * DLambda) / Dist);
end;

class function TCastleShaderGeneratorRHI.EvaluateAerodynamicWindForce(
  const AWindVelocity, AElementVelocity, AElementNormal: TVector3;
  const AElementArea, ADragCoeff: Single;
  const AAirDensity: Single): TVector3;
var
  RelVel: TVector3;
  VDotN, Pressure: Single;
begin
  RelVel := AWindVelocity - AElementVelocity;
  VDotN := TVector3.DotProduct(RelVel, AElementNormal);
  Pressure := 0.5 * Max(0.0, AAirDensity) * Max(0.0, AElementArea) *
    Max(0.0, ADragCoeff) * Abs(VDotN) * VDotN;
  Result := AElementNormal * Pressure;
end;

class function TCastleShaderGeneratorRHI.ResolveSphereCollisionConstraint(
  const AParticlePos, ASphereCenter: TVector3;
  const ASphereRadius: Single;
  const ACollisionMargin: Single): TVector3;
var
  ToParticle, Dir: TVector3;
  Dist, MinRadius: Single;
begin
  ToParticle := AParticlePos - ASphereCenter;
  Dist := ToParticle.Length;
  MinRadius := Max(0.0, ASphereRadius + ACollisionMargin);
  if Dist >= MinRadius then
    Exit(AParticlePos);
  if Dist > 1e-6 then
    Dir := ToParticle * (1.0 / Dist)
  else
    Dir := Vector3(0, 1, 0);
  Result := ASphereCenter + (Dir * MinRadius);
end;

function TCastleShaderGeneratorRHI.GenerateMultiviewStereoPreamble: String;
begin
  Result :=
    '#extension GL_EXT_multiview : enable' + NL +
    NL +
    '// Phase 37.1 & 37.2: Single-Pass Stereo (VK_KHR_multiview) & Gaze-Tracked Radial Foveated VRS' + NL +
    'vec2 ComputeAsymmetricEyeProjectionXScaleOffset(float tanLeft, float tanRight) {' + NL +
    '  float span = max(1e-5, tanRight - tanLeft);' + NL +
    '  return vec2(2.0 / span, (tanRight + tanLeft) / span);' + NL +
    '}' + NL +
    NL +
    'uint EvaluateFoveatedShadingRate(vec2 uv, vec2 gazeCenterUV, float innerFoveaRadius, float middleParafoveaRadius) {' + NL +
    '  float ecc = length(uv - gazeCenterUV);' + NL +
    '  if (ecc <= innerFoveaRadius) return 0u;' + NL +
    '  if (ecc <= middleParafoveaRadius) return 5u;' + NL +
    '  return 10u;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.ComputeAsymmetricEyeProjectionXScaleOffset(
  const ATanLeft, ATanRight: Single): TVector2;
var
  Span: Single;
begin
  Span := Max(1e-5, ATanRight - ATanLeft);
  Result := Vector2(2.0 / Span, (ATanRight + ATanLeft) / Span);
end;

class function TCastleShaderGeneratorRHI.EvaluateFoveatedShadingRate(
  const AUV, AGazeCenterUV: TVector2;
  const AInnerFoveaRadius: Single;
  const AMiddleParafoveaRadius: Single): Cardinal;
var
  Ecc: Single;
begin
  Ecc := (AUV - AGazeCenterUV).Length;
  if Ecc <= AInnerFoveaRadius then
    Result := 0  // 1x1 foveal full resolution
  else if Ecc <= AMiddleParafoveaRadius then
    Result := $5 // 2x2 parafoveal shading rate
  else
    Result := $A; // 4x4 peripheral shading rate
end;

function TCastleShaderGeneratorRHI.GenerateHDR10PQEncodingFunctions: String;
begin
  Result :=
    '// Phase 37.3: ITU-R BT.2020 Wide Color Gamut & SMPTE ST.2084 (PQ) 10,000-Nit HDR10 Encoding' + NL +
    'vec3 EvaluateLinearRec709ToRec2020(vec3 rec709) {' + NL +
    '  vec3 c = max(rec709, vec3(0.0));' + NL +
    '  return vec3(' + NL +
    '    0.6274040 * c.r + 0.3292820 * c.g + 0.0433136 * c.b,' + NL +
    '    0.0690970 * c.r + 0.9195400 * c.g + 0.0113612 * c.b,' + NL +
    '    0.0163916 * c.r + 0.0880132 * c.g + 0.8955950 * c.b);' + NL +
    '}' + NL +
    NL +
    'vec3 EncodeST2084PQ(vec3 linearNits) {' + NL +
    '  const float m1 = 0.1593017578125;' + NL +
    '  const float m2 = 78.84375;' + NL +
    '  const float c1 = 0.8359375;' + NL +
    '  const float c2 = 18.8515625;' + NL +
    '  const float c3 = 18.6875;' + NL +
    '  vec3 y = clamp(linearNits / 10000.0, vec3(0.0), vec3(1.0));' + NL +
    '  vec3 ym1 = pow(y, vec3(m1));' + NL +
    '  return pow((vec3(c1) + c2 * ym1) / (vec3(1.0) + c3 * ym1), vec3(m2));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateLinearRec709ToRec2020(
  const ALinearRec709: TVector3): TVector3;
var
  R, G, B: Single;
begin
  R := Max(0.0, ALinearRec709.X);
  G := Max(0.0, ALinearRec709.Y);
  B := Max(0.0, ALinearRec709.Z);
  Result := Vector3(
    0.6274040 * R + 0.3292820 * G + 0.0433136 * B,
    0.0690970 * R + 0.9195400 * G + 0.0113612 * B,
    0.0163916 * R + 0.0880132 * G + 0.8955950 * B);
end;

class function TCastleShaderGeneratorRHI.EncodeST2084PQ(
  const ALinearNits: TVector3): TVector3;
const
  M1 = 0.1593017578125; // 2610 / 16384
  M2 = 78.84375;        // (2523 / 4096) * 128
  C1 = 0.8359375;       // 3424 / 4096
  C2 = 18.8515625;      // (2413 / 4096) * 32
  C3 = 18.6875;         // (2392 / 4096) * 32

  function EncodeChannel(const ANits: Single): Single;
  var
    Y, YM1: Single;
  begin
    Y := EnsureRange(ANits / 10000.0, 0.0, 1.0);
    if Y <= 0.0 then
      Exit(0.0);
    YM1 := Power(Y, M1);
    Result := EnsureRange(Power((C1 + C2 * YM1) / (1.0 + C3 * YM1), M2), 0.0, 1.0);
  end;

begin
  Result := Vector3(
    EncodeChannel(ALinearNits.X),
    EncodeChannel(ALinearNits.Y),
    EncodeChannel(ALinearNits.Z));
end;

function TCastleShaderGeneratorRHI.GenerateDualQuaternionSkinningFunctions: String;
begin
  Result :=
    '// Phase 38.1: Volume-Preserving Dual-Quaternion Skinning (DQS)' + NL +
    'void BlendDualQuaternionAntipodal(vec4 dqReal0, vec4 dqDual0, vec4 dqReal1, vec4 dqDual1,' + NL +
    '  float w0, float w1, out vec4 outReal, out vec4 outDual) {' + NL +
    '  float sign1 = (dot(dqReal0, dqReal1) < 0.0) ? -1.0 : 1.0;' + NL +
    '  vec4 rawReal = dqReal0 * w0 + dqReal1 * (w1 * sign1);' + NL +
    '  vec4 rawDual = dqDual0 * w0 + dqDual1 * (w1 * sign1);' + NL +
    '  float invNorm = 1.0 / max(1e-6, length(rawReal));' + NL +
    '  outReal = rawReal * invNorm;' + NL +
    '  outDual = rawDual * invNorm;' + NL +
    '}' + NL +
    NL +
    'vec3 EvaluateDualQuaternionTransform(vec4 dqReal, vec4 dqDual, vec3 pos) {' + NL +
    '  vec3 rVec = dqReal.xyz;' + NL +
    '  vec3 dVec = dqDual.xyz;' + NL +
    '  vec3 rotated = pos + 2.0 * cross(rVec, cross(rVec, pos) + pos * dqReal.w);' + NL +
    '  vec3 trans = 2.0 * (dVec * dqReal.w - rVec * dqDual.w + cross(rVec, dVec));' + NL +
    '  return rotated + trans;' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.BlendDualQuaternionAntipodal(
  const ADQReal0, ADQDual0, ADQReal1, ADQDual1: TVector4;
  const AW0, AW1: Single;
  out AOutReal, AOutDual: TVector4);
var
  Sign1, InvNorm: Single;
  RawReal, RawDual: TVector4;
begin
  if TVector4.DotProduct(ADQReal0, ADQReal1) < 0.0 then
    Sign1 := -1.0
  else
    Sign1 := 1.0;
  RawReal := (ADQReal0 * AW0) + (ADQReal1 * (AW1 * Sign1));
  RawDual := (ADQDual0 * AW0) + (ADQDual1 * (AW1 * Sign1));
  InvNorm := 1.0 / Max(1e-6, RawReal.Length);
  AOutReal := RawReal * InvNorm;
  AOutDual := RawDual * InvNorm;
end;

class function TCastleShaderGeneratorRHI.EvaluateDualQuaternionTransform(
  const ADQReal, ADQDual: TVector4;
  const APosition: TVector3): TVector3;
var
  RVec, DVec, Rotated, Trans: TVector3;
begin
  RVec := Vector3(ADQReal.X, ADQReal.Y, ADQReal.Z);
  DVec := Vector3(ADQDual.X, ADQDual.Y, ADQDual.Z);
  Rotated := APosition + (TVector3.CrossProduct(RVec,
    TVector3.CrossProduct(RVec, APosition) + (APosition * ADQReal.W)) * 2.0);
  Trans := ((DVec * ADQReal.W) - (RVec * ADQDual.W) + TVector3.CrossProduct(RVec, DVec)) * 2.0;
  Result := Rotated + Trans;
end;

function TCastleShaderGeneratorRHI.GenerateDisplacedMicroMeshFunctions: String;
begin
  Result :=
    '// Phase 38.2: MegaGeometry Displaced Micro-Mesh Barycentric Tessellation & Crack-Free Edge Snapping' + NL +
    'vec3 EvaluateMicroMeshDisplacedPosition(vec3 p0, vec3 p1, vec3 p2,' + NL +
    '  vec3 n0, vec3 n1, vec3 n2, vec2 baryUV, float heightSample,' + NL +
    '  float displacementScale, float displacementBias) {' + NL +
    '  float w0 = 1.0 - baryUV.x - baryUV.y;' + NL +
    '  vec3 basePos = p0 * w0 + p1 * baryUV.x + p2 * baryUV.y;' + NL +
    '  vec3 baseNorm = normalize(n0 * w0 + n1 * baryUV.x + n2 * baryUV.y);' + NL +
    '  float disp = heightSample * displacementScale + displacementBias;' + NL +
    '  return basePos + baseNorm * disp;' + NL +
    '}' + NL +
    NL +
    'vec2 SnapMicroMeshBarycentricToGrid(vec2 baryUV, uint subdivisionSegments) {' + NL +
    '  float n = max(1.0, float(subdivisionSegments));' + NL +
    '  float u = clamp(round(clamp(baryUV.x, 0.0, 1.0) * n) / n, 0.0, 1.0);' + NL +
    '  float v = clamp(round(clamp(baryUV.y, 0.0, 1.0) * n) / n, 0.0, 1.0 - u);' + NL +
    '  return vec2(u, v);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateMicroMeshDisplacedPosition(
  const AP0, AP1, AP2, AN0, AN1, AN2: TVector3;
  const ABaryUV: TVector2;
  const AHeightSample, ADisplacementScale: Single;
  const ADisplacementBias: Single): TVector3;
var
  W0, Disp: Single;
  BasePos, BaseNorm: TVector3;
begin
  W0 := 1.0 - ABaryUV.X - ABaryUV.Y;
  BasePos := (AP0 * W0) + (AP1 * ABaryUV.X) + (AP2 * ABaryUV.Y);
  BaseNorm := (AN0 * W0) + (AN1 * ABaryUV.X) + (AN2 * ABaryUV.Y);
  if BaseNorm.LengthSqr > 1e-12 then
    BaseNorm := BaseNorm.Normalize
  else
    BaseNorm := Vector3(0, 0, 1);
  Disp := AHeightSample * ADisplacementScale + ADisplacementBias;
  Result := BasePos + (BaseNorm * Disp);
end;

class function TCastleShaderGeneratorRHI.SnapMicroMeshBarycentricToGrid(
  const ABaryUV: TVector2;
  const ASubdivisionSegments: Cardinal): TVector2;
var
  N, U, V: Single;
begin
  N := Max(1.0, Single(ASubdivisionSegments));
  U := EnsureRange(Round(EnsureRange(ABaryUV.X, 0.0, 1.0) * N) / N, 0.0, 1.0);
  V := EnsureRange(Round(EnsureRange(ABaryUV.Y, 0.0, 1.0) * N) / N, 0.0, 1.0 - U);
  Result := Vector2(U, V);
end;

function TCastleShaderGeneratorRHI.GenerateOpticalFlowBlockMatchingShader: String;
begin
  Result :=
    '// Phase 39.1: Optical Flow SAD Block Matching & Parabolic Sub-Pixel Motion Estimation' + NL +
    'float EvaluateOpticalFlowPatchSAD(vec4 patchCurr, vec4 patchPrev) {' + NL +
    '  vec4 diff = abs(patchCurr - patchPrev);' + NL +
    '  return diff.x + diff.y + diff.z + diff.w;' + NL +
    '}' + NL +
    NL +
    'vec2 EstimateParabolicSubpixelOffset(float sadLeft, float sadCenter, float sadRight,' + NL +
    '  float sadDown, float sadUp) {' + NL +
    '  float denomX = max(1e-5, sadLeft - 2.0 * sadCenter + sadRight);' + NL +
    '  float denomY = max(1e-5, sadDown - 2.0 * sadCenter + sadUp);' + NL +
    '  float dx = clamp((sadLeft - sadRight) / (2.0 * denomX), -0.5, 0.5);' + NL +
    '  float dy = clamp((sadDown - sadUp) / (2.0 * denomY), -0.5, 0.5);' + NL +
    '  return vec2(dx, dy);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateOpticalFlowPatchSAD(
  const APatchCurr, APatchPrev: TVector4): Single;
begin
  Result :=
    Abs(APatchCurr.X - APatchPrev.X) +
    Abs(APatchCurr.Y - APatchPrev.Y) +
    Abs(APatchCurr.Z - APatchPrev.Z) +
    Abs(APatchCurr.W - APatchPrev.W);
end;

class function TCastleShaderGeneratorRHI.EstimateParabolicSubpixelOffset(
  const ASADLeft, ASADCenter, ASADRight, ASADDown, ASADUp: Single): TVector2;
var
  DenomX, DenomY, DX, DY: Single;
begin
  DenomX := Max(1e-5, ASADLeft - 2.0 * ASADCenter + ASADRight);
  DenomY := Max(1e-5, ASADDown - 2.0 * ASADCenter + ASADUp);
  DX := EnsureRange((ASADLeft - ASADRight) / (2.0 * DenomX), -0.5, 0.5);
  DY := EnsureRange((ASADDown - ASADUp) / (2.0 * DenomY), -0.5, 0.5);
  Result := Vector2(DX, DY);
end;

function TCastleShaderGeneratorRHI.GenerateSpatioTemporalBlueNoiseFunctions: String;
begin
  Result :=
    '// Phase 39.2: Martin Roberts R2 Quasi-Random Sequence & Golden-Ratio Spatio-Temporal Blue-Noise' + NL +
    'vec2 EvaluateR2Sequence2D(uint sampleIndex) {' + NL +
    '  const vec2 alpha = vec2(0.7548776662466927, 0.5698402909980532);' + NL +
    '  return fract(vec2(0.5) + float(sampleIndex + 1u) * alpha);' + NL +
    '}' + NL +
    NL +
    'float EvaluateTemporalBlueNoiseJitter(float baseBlueNoise, uint frameIndex) {' + NL +
    '  const float goldenConj = 0.6180339887498949;' + NL +
    '  return fract(baseBlueNoise + float(frameIndex & 63u) * goldenConj);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateR2Sequence2D(
  const ASampleIndex: Cardinal): TVector2;
const
  Alpha1 = 0.7548776662466927;
  Alpha2 = 0.5698402909980532;
var
  N: Double;
begin
  N := Double(ASampleIndex) + 1.0;
  Result := Vector2(
    Single(Frac(0.5 + N * Alpha1)),
    Single(Frac(0.5 + N * Alpha2)));
end;

class function TCastleShaderGeneratorRHI.EvaluateTemporalBlueNoiseJitter(
  const ABaseBlueNoise: Single;
  const AFrameIndex: Cardinal): Single;
const
  GoldenConj = 0.6180339887498949;
var
  Raw: Double;
begin
  Raw := EnsureRange(ABaseBlueNoise, 0.0, 1.0) + Double(AFrameIndex and 63) * GoldenConj;
  Result := Single(Frac(Raw));
end;

function TCastleShaderGeneratorRHI.GenerateSERCoherenceHintFunctions: String;
begin
  Result :=
    '#extension GL_NV_shader_invocation_reorder : enable' + NL +
    NL +
    '// Phase 40.1: Shader Execution Reordering (SER) Coherence Hint Packing' + NL +
    'uint PackSERCoherenceHintKey(bool isHit, uint materialShadingModelID, vec3 rayDirection) {' + NL +
    '  uint hitBit = isHit ? 128u : 0u;' + NL +
    '  uint matBits = (materialShadingModelID & 15u) << 3u;' + NL +
    '  uint octant = ((rayDirection.x >= 0.0) ? 1u : 0u) |' + NL +
    '                ((rayDirection.y >= 0.0) ? 2u : 0u) |' + NL +
    '                ((rayDirection.z >= 0.0) ? 4u : 0u);' + NL +
    '  return hitBit | matBits | octant;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.PackSERCoherenceHintKey(
  const AIsHit: Boolean;
  const AMaterialShadingModelID: Cardinal;
  const ARayDirection: TVector3): Cardinal;
var
  HitBit, MatBits, Octant: Cardinal;
begin
  if AIsHit then
    HitBit := 128
  else
    HitBit := 0;
  MatBits := (AMaterialShadingModelID and 15) shl 3;
  Octant := 0;
  if ARayDirection.X >= 0.0 then
    Octant := Octant or 1;
  if ARayDirection.Y >= 0.0 then
    Octant := Octant or 2;
  if ARayDirection.Z >= 0.0 then
    Octant := Octant or 4;
  Result := HitBit or MatBits or Octant;
end;

function TCastleShaderGeneratorRHI.GenerateDualSourceGlassTransmissionFunctions: String;
begin
  Result :=
    '// Phase 40.2: Dual-Source Color Blending for Single-Pass Colored Glass Transmission' + NL +
    '// layout(location = 0, index = 0) out vec4 out_GlassSpecularAdd;' + NL +
    '// layout(location = 0, index = 1) out vec4 out_GlassTransmittanceMul;' + NL +
    'void EvaluateDualSourceGlassOutputs(vec3 specularLightingRGB, vec3 glassTintRGB,' + NL +
    '  float fresnelF, float glassOpacity, out vec3 outSpecularAdd, out vec3 outTransmittanceMul) {' + NL +
    '  float f = clamp(fresnelF, 0.0, 1.0);' + NL +
    '  float op = clamp(glassOpacity, 0.0, 1.0);' + NL +
    '  vec3 filterColor = mix(vec3(1.0), clamp(glassTintRGB, vec3(0.0), vec3(1.0)), op);' + NL +
    '  outSpecularAdd = max(specularLightingRGB, vec3(0.0));' + NL +
    '  outTransmittanceMul = filterColor * (1.0 - f);' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateDualSourceGlassOutputs(
  const ASpecularLightingRGB, AGlassTintRGB: TVector3;
  const AFresnelF, AGlassOpacity: Single;
  out AOutSpecularAdd, AOutTransmittanceMul: TVector3);
var
  F, Op, OneMinusF: Single;
  ClampedTint, FilterColor: TVector3;
begin
  F := EnsureRange(AFresnelF, 0.0, 1.0);
  Op := EnsureRange(AGlassOpacity, 0.0, 1.0);
  OneMinusF := 1.0 - F;
  ClampedTint := Vector3(
    EnsureRange(AGlassTintRGB.X, 0.0, 1.0),
    EnsureRange(AGlassTintRGB.Y, 0.0, 1.0),
    EnsureRange(AGlassTintRGB.Z, 0.0, 1.0));
  FilterColor := Vector3(
    1.0 + (ClampedTint.X - 1.0) * Op,
    1.0 + (ClampedTint.Y - 1.0) * Op,
    1.0 + (ClampedTint.Z - 1.0) * Op);
  AOutSpecularAdd := Vector3(
    Max(0.0, ASpecularLightingRGB.X),
    Max(0.0, ASpecularLightingRGB.Y),
    Max(0.0, ASpecularLightingRGB.Z));
  AOutTransmittanceMul := FilterColor * OneMinusF;
end;

function TCastleShaderGeneratorRHI.GenerateDynamicRenderingLocalReadPreamble: String;
begin
  Result :=
    '// Phase 41.1: Vulkan 1.4 Dynamic Rendering Local Read (VK_KHR_dynamic_rendering_local_read)' + NL +
    'layout(input_attachment_index = 0, set = 0, binding = 0) uniform subpassInput castle_TileGBufferAlbedo;' + NL +
    'layout(input_attachment_index = 1, set = 0, binding = 1) uniform subpassInput castle_TileGBufferNormalRoughness;' + NL +
    NL +
    'vec3 EvaluateTileLocalDeferredLighting(vec3 albedo, vec3 normalWS, float roughness,' + NL +
    '  vec3 lightDirWS, vec3 viewDirWS, vec3 lightColor) {' + NL +
    '  vec3 N = normalize(normalWS);' + NL +
    '  vec3 L = normalize(lightDirWS);' + NL +
    '  vec3 V = normalize(viewDirWS);' + NL +
    '  float NdotL = max(0.0, dot(N, L));' + NL +
    '  if (NdotL <= 0.0) return vec3(0.0);' + NL +
    '  vec3 H = normalize(L + V);' + NL +
    '  float NdotH = max(0.0, dot(N, H));' + NL +
    '  float r = clamp(roughness, 0.04, 1.0);' + NL +
    '  float alpha2 = max(0.001, r * r * r * r);' + NL +
    '  float specPower = max(2.0, (2.0 / alpha2) - 2.0);' + NL +
    '  float specTerm = ((specPower + 2.0) / 25.1327412) * pow(NdotH, specPower);' + NL +
    '  return (albedo * 0.318309886 + vec3(specTerm)) * lightColor * NdotL;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateTileLocalDeferredLighting(
  const AAlbedo, ANormalWS: TVector3;
  const ARoughness: Single;
  const ALightDirWS, AViewDirWS, ALightColor: TVector3): TVector3;
var
  N, L, V, H: TVector3;
  NdotL, NdotH, R, Alpha2, SpecPower, SpecTerm, InvPi: Single;
begin
  if (ANormalWS.LengthSqr <= 1e-12) or (ALightDirWS.LengthSqr <= 1e-12) then
    Exit(TVector3.Zero);
  N := ANormalWS.Normalize;
  L := ALightDirWS.Normalize;
  NdotL := Max(0.0, TVector3.DotProduct(N, L));
  if NdotL <= 0.0 then
    Exit(TVector3.Zero);

  if AViewDirWS.LengthSqr > 1e-12 then
    V := AViewDirWS.Normalize
  else
    V := L;
  if (L + V).LengthSqr > 1e-12 then
    H := (L + V).Normalize
  else
    H := N;

  NdotH := Max(0.0, TVector3.DotProduct(N, H));
  R := EnsureRange(ARoughness, 0.04, 1.0);
  Alpha2 := Max(0.001, Sqr(Sqr(R)));
  SpecPower := Max(2.0, (2.0 / Alpha2) - 2.0);
  SpecTerm := ((SpecPower + 2.0) / (8.0 * Pi)) * Power(NdotH, SpecPower);
  InvPi := 1.0 / Pi;

  Result := Vector3(
    (Max(0.0, AAlbedo.X) * InvPi + SpecTerm) * Max(0.0, ALightColor.X) * NdotL,
    (Max(0.0, AAlbedo.Y) * InvPi + SpecTerm) * Max(0.0, ALightColor.Y) * NdotL,
    (Max(0.0, AAlbedo.Z) * InvPi + SpecTerm) * Max(0.0, ALightColor.Z) * NdotL);
end;

function TCastleShaderGeneratorRHI.GenerateSubgroupWaveIntrinsicsFunctions: String;
begin
  Result :=
    '#extension GL_KHR_shader_subgroup_basic : enable' + NL +
    '#extension GL_KHR_shader_subgroup_ballot : enable' + NL +
    '#extension GL_KHR_shader_subgroup_arithmetic : enable' + NL +
    NL +
    '// Phase 41.2: Subgroup Wave Intrinsics for Lock-Free Reduction & Prefix Scans' + NL +
    'uint EvaluateSubgroupWaveActiveCount(bool predicate) {' + NL +
    '  uvec4 ballot = subgroupBallot(predicate);' + NL +
    '  return subgroupBallotBitCount(ballot);' + NL +
    '}' + NL +
    NL +
    'uint EvaluateSubgroupExclusivePrefixSum(uint value) {' + NL +
    '  return subgroupExclusiveAdd(value);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSubgroupBallotBitCount(
  const ABallotMask: UInt64): Cardinal;
var
  Mask: UInt64;
begin
  Result := 0;
  Mask := ABallotMask;
  while Mask <> 0 do
  begin
    Inc(Result, Cardinal(Mask and 1));
    Mask := Mask shr 1;
  end;
end;

class function TCastleShaderGeneratorRHI.EvaluateSubgroupExclusivePrefixSum4(
  const AValues: TVector4Cardinal): TVector4Cardinal;
begin
  Result.X := 0;
  Result.Y := AValues.X;
  Result.Z := AValues.X + AValues.Y;
  Result.W := AValues.X + AValues.Y + AValues.Z;
end;

function TCastleShaderGeneratorRHI.GenerateBufferDeviceAddressPreamble: String;
begin
  Result :=
    '#extension GL_EXT_buffer_reference2 : require' + NL +
    '#extension GL_EXT_scalar_block_layout : require' + NL +
    '#extension GL_EXT_shader_explicit_arithmetic_types_int64 : require' + NL +
    NL +
    '// Phase 42.1: Bindless 64-Bit Buffer Device Address (VK_KHR_buffer_device_address)' + NL +
    'layout(buffer_reference, scalar, buffer_reference_align = 16) readonly buffer CastleBDAVertexBuffer {' + NL +
    '  vec4 positions[];' + NL +
    '};' + NL +
    'layout(buffer_reference, scalar, buffer_reference_align = 4) readonly buffer CastleBDAIndexBuffer {' + NL +
    '  uint indices[];' + NL +
    '};' + NL;
end;

class function TCastleShaderGeneratorRHI.AlignBufferDeviceAddress(
  const AAddress: UInt64;
  const AAlignment: Cardinal): UInt64;
var
  Align64, Mask: UInt64;
begin
  if AAlignment <= 1 then
    Exit(AAddress);
  Align64 := UInt64(AAlignment);
  Mask := Align64 - 1;
  Result := (AAddress + Mask) and (not Mask);
end;

function TCastleShaderGeneratorRHI.GenerateVoxelConeTracingFunctions: String;
begin
  Result :=
    '// Phase 42.2: Dominant-Axis Voxelization & 3D Mipmapped Voxel Cone Tracing (VXGI)' + NL +
    'uint SelectDominantVoxelProjectionAxis(vec3 faceNormal) {' + NL +
    '  vec3 an = abs(faceNormal);' + NL +
    '  if (an.x >= an.y && an.x >= an.z) return 0u;' + NL +
    '  if (an.y >= an.x && an.y >= an.z) return 1u;' + NL +
    '  return 2u;' + NL +
    '}' + NL +
    NL +
    'vec2 EvaluateVoxelConeStepAperture(float distanceAlongCone, float coneHalfAngleTan, float baseVoxelSize) {' + NL +
    '  float safeVoxel = max(1e-4, baseVoxelSize);' + NL +
    '  float diameter = max(safeVoxel, 2.0 * max(0.0, distanceAlongCone) * max(0.0, coneHalfAngleTan));' + NL +
    '  float mipLevel = max(0.0, log2(diameter / safeVoxel));' + NL +
    '  return vec2(diameter, mipLevel);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.SelectDominantVoxelProjectionAxis(
  const AFaceNormal: TVector3): Cardinal;
var
  AX, AY, AZ: Single;
begin
  AX := Abs(AFaceNormal.X);
  AY := Abs(AFaceNormal.Y);
  AZ := Abs(AFaceNormal.Z);
  if (AX >= AY) and (AX >= AZ) then
    Result := 0
  else
  if (AY >= AX) and (AY >= AZ) then
    Result := 1
  else
    Result := 2;
end;

class function TCastleShaderGeneratorRHI.EvaluateVoxelConeStepAperture(
  const ADistanceAlongCone, AConeHalfAngleTan, ABaseVoxelSize: Single): TVector2;
var
  SafeVoxel, Diameter, MipLevel: Single;
begin
  SafeVoxel := Max(1e-4, ABaseVoxelSize);
  Diameter := Max(SafeVoxel, 2.0 * Max(0.0, ADistanceAlongCone) * Max(0.0, AConeHalfAngleTan));
  MipLevel := Max(0.0, Log2(Diameter / SafeVoxel));
  Result := Vector2(Diameter, MipLevel);
end;

function TCastleShaderGeneratorRHI.GenerateSpecularNormalVarianceAAFunctions: String;
begin
  Result :=
    '// Phase 43.1: Normal-Variance Specular Anti-Aliasing (Kaplanyan & Antonov 2016)' + NL +
    'float EvaluateSpecularAARoughness(float baseRoughness, vec3 dNdU, vec3 dNdV,' + NL +
    '  float varianceStrength, float maxVarianceThreshold) {' + NL +
    '  float r = clamp(baseRoughness, 0.0, 1.0);' + NL +
    '  float alpha = r * r;' + NL +
    '  float normalVar = dot(dNdU, dNdU) + dot(dNdV, dNdV);' + NL +
    '  float kernelVar = min(max(0.0, maxVarianceThreshold), max(0.0, varianceStrength) * normalVar);' + NL +
    '  float filteredAlpha2 = min(1.0, alpha * alpha + kernelVar);' + NL +
    '  return sqrt(sqrt(filteredAlpha2));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSpecularAARoughness(
  const ABaseRoughness: Single;
  const ADNdU, ADNdV: TVector3;
  const AVarianceStrength: Single;
  const AMaxVarianceThreshold: Single): Single;
var
  R, Alpha, NormalVar, KernelVar, FilteredAlpha2: Single;
begin
  R := EnsureRange(ABaseRoughness, 0.0, 1.0);
  Alpha := R * R;
  NormalVar := ADNdU.LengthSqr + ADNdV.LengthSqr;
  KernelVar := Min(Max(0.0, AMaxVarianceThreshold), Max(0.0, AVarianceStrength) * NormalVar);
  FilteredAlpha2 := Min(1.0, Alpha * Alpha + KernelVar);
  Result := Sqrt(Sqrt(FilteredAlpha2));
end;

function TCastleShaderGeneratorRHI.GenerateRectilinearShadowWarpFunctions: String;
begin
  Result :=
    '// Phase 43.2: Rectilinear Shadow Map UV Warping' + NL +
    'vec2 EvaluateRectilinearShadowWarpUV(vec2 shadowUV, float warpStrength) {' + NL +
    '  float w = max(0.0, warpStrength);' + NL +
    '  vec2 p = clamp(shadowUV, vec2(0.0), vec2(1.0)) * 2.0 - 1.0;' + NL +
    '  vec2 warpedP = p * (1.0 + w) / (1.0 + w * abs(p));' + NL +
    '  return warpedP * 0.5 + 0.5;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateRectilinearShadowWarpUV(
  const AShadowUV: TVector2;
  const AWarpStrength: Single): TVector2;
var
  W, PX, PY, WarpedX, WarpedY: Single;
begin
  W := Max(0.0, AWarpStrength);
  PX := EnsureRange(AShadowUV.X, 0.0, 1.0) * 2.0 - 1.0;
  PY := EnsureRange(AShadowUV.Y, 0.0, 1.0) * 2.0 - 1.0;
  WarpedX := PX * (1.0 + W) / (1.0 + W * Abs(PX));
  WarpedY := PY * (1.0 + W) / (1.0 + W * Abs(PY));
  Result := Vector2(WarpedX * 0.5 + 0.5, WarpedY * 0.5 + 0.5);
end;

function TCastleShaderGeneratorRHI.GenerateMSDFTextFunctions: String;
begin
  Result :=
    '// Phase 44.1: Multi-Channel Signed Distance Field (MSDF) Median-of-Three Text & Vector Rendering' + NL +
    'float EvaluateMSDFMedianOfThree(float r, float g, float b) {' + NL +
    '  return max(min(r, g), min(max(r, g), b));' + NL +
    '}' + NL +
    NL +
    'float EvaluateMSDFScreenPxOpacity(vec3 msdfRGB, float screenPxRange) {' + NL +
    '  float sd = EvaluateMSDFMedianOfThree(msdfRGB.r, msdfRGB.g, msdfRGB.b) - 0.5;' + NL +
    '  float screenPxDistance = max(1.0, screenPxRange) * sd;' + NL +
    '  return clamp(screenPxDistance + 0.5, 0.0, 1.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateMSDFMedianOfThree(
  const AR, AG, AB: Single): Single;
begin
  Result := Max(Min(AR, AG), Min(Max(AR, AG), AB));
end;

class function TCastleShaderGeneratorRHI.EvaluateMSDFScreenPxOpacity(
  const AMSDFSampleRGB: TVector3;
  const AScreenPxRange: Single): Single;
var
  SD, ScreenPxDist: Single;
begin
  SD := EvaluateMSDFMedianOfThree(AMSDFSampleRGB.X, AMSDFSampleRGB.Y, AMSDFSampleRGB.Z) - 0.5;
  ScreenPxDist := Max(1.0, AScreenPxRange) * SD;
  Result := EnsureRange(ScreenPxDist + 0.5, 0.0, 1.0);
end;

function TCastleShaderGeneratorRHI.GenerateProceduralGrassBladeFunctions: String;
begin
  Result :=
    '// Phase 44.2: Procedural Quadratic Bezier Grass Blade Position & Tangent' + NL +
    'void EvaluateQuadraticBezierGrassBlade(vec3 p0, vec3 p1, vec3 p2, float t,' + NL +
    '  out vec3 outPosition, out vec3 outTangent) {' + NL +
    '  float s = clamp(t, 0.0, 1.0);' + NL +
    '  float omt = 1.0 - s;' + NL +
    '  outPosition = (omt * omt) * p0 + (2.0 * omt * s) * p1 + (s * s) * p2;' + NL +
    '  vec3 deriv = 2.0 * omt * (p1 - p0) + 2.0 * s * (p2 - p1);' + NL +
    '  outTangent = normalize(deriv);' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateQuadraticBezierGrassBlade(
  const AP0, AP1, AP2: TVector3;
  const AT: Single;
  out AOutPosition, AOutTangent: TVector3);
var
  S, Omt: Single;
  Deriv: TVector3;
begin
  S := EnsureRange(AT, 0.0, 1.0);
  Omt := 1.0 - S;
  AOutPosition := AP0 * (Omt * Omt) + AP1 * (2.0 * Omt * S) + AP2 * (S * S);
  Deriv := (AP1 - AP0) * (2.0 * Omt) + (AP2 - AP1) * (2.0 * S);
  if Deriv.LengthSqr > 1e-12 then
    AOutTangent := Deriv.Normalize
  else
    AOutTangent := Vector3(0.0, 1.0, 0.0);
end;

function TCastleShaderGeneratorRHI.GeneratePushDescriptorPreamble(
  const ASetIndex: Cardinal): String;
begin
  Result :=
    '// Phase 45.1: Vulkan 1.4 Push Descriptor Set Preamble (VK_KHR_push_descriptor)' + NL +
    Format('layout(set = %d, binding = 0) uniform CastlePushMaterialBlock {', [ASetIndex]) + NL +
    '  vec4 baseColorFactor;' + NL +
    '  vec4 metallicRoughnessNormalOcclusion;' + NL +
    '} castle_PushMaterial;' + NL +
    Format('layout(set = %d, binding = 1) uniform sampler2D castle_PushBaseColorTex;', [ASetIndex]) + NL +
    Format('layout(set = %d, binding = 2) uniform sampler2D castle_PushNormalRoughnessTex;', [ASetIndex]) + NL;
end;

class function TCastleShaderGeneratorRHI.ComputeHostImageCopyByteSize(
  const AWidth, AHeight, ADepth, ABytesPerTexel: Cardinal;
  const ARowAlignmentBytes: Cardinal): UInt64;
var
  W, H, D, Bpp, AlignVal, RawRowBytes, AlignedRowPitch: UInt64;
begin
  W := Max(Cardinal(1), AWidth);
  H := Max(Cardinal(1), AHeight);
  D := Max(Cardinal(1), ADepth);
  Bpp := Max(Cardinal(1), ABytesPerTexel);
  AlignVal := Max(Cardinal(1), ARowAlignmentBytes);
  RawRowBytes := W * Bpp;
  AlignedRowPitch := ((RawRowBytes + AlignVal - 1) div AlignVal) * AlignVal;
  Result := AlignedRowPitch * H * D;
end;

function TCastleShaderGeneratorRHI.GenerateSphericalGaussianFunctions: String;
begin
  Result :=
    '// Phase 45.2: Spherical Gaussian (SG) Fast Specular & Diffuse Lobe Approximation' + NL +
    'vec3 EvaluateSphericalGaussian(vec3 dir, vec3 axis, float sharpness, vec3 amplitude) {' + NL +
    '  return amplitude * exp(sharpness * (dot(dir, axis) - 1.0));' + NL +
    '}' + NL +
    'vec3 EvaluateSphericalGaussianIntegral(vec3 amplitude, float sharpness) {' + NL +
    '  if (sharpness <= 1e-4) return amplitude * (4.0 * 3.14159265359);' + NL +
    '  float expTerm = 1.0 - exp(-2.0 * sharpness);' + NL +
    '  return amplitude * (2.0 * 3.14159265359 * expTerm / sharpness);' + NL +
    '}' + NL;
end;

class procedure TCastleShaderGeneratorRHI.EvaluateSphericalGaussianProduct(
  const AAxis1: TVector3; const ASharpness1: Single;
  const AAxis2: TVector3; const ASharpness2: Single;
  out AProductAxis: TVector3;
  out AProductSharpness, ALogAmplitudeScale: Single);
var
  S1, S2, UmLen: Single;
  Um: TVector3;
begin
  S1 := Max(0.0, ASharpness1);
  S2 := Max(0.0, ASharpness2);
  Um := AAxis1 * S1 + AAxis2 * S2;
  UmLen := Um.Length;
  AProductSharpness := UmLen;
  if UmLen > 1e-6 then
    AProductAxis := Um / UmLen
  else
    AProductAxis := Vector3(0.0, 1.0, 0.0);
  ALogAmplitudeScale := UmLen - (S1 + S2);
end;

class function TCastleShaderGeneratorRHI.EvaluateSphericalGaussianIntegral(
  const AAmplitude: TVector3;
  const ASharpness: Single): TVector3;
var
  S, ExpTerm, Factor: Single;
begin
  S := Max(0.0, ASharpness);
  if S <= 1e-4 then
    Factor := 4.0 * Pi
  else
  begin
    ExpTerm := 1.0 - Exp(-2.0 * S);
    Factor := 2.0 * Pi * ExpTerm / S;
  end;
  Result := AAmplitude * Factor;
end;

function TCastleShaderGeneratorRHI.GenerateGaussianSplattingFunctions: String;
begin
  Result :=
    '// Phase 46.1 & 46.2: 3D Gaussian Splatting (3DGS) 2D Conic Opacity & L1 Spherical Harmonics' + NL +
    'float EvaluateGaussianSplatOpacity(vec3 cov2D, vec2 pixelOffset, float baseOpacity) {' + NL +
    '  float covXX = cov2D.x + 0.3;' + NL +
    '  float covXY = cov2D.y;' + NL +
    '  float covYY = cov2D.z + 0.3;' + NL +
    '  float det = covXX * covYY - covXY * covXY;' + NL +
    '  if (det <= 1e-6) return 0.0;' + NL +
    '  float invDet = 1.0 / det;' + NL +
    '  vec3 conic = vec3(covYY * invDet, -covXY * invDet, covXX * invDet);' + NL +
    '  float power = -0.5 * (conic.x * pixelOffset.x * pixelOffset.x + conic.z * pixelOffset.y * pixelOffset.y) - conic.y * pixelOffset.x * pixelOffset.y;' + NL +
    '  if (power > 0.0) return 0.0;' + NL +
    '  return clamp(baseOpacity * exp(power), 0.0, 0.99);' + NL +
    '}' + NL +
    'vec3 EvaluateSphericalHarmonicsL1RGB(vec3 sh0, vec3 sh1X, vec3 sh1Y, vec3 sh1Z, vec3 viewDir) {' + NL +
    '  const float SH_C0 = 0.28209479177387814;' + NL +
    '  const float SH_C1 = 0.4886025119029199;' + NL +
    '  vec3 color = vec3(0.5) + SH_C0 * sh0 - SH_C1 * viewDir.y * sh1Y + SH_C1 * viewDir.z * sh1Z - SH_C1 * viewDir.x * sh1X;' + NL +
    '  return max(color, vec3(0.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateGaussianSplatOpacity(
  const ACov2D: TVector3;
  const APixelOffset: TVector2;
  const ABaseOpacity: Single): Single;
var
  CovXX, CovXY, CovYY, Det, InvDet, ConicX, ConicY, ConicZ, Power: Single;
begin
  CovXX := ACov2D.X + 0.3;
  CovXY := ACov2D.Y;
  CovYY := ACov2D.Z + 0.3;
  Det := CovXX * CovYY - CovXY * CovXY;
  if Det <= 1e-6 then
    Exit(0.0);
  InvDet := 1.0 / Det;
  ConicX := CovYY * InvDet;
  ConicY := -CovXY * InvDet;
  ConicZ := CovXX * InvDet;
  Power := -0.5 * (ConicX * Sqr(APixelOffset.X) + ConicZ * Sqr(APixelOffset.Y)) -
    ConicY * APixelOffset.X * APixelOffset.Y;
  if Power > 0.0 then
    Exit(0.0);
  Result := EnsureRange(EnsureRange(ABaseOpacity, 0.0, 1.0) * Exp(Power), 0.0, 0.99);
end;

class function TCastleShaderGeneratorRHI.EvaluateSphericalHarmonicsL1RGB(
  const ASH0, ASH1X, ASH1Y, ASH1Z: TVector3;
  const AViewDir: TVector3): TVector3;
const
  SH_C0 = 0.28209479177387814;
  SH_C1 = 0.4886025119029199;
var
  Dir: TVector3;
begin
  if AViewDir.LengthSqr > 1e-12 then
    Dir := AViewDir.Normalize
  else
    Dir := Vector3(0.0, 0.0, 1.0);
  Result := Vector3(0.5, 0.5, 0.5) +
    ASH0 * SH_C0 -
    ASH1Y * (SH_C1 * Dir.Y) +
    ASH1Z * (SH_C1 * Dir.Z) -
    ASH1X * (SH_C1 * Dir.X);
  Result.X := Max(0.0, Result.X);
  Result.Y := Max(0.0, Result.Y);
  Result.Z := Max(0.0, Result.Z);
end;

function TCastleShaderGeneratorRHI.GeneratePCSSSoftShadowFunctions: String;
begin
  Result :=
    '// Phase 47.1: Percentage-Closer Soft Shadows (PCSS) Contact-Hardening Penumbra' + NL +
    'float EvaluatePCSSPenumbraWidth(float receiverDepth, float avgBlockerDepth, float lightSize) {' + NL +
    '  if (avgBlockerDepth <= 1e-6 || receiverDepth <= avgBlockerDepth) return 0.0;' + NL +
    '  return max(0.0, (receiverDepth - avgBlockerDepth) * max(0.0, lightSize) / avgBlockerDepth);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluatePCSSPenumbraWidth(
  const AReceiverDepth, AAvgBlockerDepth, ALightSize: Single): Single;
begin
  if (AAvgBlockerDepth <= 1e-6) or (AReceiverDepth <= AAvgBlockerDepth) then
    Exit(0.0);
  Result := Max(0.0, (AReceiverDepth - AAvgBlockerDepth) * Max(0.0, ALightSize) / AAvgBlockerDepth);
end;

class function TCastleShaderGeneratorRHI.ComputeDescriptorBufferAlignedOffset(
  const ASetIndex: Cardinal;
  const ASetByteSize: UInt64;
  const AOffsetAlignment: UInt64): UInt64;
var
  AlignVal, AlignedStride: UInt64;
begin
  AlignVal := Max(UInt64(1), AOffsetAlignment);
  AlignedStride := ((Max(UInt64(1), ASetByteSize) + AlignVal - 1) div AlignVal) * AlignVal;
  Result := UInt64(ASetIndex) * AlignedStride;
end;

function TCastleShaderGeneratorRHI.GenerateCarPaintFlakeFunctions: String;
begin
  Result :=
    '// Phase 48.1: Multi-Layer Metallic Flake & Clear-Coat Car-Paint BRDF' + NL +
    'vec3 EvaluateCarPaintMultiLayerBRDF(vec3 baseNormal, vec3 baseGrazing, vec3 flakeColor,' + NL +
    '  float ndotV, float ndotH, float flakeNdotH, float flakeExp) {' + NL +
    '  float v = clamp(ndotV, 0.0, 1.0);' + NL +
    '  float fresnelClearCoat = 0.04 + 0.96 * pow(1.0 - v, 5.0);' + NL +
    '  vec3 twoToneBase = mix(baseGrazing, baseNormal, v);' + NL +
    '  float flakeSpec = pow(clamp(flakeNdotH, 0.0, 1.0), max(1.0, flakeExp));' + NL +
    '  float coatSpec = pow(clamp(ndotH, 0.0, 1.0), 128.0) * fresnelClearCoat;' + NL +
    '  return (twoToneBase + flakeColor * flakeSpec) * (1.0 - fresnelClearCoat) + vec3(coatSpec);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateCarPaintMultiLayerBRDF(
  const ABaseColorNormal, ABaseColorGrazing, AFlakeColor: TVector3;
  const ANdotV, ANdotH, AFlakeNdotH, AFlakeExponent: Single): TVector3;
var
  V, OneMinusV, FresnelClearCoat, FlakeSpec, CoatSpec: Single;
  TwoToneBase: TVector3;
begin
  V := EnsureRange(ANdotV, 0.0, 1.0);
  OneMinusV := 1.0 - V;
  FresnelClearCoat := 0.04 + 0.96 * (OneMinusV * OneMinusV * OneMinusV * OneMinusV * OneMinusV);
  TwoToneBase := ABaseColorGrazing * (1.0 - V) + ABaseColorNormal * V;
  FlakeSpec := Power(EnsureRange(AFlakeNdotH, 0.0, 1.0), Max(1.0, AFlakeExponent));
  CoatSpec := Power(EnsureRange(ANdotH, 0.0, 1.0), 128.0) * FresnelClearCoat;
  Result := (TwoToneBase + AFlakeColor * FlakeSpec) * (1.0 - FresnelClearCoat) +
    Vector3(CoatSpec, CoatSpec, CoatSpec);
end;

function TCastleShaderGeneratorRHI.GenerateScreenSpaceCurvatureFunctions: String;
begin
  Result :=
    '// Phase 48.2: Screen-Space Normal Curvature & Ridge/Valley Cavity Shading' + NL +
    'float EvaluateScreenSpaceCurvatureCavity(vec3 dNdx, vec3 dNdy, float curvatureScale) {' + NL +
    '  float curv = (dNdx.x + dNdy.y) * curvatureScale;' + NL +
    '  return clamp(1.0 + curv, 0.0, 2.0);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateScreenSpaceCurvatureCavity(
  const ADNormalDx, ADNormalDy: TVector3;
  const ACurvatureScale: Single): Single;
var
  Curv: Single;
begin
  Curv := (ADNormalDx.X + ADNormalDy.Y) * ACurvatureScale;
  Result := EnsureRange(1.0 + Curv, 0.0, 2.0);
end;

function TCastleShaderGeneratorRHI.GenerateCameraEV100ExposureFunctions: String;
begin
  Result :=
    '// Phase 49.1: Physically Based Camera Exposure (EV100)' + NL +
    'float EvaluateEV100FromCameraSettings(float apertureFStops, float shutterSeconds, float iso) {' + NL +
    '  float n2 = max(0.5, apertureFStops) * max(0.5, apertureFStops);' + NL +
    '  float t = max(1e-6, shutterSeconds);' + NL +
    '  float s = max(1.0, iso);' + NL +
    '  return log2((n2 / t) * (100.0 / s));' + NL +
    '}' + NL +
    'float EvaluateExposureMultiplierFromEV100(float ev100) {' + NL +
    '  return 1.0 / (1.2 * exp2(ev100));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateEV100FromCameraSettings(
  const AApertureFStops, AShutterSeconds, AISO: Single): Single;
var
  N, T, S: Single;
begin
  N := Max(0.5, AApertureFStops);
  T := Max(1e-6, AShutterSeconds);
  S := Max(1.0, AISO);
  Result := Log2(((N * N) / T) * (100.0 / S));
end;

class function TCastleShaderGeneratorRHI.EvaluateExposureMultiplierFromEV100(
  const AEV100: Single): Single;
begin
  Result := 1.0 / (1.2 * Power(2.0, AEV100));
end;

function TCastleShaderGeneratorRHI.GenerateAgXGamutCompressionFunctions: String;
begin
  Result :=
    '// Phase 49.2: AgX Filmic Log Encoding & Highlight Chroma Gamut Compression' + NL +
    'vec3 EvaluateChromaGamutCompression(vec3 linearRGB, float compressionStrength) {' + NL +
    '  float luma = dot(linearRGB, vec3(0.2126, 0.7152, 0.0722));' + NL +
    '  float maxCh = max(max(linearRGB.r, linearRGB.g), linearRGB.b);' + NL +
    '  float highlightFactor = maxCh / (1.0 + maxCh);' + NL +
    '  float desat = clamp(highlightFactor * clamp(compressionStrength, 0.0, 1.0), 0.0, 1.0);' + NL +
    '  return mix(linearRGB, vec3(luma), desat);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateChromaGamutCompression(
  const ALinearRGB: TVector3;
  const ACompressionStrength: Single): TVector3;
var
  Luma, MaxCh, HighlightFactor, Desat: Single;
begin
  Luma := ALinearRGB.X * 0.2126 + ALinearRGB.Y * 0.7152 + ALinearRGB.Z * 0.0722;
  MaxCh := Max(Max(ALinearRGB.X, ALinearRGB.Y), ALinearRGB.Z);
  HighlightFactor := MaxCh / (1.0 + MaxCh);
  Desat := EnsureRange(HighlightFactor * EnsureRange(ACompressionStrength, 0.0, 1.0), 0.0, 1.0);
  Result := ALinearRGB * (1.0 - Desat) + Vector3(Luma, Luma, Luma) * Desat;
end;

function TCastleShaderGeneratorRHI.GenerateCapsuleOcclusionFunctions: String;
begin
  Result :=
    '// Phase 50.1: Analytical Capsule Soft Shadow & Ambient Occlusion' + NL +
    'float EvaluateCapsuleSoftShadowOcclusion(vec3 shadingPos, vec3 lightDir,' + NL +
    '  vec3 capA, vec3 capB, float capRadius, float coneTang) {' + NL +
    '  vec3 ba = capB - capA;' + NL +
    '  float l2 = dot(ba, ba);' + NL +
    '  vec3 mid = 0.5 * (capA + capB);' + NL +
    '  vec3 oc = mid - shadingPos;' + NL +
    '  float tRay = max(0.001, dot(oc, lightDir));' + NL +
    '  vec3 rayPt = shadingPos + lightDir * tRay;' + NL +
    '  float hSeg = (l2 > 1e-6) ? clamp(dot(rayPt - capA, ba) / l2, 0.0, 1.0) : 0.0;' + NL +
    '  vec3 closestCap = capA + ba * hSeg;' + NL +
    '  float dist = length(rayPt - closestCap) - capRadius;' + NL +
    '  float coneWidth = max(1e-4, tRay * max(1e-4, coneTang));' + NL +
    '  float u = clamp(0.5 + 0.5 * (dist / coneWidth), 0.0, 1.0);' + NL +
    '  return u * u * (3.0 - 2.0 * u);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateCapsuleSoftShadowOcclusion(
  const AShadingPos, ALightDir, ACapsuleA, ACapsuleB: TVector3;
  const ACapsuleRadius, AConeTang: Single): Single;
var
  BA, Mid, OC, RayPt, ClosestCap: TVector3;
  L2, TRay, HSeg, Dist, ConeWidth, U: Single;
begin
  BA := ACapsuleB - ACapsuleA;
  L2 := TVector3.DotProduct(BA, BA);
  Mid := (ACapsuleA + ACapsuleB) * 0.5;
  OC := Mid - AShadingPos;
  TRay := Max(0.001, TVector3.DotProduct(OC, ALightDir));
  RayPt := AShadingPos + ALightDir * TRay;
  if L2 > 1e-6 then
    HSeg := EnsureRange(TVector3.DotProduct(RayPt - ACapsuleA, BA) / L2, 0.0, 1.0)
  else
    HSeg := 0.0;
  ClosestCap := ACapsuleA + BA * HSeg;
  Dist := (RayPt - ClosestCap).Length - ACapsuleRadius;
  ConeWidth := Max(1e-4, TRay * Max(1e-4, AConeTang));
  U := EnsureRange(0.5 + 0.5 * (Dist / ConeWidth), 0.0, 1.0);
  Result := U * U * (3.0 - 2.0 * U);
end;

function TCastleShaderGeneratorRHI.GenerateEpipolarLightShaftFunctions: String;
begin
  Result :=
    '// Phase 50.2: Epipolar Volumetric Light Shafts (Crepuscular God Rays)' + NL +
    'vec2 EvaluateEpipolarSliceRayStep(vec2 lightScreenUV, vec2 sliceEndpointUV, float sliceFraction) {' + NL +
    '  return mix(lightScreenUV, sliceEndpointUV, clamp(sliceFraction, 0.0, 1.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateEpipolarSliceRayStep(
  const ALightScreenUV, ASliceEndpointUV: TVector2;
  const ASliceFraction: Single): TVector2;
var
  T: Single;
begin
  T := EnsureRange(ASliceFraction, 0.0, 1.0);
  Result := ALightScreenUV * (1.0 - T) + ASliceEndpointUV * T;
end;

function TCastleShaderGeneratorRHI.GenerateCooperativeMatrixPreamble: String;
begin
  Result :=
    '// Phase 51.1: Cooperative Matrix / Hardware Tensor Core Intrinsics (VK_KHR_cooperative_matrix)' + NL +
    '#extension GL_KHR_cooperative_matrix : enable' + NL +
    '#extension GL_KHR_memory_scope_semantics : enable' + NL +
    '#extension GL_EXT_shader_explicit_arithmetic_types_float16 : enable' + NL +
    'float EvaluateQuantizedFP8ToFloat(uint fp8Byte) {' + NL +
    '  uint signBit = (fp8Byte >> 7u) & 1u;' + NL +
    '  uint expBits = (fp8Byte >> 3u) & 15u;' + NL +
    '  uint mantBits = fp8Byte & 7u;' + NL +
    '  if (expBits == 0u && mantBits == 0u) return 0.0;' + NL +
    '  float m = (expBits == 0u) ? (float(mantBits) / 8.0) : (1.0 + float(mantBits) / 8.0);' + NL +
    '  int e = (expBits == 0u) ? -6 : (int(expBits) - 7);' + NL +
    '  float val = ldexp(m, e);' + NL +
    '  return (signBit != 0u) ? -val : val;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateQuantizedFP8ToFloat(
  const AFP8Byte: Byte): Single;
var
  SignBit, ExpBits, MantBits: Byte;
  M: Single;
  E: Integer;
begin
  SignBit := (AFP8Byte shr 7) and 1;
  ExpBits := (AFP8Byte shr 3) and 15;
  MantBits := AFP8Byte and 7;
  if (ExpBits = 0) and (MantBits = 0) then
    Exit(0.0);
  if ExpBits = 0 then
  begin
    M := MantBits / 8.0;
    E := -6;
  end else
  begin
    M := 1.0 + (MantBits / 8.0);
    E := Integer(ExpBits) - 7;
  end;
  Result := M * Power(2.0, E);
  if SignBit <> 0 then
    Result := -Result;
end;

function TCastleShaderGeneratorRHI.GenerateAnisotropicKuwaharaFunctions: String;
begin
  Result :=
    '// Phase 51.2: Anisotropic Kuwahara Structure-Tensor Filter' + NL +
    'vec2 EvaluateStructureTensorEigenOrientation(float jxx, float jyy, float jxy) {' + NL +
    '  float angle = 0.5 * atan(2.0 * jxy, jxx - jyy);' + NL +
    '  float d = sqrt(max(0.0, (jxx - jyy) * (jxx - jyy) + 4.0 * jxy * jxy));' + NL +
    '  float l1 = 0.5 * (jxx + jyy + d);' + NL +
    '  float l2 = 0.5 * (jxx + jyy - d);' + NL +
    '  float aniso = (l1 + l2 > 1e-6) ? clamp((l1 - l2) / (l1 + l2), 0.0, 1.0) : 0.0;' + NL +
    '  return vec2(angle, aniso);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateStructureTensorEigenOrientation(
  const AJxx, AJyy, AJxy: Single;
  out AAnisotropy: Single): Single;
var
  D, L1, L2: Single;
begin
  Result := 0.5 * ArcTan2(2.0 * AJxy, AJxx - AJyy);
  D := Sqrt(Max(0.0, Sqr(AJxx - AJyy) + 4.0 * Sqr(AJxy)));
  L1 := 0.5 * (AJxx + AJyy + D);
  L2 := 0.5 * (AJxx + AJyy - D);
  if (L1 + L2) > 1e-6 then
    AAnisotropy := EnsureRange((L1 - L2) / (L1 + L2), 0.0, 1.0)
  else
    AAnisotropy := 0.0;
end;

function TCastleShaderGeneratorRHI.GeneratePhysicalEyeRefractionFunctions: String;
begin
  Result :=
    '// Phase 52.1: Physically Based Eye Cornea Snell Refraction & Iris Parallax' + NL +
    'vec2 EvaluateEyeIrisRefractionUVOffset(vec3 viewDirTS, vec3 corneaNormalTS,' + NL +
    '  float corneaIOR, float anteriorChamberDepth) {' + NL +
    '  float eta = 1.0 / max(1.0, corneaIOR);' + NL +
    '  vec3 refracted = refract(- normalize(viewDirTS), normalize(corneaNormalTS), eta);' + NL +
    '  float cosZ = max(0.15, abs(refracted.z));' + NL +
    '  return refracted.xy * (anteriorChamberDepth / cosZ);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateEyeIrisRefractionUVOffset(
  const AViewDirTS, ACorneaNormalTS: TVector3;
  const ACorneaIOR, AAnteriorChamberDepth: Single): TVector2;
var
  I, N, Refracted: TVector3;
  Eta, NdotI, K, CosZ: Single;
begin
  Eta := 1.0 / Max(1.0, ACorneaIOR);
  I := -AViewDirTS.Normalize;
  N := ACorneaNormalTS.Normalize;
  NdotI := TVector3.DotProduct(N, I);
  K := 1.0 - Eta * Eta * (1.0 - NdotI * NdotI);
  if K < 0.0 then
    Refracted := TVector3.Zero
  else
    Refracted := I * Eta - N * (Eta * NdotI + Sqrt(K));
  CosZ := Max(0.15, Abs(Refracted.Z));
  Result := Vector2(
    Refracted.X * (AAnteriorChamberDepth / CosZ),
    Refracted.Y * (AAnteriorChamberDepth / CosZ));
end;

function TCastleShaderGeneratorRHI.GenerateSixPointLightmapFunctions: String;
begin
  Result :=
    '// Phase 52.2: Six-Point Directional Lightmap Basis (6-Axis Smoke/Cloud Lighting)' + NL +
    'float EvaluateSixPointDirectionalBasis(vec3 lightDirTS, vec3 mapPosXNegXPosY, vec3 mapNegYPosZNegZ) {' + NL +
    '  vec3 d = normalize(lightDirTS);' + NL +
    '  vec3 d2 = d * d;' + NL +
    '  float wx = (d.x >= 0.0) ? mapPosXNegXPosY.x : mapPosXNegXPosY.y;' + NL +
    '  float wy = (d.y >= 0.0) ? mapPosXNegXPosY.z : mapNegYPosZNegZ.x;' + NL +
    '  float wz = (d.z >= 0.0) ? mapNegYPosZNegZ.y : mapNegYPosZNegZ.z;' + NL +
    '  return d2.x * wx + d2.y * wy + d2.z * wz;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSixPointDirectionalBasis(
  const ALightDirTS, AMapPosXNegXPosY, AMapNegYPosZNegZ: TVector3): Single;
var
  D: TVector3;
  WX, WY, WZ: Single;
begin
  D := ALightDirTS.Normalize;
  if D.X >= 0.0 then WX := AMapPosXNegXPosY.X else WX := AMapPosXNegXPosY.Y;
  if D.Y >= 0.0 then WY := AMapPosXNegXPosY.Z else WY := AMapNegYPosZNegZ.X;
  if D.Z >= 0.0 then WZ := AMapNegYPosZNegZ.Y else WZ := AMapNegYPosZNegZ.Z;
  Result := Sqr(D.X) * WX + Sqr(D.Y) * WY + Sqr(D.Z) * WZ;
end;

function TCastleShaderGeneratorRHI.GenerateParallaxOcclusionMappingFunctions: String;
begin
  Result :=
    '// Phase 53.1: Secant-Refined Parallax Occlusion Mapping (POM)' + NL +
    'float EvaluateSecantRefinedPOMIntersection(float prevLayerDepth, float currLayerDepth,' + NL +
    '  float prevHeight, float currHeight) {' + NL +
    '  float afterDepth = currHeight - currLayerDepth;' + NL +
    '  float beforeDepth = prevHeight - prevLayerDepth;' + NL +
    '  float denom = afterDepth - beforeDepth;' + NL +
    '  float weight = (abs(denom) > 1e-6) ? clamp(afterDepth / denom, 0.0, 1.0) : 0.5;' + NL +
    '  return mix(currLayerDepth, prevLayerDepth, weight);' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateSecantRefinedPOMIntersection(
  const APrevLayerDepth, ACurrLayerDepth, APrevHeight, ACurrHeight: Single): Single;
var
  AfterDepth, BeforeDepth, Denom, Weight: Single;
begin
  AfterDepth := ACurrHeight - ACurrLayerDepth;
  BeforeDepth := APrevHeight - APrevLayerDepth;
  Denom := AfterDepth - BeforeDepth;
  if Abs(Denom) > 1e-6 then
    Weight := EnsureRange(AfterDepth / Denom, 0.0, 1.0)
  else
    Weight := 0.5;
  Result := ACurrLayerDepth * (1.0 - Weight) + APrevLayerDepth * Weight;
end;

function TCastleShaderGeneratorRHI.GenerateChromaticDispersionFlareFunctions: String;
begin
  Result :=
    '// Phase 53.2: Spectral Cauchy Chromatic Dispersion & Lens Flare' + NL +
    'vec3 EvaluateCauchyRefractiveIndexRGB(float coeffA, float coeffBMicronsSq) {' + NL +
    '  const vec3 invLambdaSq = vec3(1.0 / (0.65 * 0.65), 1.0 / (0.55 * 0.55), 1.0 / (0.45 * 0.45));' + NL +
    '  return vec3(coeffA) + coeffBMicronsSq * invLambdaSq;' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.EvaluateCauchyRefractiveIndexRGB(
  const ACoeffA, ACoeffBMicronsSq: Single): TVector3;
begin
  Result := Vector3(
    ACoeffA + ACoeffBMicronsSq / (0.65 * 0.65),
    ACoeffA + ACoeffBMicronsSq / (0.55 * 0.55),
    ACoeffA + ACoeffBMicronsSq / (0.45 * 0.45));
end;

class function TCastleShaderGeneratorRHI.BuildShapeRHIPushConstants(
  const AShapeModelView: TMatrix4;
  const ANormalMatrix: TMatrix3;
  const AHasSkinJoints, ADualQuaternionSkinning, ASpecularAntiAliasing: Boolean;
  const AShadowMapSystemModeOrd: Cardinal;
  const ABindlessMaterialIndex: Cardinal): TCastlePushConstantsBlock;
var
  SkinMode, SpecAA: Single;
begin
  Result.ModelViewMatrix := AShapeModelView;
  { GLSL mat3(v0, v1, v2) constructs a matrix from three COLUMN vectors }
  Result.NormalMatrixRow0 := Vector4(ANormalMatrix.Columns[0], 0.0);
  Result.NormalMatrixRow1 := Vector4(ANormalMatrix.Columns[1], 0.0);
  Result.NormalMatrixRow2 := Vector4(ANormalMatrix.Columns[2], 0.0);
  if not AHasSkinJoints then
    SkinMode := 0.0
  else if ADualQuaternionSkinning then
    SkinMode := 2.0
  else
    SkinMode := 1.0;
  if ASpecularAntiAliasing then
    SpecAA := 1.0
  else
    SpecAA := 0.0;
  Result.ObjectParams := Vector4(
    Single(ABindlessMaterialIndex),
    SkinMode,
    SpecAA,
    Single(AShadowMapSystemModeOrd));
end;

class procedure TCastleShaderGeneratorRHI.UnpackShapeRHIPushConstantsObjectParams(
  const APushConstants: TCastlePushConstantsBlock;
  out ABindlessMaterialIndex, ASkinningMode: Cardinal;
  out ASpecularAntiAliasing: Boolean;
  out AShadowMapSystemModeOrd: Cardinal);
begin
  ABindlessMaterialIndex := Cardinal(Max(0, Round(APushConstants.ObjectParams.X)));
  ASkinningMode := Cardinal(Max(0, Round(APushConstants.ObjectParams.Y)));
  ASpecularAntiAliasing := APushConstants.ObjectParams.Z > 0.5;
  AShadowMapSystemModeOrd := Cardinal(Max(0, Round(APushConstants.ObjectParams.W)));
end;

function TCastleShaderGeneratorRHI.GenerateHDRCompositeVertexShader: String;
begin
  Result :=
    '#version 450' + NL +
    '/* Phase 111: Procedural Fullscreen Triangle Vertex Shader (HDRCompositeVS) */' + NL +
    'layout(location = 0) out vec2 v_TexCoord;' + NL +
    'void main()' + NL +
    '{' + NL +
    '  vec2 uv = vec2(float((gl_VertexIndex << 1) & 2), float(gl_VertexIndex & 2));' + NL +
    '  gl_Position = vec4(uv * 2.0 - 1.0, 0.0, 1.0);' + NL +
    '  v_TexCoord = vec2(uv.x, 1.0 - uv.y);' + NL +
    '}' + NL;
end;

function TCastleShaderGeneratorRHI.GeneratePostProcessModulateComputeShader(
  const AWorkgroupSizeX, AWorkgroupSizeY: Cardinal): String;
begin
  Result :=
    '#version 450' + NL +
    Format('layout(local_size_x = %d, local_size_y = %d, local_size_z = 1) in;',
      [Max(Cardinal(1), AWorkgroupSizeX), Max(Cardinal(1), AWorkgroupSizeY)]) + NL +
    'layout(set = 0, binding = 0) uniform sampler2D u_HDRSceneTexture;' + NL +
    'layout(set = 0, binding = 1) uniform sampler2D u_GTAOTexture;' + NL +
    'layout(set = 0, binding = 2) uniform sampler2D u_SSRTexture;' + NL +
    'layout(set = 0, binding = 3, rgba16f) uniform writeonly image2D u_OutModulatedHDR;' + NL +
    'layout(push_constant) uniform PostProcessModulatePC {' + NL +
    '  float AOStrength;' + NL +
    '  float SSRStrength;' + NL +
    '  uint DstWidth;' + NL +
    '  uint DstHeight;' + NL +
    '} pc;' + NL +
    'void main()' + NL +
    '{' + NL +
    '  uvec2 gid = gl_GlobalInvocationID.xy;' + NL +
    '  if (gid.x >= pc.DstWidth || gid.y >= pc.DstHeight) return;' + NL +
    '  vec2 uv = (vec2(gid) + vec2(0.5)) / max(vec2(pc.DstWidth, pc.DstHeight), vec2(1.0));' + NL +
    '  vec3 hdr = textureLod(u_HDRSceneTexture, uv, 0.0).rgb;' + NL +
    '  float ao = clamp(textureLod(u_GTAOTexture, uv, 0.0).r, 0.0, 1.0);' + NL +
    '  vec3 ssr = textureLod(u_SSRTexture, uv, 0.0).rgb;' + NL +
    '  vec3 lit = max(hdr * mix(1.0, ao, pc.AOStrength) + ssr * pc.SSRStrength, vec3(0.0));' + NL +
    '  imageStore(u_OutModulatedHDR, ivec2(gid), vec4(lit, 1.0));' + NL +
    '}' + NL;
end;

class function TCastleShaderGeneratorRHI.ComputeSourceHash(const FinalGLSL: String): UInt64;
const
  FNV_OFFSET_BASIS: UInt64 = QWord($CBF29CE484222325);
  FNV_PRIME: UInt64 = $100000001B3;
var
  I: Integer;
begin
  {$Q-}{$R-}
  Result := FNV_OFFSET_BASIS;
  for I := 1 to Length(FinalGLSL) do
  begin
    Result := Result xor Ord(FinalGLSL[I]);
    Result := Result * FNV_PRIME;
  end;
  {$Q+}{$R+}
end;

end.
