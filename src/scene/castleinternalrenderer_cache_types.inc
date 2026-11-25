{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types used by TRendererContextCache. }

{$ifdef read_interface}
//type
  TTextureImageCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: String;
    FlipVertically: Boolean;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLName: TGLTexture;
    RHITexture: TRHITexture;
    RHIOwnerDevice: TRHIDevice;
    AllocatedWidth, AllocatedHeight, TextureScale: Cardinal;
  end;
  TTextureImageCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureImageCache>;

  TTextureVideoCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: String;
    FlipVertically: Boolean;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap2D;
    GUITexture: boolean;
    References: Cardinal;
    GLVideo: TGLVideo3D;
  end;
  TTextureVideoCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureVideoCache>;

  TTextureCubeMapCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: String;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    GUITexture: Boolean;
    References: Cardinal;
    GLName: TGLTexture;
    RHITexture: TRHITexture;
    RHIOwnerDevice: TRHIDevice;
    TextureScale: Cardinal;
  end;
  TTextureCubeMapCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureCubeMapCache>;

  TTexture3DCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: String;
    Filter: TTextureFilter;
    Anisotropy: TGLfloat;
    Wrap: TTextureWrap3D;
    References: Cardinal;
    GLName: TGLTexture;
    RHITexture: TRHITexture;
    RHIOwnerDevice: TRHIDevice;
  end;
  TTexture3DCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTexture3DCache>;

  { Cached depth or float texture.
    For now, depth and float textures require the same fields. }
  TTextureDepthOrFloatCache = class
    { Full URL of used texture image. Empty ('') if not known
      (or maybe this texture didn't come from any URL, e.g. it's generated). }
    FullUrl: String;
    Wrap: TTextureWrap2D;
    References: Cardinal;
    GLName: TGLTexture;
  end;
  TTextureDepthOrFloatCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TTextureDepthOrFloatCache>;

  TX3DRendererShape = class;
  TVboType = (vtCoordinate, vtAttribute, vtIndex);
  TVboTypes = set of TVboType;
  TVboArrays = array [TVboType] of TGLBuffer;

  { Cached shape resources. }
  TShapeCache = class
  private
    RenderOptions: TCastleRenderOptions;
    Geometry: TAbstractGeometryNode;
    State: TX3DGraphTraverseState;
    FogVolumetric: boolean;
    FogVolumetricDirection: TVector3;
    FogVolumetricVisibilityStart: Single;
    References: Cardinal;

    { An instance of TGeometryArrays, decomposing this shape geometry.
      Used to easily render and process this geometry.

      When VBO are supported (all non-ancient GPUs) then TShapeCache
      uses an instance of it only once (in LoadArraysToVbo,
      to load contents to GPU, and then calls Arrays.FreeData).
      But it is later used by outside code multiple times to make draw calls
      (as it is passed by TBaseCoordinateRenderer).
      So storing it in TShapeCache (to be shared when possible) makes sense.

      TShapeCache owns this -- it will be freed when TShapeCache is freed. }
    Arrays: TGeometryArrays;

    { What VBOs do we need to reload at next LoadArraysToVbo call.
      This also implies that content for this VBOs needs to be created,
      which may mean we need to change Arrays.
      This is extended at each InvalidateVertexData call. }
    VboToReload: TVboTypes;

    Vbo: TVboArrays;
    VboAllocatedUsage: TGLenum;
    VboAllocatedSize: array [TVboType] of Cardinal;
    VboCoordinatePreserveGeometryOrder: Boolean; //< copied from TGeometryArrays.CoordinatePreserveGeometryOrder

    Vao: TVertexArrayObject;
    FRHICoordBuffer: TRHIBuffer;
    FRHIAttribBuffer: TRHIBuffer;
    FRHIIndexBuffer: TRHIBuffer;
    FRHIJointBuffer: TRHIBuffer;
    FRHISkinningUBOBuffer: TRHIBuffer;
    FRHIMaterialBuffer: TRHIBuffer;
    FRHIMaterialDescriptorLayout: TRHIDescriptorSetLayout;
    FRHIMaterialDescriptorSet: TRHIDescriptorSet;
    FRHIBaseColorSampler: TRHISampler;
    FRHINormalSampler: TRHISampler;
    FRHIMetallicRoughnessSampler: TRHISampler;
    FRHIEmissiveSampler: TRHISampler;
    FRHIOcclusionSampler: TRHISampler;
    FRHIEnvSpecularTexture: TRHITexture;
    FRHIEnvSpecularSampler: TRHISampler;
    FRHIBrdfLutTexture: TRHITexture;
    FRHIBrdfLutSampler: TRHISampler;
    FRHIDefaultBrdfLutTexture: TRHITexture;
    FRHITextureTransform: TMatrix4;
    FRHIHasTextureTransform: Boolean;
    FRHITextureTransform1: TMatrix4;
    FRHIHasTextureTransform1: Boolean;
    FRHIHasSecondTexture: Boolean;
    FRHISecondTextureCombineRGB: TCombine;
    FRHISecondTextureCombineAlpha: TCombine;
    FRHISecondTextureConstantColor: TVector4;
    FRHISecondTextureEnv: TTextureEnv;
    FRHIHasMultiTextureStage: array[2..3] of Boolean;
    FRHIMultiTextureStageEnv: array[2..3] of TTextureEnv;
    FRHIMultiTextureStageConstantColor: array[2..3] of TVector4;
    FRHIMultiTextureStageDimension: array[1..3] of TRHITextureDimension;
    FRHIBaseColorTextureDimension: TRHITextureDimension;
    FRHIClipPlane: TVector4;
    FRHIHasClipPlane: Boolean;
    FRHIClipPlanes: array[0..3] of TVector4;
    FRHIClipPlaneCount: Cardinal;
    FRHIShadowSampling: TShadowSampling;
    FRHIShadowSamplingExplicit: Boolean;
    FRHIShadowMapSize: Cardinal;
    FRHIBaseColorTexCoordIndex: Cardinal;
    FRHINormalTexCoordIndex: Cardinal;
    FRHIMetallicRoughnessTexCoordIndex: Cardinal;
    FRHIEmissiveTexCoordIndex: Cardinal;
    FRHIOcclusionTexCoordIndex: Cardinal;
    FRHITexCoordGen: TTextureCoordinateGeneration;
    FRHITexGenBoundsS: TVector4;
    FRHITexGenBoundsT: TVector4;
    FRHITexGenBoundsR: TVector4;
    FRHITexGenProjectorMatrix: TMatrix4;
    FRHIHasMirrorPlaneUniforms: Boolean;
    FRHIMirrorNormalizedPlane: TVector4;
    FRHIMirrorCameraPositionOnPlane: TVector3;
    FRHIMirrorCameraSide: TVector3;
    FRHIMirrorCameraUp: TVector3;
    FRHIMirrorFrustumDimensions: TVector4;
    FRHIPointSize: Single;
    FRHILineType: TLineType;
    FRHILineStipplePattern: Word;
    FRHIHasParallaxBumpMapping: Boolean;
    FRHISteepParallaxBumpMapping: Boolean;
    FRHISteepParallaxSelfShadowing: Boolean;
    FRHIParallaxHeight: Single;
    FRHIIsPhysicalMaterial: Boolean;
    FRHIIsPhongMaterial: Boolean;
    FRHIPhongHasSpecularTex: Boolean;
    FRHIPhongHasShininessTex: Boolean;
    FRHIPhongHasSeparateShininessTex: Boolean;
    FRHIPhongHasAmbientTex: Boolean;
    FRHIPhongAmbientIntensity: Single;
    FRHIHasAlphaTexture: Boolean;
    FRHIInvertAlphaTexture: Boolean;
    FRHIHasSeparateDiffuseEnvTex: Boolean;
    FRHIEnvLightColorIntensity: TVector3;
    FRHIVertexEffectCode: String;
    FRHIFragmentEffectCode: String;
    FRHIGeometryEffectCode: String;
    FRHIComposedVertSource: String;
    FRHIComposedFragSource: String;
    FRHIComposedGeomSource: String;
    FRHIUserEffectLayout: TUserEffectShaderLayout;
    FRHIAltUserEffectLayout: TUserEffectShaderLayout;
    FRHIUserEffectBuffer: TRHIBuffer;
    FRHISet2FallbackUBO: TRHIBuffer;
    FRHIUserEffectUploadCount: Cardinal;
    FRHIUserEffectTextures: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHITexture;
    FRHIUserEffectSamplers: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHISampler;
    FRHISkinningDescriptorLayout: TRHIDescriptorSetLayout;
    FRHISkinningDescriptorSet: TRHIDescriptorSet;
    FRHIVertexShader: TRHIShaderModule;
    FRHIFragmentShader: TRHIShaderModule;
    FRHIGeometryShader: TRHIShaderModule;
    FRHIVertexShaderSource: String;
    FRHIFragmentShaderSource: String;
    FRHIGeometryShaderSource: String;
    FRHIShaderPermutationMask: QWord;
    FRHIShadersInitialized: Boolean;
    FRHIAltVertexShader: TRHIShaderModule;
    FRHIAltFragmentShader: TRHIShaderModule;
    FRHIAltGeometryShader: TRHIShaderModule;
    FRHIAltVertexShaderSource: String;
    FRHIAltFragmentShaderSource: String;
    FRHIAltGeometryShaderSource: String;
    FRHIAltShaderPermutationMask: QWord;
    FRHIAltShadersInitialized: Boolean;
    FRHIShaderCompileCount: Cardinal;
    FRHIOwnerDevice: TRHIDevice;
    FRHIMaterialBlock: TCastleMaterialUniformBlock;
    FRHIMaterialUploadCount: Cardinal;
    FRHIUploadCount: Cardinal;
    FRHIUserEffectSlots: array of record
      Buffer: TRHIBuffer;
      DescriptorSet: TRHIDescriptorSet;
      Data: TBytes;
      SkinningBuffer: TRHIBuffer;
      Textures: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHITexture;
      Samplers: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHISampler;
    end;
    FActiveRHIUserEffectSlot: Integer;
    FNextRHIUserEffectSlotEvict: Cardinal;

    { Like TX3DRendererShape.LoadArraysToVbo,
      but takes explicit DynamicGeometry. }
    procedure LoadArraysToVbo(const DynamicGeometry: boolean);
    procedure GLContextClose;
    procedure FreeRHIUserEffectSlots;
    procedure EnsureRHIDeviceMatch;
    procedure EnsureRHISet2DescriptorSet;
    function GetActiveRHIUserEffectBuffer: TRHIBuffer;
    function GetActiveRHISkinningDescriptorSet: TRHIDescriptorSet;
    function GetRHIUserEffectTexture(const AIndex: Cardinal): TRHITexture;
    function GetRHIUserEffectSampler(const AIndex: Cardinal): TRHISampler;
    function GetRHIClipPlane(const AIndex: Cardinal): TVector4;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InvalidateVertexData(const Changed: TVboTypes);
    procedure UploadJointMatrices(const JointMatrices: Pointer; const Count: Cardinal);
    procedure UploadUserEffectBlock(const AData: Pointer; const ASize: PtrUInt);
    procedure BindUserEffectTexture(const ASlotIndex: Cardinal;
      const ATexture: TRHITexture; const ASampler: TRHISampler = nil);
    procedure UploadMaterialBlock(const AMatBlock: TCastleMaterialUniformBlock);
    procedure BindMaterialTexturesToDescriptorSet(
      const ABaseColorTex, ANormalTex, AMetallicRoughnessTex,
            AEmissiveTex, AOcclusionTex: TRHITexture;
      const ABaseColorSampler: TRHISampler = nil;
      const ANormalSampler: TRHISampler = nil;
      const AMetallicRoughnessSampler: TRHISampler = nil;
      const AEmissiveSampler: TRHISampler = nil;
      const AOcclusionSampler: TRHISampler = nil);
    procedure BindEnvironmentTexturesToDescriptorSet(
      const AEnvSpecularTex: TRHITexture;
      const AEnvSpecularSampler: TRHISampler = nil;
      const ABrdfLutTex: TRHITexture = nil;
      const ABrdfLutSampler: TRHISampler = nil);
    procedure SetTextureTransform(const AHasTransform: Boolean; const AMatrix: TMatrix4);
    procedure SetTextureTransform1(const AHasTransform: Boolean; const AMatrix: TMatrix4);
    procedure SetSecondTexture(
      const AHasSecondTex: Boolean;
      const ACombineRGB: TCombine = coModulate;
      const ACombineAlpha: TCombine = coModulate); overload;
    procedure SetSecondTexture(
      const AHasSecondTex: Boolean;
      const ACombineRGB, ACombineAlpha: TCombine;
      const AConstantColor: TVector4); overload;
    procedure SetSecondTextureEnv(
      const AHasSecondTex: Boolean;
      const AEnv: TTextureEnv;
      const AConstantColor: TVector4);
    procedure SetMultiTextureStageEnv(
      const AStageIndex: Cardinal;
      const AHasStageTex: Boolean;
      const AEnv: TTextureEnv;
      const AConstantColor: TVector4);
    procedure SetMultiTextureStageDimension(
      const AStageIndex: Cardinal;
      const ADimension: TRHITextureDimension);
    procedure SetClipPlane(const AHasClipPlane: Boolean; const APlane: TVector4);
    procedure SetClipPlanes(const APlanes: array of TVector4; const ACount: Cardinal);
    procedure SetShadowMapParams(const ASampling: TShadowSampling; const ASize: Cardinal = 128);
    procedure SetTextureCoordMappings(
      const ABaseColorIdx, ANormalIdx, AMetallicRoughnessIdx,
            AEmissiveIdx, AOcclusionIdx: Cardinal);
    procedure SetTexCoordGen(const AGeneration: TTextureCoordinateGeneration;
      const ABoundsS, ABoundsT: TVector4); overload;
    procedure SetTexCoordGen(const AGeneration: TTextureCoordinateGeneration;
      const ABoundsS, ABoundsT, ABoundsR: TVector4); overload;
    procedure SetTexCoordGenProjectorMatrix(const AMatrix: TMatrix4);
    procedure SetMirrorPlaneUniforms(
      const AHasMirrorUniforms: Boolean;
      {$ifdef FPC}constref{$else}const{$endif} ANormalizedPlane: TVector4;
      {$ifdef FPC}constref{$else}const{$endif} ACameraPositionOnPlane, ACameraSide, ACameraUp: TVector3;
      {$ifdef FPC}constref{$else}const{$endif} AFrustumDimensions: TVector4);
    procedure SetPointSize(const APointSize: Single);
    procedure SetLineType(const ALineType: TLineType);
    procedure SetParallaxBumpMapping(const AEnabled, ASteep: Boolean; const AHeight: Single); overload;
    procedure SetParallaxBumpMapping(const AEnabled, ASteep, ASelfShadowing: Boolean; const AHeight: Single); overload;
    procedure SetIsPhysicalMaterial(const AIsPhysical: Boolean);
    procedure SetPhongSurfaceParams(
      const AIsPhong, AHasSpecularTex, AHasShininessTex, AHasAmbientTex: Boolean;
      const AAmbientIntensity: Single);
    procedure SetPhongExtendedSurfaceParams(
      const AHasSeparateShininessTex, AHasAlphaTex, AInvertAlpha: Boolean);
    procedure SetEnvironmentLightParams(
      const AHasSeparateDiffuse: Boolean;
      const AColorIntensity: TVector3);
    procedure SetShaderEffects(const AVertexEffectCode, AFragmentEffectCode: String); overload;
    procedure SetShaderEffects(const AVertexEffectCode, AFragmentEffectCode, AGeometryEffectCode: String); overload;
    procedure SetComposedShader(const ACustomVertSource, ACustomFragSource: String); overload;
    procedure SetComposedShader(const ACustomVertSource, ACustomFragSource, ACustomGeomSource: String); overload;
    procedure EnsureRHIShaders(
      const AHasSkinning, AHasTangents,
            AHasBaseColorTex, AHasNormalTex, AHasMetallicRoughnessTex,
            AHasEmissiveTex, AHasOcclusionTex, AUseAlphaTest: Boolean;
      const AHasColorPerVertex: Boolean = False;
      const AHasTextureTransform: Boolean = False;
      const AHasClipPlane: Boolean = False;
      const AHasIBL: Boolean = False;
      const AHasShadowMap: Boolean = False;
      const AHasTexCoordGen: Boolean = False;
      const AHasShaderEffects: Boolean = False;
      const AHasComposedShader: Boolean = False);
    { Debug description of this shape cache. }
    function ToString: String; override;

    property RHICoordinateBuffer: TRHIBuffer read FRHICoordBuffer;
    property RHIRenderOptions: TCastleRenderOptions read RenderOptions write RenderOptions;
    property GeometryArrays: TGeometryArrays read Arrays;
    property RHIAttributeBuffer: TRHIBuffer read FRHIAttribBuffer;
    property RHIIndexBuffer: TRHIBuffer read FRHIIndexBuffer;
    property RHIJointBuffer: TRHIBuffer read FRHIJointBuffer;
    property RHISkinningUBOBuffer: TRHIBuffer read FRHISkinningUBOBuffer;
    property RHIMaterialBuffer: TRHIBuffer read FRHIMaterialBuffer;
    property RHIMaterialDescriptorLayout: TRHIDescriptorSetLayout read FRHIMaterialDescriptorLayout;
    property RHIMaterialDescriptorSet: TRHIDescriptorSet read FRHIMaterialDescriptorSet;
    property RHIBaseColorSampler: TRHISampler read FRHIBaseColorSampler;
    property RHINormalSampler: TRHISampler read FRHINormalSampler;
    property RHIMetallicRoughnessSampler: TRHISampler read FRHIMetallicRoughnessSampler;
    property RHIEmissiveSampler: TRHISampler read FRHIEmissiveSampler;
    property RHIOcclusionSampler: TRHISampler read FRHIOcclusionSampler;
    property RHIEnvSpecularTexture: TRHITexture read FRHIEnvSpecularTexture;
    property RHIEnvSpecularSampler: TRHISampler read FRHIEnvSpecularSampler;
    property RHIBrdfLutTexture: TRHITexture read FRHIBrdfLutTexture;
    property RHIBrdfLutSampler: TRHISampler read FRHIBrdfLutSampler;
    property RHITextureTransform: TMatrix4 read FRHITextureTransform;
    property RHIHasTextureTransform: Boolean read FRHIHasTextureTransform;
    property RHITextureTransform1: TMatrix4 read FRHITextureTransform1;
    property RHIHasTextureTransform1: Boolean read FRHIHasTextureTransform1;
    property RHIHasSecondTexture: Boolean read FRHIHasSecondTexture;
    property RHISecondTextureCombineRGB: TCombine read FRHISecondTextureCombineRGB;
    property RHISecondTextureCombineAlpha: TCombine read FRHISecondTextureCombineAlpha;
    property RHISecondTextureConstantColor: TVector4 read FRHISecondTextureConstantColor;
    property RHISecondTextureEnv: TTextureEnv read FRHISecondTextureEnv;
    property RHIBaseColorTextureDimension: TRHITextureDimension read FRHIBaseColorTextureDimension;
    property RHIClipPlane: TVector4 read FRHIClipPlane;
    property RHIHasClipPlane: Boolean read FRHIHasClipPlane;
    property RHIClipPlaneCount: Cardinal read FRHIClipPlaneCount;
    property RHIClipPlanes[const AIndex: Cardinal]: TVector4 read GetRHIClipPlane;
    property RHIShadowSampling: TShadowSampling read FRHIShadowSampling;
    property RHIShadowMapSize: Cardinal read FRHIShadowMapSize;
    property RHIBaseColorTexCoordIndex: Cardinal read FRHIBaseColorTexCoordIndex;
    property RHINormalTexCoordIndex: Cardinal read FRHINormalTexCoordIndex;
    property RHIMetallicRoughnessTexCoordIndex: Cardinal read FRHIMetallicRoughnessTexCoordIndex;
    property RHIEmissiveTexCoordIndex: Cardinal read FRHIEmissiveTexCoordIndex;
    property RHIOcclusionTexCoordIndex: Cardinal read FRHIOcclusionTexCoordIndex;
    property RHITexCoordGen: TTextureCoordinateGeneration read FRHITexCoordGen;
    property RHITexGenBoundsS: TVector4 read FRHITexGenBoundsS;
    property RHITexGenBoundsT: TVector4 read FRHITexGenBoundsT;
    property RHITexGenBoundsR: TVector4 read FRHITexGenBoundsR;
    property RHITexGenProjectorMatrix: TMatrix4 read FRHITexGenProjectorMatrix;
    property RHIHasMirrorPlaneUniforms: Boolean read FRHIHasMirrorPlaneUniforms;
    property RHIMirrorNormalizedPlane: TVector4 read FRHIMirrorNormalizedPlane;
    property RHIMirrorCameraPositionOnPlane: TVector3 read FRHIMirrorCameraPositionOnPlane;
    property RHIMirrorCameraSide: TVector3 read FRHIMirrorCameraSide;
    property RHIMirrorCameraUp: TVector3 read FRHIMirrorCameraUp;
    property RHIMirrorFrustumDimensions: TVector4 read FRHIMirrorFrustumDimensions;
    property RHIPointSize: Single read FRHIPointSize;
    property RHILineType: TLineType read FRHILineType;
    property RHILineStipplePattern: Word read FRHILineStipplePattern;
    property RHIHasParallaxBumpMapping: Boolean read FRHIHasParallaxBumpMapping;
    property RHISteepParallaxBumpMapping: Boolean read FRHISteepParallaxBumpMapping;
    property RHISteepParallaxSelfShadowing: Boolean read FRHISteepParallaxSelfShadowing;
    property RHIParallaxHeight: Single read FRHIParallaxHeight;
    property RHIIsPhongMaterial: Boolean read FRHIIsPhongMaterial;
    property RHIPhongHasSpecularTex: Boolean read FRHIPhongHasSpecularTex;
    property RHIPhongHasShininessTex: Boolean read FRHIPhongHasShininessTex;
    property RHIPhongHasSeparateShininessTex: Boolean read FRHIPhongHasSeparateShininessTex;
    property RHIPhongHasAmbientTex: Boolean read FRHIPhongHasAmbientTex;
    property RHIPhongAmbientIntensity: Single read FRHIPhongAmbientIntensity;
    property RHIHasAlphaTexture: Boolean read FRHIHasAlphaTexture;
    property RHIInvertAlphaTexture: Boolean read FRHIInvertAlphaTexture;
    property RHIHasMultiTextureStage2: Boolean read FRHIHasMultiTextureStage[2];
    property RHIHasMultiTextureStage3: Boolean read FRHIHasMultiTextureStage[3];
    property RHIHasSeparateDiffuseEnvTex: Boolean read FRHIHasSeparateDiffuseEnvTex;
    property RHIEnvLightColorIntensity: TVector3 read FRHIEnvLightColorIntensity;
    property RHIVertexEffectCode: String read FRHIVertexEffectCode;
    property RHIFragmentEffectCode: String read FRHIFragmentEffectCode;
    property RHIGeometryEffectCode: String read FRHIGeometryEffectCode;
    property RHIComposedVertSource: String read FRHIComposedVertSource;
    property RHIComposedFragSource: String read FRHIComposedFragSource;
    property RHIComposedGeomSource: String read FRHIComposedGeomSource;
    property RHIUserEffectLayout: TUserEffectShaderLayout read FRHIUserEffectLayout;
    property RHIUserEffectBuffer: TRHIBuffer read GetActiveRHIUserEffectBuffer;
    property RHIUserEffectUploadCount: Cardinal read FRHIUserEffectUploadCount;
    property RHIUserEffectTexture[const AIndex: Cardinal]: TRHITexture read GetRHIUserEffectTexture;
    property RHIUserEffectSampler[const AIndex: Cardinal]: TRHISampler read GetRHIUserEffectSampler;
    property RHISkinningDescriptorLayout: TRHIDescriptorSetLayout read FRHISkinningDescriptorLayout;
    property RHISkinningDescriptorSet: TRHIDescriptorSet read GetActiveRHISkinningDescriptorSet;
    property RHIVertexShader: TRHIShaderModule read FRHIVertexShader;
    property RHIFragmentShader: TRHIShaderModule read FRHIFragmentShader;
    property RHIGeometryShader: TRHIShaderModule read FRHIGeometryShader;
    property RHIVertexShaderSource: String read FRHIVertexShaderSource;
    property RHIFragmentShaderSource: String read FRHIFragmentShaderSource;
    property RHIGeometryShaderSource: String read FRHIGeometryShaderSource;
    property RHIShaderPermutationMask: QWord read FRHIShaderPermutationMask;
    property RHIShaderCompileCount: Cardinal read FRHIShaderCompileCount;
    property RHIMaterialBlock: TCastleMaterialUniformBlock read FRHIMaterialBlock;
    property RHIMaterialUploadCount: Cardinal read FRHIMaterialUploadCount;
    property RHIUploadCount: Cardinal read FRHIUploadCount;
  end;

  TShapeCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TShapeCache>;

  TX3DGLSLProgram = class;

  TShaderProgramCache = class
  public
    { Hash of TShader code when initializing this shader
      by LinkProgram. Used to decide when shader needs to be regenerated,
      and when it can be shared. }
    Hash: TShaderCodeHash;

    { Actual GLSL program. May be @nil (if it failed to link). }
    ShaderProgram: TX3DGLSLProgram;

    References: Cardinal;

    destructor Destroy; override;
  end;

  TShaderProgramCacheList = {$ifdef FPC}specialize{$endif} TObjectList<TShaderProgramCache>;
{$endif read_interface}

{$ifdef read_implementation}

{ TShapeCache ---------------------------------------------------------------- }

constructor TShapeCache.Create;
begin
  inherited;
  VboToReload := AllVboTypes;
  FRHIPointSize := 1.0;
  FRHILineType := ltSolid;
  FRHILineStipplePattern := $FFFF;
  FRHIParallaxHeight := 0.01;
  FRHIPhongAmbientIntensity := 0.2;
  FRHIEnvLightColorIntensity := Vector3(1.0, 1.0, 1.0);
  FRHITexGenProjectorMatrix := TMatrix4.Identity;
  FRHIShadowMapSize := 128;
  FActiveRHIUserEffectSlot := -1;
end;

procedure TShapeCache.FreeRHIUserEffectSlots;
var
  SlotIdx: Integer;
begin
  for SlotIdx := 0 to High(FRHIUserEffectSlots) do
  begin
    FreeAndNil(FRHIUserEffectSlots[SlotIdx].DescriptorSet);
    FreeAndNil(FRHIUserEffectSlots[SlotIdx].Buffer);
  end;
  SetLength(FRHIUserEffectSlots, 0);
  FActiveRHIUserEffectSlot := -1;
  FNextRHIUserEffectSlotEvict := 0;
end;

destructor TShapeCache.Destroy;
begin
  FreeAndNil(Arrays);
  GLContextClose;
  inherited;
end;

procedure TShapeCache.GLContextClose;
var
  I: TVboType;
begin
  if Vbo[vtCoordinate] <> GLObjectNone then
  begin
    for I := Low(I) to High(I) do
    begin
      { All Vbo must be zero, or none. }
      Assert(Vbo[I] <> GLObjectNone);
      FreeBuffer(Vbo[I]);
    end;
  end;

  FreeAndNil(Vao);
  FreeAndNil(FRHICoordBuffer);
  FreeAndNil(FRHIAttribBuffer);
  FreeAndNil(FRHIIndexBuffer);
  FreeAndNil(FRHIJointBuffer);
  FreeAndNil(FRHISkinningUBOBuffer);
  FreeRHIUserEffectSlots;
  FreeAndNil(FRHIUserEffectBuffer);
  FreeAndNil(FRHISet2FallbackUBO);
  FreeAndNil(FRHIMaterialBuffer);
  FreeAndNil(FRHIMaterialDescriptorSet);
  FreeAndNil(FRHIMaterialDescriptorLayout);
  FreeAndNil(FRHISkinningDescriptorSet);
  FreeAndNil(FRHISkinningDescriptorLayout);
  FreeAndNil(FRHIDefaultBrdfLutTexture);
  FreeAndNil(FRHIVertexShader);
  FreeAndNil(FRHIFragmentShader);
  FreeAndNil(FRHIGeometryShader);
  FreeAndNil(FRHIAltVertexShader);
  FreeAndNil(FRHIAltFragmentShader);
  FreeAndNil(FRHIAltGeometryShader);
  FillChar(FRHIUserEffectTextures, SizeOf(FRHIUserEffectTextures), 0);
  FillChar(FRHIUserEffectSamplers, SizeOf(FRHIUserEffectSamplers), 0);
  FRHIBaseColorSampler := nil;
  FRHINormalSampler := nil;
  FRHIMetallicRoughnessSampler := nil;
  FRHIEmissiveSampler := nil;
  FRHIOcclusionSampler := nil;
  FRHIEnvSpecularTexture := nil;
  FRHIEnvSpecularSampler := nil;
  FRHIBrdfLutTexture := nil;
  FRHIBrdfLutSampler := nil;
  FRHIShadersInitialized := False;
  FRHIAltShadersInitialized := False;
  FRHIOwnerDevice := nil;
end;

procedure TShapeCache.EnsureRHIDeviceMatch;
begin
  if FRHIOwnerDevice <> RHIDevice then
  begin
    FreeAndNil(FRHICoordBuffer);
    FreeAndNil(FRHIAttribBuffer);
    FreeAndNil(FRHIIndexBuffer);
    FreeAndNil(FRHIJointBuffer);
    FreeAndNil(FRHISkinningUBOBuffer);
    FreeRHIUserEffectSlots;
    FreeAndNil(FRHIUserEffectBuffer);
    FreeAndNil(FRHISet2FallbackUBO);
    FillChar(FRHIUserEffectTextures, SizeOf(FRHIUserEffectTextures), 0);
    FillChar(FRHIUserEffectSamplers, SizeOf(FRHIUserEffectSamplers), 0);
    FreeAndNil(FRHIMaterialBuffer);
    FreeAndNil(FRHIMaterialDescriptorSet);
    FreeAndNil(FRHIMaterialDescriptorLayout);
    FRHIBaseColorSampler := nil;
    FRHINormalSampler := nil;
    FRHIMetallicRoughnessSampler := nil;
    FRHIEmissiveSampler := nil;
    FRHIOcclusionSampler := nil;
    FRHIEnvSpecularTexture := nil;
    FRHIEnvSpecularSampler := nil;
    FRHIBrdfLutTexture := nil;
    FRHIBrdfLutSampler := nil;
    FreeAndNil(FRHIDefaultBrdfLutTexture);
    FreeAndNil(FRHISkinningDescriptorSet);
    FreeAndNil(FRHISkinningDescriptorLayout);
    FreeAndNil(FRHIVertexShader);
    FreeAndNil(FRHIFragmentShader);
    FreeAndNil(FRHIGeometryShader);
    FreeAndNil(FRHIAltVertexShader);
    FreeAndNil(FRHIAltFragmentShader);
    FreeAndNil(FRHIAltGeometryShader);
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
    FRHIOwnerDevice := RHIDevice;
  end;
end;

procedure TShapeCache.EnsureRHISet2DescriptorSet;
var
  Bindings: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS + 1] of TRHIDescriptorBindingDesc;
  FallbackBufDesc: TRHIBufferDesc;
  ZeroBytes: array[0..63] of Byte;
  I: Integer;
begin
  if RHIDevice = nil then
    Exit;
  EnsureRHIDeviceMatch;
  if FRHISkinningDescriptorLayout = nil then
  begin
    FillChar(Bindings, SizeOf(Bindings), 0);
    { Binding 0: CastleSkinningBlock }
    Bindings[0].Binding := 0;
    Bindings[0].DescriptorType := dtUniformBuffer;
    Bindings[0].Count := 1;
    Bindings[0].StageFlags := [ssVertex];
    { Binding 1: CastleUserEffectBlock }
    Bindings[1].Binding := 1;
    Bindings[1].DescriptorType := dtUniformBuffer;
    Bindings[1].Count := 1;
    Bindings[1].StageFlags := [ssVertex, ssGeometry, ssFragment];
    { Bindings 2..9: Custom User Effect Samplers }
    for I := 2 to High(Bindings) do
    begin
      Bindings[I].Binding := Cardinal(I);
      Bindings[I].DescriptorType := dtCombinedImageSampler;
      Bindings[I].Count := 1;
      Bindings[I].StageFlags := [ssVertex, ssGeometry, ssFragment];
    end;
    FRHISkinningDescriptorLayout := RHIDevice.CreateDescriptorSetLayout(Bindings);
  end;
  if (FRHISkinningDescriptorSet = nil) and (FRHISkinningDescriptorLayout <> nil) then
  begin
    FRHISkinningDescriptorSet := RHIDevice.CreateDescriptorSet(FRHISkinningDescriptorLayout);
    if FRHISet2FallbackUBO = nil then
    begin
      FillChar(ZeroBytes, SizeOf(ZeroBytes), 0);
      FallbackBufDesc := TRHIBufferDesc.Create(SizeOf(ZeroBytes), [buUniform], muCpuToGpu, 'CastleSet2FallbackUBO');
      FRHISet2FallbackUBO := RHIDevice.CreateBuffer(FallbackBufDesc, @ZeroBytes[0]);
    end;
    if FRHISkinningDescriptorSet <> nil then
    begin
      if FRHISet2FallbackUBO <> nil then
      begin
        FRHISkinningDescriptorSet.BindUniformBuffer(0, FRHISet2FallbackUBO);
        FRHISkinningDescriptorSet.BindUniformBuffer(1, FRHISet2FallbackUBO);
      end;
      {$ifndef WASI}
      if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
        for I := 2 to High(Bindings) do
          FRHISkinningDescriptorSet.BindTexture(Cardinal(I), TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      {$endif}
      FRHISkinningDescriptorSet.Update;
    end;
  end;
end;

function TShapeCache.GetActiveRHIUserEffectBuffer: TRHIBuffer;
begin
  if (FActiveRHIUserEffectSlot >= 0) and (FActiveRHIUserEffectSlot <= High(FRHIUserEffectSlots)) then
    Result := FRHIUserEffectSlots[FActiveRHIUserEffectSlot].Buffer
  else
    Result := FRHIUserEffectBuffer;
end;

function TShapeCache.GetActiveRHISkinningDescriptorSet: TRHIDescriptorSet;
begin
  if (FActiveRHIUserEffectSlot >= 0) and (FActiveRHIUserEffectSlot <= High(FRHIUserEffectSlots)) then
    Result := FRHIUserEffectSlots[FActiveRHIUserEffectSlot].DescriptorSet
  else
    Result := FRHISkinningDescriptorSet;
end;

function TShapeCache.GetRHIUserEffectTexture(const AIndex: Cardinal): TRHITexture;
begin
  if AIndex <= High(FRHIUserEffectTextures) then
    Result := FRHIUserEffectTextures[AIndex]
  else
    Result := nil;
end;

function TShapeCache.GetRHIUserEffectSampler(const AIndex: Cardinal): TRHISampler;
begin
  if AIndex <= High(FRHIUserEffectSamplers) then
    Result := FRHIUserEffectSamplers[AIndex]
  else
    Result := nil;
end;

function TShapeCache.GetRHIClipPlane(const AIndex: Cardinal): TVector4;
begin
  if AIndex <= High(FRHIClipPlanes) then
    Result := FRHIClipPlanes[AIndex]
  else
    Result := TVector4.Zero;
end;

procedure TShapeCache.InvalidateVertexData(const Changed: TVboTypes);
begin
  VboToReload := VboToReload + Changed;
end;

procedure TShapeCache.UploadJointMatrices(const JointMatrices: Pointer; const Count: Cardinal);
var
  BufDesc: TRHIBufferDesc;
  DataSize: PtrUInt;
  {$ifndef WASI}
  SkinBlock: PCastleSkinningUniformBlock;
  ClampedCount: Cardinal;
  {$endif}
begin
  if (RHIDevice = nil) or (Count = 0) then
    Exit;

  DataSize := Count * SizeOf(TMatrix4);
  EnsureRHISet2DescriptorSet;
  if (FRHIJointBuffer <> nil) and (FRHIJointBuffer.Desc.Size = DataSize) then
    FRHIJointBuffer.UploadData(JointMatrices^, DataSize)
  else
  begin
    FreeAndNil(FRHIJointBuffer);
    BufDesc := TRHIBufferDesc.Create(DataSize, [buUniform], muCpuToGpu, 'CastleJointMatricesUBO');
    FRHIJointBuffer := RHIDevice.CreateBuffer(BufDesc, JointMatrices);
  end;
  {$ifndef WASI}
  if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
  begin
    New(SkinBlock);
    try
      FillChar(SkinBlock^, SizeOf(TCastleSkinningUniformBlock), 0);
      ClampedCount := Min(Count, Cardinal(CASTLE_MAX_BONE_MATRICES));
      if (JointMatrices <> nil) and (ClampedCount > 0) then
        Move(JointMatrices^, SkinBlock^.JointMatrices[0], ClampedCount * SizeOf(TMatrix4));
      SkinBlock^.JointCount := ClampedCount;
      if (FRHISkinningUBOBuffer <> nil) and
         (FRHISkinningUBOBuffer.Desc.Size = SizeOf(TCastleSkinningUniformBlock)) then
        FRHISkinningUBOBuffer.UploadData(SkinBlock^, SizeOf(TCastleSkinningUniformBlock))
      else
      begin
        FreeAndNil(FRHISkinningUBOBuffer);
        BufDesc := TRHIBufferDesc.Create(SizeOf(TCastleSkinningUniformBlock),
          [buUniform], muCpuToGpu, 'CastleSkinningBlockUBO');
        FRHISkinningUBOBuffer := RHIDevice.CreateBuffer(BufDesc, SkinBlock);
      end;
    finally
      Dispose(SkinBlock);
    end;
    if (FRHISkinningDescriptorSet <> nil) and (FRHISkinningUBOBuffer <> nil) then
    begin
      FRHISkinningDescriptorSet.BindUniformBuffer(0, FRHISkinningUBOBuffer);
      FRHISkinningDescriptorSet.Update;
    end;
  end else
  {$endif}
  if (FRHISkinningDescriptorSet <> nil) and (FRHIJointBuffer <> nil) then
  begin
    FRHISkinningDescriptorSet.BindUniformBuffer(0, FRHIJointBuffer);
    FRHISkinningDescriptorSet.Update;
  end;
  if FRHIShadersInitialized and ((FRHIShaderPermutationMask and $01) = 0) then
  begin
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.UploadUserEffectBlock(const AData: Pointer; const ASize: PtrUInt);
const
  MaxRHIUserEffectSlots = 16;
var
  BufDesc: TRHIBufferDesc;
  AlignedSize: PtrUInt;
  {$ifndef WASI}
  ActiveSkinBuf: TRHIBuffer;
  FoundSlot, SlotIdx, TexIdx: Integer;
  TexMatch: Boolean;
  {$endif}
begin
  if (RHIDevice = nil) or (AData = nil) or (ASize = 0) then
    Exit;

  AlignedSize := (ASize + 15) and not PtrUInt(15);
  EnsureRHISet2DescriptorSet;
  {$ifndef WASI}
  if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) and
     (FRHISkinningDescriptorLayout <> nil) then
  begin
    if FRHISkinningUBOBuffer <> nil then
      ActiveSkinBuf := FRHISkinningUBOBuffer
    else if FRHIJointBuffer <> nil then
      ActiveSkinBuf := FRHIJointBuffer
    else
      ActiveSkinBuf := FRHISet2FallbackUBO;

    FoundSlot := -1;
    for SlotIdx := 0 to High(FRHIUserEffectSlots) do
    begin
      if (Length(FRHIUserEffectSlots[SlotIdx].Data) = Integer(ASize)) and
         (FRHIUserEffectSlots[SlotIdx].SkinningBuffer = ActiveSkinBuf) and
         CompareMem(@FRHIUserEffectSlots[SlotIdx].Data[0], AData, ASize) then
      begin
        TexMatch := True;
        for TexIdx := 0 to High(FRHIUserEffectTextures) do
          if (FRHIUserEffectSlots[SlotIdx].Textures[TexIdx] <> FRHIUserEffectTextures[TexIdx]) or
             (FRHIUserEffectSlots[SlotIdx].Samplers[TexIdx] <> FRHIUserEffectSamplers[TexIdx]) then
          begin
            TexMatch := False;
            Break;
          end;
        if TexMatch then
        begin
          FoundSlot := SlotIdx;
          Break;
        end;
      end;
    end;

    if FoundSlot >= 0 then
    begin
      FActiveRHIUserEffectSlot := FoundSlot;
      Inc(FRHIUserEffectUploadCount);
      Exit;
    end;

    if Length(FRHIUserEffectSlots) < MaxRHIUserEffectSlots then
    begin
      SlotIdx := Length(FRHIUserEffectSlots);
      SetLength(FRHIUserEffectSlots, SlotIdx + 1);
      FRHIUserEffectSlots[SlotIdx].Buffer := nil;
      FRHIUserEffectSlots[SlotIdx].DescriptorSet := RHIDevice.CreateDescriptorSet(FRHISkinningDescriptorLayout);
    end else
    begin
      SlotIdx := Integer(FNextRHIUserEffectSlotEvict mod MaxRHIUserEffectSlots);
      Inc(FNextRHIUserEffectSlotEvict);
    end;

    if (FRHIUserEffectSlots[SlotIdx].Buffer <> nil) and
       (FRHIUserEffectSlots[SlotIdx].Buffer.Desc.Size = AlignedSize) then
      FRHIUserEffectSlots[SlotIdx].Buffer.UploadData(AData^, ASize)
    else
    begin
      FreeAndNil(FRHIUserEffectSlots[SlotIdx].Buffer);
      BufDesc := TRHIBufferDesc.Create(AlignedSize, [buUniform], muCpuToGpu, 'CastleUserEffectUBO');
      FRHIUserEffectSlots[SlotIdx].Buffer := RHIDevice.CreateBuffer(BufDesc, nil);
      FRHIUserEffectSlots[SlotIdx].Buffer.UploadData(AData^, ASize);
    end;

    SetLength(FRHIUserEffectSlots[SlotIdx].Data, ASize);
    Move(AData^, FRHIUserEffectSlots[SlotIdx].Data[0], ASize);
    FRHIUserEffectSlots[SlotIdx].SkinningBuffer := ActiveSkinBuf;
    for TexIdx := 0 to High(FRHIUserEffectTextures) do
    begin
      FRHIUserEffectSlots[SlotIdx].Textures[TexIdx] := FRHIUserEffectTextures[TexIdx];
      FRHIUserEffectSlots[SlotIdx].Samplers[TexIdx] := FRHIUserEffectSamplers[TexIdx];
    end;

    if FRHIUserEffectSlots[SlotIdx].DescriptorSet <> nil then
    begin
      if ActiveSkinBuf <> nil then
        FRHIUserEffectSlots[SlotIdx].DescriptorSet.BindUniformBuffer(0, ActiveSkinBuf);
      if FRHIUserEffectSlots[SlotIdx].Buffer <> nil then
        FRHIUserEffectSlots[SlotIdx].DescriptorSet.BindUniformBuffer(1, FRHIUserEffectSlots[SlotIdx].Buffer);
      for TexIdx := 0 to High(FRHIUserEffectTextures) do
        if FRHIUserEffectTextures[TexIdx] <> nil then
          FRHIUserEffectSlots[SlotIdx].DescriptorSet.BindTexture(
            Cardinal(2 + TexIdx), FRHIUserEffectTextures[TexIdx], FRHIUserEffectSamplers[TexIdx])
        else
          FRHIUserEffectSlots[SlotIdx].DescriptorSet.BindTexture(
            Cardinal(2 + TexIdx), TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      FRHIUserEffectSlots[SlotIdx].DescriptorSet.Update;
    end;

    FActiveRHIUserEffectSlot := SlotIdx;
    Inc(FRHIUserEffectUploadCount);
    Exit;
  end;
  {$endif}

  FActiveRHIUserEffectSlot := -1;
  if (FRHIUserEffectBuffer <> nil) and (FRHIUserEffectBuffer.Desc.Size = AlignedSize) then
    FRHIUserEffectBuffer.UploadData(AData^, ASize)
  else
  begin
    FreeAndNil(FRHIUserEffectBuffer);
    BufDesc := TRHIBufferDesc.Create(AlignedSize, [buUniform], muCpuToGpu, 'CastleUserEffectUBO');
    FRHIUserEffectBuffer := RHIDevice.CreateBuffer(BufDesc, nil);
    FRHIUserEffectBuffer.UploadData(AData^, ASize);
  end;
  Inc(FRHIUserEffectUploadCount);
  if (FRHISkinningDescriptorSet <> nil) and (FRHIUserEffectBuffer <> nil) then
  begin
    FRHISkinningDescriptorSet.BindUniformBuffer(1, FRHIUserEffectBuffer);
    FRHISkinningDescriptorSet.Update;
  end;
end;

procedure TShapeCache.BindUserEffectTexture(const ASlotIndex: Cardinal;
  const ATexture: TRHITexture; const ASampler: TRHISampler);
begin
  if (RHIDevice = nil) or (ASlotIndex > High(FRHIUserEffectTextures)) or (ATexture = nil) then
    Exit;

  EnsureRHISet2DescriptorSet;
  FRHIUserEffectTextures[ASlotIndex] := ATexture;
  FRHIUserEffectSamplers[ASlotIndex] := ASampler;
  if FRHISkinningDescriptorSet <> nil then
  begin
    FRHISkinningDescriptorSet.BindTexture(2 + ASlotIndex, ATexture, ASampler);
    FRHISkinningDescriptorSet.Update;
  end;
  if (FActiveRHIUserEffectSlot >= 0) and (FActiveRHIUserEffectSlot <= High(FRHIUserEffectSlots)) and
     (FRHIUserEffectSlots[FActiveRHIUserEffectSlot].DescriptorSet <> nil) and
     ((FRHIUserEffectSlots[FActiveRHIUserEffectSlot].Textures[ASlotIndex] <> ATexture) or
      (FRHIUserEffectSlots[FActiveRHIUserEffectSlot].Samplers[ASlotIndex] <> ASampler)) then
  begin
    FRHIUserEffectSlots[FActiveRHIUserEffectSlot].Textures[ASlotIndex] := ATexture;
    FRHIUserEffectSlots[FActiveRHIUserEffectSlot].Samplers[ASlotIndex] := ASampler;
    FRHIUserEffectSlots[FActiveRHIUserEffectSlot].DescriptorSet.BindTexture(2 + ASlotIndex, ATexture, ASampler);
    FRHIUserEffectSlots[FActiveRHIUserEffectSlot].DescriptorSet.Update;
  end;
end;

procedure TShapeCache.UploadMaterialBlock(const AMatBlock: TCastleMaterialUniformBlock);
var
  BufDesc: TRHIBufferDesc;
  Bindings: array[0..7] of TRHIDescriptorBindingDesc;
  I: Integer;
begin
  FRHIMaterialBlock := AMatBlock;
  if RHIDevice = nil then
    Exit;
  EnsureRHIDeviceMatch;

  if FRHIMaterialBuffer = nil then
  begin
    BufDesc := TRHIBufferDesc.Create(SizeOf(TCastleMaterialUniformBlock), [buUniform], muCpuToGpu, 'CastleMaterialUBO');
    FRHIMaterialBuffer := RHIDevice.CreateBuffer(BufDesc, @FRHIMaterialBlock);
  end else
    FRHIMaterialBuffer.UploadData(FRHIMaterialBlock, SizeOf(TCastleMaterialUniformBlock));
  begin
    Inc(FRHIMaterialUploadCount);
  if FRHIMaterialDescriptorLayout = nil then
  begin
    FillChar(Bindings, SizeOf(Bindings), 0);
    Bindings[0].Binding := 0;
    Bindings[0].DescriptorType := dtUniformBuffer;
    Bindings[0].Count := 1;
    Bindings[0].StageFlags := [ssVertex, ssGeometry, ssFragment];
    for I := 1 to 7 do
    begin
      Bindings[I].Binding := Cardinal(I);
      Bindings[I].DescriptorType := dtCombinedImageSampler;
      Bindings[I].Count := 1;
      Bindings[I].StageFlags := [ssFragment];
    end;
    FRHIMaterialDescriptorLayout := RHIDevice.CreateDescriptorSetLayout(Bindings);
  end;
  if (FRHIMaterialDescriptorSet = nil) and (FRHIMaterialDescriptorLayout <> nil) then
  begin
    FRHIMaterialDescriptorSet := RHIDevice.CreateDescriptorSet(FRHIMaterialDescriptorLayout);
    {$ifndef WASI}
    if (FRHIMaterialDescriptorSet <> nil) and
       (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
    begin
      FRHIMaterialDescriptorSet.BindTexture(1, TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(2, TVulkanRHIDevice(RHIDevice).GetDefaultFlatNormalTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(3, TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(4, TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(5, TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(6, TVulkanRHIDevice(RHIDevice).GetDefaultBrdfLutTexture, nil);
      FRHIMaterialDescriptorSet.BindTexture(7, TVulkanRHIDevice(RHIDevice).GetDefaultWhiteCubeTexture, nil);
    end;
    {$endif}
  end;
  if (FRHIMaterialDescriptorSet <> nil) and (FRHIMaterialBuffer <> nil) then
  begin
    FRHIMaterialDescriptorSet.BindUniformBuffer(0, FRHIMaterialBuffer);
    FRHIMaterialDescriptorSet.Update;
  end;
  end;
end;

procedure TShapeCache.BindMaterialTexturesToDescriptorSet(
  const ABaseColorTex, ANormalTex, AMetallicRoughnessTex,
        AEmissiveTex, AOcclusionTex: TRHITexture;
  const ABaseColorSampler: TRHISampler;
  const ANormalSampler: TRHISampler;
  const AMetallicRoughnessSampler: TRHISampler;
  const AEmissiveSampler: TRHISampler;
  const AOcclusionSampler: TRHISampler);
var
  AnyBound: Boolean;
  NewBaseDim: TRHITextureDimension;
  {$ifndef WASI}
  RealVkDev: TVulkanRHIDevice;
  {$endif}
begin
  if ABaseColorTex <> nil then
    NewBaseDim := ABaseColorTex.Desc.Dimension
  else
    NewBaseDim := td2D;
  if FRHIBaseColorTextureDimension <> NewBaseDim then
  begin
    FRHIBaseColorTextureDimension := NewBaseDim;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
  FRHIBaseColorSampler := ABaseColorSampler;
  FRHINormalSampler := ANormalSampler;
  FRHIMetallicRoughnessSampler := AMetallicRoughnessSampler;
  FRHIEmissiveSampler := AEmissiveSampler;
  FRHIOcclusionSampler := AOcclusionSampler;
  if FRHIMaterialDescriptorSet = nil then
    Exit;
  {$ifndef WASI}
  if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
    RealVkDev := TVulkanRHIDevice(RHIDevice)
  else
    RealVkDev := nil;
  {$endif}
  AnyBound := False;
  if ABaseColorTex <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(1, ABaseColorTex, ABaseColorSampler);
    AnyBound := True;
  end {$ifndef WASI} else if RealVkDev <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(1, RealVkDev.GetDefaultWhiteTexture, nil);
    AnyBound := True;
  end {$endif};
  if ANormalTex <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(2, ANormalTex, ANormalSampler);
    AnyBound := True;
  end {$ifndef WASI} else if RealVkDev <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(2, RealVkDev.GetDefaultFlatNormalTexture, nil);
    AnyBound := True;
  end {$endif};
  if AMetallicRoughnessTex <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(3, AMetallicRoughnessTex, AMetallicRoughnessSampler);
    AnyBound := True;
  end {$ifndef WASI} else if RealVkDev <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(3, RealVkDev.GetDefaultWhiteTexture, nil);
    AnyBound := True;
  end {$endif};
  if AEmissiveTex <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(4, AEmissiveTex, AEmissiveSampler);
    AnyBound := True;
  end {$ifndef WASI} else if RealVkDev <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(4, RealVkDev.GetDefaultWhiteTexture, nil);
    AnyBound := True;
  end {$endif};
  if AOcclusionTex <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(5, AOcclusionTex, AOcclusionSampler);
    AnyBound := True;
  end {$ifndef WASI} else if RealVkDev <> nil then
  begin
    FRHIMaterialDescriptorSet.BindTexture(5, RealVkDev.GetDefaultWhiteTexture, nil);
    AnyBound := True;
  end {$endif};
  if AnyBound then
    FRHIMaterialDescriptorSet.Update;
end;

procedure TShapeCache.BindEnvironmentTexturesToDescriptorSet(
  const AEnvSpecularTex: TRHITexture;
  const AEnvSpecularSampler: TRHISampler;
  const ABrdfLutTex: TRHITexture;
  const ABrdfLutSampler: TRHISampler);
var
  TexDesc: TRHITextureDesc;
  EffectiveBrdfLut: TRHITexture;
  EffectiveBrdfSampler: TRHISampler;
begin
  FRHIEnvSpecularTexture := AEnvSpecularTex;
  FRHIEnvSpecularSampler := AEnvSpecularSampler;
  EffectiveBrdfSampler := ABrdfLutSampler;
  EffectiveBrdfLut := ABrdfLutTex;
  if (AEnvSpecularTex <> nil) and (EffectiveBrdfLut = nil) and (RHIDevice <> nil) then
  begin
    EnsureRHIDeviceMatch;
    {$ifndef WASI}
    if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
    begin
      EffectiveBrdfLut := TVulkanRHIDevice(RHIDevice).GetDefaultBrdfLutTexture;
      if EffectiveBrdfSampler = nil then
        EffectiveBrdfSampler := TVulkanRHIDevice(RHIDevice).GetDefaultBrdfLutSampler;
    end else
    {$endif}
    begin
      if FRHIDefaultBrdfLutTexture = nil then
      begin
        TexDesc := TRHITextureDesc.Create2D(4, 4, pfRGBA8_UNorm, [tuSampled, tuTransferDst], 1, 1);
        TexDesc.DebugName := 'DefaultBrdfLut2D';
        FRHIDefaultBrdfLutTexture := RHIDevice.CreateTexture(TexDesc);
      end;
      EffectiveBrdfLut := FRHIDefaultBrdfLutTexture;
    end;
  end;
  FRHIBrdfLutSampler := EffectiveBrdfSampler;
  FRHIBrdfLutTexture := EffectiveBrdfLut;
  if FRHIMaterialDescriptorSet = nil then
    Exit;
  if EffectiveBrdfLut <> nil then
    FRHIMaterialDescriptorSet.BindTexture(6, EffectiveBrdfLut, EffectiveBrdfSampler);
  if AEnvSpecularTex <> nil then
    FRHIMaterialDescriptorSet.BindTexture(7, AEnvSpecularTex, AEnvSpecularSampler);
  if (EffectiveBrdfLut <> nil) or (AEnvSpecularTex <> nil) then
    FRHIMaterialDescriptorSet.Update;
end;

procedure TShapeCache.SetTextureTransform(const AHasTransform: Boolean; const AMatrix: TMatrix4);
begin
  if (FRHIHasTextureTransform <> AHasTransform) or
     (AHasTransform and not TMatrix4.PerfectlyEquals(FRHITextureTransform, AMatrix)) then
  begin
    FRHIHasTextureTransform := AHasTransform;
    FRHITextureTransform := AMatrix;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetTextureTransform1(const AHasTransform: Boolean; const AMatrix: TMatrix4);
begin
  if (FRHIHasTextureTransform1 <> AHasTransform) or
     (AHasTransform and not TMatrix4.PerfectlyEquals(FRHITextureTransform1, AMatrix)) then
  begin
    FRHIHasTextureTransform1 := AHasTransform;
    FRHITextureTransform1 := AMatrix;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetSecondTexture(
  const AHasSecondTex: Boolean;
  const ACombineRGB: TCombine;
  const ACombineAlpha: TCombine);
begin
  SetSecondTexture(AHasSecondTex, ACombineRGB, ACombineAlpha, Vector4(1, 1, 1, 1));
end;

procedure TShapeCache.SetSecondTexture(
  const AHasSecondTex: Boolean;
  const ACombineRGB, ACombineAlpha: TCombine;
  const AConstantColor: TVector4);
var
  Env: TTextureEnv;
begin
  Env.Init('MODULATE', '', '');
  Env.Combine[cRGB] := ACombineRGB;
  Env.Combine[cAlpha] := ACombineAlpha;
  if (ACombineRGB = coBlend) or (ACombineAlpha = coBlend) then
    Env.BlendAlphaSource := csConstant;
  SetSecondTextureEnv(AHasSecondTex, Env, AConstantColor);
end;

procedure TShapeCache.SetSecondTextureEnv(
  const AHasSecondTex: Boolean;
  const AEnv: TTextureEnv;
  const AConstantColor: TVector4);
begin
  if (FRHIHasSecondTexture <> AHasSecondTex) or
     (AHasSecondTex and
      ((FRHISecondTextureEnv.Hash <> AEnv.Hash) or
       (FRHISecondTextureCombineRGB <> AEnv.Combine[cRGB]) or
       (FRHISecondTextureCombineAlpha <> AEnv.Combine[cAlpha]) or
       (FRHISecondTextureEnv.TextureFunction <> AEnv.TextureFunction) or
       (not SameValue(FRHISecondTextureEnv.Scale[cRGB], AEnv.Scale[cRGB], 1e-4)) or
       (not SameValue(FRHISecondTextureEnv.Scale[cAlpha], AEnv.Scale[cAlpha], 1e-4)) or
       (not TVector4.PerfectlyEquals(FRHISecondTextureConstantColor, AConstantColor)))) then
  begin
    FRHIHasSecondTexture := AHasSecondTex;
    FRHISecondTextureEnv := AEnv;
    FRHISecondTextureCombineRGB := AEnv.Combine[cRGB];
    FRHISecondTextureCombineAlpha := AEnv.Combine[cAlpha];
    FRHISecondTextureConstantColor := AConstantColor;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetMultiTextureStageEnv(
  const AStageIndex: Cardinal;
  const AHasStageTex: Boolean;
  const AEnv: TTextureEnv;
  const AConstantColor: TVector4);
begin
  if AStageIndex = 1 then
  begin
    SetSecondTextureEnv(AHasStageTex, AEnv, AConstantColor);
    Exit;
  end;
  if not (AStageIndex in [2, 3]) then
    Exit;
  if (FRHIHasMultiTextureStage[AStageIndex] <> AHasStageTex) or
     (AHasStageTex and
      ((FRHIMultiTextureStageEnv[AStageIndex].Hash <> AEnv.Hash) or
       (FRHIMultiTextureStageEnv[AStageIndex].Combine[cRGB] <> AEnv.Combine[cRGB]) or
       (FRHIMultiTextureStageEnv[AStageIndex].Combine[cAlpha] <> AEnv.Combine[cAlpha]) or
       (FRHIMultiTextureStageEnv[AStageIndex].TextureFunction <> AEnv.TextureFunction) or
       (not SameValue(FRHIMultiTextureStageEnv[AStageIndex].Scale[cRGB], AEnv.Scale[cRGB], 1e-4)) or
       (not SameValue(FRHIMultiTextureStageEnv[AStageIndex].Scale[cAlpha], AEnv.Scale[cAlpha], 1e-4)) or
       (not TVector4.PerfectlyEquals(FRHIMultiTextureStageConstantColor[AStageIndex], AConstantColor)))) then
  begin
    FRHIHasMultiTextureStage[AStageIndex] := AHasStageTex;
    FRHIMultiTextureStageEnv[AStageIndex] := AEnv;
    FRHIMultiTextureStageConstantColor[AStageIndex] := AConstantColor;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetMultiTextureStageDimension(
  const AStageIndex: Cardinal;
  const ADimension: TRHITextureDimension);
begin
  if not (AStageIndex in [1..3]) then
    Exit;
  if FRHIMultiTextureStageDimension[AStageIndex] <> ADimension then
  begin
    FRHIMultiTextureStageDimension[AStageIndex] := ADimension;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetClipPlane(const AHasClipPlane: Boolean; const APlane: TVector4);
begin
  if (FRHIHasClipPlane <> AHasClipPlane) or
     (AHasClipPlane and ((FRHIClipPlaneCount <> 1) or not TVector4.PerfectlyEquals(FRHIClipPlane, APlane))) then
  begin
    FRHIHasClipPlane := AHasClipPlane;
    FRHIClipPlane := APlane;
    if AHasClipPlane then
    begin
      FRHIClipPlanes[0] := APlane;
      FRHIClipPlaneCount := 1;
    end else
      FRHIClipPlaneCount := 0;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetClipPlanes(const APlanes: array of TVector4; const ACount: Cardinal);
var
  EffectiveCount: Cardinal;
  I: Integer;
  Changed: Boolean;
begin
  EffectiveCount := Min(Min(ACount, Cardinal(Length(APlanes))), Cardinal(Length(FRHIClipPlanes)));
  Changed := (FRHIClipPlaneCount <> EffectiveCount) or (FRHIHasClipPlane <> (EffectiveCount > 0));
  if not Changed then
    for I := 0 to Integer(EffectiveCount) - 1 do
      if not TVector4.PerfectlyEquals(FRHIClipPlanes[I], APlanes[I]) then
      begin
        Changed := True;
        Break;
      end;
  if Changed then
  begin
    FRHIClipPlaneCount := EffectiveCount;
    FRHIHasClipPlane := EffectiveCount > 0;
    for I := 0 to Integer(EffectiveCount) - 1 do
      FRHIClipPlanes[I] := APlanes[I];
    if EffectiveCount > 0 then
      FRHIClipPlane := FRHIClipPlanes[0]
    else
      FRHIClipPlane := TVector4.Zero;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetShadowMapParams(const ASampling: TShadowSampling; const ASize: Cardinal);
var
  EffectiveSize: Cardinal;
begin
  EffectiveSize := Max(Cardinal(1), ASize);
  if (not FRHIShadowSamplingExplicit) or
     (FRHIShadowSampling <> ASampling) or
     (FRHIShadowMapSize <> EffectiveSize) then
  begin
    FRHIShadowSamplingExplicit := True;
    FRHIShadowSampling := ASampling;
    FRHIShadowMapSize := EffectiveSize;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetTextureCoordMappings(
  const ABaseColorIdx, ANormalIdx, AMetallicRoughnessIdx,
        AEmissiveIdx, AOcclusionIdx: Cardinal);
begin
  if (FRHIBaseColorTexCoordIndex <> ABaseColorIdx) or
     (FRHINormalTexCoordIndex <> ANormalIdx) or
     (FRHIMetallicRoughnessTexCoordIndex <> AMetallicRoughnessIdx) or
     (FRHIEmissiveTexCoordIndex <> AEmissiveIdx) or
     (FRHIOcclusionTexCoordIndex <> AOcclusionIdx) then
  begin
    FRHIBaseColorTexCoordIndex := ABaseColorIdx;
    FRHINormalTexCoordIndex := ANormalIdx;
    FRHIMetallicRoughnessTexCoordIndex := AMetallicRoughnessIdx;
    FRHIEmissiveTexCoordIndex := AEmissiveIdx;
    FRHIOcclusionTexCoordIndex := AOcclusionIdx;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetTexCoordGen(const AGeneration: TTextureCoordinateGeneration;
  const ABoundsS, ABoundsT: TVector4);
begin
  SetTexCoordGen(AGeneration, ABoundsS, ABoundsT, Vector4(0, 0, 1, 0));
end;

procedure TShapeCache.SetTexCoordGen(const AGeneration: TTextureCoordinateGeneration;
  const ABoundsS, ABoundsT, ABoundsR: TVector4);
begin
  if (FRHITexCoordGen <> AGeneration) or
     ((AGeneration in [tgBounds2d, tgBounds3d]) and
      (not TVector4.PerfectlyEquals(FRHITexGenBoundsS, ABoundsS) or
       not TVector4.PerfectlyEquals(FRHITexGenBoundsT, ABoundsT) or
       not TVector4.PerfectlyEquals(FRHITexGenBoundsR, ABoundsR))) then
  begin
    FRHITexCoordGen := AGeneration;
    FRHITexGenBoundsS := ABoundsS;
    FRHITexGenBoundsT := ABoundsT;
    FRHITexGenBoundsR := ABoundsR;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetTexCoordGenProjectorMatrix(const AMatrix: TMatrix4);
begin
  if not TMatrix4.PerfectlyEquals(FRHITexGenProjectorMatrix, AMatrix) then
  begin
    FRHITexGenProjectorMatrix := AMatrix;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

{ Workaround FPC bug on Darwin for AArch64 ("Fatal: Internal error 2014121702"
  when passing 5 HFA vector records exceeding v0..v7 onto packed ARM64 stack):
  pass vector parameters via constref on FPC. }
procedure TShapeCache.SetMirrorPlaneUniforms(
  const AHasMirrorUniforms: Boolean;
  {$ifdef FPC}constref{$else}const{$endif} ANormalizedPlane: TVector4;
  {$ifdef FPC}constref{$else}const{$endif} ACameraPositionOnPlane, ACameraSide, ACameraUp: TVector3;
  {$ifdef FPC}constref{$else}const{$endif} AFrustumDimensions: TVector4);
begin
  if (FRHIHasMirrorPlaneUniforms <> AHasMirrorUniforms) or
     (not TVector4.Equals(FRHIMirrorNormalizedPlane, ANormalizedPlane, 1e-5)) or
     (not TVector3.Equals(FRHIMirrorCameraPositionOnPlane, ACameraPositionOnPlane, 1e-5)) or
     (not TVector3.Equals(FRHIMirrorCameraSide, ACameraSide, 1e-5)) or
     (not TVector3.Equals(FRHIMirrorCameraUp, ACameraUp, 1e-5)) or
     (not TVector4.Equals(FRHIMirrorFrustumDimensions, AFrustumDimensions, 1e-5)) then
  begin
    FRHIHasMirrorPlaneUniforms := AHasMirrorUniforms;
    FRHIMirrorNormalizedPlane := Vector4(
      ANormalizedPlane.X, ANormalizedPlane.Y, ANormalizedPlane.Z, ANormalizedPlane.W);
    FRHIMirrorCameraPositionOnPlane := Vector3(
      ACameraPositionOnPlane.X, ACameraPositionOnPlane.Y, ACameraPositionOnPlane.Z);
    FRHIMirrorCameraSide := Vector3(ACameraSide.X, ACameraSide.Y, ACameraSide.Z);
    FRHIMirrorCameraUp := Vector3(ACameraUp.X, ACameraUp.Y, ACameraUp.Z);
    FRHIMirrorFrustumDimensions := Vector4(
      AFrustumDimensions.X, AFrustumDimensions.Y, AFrustumDimensions.Z, AFrustumDimensions.W);
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetPointSize(const APointSize: Single);
var
  ClampedSize: Single;
begin
  ClampedSize := Max(1.0, APointSize);
  if not SameValue(FRHIPointSize, ClampedSize, 1e-4) then
  begin
    FRHIPointSize := ClampedSize;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetLineType(const ALineType: TLineType);
var
  NewPattern: Word;
begin
  NewPattern := TRenderContext.LineTypeToStipplePattern(ALineType);
  FRHILineType := ALineType;
  FRHILineStipplePattern := NewPattern;
end;

procedure TShapeCache.SetParallaxBumpMapping(const AEnabled, ASteep: Boolean; const AHeight: Single);
begin
  SetParallaxBumpMapping(AEnabled, ASteep, False, AHeight);
end;

procedure TShapeCache.SetParallaxBumpMapping(const AEnabled, ASteep, ASelfShadowing: Boolean; const AHeight: Single);
begin
  if (FRHIHasParallaxBumpMapping <> AEnabled) or
     (FRHISteepParallaxBumpMapping <> ASteep) or
     (FRHISteepParallaxSelfShadowing <> ASelfShadowing) or
     (not SameValue(FRHIParallaxHeight, AHeight, 1e-6)) then
  begin
    FRHIHasParallaxBumpMapping := AEnabled;
    FRHISteepParallaxBumpMapping := ASteep;
    FRHISteepParallaxSelfShadowing := ASelfShadowing;
    FRHIParallaxHeight := AHeight;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetIsPhysicalMaterial(const AIsPhysical: Boolean);
begin
  if FRHIIsPhysicalMaterial <> AIsPhysical then
  begin
    FRHIIsPhysicalMaterial := AIsPhysical;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetPhongSurfaceParams(
  const AIsPhong, AHasSpecularTex, AHasShininessTex, AHasAmbientTex: Boolean;
  const AAmbientIntensity: Single);
begin
  if not SameValue(FRHIPhongAmbientIntensity, AAmbientIntensity, 1e-4) then
  begin
    FRHIPhongAmbientIntensity := AAmbientIntensity;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
  FRHIIsPhongMaterial := AIsPhong;
  FRHIPhongHasSpecularTex := AHasSpecularTex;
  FRHIPhongHasShininessTex := AHasShininessTex;
  FRHIPhongHasAmbientTex := AHasAmbientTex;
end;

procedure TShapeCache.SetPhongExtendedSurfaceParams(
  const AHasSeparateShininessTex, AHasAlphaTex, AInvertAlpha: Boolean);
begin
  FRHIPhongHasSeparateShininessTex := AHasSeparateShininessTex;
  FRHIHasAlphaTexture := AHasAlphaTex;
  FRHIInvertAlphaTexture := AInvertAlpha;
end;

procedure TShapeCache.SetEnvironmentLightParams(
  const AHasSeparateDiffuse: Boolean;
  const AColorIntensity: TVector3);
begin
  if (FRHIHasSeparateDiffuseEnvTex <> AHasSeparateDiffuse) or
     (not TVector3.Equals(FRHIEnvLightColorIntensity, AColorIntensity, 1e-4)) then
  begin
    FRHIHasSeparateDiffuseEnvTex := AHasSeparateDiffuse;
    FRHIEnvLightColorIntensity := AColorIntensity;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetShaderEffects(const AVertexEffectCode, AFragmentEffectCode: String);
begin
  SetShaderEffects(AVertexEffectCode, AFragmentEffectCode, '');
end;

procedure TShapeCache.SetShaderEffects(const AVertexEffectCode, AFragmentEffectCode, AGeometryEffectCode: String);
begin
  if (FRHIVertexEffectCode <> AVertexEffectCode) or
     (FRHIFragmentEffectCode <> AFragmentEffectCode) or
     (FRHIGeometryEffectCode <> AGeometryEffectCode) then
  begin
    FRHIVertexEffectCode := AVertexEffectCode;
    FRHIFragmentEffectCode := AFragmentEffectCode;
    FRHIGeometryEffectCode := AGeometryEffectCode;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.SetComposedShader(const ACustomVertSource, ACustomFragSource: String);
begin
  SetComposedShader(ACustomVertSource, ACustomFragSource, '');
end;

procedure TShapeCache.SetComposedShader(const ACustomVertSource, ACustomFragSource, ACustomGeomSource: String);
begin
  if (FRHIComposedVertSource <> ACustomVertSource) or
     (FRHIComposedFragSource <> ACustomFragSource) or
     (FRHIComposedGeomSource <> ACustomGeomSource) then
  begin
    FRHIComposedVertSource := ACustomVertSource;
    FRHIComposedFragSource := ACustomFragSource;
    FRHIComposedGeomSource := ACustomGeomSource;
    FRHIShadersInitialized := False;
    FRHIAltShadersInitialized := False;
  end;
end;

procedure TShapeCache.EnsureRHIShaders(
  const AHasSkinning, AHasTangents,
        AHasBaseColorTex, AHasNormalTex, AHasMetallicRoughnessTex,
        AHasEmissiveTex, AHasOcclusionTex, AUseAlphaTest: Boolean;
  const AHasColorPerVertex: Boolean;
  const AHasTextureTransform: Boolean;
  const AHasClipPlane: Boolean;
  const AHasIBL: Boolean;
  const AHasShadowMap: Boolean;
  const AHasTexCoordGen: Boolean;
  const AHasShaderEffects: Boolean;
  const AHasComposedShader: Boolean);

  function StripGLSLComments(const ASource: String): String;
  var
    SB: TStringBuilder;
    Idx, SrcLen: Integer;
    InLineComment, InBlockComment: Boolean;
    Ch: Char;
  begin
    if ASource = '' then
      Exit('');
    SrcLen := Length(ASource);
    SB := TStringBuilder.Create(SrcLen);
    try
      Idx := 1;
      InLineComment := False;
      InBlockComment := False;
      while Idx <= SrcLen do
      begin
        Ch := ASource[Idx];
        if InLineComment then
        begin
          if (Ch = #10) or (Ch = #13) then
          begin
            InLineComment := False;
            SB.Append(Ch);
          end;
          Inc(Idx);
        end else
        if InBlockComment then
        begin
          if (Idx < SrcLen) and (Ch = '*') and (ASource[Idx + 1] = '/') then
          begin
            InBlockComment := False;
            SB.Append('  ');
            Inc(Idx, 2);
          end else
          begin
            if (Ch = #10) or (Ch = #13) then
              SB.Append(Ch);
            Inc(Idx);
          end;
        end else
        begin
          if (Idx < SrcLen) and (Ch = '/') and (ASource[Idx + 1] = '/') then
          begin
            InLineComment := True;
            SB.Append('  ');
            Inc(Idx, 2);
          end else
          if (Idx < SrcLen) and (Ch = '/') and (ASource[Idx + 1] = '*') then
          begin
            InBlockComment := True;
            SB.Append('  ');
            Inc(Idx, 2);
          end else
          begin
            SB.Append(Ch);
            Inc(Idx);
          end;
        end;
      end;
      Result := SB.ToString;
    finally
      FreeAndNil(SB);
    end;
  end;

  function IsPlugDefinitionAfterName(const ASource: String; const AAfterIdx: Integer): Boolean;
  var
    ScanIdx, SrcLen, ParenDepth: Integer;
  begin
    Result := False;
    SrcLen := Length(ASource);
    ScanIdx := AAfterIdx;
    while (ScanIdx <= SrcLen) and (ASource[ScanIdx] in [' ', #9, #10, #13]) do
      Inc(ScanIdx);
    if (ScanIdx > SrcLen) or (ASource[ScanIdx] <> '(') then
      Exit;
    ParenDepth := 1;
    Inc(ScanIdx);
    while (ScanIdx <= SrcLen) and (ParenDepth > 0) do
    begin
      if ASource[ScanIdx] = '(' then
        Inc(ParenDepth)
      else if ASource[ScanIdx] = ')' then
        Dec(ParenDepth);
      Inc(ScanIdx);
    end;
    if ParenDepth <> 0 then
      Exit;
    while (ScanIdx <= SrcLen) and (ASource[ScanIdx] in [' ', #9, #10, #13]) do
      Inc(ScanIdx);
    Result := (ScanIdx <= SrcLen) and (ASource[ScanIdx] = '{');
  end;

  function HasPlugFunction(const ASource, APlugName: String): Boolean;
  var
    P, AfterIdx: Integer;
  begin
    Result := False;
    P := Pos(APlugName, ASource);
    while P > 0 do
    begin
      AfterIdx := P + Length(APlugName);
      if ((P = 1) or not (ASource[P - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_'])) and
         (AfterIdx <= Length(ASource)) and
         (ASource[AfterIdx] in ['(', ' ', #9, #10, #13]) and
         IsPlugDefinitionAfterName(ASource, AfterIdx) then
        Exit(True);
      P := Pos(APlugName, ASource, P + 1);
    end;
  end;

  procedure DeduplicatePlugDefinitions(var ASource: String; const APlugName: String);
  var
    P, AfterIdx, Count: Integer;
    RenamedPlug: String;
  begin
    if ASource = '' then
      Exit;
    Count := 0;
    P := Pos(APlugName, ASource);
    while P > 0 do
    begin
      AfterIdx := P + Length(APlugName);
      if ((P = 1) or not (ASource[P - 1] in ['A'..'Z', 'a'..'z', '0'..'9', '_'])) and
         (AfterIdx <= Length(ASource)) and
         (ASource[AfterIdx] in ['(', ' ', #9, #10, #13]) and
         IsPlugDefinitionAfterName(ASource, AfterIdx) then
      begin
        Inc(Count);
        if Count >= 2 then
        begin
          RenamedPlug := Format('%s_%d', [APlugName, Count]);
          ASource := Copy(ASource, 1, P - 1) + RenamedPlug + Copy(ASource, AfterIdx, MaxInt);
          AfterIdx := P + Length(RenamedPlug);
        end;
      end;
      P := Pos(APlugName, ASource, AfterIdx);
    end;
  end;

  function BuildChainedPlugCalls(const ASource, APlugName, ACallArgs, AIndent: String): String;
  var
    Idx: Integer;
    CandidateName: String;
  begin
    Result := '';
    if not HasPlugFunction(ASource, APlugName) then
      Exit;
    Result := AIndent + APlugName + '(' + ACallArgs + ');';
    Idx := 2;
    while True do
    begin
      CandidateName := Format('%s_%d', [APlugName, Idx]);
      if not HasPlugFunction(ASource, CandidateName) then
        Break;
      Result := Result + LineEnding + AIndent + CandidateName + '(' + ACallArgs + ');';
      Inc(Idx);
    end;
  end;

  function BuildMultiTexStageSnippet(
    const AStageIdx: Integer;
    const AEnv: TTextureEnv;
    const ACombineRGB, ACombineAlpha: TCombine;
    const AConstColor: TVector4;
    const AUVExpr: String;
    const ALinearColorSpace: Boolean): String;
  var
    StageVar, SamplerName, StageArgRGB, StageArgAlpha, StageBlendExpr: String;
  begin
    StageVar := Format('multiTex%d', [AStageIdx]);
    SamplerName := Format('castle_multi_texture_%d', [AStageIdx]);
    Result := '  vec4 ' + StageVar + ' = texture(' + SamplerName + ', ' + AUVExpr + ');' + LineEnding;
    if ALinearColorSpace then
      Result := Result + '  ' + StageVar + '.rgb = pow(max(' + StageVar + '.rgb, vec3(0.0)), vec3(2.2));' + LineEnding;
    case AEnv.Source[cRGB] of
      csCurrentTexture: StageArgRGB := StageVar + '.rgb';
      csMaterial:       StageArgRGB := 'castle_MaterialBaseColor.rgb';
      csConstant:       StageArgRGB := FormatDot('vec3(%.4f, %.4f, %.4f)',
        [AConstColor.X, AConstColor.Y, AConstColor.Z]);
      else              StageArgRGB := 'baseColor.rgb';
    end;
    case AEnv.Source[cAlpha] of
      csCurrentTexture: StageArgAlpha := StageVar + '.a';
      csMaterial:       StageArgAlpha := 'castle_MaterialBaseColor.a';
      csConstant:       StageArgAlpha := FormatDot('%.4f', [AConstColor.W]);
      else              StageArgAlpha := 'baseColor.a';
    end;
    case AEnv.BlendAlphaSource of
      csCurrentTexture:  StageBlendExpr := StageVar + '.a';
      csPreviousTexture: StageBlendExpr := 'baseColor.a';
      csMaterial:        StageBlendExpr := 'castle_MaterialBaseColor.a';
      else               StageBlendExpr := FormatDot('%.4f', [AConstColor.W]);
    end;
    case ACombineRGB of
      coReplace:
        if AEnv.SourceArgument[cRGB] = ta0 then
          Result := Result + '  baseColor.rgb = ' + StageArgRGB + ';' + LineEnding
        else
          Result := Result + '  baseColor.rgb = ' + StageVar + '.rgb;' + LineEnding;
      coAdd:
        if StageArgRGB = 'baseColor.rgb' then
          Result := Result + '  baseColor.rgb += ' + StageVar + '.rgb;' + LineEnding
        else
          Result := Result + '  baseColor.rgb = ' + StageVar + '.rgb + ' + StageArgRGB + ';' + LineEnding;
      coAddSigned:
        if StageArgRGB = 'baseColor.rgb' then
          Result := Result + '  baseColor.rgb += ' + StageVar + '.rgb - vec3(0.5);' + LineEnding
        else
          Result := Result + '  baseColor.rgb = ' + StageVar + '.rgb + ' + StageArgRGB + ' - vec3(0.5);' + LineEnding;
      coSubtract:
        Result := Result + '  baseColor.rgb = ' + StageVar + '.rgb - ' + StageArgRGB + ';' + LineEnding;
      coBlend:
        Result := Result +
          '  baseColor.rgb = mix(baseColor.rgb, ' + StageVar + '.rgb, ' + StageBlendExpr + ');' + LineEnding;
      coDot3Rgb, coDot3Rgba:
        Result := Result +
          '  baseColor.rgb = vec3(dot(2.0 * (' + StageVar + '.rgb - vec3(0.5)), 2.0 * (' + StageArgRGB + ' - vec3(0.5))));' + LineEnding;
      else
        if StageArgRGB = 'baseColor.rgb' then
          Result := Result + '  baseColor.rgb *= ' + StageVar + '.rgb;' + LineEnding
        else
          Result := Result + '  baseColor.rgb = ' + StageVar + '.rgb * ' + StageArgRGB + ';' + LineEnding;
    end;
    case ACombineAlpha of
      coReplace:
        if AEnv.SourceArgument[cAlpha] = ta0 then
          Result := Result + '  baseColor.a = ' + StageArgAlpha + ';'
        else
          Result := Result + '  baseColor.a = ' + StageVar + '.a;';
      coAdd:
        if StageArgAlpha = 'baseColor.a' then
          Result := Result + '  baseColor.a += ' + StageVar + '.a;'
        else
          Result := Result + '  baseColor.a = ' + StageVar + '.a + ' + StageArgAlpha + ';';
      coAddSigned:
        if StageArgAlpha = 'baseColor.a' then
          Result := Result + '  baseColor.a += ' + StageVar + '.a - 0.5;'
        else
          Result := Result + '  baseColor.a = ' + StageVar + '.a + ' + StageArgAlpha + ' - 0.5;';
      coSubtract:
        Result := Result + '  baseColor.a = ' + StageVar + '.a - ' + StageArgAlpha + ';';
      coBlend:
        Result := Result +
          '  baseColor.a = mix(baseColor.a, ' + StageVar + '.a, ' + StageBlendExpr + ');';
      coDot3Rgb, coDot3Rgba:
        Result := Result +
          '  baseColor.a = dot(2.0 * (' + StageVar + '.rgb - vec3(0.5)), 2.0 * (' + StageArgRGB + ' - vec3(0.5)));';
      else
        if StageArgAlpha = 'baseColor.a' then
          Result := Result + '  baseColor.a *= ' + StageVar + '.a;'
        else
          Result := Result + '  baseColor.a = ' + StageVar + '.a * ' + StageArgAlpha + ';';
    end;
    if not SameValue(AEnv.Scale[cRGB], 1.0, 1e-4) then
      Result := Result + LineEnding + FormatDot(
        '  baseColor.rgb *= %.4f;', [AEnv.Scale[cRGB]]);
    if not SameValue(AEnv.Scale[cAlpha], 1.0, 1e-4) then
      Result := Result + LineEnding + FormatDot(
        '  baseColor.a *= %.4f;', [AEnv.Scale[cAlpha]]);
    case AEnv.TextureFunction of
      tfComplement:
        Result := Result + LineEnding + '  baseColor.rgb = vec3(1.0) - baseColor.rgb;';
      tfAlphaReplicate:
        Result := Result + LineEnding + '  baseColor.rgb = vec3(baseColor.a);';
      else ;
    end;
  end;

var
  Mask, TempMask: QWord;
  TempShader: TRHIShaderModule;
  TempSource: String;
  Gen: TCastleShaderGeneratorRHI;
  FogSnippet, TexGenSnippet, TexTransformSnippet, ClipPlaneSnippet, LineStippleSnippet, ShadowMapSnippet, ParallaxSnippet, SelfShadowSnippet, PhongSetupSnippet: String;
  SecondTexSnippet, SecondTexUV, AlphaTexSnippet, PlugTexColorCall, CubeCoordExpr, Tex3DCoordExpr, Tex3DMatExpr: String;
  UpgradedVertEffect, UpgradedFragEffect, DedupVertEffect, DedupFragEffect, DedupGeomEffect, PlugGeomSetCall, PlugGeomZeroCall, PlugGeomAddCall, PlugCandidate, SingleTexColorCall, EffectivePointSizeStr, AttribGLSLType, AttribDecl: String;
  TempLayout: TUserEffectShaderLayout;
  HasLinearColorSpace, HasGeomStage, NeedTexCoord1Attr: Boolean;
  EffectiveToneMapping: TToneMapping;
  EffectiveShadowSampling: TShadowSampling;
  EffectiveShadowSize, ClipCount, TexCoord0Dim: Cardinal;
  Plane: TVector4;
  I, PlugPos, ParenPos, CommaCount, PlugIdx: Integer;
  LocIdx, AttrLoc: Cardinal;
begin
  if RHIDevice = nil then
    Exit;
  EnsureRHIDeviceMatch;

  HasLinearColorSpace :=
    (CastleRenderOptions.ColorSpace = csLinear) or
    ((CastleRenderOptions.ColorSpace = csLinearWhenPhysicalMaterial) and FRHIIsPhysicalMaterial);
  EffectiveToneMapping := CastleRenderOptions.ToneMapping;
  if FRHIShadowSamplingExplicit then
    EffectiveShadowSampling := FRHIShadowSampling
  else if RenderOptions <> nil then
    EffectiveShadowSampling := RenderOptions.ShadowSampling
  else
    EffectiveShadowSampling := FRHIShadowSampling;
  EffectiveShadowSize := Max(Cardinal(1), FRHIShadowMapSize);

  if (Arrays <> nil) and (Arrays.TexCoords.Count > 0) and (Arrays.TexCoords[0] <> nil) and
     (Arrays.TexCoords[0].Generation = tgExplicit) then
    TexCoord0Dim := Arrays.TexCoords[0].Dimensions
  else
    TexCoord0Dim := 2;

  Mask := 0;
  if AHasSkinning then Mask := Mask or $01;
  if AHasTangents then Mask := Mask or $02;
  if AHasBaseColorTex then Mask := Mask or $04;
  if AHasNormalTex then Mask := Mask or $08;
  if AHasMetallicRoughnessTex then Mask := Mask or $10;
  if AHasEmissiveTex then Mask := Mask or $20;
  if AHasOcclusionTex then Mask := Mask or $40;
  if AUseAlphaTest then Mask := Mask or $80;
  if AHasColorPerVertex then Mask := Mask or $100;
  if AHasTextureTransform then Mask := Mask or $200;
  if AHasClipPlane then
  begin
    Mask := Mask or $400;
    if FRHIClipPlaneCount > 1 then
      Mask := Mask or $08000000;
  end;
  if AHasIBL then Mask := Mask or $800;
  if AHasShadowMap then
    Mask := Mask or $1000 or ((Cardinal(Ord(EffectiveShadowSampling)) and 7) shl 24);
  if AHasTexCoordGen or (FRHITexCoordGen <> tgExplicit) then Mask := Mask or $2000;
  if AHasShaderEffects or (FRHIVertexEffectCode <> '') or (FRHIFragmentEffectCode <> '') or
     (FRHIGeometryEffectCode <> '') or (FRHIPointSize > 1.0001) then Mask := Mask or $4000;
  if AHasComposedShader or (FRHIComposedVertSource <> '') or (FRHIComposedFragSource <> '') or
     (FRHIComposedGeomSource <> '') then
    Mask := Mask or $8000;
  if FRHIHasParallaxBumpMapping and AHasTangents and AHasNormalTex then
  begin
    Mask := Mask or $10000;
    if FRHISteepParallaxBumpMapping then
    begin
      Mask := Mask or $80000;
      if FRHISteepParallaxSelfShadowing then
        Mask := Mask or (QWord(1) shl 34);
    end;
  end;
  if (Arrays <> nil) and (Arrays.HasFogCoord or (Arrays.Attribs.Count > 0)) then
    Mask := Mask or $20000;
  if HasLinearColorSpace or (EffectiveToneMapping <> tmNone) then
    Mask := Mask or $40000 or (Cardinal(Ord(EffectiveToneMapping)) shl 20);
  if AHasColorPerVertex and (Arrays <> nil) and (Arrays.ColorMode = cmReplace) then
    Mask := Mask or $100000;
  if FRHIHasTextureTransform1 then
    Mask := Mask or $200000;
  if FRHITexCoordGen = tgProjection then
    Mask := Mask or $400000
  else if FRHITexCoordGen = tgMirrorPlane then
    Mask := Mask or $800000;
  if FRHIBaseColorTexCoordIndex > 0 then
    Mask := Mask or $10000000;
  if FRHINormalTexCoordIndex > 0 then
    Mask := Mask or $20000000;
  if (FRHIMetallicRoughnessTexCoordIndex > 0) or
     (FRHIEmissiveTexCoordIndex > 0) or
     (FRHIOcclusionTexCoordIndex > 0) then
    Mask := Mask or $40000000;
  if AHasBaseColorTex and FRHIHasSecondTexture then
    Mask := Mask or $80000000;
  Mask := Mask or (QWord(Ord(FRHIBaseColorTextureDimension) and 3) shl 32);
  if TexCoord0Dim <> 2 then
    Mask := Mask or (QWord(TexCoord0Dim and 7) shl 35);
  if FRHIIsPhongMaterial then
  begin
    Mask := Mask or (QWord(1) shl 38);
    if FRHIPhongHasSpecularTex then Mask := Mask or (QWord(1) shl 39);
    if FRHIPhongHasShininessTex then Mask := Mask or (QWord(1) shl 40);
    if FRHIPhongHasAmbientTex then Mask := Mask or (QWord(1) shl 41);
    if FRHIPhongHasSeparateShininessTex then Mask := Mask or (QWord(1) shl 43);
  end;
  if AHasIBL and (FRHIHasSeparateDiffuseEnvTex or
     (not TVector3.Equals(FRHIEnvLightColorIntensity, Vector3(1.0, 1.0, 1.0), 1e-4))) then
    Mask := Mask or (QWord(1) shl 42);
  if FRHIHasAlphaTexture then
    Mask := Mask or (QWord(1) shl 44);
  if FRHIInvertAlphaTexture then
    Mask := Mask or (QWord(1) shl 52);
  if AHasBaseColorTex and FRHIHasMultiTextureStage[2] then
    Mask := Mask or (QWord(1) shl 45);
  if AHasBaseColorTex and FRHIHasMultiTextureStage[3] then
    Mask := Mask or (QWord(1) shl 46);
  if (FRHITexCoordGen = tgMirrorPlane) and FRHIHasMirrorPlaneUniforms then
    Mask := Mask or (QWord(1) shl 47);
  HasGeomStage := (FRHIGeometryEffectCode <> '') or (FRHIComposedGeomSource <> '');
  if HasGeomStage then
    Mask := Mask or (QWord(1) shl 48);
  if FRHILineType <> ltSolid then
    Mask := Mask or (QWord(Ord(FRHILineType) and 7) shl 49);
  NeedTexCoord1Attr :=
    ((Arrays <> nil) and (Arrays.TexCoords.Count > 1) and (Arrays.TexCoords[1] <> nil) and
     (Arrays.TexCoords[1].Generation = tgExplicit)) or
    FRHIHasSecondTexture or
    FRHIHasTextureTransform1 or
    (FRHIBaseColorTexCoordIndex > 0) or
    (FRHINormalTexCoordIndex > 0) or
    (FRHIMetallicRoughnessTexCoordIndex > 0) or
    (FRHIEmissiveTexCoordIndex > 0) or
    (FRHIOcclusionTexCoordIndex > 0);
  if NeedTexCoord1Attr then
    Mask := Mask or (QWord(1) shl 53);
  Mask := Mask or
    (QWord(Ord(FRHIMultiTextureStageDimension[1]) and 3) shl 54) or
    (QWord(Ord(FRHIMultiTextureStageDimension[2]) and 3) shl 56) or
    (QWord(Ord(FRHIMultiTextureStageDimension[3]) and 3) shl 58);

  EnsureRHIDeviceMatch;

  if FRHIShadersInitialized and (FRHIShaderPermutationMask = Mask) and
     (FRHIVertexShader <> nil) and (FRHIFragmentShader <> nil) and
     ((not HasGeomStage) or (FRHIGeometryShader <> nil)) then
    Exit;

  if FRHIAltShadersInitialized and (FRHIAltShaderPermutationMask = Mask) and
     (FRHIAltVertexShader <> nil) and (FRHIAltFragmentShader <> nil) and
     ((not HasGeomStage) or (FRHIAltGeometryShader <> nil)) then
  begin
    TempShader := FRHIVertexShader; FRHIVertexShader := FRHIAltVertexShader; FRHIAltVertexShader := TempShader;
    TempShader := FRHIFragmentShader; FRHIFragmentShader := FRHIAltFragmentShader; FRHIAltFragmentShader := TempShader;
    TempShader := FRHIGeometryShader; FRHIGeometryShader := FRHIAltGeometryShader; FRHIAltGeometryShader := TempShader;
    TempSource := FRHIVertexShaderSource; FRHIVertexShaderSource := FRHIAltVertexShaderSource; FRHIAltVertexShaderSource := TempSource;
    TempSource := FRHIFragmentShaderSource; FRHIFragmentShaderSource := FRHIAltFragmentShaderSource; FRHIAltFragmentShaderSource := TempSource;
    TempSource := FRHIGeometryShaderSource; FRHIGeometryShaderSource := FRHIAltGeometryShaderSource; FRHIAltGeometryShaderSource := TempSource;
    TempLayout := FRHIUserEffectLayout; FRHIUserEffectLayout := FRHIAltUserEffectLayout; FRHIAltUserEffectLayout := TempLayout;
    TempMask := FRHIShaderPermutationMask; FRHIShaderPermutationMask := FRHIAltShaderPermutationMask; FRHIAltShaderPermutationMask := TempMask;
    FRHIAltShadersInitialized := FRHIShadersInitialized;
    FRHIShadersInitialized := True;
    Exit;
  end;

  if FRHIShadersInitialized and (FRHIVertexShader <> nil) and (FRHIFragmentShader <> nil) then
  begin
    FreeAndNil(FRHIAltVertexShader);
    FreeAndNil(FRHIAltFragmentShader);
    FreeAndNil(FRHIAltGeometryShader);
    FRHIAltVertexShader := FRHIVertexShader;
    FRHIAltFragmentShader := FRHIFragmentShader;
    FRHIAltGeometryShader := FRHIGeometryShader;
    FRHIAltVertexShaderSource := FRHIVertexShaderSource;
    FRHIAltFragmentShaderSource := FRHIFragmentShaderSource;
    FRHIAltGeometryShaderSource := FRHIGeometryShaderSource;
    FRHIAltUserEffectLayout := FRHIUserEffectLayout;
    FRHIAltShaderPermutationMask := FRHIShaderPermutationMask;
    FRHIAltShadersInitialized := True;
    FRHIVertexShader := nil;
    FRHIFragmentShader := nil;
    FRHIGeometryShader := nil;
  end else
  begin
    FreeAndNil(FRHIVertexShader);
    FreeAndNil(FRHIFragmentShader);
    FreeAndNil(FRHIGeometryShader);
  end;
  FRHIGeometryShaderSource := '';
  Gen := TCastleShaderGeneratorRHI.Create(rbVulkan);
  try
    DedupVertEffect := StripGLSLComments(FRHIVertexEffectCode);
    if DedupVertEffect <> '' then
    begin
      DeduplicatePlugDefinitions(DedupVertEffect, 'PLUG_vertex_object_space');
      DeduplicatePlugDefinitions(DedupVertEffect, 'PLUG_vertex_object_space_change');
      DeduplicatePlugDefinitions(DedupVertEffect, 'PLUG_vertex_eye_space_extended');
      DeduplicatePlugDefinitions(DedupVertEffect, 'PLUG_vertex_eye_space');
    end;
    DedupFragEffect := StripGLSLComments(FRHIFragmentEffectCode);
    if DedupFragEffect <> '' then
    begin
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_fog_apply');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_steep_parallax_shadow_apply');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_fragment_modify');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_fragment_end');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_lighting_apply');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_specular');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_ambient');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_light_ambient');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_shininess');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_fragment_eye_space');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_light_scale');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_metallic_roughness');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_add_light');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_occlusion');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_material_emissive');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_texture_coord_shift');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_base_color');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_main_texture_apply');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_texture_apply');
      DeduplicatePlugDefinitions(DedupFragEffect, 'PLUG_texture_color');
    end;
    DedupGeomEffect := StripGLSLComments(FRHIGeometryEffectCode);
    if DedupGeomEffect <> '' then
    begin
      DeduplicatePlugDefinitions(DedupGeomEffect, 'PLUG_geometry_vertex_set');
      DeduplicatePlugDefinitions(DedupGeomEffect, 'PLUG_geometry_vertex_zero');
      DeduplicatePlugDefinitions(DedupGeomEffect, 'PLUG_geometry_vertex_add');
    end;
    if (FRHIComposedVertSource <> '') or (FRHIComposedFragSource <> '') or (FRHIComposedGeomSource <> '') then
      FRHIUserEffectLayout := Gen.ExtractUserEffectLayout(
        FRHIComposedVertSource, FRHIComposedFragSource, FRHIComposedGeomSource, 10)
    else if (DedupVertEffect <> '') or (DedupFragEffect <> '') or (DedupGeomEffect <> '') then
      FRHIUserEffectLayout := Gen.ExtractUserEffectLayout(
        DedupVertEffect, DedupFragEffect, DedupGeomEffect, 10)
    else
      FRHIUserEffectLayout := Default(TUserEffectShaderLayout);
    if (FRHIUserEffectLayout.TotalUBOSize > 0) or (Length(FRHIUserEffectLayout.Samplers) > 0) then
    begin
      EnsureRHISet2DescriptorSet;
      {$ifndef WASI}
      if (FRHISkinningDescriptorSet <> nil) and
         (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
      begin
        for I := 0 to High(FRHIUserEffectLayout.Samplers) do
          if (FRHIUserEffectLayout.Samplers[I].SlotIndex >= 0) and
             (FRHIUserEffectLayout.Samplers[I].SlotIndex <= High(FRHIUserEffectTextures)) and
             (FRHIUserEffectTextures[FRHIUserEffectLayout.Samplers[I].SlotIndex] = nil) then
          begin
            if FRHIUserEffectLayout.Samplers[I].GLSLType = 'samplerCube' then
              FRHISkinningDescriptorSet.BindTexture(2 + Cardinal(FRHIUserEffectLayout.Samplers[I].SlotIndex),
                TVulkanRHIDevice(RHIDevice).GetDefaultWhiteCubeTexture, nil)
            else if FRHIUserEffectLayout.Samplers[I].GLSLType = 'sampler2DShadow' then
              FRHISkinningDescriptorSet.BindTexture(2 + Cardinal(FRHIUserEffectLayout.Samplers[I].SlotIndex),
                TVulkanRHIDevice(RHIDevice).GetDefaultShadowDepthTexture,
                TVulkanRHIDevice(RHIDevice).GetDefaultShadowSampler);
          end;
        FRHISkinningDescriptorSet.Update;
      end;
      {$endif}
    end;

    FRHIVertexShaderSource := Gen.GenerateDefault3DVertexShader(
      AHasSkinning, AHasTangents, NeedTexCoord1Attr, AHasColorPerVertex);
    if AHasTexCoordGen or (FRHITexCoordGen <> tgExplicit) then
    begin
      case FRHITexCoordGen of
        tgBounds2d, tgBounds3d:
          TexGenSnippet := FormatDot(
            '  vec2 rawTexCoord0 = vec2(dot(localPos, vec4(%g, %g, %g, %g)), dot(localPos, vec4(%g, %g, %g, %g)));',
            [FRHITexGenBoundsS.X, FRHITexGenBoundsS.Y, FRHITexGenBoundsS.Z, FRHITexGenBoundsS.W,
             FRHITexGenBoundsT.X, FRHITexGenBoundsT.Y, FRHITexGenBoundsT.Z, FRHITexGenBoundsT.W]);
        tgSphereMap:
          TexGenSnippet :=
            '  vec3 rSphere = reflect(normalize(castle_vertex_eye.xyz), castle_normal_eye);' + LineEnding +
            '  float mSphere = 2.0 * sqrt(rSphere.x * rSphere.x + rSphere.y * rSphere.y + (rSphere.z + 1.0) * (rSphere.z + 1.0));' + LineEnding +
            '  vec2 rawTexCoord0 = rSphere.xy / max(mSphere, 1e-5) + vec2(0.5);';
        tgCoord:
          TexGenSnippet := '  vec2 rawTexCoord0 = localPos.xy;';
        tgCoordEye:
          TexGenSnippet := '  vec2 rawTexCoord0 = castle_vertex_eye.xy;';
        tgCameraSpaceNormal:
          TexGenSnippet := '  vec2 rawTexCoord0 = castle_normal_eye.xy;';
        tgWorldSpaceNormal:
          TexGenSnippet := '  vec2 rawTexCoord0 = (mat3(castle_ViewMatrixInverse) * castle_normal_eye).xy;';
        tgCameraSpaceReflectionVector:
          TexGenSnippet := '  vec2 rawTexCoord0 = reflect(normalize(castle_vertex_eye.xyz), castle_normal_eye).xy;';
        tgWorldSpaceReflectionVector:
          TexGenSnippet := '  vec2 rawTexCoord0 = (mat3(castle_ViewMatrixInverse) * reflect(normalize(castle_vertex_eye.xyz), castle_normal_eye)).xy;';
        tgProjection:
          TexGenSnippet := FormatDot(
            '  vec4 worldPosProj = castle_ViewMatrixInverse * castle_vertex_eye;' + LineEnding +
            '  vec4 projClip = mat4(' +
            'vec4(%g, %g, %g, %g), ' +
            'vec4(%g, %g, %g, %g), ' +
            'vec4(%g, %g, %g, %g), ' +
            'vec4(%g, %g, %g, %g)) * worldPosProj;' + LineEnding +
            '  vec2 rawTexCoord0 = projClip.xy / max(projClip.w, 1e-5);',
            [FRHITexGenProjectorMatrix.Data[0, 0], FRHITexGenProjectorMatrix.Data[0, 1], FRHITexGenProjectorMatrix.Data[0, 2], FRHITexGenProjectorMatrix.Data[0, 3],
             FRHITexGenProjectorMatrix.Data[1, 0], FRHITexGenProjectorMatrix.Data[1, 1], FRHITexGenProjectorMatrix.Data[1, 2], FRHITexGenProjectorMatrix.Data[1, 3],
             FRHITexGenProjectorMatrix.Data[2, 0], FRHITexGenProjectorMatrix.Data[2, 1], FRHITexGenProjectorMatrix.Data[2, 2], FRHITexGenProjectorMatrix.Data[2, 3],
             FRHITexGenProjectorMatrix.Data[3, 0], FRHITexGenProjectorMatrix.Data[3, 1], FRHITexGenProjectorMatrix.Data[3, 2], FRHITexGenProjectorMatrix.Data[3, 3]]);
        tgMirrorPlane:
          if FRHIHasMirrorPlaneUniforms and
             (Abs(FRHIMirrorFrustumDimensions.Z) > 1e-5) and
             (Abs(FRHIMirrorFrustumDimensions.W) > 1e-5) then
            TexGenSnippet := FormatDot(
              '  vec4 mirrorWorldPos = castle_ViewMatrixInverse * castle_vertex_eye;' + LineEnding +
              '  vec4 castle_NormalizedPlane = vec4(%g, %g, %g, %g);' + LineEnding +
              '  vec3 castle_CameraPositionOnPlane = vec3(%g, %g, %g);' + LineEnding +
              '  vec3 castle_CameraSide = vec3(%g, %g, %g);' + LineEnding +
              '  vec3 castle_CameraUp = vec3(%g, %g, %g);' + LineEnding +
              '  vec4 castle_FrustumDimensions = vec4(%g, %g, %g, %g);' + LineEnding +
              '  float mirrorPlaneD = -dot(castle_NormalizedPlane.xyz, mirrorWorldPos.xyz) - castle_NormalizedPlane.w;' + LineEnding +
              '  vec3 vertexOnPlane = mirrorWorldPos.xyz + castle_NormalizedPlane.xyz * mirrorPlaneD - castle_CameraPositionOnPlane;' + LineEnding +
              '  vec2 mirrorProjected = vec2(dot(vertexOnPlane, castle_CameraSide), dot(vertexOnPlane, castle_CameraUp));' + LineEnding +
              '  vec2 rawTexCoord0 = vec2((mirrorProjected.x - castle_FrustumDimensions.x) / castle_FrustumDimensions.z, (mirrorProjected.y - castle_FrustumDimensions.y) / castle_FrustumDimensions.w);',
              [FRHIMirrorNormalizedPlane.X, FRHIMirrorNormalizedPlane.Y, FRHIMirrorNormalizedPlane.Z, FRHIMirrorNormalizedPlane.W,
               FRHIMirrorCameraPositionOnPlane.X, FRHIMirrorCameraPositionOnPlane.Y, FRHIMirrorCameraPositionOnPlane.Z,
               FRHIMirrorCameraSide.X, FRHIMirrorCameraSide.Y, FRHIMirrorCameraSide.Z,
               FRHIMirrorCameraUp.X, FRHIMirrorCameraUp.Y, FRHIMirrorCameraUp.Z,
               FRHIMirrorFrustumDimensions.X, FRHIMirrorFrustumDimensions.Y, FRHIMirrorFrustumDimensions.Z, FRHIMirrorFrustumDimensions.W])
          else
            TexGenSnippet :=
              '  vec4 mirrorClip = castle_ProjectionMatrix * castle_vertex_eye;' + LineEnding +
              '  vec2 rawTexCoord0 = (mirrorClip.xy / max(mirrorClip.w, 1e-5)) * 0.5 + vec2(0.5);';
        else
          TexGenSnippet := '  vec2 rawTexCoord0 = castle_MultiTexCoord0;';
      end;
      if AHasTextureTransform and (FRHIBaseColorTextureDimension <> td3D) then
      begin
        TexTransformSnippet := FormatDot(
          TexGenSnippet + LineEnding +
          '  castle_tex_coord0 = (mat4(' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g)) * vec4(rawTexCoord0, 0.0, 1.0)).xy;',
          [FRHITextureTransform.Data[0, 0], FRHITextureTransform.Data[0, 1], FRHITextureTransform.Data[0, 2], FRHITextureTransform.Data[0, 3],
           FRHITextureTransform.Data[1, 0], FRHITextureTransform.Data[1, 1], FRHITextureTransform.Data[1, 2], FRHITextureTransform.Data[1, 3],
           FRHITextureTransform.Data[2, 0], FRHITextureTransform.Data[2, 1], FRHITextureTransform.Data[2, 2], FRHITextureTransform.Data[2, 3],
           FRHITextureTransform.Data[3, 0], FRHITextureTransform.Data[3, 1], FRHITextureTransform.Data[3, 2], FRHITextureTransform.Data[3, 3]]);
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_tex_coord0 = castle_MultiTexCoord0;',
          TexTransformSnippet,
          []);
      end else
      begin
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_tex_coord0 = castle_MultiTexCoord0;',
          TexGenSnippet + LineEnding + '  castle_tex_coord0 = rawTexCoord0;',
          []);
      end;
    end else
    if AHasTextureTransform then
    begin
      TexTransformSnippet := FormatDot(
        '  castle_tex_coord0 = (mat4(' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g)) * vec4(castle_MultiTexCoord0, 0.0, 1.0)).xy;',
        [FRHITextureTransform.Data[0, 0], FRHITextureTransform.Data[0, 1], FRHITextureTransform.Data[0, 2], FRHITextureTransform.Data[0, 3],
          FRHITextureTransform.Data[1, 0], FRHITextureTransform.Data[1, 1], FRHITextureTransform.Data[1, 2], FRHITextureTransform.Data[1, 3],
          FRHITextureTransform.Data[2, 0], FRHITextureTransform.Data[2, 1], FRHITextureTransform.Data[2, 2], FRHITextureTransform.Data[2, 3],
          FRHITextureTransform.Data[3, 0], FRHITextureTransform.Data[3, 1], FRHITextureTransform.Data[3, 2], FRHITextureTransform.Data[3, 3]]);
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        '  castle_tex_coord0 = castle_MultiTexCoord0;',
        TexTransformSnippet,
        []);
    end;
    if FRHIHasTextureTransform1 then
    begin
      TexTransformSnippet := FormatDot(
        '  castle_tex_coord1 = (mat4(' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g), ' +
        'vec4(%g, %g, %g, %g)) * vec4(castle_MultiTexCoord1, 0.0, 1.0)).xy;',
        [FRHITextureTransform1.Data[0, 0], FRHITextureTransform1.Data[0, 1], FRHITextureTransform1.Data[0, 2], FRHITextureTransform1.Data[0, 3],
         FRHITextureTransform1.Data[1, 0], FRHITextureTransform1.Data[1, 1], FRHITextureTransform1.Data[1, 2], FRHITextureTransform1.Data[1, 3],
         FRHITextureTransform1.Data[2, 0], FRHITextureTransform1.Data[2, 1], FRHITextureTransform1.Data[2, 2], FRHITextureTransform1.Data[2, 3],
         FRHITextureTransform1.Data[3, 0], FRHITextureTransform1.Data[3, 1], FRHITextureTransform1.Data[3, 2], FRHITextureTransform1.Data[3, 3]]);
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        '  castle_tex_coord1 = castle_MultiTexCoord1;',
        TexTransformSnippet,
        []);
    end;
    if DedupVertEffect <> '' then
    begin
      UpgradedVertEffect := Gen.UpgradeUserEffectStageSource(
        ssVertex, DedupVertEffect, FRHIUserEffectLayout);
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        'void main() {',
        UpgradedVertEffect + LineEnding + 'void main() {',
        []);
      if HasPlugFunction(DedupVertEffect, 'PLUG_vertex_object_space') then
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  vec3 localTangent = castle_Tangent.xyz;',
          '  vec3 localTangent = castle_Tangent.xyz;' + LineEnding +
          BuildChainedPlugCalls(DedupVertEffect, 'PLUG_vertex_object_space', 'localPos, localNormal', '  '),
          []);
      if HasPlugFunction(DedupVertEffect, 'PLUG_vertex_object_space_change') then
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  vec3 localTangent = castle_Tangent.xyz;',
          '  vec3 localTangent = castle_Tangent.xyz;' + LineEnding +
          BuildChainedPlugCalls(DedupVertEffect, 'PLUG_vertex_object_space_change', 'localPos, localNormal', '  '),
          []);
      if HasPlugFunction(DedupVertEffect, 'PLUG_vertex_eye_space_extended') then
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_normal_eye = normalize(castle_NormalMatrix * localNormal);',
          '  castle_normal_eye = normalize(castle_NormalMatrix * localNormal);' + LineEnding +
          BuildChainedPlugCalls(DedupVertEffect, 'PLUG_vertex_eye_space_extended', 'castle_vertex_eye, castle_normal_eye, localNormal', '  '),
          []);
      if HasPlugFunction(DedupVertEffect, 'PLUG_vertex_eye_space') then
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_normal_eye = normalize(castle_NormalMatrix * localNormal);',
          '  castle_normal_eye = normalize(castle_NormalMatrix * localNormal);' + LineEnding +
          BuildChainedPlugCalls(DedupVertEffect, 'PLUG_vertex_eye_space', 'castle_vertex_eye, castle_normal_eye', '  '),
          []);
    end;
    if (TexCoord0Dim in [3, 4]) and (not AHasTexCoordGen) and (FRHITexCoordGen = tgExplicit) then
    begin
      if TexCoord0Dim = 3 then
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          'CASTLE_LOCATION(2) in vec2 castle_MultiTexCoord0;',
          'CASTLE_LOCATION(2) in vec3 castle_MultiTexCoord0;',
          [])
      else
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          'CASTLE_LOCATION(2) in vec2 castle_MultiTexCoord0;',
          'CASTLE_LOCATION(2) in vec4 castle_MultiTexCoord0;',
          []);
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        'void main() {',
        'CASTLE_LOCATION(9) out vec4 castle_tex_coord0_4d;' + LineEnding + 'void main() {',
        []);
      if TexCoord0Dim = 3 then
        TexGenSnippet := '  vec4 rawTexCoord4D = vec4(castle_MultiTexCoord0, 1.0);'
      else
        TexGenSnippet := '  vec4 rawTexCoord4D = castle_MultiTexCoord0;';
      if AHasTextureTransform then
      begin
        TexTransformSnippet := FormatDot(
          TexGenSnippet + LineEnding +
          '  castle_tex_coord0_4d = mat4(' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g), ' +
          'vec4(%g, %g, %g, %g)) * rawTexCoord4D;' + LineEnding +
          '  castle_tex_coord0 = castle_tex_coord0_4d.xy / max(castle_tex_coord0_4d.w, 1e-5);',
          [FRHITextureTransform.Data[0, 0], FRHITextureTransform.Data[0, 1], FRHITextureTransform.Data[0, 2], FRHITextureTransform.Data[0, 3],
           FRHITextureTransform.Data[1, 0], FRHITextureTransform.Data[1, 1], FRHITextureTransform.Data[1, 2], FRHITextureTransform.Data[1, 3],
           FRHITextureTransform.Data[2, 0], FRHITextureTransform.Data[2, 1], FRHITextureTransform.Data[2, 2], FRHITextureTransform.Data[2, 3],
           FRHITextureTransform.Data[3, 0], FRHITextureTransform.Data[3, 1], FRHITextureTransform.Data[3, 2], FRHITextureTransform.Data[3, 3]]);
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_tex_coord0 = (mat4(' + FormatDot(
            'vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g)) * vec4(castle_MultiTexCoord0, 0.0, 1.0)).xy;',
            [FRHITextureTransform.Data[0, 0], FRHITextureTransform.Data[0, 1], FRHITextureTransform.Data[0, 2], FRHITextureTransform.Data[0, 3],
             FRHITextureTransform.Data[1, 0], FRHITextureTransform.Data[1, 1], FRHITextureTransform.Data[1, 2], FRHITextureTransform.Data[1, 3],
             FRHITextureTransform.Data[2, 0], FRHITextureTransform.Data[2, 1], FRHITextureTransform.Data[2, 2], FRHITextureTransform.Data[2, 3],
             FRHITextureTransform.Data[3, 0], FRHITextureTransform.Data[3, 1], FRHITextureTransform.Data[3, 2], FRHITextureTransform.Data[3, 3]]),
          TexTransformSnippet,
          []);
      end else
      begin
        FRHIVertexShaderSource := StringReplace(
          FRHIVertexShaderSource,
          '  castle_tex_coord0 = castle_MultiTexCoord0;',
          TexGenSnippet + LineEnding +
          '  castle_tex_coord0_4d = rawTexCoord4D;' + LineEnding +
          '  castle_tex_coord0 = castle_tex_coord0_4d.xy / max(castle_tex_coord0_4d.w, 1e-5);',
          []);
      end;
    end;
    if (Arrays <> nil) and Arrays.HasFogCoord then
    begin
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        'void main() {',
        'CASTLE_LOCATION(8) in float castle_FogCoord;' + LineEnding +
        'CASTLE_LOCATION(8) out float castle_fog_frag_coord;' + LineEnding +
        'void main() {',
        []);
      FRHIVertexShaderSource := StringReplace(
        FRHIVertexShaderSource,
        '  gl_Position = ',
        '  castle_fog_frag_coord = castle_FogCoord;' + LineEnding + '  gl_Position = ',
        []);
    end;
    EffectivePointSizeStr := FormatDot('%g', [Max(1.0, FRHIPointSize)]);
    if Pos('.', EffectivePointSizeStr) = 0 then
      EffectivePointSizeStr := EffectivePointSizeStr + '.0';
    FRHIVertexShaderSource := StringReplace(
      FRHIVertexShaderSource,
      '  gl_Position = ',
      '  gl_PointSize = ' + EffectivePointSizeStr + ';' + LineEnding + '  gl_Position = ',
      []);
    if AHasColorPerVertex then
      FRHIVertexShaderSource := FRHIVertexShaderSource + LineEnding + '/* CASTLE_PERM_COLOR_PER_VERTEX */';
    if (AHasBaseColorTex and FRHIHasSecondTexture) or
       (FRHIBaseColorTexCoordIndex > 0) or
       (FRHINormalTexCoordIndex > 0) or
       (FRHIMetallicRoughnessTexCoordIndex > 0) or
       (FRHIEmissiveTexCoordIndex > 0) or
       (FRHIOcclusionTexCoordIndex > 0) then
      FRHIVertexShaderSource := FRHIVertexShaderSource + LineEnding + '/* CASTLE_PERM_MULTITEX_UV1 */';
    FRHIFragmentShaderSource := Gen.GenerateDefault3DFragmentShader(
      AHasBaseColorTex, AHasNormalTex, AHasMetallicRoughnessTex,
      AHasEmissiveTex, AHasOcclusionTex, AHasIBL, AUseAlphaTest, False, False);
    if HasLinearColorSpace then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '#define CASTLE_VULKAN 1',
        '#define CASTLE_VULKAN 1' + LineEnding + '#define CASTLE_GAMMA_CORRECTION 1',
        []);
    if (FRHILineType <> ltSolid) and (FRHILineStipplePattern <> $FFFF) then
    begin
      LineStippleSnippet := Format(
        '  if (((%du >> (uint(floor(gl_FragCoord.x + gl_FragCoord.y)) & 15u)) & 1u) == 0u) discard;' + LineEnding +
        '  vec4 baseColor = castle_MaterialBaseColor;',
        [Cardinal(FRHILineStipplePattern)]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  vec4 baseColor = castle_MaterialBaseColor;',
        LineStippleSnippet,
        []);
    end;
    if AHasClipPlane then
    begin
      ClipPlaneSnippet := '';
      ClipCount := Min(Max(Cardinal(1), FRHIClipPlaneCount), Cardinal(4));
      for I := 0 to Integer(ClipCount) - 1 do
      begin
        if FRHIClipPlaneCount > 0 then
          Plane := FRHIClipPlanes[I]
        else
          Plane := FRHIClipPlane;
        ClipPlaneSnippet := ClipPlaneSnippet + FormatDot(
          '  if (dot(vec4(%g, %g, %g, %g), castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0)) < 0.0) discard;' + LineEnding,
          [Plane.X, Plane.Y, Plane.Z, Plane.W]);
      end;
      ClipPlaneSnippet := ClipPlaneSnippet + '  vec4 baseColor = castle_MaterialBaseColor;';
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  vec4 baseColor = castle_MaterialBaseColor;',
        ClipPlaneSnippet,
        []);
    end;
    if AHasColorPerVertex then
    begin
      if (Arrays <> nil) and (Arrays.ColorMode = cmReplace) then
      begin
        if Arrays.ColorType = ctRgb then
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = vec4(castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);',
            [])
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_color_per_vertex;',
            []);
      end else
      begin
        if (Arrays <> nil) and (Arrays.ColorType = ctRgb) then
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = vec4(castle_MaterialBaseColor.rgb * castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);',
            [])
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_MaterialBaseColor * castle_color_per_vertex;',
            []);
      end;
    end;
    if AHasShadowMap then
    begin
      if EffectiveShadowSampling = ssVarianceShadowMaps then
      begin
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'sampler2DShadow castle_shadow_map_0;',
          'sampler2D castle_shadow_map_0;',
          []);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'sampler2DShadow castle_shadow_map_1;',
          'sampler2D castle_shadow_map_1;',
          []);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'sampler2DShadow castle_shadow_map_2;',
          'sampler2D castle_shadow_map_2;',
          []);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'sampler2DShadow castle_shadow_map_3;',
          'sampler2D castle_shadow_map_3;',
          []);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'vec2 castle_sample_vsm_slot(float slot, vec2 uv) {' + LineEnding +
          '  if (slot < 1.5) return texture(castle_shadow_map_0, uv).rg;' + LineEnding +
          '  if (slot < 2.5) return texture(castle_shadow_map_1, uv).rg;' + LineEnding +
          '  if (slot < 3.5) return texture(castle_shadow_map_2, uv).rg;' + LineEnding +
          '  return texture(castle_shadow_map_3, uv).rg;' + LineEnding +
          '}' + LineEnding +
          'void main() {',
          []);
      end else
      begin
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'float castle_sample_shadow_slot(float slot, vec3 coord) {' + LineEnding +
          '  if (slot < 1.5) return texture(castle_shadow_map_0, coord);' + LineEnding +
          '  if (slot < 2.5) return texture(castle_shadow_map_1, coord);' + LineEnding +
          '  if (slot < 3.5) return texture(castle_shadow_map_2, coord);' + LineEnding +
          '  return texture(castle_shadow_map_3, coord);' + LineEnding +
          '}' + LineEnding +
          '#define castle_sample_shadow_slot_0(slot, coord) ((slot < 1.5) ? texture(castle_shadow_map_0, coord) : castle_sample_shadow_slot(slot, coord))' + LineEnding +
          'void main() {',
          []);
      end;
      case EffectiveShadowSampling of
        ssPCF4:
          ShadowMapSnippet := FormatDot(
            '    float shadowVis = 1.0;' + LineEnding +
            '    if (light.SpotParams.w > 0.5) {' + LineEnding +
            '      vec4 shadowClip = light.ProjectorMatrix * vec4(castle_vertex_eye.xyz, 1.0);' + LineEnding +
            '      if (shadowClip.w > 1e-5) {' + LineEnding +
            '        vec3 shadowProj = shadowClip.xyz / shadowClip.w;' + LineEnding +
            '        if (shadowProj.x >= 0.0 && shadowProj.x <= 1.0 && shadowProj.y >= 0.0 && shadowProj.y <= 1.0 && shadowProj.z >= 0.0 && shadowProj.z <= 1.0) {' + LineEnding +
            '          float shadowZ = shadowProj.z - 0.002;' + LineEnding +
            '          float shadowOff = 1.0 / %g;' + LineEnding +
            '          shadowVis = 0.25 * (' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.x - shadowOff, shadowProj.y - shadowOff, shadowZ)) +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.x - shadowOff, shadowProj.y + shadowOff, shadowZ)) +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.x + shadowOff, shadowProj.y - shadowOff, shadowZ)) +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.x + shadowOff, shadowProj.y + shadowOff, shadowZ)));' + LineEnding +
            '        }' + LineEnding +
            '      }' + LineEnding +
            '    }',
            [Single(EffectiveShadowSize) * 2.0]);
        ssPCF4Bilinear:
          ShadowMapSnippet := FormatDot(
            '    float shadowVis = 1.0;' + LineEnding +
            '    if (light.SpotParams.w > 0.5) {' + LineEnding +
            '      vec4 shadowClip = light.ProjectorMatrix * vec4(castle_vertex_eye.xyz, 1.0);' + LineEnding +
            '      if (shadowClip.w > 1e-5) {' + LineEnding +
            '        vec3 shadowProj = shadowClip.xyz / shadowClip.w;' + LineEnding +
            '        if (shadowProj.x >= 0.0 && shadowProj.x <= 1.0 && shadowProj.y >= 0.0 && shadowProj.y <= 1.0 && shadowProj.z >= 0.0 && shadowProj.z <= 1.0) {' + LineEnding +
            '          float shadowZ = shadowProj.z - 0.002;' + LineEnding +
            '          float shadowSize = %g;' + LineEnding +
            '          vec2 tc_full = shadowSize * shadowProj.xy;' + LineEnding +
            '          vec2 tc = floor(tc_full);' + LineEnding +
            '          vec2 f = fract(tc_full);' + LineEnding +
            '          vec2 f1 = vec2(1.0) - f;' + LineEnding +
            '          shadowVis =' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3((tc.x) / shadowSize, (tc.y) / shadowSize, shadowZ)) * f1.x * f1.y +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3((tc.x) / shadowSize, (tc.y + 1.0) / shadowSize, shadowZ)) * f1.x * f.y +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3((tc.x + 1.0) / shadowSize, (tc.y) / shadowSize, shadowZ)) * f.x * f1.y +' + LineEnding +
            '            castle_sample_shadow_slot(light.SpotParams.w, vec3((tc.x + 1.0) / shadowSize, (tc.y + 1.0) / shadowSize, shadowZ)) * f.x * f.y;' + LineEnding +
            '        }' + LineEnding +
            '      }' + LineEnding +
            '    }',
            [Single(EffectiveShadowSize)]);
        ssPCF16:
          ShadowMapSnippet := FormatDot(
            '    float shadowVis = 1.0;' + LineEnding +
            '    if (light.SpotParams.w > 0.5) {' + LineEnding +
            '      vec4 shadowClip = light.ProjectorMatrix * vec4(castle_vertex_eye.xyz, 1.0);' + LineEnding +
            '      if (shadowClip.w > 1e-5) {' + LineEnding +
            '        vec3 shadowProj = shadowClip.xyz / shadowClip.w;' + LineEnding +
            '        if (shadowProj.x >= 0.0 && shadowProj.x <= 1.0 && shadowProj.y >= 0.0 && shadowProj.y <= 1.0 && shadowProj.z >= 0.0 && shadowProj.z <= 1.0) {' + LineEnding +
            '          float shadowZ = shadowProj.z - 0.002;' + LineEnding +
            '          float shadowOff = 1.0 / %g;' + LineEnding +
            '          float shadowSum = 0.0;' + LineEnding +
            '          for (int sy = 0; sy < 4; ++sy)' + LineEnding +
            '            for (int sx = 0; sx < 4; ++sx)' + LineEnding +
            '              shadowSum += ((light.SpotParams.w < 1.5) ? texture(castle_shadow_map_0, vec3(shadowProj.xy + (vec2(float(sx), float(sy)) - vec2(1.5)) * shadowOff, shadowZ)) : castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.xy + (vec2(float(sx), float(sy)) - vec2(1.5)) * shadowOff, shadowZ)));' + LineEnding +
            '          shadowVis = shadowSum * (1.0 / 16.0);' + LineEnding +
            '        }' + LineEnding +
            '      }' + LineEnding +
            '    }',
            [Single(EffectiveShadowSize)]);
        ssVarianceShadowMaps:
          begin
            ShadowMapSnippet :=
              '    float shadowVis = 1.0;' + LineEnding +
              '    if (light.SpotParams.w > 0.5) {' + LineEnding +
              '      vec4 shadowClip = light.ProjectorMatrix * vec4(castle_vertex_eye.xyz, 1.0);' + LineEnding +
              '      if (shadowClip.w > 1e-5) {' + LineEnding +
              '        vec3 shadowProj = shadowClip.xyz / shadowClip.w;' + LineEnding +
              '        if (shadowProj.x >= 0.0 && shadowProj.x <= 1.0 && shadowProj.y >= 0.0 && shadowProj.y <= 1.0 && shadowProj.z >= 0.0 && shadowProj.z <= 1.0) {' + LineEnding +
              '          vec2 vsmMoments = (light.SpotParams.w < 1.5) ? texture(castle_shadow_map_0, shadowProj.xy).rg : castle_sample_vsm_slot(light.SpotParams.w, shadowProj.xy);' + LineEnding +
              '          float vsmVariance = max(vsmMoments.y - vsmMoments.x * vsmMoments.x, 1e-5);' + LineEnding +
              '          float vsmDelta = vsmMoments.x - shadowProj.z;' + LineEnding +
              '          shadowVis = (shadowProj.z <= vsmMoments.x) ? 1.0 : (vsmVariance / (vsmVariance + vsmDelta * vsmDelta));' + LineEnding +
              '        }' + LineEnding +
              '      }' + LineEnding +
              '    }';
          end;
        else
          ShadowMapSnippet :=
            '    float shadowVis = 1.0;' + LineEnding +
            '    if (light.SpotParams.w > 0.5) {' + LineEnding +
            '      vec4 shadowClip = light.ProjectorMatrix * vec4(castle_vertex_eye.xyz, 1.0);' + LineEnding +
            '      if (shadowClip.w > 1e-5) {' + LineEnding +
            '        vec3 shadowProj = shadowClip.xyz / shadowClip.w;' + LineEnding +
            '        if (shadowProj.x >= 0.0 && shadowProj.x <= 1.0 && shadowProj.y >= 0.0 && shadowProj.y <= 1.0 && shadowProj.z >= 0.0 && shadowProj.z <= 1.0)' + LineEnding +
            '          shadowVis = (light.SpotParams.w < 1.5) ? texture(castle_shadow_map_0, vec3(shadowProj.xy, shadowProj.z - 0.002)) : castle_sample_shadow_slot(light.SpotParams.w, vec3(shadowProj.xy, shadowProj.z - 0.002));' + LineEnding +
            '      }' + LineEnding +
            '    }';
      end;
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '    float shadowVis = 1.0;',
        ShadowMapSnippet,
        []);
    end;
    if (Arrays <> nil) and Arrays.HasFogCoord then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'void main() {',
        'CASTLE_LOCATION(8) in float castle_fog_frag_coord;' + LineEnding +
        'void main() {',
        []);
      if Arrays.FogDirectValues then
        FogSnippet :=
          LineEnding +
          '  if (CastleScene.u_FogParams.z > 0.5) {' + LineEnding +
          '    float fogFactor = clamp(castle_fog_frag_coord, 0.0, 1.0);' + LineEnding +
          '    castle_FragColor.rgb = mix(castle_FragColor.rgb, CastleScene.u_FogColor.rgb, fogFactor);' + LineEnding +
          '  }'
      else
        FogSnippet :=
          LineEnding +
          '  if ((CastleScene.u_FogParams.z > 0.5) && (CastleScene.u_FogParams.y > 1e-5)) {' + LineEnding +
          '    float fogDist = max(castle_fog_frag_coord, 0.0);' + LineEnding +
          '    float fogFactor = (CastleScene.u_FogParams.z > 1.5)' + LineEnding +
          '      ? clamp(1.0 - exp(-CastleScene.u_FogColor.a * fogDist), 0.0, 1.0)' + LineEnding +
          '      : clamp(fogDist / CastleScene.u_FogParams.y, 0.0, 1.0);' + LineEnding +
          '    castle_FragColor.rgb = mix(castle_FragColor.rgb, CastleScene.u_FogColor.rgb, fogFactor);' + LineEnding +
          '  }';
    end else
      FogSnippet :=
        LineEnding +
        '  if ((CastleScene.u_FogParams.z > 0.5) && (CastleScene.u_FogParams.y > 1e-5)) {' + LineEnding +
        '    float fogDist = length(castle_vertex_eye.xyz);' + LineEnding +
        '    float fogFactor = (CastleScene.u_FogParams.z > 1.5)' + LineEnding +
        '      ? clamp(1.0 - exp(-CastleScene.u_FogColor.a * fogDist), 0.0, 1.0)' + LineEnding +
        '      : clamp(fogDist / CastleScene.u_FogParams.y, 0.0, 1.0);' + LineEnding +
        '    castle_FragColor.rgb = mix(castle_FragColor.rgb, CastleScene.u_FogColor.rgb, fogFactor);' + LineEnding +
        '  }';
    if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_fog_apply') then
      FogSnippet := FogSnippet + LineEnding +
        BuildChainedPlugCalls(DedupFragEffect, 'PLUG_fog_apply', 'castle_FragColor, normalize(castle_normal_eye)', '  ');
    if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_steep_parallax_shadow_apply') then
      FogSnippet := FogSnippet + LineEnding +
        BuildChainedPlugCalls(DedupFragEffect, 'PLUG_steep_parallax_shadow_apply', 'castle_FragColor', '  ');
    if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_fragment_modify') then
      FogSnippet := FogSnippet + LineEnding +
        BuildChainedPlugCalls(DedupFragEffect, 'PLUG_fragment_modify', 'castle_FragColor', '  ');
    case EffectiveToneMapping of
      tmUncharted:
        FogSnippet := FogSnippet + LineEnding + '  castle_FragColor.rgb = toneMapUncharted(castle_FragColor.rgb);';
      tmHejlRichard:
        FogSnippet := FogSnippet + LineEnding + '  castle_FragColor.rgb = toneMapHejlRichard(castle_FragColor.rgb);';
      tmACES:
        FogSnippet := FogSnippet + LineEnding + '  castle_FragColor.rgb = toneMapACES(castle_FragColor.rgb);';
      else ;
    end;
    if HasLinearColorSpace then
      FogSnippet := FogSnippet + LineEnding + '  castle_FragColor.rgb = LINEARtoSRGB(castle_FragColor.rgb);';
    if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_fragment_end') then
      FogSnippet := FogSnippet + LineEnding +
        BuildChainedPlugCalls(DedupFragEffect, 'PLUG_fragment_end', 'castle_FragColor', '  ');
    FRHIFragmentShaderSource := StringReplace(
      FRHIFragmentShaderSource,
      '    castle_FragColor = vec4(unlitColor, baseColor.a);',
      '    castle_FragColor = vec4(unlitColor, baseColor.a);' + FogSnippet,
      []);
    if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_lighting_apply') then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  castle_FragColor = vec4(Lo + emissive, baseColor.a);',
        '  castle_FragColor = vec4(Lo + emissive, baseColor.a);' + LineEnding +
        BuildChainedPlugCalls(DedupFragEffect, 'PLUG_lighting_apply', 'castle_FragColor, castle_vertex_eye, N', '  ') + FogSnippet,
        [])
    else
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  castle_FragColor = vec4(Lo + emissive, baseColor.a);',
        '  castle_FragColor = vec4(Lo + emissive, baseColor.a);' + FogSnippet,
        []);
    if FRHIIsPhongMaterial then
    begin
      if FRHIPhongHasSeparateShininessTex then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'CASTLE_SAMPLER(2, 4) uniform sampler2D castle_shininess_texture;' + LineEnding +
          'void main() {',
          []);
      PhongSetupSnippet := FormatDot(
        '  vec3 castle_specular_color = castle_MaterialSpecular;' + LineEnding +
        '  float castle_shininess_exp = max(castle_MaterialShininess, 1.0);' + LineEnding +
        '  vec3 castle_ambient_color = baseColor.rgb * %.4f;',
        [FRHIPhongAmbientIntensity]);
      if AHasMetallicRoughnessTex and FRHIPhongHasSpecularTex then
      begin
        if HasLinearColorSpace then
          PhongSetupSnippet := PhongSetupSnippet + LineEnding +
            '  castle_specular_color *= pow(max(mrSample.rgb, vec3(0.0)), vec3(2.2));'
        else
          PhongSetupSnippet := PhongSetupSnippet + LineEnding +
            '  castle_specular_color *= mrSample.rgb;';
      end;
      if FRHIPhongHasSeparateShininessTex then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          '  castle_shininess_exp = max(castle_shininess_exp * texture(castle_shininess_texture, castle_tex_coord0).a, 1.0);'
      else if AHasMetallicRoughnessTex and FRHIPhongHasShininessTex then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          '  castle_shininess_exp = max(castle_shininess_exp * mrSample.a, 1.0);';
      if AHasOcclusionTex and FRHIPhongHasAmbientTex then
      begin
        if HasLinearColorSpace then
          PhongSetupSnippet := PhongSetupSnippet + LineEnding +
            '  castle_ambient_color *= pow(max(texture(castle_occlusion_texture, castle_tex_coord0).rgb, vec3(0.0)), vec3(2.2));' + LineEnding +
            '  ao = 1.0;'
        else
          PhongSetupSnippet := PhongSetupSnippet + LineEnding +
            '  castle_ambient_color *= texture(castle_occlusion_texture, castle_tex_coord0).rgb;' + LineEnding +
            '  ao = 1.0;';
      end;
      if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_material_specular') then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_specular', 'castle_specular_color', '  ');
      if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_material_ambient') then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_ambient', 'castle_ambient_color', '  ');
      if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_material_light_ambient') then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_light_ambient', 'castle_ambient_color', '  ');
      if (DedupFragEffect <> '') and HasPlugFunction(DedupFragEffect, 'PLUG_material_shininess') then
        PhongSetupSnippet := PhongSetupSnippet + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_shininess', 'castle_shininess_exp', '  ');
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  vec3 N = normalize(castle_normal_eye);',
        PhongSetupSnippet + LineEnding + '  vec3 N = normalize(castle_normal_eye);',
        []);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '    Lo += EvaluateDirectCookTorranceGGX(N, V, L, radiance, baseColor.rgb, metallic, roughness);' + LineEnding +
        '    Lo += light.AmbientColor.rgb * baseColor.rgb * (ao * attenuation);',
        '    float castle_ndotl = max(dot(N, L), 0.0);' + LineEnding +
        '    vec3 castle_half_vec = normalize(L + V);' + LineEnding +
        '    float castle_spec_factor = (castle_ndotl > 0.0) ? pow(max(dot(castle_half_vec, N), 0.0), castle_shininess_exp) : 0.0;' + LineEnding +
        '    Lo += (baseColor.rgb * castle_ndotl + castle_specular_color * castle_spec_factor) * radiance;' + LineEnding +
        '    Lo += light.AmbientColor.rgb * castle_ambient_color * (ao * attenuation);',
        []);
    end;
    if AHasIBL then
    begin
      if FRHIHasSeparateDiffuseEnvTex then
      begin
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'CASTLE_SAMPLER(2, 5) uniform samplerCube castle_env_diffuse_texture;' + LineEnding +
          'void main() {',
          []);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  vec3 diffuseIrradiance = textureLod(castle_env_specular_texture, mat3(castle_ViewMatrixInverse) * N, 4.0).rgb;',
          '  vec3 diffuseIrradiance = texture(castle_env_diffuse_texture, mat3(castle_ViewMatrixInverse) * N).rgb;',
          []);
      end;
      if not TVector3.Equals(FRHIEnvLightColorIntensity, Vector3(1.0, 1.0, 1.0), 1e-4) then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  Lo += EvaluateIBLCookTorranceGGX(N, V, baseColor.rgb, metallic, roughness, ao, diffuseIrradiance, prefilteredEnv, brdfLut);',
          FormatDot('  Lo += EvaluateIBLCookTorranceGGX(N, V, baseColor.rgb, metallic, roughness, ao, diffuseIrradiance, prefilteredEnv, brdfLut) * vec3(%.4f, %.4f, %.4f);',
            [FRHIEnvLightColorIntensity.X, FRHIEnvLightColorIntensity.Y, FRHIEnvLightColorIntensity.Z]),
          []);
    end;
    if HasLinearColorSpace and AHasEmissiveTex then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive * texture(castle_emissive_texture, castle_tex_coord0).rgb;',
        '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive * pow(max(texture(castle_emissive_texture, castle_tex_coord0).rgb, vec3(0.0)), vec3(2.2));',
        []);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  emissive *= texture(castle_emissive_texture, castle_tex_coord0).rgb;',
        '  emissive *= pow(max(texture(castle_emissive_texture, castle_tex_coord0).rgb, vec3(0.0)), vec3(2.2));',
        []);
    end;
    if DedupFragEffect <> '' then
    begin
      UpgradedFragEffect := Gen.UpgradeUserEffectStageSource(
        ssFragment, DedupFragEffect, FRHIUserEffectLayout);
      if HasPlugFunction(DedupFragEffect, 'PLUG_light_scale') then
        UpgradedFragEffect := 'uint castle_current_light_index = 0u;' + LineEnding + UpgradedFragEffect;
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'void main() {',
        UpgradedFragEffect + LineEnding + 'void main() {',
        []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_fragment_eye_space') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  vec3 V = normalize(-castle_vertex_eye.xyz);',
          '  vec3 V = normalize(-castle_vertex_eye.xyz);' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_fragment_eye_space', 'castle_vertex_eye, N', '  '),
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_light_scale') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '    vec3 radiance = light.Color.rgb * (attenuation * shadowVis);',
          '    castle_current_light_index = i;' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_light_scale', 'attenuation, N, L', '    ') + LineEnding +
          '    vec3 radiance = light.Color.rgb * (attenuation * shadowVis);',
          []);
      if (not FRHIIsPhongMaterial) and HasPlugFunction(DedupFragEffect, 'PLUG_material_light_ambient') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '    Lo += light.AmbientColor.rgb * baseColor.rgb * (ao * attenuation);',
          '    vec3 castle_light_ambient = baseColor.rgb;' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_light_ambient', 'castle_light_ambient', '    ') + LineEnding +
          '    Lo += light.AmbientColor.rgb * castle_light_ambient * (ao * attenuation);',
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_material_metallic_roughness') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  float ao = 1.0;',
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_metallic_roughness', 'metallic, roughness', '  ') + LineEnding +
          '  float ao = 1.0;',
          []);
      if (not FRHIIsPhongMaterial) and HasPlugFunction(DedupFragEffect, 'PLUG_material_shininess') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  float ao = 1.0;',
          '  float castle_shininess = castle_MaterialShininess;' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_shininess', 'castle_shininess', '  ') + LineEnding +
          '  roughness = clamp(1.0 - sqrt(clamp(castle_shininess / 128.0, 0.0, 1.0)), 0.04, 1.0);' + LineEnding +
          '  float ao = 1.0;',
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_add_light') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  vec3 emissive = castle_MaterialEmissive;',
          '  vec4 castle_add_light_color = vec4(Lo, baseColor.a);' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_add_light', 'castle_add_light_color, castle_vertex_eye, N', '  ') + LineEnding +
          '  Lo = castle_add_light_color.rgb;' + LineEnding +
          '  vec3 emissive = castle_MaterialEmissive;',
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_material_occlusion') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  vec3 emissive = castle_MaterialEmissive;',
          '  vec4 castle_occ_result = vec4(Lo, baseColor.a);' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_occlusion', 'castle_occ_result', '  ') + LineEnding +
          '  Lo = castle_occ_result.rgb;' + LineEnding +
          '  vec3 emissive = castle_MaterialEmissive;',
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_material_emissive') then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  castle_FragColor = vec4(Lo + emissive, baseColor.a);',
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_material_emissive', 'emissive', '  ') + LineEnding +
          '  castle_FragColor = vec4(Lo + emissive, baseColor.a);',
          []);
      if HasPlugFunction(DedupFragEffect, 'PLUG_texture_coord_shift') then
      begin
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'texture(castle_base_color_texture, castle_tex_coord0)',
          'texture(castle_base_color_texture, castle_shifted_uv)',
          [rfReplaceAll]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'texture(castle_normal_texture, castle_tex_coord0)',
          'texture(castle_normal_texture, castle_shifted_uv)',
          [rfReplaceAll]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'texture(castle_metallic_roughness_texture, castle_tex_coord0)',
          'texture(castle_metallic_roughness_texture, castle_shifted_uv)',
          [rfReplaceAll]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'texture(castle_emissive_texture, castle_tex_coord0)',
          'texture(castle_emissive_texture, castle_shifted_uv)',
          [rfReplaceAll]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'texture(castle_occlusion_texture, castle_tex_coord0)',
          'texture(castle_occlusion_texture, castle_shifted_uv)',
          [rfReplaceAll]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'void main() {' + LineEnding +
          '  vec2 castle_shifted_uv = castle_tex_coord0;' + LineEnding +
          BuildChainedPlugCalls(DedupFragEffect, 'PLUG_texture_coord_shift', 'castle_shifted_uv', '  '),
          []);
      end;
      if HasPlugFunction(DedupFragEffect, 'PLUG_base_color') then
      begin
        if AHasColorPerVertex then
        begin
          if (Arrays <> nil) and (Arrays.ColorMode = cmReplace) then
          begin
            if Arrays.ColorType = ctRgb then
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                '  vec4 baseColor = vec4(castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);',
                '  vec4 baseColor = vec4(castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);' + LineEnding +
                BuildChainedPlugCalls(DedupFragEffect, 'PLUG_base_color', 'baseColor', '  '),
                [])
            else
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                '  vec4 baseColor = castle_color_per_vertex;',
                '  vec4 baseColor = castle_color_per_vertex;' + LineEnding +
                BuildChainedPlugCalls(DedupFragEffect, 'PLUG_base_color', 'baseColor', '  '),
                []);
          end else
          if (Arrays <> nil) and (Arrays.ColorType = ctRgb) then
            FRHIFragmentShaderSource := StringReplace(
              FRHIFragmentShaderSource,
              '  vec4 baseColor = vec4(castle_MaterialBaseColor.rgb * castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);',
              '  vec4 baseColor = vec4(castle_MaterialBaseColor.rgb * castle_color_per_vertex.rgb, castle_MaterialBaseColor.a);' + LineEnding +
              BuildChainedPlugCalls(DedupFragEffect, 'PLUG_base_color', 'baseColor', '  '),
              [])
          else
            FRHIFragmentShaderSource := StringReplace(
              FRHIFragmentShaderSource,
              '  vec4 baseColor = castle_MaterialBaseColor * castle_color_per_vertex;',
              '  vec4 baseColor = castle_MaterialBaseColor * castle_color_per_vertex;' + LineEnding +
              BuildChainedPlugCalls(DedupFragEffect, 'PLUG_base_color', 'baseColor', '  '),
              []);
        end else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_MaterialBaseColor;' + LineEnding +
            BuildChainedPlugCalls(DedupFragEffect, 'PLUG_base_color', 'baseColor', '  '),
            []);
      end;
      if HasPlugFunction(DedupFragEffect, 'PLUG_main_texture_apply') then
      begin
        if AHasBaseColorTex then
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + LineEnding +
            BuildChainedPlugCalls(DedupFragEffect, 'PLUG_main_texture_apply', 'baseColor, castle_normal_eye', '  '),
            [])
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_MaterialBaseColor;' + LineEnding +
            BuildChainedPlugCalls(DedupFragEffect, 'PLUG_main_texture_apply', 'baseColor, castle_normal_eye', '  '),
            []);
      end;
      if HasPlugFunction(DedupFragEffect, 'PLUG_texture_apply') then
      begin
        if AHasBaseColorTex then
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + LineEnding +
            BuildChainedPlugCalls(DedupFragEffect, 'PLUG_texture_apply', 'baseColor, castle_normal_eye', '  '),
            [])
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_MaterialBaseColor;' + LineEnding +
            BuildChainedPlugCalls(DedupFragEffect, 'PLUG_texture_apply', 'baseColor, castle_normal_eye', '  '),
            []);
      end;
      if HasPlugFunction(DedupFragEffect, 'PLUG_texture_color') then
      begin
        PlugTexColorCall := '';
        PlugIdx := 1;
        while True do
        begin
          if PlugIdx = 1 then
            PlugCandidate := 'PLUG_texture_color'
          else
            PlugCandidate := Format('PLUG_texture_color_%d', [PlugIdx]);
          if not HasPlugFunction(DedupFragEffect, PlugCandidate) then
            Break;
          PlugPos := Pos(PlugCandidate, DedupFragEffect);
          CommaCount := 0;
          ParenPos := PlugPos + Length(PlugCandidate);
          while (ParenPos <= Length(DedupFragEffect)) and (DedupFragEffect[ParenPos] <> '(') do
            Inc(ParenPos);
          if (ParenPos <= Length(DedupFragEffect)) and (DedupFragEffect[ParenPos] = '(') then
          begin
            Inc(ParenPos);
            while (ParenPos <= Length(DedupFragEffect)) and (DedupFragEffect[ParenPos] <> ')') do
            begin
              if DedupFragEffect[ParenPos] = ',' then
                Inc(CommaCount);
              Inc(ParenPos);
            end;
          end;
          if CommaCount >= 2 then
          begin
            if AHasBaseColorTex then
              SingleTexColorCall := '  ' + PlugCandidate + '(baseColor, castle_base_color_texture, vec4(castle_tex_coord0, 0.0, 1.0));'
            else
              SingleTexColorCall := '  ' + PlugCandidate + '(baseColor, vec4(castle_tex_coord0, 0.0, 1.0));';
          end else if CommaCount = 1 then
            SingleTexColorCall := '  ' + PlugCandidate + '(baseColor, vec4(castle_tex_coord0, 0.0, 1.0));'
          else
            SingleTexColorCall := '  ' + PlugCandidate + '(baseColor);';
          if PlugTexColorCall <> '' then
            PlugTexColorCall := PlugTexColorCall + LineEnding + SingleTexColorCall
          else
            PlugTexColorCall := SingleTexColorCall;
          Inc(PlugIdx);
        end;
        if AHasBaseColorTex then
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
            '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + LineEnding +
            PlugTexColorCall,
            [])
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            '  vec4 baseColor = castle_MaterialBaseColor;',
            '  vec4 baseColor = castle_MaterialBaseColor;' + LineEnding +
            PlugTexColorCall,
            []);
      end;
    end;
    if HasLinearColorSpace and AHasBaseColorTex then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + LineEnding +
        '  baseColor.rgb = pow(max(baseColor.rgb, vec3(0.0)), vec3(2.2));',
        []);
    if HasLinearColorSpace and AHasEmissiveTex then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive * texture(castle_emissive_texture, castle_tex_coord0).rgb;',
        '    vec3 unlitColor = baseColor.rgb + castle_MaterialEmissive * pow(max(texture(castle_emissive_texture, castle_tex_coord0).rgb, vec3(0.0)), vec3(2.2));',
        []);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  emissive *= texture(castle_emissive_texture, castle_tex_coord0).rgb;',
        '  emissive *= pow(max(texture(castle_emissive_texture, castle_tex_coord0).rgb, vec3(0.0)), vec3(2.2));',
        []);
    end;
    if AHasTangents and AHasNormalTex then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  N = PerturbNormalCotangent(N, -castle_vertex_eye.xyz, castle_tex_coord0, tangentNormal, castle_MaterialNormalScale);',
        '  N = PerturbNormalTBN(castle_tangent_to_eye_space, tangentNormal, castle_MaterialNormalScale);',
        []);
    if FRHIHasParallaxBumpMapping and AHasTangents and AHasNormalTex then
    begin
      if FRHISteepParallaxBumpMapping then
        ParallaxSnippet := FormatDot(
          '  vec3 castle_view_ts = normalize(transpose(castle_tangent_to_eye_space) * (-castle_vertex_eye.xyz));' + LineEnding +
          '  float castle_num_layers = mix(32.0, 8.0, abs(castle_view_ts.z));' + LineEnding +
          '  float castle_layer_depth = 1.0 / castle_num_layers;' + LineEnding +
          '  float castle_cur_layer_depth = 0.0;' + LineEnding +
          '  vec2 castle_delta_uv = castle_view_ts.xy * %g / (max(castle_view_ts.z, 0.1) * castle_num_layers);' + LineEnding +
          '  vec2 castle_parallax_uv = castle_tex_coord0;' + LineEnding +
          '  float castle_cur_height = texture(castle_normal_texture, castle_parallax_uv).a;' + LineEnding +
          '  for (int castle_step = 0; castle_step < 32; ++castle_step) {' + LineEnding +
          '    if (castle_cur_layer_depth >= castle_cur_height) break;' + LineEnding +
          '    castle_parallax_uv -= castle_delta_uv;' + LineEnding +
          '    castle_cur_height = texture(castle_normal_texture, castle_parallax_uv).a;' + LineEnding +
          '    castle_cur_layer_depth += castle_layer_depth;' + LineEnding +
          '  }',
          [Max(0.0, FRHIParallaxHeight)])
      else
        ParallaxSnippet := FormatDot(
          '  vec3 castle_view_ts = normalize(transpose(castle_tangent_to_eye_space) * (-castle_vertex_eye.xyz));' + LineEnding +
          '  float castle_parallax_h = (texture(castle_normal_texture, castle_tex_coord0).a - 0.5) * %g;' + LineEnding +
          '  vec2 castle_parallax_uv = castle_tex_coord0 + castle_parallax_h * (castle_view_ts.xy / max(castle_view_ts.z, 0.1));',
          [Max(0.0, FRHIParallaxHeight)]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_base_color_texture, castle_tex_coord0)',
        'texture(castle_base_color_texture, castle_parallax_uv)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_normal_texture, castle_tex_coord0)',
        'texture(castle_normal_texture, castle_parallax_uv)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_metallic_roughness_texture, castle_tex_coord0)',
        'texture(castle_metallic_roughness_texture, castle_parallax_uv)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_emissive_texture, castle_tex_coord0)',
        'texture(castle_emissive_texture, castle_parallax_uv)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_occlusion_texture, castle_tex_coord0)',
        'texture(castle_occlusion_texture, castle_parallax_uv)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'void main() {',
        'void main() {' + LineEnding + ParallaxSnippet,
        []);
      if FRHISteepParallaxBumpMapping and FRHISteepParallaxSelfShadowing then
      begin
        SelfShadowSnippet := FormatDot(
          '    vec3 castle_light_ts = normalize(transpose(castle_tangent_to_eye_space) * L);' + LineEnding +
          '    if (castle_light_ts.z > 0.0) {' + LineEnding +
          '      float castle_self_steps = mix(30.0, 10.0, castle_light_ts.z);' + LineEnding +
          '      float castle_self_step = 1.0 / castle_self_steps;' + LineEnding +
          '      vec2 castle_self_delta = castle_light_ts.xy * %g / (max(castle_light_ts.z, 0.1) * castle_self_steps);' + LineEnding +
          '      float castle_self_cur_h = castle_cur_height + castle_self_step;' + LineEnding +
          '      vec2 castle_self_uv = castle_parallax_uv + castle_self_delta;' + LineEnding +
          '      float castle_self_map_h = texture(castle_normal_texture, castle_self_uv).a;' + LineEnding +
          '      for (int castle_sstep = 0; castle_sstep < 30; ++castle_sstep) {' + LineEnding +
          '        if (castle_self_map_h >= castle_self_cur_h || castle_self_cur_h >= 1.0) break;' + LineEnding +
          '        castle_self_cur_h += castle_self_step;' + LineEnding +
          '        castle_self_uv += castle_self_delta;' + LineEnding +
          '        castle_self_map_h = texture(castle_normal_texture, castle_self_uv).a;' + LineEnding +
          '      }' + LineEnding +
          '      if (castle_self_map_h >= castle_self_cur_h) shadowVis *= 0.5;' + LineEnding +
          '    }' + LineEnding +
          '    vec3 radiance = light.Color.rgb * (attenuation * shadowVis);',
          [Max(0.0, FRHIParallaxHeight)]);
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '    vec3 radiance = light.Color.rgb * (attenuation * shadowVis);',
          SelfShadowSnippet,
          []);
      end;
    end;
    if FRHIBaseColorTexCoordIndex > 0 then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_base_color_texture, castle_tex_coord0)',
        'texture(castle_base_color_texture, castle_tex_coord1)',
        [rfReplaceAll]);
    if FRHINormalTexCoordIndex > 0 then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_normal_texture, castle_tex_coord0)',
        'texture(castle_normal_texture, castle_tex_coord1)',
        [rfReplaceAll]);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'PerturbNormalCotangent(N, -castle_vertex_eye.xyz, castle_tex_coord0,',
        'PerturbNormalCotangent(N, -castle_vertex_eye.xyz, castle_tex_coord1,',
        [rfReplaceAll]);
    end;
    if FRHIMetallicRoughnessTexCoordIndex > 0 then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_metallic_roughness_texture, castle_tex_coord0)',
        'texture(castle_metallic_roughness_texture, castle_tex_coord1)',
        [rfReplaceAll]);
    if FRHIEmissiveTexCoordIndex > 0 then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_emissive_texture, castle_tex_coord0)',
        'texture(castle_emissive_texture, castle_tex_coord1)',
        [rfReplaceAll]);
    if FRHIOcclusionTexCoordIndex > 0 then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'texture(castle_occlusion_texture, castle_tex_coord0)',
        'texture(castle_occlusion_texture, castle_tex_coord1)',
        [rfReplaceAll]);
    if AHasBaseColorTex and FRHIHasSecondTexture and
       (0 >= Length(FRHIUserEffectLayout.Samplers)) then
    begin
      if (Arrays = nil) or (Arrays.TexCoords.Count >= 2) or FRHIHasTextureTransform1 then
        SecondTexUV := 'castle_tex_coord1'
      else
        SecondTexUV := 'castle_tex_coord0';
      case FRHIMultiTextureStageDimension[1] of
        tdCube:
          begin
            FRHIFragmentShaderSource := StringReplace(
              FRHIFragmentShaderSource,
              'void main() {',
              'CASTLE_DESCRIPTOR_SET(2, 2) uniform samplerCube castle_multi_texture_1;' + LineEnding +
              'void main() {',
              []);
            if (Arrays <> nil) and (Arrays.TexCoords.Count > 1) and (Arrays.TexCoords[1] <> nil) then
              case Arrays.TexCoords[1].Generation of
                tgCameraSpaceNormal:
                  SecondTexUV := 'normalize(castle_normal_eye)';
                tgWorldSpaceNormal:
                  SecondTexUV := '(mat3(castle_ViewMatrixInverse) * normalize(castle_normal_eye))';
                tgCameraSpaceReflectionVector:
                  SecondTexUV := '(-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye)))';
                else
                  SecondTexUV := '(mat3(castle_ViewMatrixInverse) * (-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye))))';
              end
            else
              SecondTexUV := '(mat3(castle_ViewMatrixInverse) * (-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye))))';
          end;
        td3D:
          begin
            FRHIFragmentShaderSource := StringReplace(
              FRHIFragmentShaderSource,
              'void main() {',
              'CASTLE_DESCRIPTOR_SET(2, 2) uniform sampler3D castle_multi_texture_1;' + LineEnding +
              'void main() {',
              []);
            SecondTexUV := '((castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0)).xyz * 0.5 + vec3(0.5))';
          end;
        else
          FRHIFragmentShaderSource := StringReplace(
            FRHIFragmentShaderSource,
            'void main() {',
            'CASTLE_DESCRIPTOR_SET(2, 2) uniform sampler2D castle_multi_texture_1;' + LineEnding +
            'void main() {',
            []);
      end;
      SecondTexSnippet := BuildMultiTexStageSnippet(
        1, FRHISecondTextureEnv, FRHISecondTextureCombineRGB, FRHISecondTextureCombineAlpha,
        FRHISecondTextureConstantColor, SecondTexUV, HasLinearColorSpace);
      if FRHIHasMultiTextureStage[2] and (1 >= Length(FRHIUserEffectLayout.Samplers)) then
      begin
        case FRHIMultiTextureStageDimension[2] of
          tdCube:
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 3) uniform samplerCube castle_multi_texture_2;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  2, FRHIMultiTextureStageEnv[2],
                  FRHIMultiTextureStageEnv[2].Combine[cRGB], FRHIMultiTextureStageEnv[2].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[2],
                  '(mat3(castle_ViewMatrixInverse) * (-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye))))',
                  HasLinearColorSpace);
            end;
          td3D:
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 3) uniform sampler3D castle_multi_texture_2;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  2, FRHIMultiTextureStageEnv[2],
                  FRHIMultiTextureStageEnv[2].Combine[cRGB], FRHIMultiTextureStageEnv[2].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[2],
                  '((castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0)).xyz * 0.5 + vec3(0.5))',
                  HasLinearColorSpace);
            end;
          else
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 3) uniform sampler2D castle_multi_texture_2;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  2, FRHIMultiTextureStageEnv[2],
                  FRHIMultiTextureStageEnv[2].Combine[cRGB], FRHIMultiTextureStageEnv[2].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[2], 'castle_tex_coord0', HasLinearColorSpace);
            end;
        end;
      end;
      if FRHIHasMultiTextureStage[3] and (2 >= Length(FRHIUserEffectLayout.Samplers)) then
      begin
        case FRHIMultiTextureStageDimension[3] of
          tdCube:
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 4) uniform samplerCube castle_multi_texture_3;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  3, FRHIMultiTextureStageEnv[3],
                  FRHIMultiTextureStageEnv[3].Combine[cRGB], FRHIMultiTextureStageEnv[3].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[3],
                  '(mat3(castle_ViewMatrixInverse) * (-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye))))',
                  HasLinearColorSpace);
            end;
          td3D:
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 4) uniform sampler3D castle_multi_texture_3;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  3, FRHIMultiTextureStageEnv[3],
                  FRHIMultiTextureStageEnv[3].Combine[cRGB], FRHIMultiTextureStageEnv[3].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[3],
                  '((castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0)).xyz * 0.5 + vec3(0.5))',
                  HasLinearColorSpace);
            end;
          else
            begin
              FRHIFragmentShaderSource := StringReplace(
                FRHIFragmentShaderSource,
                'void main() {',
                'CASTLE_DESCRIPTOR_SET(2, 4) uniform sampler2D castle_multi_texture_3;' + LineEnding +
                'void main() {',
                []);
              SecondTexSnippet := SecondTexSnippet + LineEnding +
                BuildMultiTexStageSnippet(
                  3, FRHIMultiTextureStageEnv[3],
                  FRHIMultiTextureStageEnv[3].Combine[cRGB], FRHIMultiTextureStageEnv[3].Combine[cAlpha],
                  FRHIMultiTextureStageConstantColor[3], 'castle_tex_coord0', HasLinearColorSpace);
            end;
        end;
      end;
      if Pos('  baseColor *= texture(castle_base_color_texture, castle_tex_coord1);', FRHIFragmentShaderSource) > 0 then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  baseColor *= texture(castle_base_color_texture, castle_tex_coord1);',
          '  baseColor *= texture(castle_base_color_texture, castle_tex_coord1);' + LineEnding + SecondTexSnippet,
          [])
      else
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
          '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);' + LineEnding + SecondTexSnippet,
          []);
    end;
    if FRHIHasAlphaTexture or FRHIInvertAlphaTexture then
    begin
      if FRHIHasAlphaTexture then
      begin
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          'void main() {',
          'CASTLE_SAMPLER(2, 3) uniform sampler2D castle_alpha_texture;' + LineEnding +
          'void main() {',
          []);
        if FRHIInvertAlphaTexture then
          AlphaTexSnippet := '  baseColor.a *= (1.0 - texture(castle_alpha_texture, castle_tex_coord0).a);'
        else
          AlphaTexSnippet := '  baseColor.a *= texture(castle_alpha_texture, castle_tex_coord0).a;';
      end else
        AlphaTexSnippet := '  baseColor.a = castle_MaterialBaseColor.a * (1.0 - clamp(baseColor.a / max(castle_MaterialBaseColor.a, 1e-5), 0.0, 1.0));';
      if AUseAlphaTest then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  if (baseColor.a < castle_AlphaCutoff) discard;',
          AlphaTexSnippet + LineEnding + '  if (baseColor.a < castle_AlphaCutoff) discard;',
          [])
      else
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '  /* Unlit Material Early Out (SpecularColor.a < 0.0) */',
          AlphaTexSnippet + LineEnding + '  /* Unlit Material Early Out (SpecularColor.a < 0.0) */',
          []);
    end;
    if (TexCoord0Dim in [3, 4]) and (not AHasTexCoordGen) and (FRHITexCoordGen = tgExplicit) then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'void main() {',
        'CASTLE_LOCATION(9) in vec4 castle_tex_coord0_4d;' + LineEnding + 'void main() {',
        []);
    if AHasBaseColorTex and (FRHIBaseColorTextureDimension = tdCube) then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'CASTLE_SAMPLER(1, 1) uniform sampler2D castle_base_color_texture;',
        'CASTLE_SAMPLER(1, 1) uniform samplerCube castle_base_color_texture;',
        []);
      if (TexCoord0Dim in [3, 4]) and (not AHasTexCoordGen) and (FRHITexCoordGen = tgExplicit) then
        CubeCoordExpr := 'castle_tex_coord0_4d.xyz / max(castle_tex_coord0_4d.w, 1e-5)'
      else
      case FRHITexCoordGen of
        tgCameraSpaceNormal:
          CubeCoordExpr := 'normalize(castle_normal_eye)';
        tgWorldSpaceNormal:
          CubeCoordExpr := 'mat3(castle_ViewMatrixInverse) * normalize(castle_normal_eye)';
        tgCameraSpaceReflectionVector:
          CubeCoordExpr := '-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye))';
        else
          CubeCoordExpr := '((castle_MaterialShininess < 0.0) ? normalize(mat3(castle_ViewMatrixInverse) * normalize(castle_vertex_eye.xyz)) : (mat3(castle_ViewMatrixInverse) * (-reflect(-normalize(castle_vertex_eye.xyz), normalize(castle_normal_eye)))))';
      end;
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
        '  vec3 castle_cube_coord = ' + CubeCoordExpr + ';' + LineEnding +
        '  baseColor *= texture(castle_base_color_texture, castle_cube_coord);',
        []);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord1);',
        '  vec3 castle_cube_coord = ' + CubeCoordExpr + ';' + LineEnding +
        '  baseColor *= texture(castle_base_color_texture, castle_cube_coord);',
        []);
    end else
    if AHasBaseColorTex and (FRHIBaseColorTextureDimension = td3D) then
    begin
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        'CASTLE_SAMPLER(1, 1) uniform sampler2D castle_base_color_texture;',
        'CASTLE_SAMPLER(1, 1) uniform sampler3D castle_base_color_texture;',
        []);
      if (TexCoord0Dim in [3, 4]) and (not AHasTexCoordGen) and (FRHITexCoordGen = tgExplicit) then
        Tex3DCoordExpr := 'castle_tex_coord0_4d.xyz / max(castle_tex_coord0_4d.w, 1e-5)'
      else
      begin
        case FRHITexCoordGen of
          tgBounds3d:
            Tex3DCoordExpr := FormatDot(
              'vec3(castle_tex_coord0, dot(castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0), vec4(%g, %g, %g, %g)))',
              [FRHITexGenBoundsR.X, FRHITexGenBoundsR.Y, FRHITexGenBoundsR.Z, FRHITexGenBoundsR.W]);
          tgCoordEye:
            Tex3DCoordExpr := 'castle_vertex_eye.xyz';
          else
            Tex3DCoordExpr := '((castle_ViewMatrixInverse * vec4(castle_vertex_eye.xyz, 1.0)).xyz * 0.5 + vec3(0.5))';
        end;
        if AHasTextureTransform then
        begin
          Tex3DMatExpr := FormatDot(
            'mat4(vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g), vec4(%g, %g, %g, %g))',
            [FRHITextureTransform.Data[0, 0], FRHITextureTransform.Data[0, 1], FRHITextureTransform.Data[0, 2], FRHITextureTransform.Data[0, 3],
             FRHITextureTransform.Data[1, 0], FRHITextureTransform.Data[1, 1], FRHITextureTransform.Data[1, 2], FRHITextureTransform.Data[1, 3],
             FRHITextureTransform.Data[2, 0], FRHITextureTransform.Data[2, 1], FRHITextureTransform.Data[2, 2], FRHITextureTransform.Data[2, 3],
             FRHITextureTransform.Data[3, 0], FRHITextureTransform.Data[3, 1], FRHITextureTransform.Data[3, 2], FRHITextureTransform.Data[3, 3]]);
          Tex3DCoordExpr := '(' + Tex3DMatExpr + ' * vec4(' + Tex3DCoordExpr + ', 1.0)).xyz';
        end;
      end;
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord0);',
        '  vec3 castle_tex3d_coord = ' + Tex3DCoordExpr + ';' + LineEnding +
        '  baseColor *= texture(castle_base_color_texture, castle_tex3d_coord);',
        []);
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  baseColor *= texture(castle_base_color_texture, castle_tex_coord1);',
        '  vec3 castle_tex3d_coord = ' + Tex3DCoordExpr + ';' + LineEnding +
        '  baseColor *= texture(castle_base_color_texture, castle_tex3d_coord);',
        []);
    end;
    if (Arrays <> nil) and (not Arrays.CullFace) then
      FRHIFragmentShaderSource := StringReplace(
        FRHIFragmentShaderSource,
        '  vec3 N = normalize(castle_normal_eye);',
        '  vec3 N = normalize(castle_normal_eye) * (gl_FrontFacing ? 1.0 : -1.0);',
        []);
    if FRHIComposedVertSource <> '' then
      FRHIVertexShaderSource := Gen.FinalizeVertexShaderSource(
        Gen.GeneratePreambleFor3DVertex(
          AHasSkinning, AHasTangents, NeedTexCoord1Attr, AHasColorPerVertex) +
        Gen.UpgradeUserEffectStageSource(
          ssVertex, FRHIComposedVertSource, FRHIUserEffectLayout));
    if (Arrays <> nil) and (Arrays.Attribs.Count > 0) then
    begin
      LocIdx := 9;
      for I := 0 to Arrays.Attribs.Count - 1 do
      begin
        AttrLoc := LocIdx;
        case Arrays.Attribs[I].AType of
          atFloat:   begin AttribGLSLType := 'float'; Inc(LocIdx, 1); end;
          atVector2: begin AttribGLSLType := 'vec2';  Inc(LocIdx, 1); end;
          atVector3: begin AttribGLSLType := 'vec3';  Inc(LocIdx, 1); end;
          atVector4: begin AttribGLSLType := 'vec4';  Inc(LocIdx, 1); end;
          atMatrix3: begin AttribGLSLType := 'mat3';  Inc(LocIdx, 3); end;
          atMatrix4: begin AttribGLSLType := 'mat4';  Inc(LocIdx, 4); end;
          else       begin AttribGLSLType := 'vec4';  Inc(LocIdx, 1); end;
        end;
        AttribDecl := Format('CASTLE_LOCATION(%d) in %s %s;', [AttrLoc, AttribGLSLType, Arrays.Attribs[I].Name]);
        if Pos('in ' + AttribGLSLType + ' ' + Arrays.Attribs[I].Name + ';', FRHIVertexShaderSource) > 0 then
          FRHIVertexShaderSource := StringReplace(
            FRHIVertexShaderSource,
            'in ' + AttribGLSLType + ' ' + Arrays.Attribs[I].Name + ';',
            AttribDecl,
            [rfReplaceAll])
        else if Pos('attribute ' + AttribGLSLType + ' ' + Arrays.Attribs[I].Name + ';', FRHIVertexShaderSource) > 0 then
          FRHIVertexShaderSource := StringReplace(
            FRHIVertexShaderSource,
            'attribute ' + AttribGLSLType + ' ' + Arrays.Attribs[I].Name + ';',
            AttribDecl,
            [rfReplaceAll])
        else
          FRHIVertexShaderSource := StringReplace(
            FRHIVertexShaderSource,
            'void main',
            AttribDecl + LineEnding + 'void main',
            []);
      end;
    end;
    if FRHIComposedFragSource <> '' then
    begin
      FRHIFragmentShaderSource :=
        Gen.GeneratePreamble(ssFragment) +
        Gen.UpgradeUserEffectStageSource(
          ssFragment, FRHIComposedFragSource, FRHIUserEffectLayout);
      if HasLinearColorSpace then
        FRHIFragmentShaderSource := StringReplace(
          FRHIFragmentShaderSource,
          '#define CASTLE_VULKAN 1',
          '#define CASTLE_VULKAN 1' + LineEnding + '#define CASTLE_GAMMA_CORRECTION 1',
          []);
    end;
    if FRHIComposedGeomSource <> '' then
      FRHIGeometryShaderSource := Gen.FinalizeGeometryShaderSource(
        Gen.GeneratePreamble(ssGeometry) +
        Gen.UpgradeUserEffectStageSource(
          ssGeometry, FRHIComposedGeomSource, FRHIUserEffectLayout))
    else if DedupGeomEffect <> '' then
    begin
      PlugGeomSetCall := BuildChainedPlugCalls(DedupGeomEffect, 'PLUG_geometry_vertex_set', 'index', '  ');
      PlugGeomZeroCall := BuildChainedPlugCalls(DedupGeomEffect, 'PLUG_geometry_vertex_zero', '', '  ');
      PlugGeomAddCall := BuildChainedPlugCalls(DedupGeomEffect, 'PLUG_geometry_vertex_add', 'index, scale', '  ');
      FRHIGeometryShaderSource := Gen.FinalizeGeometryShaderSource(
        Gen.GeneratePreamble(ssGeometry) +
        Gen.UpgradeUserEffectStageSource(
          ssGeometry, DedupGeomEffect, FRHIUserEffectLayout),
        PlugGeomSetCall, PlugGeomZeroCall, PlugGeomAddCall);
    end;
    FRHIVertexShader := RHIDevice.CreateShaderModuleFromGLSL(
      ssVertex, FRHIVertexShaderSource, Format('CastleShapeVert_Perm%.2x', [QWord(Mask)]));
    FRHIFragmentShader := RHIDevice.CreateShaderModuleFromGLSL(
      ssFragment, FRHIFragmentShaderSource, Format('CastleShapeFrag_Perm%.2x', [QWord(Mask)]));
    if FRHIGeometryShaderSource <> '' then
      FRHIGeometryShader := RHIDevice.CreateShaderModuleFromGLSL(
        ssGeometry, FRHIGeometryShaderSource, Format('CastleShapeGeom_Perm%.2x', [QWord(Mask)]));
    FRHIShaderPermutationMask := Mask;
    FRHIShadersInitialized := True;
    Inc(FRHIShaderCompileCount);
  finally
    FreeAndNil(Gen);
  end;
end;

procedure TShapeCache.LoadArraysToVbo(const DynamicGeometry: boolean);
var
  DataUsage: TGLenum;
  NewVbos: boolean;
  RHIBufDesc: TRHIBufferDesc;
  RHIMemUsage: TRHIMemoryUsage;

  { Bind Vbo buffer and load data. Updates AllocatedSize.

    Uses glBufferSubData if possible, as it may be faster than glBufferData
    (not confirmed by tests, but OpenGL docs suggest it:
    https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBufferSubData.xhtml
    "When replacing the entire data store, consider using glBufferSubData
    rather than completely recreating the data store with glBufferData.
    This avoids the cost of reallocating the data store. " ). }
  procedure BufferData(const VboType: TVboType;
    const Target: TBufferTarget; const Size: Cardinal; const Data: Pointer);
  var
    TargetGL: TGLenum;
  begin
    { It may happen that Data = nil, because Arrays.AttributeSize may be 0
      (some shapes just don't need extra per-vertex attributes).

      We should not proceed then.
      Passing Data = nil to glBufferSubData can make actual bugs:
      see the testcase from
      https://github.com/castle-engine/castle-engine/issues/389 :
      open dragon.json, turn on "Dynamic Batching", run some animation
      -- it will update VBOs each frame, and on at least some systems
      (confirmed on 3 systems with NVidia on Windows) it will cause
      weird problems (like unrelated buffers are trashed with garbage).
      Strangely glBufferData doesn't have a problem with Data = nil,
      but we also avoid it. }
    if Data = nil then
    begin
      Assert(Size = 0);
      Exit;
    end;

    if NewVbos or
       (VboType in VboToReload) or
       { In normal circumstances, when vbo is already loaded,
         it should have already the appropriate size. But through VRML/X3D
         events, user may be able to actually incorrectly change
         coordinates, such that new ones have different size than the old ones
         --- in this case, VboToReload optimization fails, and we have
         to reload data (or we'll get terrible OpenGL segfaults later,
         as it tries to access non-existent data from vertex arrays). }
       (VboAllocatedSize[VboType] <> Size) then
    begin
      RenderContext.BindBuffer[Target] := Vbo[VboType];
      TargetGL := BufferTargetGL[Target];
      if NewVbos or
        (VboAllocatedUsage <> DataUsage) or
        (VboAllocatedSize[VboType] <> Size) then
      begin
        glBufferData(TargetGL, Size, Data, DataUsage);
        VboAllocatedSize[VboType] := Size;
      end else
        glBufferSubData(TargetGL, 0, Size, Data);
    end;
  end;

  function VboTypesToStr(const VboTypes: TVboTypes): string;
  const
    Names: array [TVboType] of string =
    ( 'Coordinate', 'Attribute', 'Index' );
  var
    I: TVboType;
  begin
    Result := '';
    for I := Low(I) to High(I) do
      if I in VboTypes then
      begin
        if Result <> '' then Result := Result + ',';
        Result := Result + Names[I];
      end;
    Result := '[' + Result + ']';
  end;

  procedure CreateVbo;
  var
    VboType: TVboType;
  begin
    for VboType := Low(VboType) to High(VboType) do
      Vbo[VboType] := glCreateBuffer();
  end;

begin
  if DynamicGeometry then
    DataUsage := GL_STREAM_DRAW
  else
    DataUsage := GL_STATIC_DRAW;

  if (GLFeatures <> nil) and GLFeatures.VertexBufferObject then
  begin
    { In case of VBO not supported (ancient GPUs), we do not free data in Arrays,
      it will be used to provide data at each draw call. }
    Assert(not Arrays.DataFreed);

    NewVbos := Vbo[vtCoordinate] = GLObjectNone;
    if NewVbos then
    begin
      CreateVbo;
      if LogRenderer then
        WritelnLog('Renderer', Format('Creating and loading data to VBOs (%d,%d,%d)',
          [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex]]));
    end else
    begin
      if LogRenderer then
        WritelnLog('Renderer', Format('Loading data to existing VBOs (%d,%d,%d), reloading %s',
          [Vbo[vtCoordinate], Vbo[vtAttribute], Vbo[vtIndex],
           VboTypesToStr(VboToReload)]));
    end;

    VboCoordinatePreserveGeometryOrder := Arrays.CoordinatePreserveGeometryOrder;
    EnsureRHIDeviceMatch;

    BufferData(vtCoordinate, btArray,
      Arrays.Count * Arrays.CoordinateSize, Arrays.CoordinateArray);

    BufferData(vtAttribute, btArray,
      Arrays.Count * Arrays.AttributeSize, Arrays.AttributeArray);

    if Arrays.Indexes <> nil then
      BufferData(vtIndex, btElementArray,
        Arrays.Indexes.Count * SizeOf(TGeometryIndex), Arrays.Indexes.L);

    VboAllocatedUsage := DataUsage;

    if GLFeatures.VertexArrayObject and (Vao = nil) then
      Vao := TVertexArrayObject.Create(nil);
  end;

  if RHIDevice <> nil then
  begin
    Assert(not Arrays.DataFreed);
    VboCoordinatePreserveGeometryOrder := Arrays.CoordinatePreserveGeometryOrder;
    EnsureRHIDeviceMatch;
    VboAllocatedUsage := DataUsage;
    VboAllocatedSize[vtCoordinate] := Arrays.Count * Arrays.CoordinateSize;
    VboAllocatedSize[vtAttribute] := Arrays.Count * Arrays.AttributeSize;
    if Arrays.Indexes <> nil then
      VboAllocatedSize[vtIndex] := Arrays.Indexes.Count * SizeOf(TGeometryIndex)
    else
      VboAllocatedSize[vtIndex] := 0;

    if DynamicGeometry then
      RHIMemUsage := muCpuToGpu
    else
      RHIMemUsage := muGpuOnly;

    if Arrays.Count > 0 then
    begin
      if (FRHICoordBuffer <> nil) and (FRHICoordBuffer.Desc.Size = Arrays.Count * Arrays.CoordinateSize) then
        FRHICoordBuffer.UploadData(Arrays.CoordinateArray^, Arrays.Count * Arrays.CoordinateSize)
      else
      begin
        FreeAndNil(FRHICoordBuffer);
        RHIBufDesc := TRHIBufferDesc.Create(Arrays.Count * Arrays.CoordinateSize, [buVertex, buTransferDst], RHIMemUsage);
        FRHICoordBuffer := RHIDevice.CreateBuffer(RHIBufDesc, Arrays.CoordinateArray);
      end;

      if Arrays.AttributeSize > 0 then
      begin
        if (FRHIAttribBuffer <> nil) and (FRHIAttribBuffer.Desc.Size = Arrays.Count * Arrays.AttributeSize) then
          FRHIAttribBuffer.UploadData(Arrays.AttributeArray^, Arrays.Count * Arrays.AttributeSize)
        else
        begin
          FreeAndNil(FRHIAttribBuffer);
          RHIBufDesc := TRHIBufferDesc.Create(Arrays.Count * Arrays.AttributeSize, [buVertex, buTransferDst], RHIMemUsage);
          FRHIAttribBuffer := RHIDevice.CreateBuffer(RHIBufDesc, Arrays.AttributeArray);
        end;
      end else
        FreeAndNil(FRHIAttribBuffer);

      if Arrays.Indexes <> nil then
      begin
        if (FRHIIndexBuffer <> nil) and (FRHIIndexBuffer.Desc.Size = Arrays.Indexes.Count * SizeOf(TGeometryIndex)) then
          FRHIIndexBuffer.UploadData(Arrays.Indexes.L^, Arrays.Indexes.Count * SizeOf(TGeometryIndex))
        else
        begin
          FreeAndNil(FRHIIndexBuffer);
          RHIBufDesc := TRHIBufferDesc.Create(Arrays.Indexes.Count * SizeOf(TGeometryIndex), [buIndex, buTransferDst], RHIMemUsage);
          FRHIIndexBuffer := RHIDevice.CreateBuffer(RHIBufDesc, Arrays.Indexes.L);
        end;
      end else
        FreeAndNil(FRHIIndexBuffer);

      Inc(FRHIUploadCount);
    end else
    begin
      FreeAndNil(FRHICoordBuffer);
      FreeAndNil(FRHIAttribBuffer);
      FreeAndNil(FRHIIndexBuffer);
    end;
  end;

  if ((GLFeatures <> nil) and GLFeatures.VertexBufferObject) or (RHIDevice <> nil) then
    Arrays.FreeData;

  VboToReload := [];
end;

function TShapeCache.ToString: String;
var
  ShapeNodeName: String;
begin
  if State.ShapeNode <> nil then
    ShapeNodeName := ' ' + State.ShapeNode.X3DName
  else
    ShapeNodeName := '';
  Result := Format('Shape%s %s (%s): %d', [
    ShapeNodeName,
    PointerToStr(Self),
    Geometry.X3DType,
    References
  ]);
end;

{ TShaderProgramCache -------------------------------------------------------- }

destructor TShaderProgramCache.Destroy;
begin
  FreeAndNil(ShaderProgram);
  inherited;
end;

{$endif}
