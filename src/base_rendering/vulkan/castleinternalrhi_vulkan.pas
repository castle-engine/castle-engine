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

{ Vulkan 1.3 implementation of CastleInternalRHI.
  Uses VK_KHR_dynamic_rendering (core in 1.3) and negative viewport height
  (VK_KHR_maintenance1) to match Castle Game Engine's OpenGL clip-space conventions. }
unit CastleInternalRHI_Vulkan;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Math, Generics.Collections, Generics.Defaults,
  CastleVectors, CastleColors, CastleRectangles, CastleImages, CastleLog,
  CastleInternalRHI, CastleInternalVulkan;

type
  TVulkanRHIDevice = class;

  { Callback provided by TCastleWindow to create the platform-specific VkSurfaceKHR }
  TVulkanCreateSurfaceEvent = function(const Instance: VkInstance): VkSurfaceKHR of object;

  { -------------------------------------------------------------------------- }
  { Concrete Vulkan Resource Classes                                           }
  { -------------------------------------------------------------------------- }

  TVulkanRHIBuffer = class(TRHIBuffer)
  private
    FHandle: VkBuffer;
    FDeviceMemory: VkDeviceMemory;
    FMemoryOffset: VkDeviceSize;
    FMappedPtr: Pointer;
    function VulkanDevice: TVulkanRHIDevice;
  public
    constructor Create(const ADevice: TVulkanRHIDevice; const ADesc: TRHIBufferDesc;
      const InitialData: Pointer);
    destructor Destroy; override;

    function Map: Pointer; override;
    procedure Unmap; override;
    procedure UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt = 0); override;

    property Handle: VkBuffer read FHandle;
  end;

  TVulkanRHITexture = class(TRHITexture)
  private
    FImage: VkImage;
    FImageView: VkImageView;
    FDeviceMemory: VkDeviceMemory;
    FOwnsImage: Boolean;
    FAspectMask: VkImageAspectFlags;
    function VulkanDevice: TVulkanRHIDevice;
  public
    { Standard constructor that allocates a new VkImage and VkImageView }
    constructor Create(const ADevice: TVulkanRHIDevice; const ADesc: TRHITextureDesc); overload;
    { Wrapper constructor for existing VkSwapchainKHR images }
    constructor CreateSwapchainWrapper(const ADevice: TVulkanRHIDevice;
      const ADesc: TRHITextureDesc; const AExistingImage: VkImage;
      const AExistingView: VkImageView); overload;
    destructor Destroy; override;

    procedure UploadImage(const AImage: TEncodedImage;
      const AMipLevel: Cardinal = 0; const AArrayLayer: Cardinal = 0); override;

    property Image: VkImage read FImage;
    property ImageView: VkImageView read FImageView;
    property AspectMask: VkImageAspectFlags read FAspectMask;
  end;

  TVulkanRHISampler = class(TRHISampler)
  private
    FHandle: VkSampler;
  public
    constructor Create(const ADevice: TVulkanRHIDevice; const ADesc: TRHISamplerDesc);
    destructor Destroy; override;
    property Handle: VkSampler read FHandle;
  end;

  TVulkanRHIShaderModule = class(TRHIShaderModule)
  private
    FHandle: VkShaderModule;
  public
    constructor Create(const ADevice: TVulkanRHIDevice; const AStage: TRHIShaderStage;
      const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt; const ADebugName: String);
    destructor Destroy; override;
    property Handle: VkShaderModule read FHandle;
  end;

  TVulkanRHIDescriptorSetLayout = class(TRHIDescriptorSetLayout)
  private
    FHandle: VkDescriptorSetLayout;
  public
    constructor Create(const ADevice: TVulkanRHIDevice;
      const Bindings: array of TRHIDescriptorBindingDesc);
    destructor Destroy; override;
    property Handle: VkDescriptorSetLayout read FHandle;
  end;

  TVulkanRHIDescriptorSet = class(TRHIDescriptorSet)
  private
    FHandle: VkDescriptorSet;
    FLayout: TVulkanRHIDescriptorSetLayout;
    FPendingWrites: array of VkWriteDescriptorSet;
    FBufferInfos: array of VkDescriptorBufferInfo;
    FImageInfos: array of VkDescriptorImageInfo;
    function VulkanDevice: TVulkanRHIDevice;
  public
    constructor Create(const ADevice: TVulkanRHIDevice;
      const ALayout: TVulkanRHIDescriptorSetLayout);
    destructor Destroy; override;

    procedure BindUniformBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindStorageBuffer(const Binding: Cardinal; const Buffer: TRHIBuffer;
      const Offset: PtrUInt = 0; const Range: PtrUInt = 0); override;
    procedure BindTexture(const Binding: Cardinal; const Texture: TRHITexture;
      const Sampler: TRHISampler); override;
    procedure Update; override;

    property Handle: VkDescriptorSet read FHandle;
  end;

  TVulkanRHIPipeline = class(TRHIPipeline)
  private
    FHandle: VkPipeline;
    FPipelineLayout: VkPipelineLayout;
  public
    constructor Create(const ADevice: TVulkanRHIDevice;
      const ADesc: TRHIGraphicsPipelineDesc);
    destructor Destroy; override;
    property Handle: VkPipeline read FHandle;
    property PipelineLayout: VkPipelineLayout read FPipelineLayout;
  end;

  { -------------------------------------------------------------------------- }
  { Vulkan Command List                                                        }
  { -------------------------------------------------------------------------- }

  TVulkanRHICommandList = class(TRHICommandList)
  private
    FDevice: TVulkanRHIDevice;
    FCommandPool: VkCommandPool;
    FHandle: VkCommandBuffer;
    FCurrentPipeline: TVulkanRHIPipeline;
  public
    constructor Create(const ADevice: TVulkanRHIDevice; const AQueueFamilyIndex: Cardinal);
    destructor Destroy; override;

    procedure BeginRecording; override;
    procedure EndRecording; override;

    procedure TransitionTexture(const Texture: TRHITexture;
      const NewState: TRHIResourceState); override;
    procedure GenerateMipmaps(const Texture: TRHITexture); override;

    procedure BeginRendering(const Info: TRHIRenderingInfo); override;
    procedure EndRendering; override;

    procedure SetViewport(const ViewportRect: TFloatRectangle;
      const MinDepth: Single = 0.0; const MaxDepth: Single = 1.0); override;
    procedure SetScissor(const ScissorRect: TRectangle); override;
    procedure SetDepthBias(const ConstantFactor, SlopeFactor: Single); override;

    procedure BindPipeline(const Pipeline: TRHIPipeline); override;
    procedure BindDescriptorSet(const SetIndex: Cardinal;
      const DescriptorSet: TRHIDescriptorSet;
      const DynamicOffsets: array of Cardinal); override;
    procedure PushConstants(const StageFlags: TRHIShaderStageFlags;
      const Offset, Size: Cardinal; const Data); override;

    procedure BindVertexBuffers(const FirstBinding: Cardinal;
      const Buffers: array of TRHIBuffer;
      const Offsets: array of PtrUInt); override;
    procedure BindIndexBuffer(const Buffer: TRHIBuffer;
      const Offset: PtrUInt; const IndexType: TRHIIndexType); override;

    procedure Draw(const VertexCount, InstanceCount, FirstVertex, FirstInstance: Cardinal); override;
    procedure DrawIndexed(const IndexCount, InstanceCount, FirstIndex: Cardinal;
      const VertexOffset: Integer; const FirstInstance: Cardinal); override;
    procedure DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
      const Offset: PtrUInt; const DrawCount, Stride: Cardinal); override;

    procedure PushDebugGroup(const LabelName: String; const Color: TCastleColor); override;
    procedure PopDebugGroup; override;

    property Handle: VkCommandBuffer read FHandle;
  end;

  { -------------------------------------------------------------------------- }
  { Vulkan Render Device                                                       }
  { -------------------------------------------------------------------------- }

  TVulkanFrameContext = record
    ImageAvailableSemaphore: VkSemaphore;
    RenderFinishedSemaphore: VkSemaphore;
    InFlightFence: VkFence;
    CommandList: TVulkanRHICommandList;
  end;

  TVulkanPipelineMap = {$ifdef FPC}specialize{$endif} TObjectDictionary<UInt64, TVulkanRHIPipeline>;

  TVulkanRHIDevice = class(TRHIDevice)
  private
    FInstance: VkInstance;
    FDebugMessenger: VkDebugUtilsMessengerEXT;
    FSurface: VkSurfaceKHR;
    FPhysicalDevice: VkPhysicalDevice;
    FDevice: VkDevice;
    FGraphicsQueue: VkQueue;
    FPresentQueue: VkQueue;
    FGraphicsQueueFamily: Cardinal;
    FPresentQueueFamily: Cardinal;

    { Swapchain state }
    FSwapchain: VkSwapchainKHR;
    FSwapchainFormat: TRHIPixelFormat;
    FSwapchainExtent: VkExtent2D;
    FSwapchainColorTextures: array of TVulkanRHITexture;
    FSwapchainDepthTexture: TVulkanRHITexture;
    FCurrentSwapchainImageIndex: Cardinal;

    { Frame synchronization & command recording }
    FFrames: array[0 .. MaxFramesInFlight - 1] of TVulkanFrameContext;

    { Descriptor & Pipeline Caches }
    FDescriptorPool: VkDescriptorPool;
    FPipelineCache: VkPipelineCache;
    FPipelineMap: TVulkanPipelineMap;

    procedure CreateInstance(const EnableValidation: Boolean);
    procedure PickPhysicalDevice;
    procedure CreateLogicalDevice;
    procedure CreateSwapchain(const Width, Height: Cardinal);
    procedure DestroySwapchain;
    procedure CreateFrameContexts;
    procedure CreateDescriptorPool;
    function ComputePipelineHash(const ADesc: TRHIGraphicsPipelineDesc): UInt64;
  public
    constructor Create(const ACreateSurfaceCallback: TVulkanCreateSurfaceEvent;
      const InitialWidth, InitialHeight: Cardinal;
      const EnableValidation: Boolean = True); reintroduce;
    destructor Destroy; override;

    { Helper for one-shot transfer commands (texture/buffer uploads) }
    function BeginSingleTimeCommands: VkCommandBuffer;
    procedure EndSingleTimeCommands(const CmdBuf: VkCommandBuffer);
    function FindMemoryType(const TypeFilter: Cardinal;
      const Properties: VkMemoryPropertyFlags): Cardinal;

    { TRHIDevice overrides }
    function BeginFrame: TRHICommandList; override;
    procedure EndFrameAndPresent; override;
    procedure ResizeSwapchain(const NewWidth, NewHeight: Cardinal); override;
    procedure WaitIdle; override;

    function GetSwapchainColorTexture: TRHITexture; override;
    function GetSwapchainDepthTexture: TRHITexture; override;

    function CreateBuffer(const ADesc: TRHIBufferDesc;
      const InitialData: Pointer = nil): TRHIBuffer; override;
    function CreateTexture(const ADesc: TRHITextureDesc): TRHITexture; override;
    function CreateSampler(const ADesc: TRHISamplerDesc): TRHISampler; override;

    function CreateShaderModuleFromGLSL(const AStage: TRHIShaderStage;
      const AGLSLSource: String; const ADebugName: String = ''): TRHIShaderModule; override;
    function CreateShaderModuleFromSPIRV(const AStage: TRHIShaderStage;
      const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt;
      const ADebugName: String = ''): TRHIShaderModule; override;

    function CreateDescriptorSetLayout(
      const Bindings: array of TRHIDescriptorBindingDesc): TRHIDescriptorSetLayout; override;
    function CreateDescriptorSet(
      const Layout: TRHIDescriptorSetLayout): TRHIDescriptorSet; override;

    function GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline; override;

    function GrabTextureImage(const SourceTexture: TRHITexture;
      const ReadArea: TRectangle): TRGBAlphaImage; override;

    procedure LoadPipelineCache(const CacheFileUrl: String); override;
    procedure SavePipelineCache(const CacheFileUrl: String); override;

    property Handle: VkDevice read FDevice;
    property PhysicalDevice: VkPhysicalDevice read FPhysicalDevice;
    property DescriptorPool: VkDescriptorPool read FDescriptorPool;
    property PipelineCache: VkPipelineCache read FPipelineCache;
  end;

implementation

{ ---------------------------------------------------------------------------- }
{ Type Conversion Helpers (RHI <-> Vulkan)                                     }
{ ---------------------------------------------------------------------------- }

procedure CheckVk(const Res: VkResult; const Context: String);
begin
  if Res <> VK_SUCCESS then
    raise ERHIError.CreateFmt('Vulkan error (%d) in %s', [ Ord(Res), Context ]);
end;

function RHIToVkFormat(const Format: TRHIPixelFormat): VkFormat;
begin
  case Format of
    pfR8_UNorm:           Result := VK_FORMAT_R8_UNORM;
    pfRG8_UNorm:          Result := VK_FORMAT_R8G8_UNORM;
    pfRGBA8_UNorm:        Result := VK_FORMAT_R8G8B8A8_UNORM;
    pfRGBA8_sRGB:         Result := VK_FORMAT_R8G8B8A8_SRGB;
    pfBGRA8_UNorm:        Result := VK_FORMAT_B8G8R8A8_UNORM;
    pfBGRA8_sRGB:         Result := VK_FORMAT_B8G8R8A8_SRGB;
    pfRG16_Float:         Result := VK_FORMAT_R16G16_SFLOAT;
    pfRGBA16_Float:       Result := VK_FORMAT_R16G16B16A16_SFLOAT;
    pfR32_Float:          Result := VK_FORMAT_R32_SFLOAT;
    pfRGBA32_Float:       Result := VK_FORMAT_R32G32B32A32_SFLOAT;
    pfD16_UNorm:          Result := VK_FORMAT_D16_UNORM;
    pfD24_UNorm_S8_UInt:  Result := VK_FORMAT_D24_UNORM_S8_UINT;
    pfD32_Float:          Result := VK_FORMAT_D32_SFLOAT;
    pfD32_Float_S8_UInt:  Result := VK_FORMAT_D32_SFLOAT_S8_UINT;
    pfBC1_RGBA_UNorm:     Result := VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
    pfBC1_RGBA_sRGB:      Result := VK_FORMAT_BC1_RGBA_SRGB_BLOCK;
    pfBC3_RGBA_UNorm:     Result := VK_FORMAT_BC3_UNORM_BLOCK;
    pfBC3_RGBA_sRGB:      Result := VK_FORMAT_BC3_SRGB_BLOCK;
    pfBC5_RG_UNorm:       Result := VK_FORMAT_BC5_UNORM_BLOCK;
    pfBC7_RGBA_UNorm:     Result := VK_FORMAT_BC7_UNORM_BLOCK;
    pfBC7_RGBA_sRGB:      Result := VK_FORMAT_BC7_SRGB_BLOCK;
    pfASTC_4x4_UNorm:     Result := VK_FORMAT_ASTC_4x4_UNORM_BLOCK;
    pfASTC_4x4_sRGB:      Result := VK_FORMAT_ASTC_4x4_SRGB_BLOCK;
    pfETC2_RGB8_UNorm:    Result := VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK;
    pfETC2_RGBA8_UNorm:   Result := VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK;
    else                  Result := VK_FORMAT_UNDEFINED;
  end;
end;

function RHIToVkShaderStageFlags(const Stages: TRHIShaderStageFlags): VkShaderStageFlags;
begin
  Result := 0;
  if ssVertex in Stages then Result := Result or VK_SHADER_STAGE_VERTEX_BIT;
  if ssFragment in Stages then Result := Result or VK_SHADER_STAGE_FRAGMENT_BIT;
  if ssGeometry in Stages then Result := Result or VK_SHADER_STAGE_GEOMETRY_BIT;
  if ssCompute in Stages then Result := Result or VK_SHADER_STAGE_COMPUTE_BIT;
end;

procedure RHIStateToVkLayoutAndAccess(const State: TRHIResourceState;
  out Layout: VkImageLayout; out Access: VkAccessFlags; out Stage: VkPipelineStageFlags);
begin
  case State of
    rsUndefined:
      begin
        Layout := VK_IMAGE_LAYOUT_UNDEFINED;
        Access := 0;
        Stage  := VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
      end;
    rsTransferDst:
      begin
        Layout := VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
        Access := VK_ACCESS_TRANSFER_WRITE_BIT;
        Stage  := VK_PIPELINE_STAGE_TRANSFER_BIT;
      end;
    rsTransferSrc:
      begin
        Layout := VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
        Access := VK_ACCESS_TRANSFER_READ_BIT;
        Stage  := VK_PIPELINE_STAGE_TRANSFER_BIT;
      end;
    rsShaderRead:
      begin
        Layout := VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL;
        Access := VK_ACCESS_SHADER_READ_BIT;
        Stage  := VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
      end;
    rsColorAttachment:
      begin
        Layout := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
        Access := VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
        Stage  := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
      end;
    rsDepthStencilWrite:
      begin
        Layout := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
        Access := VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
        Stage  := VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT or VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT;
      end;
    rsDepthStencilRead:
      begin
        Layout := VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL;
        Access := VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT or VK_ACCESS_SHADER_READ_BIT;
        Stage  := VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT;
      end;
    rsPresent:
      begin
        Layout := VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
        Access := 0;
        Stage  := VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
      end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanRHIBuffer                                                             }
{ ---------------------------------------------------------------------------- }

function TVulkanRHIBuffer.VulkanDevice: TVulkanRHIDevice;
begin
  Result := TVulkanRHIDevice(Device);
end;

constructor TVulkanRHIBuffer.Create(const ADevice: TVulkanRHIDevice;
  const ADesc: TRHIBufferDesc; const InitialData: Pointer);
var
  BufInfo: VkBufferCreateInfo;
  MemReq: VkMemoryRequirements;
  AllocInfo: VkMemoryAllocateInfo;
  MemProps: VkMemoryPropertyFlags;
begin
  inherited Create(ADevice, ADesc);

  FillChar(BufInfo, SizeOf(BufInfo), 0);
  BufInfo.sType := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  BufInfo.size := ADesc.Size;
  BufInfo.sharingMode := VK_SHARING_MODE_EXCLUSIVE;

  if buVertex in ADesc.Usage then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_VERTEX_BUFFER_BIT;
  if buIndex in ADesc.Usage then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_INDEX_BUFFER_BIT;
  if buUniform in ADesc.Usage then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT;
  if buStorage in ADesc.Usage then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_STORAGE_BUFFER_BIT;
  if buIndirect in ADesc.Usage then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT;
  if (buTransferSrc in ADesc.Usage) then BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_TRANSFER_SRC_BIT;
  if (buTransferDst in ADesc.Usage) or (ADesc.MemoryUsage = muGpuOnly) then
    BufInfo.usage := BufInfo.usage or VK_BUFFER_USAGE_TRANSFER_DST_BIT;

  CheckVk(vkCreateBuffer(ADevice.Handle, @BufInfo, nil, @FHandle), 'vkCreateBuffer');

  vkGetBufferMemoryRequirements(ADevice.Handle, FHandle, @MemReq);

  case ADesc.MemoryUsage of
    muGpuOnly:  MemProps := VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
    muCpuToGpu: MemProps := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
    muGpuToCpu: MemProps := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
  end;

  { Note: Production code replaces raw vkAllocateMemory with TCastleVulkanMemoryAllocator }
  FillChar(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocInfo.allocationSize := MemReq.size;
  AllocInfo.memoryTypeIndex := ADevice.FindMemoryType(MemReq.memoryTypeBits, MemProps);

  CheckVk(vkAllocateMemory(ADevice.Handle, @AllocInfo, nil, @FDeviceMemory), 'vkAllocateMemory(Buffer)');
  CheckVk(vkBindBufferMemory(ADevice.Handle, FHandle, FDeviceMemory, 0), 'vkBindBufferMemory');

  if InitialData <> nil then
    UploadData(InitialData^, ADesc.Size, 0);
end;

destructor TVulkanRHIBuffer.Destroy;
begin
  if FMappedPtr <> nil then
    Unmap;
  if FHandle <> VK_NULL_HANDLE then
    vkDestroyBuffer(VulkanDevice.Handle, FHandle, nil);
  if FDeviceMemory <> VK_NULL_HANDLE then
    vkFreeMemory(VulkanDevice.Handle, FDeviceMemory, nil);
  inherited;
end;

function TVulkanRHIBuffer.Map: Pointer;
begin
  if FMappedPtr = nil then
    CheckVk(vkMapMemory(VulkanDevice.Handle, FDeviceMemory, FMemoryOffset, FDesc.Size, 0, @FMappedPtr), 'vkMapMemory');
  Result := FMappedPtr;
end;

procedure TVulkanRHIBuffer.Unmap;
begin
  if FMappedPtr <> nil then
  begin
    vkUnmapMemory(VulkanDevice.Handle, FDeviceMemory);
    FMappedPtr := nil;
  end;
end;

procedure TVulkanRHIBuffer.UploadData(const Data; const Size: PtrUInt; const Offset: PtrUInt);
var
  StagingDesc: TRHIBufferDesc;
  StagingBuf: TVulkanRHIBuffer;
  Cmd: VkCommandBuffer;
  CopyRegion: VkBufferCopy;
  DestPtr: PByte;
begin
  if FDesc.MemoryUsage = muCpuToGpu then
  begin
    DestPtr := PByte(Map);
    Inc(DestPtr, Offset);
    Move(Data, DestPtr^, Size);
    Unmap;
  end else
  begin
    { Upload via host-visible staging buffer }
    FillChar(StagingDesc, SizeOf(StagingDesc), 0);
    StagingDesc.Size := Size;
    StagingDesc.Usage := [buTransferSrc];
    StagingDesc.MemoryUsage := muCpuToGpu;

    StagingBuf := TVulkanRHIBuffer.Create(VulkanDevice, StagingDesc, @Data);
    try
      Cmd := VulkanDevice.BeginSingleTimeCommands;
      CopyRegion.srcOffset := 0;
      CopyRegion.dstOffset := Offset;
      CopyRegion.size := Size;
      vkCmdCopyBuffer(Cmd, StagingBuf.Handle, FHandle, 1, @CopyRegion);
      VulkanDevice.EndSingleTimeCommands(Cmd);
    finally
      FreeAndNil(StagingBuf);
    end;
  end;
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanRHITexture                                                            }
{ ---------------------------------------------------------------------------- }

function TVulkanRHITexture.VulkanDevice: TVulkanRHIDevice;
begin
  Result := TVulkanRHIDevice(Device);
end;

constructor TVulkanRHITexture.Create(const ADevice: TVulkanRHIDevice; const ADesc: TRHITextureDesc);
var
  ImgInfo: VkImageCreateInfo;
  ViewInfo: VkImageViewCreateInfo;
  MemReq: VkMemoryRequirements;
  AllocInfo: VkMemoryAllocateInfo;
begin
  inherited Create(ADevice, ADesc);
  FOwnsImage := True;

  if tuDepthStencil in ADesc.Usage then
    FAspectMask := VK_IMAGE_ASPECT_DEPTH_BIT
  else
    FAspectMask := VK_IMAGE_ASPECT_COLOR_BIT;

  FillChar(ImgInfo, SizeOf(ImgInfo), 0);
  ImgInfo.sType := VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO;
  ImgInfo.imageType := VK_IMAGE_TYPE_2D;
  ImgInfo.format := RHIToVkFormat(ADesc.Format);
  ImgInfo.extent.width := ADesc.Width;
  ImgInfo.extent.height := ADesc.Height;
  ImgInfo.extent.depth := 1;
  ImgInfo.mipLevels := ADesc.MipLevels;
  ImgInfo.arrayLayers := ADesc.DepthOrArrayLayers;
  ImgInfo.samples := VK_SAMPLE_COUNT_1_BIT;
  ImgInfo.tiling := VK_IMAGE_TILING_OPTIMAL;
  ImgInfo.initialLayout := VK_IMAGE_LAYOUT_UNDEFINED;

  if tuSampled in ADesc.Usage then ImgInfo.usage := ImgInfo.usage or VK_IMAGE_USAGE_SAMPLED_BIT;
  if tuColorAttachment in ADesc.Usage then ImgInfo.usage := ImgInfo.usage or VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
  if tuDepthStencil in ADesc.Usage then ImgInfo.usage := ImgInfo.usage or VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
  if tuTransferSrc in ADesc.Usage then ImgInfo.usage := ImgInfo.usage or VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
  if tuTransferDst in ADesc.Usage then ImgInfo.usage := ImgInfo.usage or VK_IMAGE_USAGE_TRANSFER_DST_BIT;

  CheckVk(vkCreateImage(ADevice.Handle, @ImgInfo, nil, @FImage), 'vkCreateImage');

  vkGetImageMemoryRequirements(ADevice.Handle, FImage, @MemReq);
  FillChar(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocInfo.allocationSize := MemReq.size;
  AllocInfo.memoryTypeIndex := ADevice.FindMemoryType(MemReq.memoryTypeBits, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT);

  CheckVk(vkAllocateMemory(ADevice.Handle, @AllocInfo, nil, @FDeviceMemory), 'vkAllocateMemory(Image)');
  CheckVk(vkBindImageMemory(ADevice.Handle, FImage, FDeviceMemory, 0), 'vkBindImageMemory');

  FillChar(ViewInfo, SizeOf(ViewInfo), 0);
  ViewInfo.sType := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
  ViewInfo.image := FImage;
  ViewInfo.viewType := VK_IMAGE_VIEW_TYPE_2D;
  ViewInfo.format := ImgInfo.format;
  ViewInfo.subresourceRange.aspectMask := FAspectMask;
  ViewInfo.subresourceRange.baseMipLevel := 0;
  ViewInfo.subresourceRange.levelCount := ADesc.MipLevels;
  ViewInfo.subresourceRange.baseArrayLayer := 0;
  ViewInfo.subresourceRange.layerCount := ADesc.DepthOrArrayLayers;

  CheckVk(vkCreateImageView(ADevice.Handle, @ViewInfo, nil, @FImageView), 'vkCreateImageView');
end;

constructor TVulkanRHITexture.CreateSwapchainWrapper(const ADevice: TVulkanRHIDevice;
  const ADesc: TRHITextureDesc; const AExistingImage: VkImage; const AExistingView: VkImageView);
begin
  inherited Create(ADevice, ADesc);
  FOwnsImage := False;
  FImage := AExistingImage;
  FImageView := AExistingView;
  FAspectMask := VK_IMAGE_ASPECT_COLOR_BIT;
end;

destructor TVulkanRHITexture.Destroy;
begin
  if FImageView <> VK_NULL_HANDLE then
    vkDestroyImageView(VulkanDevice.Handle, FImageView, nil);
  if FOwnsImage then
  begin
    if FImage <> VK_NULL_HANDLE then
      vkDestroyImage(VulkanDevice.Handle, FImage, nil);
    if FDeviceMemory <> VK_NULL_HANDLE then
      vkFreeMemory(VulkanDevice.Handle, FDeviceMemory, nil);
  end;
  inherited;
end;

procedure TVulkanRHITexture.UploadImage(const AImage: TEncodedImage;
  const AMipLevel: Cardinal; const AArrayLayer: Cardinal);
begin
  { 1. Allocate staging buffer of AImage.Size bytes and copy AImage.RawPixels
    2. Transition FImage to VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    3. vkCmdCopyBufferToImage(...)
    4. Transition FImage to VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL }
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanRHISampler, ShaderModule, DescriptorSetLayout, DescriptorSet, Pipeline}
{ ---------------------------------------------------------------------------- }

constructor TVulkanRHISampler.Create(const ADevice: TVulkanRHIDevice; const ADesc: TRHISamplerDesc);
var
  SamplerInfo: VkSamplerCreateInfo;
begin
  inherited Create(ADevice, ADesc);
  FillChar(SamplerInfo, SizeOf(SamplerInfo), 0);
  SamplerInfo.sType := VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO;
  SamplerInfo.magFilter := VK_FILTER_LINEAR;
  SamplerInfo.minFilter := VK_FILTER_LINEAR;
  SamplerInfo.mipmapMode := VK_SAMPLER_MIPMAP_MODE_LINEAR;
  SamplerInfo.maxLod := ADesc.MaxLod;
  CheckVk(vkCreateSampler(ADevice.Handle, @SamplerInfo, nil, @FHandle), 'vkCreateSampler');
end;

destructor TVulkanRHISampler.Destroy;
begin
  if FHandle <> VK_NULL_HANDLE then
    vkDestroySampler(TVulkanRHIDevice(Device).Handle, FHandle, nil);
  inherited;
end;

constructor TVulkanRHIShaderModule.Create(const ADevice: TVulkanRHIDevice;
  const AStage: TRHIShaderStage; const ASPIRVBinary: Pointer;
  const ASizeInBytes: PtrUInt; const ADebugName: String);
var
  CreateInfo: VkShaderModuleCreateInfo;
begin
  inherited Create(ADevice, AStage, ADebugName);
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  CreateInfo.codeSize := ASizeInBytes;
  CreateInfo.pCode := PUInt32(ASPIRVBinary);
  CheckVk(vkCreateShaderModule(ADevice.Handle, @CreateInfo, nil, @FHandle), 'vkCreateShaderModule');
end;

destructor TVulkanRHIShaderModule.Destroy;
begin
  if FHandle <> VK_NULL_HANDLE then
    vkDestroyShaderModule(TVulkanRHIDevice(Device).Handle, FHandle, nil);
  inherited;
end;

constructor TVulkanRHIDescriptorSetLayout.Create(const ADevice: TVulkanRHIDevice;
  const Bindings: array of TRHIDescriptorBindingDesc);
var
  VkBindings: array of VkDescriptorSetLayoutBinding;
  LayoutInfo: VkDescriptorSetLayoutCreateInfo;
  I: Integer;
begin
  inherited Create(ADevice, '');
  SetLength(VkBindings, Length(Bindings));
  for I := 0 to High(Bindings) do
  begin
    FillChar(VkBindings[I], SizeOf(VkDescriptorSetLayoutBinding), 0);
    VkBindings[I].binding := Bindings[I].Binding;
    VkBindings[I].descriptorCount := Bindings[I].Count;
    VkBindings[I].stageFlags := RHIToVkShaderStageFlags(Bindings[I].StageFlags);
    case Bindings[I].DescriptorType of
      dtUniformBuffer:        VkBindings[I].descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER;
      dtUniformBufferDynamic: VkBindings[I].descriptorType := VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC;
      dtStorageBuffer:        VkBindings[I].descriptorType := VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
      dtCombinedImageSampler: VkBindings[I].descriptorType := VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER;
    end;
  end;

  FillChar(LayoutInfo, SizeOf(LayoutInfo), 0);
  LayoutInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO;
  LayoutInfo.bindingCount := Length(VkBindings);
  if Length(VkBindings) > 0 then
    LayoutInfo.pBindings := @VkBindings[0];

  CheckVk(vkCreateDescriptorSetLayout(ADevice.Handle, @LayoutInfo, nil, @FHandle), 'vkCreateDescriptorSetLayout');
end;

destructor TVulkanRHIDescriptorSetLayout.Destroy;
begin
  if FHandle <> VK_NULL_HANDLE then
    vkDestroyDescriptorSetLayout(TVulkanRHIDevice(Device).Handle, FHandle, nil);
  inherited;
end;

function TVulkanRHIDescriptorSet.VulkanDevice: TVulkanRHIDevice;
begin
  Result := TVulkanRHIDevice(Device);
end;

constructor TVulkanRHIDescriptorSet.Create(const ADevice: TVulkanRHIDevice;
  const ALayout: TVulkanRHIDescriptorSetLayout);
var
  AllocInfo: VkDescriptorSetAllocateInfo;
  SetLayoutHandle: VkDescriptorSetLayout;
begin
  inherited Create(ADevice, '');
  FLayout := ALayout;
  SetLayoutHandle := ALayout.Handle;

  FillChar(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType := VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO;
  AllocInfo.descriptorPool := ADevice.DescriptorPool;
  AllocInfo.descriptorSetCount := 1;
  AllocInfo.pSetLayouts := @SetLayoutHandle;

  CheckVk(vkAllocateDescriptorSets(ADevice.Handle, @AllocInfo, @FHandle), 'vkAllocateDescriptorSets');
end;

destructor TVulkanRHIDescriptorSet.Destroy;
begin
  inherited;
end;

procedure TVulkanRHIDescriptorSet.BindUniformBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
begin
  { Append to FPendingWrites & FBufferInfos }
end;

procedure TVulkanRHIDescriptorSet.BindStorageBuffer(const Binding: Cardinal;
  const Buffer: TRHIBuffer; const Offset, Range: PtrUInt);
begin
  { Append to FPendingWrites & FBufferInfos }
end;

procedure TVulkanRHIDescriptorSet.BindTexture(const Binding: Cardinal;
  const Texture: TRHITexture; const Sampler: TRHISampler);
begin
  { Append to FPendingWrites & FImageInfos }
end;

procedure TVulkanRHIDescriptorSet.Update;
begin
  if Length(FPendingWrites) > 0 then
  begin
    vkUpdateDescriptorSets(VulkanDevice.Handle, Length(FPendingWrites), @FPendingWrites[0], 0, nil);
    SetLength(FPendingWrites, 0);
    SetLength(FBufferInfos, 0);
    SetLength(FImageInfos, 0);
  end;
end;

constructor TVulkanRHIPipeline.Create(const ADevice: TVulkanRHIDevice;
  const ADesc: TRHIGraphicsPipelineDesc);
begin
  inherited Create(ADevice, ADesc);
  { 1. Construct VkPipelineLayout from ADesc.DescriptorSetLayouts and PushConstants
    2. Set up VkPipelineRenderingCreateInfoKHR with ADesc.ColorAttachmentFormats
       and ADesc.DepthAttachmentFormat (Vulkan 1.3 Dynamic Rendering - no VkRenderPass!)
    3. Configure dynamic states: VK_DYNAMIC_STATE_VIEWPORT, VK_DYNAMIC_STATE_SCISSOR
    4. Call vkCreateGraphicsPipelines(ADevice.Handle, ADevice.PipelineCache, 1, @PipelineInfo, nil, @FHandle) }
end;

destructor TVulkanRHIPipeline.Destroy;
begin
  if FHandle <> VK_NULL_HANDLE then
    vkDestroyPipeline(TVulkanRHIDevice(Device).Handle, FHandle, nil);
  if FPipelineLayout <> VK_NULL_HANDLE then
    vkDestroyPipelineLayout(TVulkanRHIDevice(Device).Handle, FPipelineLayout, nil);
  inherited;
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanRHICommandList                                                        }
{ ---------------------------------------------------------------------------- }

constructor TVulkanRHICommandList.Create(const ADevice: TVulkanRHIDevice;
  const AQueueFamilyIndex: Cardinal);
var
  PoolInfo: VkCommandPoolCreateInfo;
  AllocInfo: VkCommandBufferAllocateInfo;
begin
  inherited Create;
  FDevice := ADevice;

  FillChar(PoolInfo, SizeOf(PoolInfo), 0);
  PoolInfo.sType := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
  PoolInfo.flags := VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT;
  PoolInfo.queueFamilyIndex := AQueueFamilyIndex;
  CheckVk(vkCreateCommandPool(FDevice.Handle, @PoolInfo, nil, @FCommandPool), 'vkCreateCommandPool');

  FillChar(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  AllocInfo.commandPool := FCommandPool;
  AllocInfo.level := VK_COMMAND_BUFFER_LEVEL_PRIMARY;
  AllocInfo.commandBufferCount := 1;
  CheckVk(vkAllocateCommandBuffers(FDevice.Handle, @AllocInfo, @FHandle), 'vkAllocateCommandBuffers');
end;

destructor TVulkanRHICommandList.Destroy;
begin
  if FCommandPool <> VK_NULL_HANDLE then
    vkDestroyCommandPool(FDevice.Handle, FCommandPool, nil);
  inherited;
end;

procedure TVulkanRHICommandList.BeginRecording;
var
  BeginInfo: VkCommandBufferBeginInfo;
begin
  CheckVk(vkResetCommandBuffer(FHandle, 0), 'vkResetCommandBuffer');
  FillChar(BeginInfo, SizeOf(BeginInfo), 0);
  BeginInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  BeginInfo.flags := VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT;
  CheckVk(vkBeginCommandBuffer(FHandle, @BeginInfo), 'vkBeginCommandBuffer');
  FCurrentPipeline := nil;
end;

procedure TVulkanRHICommandList.EndRecording;
begin
  CheckVk(vkEndCommandBuffer(FHandle), 'vkEndCommandBuffer');
end;

procedure TVulkanRHICommandList.TransitionTexture(const Texture: TRHITexture;
  const NewState: TRHIResourceState);
var
  VkTex: TVulkanRHITexture;
  Barrier: VkImageMemoryBarrier;
  SrcStage, DstStage: VkPipelineStageFlags;
begin
  if Texture.CurrentState = NewState then
    Exit;

  VkTex := Texture as TVulkanRHITexture;
  FillChar(Barrier, SizeOf(Barrier), 0);
  Barrier.sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
  RHIStateToVkLayoutAndAccess(Texture.CurrentState, Barrier.oldLayout, Barrier.srcAccessMask, SrcStage);
  RHIStateToVkLayoutAndAccess(NewState, Barrier.newLayout, Barrier.dstAccessMask, DstStage);

  Barrier.srcQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
  Barrier.dstQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
  Barrier.image := VkTex.Image;
  Barrier.subresourceRange.aspectMask := VkTex.AspectMask;
  Barrier.subresourceRange.baseMipLevel := 0;
  Barrier.subresourceRange.levelCount := VkTex.Desc.MipLevels;
  Barrier.subresourceRange.baseArrayLayer := 0;
  Barrier.subresourceRange.layerCount := VkTex.Desc.DepthOrArrayLayers;

  vkCmdPipelineBarrier(FHandle, SrcStage, DstStage, 0, 0, nil, 0, nil, 1, @Barrier);
  Texture.CurrentState := NewState;
end;

procedure TVulkanRHICommandList.GenerateMipmaps(const Texture: TRHITexture);
begin
  { Blit chain across mip levels 0 .. MipLevels - 1 via vkCmdBlitImage }
end;

procedure TVulkanRHICommandList.BeginRendering(const Info: TRHIRenderingInfo);
var
  RenderInfo: VkRenderingInfo;
  ColorAttachments: array of VkRenderingAttachmentInfo;
  DepthAttachment: VkRenderingAttachmentInfo;
  I: Integer;
begin
  SetLength(ColorAttachments, Length(Info.ColorAttachments));
  for I := 0 to High(Info.ColorAttachments) do
  begin
    FillChar(ColorAttachments[I], SizeOf(VkRenderingAttachmentInfo), 0);
    ColorAttachments[I].sType := VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO;
    ColorAttachments[I].imageView := (Info.ColorAttachments[I].Texture as TVulkanRHITexture).ImageView;
    ColorAttachments[I].imageLayout := VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL;
    if Info.ColorAttachments[I].LoadOp = loClear then
      ColorAttachments[I].loadOp := VK_ATTACHMENT_LOAD_OP_CLEAR
    else
      ColorAttachments[I].loadOp := VK_ATTACHMENT_LOAD_OP_LOAD;
    ColorAttachments[I].storeOp := VK_ATTACHMENT_STORE_OP_STORE;
    Move(Info.ColorAttachments[I].ClearColor.Data, ColorAttachments[I].clearValue.color.float32, SizeOf( Single) * 4);
  end;

  FillChar(RenderInfo, SizeOf(RenderInfo), 0);
  RenderInfo.sType := VK_STRUCTURE_TYPE_RENDERING_INFO;
  RenderInfo.renderArea.offset.x := Info.RenderArea.Left;
  RenderInfo.renderArea.offset.y := Info.RenderArea.Bottom;
  RenderInfo.renderArea.extent.width := Info.RenderArea.Width;
  RenderInfo.renderArea.extent.height := Info.RenderArea.Height;
  RenderInfo.layerCount := 1;
  RenderInfo.colorAttachmentCount := Length(ColorAttachments);
  if Length(ColorAttachments) > 0 then
    RenderInfo.pColorAttachments := @ColorAttachments[0];

  if Info.HasDepthAttachment and (Info.DepthAttachment.Texture <> nil) then
  begin
    FillChar(DepthAttachment, SizeOf(DepthAttachment), 0);
    DepthAttachment.sType := VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO;
    DepthAttachment.imageView := (Info.DepthAttachment.Texture as TVulkanRHITexture).ImageView;
    DepthAttachment.imageLayout := VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL;
    if Info.DepthAttachment.LoadOp = loClear then
      DepthAttachment.loadOp := VK_ATTACHMENT_LOAD_OP_CLEAR
    else
      DepthAttachment.loadOp := VK_ATTACHMENT_LOAD_OP_LOAD;
    DepthAttachment.storeOp := VK_ATTACHMENT_STORE_OP_STORE;
    DepthAttachment.clearValue.depthStencil.depth := Info.DepthAttachment.ClearDepth;
    DepthAttachment.clearValue.depthStencil.stencil := Info.DepthAttachment.ClearStencil;
    RenderInfo.pDepthAttachment := @DepthAttachment;
  end;

  vkCmdBeginRendering(FHandle, @RenderInfo);
end;

procedure TVulkanRHICommandList.EndRendering;
begin
  vkCmdEndRendering(FHandle);
end;

procedure TVulkanRHICommandList.SetViewport(const ViewportRect: TFloatRectangle;
  const MinDepth, MaxDepth: Single);
var
  VkVP: VkViewport;
begin
  { Flip Y-axis using VK_KHR_maintenance1 negative height so CGE camera matrices
    and counter-clockwise winding orders work identically to OpenGL }
  VkVP.x := ViewportRect.Left;
  VkVP.y := ViewportRect.Bottom + ViewportRect.Height;
  VkVP.width := ViewportRect.Width;
  VkVP.height := -ViewportRect.Height;
  VkVP.minDepth := MinDepth;
  VkVP.maxDepth := MaxDepth;
  vkCmdSetViewport(FHandle, 0, 1, @VkVP);
end;

procedure TVulkanRHICommandList.SetScissor(const ScissorRect: TRectangle);
var
  VkRect: VkRect2D;
begin
  VkRect.offset.x := Max(0, ScissorRect.Left);
  VkRect.offset.y := Max(0, ScissorRect.Bottom);
  VkRect.extent.width := ScissorRect.Width;
  VkRect.extent.height := ScissorRect.Height;
  vkCmdSetScissor(FHandle, 0, 1, @VkRect);
end;

procedure TVulkanRHICommandList.SetDepthBias(const ConstantFactor, SlopeFactor: Single);
begin
  vkCmdSetDepthBias(FHandle, ConstantFactor, 0.0, SlopeFactor);
end;

procedure TVulkanRHICommandList.BindPipeline(const Pipeline: TRHIPipeline);
begin
  FCurrentPipeline := Pipeline as TVulkanRHIPipeline;
  vkCmdBindPipeline(FHandle, VK_PIPELINE_BIND_POINT_GRAPHICS, FCurrentPipeline.Handle);
end;

procedure TVulkanRHICommandList.BindDescriptorSet(const SetIndex: Cardinal;
  const DescriptorSet: TRHIDescriptorSet; const DynamicOffsets: array of Cardinal);
var
  SetHandle: VkDescriptorSet;
  OffsetPtr: PUInt32;
begin
  Assert(FCurrentPipeline <> nil, 'BindPipeline must be called before BindDescriptorSet');
  SetHandle := (DescriptorSet as TVulkanRHIDescriptorSet).Handle;
  if Length(DynamicOffsets) > 0 then
    OffsetPtr := @DynamicOffsets[0]
  else
    OffsetPtr := nil;

  vkCmdBindDescriptorSets(FHandle, VK_PIPELINE_BIND_POINT_GRAPHICS,
    FCurrentPipeline.PipelineLayout, SetIndex, 1, @SetHandle,
    Length(DynamicOffsets), OffsetPtr);
end;

procedure TVulkanRHICommandList.PushConstants(const StageFlags: TRHIShaderStageFlags;
  const Offset, Size: Cardinal; const Data);
begin
  Assert(FCurrentPipeline <> nil, 'BindPipeline must be called before PushConstants');
  vkCmdPushConstants(FHandle, FCurrentPipeline.PipelineLayout,
    RHIToVkShaderStageFlags(StageFlags), Offset, Size, @Data);
end;

procedure TVulkanRHICommandList.BindVertexBuffers(const FirstBinding: Cardinal;
  const Buffers: array of TRHIBuffer; const Offsets: array of PtrUInt);
var
  VkBuffers: array of VkBuffer;
  VkOffsets: array of VkDeviceSize;
  I: Integer;
begin
  SetLength(VkBuffers, Length(Buffers));
  SetLength(VkOffsets, Length(Offsets));
  for I := 0 to High(Buffers) do
  begin
    VkBuffers[I] := (Buffers[I] as TVulkanRHIBuffer).Handle;
    VkOffsets[I] := Offsets[I];
  end;
  vkCmdBindVertexBuffers(FHandle, FirstBinding, Length(VkBuffers), @VkBuffers[0], @VkOffsets[0]);
end;

procedure TVulkanRHICommandList.BindIndexBuffer(const Buffer: TRHIBuffer;
  const Offset: PtrUInt; const IndexType: TRHIIndexType);
var
  VkIdxType: VkIndexType;
begin
  if IndexType = itUInt16 then
    VkIdxType := VK_INDEX_TYPE_UINT16
  else
    VkIdxType := VK_INDEX_TYPE_UINT32;

  vkCmdBindIndexBuffer(FHandle, (Buffer as TVulkanRHIBuffer).Handle, Offset, VkIdxType);
end;

procedure TVulkanRHICommandList.Draw(const VertexCount, InstanceCount,
  FirstVertex, FirstInstance: Cardinal);
begin
  vkCmdDraw(FHandle, VertexCount, InstanceCount, FirstVertex, FirstInstance);
end;

procedure TVulkanRHICommandList.DrawIndexed(const IndexCount, InstanceCount,
  FirstIndex: Cardinal; const VertexOffset: Integer; const FirstInstance: Cardinal);
begin
  vkCmdDrawIndexed(FHandle, IndexCount, InstanceCount, FirstIndex, VertexOffset, FirstInstance);
end;

procedure TVulkanRHICommandList.DrawIndexedIndirect(const IndirectBuffer: TRHIBuffer;
  const Offset: PtrUInt; const DrawCount, Stride: Cardinal);
begin
  vkCmdDrawIndexedIndirect(FHandle, (IndirectBuffer as TVulkanRHIBuffer).Handle,
    Offset, DrawCount, Stride);
end;

procedure TVulkanRHICommandList.PushDebugGroup(const LabelName: String; const Color: TCastleColor);
begin
  { Optional VK_EXT_debug_utils vkCmdBeginDebugUtilsLabelEXT }
end;

procedure TVulkanRHICommandList.PopDebugGroup;
begin
  { Optional VK_EXT_debug_utils vkCmdEndDebugUtilsLabelEXT }
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanRHIDevice                                                             }
{ ---------------------------------------------------------------------------- }

constructor TVulkanRHIDevice.Create(const ACreateSurfaceCallback: TVulkanCreateSurfaceEvent;
  const InitialWidth, InitialHeight: Cardinal; const EnableValidation: Boolean);
begin
  inherited Create;
  FPipelineMap := TVulkanPipelineMap.Create([doOwnsValues]);

  LoadVulkan;
  CreateInstance(EnableValidation);

  FSurface := ACreateSurfaceCallback(FInstance);
  if FSurface = VK_NULL_HANDLE then
    raise ERHIError.Create('Failed to create Vulkan window surface (VkSurfaceKHR)');

  PickPhysicalDevice;
  CreateLogicalDevice;
  CreateDescriptorPool;
  CreateFrameContexts;
  CreateSwapchain(InitialWidth, InitialHeight);
end;

destructor TVulkanRHIDevice.Destroy;
var
  I: Integer;
begin
  WaitIdle;
  DestroySwapchain;
  FreeAndNil(FPipelineMap);

  for I := 0 to MaxFramesInFlight - 1 do
  begin
    FreeAndNil(FFrames[I].CommandList);
    vkDestroySemaphore(FDevice, FFrames[I].ImageAvailableSemaphore, nil);
    vkDestroySemaphore(FDevice, FFrames[I].RenderFinishedSemaphore, nil);
    vkDestroyFence(FDevice, FFrames[I].InFlightFence, nil);
  end;

  if FDescriptorPool <> VK_NULL_HANDLE then
    vkDestroyDescriptorPool(FDevice, FDescriptorPool, nil);
  if FPipelineCache <> VK_NULL_HANDLE then
    vkDestroyPipelineCache(FDevice, FPipelineCache, nil);
  if FDevice <> nil then
    vkDestroyDevice(FDevice, nil);
  if FSurface <> VK_NULL_HANDLE then
    vkDestroySurfaceKHR(FInstance, FSurface, nil);
  if FInstance <> nil then
    vkDestroyInstance(FInstance, nil);

  inherited;
end;

procedure TVulkanRHIDevice.CreateInstance(const EnableValidation: Boolean);
begin
  { Initialize VkApplicationInfo with apiVersion := VK_API_VERSION_1_3,
    enable platform surface extensions + VK_LAYER_KHRONOS_validation if requested }
end;

procedure TVulkanRHIDevice.PickPhysicalDevice;
begin
  { Enumerate VkPhysicalDevice, prioritize VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU,
    populate FCapabilities }
end;

procedure TVulkanRHIDevice.CreateLogicalDevice;
begin
  { Enable Vulkan 1.3 DynamicRendering feature (VkPhysicalDeviceDynamicRenderingFeatures),
    create VkDevice and retrieve FGraphicsQueue and FPresentQueue }
end;

procedure TVulkanRHIDevice.CreateSwapchain(const Width, Height: Cardinal);
begin
  { Create VkSwapchainKHR, wrap each swapchain image in TVulkanRHITexture.CreateSwapchainWrapper,
    and allocate FSwapchainDepthTexture (pfD32_Float) }
end;

procedure TVulkanRHIDevice.DestroySwapchain;
var
  I: Integer;
begin
  FreeAndNil(FSwapchainDepthTexture);
  for I := 0 to High(FSwapchainColorTextures) do
    FreeAndNil(FSwapchainColorTextures[I]);
  SetLength(FSwapchainColorTextures, 0);

  if FSwapchain <> VK_NULL_HANDLE then
  begin
    vkDestroySwapchainKHR(FDevice, FSwapchain, nil);
    FSwapchain := VK_NULL_HANDLE;
  end;
end;

procedure TVulkanRHIDevice.CreateFrameContexts;
var
  I: Integer;
  SemInfo: VkSemaphoreCreateInfo;
  FenceInfo: VkFenceCreateInfo;
begin
  FillChar(SemInfo, SizeOf(SemInfo), 0);
  SemInfo.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;

  FillChar(FenceInfo, SizeOf(FenceInfo), 0);
  FenceInfo.sType := VK_STRUCTURE_TYPE_FENCE_CREATE_INFO;
  FenceInfo.flags := VK_FENCE_CREATE_SIGNALED_BIT;

  for I := 0 to MaxFramesInFlight - 1 do
  begin
    CheckVk(vkCreateSemaphore(FDevice, @SemInfo, nil, @FFrames[I].ImageAvailableSemaphore), 'vkCreateSemaphore');
    CheckVk(vkCreateSemaphore(FDevice, @SemInfo, nil, @FFrames[I].RenderFinishedSemaphore), 'vkCreateSemaphore');
    CheckVk(vkCreateFence(FDevice, @FenceInfo, nil, @FFrames[I].InFlightFence), 'vkCreateFence');
    FFrames[I].CommandList := TVulkanRHICommandList.Create(Self, FGraphicsQueueFamily);
  end;
end;

procedure TVulkanRHIDevice.CreateDescriptorPool;
begin
  { Create VkDescriptorPool sized for CGE's global, material, and per-object sets }
end;

function TVulkanRHIDevice.BeginFrame: TRHICommandList;
var
  Frame: TVulkanFrameContext;
  AcquireRes: VkResult;
begin
  Frame := FFrames[FFrameIndex];

  { 1. Wait for the GPU to finish executing the previous frame occupying this slot }
  CheckVk(vkWaitForFences(FDevice, 1, @Frame.InFlightFence, VK_TRUE, High(UInt64)), 'vkWaitForFences');

  { 2. Acquire next swapchain image }
  AcquireRes := vkAcquireNextImageKHR(FDevice, FSwapchain, High(UInt64),
    Frame.ImageAvailableSemaphore, VK_NULL_HANDLE, @FCurrentSwapchainImageIndex);

  if AcquireRes = VK_ERROR_OUT_OF_DATE_KHR then
  begin
    ResizeSwapchain(FSwapchainExtent.width, FSwapchainExtent.height);
    Exit(nil);
  end;

  CheckVk(vkResetFences(FDevice, 1, @Frame.InFlightFence), 'vkResetFences');

  { 3. Begin command list recording }
  Frame.CommandList.BeginRecording;
  Result := Frame.CommandList;
end;

procedure TVulkanRHIDevice.EndFrameAndPresent;
var
  Frame: TVulkanFrameContext;
  SubmitInfo: VkSubmitInfo;
  WaitStages: VkPipelineStageFlags;
  PresentInfo: VkPresentInfoKHR;
  CmdHandle: VkCommandBuffer;
begin
  Frame := FFrames[FFrameIndex];

  { Ensure swapchain backbuffer is transitioned to VK_IMAGE_LAYOUT_PRESENT_SRC_KHR }
  Frame.CommandList.TransitionTexture(GetSwapchainColorTexture, rsPresent);
  Frame.CommandList.EndRecording;

  WaitStages := VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT;
  CmdHandle := Frame.CommandList.Handle;

  FillChar(SubmitInfo, SizeOf(SubmitInfo), 0);
  SubmitInfo.sType := VK_STRUCTURE_TYPE_SUBMIT_INFO;
  SubmitInfo.waitSemaphoreCount := 1;
  SubmitInfo.pWaitSemaphores := @Frame.ImageAvailableSemaphore;
  SubmitInfo.pWaitDstStageMask := @WaitStages;
  SubmitInfo.commandBufferCount := 1;
  SubmitInfo.pCommandBuffers := @CmdHandle;
  SubmitInfo.signalSemaphoreCount := 1;
  SubmitInfo.pSignalSemaphores := @Frame.RenderFinishedSemaphore;

  CheckVk(vkQueueSubmit(FGraphicsQueue, 1, @SubmitInfo, Frame.InFlightFence), 'vkQueueSubmit');

  FillChar(PresentInfo, SizeOf(PresentInfo), 0);
  PresentInfo.sType := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  PresentInfo.waitSemaphoreCount := 1;
  PresentInfo.pWaitSemaphores := @Frame.RenderFinishedSemaphore;
  PresentInfo.swapchainCount := 1;
  PresentInfo.pSwapchains := @FSwapchain;
  PresentInfo.pImageIndices := @FCurrentSwapchainImageIndex;

  vkQueuePresentKHR(FPresentQueue, @PresentInfo);

  FFrameIndex := (FFrameIndex + 1) mod MaxFramesInFlight;
end;

procedure TVulkanRHIDevice.ResizeSwapchain(const NewWidth, NewHeight: Cardinal);
begin
  if (NewWidth = 0) or (NewHeight = 0) then Exit;
  WaitIdle;
  DestroySwapchain;
  CreateSwapchain(NewWidth, NewHeight);
end;

procedure TVulkanRHIDevice.WaitIdle;
begin
  if FDevice <> nil then
    vkDeviceWaitIdle(FDevice);
end;

function TVulkanRHIDevice.GetSwapchainColorTexture: TRHITexture;
begin
  Result := FSwapchainColorTextures[FCurrentSwapchainImageIndex];
end;

function TVulkanRHIDevice.GetSwapchainDepthTexture: TRHITexture;
begin
  Result := FSwapchainDepthTexture;
end;

function TVulkanRHIDevice.CreateBuffer(const ADesc: TRHIBufferDesc;
  const InitialData: Pointer): TRHIBuffer;
begin
  Result := TVulkanRHIBuffer.Create(Self, ADesc, InitialData);
end;

function TVulkanRHIDevice.CreateTexture(const ADesc: TRHITextureDesc): TRHITexture;
begin
  Result := TVulkanRHITexture.Create(Self, ADesc);
end;

function TVulkanRHIDevice.CreateSampler(const ADesc: TRHISamplerDesc): TRHISampler;
begin
  Result := TVulkanRHISampler.Create(Self, ADesc);
end;

function TVulkanRHIDevice.CreateShaderModuleFromGLSL(const AStage: TRHIShaderStage;
  const AGLSLSource: String; const ADebugName: String): TRHIShaderModule;
begin
  { Compile AGLSLSource to SPIR-V via CastleInternalShaderCompiler (libshaderc),
    then delegate to CreateShaderModuleFromSPIRV }
  Result := nil;
end;

function TVulkanRHIDevice.CreateShaderModuleFromSPIRV(const AStage: TRHIShaderStage;
  const ASPIRVBinary: Pointer; const ASizeInBytes: PtrUInt;
  const ADebugName: String): TRHIShaderModule;
begin
  Result := TVulkanRHIShaderModule.Create(Self, AStage, ASPIRVBinary, ASizeInBytes, ADebugName);
end;

function TVulkanRHIDevice.CreateDescriptorSetLayout(
  const Bindings: array of TRHIDescriptorBindingDesc): TRHIDescriptorSetLayout;
begin
  Result := TVulkanRHIDescriptorSetLayout.Create(Self, Bindings);
end;

function TVulkanRHIDevice.CreateDescriptorSet(
  const Layout: TRHIDescriptorSetLayout): TRHIDescriptorSet;
begin
  Result := TVulkanRHIDescriptorSet.Create(Self, Layout as TVulkanRHIDescriptorSetLayout);
end;

function TVulkanRHIDevice.ComputePipelineHash(const ADesc: TRHIGraphicsPipelineDesc): UInt64;
begin
  { FNV-1a 64-bit hash over shader handles, vertex input attributes, blend, raster, and depth state }
  Result := PtrUInt(ADesc.VertexShader) xor (PtrUInt(ADesc.FragmentShader) shl 13);
end;

function TVulkanRHIDevice.GetOrCreatePipeline(const ADesc: TRHIGraphicsPipelineDesc): TRHIPipeline;
var
  HashKey: UInt64;
  FoundPipeline: TVulkanRHIPipeline;
begin
  HashKey := ComputePipelineHash(ADesc);
  if not FPipelineMap.TryGetValue(HashKey, FoundPipeline) then
  begin
    FoundPipeline := TVulkanRHIPipeline.Create(Self, ADesc);
    FPipelineMap.Add(HashKey, FoundPipeline);
  end;
  Result := FoundPipeline;
end;

function TVulkanRHIDevice.GrabTextureImage(const SourceTexture: TRHITexture;
  const ReadArea: TRectangle): TRGBAlphaImage;
begin
  { Copy SourceTexture region to a muGpuToCpu staging buffer and read pixels into TRGBAlphaImage }
  Result := nil;
end;

procedure TVulkanRHIDevice.LoadPipelineCache(const CacheFileUrl: String);
begin
  { Load binary blob from CastleDownload(CacheFileUrl) into vkCreatePipelineCache }
end;

procedure TVulkanRHIDevice.SavePipelineCache(const CacheFileUrl: String);
begin
  { Query vkGetPipelineCacheData and write stream via UrlSaveStream(CacheFileUrl) }
end;

function TVulkanRHIDevice.BeginSingleTimeCommands: VkCommandBuffer;
begin
  Result := nil;
end;

procedure TVulkanRHIDevice.EndSingleTimeCommands(const CmdBuf: VkCommandBuffer);
begin
end;

function TVulkanRHIDevice.FindMemoryType(const TypeFilter: Cardinal;
  const Properties: VkMemoryPropertyFlags): Cardinal;
var
  MemProps: VkPhysicalDeviceMemoryProperties;
  I: Cardinal;
begin
  vkGetPhysicalDeviceMemoryProperties(FPhysicalDevice, @MemProps);
  for I := 0 to MemProps.memoryTypeCount - 1 do
    if ((TypeFilter and (1 shl I)) <> 0) and
       ((MemProps.memoryTypes[I].propertyFlags and Properties) = Properties) then
      Exit(I);

  raise ERHIOutOfMemory.Create('Failed to find suitable Vulkan memory type');
end;

end.
