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

{ Vulkan GPU memory sub-allocator and per-frame ring buffer for Castle Game Engine.
  Avoids hitting maxMemoryAllocationCount limits by sub-allocating buffers and
  textures from pooled VkDeviceMemory blocks with automatic free-block coalescing. }
unit CastleInternalVulkanMemory;

{$I castleconf.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, SyncObjs,
  CastleLog, CastleInternalRHI, CastleInternalVulkan;

type
  TCastleVulkanMemoryBlock = class;
  TCastleVulkanMemoryAllocator = class;

  { Represents a single sub-allocation (or dedicated allocation) of Vulkan memory. }
  TVulkanMemoryAllocation = record
    { Underlying VkDeviceMemory handle (owned by Block, or standalone if IsDedicated) }
    DeviceMemory: VkDeviceMemory;
    { Byte offset within DeviceMemory (must be passed to vkBindBufferMemory / vkBindImageMemory) }
    Offset: VkDeviceSize;
    { Usable byte size of this allocation }
    Size: VkDeviceSize;
    { Non-nil if allocated from a host-visible, persistently mapped block }
    MappedPtr: Pointer;
    { Owning pool block (nil if IsDedicated = True) }
    Block: TCastleVulkanMemoryBlock;
    { Internal chunk ID for fast free-list lookup }
    ChunkId: Cardinal;
    { True if this allocation bypassed the pool and owns its own VkDeviceMemory }
    IsDedicated: Boolean;
    function IsValid: Boolean;
  end;

  { Fast per-frame bump/ring allocation result for transient UBOs & dynamic vertices }
  TVulkanTransientAllocation = record
    Buffer: VkBuffer;
    Offset: VkDeviceSize;
    Size: VkDeviceSize;
    MappedPtr: Pointer;
  end;

  { Statistics for CGE Profiler / Viewport.Statistics }
  TVulkanMemoryStats = record
    TotalBlockCount: Cardinal;
    DedicatedAllocationCount: Cardinal;
    TotalAllocatedBytes: UInt64;
    TotalUsedBytes: UInt64;
  end;

  { Internal linked-list node tracking used and free sub-ranges inside a block }
  PVulkanSubAllocationNode = ^TVulkanSubAllocationNode;
  TVulkanSubAllocationNode = record
    ChunkId: Cardinal;
    Offset: VkDeviceSize;
    Size: VkDeviceSize;
    IsFree: Boolean;
    IsBufferImageGranularityTainted: Boolean;
    Prev: PVulkanSubAllocationNode;
    Next: PVulkanSubAllocationNode;
  end;

  { A single large VkDeviceMemory allocation (e.g. 64 MB) subdivided into chunks. }
  TCastleVulkanMemoryBlock = class
  private
    FDevice: VkDevice;
    FMemoryTypeIndex: Cardinal;
    FDeviceMemory: VkDeviceMemory;
    FSize: VkDeviceSize;
    FUsedBytes: VkDeviceSize;
    FBaseMappedPtr: PByte;
    FHead: PVulkanSubAllocationNode;
    FNextChunkId: Cardinal;
  public
    constructor Create(const ADevice: VkDevice; const AMemoryTypeIndex: Cardinal;
      const ABlockSize: VkDeviceSize; const AHostVisible: Boolean);
    destructor Destroy; override;

    { Attempts to sub-allocate Size bytes aligned to Alignment (and respecting
      bufferImageGranularity). Returns True on success. }
    function TrySubAllocate(const ASize, AAlignment, AGranularity: VkDeviceSize;
      const AIsLinearResource: Boolean; out AAlloc: TVulkanMemoryAllocation): Boolean;

    { Frees a previously sub-allocated chunk and coalesces adjacent free nodes. }
    procedure FreeSubAllocation(const AAlloc: TVulkanMemoryAllocation);

    function IsEmpty: Boolean;

    property DeviceMemory: VkDeviceMemory read FDeviceMemory;
    property MemoryTypeIndex: Cardinal read FMemoryTypeIndex;
    property Size: VkDeviceSize read FSize;
    property UsedBytes: VkDeviceSize read FUsedBytes;
  end;

  TCastleVulkanBlockList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleVulkanMemoryBlock>;

  { Fast lock-free bump allocator for per-frame dynamic Uniform Buffers and 2D/UI vertices. }
  TCastleVulkanFrameRingBuffer = class
  private
    FAllocator: TCastleVulkanMemoryAllocator;
    FDevice: VkDevice;
    FBuffer: VkBuffer;
    FAllocation: TVulkanMemoryAllocation;
    FCapacity: VkDeviceSize;
    FCurrentOffset: VkDeviceSize;
    FMinAlignment: VkDeviceSize;
  public
    constructor Create(const AAllocator: TCastleVulkanMemoryAllocator;
      const ACapacity: VkDeviceSize; const AMinAlignment: VkDeviceSize);
    destructor Destroy; override;

    { Resets the write cursor to 0 at the start of a frame-in-flight (after fence wait). }
    procedure Reset;

    { Allocates aligned transient bytes from the ring buffer. }
    function Allocate(const ASize: VkDeviceSize; const AAlignment: VkDeviceSize = 256): TVulkanTransientAllocation;

    property Buffer: VkBuffer read FBuffer;
    property Capacity: VkDeviceSize read FCapacity;
    property UsedBytes: VkDeviceSize read FCurrentOffset;
  end;

  { Central GPU memory manager owned by TVulkanRHIDevice. }
  TCastleVulkanMemoryAllocator = class
  private
    FPhysicalDevice: VkPhysicalDevice;
    FDevice: VkDevice;
    FMemProperties: VkPhysicalDeviceMemoryProperties;
    FBufferImageGranularity: VkDeviceSize;
    FMinUniformBufferAlignment: VkDeviceSize;
    FLock: TCriticalSection;

    { Separate block lists per Vulkan memoryTypeIndex }
    FPools: array[0 .. VK_MAX_MEMORY_TYPES - 1] of TCastleVulkanBlockList;

    { Per-frame transient ring buffers (1 per frame-in-flight) }
    FFrameRingBuffers: array[0 .. MaxFramesInFlight - 1] of TCastleVulkanFrameRingBuffer;

    FDedicatedCount: Cardinal;
    FDedicatedBytes: UInt64;

    function SelectMemoryType(const ATypeBits: UInt32;
      const AUsage: TRHIMemoryUsage; out AIsHostVisible: Boolean): Cardinal;
    function GetPreferredBlockSize(const AUsage: TRHIMemoryUsage): VkDeviceSize;
  public
    constructor Create(const APhysicalDevice: VkPhysicalDevice; const ADevice: VkDevice);
    destructor Destroy; override;

    { Sub-allocates memory matching VkMemoryRequirements and TRHIMemoryUsage. }
    function Allocate(const AMemReq: VkMemoryRequirements;
      const AUsage: TRHIMemoryUsage;
      const AIsLinearResource: Boolean = True): TVulkanMemoryAllocation;

    { Frees a sub-allocation or dedicated allocation. }
    procedure Free(var AAlloc: TVulkanMemoryAllocation);

    { Returns the transient ring buffer for the given frame-in-flight index. }
    function GetFrameRingBuffer(const AFrameIndex: Cardinal): TCastleVulkanFrameRingBuffer;

    { Releases completely empty blocks back to the Vulkan driver. }
    procedure TrimEmptyBlocks;

    { Retrieves live memory budget and utilization statistics. }
    function GetStats: TVulkanMemoryStats;

    property Device: VkDevice read FDevice;
    property MinUniformBufferAlignment: VkDeviceSize read FMinUniformBufferAlignment;
  end;

implementation

const
  { Default pool block sizes }
  GPU_ONLY_BLOCK_SIZE   = 64 * 1024 * 1024; // 64 MB per block for static VRAM
  HOST_VISIBLE_BLOCK_SIZE = 16 * 1024 * 1024; // 16 MB per block for CPU-visible RAM
  FRAME_RING_BUFFER_SIZE  = 4 * 1024 * 1024;  // 4 MB per frame-in-flight ring buffer

function AlignUp(const Value, Alignment: VkDeviceSize): VkDeviceSize; inline;
begin
  if Alignment <= 1 then
    Result := Value
  else
    Result := (Value + (Alignment - 1)) and not (Alignment - 1);
end;

{ ---------------------------------------------------------------------------- }
{ TVulkanMemoryAllocation                                                      }
{ ---------------------------------------------------------------------------- }

function TVulkanMemoryAllocation.IsValid: Boolean;
begin
  Result := DeviceMemory <> VK_NULL_HANDLE;
end;

{ ---------------------------------------------------------------------------- }
{ TCastleVulkanMemoryBlock                                                     }
{ ---------------------------------------------------------------------------- }

constructor TCastleVulkanMemoryBlock.Create(const ADevice: VkDevice;
  const AMemoryTypeIndex: Cardinal; const ABlockSize: VkDeviceSize;
  const AHostVisible: Boolean);
var
  AllocInfo: VkMemoryAllocateInfo;
  Res: VkResult;
begin
  inherited Create;
  FDevice := ADevice;
  FMemoryTypeIndex := AMemoryTypeIndex;
  FSize := ABlockSize;
  FUsedBytes := 0;
  FNextChunkId := 1;

  FillChar(AllocInfo, SizeOf(AllocInfo), 0);
  AllocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
  AllocInfo.allocationSize := ABlockSize;
  AllocInfo.memoryTypeIndex := AMemoryTypeIndex;

  Res := vkAllocateMemory(FDevice, @AllocInfo, nil, @FDeviceMemory);
  if Res <> VK_SUCCESS then
    raise ERHIOutOfMemory.CreateFmt(
      'Vulkan out of device memory allocating %d MB block (VkResult: %d)',
      [ABlockSize div (1024 * 1024), Ord(Res)]);

  { Persistently map host-visible blocks once for the entire lifetime of the block }
  if AHostVisible then
  begin
    Res := vkMapMemory(FDevice, FDeviceMemory, 0, ABlockSize, 0, @FBaseMappedPtr);
    if Res <> VK_SUCCESS then
    begin
      vkFreeMemory(FDevice, FDeviceMemory, nil);
      raise ERHIOutOfMemory.CreateFmt('vkMapMemory failed for block (VkResult: %d)', [Ord(Res)]);
    end;
  end;

  { Initialize linked list with a single free node spanning the entire block }
  New(FHead);
  FHead^.ChunkId := 0;
  FHead^.Offset := 0;
  FHead^.Size := ABlockSize;
  FHead^.IsFree := True;
  FHead^.IsBufferImageGranularityTainted := False;
  FHead^.Prev := nil;
  FHead^.Next := nil;
end;

destructor TCastleVulkanMemoryBlock.Destroy;
var
  Curr, NextNode: PVulkanSubAllocationNode;
begin
  Curr := FHead;
  while Curr <> nil do
  begin
    NextNode := Curr^.Next;
    Dispose(Curr);
    Curr := NextNode;
  end;

  if FBaseMappedPtr <> nil then
    vkUnmapMemory(FDevice, FDeviceMemory);
  if FDeviceMemory <> VK_NULL_HANDLE then
    vkFreeMemory(FDevice, FDeviceMemory, nil);

  inherited;
end;

function TCastleVulkanMemoryBlock.IsEmpty: Boolean;
begin
  Result := (FHead <> nil) and FHead^.IsFree and (FHead^.Next = nil);
end;

function TCastleVulkanMemoryBlock.TrySubAllocate(
  const ASize, AAlignment, AGranularity: VkDeviceSize;
  const AIsLinearResource: Boolean; out AAlloc: TVulkanMemoryAllocation): Boolean;
var
  Curr, SplitStartNode, SplitEndNode: PVulkanSubAllocationNode;
  AlignedOffset, PaddingFront, PaddingBack, EffectiveAlignment: VkDeviceSize;
begin
  Result := False;
  if FSize - FUsedBytes < ASize then Exit;

  { Enforce bufferImageGranularity when linear buffers and optimal tiled images share a block }
  EffectiveAlignment := AAlignment;
  if ( not AIsLinearResource ) and (AGranularity > EffectiveAlignment) then
    EffectiveAlignment := AGranularity;

  Curr := FHead;
  while Curr <> nil do
  begin
    if Curr^.IsFree and (Curr^.Size >= ASize) then
    begin
      AlignedOffset := AlignUp(Curr^.Offset, EffectiveAlignment);

      { Also align up to granularity if the previous neighbor had a different linearity }
      if (Curr^.Prev <> nil) and (Curr^.Prev^.IsBufferImageGranularityTainted <> (not AIsLinearResource)) then
        AlignedOffset := AlignUp(AlignedOffset, AGranularity);

      PaddingFront := AlignedOffset - Curr^.Offset;

      if Curr^.Size >= PaddingFront + ASize then
      begin
        PaddingBack := Curr^.Size - (PaddingFront + ASize);

        { 1. If there is alignment padding at the front, split off a small free node before Curr }
        if PaddingFront > 0 then
        begin
          New(SplitStartNode);
          SplitStartNode^.ChunkId := 0;
          SplitStartNode^.Offset := Curr^.Offset;
          SplitStartNode^.Size := PaddingFront;
          SplitStartNode^.IsFree := True;
          SplitStartNode^.IsBufferImageGranularityTainted := False;
          SplitStartNode^.Prev := Curr^.Prev;
          SplitStartNode^.Next := Curr;

          if Curr^.Prev <> nil then
            Curr^.Prev^.Next := SplitStartNode
          else
            FHead := SplitStartNode;

          Curr^.Prev := SplitStartNode;
          Curr^.Offset := AlignedOffset;
          Curr^.Size := Curr^.Size - PaddingFront;
        end;

        { 2. If there is remaining space at the back, split off a trailing free node }
        if PaddingBack > 0 then
        begin
          New(SplitEndNode);
          SplitEndNode^.ChunkId := 0;
          SplitEndNode^.Offset := AlignedOffset + ASize;
          SplitEndNode^.Size := PaddingBack;
          SplitEndNode^.IsFree := True;
          SplitEndNode^.IsBufferImageGranularityTainted := False;
          SplitEndNode^.Prev := Curr;
          SplitEndNode^.Next := Curr^.Next;

          if Curr^.Next <> nil then
            Curr^.Next^.Prev := SplitEndNode;
          Curr^.Next := SplitEndNode;
          Curr^.Size := ASize;
        end;

        { 3. Mark Curr as allocated }
        Curr^.IsFree := False;
        Curr^.IsBufferImageGranularityTainted := not AIsLinearResource;
        Curr^.ChunkId := FNextChunkId;
        Inc(FNextChunkId);

        Inc(FUsedBytes, ASize);

        FillChar(AAlloc, SizeOf(AAlloc), 0);
        AAlloc.DeviceMemory := FDeviceMemory;
        AAlloc.Offset := Curr^.Offset;
        AAlloc.Size := Curr^.Size;
        AAlloc.Block := Self;
        AAlloc.ChunkId := Curr^.ChunkId;
        AAlloc.IsDedicated := False;
        if FBaseMappedPtr <> nil then
          AAlloc.MappedPtr := FBaseMappedPtr + Curr^.Offset;

        Exit(True);
      end;
    end;
    Curr := Curr^.Next;
  end;
end;

procedure TCastleVulkanMemoryBlock.FreeSubAllocation(const AAlloc: TVulkanMemoryAllocation);
var
  Curr, Neighbor: PVulkanSubAllocationNode;
begin
  Curr := FHead;
  while Curr <> nil do
  begin
    if (not Curr^.IsFree) and (Curr^.ChunkId = AAlloc.ChunkId) and (Curr^.Offset = AAlloc.Offset) then
    begin
      Curr^.IsFree := True;
      Curr^.ChunkId := 0;
      Dec(FUsedBytes, Curr^.Size);

      { Coalesce with next node if free }
      Neighbor := Curr^.Next;
      if (Neighbor <> nil) and Neighbor^.IsFree then
      begin
        Curr^.Size := Curr^.Size + Neighbor^.Size;
        Curr^.Next := Neighbor^.Next;
        if Curr^.Next <> nil then
          Curr^.Next^.Prev := Curr;
        Dispose(Neighbor);
      end;

      { Coalesce with previous node if free }
      Neighbor := Curr^.Prev;
      if (Neighbor <> nil) and Neighbor^.IsFree then
      begin
        Neighbor^.Size := Neighbor^.Size + Curr^.Size;
        Neighbor^.Next := Curr^.Next;
        if Neighbor^.Next <> nil then
          Neighbor^.Next^.Prev := Neighbor;
        Dispose(Curr);
      end;

      Exit;
    end;
    Curr := Curr^.Next;
  end;

  WritelnWarning('VulkanMemory', 'Attempted to free unknown sub-allocation chunk %d at offset %d',
    [AAlloc.ChunkId, AAlloc.Offset]);
end;

{ ---------------------------------------------------------------------------- }
{ TCastleVulkanFrameRingBuffer                                                 }
{ ---------------------------------------------------------------------------- }

constructor TCastleVulkanFrameRingBuffer.Create(
  const AAllocator: TCastleVulkanMemoryAllocator;
  const ACapacity, AMinAlignment: VkDeviceSize);
var
  BufInfo: VkBufferCreateInfo;
  MemReq: VkMemoryRequirements;
begin
  inherited Create;
  FAllocator := AAllocator;
  FDevice := AAllocator.Device;
  FCapacity := ACapacity;
  FMinAlignment := AMinAlignment;
  FCurrentOffset := 0;

  FillChar(BufInfo, SizeOf(BufInfo), 0);
  BufInfo.sType := VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO;
  BufInfo.size := ACapacity;
  BufInfo.usage :=
    VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT or
    VK_BUFFER_USAGE_VERTEX_BUFFER_BIT or
    VK_BUFFER_USAGE_INDEX_BUFFER_BIT or
    VK_BUFFER_USAGE_TRANSFER_SRC_BIT;
  BufInfo.sharingMode := VK_SHARING_MODE_EXCLUSIVE;

  if vkCreateBuffer(FDevice, @BufInfo, nil, @FBuffer) <> VK_SUCCESS then
    raise ERHIOutOfMemory.Create('Failed to create Vulkan per-frame ring buffer');

  vkGetBufferMemoryRequirements(FDevice, FBuffer, @MemReq);
  FAllocation := FAllocator.Allocate(MemReq, muCpuToGpu, True);
  vkBindBufferMemory(FDevice, FBuffer, FAllocation.DeviceMemory, FAllocation.Offset);
end;

destructor TCastleVulkanFrameRingBuffer.Destroy;
begin
  if FBuffer <> VK_NULL_HANDLE then
    vkDestroyBuffer(FDevice, FBuffer, nil);
  if FAllocation.IsValid then
    FAllocator.Free(FAllocation);
  inherited;
end;

procedure TCastleVulkanFrameRingBuffer.Reset;
begin
  FCurrentOffset := 0;
end;

function TCastleVulkanFrameRingBuffer.Allocate(
  const ASize, AAlignment: VkDeviceSize): TVulkanTransientAllocation;
var
  EffAlign, AlignedOffset: VkDeviceSize;
begin
  EffAlign := AAlignment;
  if FMinAlignment > EffAlign then
    EffAlign := FMinAlignment;

  AlignedOffset := AlignUp(FCurrentOffset, EffAlign);
  if AlignedOffset + ASize > FCapacity then
    raise ERHIOutOfMemory.CreateFmt(
      'Vulkan per-frame ring buffer exhausted (%d / %d bytes requested). Increase FRAME_RING_BUFFER_SIZE.',
      [AlignedOffset + ASize, FCapacity]);

  Result.Buffer := FBuffer;
  Result.Offset := AlignedOffset;
  Result.Size := ASize;
  Result.MappedPtr := PByte(FAllocation.MappedPtr) + AlignedOffset;

  FCurrentOffset := AlignedOffset + ASize;
end;

{ ---------------------------------------------------------------------------- }
{ TCastleVulkanMemoryAllocator                                                 }
{ ---------------------------------------------------------------------------- }

constructor TCastleVulkanMemoryAllocator.Create(
  const APhysicalDevice: VkPhysicalDevice; const ADevice: VkDevice);
var
  DevProps: VkPhysicalDeviceProperties;
  I: Integer;
begin
  inherited Create;
  FPhysicalDevice := APhysicalDevice;
  FDevice := ADevice;
  FLock := TCriticalSection.Create;

  vkGetPhysicalDeviceMemoryProperties(FPhysicalDevice, @FMemProperties);
  vkGetPhysicalDeviceProperties(FPhysicalDevice, @DevProps);

  FBufferImageGranularity := DevProps.limits.bufferImageGranularity;
  FMinUniformBufferAlignment := DevProps.limits.minUniformBufferOffsetAlignment;

  for I := 0 to VK_MAX_MEMORY_TYPES - 1 do
    FPools[I] := TCastleVulkanBlockList.Create(True);

  for I := 0 to MaxFramesInFlight - 1 do
    FFrameRingBuffers[I] := TCastleVulkanFrameRingBuffer.Create(
      Self, FRAME_RING_BUFFER_SIZE, FMinUniformBufferAlignment);
end;

destructor TCastleVulkanMemoryAllocator.Destroy;
var
  I: Integer;
begin
  for I := 0 to MaxFramesInFlight - 1 do
    FreeAndNil(FFrameRingBuffers[I]);

  for I := 0 to VK_MAX_MEMORY_TYPES - 1 do
    FreeAndNil(FPools[I]);

  FreeAndNil(FLock);
  inherited;
end;

function TCastleVulkanMemoryAllocator.SelectMemoryType(const ATypeBits: UInt32;
  const AUsage: TRHIMemoryUsage; out AIsHostVisible: Boolean): Cardinal;

  function TryFindWithFlags(const Required, Preferred: VkMemoryPropertyFlags;
    out FoundIdx: Cardinal): Boolean;
  var
    I: Cardinal;
    Flags: VkMemoryPropertyFlags;
    BestScore, Score: Integer;
  begin
    Result := False;
    BestScore := -1;
    FoundIdx := 0;

    for I := 0 to FMemProperties.memoryTypeCount - 1 do
      if (ATypeBits and (1 shl I)) <> 0 then
      begin
        Flags := FMemProperties.memoryTypes[I].propertyFlags;
        if (Flags and Required) = Required then
        begin
          Score := 0;
          if (Flags and Preferred) = Preferred then
            Score := 10;
          if Score > BestScore then
          begin
            BestScore := Score;
            FoundIdx := I;
            Result := True;
          end;
        end;
      end;
  end;

var
  RequiredFlags, PreferredFlags: VkMemoryPropertyFlags;
begin
  case AUsage of
    muGpuOnly:
      begin
        RequiredFlags := VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        PreferredFlags := VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        AIsHostVisible := False;
      end;
    muCpuToGpu:
      begin
        RequiredFlags := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
        { Prefer Resizable BAR / ReBAR (DEVICE_LOCAL + HOST_VISIBLE) when available }
        PreferredFlags := RequiredFlags or VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT;
        AIsHostVisible := True;
      end;
    muGpuToCpu:
      begin
        RequiredFlags := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT;
        PreferredFlags := VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT or VK_MEMORY_PROPERTY_HOST_CACHED_BIT;
        AIsHostVisible := True;
      end;
  end;

  if not TryFindWithFlags(RequiredFlags, PreferredFlags, Result) then
    if not TryFindWithFlags(0, 0, Result) then
      raise ERHIOutOfMemory.Create('Could not find compatible Vulkan memory type');
end;

function TCastleVulkanMemoryAllocator.GetPreferredBlockSize(
  const AUsage: TRHIMemoryUsage): VkDeviceSize;
begin
  if AUsage = muGpuOnly then
    Result := GPU_ONLY_BLOCK_SIZE
  else
    Result := HOST_VISIBLE_BLOCK_SIZE;
end;

function TCastleVulkanMemoryAllocator.Allocate(
  const AMemReq: VkMemoryRequirements; const AUsage: TRHIMemoryUsage;
  const AIsLinearResource: Boolean): TVulkanMemoryAllocation;
var
  MemTypeIdx: Cardinal;
  IsHostVisible: Boolean;
  PrefBlockSize: VkDeviceSize;
  Pool: TCastleVulkanBlockList;
  Block, NewBlock: TCastleVulkanMemoryBlock;
  AllocInfo: VkMemoryAllocateInfo;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  MemTypeIdx := SelectMemoryType(AMemReq.memoryTypeBits, AUsage, IsHostVisible);
  PrefBlockSize := GetPreferredBlockSize(AUsage);

  FLock.Acquire;
  try
    { 1. Dedicated Allocation Path: If the resource is >= 50% of a pool block
         (e.g. a 4K/8K render target or huge terrain buffer), allocate its own VkDeviceMemory }
    if AMemReq.size >= (PrefBlockSize div 2) then
    begin
      FillChar(AllocInfo, SizeOf(AllocInfo), 0);
      AllocInfo.sType := VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO;
      AllocInfo.allocationSize := AMemReq.size;
      AllocInfo.memoryTypeIndex := MemTypeIdx;

      if vkAllocateMemory(FDevice, @AllocInfo, nil, @Result.DeviceMemory) <> VK_SUCCESS then
        raise ERHIOutOfMemory.CreateFmt('Failed dedicated Vulkan allocation of %d bytes', [AMemReq.size]);

      Result.Offset := 0;
      Result.Size := AMemReq.size;
      Result.IsDedicated := True;
      if IsHostVisible then
        vkMapMemory(FDevice, Result.DeviceMemory, 0, AMemReq.size, 0, @Result.MappedPtr);

      Inc(FDedicatedCount);
      Inc(FDedicatedBytes, AMemReq.size);
      Exit;
    end;

    { 2. Pooled Sub-Allocation Path: Try existing blocks for this memoryTypeIndex }
    Pool := FPools[MemTypeIdx];
    for I := 0 to Pool.Count - 1 do
    begin
      Block := Pool[I];
      if Block.TrySubAllocate(AMemReq.size, AMemReq.alignment,
           FBufferImageGranularity, AIsLinearResource, Result) then
        Exit;
    end;

    { 3. Allocate a new block and sub-allocate from it }
    NewBlock := TCastleVulkanMemoryBlock.Create(
      FDevice, MemTypeIdx, PrefBlockSize, IsHostVisible);
    Pool.Add(NewBlock);

    if not NewBlock.TrySubAllocate(AMemReq.size, AMemReq.alignment,
         FBufferImageGranularity, AIsLinearResource, Result) then
      raise ERHIOutOfMemory.Create('Sub-allocation failed on freshly created memory block');
  finally
    FLock.Release;
  end;
end;

procedure TCastleVulkanMemoryAllocator.Free(var AAlloc: TVulkanMemoryAllocation);
begin
  if not AAlloc.IsValid then Exit;

  FLock.Acquire;
  try
    if AAlloc.IsDedicated then
    begin
      if AAlloc.MappedPtr <> nil then
        vkUnmapMemory(FDevice, AAlloc.DeviceMemory);
      vkFreeMemory(FDevice, AAlloc.DeviceMemory, nil);
      Dec(FDedicatedCount);
      Dec(FDedicatedBytes, AAlloc.Size);
    end else
    if AAlloc.Block <> nil then
      AAlloc.Block.FreeSubAllocation(AAlloc);

    FillChar(AAlloc, SizeOf(AAlloc), 0);
  finally
    FLock.Release;
  end;
end;

function TCastleVulkanMemoryAllocator.GetFrameRingBuffer(
  const AFrameIndex: Cardinal): TCastleVulkanFrameRingBuffer;
begin
  Result := FFrameRingBuffers[AFrameIndex mod MaxFramesInFlight];
end;

procedure TCastleVulkanMemoryAllocator.TrimEmptyBlocks;
var
  I, J: Integer;
  Pool: TCastleVulkanBlockList;
begin
  FLock.Acquire;
  try
    for I := 0 to VK_MAX_MEMORY_TYPES - 1 do
    begin
      Pool := FPools[I];
      { Keep at least 1 block per active pool to avoid thrashing vkAllocateMemory }
      for J := Pool.Count - 1 downto 1 do
        if Pool[J].IsEmpty then
          Pool.Delete(J);
    end;
  finally
    FLock.Release;
  end;
end;

function TCastleVulkanMemoryAllocator.GetStats: TVulkanMemoryStats;
var
  I, J: Integer;
  Pool: TCastleVulkanBlockList;
begin
  FillChar(Result, SizeOf(Result), 0);
  FLock.Acquire;
  try
    Result.DedicatedAllocationCount := FDedicatedCount;
    Result.TotalAllocatedBytes := FDedicatedBytes;
    Result.TotalUsedBytes := FDedicatedBytes;

    for I := 0 to VK_MAX_MEMORY_TYPES - 1 do
    begin
      Pool := FPools[I];
      Inc(Result.TotalBlockCount, Pool.Count);
      for J := 0 to Pool.Count - 1 do
      begin
        Inc(Result.TotalAllocatedBytes, Pool[J].Size);
        Inc(Result.TotalUsedBytes, Pool[J].UsedBytes);
      end;
    end;
  finally
    FLock.Release;
  end;
end;

end.
