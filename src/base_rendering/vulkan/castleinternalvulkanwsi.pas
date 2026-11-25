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

{ Platform-specific Vulkan Window System Integration (WSI) bindings and
  backend fallback selection for Windows (Win32), Linux (Xlib / Wayland),
  Android (ANativeWindow), macOS / iOS (Metal / MoltenVK), and
  Nintendo Switch (VK_NN_vi_surface). }
unit CastleInternalVulkanWSI;

{$I castleconf.inc}

interface

uses
  SysUtils,
  {$if defined(MSWINDOWS)} Windows, {$endif}
  {$if defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)} X, Xlib, {$endif}
  CastleDynLib, CastleApplicationProperties, CastleInternalRHI, CastleInternalVulkan;

const
  VK_KHR_SURFACE_EXTENSION_NAME                 = 'VK_KHR_surface';
  VK_KHR_WIN32_SURFACE_EXTENSION_NAME           = 'VK_KHR_win32_surface';
  VK_KHR_XLIB_SURFACE_EXTENSION_NAME            = 'VK_KHR_xlib_surface';
  VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME         = 'VK_KHR_wayland_surface';
  VK_KHR_ANDROID_SURFACE_EXTENSION_NAME         = 'VK_KHR_android_surface';
  VK_EXT_METAL_SURFACE_EXTENSION_NAME           = 'VK_EXT_metal_surface';
  VK_NN_VI_SURFACE_EXTENSION_NAME               = 'VK_NN_vi_surface';
  VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME        = 'VK_EXT_headless_surface';
  VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME = 'VK_KHR_portability_enumeration';

  VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR    = 1000009000;
  VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR     = 1000004000;
  VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR  = 1000006000;
  VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR  = 1000008000;
  VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN        = 1000062000;
  VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT    = 1000217000;
  VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT = 1000256000;

  VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR   = $00000001;

  VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR  = $00000002;
  VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = $00000004;
  VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR         = $00000008;

type
  TAnsiStringArray = array of AnsiString;

  { Vulkan Window System Integration (WSI) platform identifier }
  TVulkanWSIPlatform = (
    vwsiAuto,
    vwsiWin32,
    vwsiXlib,
    vwsiWayland,
    vwsiAndroid,
    vwsiMetal,
    vwsiViSwitch,
    vwsiHeadless
  );

  { Win32 WSI (VK_KHR_win32_surface) }
  {$if defined(MSWINDOWS)}
  VkWin32HINSTANCE = HINST;
  VkWin32HWND = HWND;
  {$else}
  VkWin32HINSTANCE = PtrUInt;
  VkWin32HWND = PtrUInt;
  {$endif}
  VkWin32SurfaceCreateFlagsKHR = VkFlags;
  PVkWin32SurfaceCreateInfoKHR = ^VkWin32SurfaceCreateInfoKHR;
  VkWin32SurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkWin32SurfaceCreateFlagsKHR;
    hinstance: VkWin32HINSTANCE;
    hwnd: VkWin32HWND;
  end;

  PFN_vkCreateWin32SurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkWin32SurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { X11 / Xlib WSI (VK_KHR_xlib_surface) }
  {$if defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)}
  PVkXlibDisplay = PDisplay;
  VkXlibWindow = TWindow;
  {$else}
  PVkXlibDisplay = Pointer;
  VkXlibWindow = PtrUInt;
  {$endif}
  VkXlibSurfaceCreateFlagsKHR = VkFlags;
  PVkXlibSurfaceCreateInfoKHR = ^VkXlibSurfaceCreateInfoKHR;
  VkXlibSurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkXlibSurfaceCreateFlagsKHR;
    dpy: PVkXlibDisplay;
    window: VkXlibWindow;
  end;

  PFN_vkCreateXlibSurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkXlibSurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { Wayland WSI (VK_KHR_wayland_surface for GTK3 / native Wayland backends) }
  Pwl_display = Pointer;
  Pwl_surface = Pointer;
  VkWaylandSurfaceCreateFlagsKHR = VkFlags;
  PVkWaylandSurfaceCreateInfoKHR = ^VkWaylandSurfaceCreateInfoKHR;
  VkWaylandSurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkWaylandSurfaceCreateFlagsKHR;
    display: Pwl_display;
    surface: Pwl_surface;
  end;

  PFN_vkCreateWaylandSurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkWaylandSurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { Android WSI (ANativeWindow / VK_KHR_android_surface) }
  PANativeWindow = Pointer;
  VkAndroidSurfaceCreateFlagsKHR = VkFlags;
  PVkAndroidSurfaceCreateInfoKHR = ^VkAndroidSurfaceCreateInfoKHR;
  VkAndroidSurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkAndroidSurfaceCreateFlagsKHR;
    window: PANativeWindow;
  end;

  PFN_vkCreateAndroidSurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkAndroidSurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { macOS / iOS Metal WSI (CAMetalLayer via MoltenVK / VK_EXT_metal_surface) }
  PCAMetalLayer = Pointer;
  VkMetalSurfaceCreateFlagsEXT = VkFlags;
  PVkMetalSurfaceCreateInfoEXT = ^VkMetalSurfaceCreateInfoEXT;
  VkMetalSurfaceCreateInfoEXT = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkMetalSurfaceCreateFlagsEXT;
    pLayer: PCAMetalLayer;
  end;

  PFN_vkCreateMetalSurfaceEXT = function(
    instance: VkInstance;
    const pCreateInfo: PVkMetalSurfaceCreateInfoEXT;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { Nintendo Switch WSI (nn::vi::Layer* / VK_NN_vi_surface) }
  PViWindowHandleNN = Pointer;
  VkViSurfaceCreateFlagsNN = VkFlags;
  PVkViSurfaceCreateInfoNN = ^VkViSurfaceCreateInfoNN;
  VkViSurfaceCreateInfoNN = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkViSurfaceCreateFlagsNN;
    window: PViWindowHandleNN;
  end;

  PFN_vkCreateViSurfaceNN = function(
    instance: VkInstance;
    const pCreateInfo: PVkViSurfaceCreateInfoNN;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { Headless WSI (VK_EXT_headless_surface) }
  VkHeadlessSurfaceCreateFlagsEXT = VkFlags;
  PVkHeadlessSurfaceCreateInfoEXT = ^VkHeadlessSurfaceCreateInfoEXT;
  VkHeadlessSurfaceCreateInfoEXT = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkHeadlessSurfaceCreateFlagsEXT;
  end;

  PFN_vkCreateHeadlessSurfaceEXT = function(
    instance: VkInstance;
    const pCreateInfo: PVkHeadlessSurfaceCreateInfoEXT;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

  { Unified cross-platform native window surface descriptor populated by TCastleWindow }
  TVulkanNativeSurfaceDesc = record
    Platform: TVulkanWSIPlatform;
    Win32HInstance: VkWin32HINSTANCE;
    Win32WindowHandle: VkWin32HWND;
    XlibDisplay: PVkXlibDisplay;
    XlibWindow: VkXlibWindow;
    WaylandDisplay: Pwl_display;
    WaylandSurface: Pwl_surface;
    AndroidNativeWindow: PANativeWindow;
    MetalLayer: PCAMetalLayer;
    ViWindowHandle: PViWindowHandleNN;
    class function CreateWin32(const AHInstance: VkWin32HINSTANCE;
      const AWindowHandle: VkWin32HWND): TVulkanNativeSurfaceDesc; static;
    class function CreateXlib(const ADisplay: PVkXlibDisplay;
      const AWindow: VkXlibWindow): TVulkanNativeSurfaceDesc; static;
    class function CreateWayland(const AWlDisplay: Pwl_display;
      const AWlSurface: Pwl_surface): TVulkanNativeSurfaceDesc; static;
    class function CreateAndroid(const ANativeWindow: PANativeWindow): TVulkanNativeSurfaceDesc; static;
    class function CreateMetal(const AMetalLayer: PCAMetalLayer): TVulkanNativeSurfaceDesc; static;
    class function CreateViSwitch(const AViWindow: PViWindowHandleNN): TVulkanNativeSurfaceDesc; static;
    class function CreateHeadless: TVulkanNativeSurfaceDesc; static;
  end;

function CreateVulkanWin32Surface(const AInstance: VkInstance;
  const AHInstance: VkWin32HINSTANCE; const AWindowHandle: VkWin32HWND): VkSurfaceKHR;

function CreateVulkanXlibSurface(const AInstance: VkInstance;
  const ADisplay: PVkXlibDisplay; const AXWindow: VkXlibWindow): VkSurfaceKHR;

function CreateVulkanWaylandSurface(const AInstance: VkInstance;
  const AWlDisplay: Pwl_display; const AWlSurface: Pwl_surface): VkSurfaceKHR;

function CreateVulkanAndroidSurface(const AInstance: VkInstance;
  const ANativeWindow: PANativeWindow): VkSurfaceKHR;

function CreateVulkanMetalSurface(const AInstance: VkInstance;
  const AMetalLayer: PCAMetalLayer): VkSurfaceKHR;

function CreateVulkanViSurface(const AInstance: VkInstance;
  const AViWindow: PViWindowHandleNN): VkSurfaceKHR;

function CreateVulkanHeadlessSurface(const AInstance: VkInstance): VkSurfaceKHR;

{ Returns the default WSI platform for the compiled target OS }
function GetDefaultVulkanWSIPlatform: TVulkanWSIPlatform;

{ Returns the required VkInstance surface extensions for a specific WSI platform }
function GetRequiredVulkanInstanceExtensionsForPlatform(
  const APlatform: TVulkanWSIPlatform): TAnsiStringArray;

{ Returns True when the descriptor has non-null native handles for its WSI platform }
function ValidateVulkanNativeSurfaceDesc(const ADesc: TVulkanNativeSurfaceDesc): Boolean;

{ Dispatches VkSurfaceKHR creation to the appropriate OS WSI extension function
  (vkCreateWin32SurfaceKHR, vkCreateXlibSurfaceKHR, vkCreateWaylandSurfaceKHR,
  vkCreateAndroidSurfaceKHR, vkCreateMetalSurfaceEXT, vkCreateViSurfaceNN,
  or vkCreateHeadlessSurfaceEXT) }
function CreateVulkanSurfaceFromNativeDesc(const AInstance: VkInstance;
  const ADesc: TVulkanNativeSurfaceDesc): VkSurfaceKHR;

{ Returns the required VkInstance surface extensions for the current OS }
function GetRequiredVulkanInstanceExtensions: TAnsiStringArray;

{ Enumerates all VkInstance extension names supported by the active Vulkan loader }
function QuerySupportedVulkanInstanceExtensions: TAnsiStringArray;

{ Returns True when AExtensionName is advertised by vkEnumerateInstanceExtensionProperties }
function IsVulkanInstanceExtensionSupported(const AExtensionName: AnsiString): Boolean;

{ Enumerates all VkInstance layer names advertised by vkEnumerateInstanceLayerProperties }
function QuerySupportedVulkanInstanceLayers: TAnsiStringArray;

{ Returns True when ALayerName is advertised by vkEnumerateInstanceLayerProperties }
function IsVulkanInstanceLayerSupported(const ALayerName: AnsiString): Boolean;

{ Returns True when VK_LAYER_KHRONOS_validation is installed and advertised by the Vulkan loader }
function IsVulkanValidationLayerSupported: Boolean;

{ Filters known WSI instance extensions (VK_KHR_surface, Xlib, Wayland, Win32,
  Android, Metal, Switch Vi, VK_EXT_headless_surface, VK_KHR_portability_enumeration)
  against AAvailableExtensions, setting AEnablePortabilityBit when portability enumeration is included }
function BuildAvailableVulkanWSIInstanceExtensions(
  const AAvailableExtensions: array of AnsiString;
  out AEnablePortabilityBit: Boolean): TAnsiStringArray;

{ Queries the live Vulkan loader and returns all supported WSI instance extensions }
function GetAvailableVulkanWSIInstanceExtensions(
  out AEnablePortabilityBit: Boolean): TAnsiStringArray;

{ Attaches (or retrieves an existing) CAMetalLayer on a macOS Cocoa NSView pointer
  via the Objective-C runtime (non-variadic arm64/x86_64 ABI safe) and optionally
  updates its contentsScale for Retina HiDPI backing stores. Returns nil off-Darwin. }
function EnsureDarwinNSViewMetalLayer(const ANSViewPtr: Pointer;
  const AContentsScale: Double = 0.0): PCAMetalLayer;

{ Resolves the active render backend given hardware capabilities.
  Enforces the Vulkan 1.1+ minimum baseline and falls back to OpenGLES 3.0 on
  mobile/Android or OpenGL on desktop when Vulkan 1.1+ is unavailable. }
function SelectOptimalRenderBackend(const Requested: TRenderBackend;
  const VulkanAvailable: Boolean; const VulkanMajor: Cardinal = 1;
  const VulkanMinor: Cardinal = 3;
  const MobileGLESFallback: Boolean = {$if defined(ANDROID) or defined(CASTLE_IOS)}True{$else}False{$endif}): TRenderBackend;

implementation

class function TVulkanNativeSurfaceDesc.CreateWin32(
  const AHInstance: VkWin32HINSTANCE; const AWindowHandle: VkWin32HWND): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiWin32;
  Result.Win32HInstance := AHInstance;
  Result.Win32WindowHandle := AWindowHandle;
end;

class function TVulkanNativeSurfaceDesc.CreateXlib(
  const ADisplay: PVkXlibDisplay; const AWindow: VkXlibWindow): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiXlib;
  Result.XlibDisplay := ADisplay;
  Result.XlibWindow := AWindow;
end;

class function TVulkanNativeSurfaceDesc.CreateWayland(
  const AWlDisplay: Pwl_display; const AWlSurface: Pwl_surface): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiWayland;
  Result.WaylandDisplay := AWlDisplay;
  Result.WaylandSurface := AWlSurface;
end;

class function TVulkanNativeSurfaceDesc.CreateAndroid(
  const ANativeWindow: PANativeWindow): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiAndroid;
  Result.AndroidNativeWindow := ANativeWindow;
end;

class function TVulkanNativeSurfaceDesc.CreateMetal(
  const AMetalLayer: PCAMetalLayer): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiMetal;
  Result.MetalLayer := AMetalLayer;
end;

class function TVulkanNativeSurfaceDesc.CreateViSwitch(
  const AViWindow: PViWindowHandleNN): TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiViSwitch;
  Result.ViWindowHandle := AViWindow;
end;

class function TVulkanNativeSurfaceDesc.CreateHeadless: TVulkanNativeSurfaceDesc;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Platform := vwsiHeadless;
end;

function ResolveSurfaceProc(const AInstance: VkInstance; const AProcName: PAnsiChar): Pointer;
begin
  Result := nil;
  if Assigned(vkGetInstanceProcAddr) then
    Result := vkGetInstanceProcAddr(AInstance, AProcName);
  if Result = nil then
    raise ERHIError.CreateFmt('%s not found in Vulkan instance', [String(AProcName)]);
end;

procedure CheckSurfaceResult(const ARes: VkResult; const AProcName: String);
begin
  if ARes <> VK_SUCCESS then
    raise ERHIError.CreateFmt('%s failed (VkResult: %d)', [AProcName, Ord(ARes)]);
end;

function GetDefaultVulkanWSIPlatform: TVulkanWSIPlatform;
begin
  {$if defined(MSWINDOWS)}
  Result := vwsiWin32;
  {$elseif defined(ANDROID)}
  Result := vwsiAndroid;
  {$elseif defined(CASTLE_NINTENDO_SWITCH)}
  Result := vwsiViSwitch;
  {$elseif defined(DARWIN)}
  Result := vwsiMetal;
  {$elseif defined(UNIX)}
  Result := vwsiXlib;
  {$else}
  Result := vwsiHeadless;
  {$endif}
end;

function GetRequiredVulkanInstanceExtensionsForPlatform(
  const APlatform: TVulkanWSIPlatform): TAnsiStringArray;
var
  ResolvedPlatform: TVulkanWSIPlatform;
begin
  ResolvedPlatform := APlatform;
  if ResolvedPlatform = vwsiAuto then
    ResolvedPlatform := GetDefaultVulkanWSIPlatform;

  case ResolvedPlatform of
    vwsiWin32:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_WIN32_SURFACE_EXTENSION_NAME];
    vwsiXlib:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_XLIB_SURFACE_EXTENSION_NAME];
    vwsiWayland:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME];
    vwsiAndroid:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_ANDROID_SURFACE_EXTENSION_NAME];
    vwsiMetal:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_EXT_METAL_SURFACE_EXTENSION_NAME,
                 VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME];
    vwsiViSwitch:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_NN_VI_SURFACE_EXTENSION_NAME];
    vwsiHeadless:
      Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME];
  else
    Result := [VK_KHR_SURFACE_EXTENSION_NAME];
  end;
end;

function GetRequiredVulkanInstanceExtensions: TAnsiStringArray;
begin
  Result := GetRequiredVulkanInstanceExtensionsForPlatform(vwsiAuto);
end;

function QuerySupportedVulkanInstanceExtensions: TAnsiStringArray;
var
  ExtCount, I: Cardinal;
  Exts: array of VkExtensionProperties;
begin
  SetLength(Result, 0);
  if not IsVulkanLoaded then
  begin
    if not LoadVulkan then
      Exit;
  end;
  if not Assigned(vkEnumerateInstanceExtensionProperties) then
    Exit;
  ExtCount := 0;
  if vkEnumerateInstanceExtensionProperties(nil, @ExtCount, nil) <> VK_SUCCESS then
    Exit;
  if ExtCount = 0 then
    Exit;
  SetLength(Exts, ExtCount);
  if vkEnumerateInstanceExtensionProperties(nil, @ExtCount, @Exts[0]) <> VK_SUCCESS then
    Exit;
  SetLength(Result, ExtCount);
  for I := 0 to ExtCount - 1 do
    Result[I] := PAnsiChar(@Exts[I].extensionName[0]);
end;

function IsVulkanInstanceExtensionSupported(const AExtensionName: AnsiString): Boolean;
var
  Supported: TAnsiStringArray;
  I: Integer;
begin
  Supported := QuerySupportedVulkanInstanceExtensions;
  for I := 0 to High(Supported) do
    if Supported[I] = AExtensionName then
      Exit(True);
  Result := False;
end;

function QuerySupportedVulkanInstanceLayers: TAnsiStringArray;
var
  LayerCount, I: Cardinal;
  Layers: array of VkLayerProperties;
begin
  Result := nil;
  SetLength(Result, 0);
  if not IsVulkanLoaded then
  begin
    if not LoadVulkan then
      Exit;
  end;
  if not Assigned(vkEnumerateInstanceLayerProperties) then
    Exit;
  LayerCount := 0;
  if vkEnumerateInstanceLayerProperties(@LayerCount, nil) <> VK_SUCCESS then
    Exit;
  if LayerCount = 0 then
    Exit;
  SetLength(Layers, LayerCount);
  if vkEnumerateInstanceLayerProperties(@LayerCount, @Layers[0]) <> VK_SUCCESS then
    Exit;
  SetLength(Result, LayerCount);
  for I := 0 to LayerCount - 1 do
    Result[I] := PAnsiChar(@Layers[I].layerName[0]);
end;

function IsVulkanInstanceLayerSupported(const ALayerName: AnsiString): Boolean;
var
  Supported: TAnsiStringArray;
  I: Integer;
begin
  Supported := QuerySupportedVulkanInstanceLayers;
  for I := 0 to High(Supported) do
    if Supported[I] = ALayerName then
      Exit(True);
  Result := False;
end;

function IsVulkanValidationLayerSupported: Boolean;
begin
  Result := IsVulkanInstanceLayerSupported(VK_LAYER_KHRONOS_validation);
end;

function BuildAvailableVulkanWSIInstanceExtensions(
  const AAvailableExtensions: array of AnsiString;
  out AEnablePortabilityBit: Boolean): TAnsiStringArray;
const
  CandidateExts: array[0..8] of AnsiString = (
    VK_KHR_SURFACE_EXTENSION_NAME,
    VK_KHR_WIN32_SURFACE_EXTENSION_NAME,
    VK_KHR_XLIB_SURFACE_EXTENSION_NAME,
    VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME,
    VK_KHR_ANDROID_SURFACE_EXTENSION_NAME,
    VK_EXT_METAL_SURFACE_EXTENSION_NAME,
    VK_NN_VI_SURFACE_EXTENSION_NAME,
    VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME,
    VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
  );

  function HasAvailable(const AExt: AnsiString): Boolean;
  var
    J: Integer;
  begin
    for J := Low(AAvailableExtensions) to High(AAvailableExtensions) do
      if AAvailableExtensions[J] = AExt then
        Exit(True);
    Result := False;
  end;

var
  I, OutCount: Integer;
begin
  AEnablePortabilityBit := False;
  SetLength(Result, 0);
  if Length(AAvailableExtensions) = 0 then
  begin
    Result := GetRequiredVulkanInstanceExtensions;
    for I := 0 to High(Result) do
      if Result[I] = VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME then
        AEnablePortabilityBit := True;
    Exit;
  end;

  OutCount := 0;
  SetLength(Result, Length(CandidateExts));
  for I := Low(CandidateExts) to High(CandidateExts) do
    if HasAvailable(CandidateExts[I]) then
    begin
      Result[OutCount] := CandidateExts[I];
      if CandidateExts[I] = VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME then
        AEnablePortabilityBit := True;
      Inc(OutCount);
    end;
  SetLength(Result, OutCount);
end;

function GetAvailableVulkanWSIInstanceExtensions(
  out AEnablePortabilityBit: Boolean): TAnsiStringArray;
var
  Supported: TAnsiStringArray;
begin
  Supported := QuerySupportedVulkanInstanceExtensions;
  Result := BuildAvailableVulkanWSIInstanceExtensions(Supported, AEnablePortabilityBit);
end;

function ValidateVulkanNativeSurfaceDesc(const ADesc: TVulkanNativeSurfaceDesc): Boolean;
var
  ResolvedPlatform: TVulkanWSIPlatform;
begin
  ResolvedPlatform := ADesc.Platform;
  if ResolvedPlatform = vwsiAuto then
    ResolvedPlatform := GetDefaultVulkanWSIPlatform;

  case ResolvedPlatform of
    vwsiWin32:
      Result := ADesc.Win32WindowHandle <> 0;
    vwsiXlib:
      Result := (ADesc.XlibDisplay <> nil) and (ADesc.XlibWindow <> 0);
    vwsiWayland:
      Result := (ADesc.WaylandDisplay <> nil) and (ADesc.WaylandSurface <> nil);
    vwsiAndroid:
      Result := ADesc.AndroidNativeWindow <> nil;
    vwsiMetal:
      Result := ADesc.MetalLayer <> nil;
    vwsiViSwitch:
      Result := ADesc.ViWindowHandle <> nil;
    vwsiHeadless:
      Result := ADesc.Platform = vwsiHeadless;
  else
    Result := False;
  end;
end;

function CreateVulkanSurfaceFromNativeDesc(const AInstance: VkInstance;
  const ADesc: TVulkanNativeSurfaceDesc): VkSurfaceKHR;
var
  ResolvedPlatform: TVulkanWSIPlatform;
begin
  ResolvedPlatform := ADesc.Platform;
  if ResolvedPlatform = vwsiAuto then
    ResolvedPlatform := GetDefaultVulkanWSIPlatform;

  if not ValidateVulkanNativeSurfaceDesc(ADesc) then
    raise ERHIError.CreateFmt(
      'Invalid or uninitialized native window handle for Vulkan WSI platform %d',
      [Ord(ResolvedPlatform)]);

  case ResolvedPlatform of
    vwsiWin32:
      Result := CreateVulkanWin32Surface(AInstance, ADesc.Win32HInstance, ADesc.Win32WindowHandle);
    vwsiXlib:
      Result := CreateVulkanXlibSurface(AInstance, ADesc.XlibDisplay, ADesc.XlibWindow);
    vwsiWayland:
      Result := CreateVulkanWaylandSurface(AInstance, ADesc.WaylandDisplay, ADesc.WaylandSurface);
    vwsiAndroid:
      Result := CreateVulkanAndroidSurface(AInstance, ADesc.AndroidNativeWindow);
    vwsiMetal:
      Result := CreateVulkanMetalSurface(AInstance, ADesc.MetalLayer);
    vwsiViSwitch:
      Result := CreateVulkanViSurface(AInstance, ADesc.ViWindowHandle);
    vwsiHeadless:
      Result := CreateVulkanHeadlessSurface(AInstance);
  else
    raise ERHIError.CreateFmt('Unsupported Vulkan WSI platform %d', [Ord(ResolvedPlatform)]);
  end;
end;

function SelectOptimalRenderBackend(const Requested: TRenderBackend;
  const VulkanAvailable: Boolean; const VulkanMajor: Cardinal;
  const VulkanMinor: Cardinal; const MobileGLESFallback: Boolean): TRenderBackend;
var
  SupportsVulkanBaseline: Boolean;
  FallbackBackend: TRenderBackend;
begin
  SupportsVulkanBaseline := VulkanAvailable and
    ((VulkanMajor > 1) or ((VulkanMajor = 1) and (VulkanMinor >= 1)));

  if MobileGLESFallback then
    FallbackBackend := backendOpenGLES
  else
    FallbackBackend := backendOpenGL;

  case Requested of
    backendAuto, backendVulkan:
      if SupportsVulkanBaseline then
        Result := backendVulkan
      else
        Result := FallbackBackend;
  else
    Result := Requested;
  end;
end;

function CreateVulkanWin32Surface(const AInstance: VkInstance;
  const AHInstance: VkWin32HINSTANCE; const AWindowHandle: VkWin32HWND): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateWin32SurfaceKHR;
  CreateInfo: VkWin32SurfaceCreateInfoKHR;
begin
  Result := VK_NULL_HANDLE;
  if AWindowHandle = 0 then
    raise ERHIError.Create('vkCreateWin32SurfaceKHR failed: HWND is 0');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateWin32SurfaceKHR');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
  CreateInfo.hinstance := AHInstance;
  CreateInfo.hwnd := AWindowHandle;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateWin32SurfaceKHR');
end;

function CreateVulkanXlibSurface(const AInstance: VkInstance;
  const ADisplay: PVkXlibDisplay; const AXWindow: VkXlibWindow): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateXlibSurfaceKHR;
  CreateInfo: VkXlibSurfaceCreateInfoKHR;
begin
  Result := VK_NULL_HANDLE;
  if (ADisplay = nil) or (AXWindow = 0) then
    raise ERHIError.Create('vkCreateXlibSurfaceKHR failed: Display or XWindow is nil/0');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateXlibSurfaceKHR');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
  CreateInfo.dpy := ADisplay;
  CreateInfo.window := AXWindow;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateXlibSurfaceKHR');
end;

function CreateVulkanWaylandSurface(const AInstance: VkInstance;
  const AWlDisplay: Pwl_display; const AWlSurface: Pwl_surface): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateWaylandSurfaceKHR;
  CreateInfo: VkWaylandSurfaceCreateInfoKHR;
begin
  Result := VK_NULL_HANDLE;
  if (AWlDisplay = nil) or (AWlSurface = nil) then
    raise ERHIError.Create('vkCreateWaylandSurfaceKHR failed: wl_display or wl_surface is nil');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateWaylandSurfaceKHR');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
  CreateInfo.display := AWlDisplay;
  CreateInfo.surface := AWlSurface;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateWaylandSurfaceKHR');
end;

function CreateVulkanAndroidSurface(const AInstance: VkInstance;
  const ANativeWindow: PANativeWindow): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateAndroidSurfaceKHR;
  CreateInfo: VkAndroidSurfaceCreateInfoKHR;
begin
  Result := VK_NULL_HANDLE;
  if ANativeWindow = nil then
    raise ERHIError.Create('vkCreateAndroidSurfaceKHR failed: ANativeWindow is nil');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateAndroidSurfaceKHR');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR;
  CreateInfo.window := ANativeWindow;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateAndroidSurfaceKHR');
end;

function CreateVulkanMetalSurface(const AInstance: VkInstance;
  const AMetalLayer: PCAMetalLayer): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateMetalSurfaceEXT;
  CreateInfo: VkMetalSurfaceCreateInfoEXT;
begin
  Result := VK_NULL_HANDLE;
  if AMetalLayer = nil then
    raise ERHIError.Create('vkCreateMetalSurfaceEXT failed: CAMetalLayer is nil');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateMetalSurfaceEXT');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
  CreateInfo.pLayer := AMetalLayer;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateMetalSurfaceEXT');
end;

function CreateVulkanViSurface(const AInstance: VkInstance;
  const AViWindow: PViWindowHandleNN): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateViSurfaceNN;
  CreateInfo: VkViSurfaceCreateInfoNN;
begin
  Result := VK_NULL_HANDLE;
  if AViWindow = nil then
    raise ERHIError.Create('vkCreateViSurfaceNN failed: ViWindowHandle is nil');
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateViSurfaceNN');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN;
  CreateInfo.window := AViWindow;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateViSurfaceNN');
end;

function CreateVulkanHeadlessSurface(const AInstance: VkInstance): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateHeadlessSurfaceEXT;
  CreateInfo: VkHeadlessSurfaceCreateInfoEXT;
begin
  Result := VK_NULL_HANDLE;
  Pointer(CreateFn) := ResolveSurfaceProc(AInstance, 'vkCreateHeadlessSurfaceEXT');
  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT;
  CheckSurfaceResult(CreateFn(AInstance, @CreateInfo, nil, @Result), 'vkCreateHeadlessSurfaceEXT');
end;

{$if defined(DARWIN)}
var
  GDarwinQuartzLib: TDynLib = nil;
  GDarwinObjcLib: TDynLib = nil;
{$endif}

function EnsureDarwinNSViewMetalLayer(const ANSViewPtr: Pointer;
  const AContentsScale: Double): PCAMetalLayer;
{$if defined(DARWIN)}
type
  TObjcLookUpClass = function(const AName: PAnsiChar): Pointer; cdecl;
  TSelRegisterName = function(const AName: PAnsiChar): Pointer; cdecl;
  TMsgSendId = function(AReceiver, ASelector: Pointer): Pointer; cdecl;
  TMsgSendVoidId = procedure(AReceiver, ASelector, AArg: Pointer); cdecl;
  TMsgSendVoidBool = procedure(AReceiver, ASelector: Pointer; AFlag: ByteBool); cdecl;
  TMsgSendVoidDouble = procedure(AReceiver, ASelector: Pointer; AScale: Double); cdecl;
  TMsgSendBoolId = function(AReceiver, ASelector, AClass: Pointer): ByteBool; cdecl;
var
  LookUpClassFn: TObjcLookUpClass;
  SelRegisterFn: TSelRegisterName;
  MsgSendSym, MetalLayerClass, LayerObj: Pointer;
begin
  Result := nil;
  if ANSViewPtr = nil then
    Exit;

  if GDarwinQuartzLib = nil then
    GDarwinQuartzLib := TDynLib.Load('/System/Library/Frameworks/QuartzCore.framework/QuartzCore', False);
  if GDarwinObjcLib = nil then
    GDarwinObjcLib := TDynLib.Load('/usr/lib/libobjc.A.dylib', False);
  if GDarwinObjcLib = nil then
    Exit;

  Pointer(LookUpClassFn) := GDarwinObjcLib.Symbol('objc_lookUpClass');
  Pointer(SelRegisterFn) := GDarwinObjcLib.Symbol('sel_registerName');
  MsgSendSym := GDarwinObjcLib.Symbol('objc_msgSend');
  if (not Assigned(LookUpClassFn)) or (not Assigned(SelRegisterFn)) or (MsgSendSym = nil) then
    Exit;

  MetalLayerClass := LookUpClassFn('CAMetalLayer');
  if MetalLayerClass = nil then
    Exit;

  LayerObj := TMsgSendId(MsgSendSym)(ANSViewPtr, SelRegisterFn('layer'));
  if (LayerObj = nil) or
     (not TMsgSendBoolId(MsgSendSym)(LayerObj, SelRegisterFn('isKindOfClass:'), MetalLayerClass)) then
  begin
    LayerObj := TMsgSendId(MsgSendSym)(MetalLayerClass, SelRegisterFn('layer'));
    if LayerObj = nil then
      Exit;
    TMsgSendVoidId(MsgSendSym)(ANSViewPtr, SelRegisterFn('setLayer:'), LayerObj);
    TMsgSendVoidBool(MsgSendSym)(ANSViewPtr, SelRegisterFn('setWantsLayer:'), True);
  end;

  if AContentsScale > 0.0 then
    TMsgSendVoidDouble(MsgSendSym)(LayerObj, SelRegisterFn('setContentsScale:'), AContentsScale);

  Result := PCAMetalLayer(LayerObj);
end;
{$else}
begin
  if (ANSViewPtr = nil) and (AContentsScale < 0.0) then ;
  Result := nil;
end;
{$endif}

{$if defined(DARWIN)}
initialization
finalization
  FreeAndNil(GDarwinObjcLib);
  FreeAndNil(GDarwinQuartzLib);
{$endif}

end.
