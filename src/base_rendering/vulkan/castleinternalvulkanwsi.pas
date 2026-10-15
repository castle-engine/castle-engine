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

{ Platform-specific Vulkan Window System Integration (WSI) bindings
  for Windows (Win32) and Linux (Xlib / Wayland). }
unit CastleInternalVulkanWSI;

{$I castleconf.inc}

interface

uses
  SysUtils,
  {$if defined(MSWINDOWS)} Windows, {$endif}
  {$if defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)} X, Xlib, {$endif}
  CastleInternalRHI, CastleInternalVulkan;

const
  VK_KHR_SURFACE_EXTENSION_NAME         = 'VK_KHR_surface';
  VK_KHR_WIN32_SURFACE_EXTENSION_NAME   = 'VK_KHR_win32_surface';
  VK_KHR_XLIB_SURFACE_EXTENSION_NAME    = 'VK_KHR_xlib_surface';
  VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME = 'VK_KHR_wayland_surface';

  VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR   = 1000009000;
  VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR    = 1000004000;
  VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = 1000006000;

{$if defined(MSWINDOWS)}
type
  VkWin32SurfaceCreateFlagsKHR = VkFlags;
  PVkWin32SurfaceCreateInfoKHR = ^VkWin32SurfaceCreateInfoKHR;
  VkWin32SurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkWin32SurfaceCreateFlagsKHR;
    hinstance: HINST;
    hwnd: HWND;
  end;

  PFN_vkCreateWin32SurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkWin32SurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; {$ifdef MSWINDOWS}stdcall{$else}cdecl{$endif};

function CreateVulkanWin32Surface(const AInstance: VkInstance;
  const AHInstance: HINST; const AWindowHandle: HWND): VkSurfaceKHR;
{$endif}

{$if defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)}
type
  { X11 / Xlib WSI }
  VkXlibSurfaceCreateFlagsKHR = VkFlags;
  PVkXlibSurfaceCreateInfoKHR = ^VkXlibSurfaceCreateInfoKHR;
  VkXlibSurfaceCreateInfoKHR = record
    sType: VkStructureType;
    pNext: Pointer;
    flags: VkXlibSurfaceCreateFlagsKHR;
    dpy: PDisplay;
    window: TWindow;
  end;

  PFN_vkCreateXlibSurfaceKHR = function(
    instance: VkInstance;
    const pCreateInfo: PVkXlibSurfaceCreateInfoKHR;
    const pAllocator: PVkAllocationCallbacks;
    pSurface: PVkSurfaceKHR): VkResult; cdecl;

  { Wayland WSI (for GTK3 / native Wayland backends) }
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
    pSurface: PVkSurfaceKHR): VkResult; cdecl;

function CreateVulkanXlibSurface(const AInstance: VkInstance;
  const ADisplay: PDisplay; const AXWindow: TWindow): VkSurfaceKHR;

function CreateVulkanWaylandSurface(const AInstance: VkInstance;
  const AWlDisplay: Pwl_display; const AWlSurface: Pwl_surface): VkSurfaceKHR;
{$endif}

type
  TAnsiStringArray = array of AnsiString;

{ Returns the required VkInstance surface extensions for the current OS }
function GetRequiredVulkanInstanceExtensions: TAnsiStringArray;

implementation

function GetRequiredVulkanInstanceExtensions: TAnsiStringArray;
begin
  {$if defined(MSWINDOWS)}
  Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_WIN32_SURFACE_EXTENSION_NAME];
  {$elseif defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)}
  Result := [VK_KHR_SURFACE_EXTENSION_NAME, VK_KHR_XLIB_SURFACE_EXTENSION_NAME];
  {$else}
  Result := [VK_KHR_SURFACE_EXTENSION_NAME];
  {$endif}
end;

{$if defined(MSWINDOWS)}
function CreateVulkanWin32Surface(const AInstance: VkInstance;
  const AHInstance: HINST; const AWindowHandle: HWND): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateWin32SurfaceKHR;
  CreateInfo: VkWin32SurfaceCreateInfoKHR;
  Res: VkResult;
begin
  Result := VK_NULL_HANDLE;
  Pointer(CreateFn) := vkGetInstanceProcAddr(AInstance, 'vkCreateWin32SurfaceKHR');
  if not Assigned(CreateFn) then
    raise ERHIError.Create('vkCreateWin32SurfaceKHR not found in Vulkan instance');

  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
  CreateInfo.hinstance := AHInstance;
  CreateInfo.hwnd := AWindowHandle;

  Res := CreateFn(AInstance, @CreateInfo, nil, @Result);
  if Res <> VK_SUCCESS then
    raise ERHIError.CreateFmt('vkCreateWin32SurfaceKHR failed (VkResult: %d)', [Ord(Res)]);
end;
{$endif}

{$if defined(UNIX) and not defined(DARWIN) and not defined(ANDROID)}
function CreateVulkanXlibSurface(const AInstance: VkInstance;
  const ADisplay: PDisplay; const AXWindow: TWindow): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateXlibSurfaceKHR;
  CreateInfo: VkXlibSurfaceCreateInfoKHR;
  Res: VkResult;
begin
  Result := VK_NULL_HANDLE;
  Pointer(CreateFn) := vkGetInstanceProcAddr(AInstance, 'vkCreateXlibSurfaceKHR');
  if not Assigned(CreateFn) then
    raise ERHIError.Create('vkCreateXlibSurfaceKHR not found in Vulkan instance');

  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR;
  CreateInfo.dpy := ADisplay;
  CreateInfo.window := AXWindow;

  Res := CreateFn(AInstance, @CreateInfo, nil, @Result);
  if Res <> VK_SUCCESS then
    raise ERHIError.CreateFmt('vkCreateXlibSurfaceKHR failed (VkResult: %d)', [Ord(Res)]);
end;

function CreateVulkanWaylandSurface(const AInstance: VkInstance;
  const AWlDisplay: Pwl_display; const AWlSurface: Pwl_surface): VkSurfaceKHR;
var
  CreateFn: PFN_vkCreateWaylandSurfaceKHR;
  CreateInfo: VkWaylandSurfaceCreateInfoKHR;
  Res: VkResult;
begin
  Result := VK_NULL_HANDLE;
  Pointer(CreateFn) := vkGetInstanceProcAddr(AInstance, 'vkCreateWaylandSurfaceKHR');
  if not Assigned(CreateFn) then
    raise ERHIError.Create('vkCreateWaylandSurfaceKHR not found in Vulkan instance');

  FillChar(CreateInfo, SizeOf(CreateInfo), 0);
  CreateInfo.sType := VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR;
  CreateInfo.display := AWlDisplay;
  CreateInfo.surface := AWlSurface;

  Res := CreateFn(AInstance, @CreateInfo, nil, @Result);
  if Res <> VK_SUCCESS then
    raise ERHIError.CreateFmt('vkCreateWaylandSurfaceKHR failed (VkResult: %d)', [Ord(Res)]);
end;
{$endif}

end.
