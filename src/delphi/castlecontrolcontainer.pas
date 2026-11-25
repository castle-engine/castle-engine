{
  Copyright 2022-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container class TCastleControlContainer used inside FMX and VCL TCastleControl. }
unit CastleControlContainer;

{ Implementation notes:
  This is easier to use than bare TCastleContainer.
  This relies on

  - private OpenGL(ES) context handled by TGLContext descendants

  - with requirements expressed by public TGLContextRequirements.

  TODO: It is our long-term plan to merge TCastleControlContainer capabilities
  into TCastleContainer. Thus, the implementation done here will be shared
  by TCastleWindow (all backends) and LCL TCastleControl,
  unifying them more.
  Right now, TCastleWindow and LCL TCastleControl do some things manually
  for historic purposes, e.g. they expose GL context requirements
  like TCastleWindow.DepthBits.
  Nowadays we know that

  - API of classes like TCastleWindow / TCastleControl should be simpler and just expose
    Container for everything reasonable.

  - Container also should delegate to TCastleView and in turn to TCastleUserInterface
    for everything reasonable.
}

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  {$ifdef MSWINDOWS} CastleInternalContextWgl, {$endif}
  {$ifdef LINUX} CastleInternalContextEgl, {$endif}
  CastleRectangles, CastleImages,
  CastleUIControls, CastleInternalContextBase,
  CastleInternalRHI, CastleInternalVulkan, CastleInternalVulkanWSI, CastleInternalRHI_Vulkan;

type
  TCastleControlContainer = class(TCastleContainer)
  strict private
    FRequirements: TGLContextRequirements;
    { Internal platform-specific context data and initialization/finalization.
      This is in contrast to TCastleContainer.Context, that is public
      and manages context properties that are cross-platform and available
      for all OpenGL(ES). }
    FPlatformContext: TGLContext;
    FGLInitialized: Boolean;
    FAutoRedisplay: Boolean;
    { Copy of Requirements.DoubleBuffer when the context was created. }
    FEffectiveDoubleBuffer: Boolean;
    FDesignUrl: String;
    FDesignLoaded: TCastleUserInterface;
    FDesignLoadedOwner: TComponent;
    FVulkanSwapchainSurface: TCastleVulkanSwapchainSurface;
    FVulkanInitialized: Boolean;
    FDisableVulkanForContainer: Boolean;
    FHasCustomVulkanSurfaceDesc: Boolean;
    FCustomVulkanSurfaceDesc: TVulkanNativeSurfaceDesc;
    FOnCreateVulkanSurface: TVulkanCreateSurfaceEvent;
    FVulkanFramesRenderedCount: Cardinal;
    procedure SetAutoRedisplay(const Value: Boolean);
    procedure DoUpdate;
    procedure SetDesignUrl(const Value: String);
    procedure LoadDesign;
    procedure UnLoadDesign;
    function IsVulkanBackendRequested: Boolean;
    function CreateVulkanSurface(const AInstance: VkInstance): VkSurfaceKHR;
    function InitializeVulkanContainerIfNeeded: Boolean;
    function BeginVulkanContainerFrame: Boolean;
    procedure EndVulkanContainerFrame;
  protected
    { Adjust context parameters right before usage. }
    procedure AdjustContext(const PlatformContext: TGLContext); virtual;

    { Extract native OS child-control handle for VkSurfaceKHR creation (Phase 115). }
    function BuildVulkanNativeSurfaceDesc: TVulkanNativeSurfaceDesc; virtual;

    { Call these methods from final components that wrap TCastleControlContainer,
      like TCastleControl, TCastleWindow. }
    procedure InitializeContext;
    procedure FinalizeContext;
    procedure DoRender;

    class var
      { "Updating" means that the mechanism to call DoUpdateEverything
        continuosly is set up. }
      UpdatingEnabled: Boolean;

    class procedure DoUpdateEverything;
    class procedure UpdatingEnable; virtual; abstract;
    class procedure UpdatingDisable; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; override;

    { Is OpenGL(ES) context initialized for this container. }
    function GLInitialized: boolean; override;

    { Make given OpenGL(ES) context current.
      Can only be used if GLInitialized. }
    procedure MakeContextCurrent;

    { Synchronize per-control Vulkan swapchain extent after control resize (Phase 115). }
    procedure ResizeVulkanSwapchainIfNeeded;
    procedure SetCustomVulkanSurfaceDesc(const ADesc: TVulkanNativeSurfaceDesc);
    procedure ClearCustomVulkanSurfaceDesc;
    class function SharedVulkanDevice: TVulkanRHIDevice;
    property OnCreateVulkanSurface: TVulkanCreateSurfaceEvent
      read FOnCreateVulkanSurface write FOnCreateVulkanSurface;
    property VulkanSwapchainSurface: TCastleVulkanSwapchainSurface read FVulkanSwapchainSurface;
    property VulkanInitialized: Boolean read FVulkanInitialized;
    property VulkanFramesRenderedCount: Cardinal read FVulkanFramesRenderedCount;

    { When the DesignUrl is set you can use this method to find
      loaded components. Like this:

      @longCode(#
      MyButton := MyCastleControl.Container.DesignedComponent('MyButton') as TCastleButton;
      #)

      When the name is not found, raises exception (unless Required is @false,
      then it returns @nil).

      @seealso DesignUrl }
    function DesignedComponent(const ComponentName: String;
      const Required: Boolean = true): TComponent;
  published
    { Should we automatically redraw the window all the time,
      without the need for an @link(Invalidate) call.
      If @true (the default), rendering and other processing will
      be called constantly.

      If your game may have a still screen (nothing animates),
      then this approach is a little unoptimal, as we use CPU and GPU
      for drawing, when it's not needed. In such case, you can set this
      property to @false, and make sure that you call
      @link(Invalidate) always when you need to redraw the screen.
      Note that the engine components always call @link(Invalidate) when
      necessary, so usually you should only call it yourself if you do custom
      rendering in overridden @link(TCastleUserInterface.Render) implementation. }
    property AutoRedisplay: Boolean read FAutoRedisplay write SetAutoRedisplay
      default true;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;

    { Load and show the design (.castle-user-interface file).
      You can reference the loaded components by name using @link(DesignedComponent).

      If you have more complicated control flow,
      we recommend to leave this property empty, and split your management
      into a number of states (TCastleView) instead.
      In this case, load design using TCastleView.DesignUrl.
      This property makes it however easy to use .castle-user-interface
      in simple cases, e.g. when TCastleControl just shows one UI.

      The design loaded here is visible also at design-time,
      when editing the form in Lazarus/Delphi.
      Though we have to way to edit it now in Lazarus/Delphi (you have to use CGE editor
      to edit the design), so it is just a preview in this case.

      See https://castle-engine.io/control_on_form for documentation how to use TCastleControl. }
    property DesignUrl: String read FDesignUrl write SetDesignUrl;
  end;

  TCastleControlContainerList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleControlContainer>;

implementation

uses SysUtils, Math,
  CastleColors, CastleVectors,
  CastleRenderContext, CastleGLUtils, CastleApplicationProperties, CastleGLImages,
  CastleGLVersion, CastleTimeUtils, CastleUtils, CastleLog, CastleUriUtils,
  CastleComponentSerialize, CastleInternalDelphiUtils, CastleFilesUtils;

var
  { All TCastleControlContainer instances created. }
  ContainersList: TCastleControlContainerList;

  { Shared Vulkan RHI device across all open VCL/FMX TCastleControlContainer instances (Phase 115). }
  SharedControlVulkanDevice: TVulkanRHIDevice;
  SharedControlVulkanOpenCount: Cardinal;
  OwnsSharedControlVulkanDevice: Boolean;

  LastLimitFPSTime: TTimerResult;

constructor TCastleControlContainer.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FPlatformContext := ContextCreateBestInstance;

  FAutoRedisplay := true;

  ContainersList.Add(Self);
end;

destructor TCastleControlContainer.Destroy;
begin
  UnLoadDesign;
  FreeAndNil(FPlatformContext);
  if ContainersList <> nil then
    ContainersList.Remove(Self);
  inherited;
end;

procedure TCastleControlContainer.MakeContextCurrent;
begin
  Assert(GLInitialized);
  RenderContext := Context;
  if FVulkanInitialized then
  begin
    if SharedControlVulkanDevice <> nil then
      SharedControlVulkanDevice.ActiveSwapchainSurface := FVulkanSwapchainSurface;
  end else
    FPlatformContext.MakeCurrent;
end;

procedure TCastleControlContainer.SetCustomVulkanSurfaceDesc(const ADesc: TVulkanNativeSurfaceDesc);
begin
  FCustomVulkanSurfaceDesc := ADesc;
  FHasCustomVulkanSurfaceDesc := True;
end;

procedure TCastleControlContainer.ClearCustomVulkanSurfaceDesc;
begin
  FillChar(FCustomVulkanSurfaceDesc, SizeOf(FCustomVulkanSurfaceDesc), 0);
  FHasCustomVulkanSurfaceDesc := False;
end;

class function TCastleControlContainer.SharedVulkanDevice: TVulkanRHIDevice;
begin
  Result := SharedControlVulkanDevice;
end;

function TCastleControlContainer.BuildVulkanNativeSurfaceDesc: TVulkanNativeSurfaceDesc;
begin
  if FHasCustomVulkanSurfaceDesc then
    Exit(FCustomVulkanSurfaceDesc);
  FillChar(Result, SizeOf(Result), 0);
end;

function TCastleControlContainer.IsVulkanBackendRequested: Boolean;
begin
  Result := (ApplicationProperties.RenderBackend = backendVulkan) and
            (not FDisableVulkanForContainer);
end;

function TCastleControlContainer.CreateVulkanSurface(const AInstance: VkInstance): VkSurfaceKHR;
begin
  if Assigned(FOnCreateVulkanSurface) then
    Result := FOnCreateVulkanSurface(AInstance)
  else
    Result := CreateVulkanSurfaceFromNativeDesc(AInstance, BuildVulkanNativeSurfaceDesc);
end;

function TCastleControlContainer.InitializeVulkanContainerIfNeeded: Boolean;
var
  Desc: TVulkanNativeSurfaceDesc;
  CreatedSharedDevice: Boolean;
  CtrlDebugName: String;
begin
  if FVulkanInitialized then
    Exit(True);
  if not IsVulkanBackendRequested then
    Exit(False);

  Desc := BuildVulkanNativeSurfaceDesc;
  if (not Assigned(FOnCreateVulkanSurface)) and
     (not ValidateVulkanNativeSurfaceDesc(Desc)) then
  begin
    WritelnLog('Vulkan', 'TCastleControlContainer native widget surface handle not available; falling back to OpenGL');
    FDisableVulkanForContainer := True;
    Exit(False);
  end;

  CreatedSharedDevice := False;
  try
    if (Owner <> nil) and (Owner.Name <> '') then
      CtrlDebugName := 'CastleControl_' + Owner.Name
    else
      CtrlDebugName := 'CastleControl';

    if SharedControlVulkanDevice = nil then
    begin
      SharedControlVulkanDevice := TVulkanRHIDevice.CreateSharedWithWSI(
        False, {$ifdef FPC}@{$endif}CreateVulkanSurface);
      OwnsSharedControlVulkanDevice := True;
      CreatedSharedDevice := True;
    end;

    FVulkanSwapchainSurface := SharedControlVulkanDevice.CreateSwapchainSurface(
      {$ifdef FPC}@{$endif}CreateVulkanSurface,
      Cardinal(Max(1, PixelsWidth)),
      Cardinal(Max(1, PixelsHeight)),
      CtrlDebugName);
    if Requirements.MultiSampling > 1 then
      FVulkanSwapchainSurface.SampleCount := Requirements.MultiSampling;

    SetRHIDevice(SharedControlVulkanDevice);
    FVulkanInitialized := True;
    Inc(SharedControlVulkanOpenCount);

    if (TGLContext.InitializedContextsCount = 0) and (SharedControlVulkanOpenCount = 1) then
      FreeAndNil(GLFeatures);

    Result := True;
  except
    on E: Exception do
    begin
      WritelnLog('Vulkan', 'TCastleControlContainer Vulkan WSI initialization fell back cleanly to OpenGL: ' + E.Message);
      if (SharedControlVulkanDevice <> nil) and (FVulkanSwapchainSurface <> nil) then
        SharedControlVulkanDevice.DestroySwapchainSurface(FVulkanSwapchainSurface);
      if CreatedSharedDevice and (SharedControlVulkanOpenCount = 0) then
      begin
        FreeAndNil(SharedControlVulkanDevice);
        OwnsSharedControlVulkanDevice := False;
      end;
      FDisableVulkanForContainer := True;
      Result := False;
    end;
  end;
end;

procedure TCastleControlContainer.ResizeVulkanSwapchainIfNeeded;
var
  ReqW, ReqH: Cardinal;
begin
  if (not FVulkanInitialized) or (FVulkanSwapchainSurface = nil) then
    Exit;
  ReqW := Cardinal(Max(0, PixelsWidth));
  ReqH := Cardinal(Max(0, PixelsHeight));
  if (ReqW = 0) or (ReqH = 0) or
     (FVulkanSwapchainSurface.RequestedWidth <> ReqW) or
     (FVulkanSwapchainSurface.RequestedHeight <> ReqH) then
    FVulkanSwapchainSurface.Resize(ReqW, ReqH);
end;

function TCastleControlContainer.BeginVulkanContainerFrame: Boolean;
var
  Cmd: TRHICommandList;
  ColorTex: TRHITexture;
  ClearCol: TCastleColor;
  TargetW, TargetH: Integer;
  PassArea: TRectangle;
  PassInfo: TRHIRenderingInfo;
begin
  Result := False;
  if (not FVulkanInitialized) or (SharedControlVulkanDevice = nil) or
     (FVulkanSwapchainSurface = nil) then
    Exit;

  ResizeVulkanSwapchainIfNeeded;
  if (Requirements.MultiSampling > 1) and
     (FVulkanSwapchainSurface.SampleCount <> Requirements.MultiSampling) then
    FVulkanSwapchainSurface.SampleCount := Requirements.MultiSampling;

  Cmd := SharedControlVulkanDevice.BeginSurfaceFrame(FVulkanSwapchainSurface);
  if Cmd = nil then
    Exit;

  Result := True;
  ColorTex := FVulkanSwapchainSurface.GetCurrentColorTexture;
  if (ColorTex <> nil) and (not Cmd.IsRendering) then
  begin
    if PixelsWidth > 0 then
      TargetW := Min(Integer(ColorTex.Desc.Width), PixelsWidth)
    else
      TargetW := Max(1, Integer(ColorTex.Desc.Width));
    if PixelsHeight > 0 then
      TargetH := Min(Integer(ColorTex.Desc.Height), PixelsHeight)
    else
      TargetH := Max(1, Integer(ColorTex.Desc.Height));
    PassArea := Rectangle(0, 0, Max(1, TargetW), Max(1, TargetH));
    if BackgroundEnable then
      ClearCol := BackgroundColor
    else
      ClearCol := Vector4(0.0, 0.0, 0.0, 1.0);
    PassInfo := FVulkanSwapchainSurface.BuildRenderingInfo(
      PassArea, ClearCol, loClear, loClear, 1.0);
    Cmd.BeginRendering(PassInfo);
    Cmd.SetViewport(FloatRectangle(PassArea), 0.0, 1.0);
    Cmd.SetScissor(PassArea);
    if RenderContext <> nil then
      RenderContext.Viewport := PassArea;
  end;
end;

procedure TCastleControlContainer.EndVulkanContainerFrame;
begin
  if (not FVulkanInitialized) or (SharedControlVulkanDevice = nil) or
     (FVulkanSwapchainSurface = nil) then
    Exit;
  TDrawableImage.BatchingFlush;
  if (ActiveCommandList <> nil) and ActiveCommandList.IsRendering then
    ActiveCommandList.EndRendering;
  SharedControlVulkanDevice.EndSurfaceFrameAndPresent(FVulkanSwapchainSurface);
  Inc(FVulkanFramesRenderedCount);
end;

function TCastleControlContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;

  { Color buffer where we draw, and from which it makes sense to grab pixels. }
  function SaveScreenBuffer: TColorBuffer;
  begin
    if FEffectiveDoubleBuffer then
      Result := cbBack
    else
      Result := cbFront;
  end;

begin
  if (RHIDevice <> nil) and (ActiveCommandList = nil) then
    Exit(inherited SaveScreen(SaveRect));
  MakeContextCurrent;
  EventBeforeRender;
  EventRender;
  Result := SaveScreen_NoFlush(SaveRect, SaveScreenBuffer);
end;

procedure TCastleControlContainer.InitializeContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;

    if InitializeVulkanContainerIfNeeded then
    begin
      FEffectiveDoubleBuffer := True;
      MakeContextCurrent;
      RenderContext.Viewport := Rect;
      ApplicationProperties._GLContextEarlyOpen;
      EventOpen(TGLContext.InitializedContextsCount + SharedControlVulkanOpenCount);
      EventResize;
      Invalidate;
    end else
    begin
      AdjustContext(FPlatformContext);

      FPlatformContext.Initialize(FRequirements);

      FEffectiveDoubleBuffer := Requirements.DoubleBuffer;

      // initialize CGE OpenGL resources
      MakeContextCurrent;
      GLInformationInitialize;
      RenderContext.Viewport := Rect;
      ApplicationProperties._GLContextEarlyOpen;

      // Note that this will cause ApplicationProperties._GLContextOpen if necessary
      EventOpen(TGLContext.InitializedContextsCount + SharedControlVulkanOpenCount);
      EventResize;
      Invalidate;
    end;

    // do this even at design-time, to allow animating in Delphi IDE
    if {(not (csDesigning in ComponentState)) and} (not UpdatingEnabled) then
    begin
      UpdatingEnabled := true;
      UpdatingEnable;
    end;
  end;
end;

procedure TCastleControlContainer.AdjustContext(const PlatformContext: TGLContext);
begin
end;

procedure TCastleControlContainer.FinalizeContext;
begin
  if FGLInitialized then
  begin
    if FVulkanInitialized and (SharedControlVulkanDevice <> nil) then
    begin
      SharedControlVulkanDevice.WaitIdle;
      SharedControlVulkanDevice.ActiveSwapchainSurface := FVulkanSwapchainSurface;
    end;

    EventClose(TGLContext.InitializedContextsCount + SharedControlVulkanOpenCount);
    FGLInitialized := false;

    if FVulkanInitialized then
    begin
      FVulkanInitialized := False;
      if (SharedControlVulkanDevice <> nil) and (FVulkanSwapchainSurface <> nil) then
        SharedControlVulkanDevice.DestroySwapchainSurface(FVulkanSwapchainSurface)
      else
        FreeAndNil(FVulkanSwapchainSurface);
      if SharedControlVulkanOpenCount > 0 then
        Dec(SharedControlVulkanOpenCount);
      if SharedControlVulkanOpenCount = 0 then
      begin
        if RHIDevice = SharedControlVulkanDevice then
          SetRHIDevice(nil);
        if OwnsSharedControlVulkanDevice then
        begin
          FreeAndNil(SharedControlVulkanDevice);
          OwnsSharedControlVulkanDevice := False;
        end;
      end;
    end else
      FPlatformContext.Finalize;

    if UpdatingEnabled and
       ((TGLContext.InitializedContextsCount + SharedControlVulkanOpenCount) = 0) then
    begin
      UpdatingEnabled := false;
      UpdatingDisable;
    end;
  end;
  inherited;
end;

function TCastleControlContainer.GLInitialized: boolean;
begin
  Result := FGLInitialized;
end;

procedure TCastleControlContainer.DoRender;
begin
  MakeContextCurrent;

  EventBeforeRender;
  Fps.InternalRenderBegin;
  try
    if FVulkanInitialized and (FVulkanSwapchainSurface <> nil) then
    begin
      if BeginVulkanContainerFrame then
      begin
        try
          EventRender;
        finally
          EndVulkanContainerFrame;
        end;
      end;
    end else
    begin
      EventRender;
      if (GLVersion <> nil) and GLVersion.BuggySwapNonStandardViewport then
        RenderContext.Viewport := Rect;
      FPlatformContext.SwapBuffers;
    end;

    // Note that calling Invalidate from RenderContext is not allowed,
    // it doesn't play OK with LCL or VCL.
    // if AutoRedisplay then Invalidate;
  finally Fps.InternalRenderEnd end;
end;

procedure TCastleControlContainer.DoUpdate;
begin
  if AutoRedisplay then
    Invalidate;

  { Update event also requires that proper OpenGL context is current.

    This matters because OpenGL resources may be used durign update,
    e.g. TCastleScene.Update will update auto-generated textures,
    doing e.g. TGLGeneratedCubeMapTextureNode.Update.
    This should run in proper OpenGL context.
    Esp. as not all resources must be shared between contexts:
    FBO are not shared in new OpenGL versions, see
    https://stackoverflow.com/questions/4385655/is-it-possible-to-share-an-opengl-framebuffer-object-between-contexts-threads

    Testcase: open examples/mobile/simple_3d_demo/ in editor,
    open main design,
    click on previews with GeneratedCubeMap like castle_with_lights_and_camera.wrl .
    Without this fix, we'll have an OpenGL error.

    Doing MakeCurrent here is consistent with TCastleWindow.DoUpdate . }
  MakeContextCurrent;
  EventUpdate;
end;

procedure TCastleControlContainer.SetAutoRedisplay(const Value: boolean);
begin
  if FAutoRedisplay <> Value then
  begin
    FAutoRedisplay := value;
    if Value then
      Invalidate;
  end;
end;

class procedure TCastleControlContainer.DoUpdateEverything;

  procedure DoLimitFPS;
  var
    NowTime: TTimerResult;
    TimeRemainingFloat: Single;
  begin
    if ApplicationProperties.LimitFPS > 0 then
    begin
      NowTime := Timer;

      { When this is run for the 1st time, LastLimitFPSTime is zero,
        so NowTime - LastLimitFPSTime is huge, so we will not do any Sleep
        and only update LastLimitFPSTime.

        For the same reason, it is not a problem if you do not call DoLimitFPS
        often enough (for example, you do a couple of ProcessMessage calls
        without DoLimitFPS for some reason), or when user temporarily sets
        LimitFPS to zero and then back to 100.0.
        In every case, NowTime - LastLimitFPSTime will be large, and no sleep
        will happen. IOW, in the worst case --- we will not limit FPS,
        but we will *never* slow down the program when it's not really necessary. }

      TimeRemainingFloat :=
        { how long I should wait between _LimitFPS calls }
        1 / ApplicationProperties.LimitFPS -
        { how long I actually waited between _LimitFPS calls }
        TimerSeconds(NowTime, LastLimitFPSTime);
      { Don't do Sleep with too small values.
        It's better to have larger FPS values than limit,
        than to have them too small. }
      if TimeRemainingFloat > 0.001 then
      begin
        Sleep(Round(1000 * TimeRemainingFloat));
        LastLimitFPSTime := Timer;
      end else
        LastLimitFPSTime := NowTime;
    end;
  end;

var
  I: Integer;
  C: TCastleControlContainer;
begin
  { Be extra careful about processing here, so check ContainersList <> nil.

    As this may run after our
    "finalization" has run in case of VCL TCastleControl,
    when ReportMemoryLeaksOnShutdown:=true is used.
    In this case, the dialog shown by ReportMemoryLeaksOnShutdown happens
    after our "finalization", but before
    the TCastleControl.TContainer.UpdatingDisable from Vcl.CastleControl,
    so before the TCastleControl lost GL context.
    And during ReportMemoryLeaksOnShutdown, the WinAPI message loop runs,
    making the TCastleControl timer execute.
  }

  if ContainersList <> nil then
    for I := ContainersList.Count - 1 downto 0 do
    begin
      C := ContainersList[I];
      if C.GLInitialized then
        C.DoUpdate;
    end;
  if ApplicationProperties(false) <> nil then
  begin
    ApplicationProperties(false)._Update;

    { For unknown reason, DoLimitFPS causes weird application freezes on FMXLinux.
      Application update seems not to work, controls (both our OpenGL and
      regular FMX) seem occasionally not rendered (esp. after window resize),
      buttons react to clicks unreliably.

      Note: in case of Linux, this code is only used when Delphi + Linux + FMX.
      Because TCastleControlContainer is only used with Delphi for FMX and VCL,
      and on Linux only FMX is possible. }
    {$ifndef LINUX}
    DoLimitFPS;
    {$endif}
  end;

end;

procedure TCastleControlContainer.LoadDesign;

{ Note: implementation of LoadDesign, UnLoadDesign and friends follow similar
  methods in TCastleView. Here they are much simplified, as we have no concept
  of "started" / "stopped", so no DesignPreload too. }

  procedure FixApplicationDataInIDE;
  var
    ProjectPath: String;
  begin
    if CastleDesignMode and
       (csDesigning in ComponentState) and
       Assigned(OnGetDesignTimeProjectPath) then
    begin
      ProjectPath := OnGetDesignTimeProjectPath();

      { Override ApplicationData interpretation, and castle-data:/xxx URL meaning. }
      ApplicationDataOverride := FilenameToUriSafe(
        InclPathDelim(ProjectPath) + 'data' + PathDelim);
    end;
  end;

var
  OldCastleApplicationMode: TCastleApplicationMode;
begin
  if DesignUrl <> '' then
  begin
    { Make sure InternalCastleApplicationMode is correct, to
      - not e.g. do physics in Lazarus/Delphi form designer.
      - not show design-time stuff in DesignUrl loaded in CGE editor "help->system information".

      Note that we restore later InternalCastleApplicationMode.
      This way we avoid changing InternalCastleApplicationMode for future loads,
      when TCastleControl is used inside castle-editor.
      Testcase:
        in CGE editor:
        - open tools/castle-editor project
        - double click on demo design in data/demo_animation/
        - open help->system information (this uses TCastleControl too, with DesignUrl assigned)
        - close help->system information
        - close design
        - reopen design
    }
    OldCastleApplicationMode := InternalCastleApplicationMode;
    try
      if csDesigning in ComponentState then
        InternalCastleApplicationMode := appDesign
      else
        InternalCastleApplicationMode := appRunning;
      FixApplicationDataInIDE; // in case DesignUrl uses castle-data: protocol, which is most often the case

      FDesignLoadedOwner := TComponent.Create(nil);
      try
        FDesignLoaded := UserInterfaceLoad(DesignUrl, FDesignLoadedOwner);
      except
        { If loading design file failed, and we're inside form designer,
          merely report a warning. This allows deserializing LFMs with broken URLs. }
        on E: Exception do
        begin
          if CastleDesignMode then // looks at InternalCastleApplicationMode
          begin
            WritelnWarning('TCastleControl', 'Failed to load design "%s": %s', [
              UriDisplay(DesignUrl),
              ExceptMessage(E)
            ]);
            Exit;
          end else
            raise;
        end;
      end;
      Controls.InsertFront(FDesignLoaded);
    finally
      InternalCastleApplicationMode := OldCastleApplicationMode;
    end;
  end;
end;

procedure TCastleControlContainer.UnLoadDesign;
begin
  FreeAndNil(FDesignLoadedOwner);
  FDesignLoaded := nil; // freeing FDesignLoadedOwner must have freed this too
end;

procedure TCastleControlContainer.SetDesignUrl(const Value: String);
begin
  if FDesignUrl <> Value then
  begin
    UnLoadDesign;
    FDesignUrl := Value;
    LoadDesign;
  end;
end;

function TCastleControlContainer.DesignedComponent(const ComponentName: String;
  const Required: Boolean = true): TComponent;
begin
  if FDesignLoaded <> nil then
    Result := FDesignLoadedOwner.FindComponent(ComponentName)
  else
    Result := nil;

  if Required and (Result = nil) then
    raise EComponentNotFound.CreateFmt('Cannot find component named "%s" in design "%s"', [
      ComponentName,
      UriDisplay(DesignUrl)
    ]);
end;

initialization
  {
  // This is some way to debug behavior at design-time when this code runs as part of Delphi IDE.
  // Uncomment to just have WritelnLog / WritelnWarning be recorded in the indicated file.
  LogFileName := 'c:/cygwin64/home/michalis/log2.txt';
  InitializeLog;
  }

  ContainersList := TCastleControlContainerList.Create(false);
  //TODO: InitializeClipboard;
  //TODO: OnMainContainer := @TCastleControl(nil).GetMainContainer;
finalization
  //TODO: OnMainContainer := nil;
  if OwnsSharedControlVulkanDevice then
  begin
    FreeAndNil(SharedControlVulkanDevice);
    OwnsSharedControlVulkanDevice := False;
  end;
  FreeAndNil(ContainersList);
end.
