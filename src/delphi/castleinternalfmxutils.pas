{
  Copyright 2022-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specific to FMX in Castle Game Engine.
  This allows sharing of solutions between FMX TOpenGLControl and FMX TCastleControl. }
unit CastleInternalFmxUtils;

{$I castleconf.inc}

{ Platforms using TGLContextExisting with FMX (FireMonkey) for
  the rendering context.
  In other words, just relying that FMX will create context for us. }
{$if defined(DELPHI) and (defined(ANDROID) or defined(IOS))}
  {$define CASTLE_MOBILE_FMX}
{$endif}

interface

uses FMX.Controls, FMX.Controls.Presentation, FMX.Types, FMX.Graphics,
  UITypes,
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  {$ifdef LINUX} FMX.Platform.Linux, {$endif}
  {$ifdef CASTLE_MOBILE_FMX} CastleGLES, {$endif}
  CastleInternalContextBase, CastleVectors, CastleRenderContext;

type
  THandleEvent = procedure of object;

  { Utility for FMX controls to help them initialize OpenGL context.

    This tries to abstract as much as possible the platform-specific ways
    how to get OpenGL context (and native handle) on a given control,
    in a way that is useful for both FMX TOpenGLControl and FMX TCastleControl.

    The current state:

    - On Windows: make sure native Windows handle is initialized when necessary,
      and pass it to TGLContextWgl.

    - On Linux: we have to create our own Gtk widget (since FMXLinux only ever
      creates native handle for the whole form, it seems).
      And insert it into FMX form, keeping the existing FMX drawing area too.
      And then use TGLContextEgl to connect to our own Gtk widget.

    - On other platforms: just rely on FMX to create / release the OpenGL context.
      This makes sense on Delphi/Android and Delphi/iOS.

    Note: We could not make TCastleControl descend from TOpenGLControl on FMX
    (like we did on LCL), since the GL work of TCastleControl is partially
    done by the container (to be shared, in turn,
    with VCL and in future LCL implementations).
    So to have enough flexibility how to organize hierarchy, this is rather
    a separate class that is just created and used by both
    FMX TOpenGLControl and FMX TCastleControl. }
  TFmxOpenGLUtility = class

  {$define read_TFmxOpenGLUtility_interface}
    {$if defined(MSWINDOWS)}
      {$I castleinternalfmxutils_windows.inc}
    {$elseif defined(LINUX)}
      {$I castleinternalfmxutils_linux.inc}
    {$else}
      {$I castleinternalfmxutils_other_os.inc}
    {$endif}
  {$undef read_TFmxOpenGLUtility_interface}

  public
    { Set before calling HandleNeeded.
      Cannot change during lifetime of this instance, for now. }
    Control: TPresentedControl;

    { Called, if assigned, after creation (OnHandleAfterCreateEvent)
      or before destruction (OnHandleBeforeDestroyEvent)
      of a native handle.

      @bold(This is called only on platforms where
      FMX Presentation is not available.)
      This means platforms where

      - our code creates the native handle (in general:
        any system-specific resources) we need.
        E.g. Gtk handle on Linux, created by Delphi/Linux.

      - or when there's no need to create anything..E.g. FMX on Android or iOS
        just uses the existing context, so we don't need to create anything more.
        So this applies to Delphi/Android and Delphi/iOS.

      TODO: Delphi/macOS: to be figured out.

      In contrast, on platforms where FMX Presentation is available
      (like Delphi/Windows), we use FMX Presentation features,
      and we don't need extra notifications from this class when handle
      is created/destroyed. }
    OnHandleAfterCreateEvent: THandleEvent;
    OnHandleBeforeDestroyEvent: THandleEvent;

    { Is initializing internal resources, needed by HandleNeeded,
      possible now. Use this if calling HandleNeeded is not necessary now,

      This does something only on platforms where
      FMX Presentation is not available and we need to take care ourselves
      of the necessary internal per-control handle where OpenGL context
      exists. In practice: only on Delphi/Linux now.

      On other platforms, it always returns true. }
    // Not needed in the end // function HandlePossible: Boolean;

    { Make sure that Control has initialized internal handle,
      necessary to later initialize OpenGL context for this.
      This must be called before ContextAdjustEarly.

      - On some platforms, like Windows,
        creating a handle should provoke ContextAdjustEarly
        and TGLContext.Initialize.
        The caller should make it happen: see
        TPresentationProxyFactory.Current.Register and our presentation classes.
        This works nicely when FMX platform defines
        Presentation and classes like TWinPresentation.

      - On other platforms, like Linux, we create handle ourselves.
        Manually make sure context is created
        after handle is obtained.
        Needed for Delphi/Linux that doesn't define any "presentation"
        stuff and only creates a handle for the entire form. }
    procedure HandleNeeded;

    { Release handle that was created by @link(HandleNeeded).

      This does something only on platforms where
      FMX Presentation is not available and we need to take care ourselves
      of the necessary internal per-control handle where OpenGL context
      exists. }
    procedure HandleRelease;

    { Adjust TGLContext parameters before calling TGLContext.CreateContext.
      You must call HandleNeeded earlier.

      Extracts platform-specific bits from given FMX Control,
      and puts in platform-specific bits of TGLContext descendants.
      This is used by TCastleControl and TOpenGLControl.
      It does quite low-level and platform-specific job, dictated
      by the necessity of how FMX works, to be able to get context.

      This is synchronized with what ContextCreateBestInstance does on this platform. }
    procedure ContextAdjustEarly(const PlatformContext: TGLContext);

    { Call this often to perform platform-specific adjustments.

      At this point, this is required on Linux: we need to synchronize
      internal GTK control with desired position and size of FMX TCastleControl. }
    procedure Update;

    { Size reported by FMX controls needs to be multiplied by this
      to get size in physical pixels (which we need e.g. for OpenGL context). }
    function Scale: Single;
  end;

const
  { On some platforms (Windows), the engine control must always have "native style",
    which means it has ControlType = Platform. See FMX docs about native controls:
    https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
    Native controls are always on top of non-native controls.

    On other platforms (Android and iOS), it must be Styled
    (which is also default for FMX controls).
    TControlType.Platform for mobile would cause creation of "native view"
    that intercepts touches, making OnTouch not work properly. }
  DefaultControlType =
    {$if (not defined(ANDROID)) and (not defined(IOS))}
      TControlType.Platform
    {$else}
      TControlType.Styled
    {$endif};

type
  { Utility to help with rendering OpenGL in FMX controls. }
  TFmxOpenGLRenderingUtility = record
  strict private
    SavedViewport: array [0..3] of TGLint;
  public
    { Perform necessary preparations before direct OpenGL(ES) rendering
      that must cooperate with FMX's rendering state. }
    procedure BeforeDirectRendering(const Canvas: TCanvas;
      const RenderContext: TRenderContext);

    { Perform necessary cleanup after direct OpenGL(ES) rendering
      that must cooperate with FMX's rendering state. }
    procedure AfterDirectRendering(const Canvas: TCanvas;
      const RenderContext: TRenderContext);
  end;

implementation

uses SysUtils,
  {$ifdef CASTLE_MOBILE_FMX}
  FMX.Canvas.GPU, FMX.Types3D,
  {$endif CASTLE_MOBILE_FMX}
  CastleInternalGLUtils;

{ TFmxOpenGLUtility ---------------------------------------------------------- }

{$define read_implementation}
{$if defined(MSWINDOWS)}
  {$I castleinternalfmxutils_windows.inc}
{$elseif defined(LINUX)}
  {$I castleinternalfmxutils_linux.inc}
{$else}
  {$I castleinternalfmxutils_other_os.inc}
{$endif}

{ TFmxOpenGLRenderingUtility ------------------------------------------------- }

procedure TFmxOpenGLRenderingUtility.BeforeDirectRendering(const Canvas: TCanvas;
  const RenderContext: TRenderContext);
begin
  {$ifdef CASTLE_MOBILE_FMX}
  { We need TCustomCanvasGpu. Raise (early) if this is not the case.
    TODO: Confirm: This will likely happen if you try to use CGE with Skia
    on mobile. }
  if not (Canvas is TCustomCanvasGpu) then
    raise Exception.CreateFmt('Canvas class unsupported: %s, we cannot render using Castle Game Engine',
      [Canvas.ClassName]);
  {$endif}

  { Flush the FMX canvas BEFORE calling any raw GL.
    FMX batches draw calls; if we call glDrawArrays while FMX still has
    pending batched commands the interleaving causes visual corruption.
    Testcase: run on Android, using Delphi, e.g. platformer (or any other demo)
    -- without this, it looks like our rendering is ignored. }
  Canvas.Flush;

  {$ifdef CASTLE_MOBILE_FMX}
  { Clear scissor, matching RenderContext.
    TODO: Instead, read current scissor, and make RenderContext aware of it.
    Note that at end, PopContextStates will restore the previous
    scissor state, so FMX scissor state is already restored OK. }
  glDisable(GL_SCISSOR_TEST);

  { Save + restore FMX viewport.
    FMX sets this once at frame start (TContextAndroid.DoBeginScene,
    TContextIOS.DoBeginScene) and controls rendering expect it. }
  glGetIntegerv(GL_VIEWPORT, @SavedViewport[0]);

  RenderContext.SynchronizeState;
  {$endif}
end;

procedure TFmxOpenGLRenderingUtility.AfterDirectRendering(const Canvas: TCanvas;
  const RenderContext: TRenderContext);
{$ifdef CASTLE_MOBILE_FMX}
var
  Ctx: TContext3D;
{$endif}
begin
  {$ifdef CASTLE_MOBILE_FMX}
  // Restore FMX viewport
  glViewport(SavedViewport[0], SavedViewport[1], SavedViewport[2], SavedViewport[3]);

  { Clean state, that FMX never touches, just assumes it is clean. }

  { FMX doesn't draw using VBOs, make sure it doesn't access our
    by accident. }
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  RenderContext.CurrentVao := nil; // paranoid; this does nothing on OpenGLES
  RenderContext.CurrentProgram := nil;

  { FMX never sets glStencilMask, but it can do
    glEnable(GL_STENCIL_TEST), and glClear -> assuming glStencilMask.
    Hm, although CGE doesn't call glStencilMask now, so it doesn't matter. }
  glStencilMask($FFFFFFFF);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, 0); // make sure our texture does not leak

  { Check GL errors after all CGE rendering.
    Don't limit this to debug-only, as FMX does glGetError always,
    see TGlesDiagnostic.RaiseIfHasError .
    So it's better to catch GL errors at the end of CGE rendering,
    and report them as CGE rendering issues,
    not let FMX worry about them (and warn/abort depending on
    OpenGlErrorReporting).

    TODO: CheckGLErrorsAll.
    Like CheckGLErrors, but calls glGetError repeatedly until all errors are
    collected, and glGetError reports GL_NO_ERROR.
    Then we make exception or log, about all collected errors,
    just like CheckGLErrors.
  }
  try
    CheckGLErrors('after Castle Game Engine rendering (in the middle of FMX rendering)');
  finally
    { Make OpenGLES context state correspond to what FMX thinks
      it should be (TContext3D.CurrentStates).

      We do this by TContext3D.ResetStates + Ctx.PopContextStates.
      - TContext3D.ResetStates sets TContext3D.CurrentStates
        "unknown state now"
      - Ctx.PopContextStates will execute necessary
        TCustomContextOpenGL.DoSetContextState that make
        all OpenGLES calls.
        It also sets proper scissor.

      This way rest of FMX rendering (even if FMX renders more controls
      right after Castle Game Engine FMX control) will use correct
      state.

      Note that placement of Ctx.PushContextStates doesn't matter much.
      We could do it before "try", before all CGE rendering.
      But actually nothing changes FMX cached state (CurrentStates),
      so we may as well just do PushContextStates here.
      We really call PushContextStates only to pair it with PopContextStates.
    }
    Ctx := TCustomCanvasGpu(Canvas).Context;
    Ctx.PushContextStates;
    TContext3D.ResetStates;
    Ctx.PopContextStates;
  end;

  {$endif CASTLE_MOBILE_FMX}
end;

end.
