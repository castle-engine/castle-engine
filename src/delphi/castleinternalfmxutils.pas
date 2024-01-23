{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities specific to FMX in Castle Game Engine.
  This allows sharing of solutions between FMX TOpenGLControl and FMX TCastleControl. }
unit CastleInternalFmxUtils;

interface

uses FMX.Controls, FMX.Controls.Presentation, FMX.Types, UITypes,
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  {$ifdef LINUX} FMX.Platform.Linux, {$endif}
  CastleInternalContextBase, CastleVectors;

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

    Note: We could not make TCastleControl descend from TOpenGLControl on FMX
    (like we did on LCL), since the GL work of TCastleControl is partially
    done by the container (to be shared, in turn,
    with VCL and in future LCL implementations).
    So to have enough flexibility how to organize hierarchy, this is rather
    a separate class that is just created and used by both
    FMX TOpenGLControl and FMX TCastleControl. }
  TFmxOpenGLUtility = class
  {$ifdef LINUX}
  private
    GLAreaInitialized: Boolean;
    GLAreaGtk: Pointer;
    GLAreaRect: TVector4Integer; //< x, y, width, height
    DrawingAreaParent: Pointer;
  {$endif}
  public
    { Set before calling HandleNeeded.
      Cannot change during lifetime of this instance, for now. }
    Control: TPresentedControl;

    { Called, if assigned, after creation (OnHandleAfterCreateEvent)
      or before destruction (OnHandleBeforeDestroyEvent)
      of a native handle.

      @bold(This is called only on platforms where
      FMX Presentation is not available.)
      This also implies it is called only on platforms
      where our code creates the native handle we need,
      e.g. Gtk handle on Linux.

      In practice this means this is called now only on Delphi/Linux.

      On Delphi/Windows, use FMX Presentation features
      instead of register notifications when handle is created/
      destroyed. }
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

implementation

{$if defined(MSWINDOWS)}
  {$I castleinternalfmxutils_windows.inc}
{$elseif defined(LINUX)}
  {$I castleinternalfmxutils_linux.inc}
{$else}
  {$I castleinternalfmxutils_other_os.inc}
{$endif}

end.
