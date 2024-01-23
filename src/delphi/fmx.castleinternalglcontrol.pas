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

{ Control with OpenGL context (and nothing else, like CGE container)
  on a Delphi FMX form.
  This is like TCastleControl, but stripped from all CGE container features.
  It is similar in concept to TOpenGLControl from LCL,
  though implementation is independent -- we use special tricks
  to get OpenGL context on FMX form.

  This is only internal for TCastleWindow in CASTLE_WINDOW_FORM case.
  So we don't register it, don't care about publishing stuff,
  don't care how it looks in Delphi IDE etc. }
unit Fmx.CastleInternalGLControl;

{$I castleconf.inc}

interface

uses // standard units
  {$ifdef MSWINDOWS} Windows, {$endif}
  SysUtils, Classes,
  // fmx
  {$ifdef MSWINDOWS} FMX.Presentation.Win, {$endif}
  FMX.Controls, FMX.Controls.Presentation, FMX.Types, UITypes,
  // cge
  CastleInternalContextBase, CastleInternalFmxUtils;

type
  { Control with OpenGL context (and nothing else, like CGE container)
    on a Delphi FMX form.
    This is like TCastleControl, but stripped from all CGE container features. }
  TOpenGLControl = class(TPresentedControl)
  private
    FRequirements: TGLContextRequirements;
    { Internal platform-specific context data and initialization/finalization.
      This is in contrast to TCastleContainer.Context, that is public
      and manages context properties that are cross-platform and available
      for all OpenGL(ES). }
    FPlatformContext: TGLContext;
    FGLInitialized: Boolean;
    FGLUtility: TFmxOpenGLUtility;
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure InitializeContext;
    procedure FinalizeContext;
  protected
    function DefinePresentationName: String; override;
    procedure DoRootChanging(const NewRoot: IRoot); override;
    procedure SetVisible(const AValue: Boolean); override;
  public
    OnPaint: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    procedure Invalidate; virtual;
    procedure SwapBuffers;
    procedure MakeCurrent;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;

    { If Handle not allocated yet, allocate it now.
      This makes sure we have OpenGL context created.
      Our OpenBackend must guarantee it, we want to initialize GLVersion
      afterwards etc. }
    procedure HandleNeeded;

    { Call this continuosly. }
    procedure Update;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;

    { Size in pixels, not scaled by anything.
      Such size can be passed e.g. to OpenGL viewport. }
    function PixelsWidth: Integer;
    function PixelsHeight: Integer;

    { Scaling of FMX reported mouse coordinates to pixels. }
    function MousePosScale: Single;

    { Pixels per inch, reported by system, affected by user OS-wide preferences
      like font scaling. }
    function PixelsPerInch: Single;
  end;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics,
  CastleLog, CastleUtils, CastleInternalDelphiUtils;

{$ifdef MSWINDOWS}

{ TWinNativeOpenGLControl -------------------------------------------------------- }

type
  { Presentation for TOpenGLControl.
    This class is necessary to manage WinAPI HWND associated with FMX control. }
  TWinNativeOpenGLControl = class(TWinPresentation)
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    function OpenGLControl: TOpenGLControl;
  end;

function TWinNativeOpenGLControl.OpenGLControl: TOpenGLControl;
begin
  Result := Control as TOpenGLControl;
end;

procedure TWinNativeOpenGLControl.CreateHandle;
begin
  inherited;
  { Looking at TWinNativeMemo.CreateHandle confirms this can be called with Handle = null }
  if Handle <> NullHWnd then
    OpenGLControl.CreateHandle;
end;

procedure TWinNativeOpenGLControl.DestroyHandle;
begin
  if Handle <> NullHWnd then
    OpenGLControl.DestroyHandle;
  inherited;
end;

{$endif}

{ TOpenGLControl ------------------------------------------------------------ }

procedure TOpenGLControl.InitializeContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;
    FGLUtility.ContextAdjustEarly(FPlatformContext);
    FPlatformContext.Initialize(FRequirements);
    // Invalidate; // would be too early, CASTLE_WINDOW_FORM will do it later
  end;
end;

procedure TOpenGLControl.FinalizeContext;
begin
  if FGLInitialized then
  begin
    FGLInitialized := false;
    FPlatformContext.Finalize;
  end;
  inherited;
end;

constructor TOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FPlatformContext := ContextCreateBestInstance;

  FGLUtility := TFmxOpenGLUtility.Create;
  FGLUtility.Control := Self;
  FGLUtility.OnHandleAfterCreateEvent := CreateHandle;
  FGLUtility.OnHandleBeforeDestroyEvent := DestroyHandle;

  TabStop := true;
  CanFocus := True;
  Assert(not (csDesigning in ComponentState)); // this is not ready for design-time
  ControlType := TControlType.Platform;
end;

destructor TOpenGLControl.Destroy;
begin
  FreeAndNil(FPlatformContext);
  FreeAndNil(FGLUtility);
  inherited;
end;

procedure TOpenGLControl.CreateHandle;
begin
  InitializeContext;
end;

procedure TOpenGLControl.DestroyHandle;
begin
  FinalizeContext;
end;

procedure TOpenGLControl.Paint;
begin
  { We must have OpenGL context at this point,
    and on Delphi/Linux there is no way to register "on native handle creation",
    we must manually perform native handle creation (with GL context)
    using our FGLUtility.

    Maybe in the future it will make sense to do this also from some other events,
    like update or mouse/key press?
    Doesn't seem necessary in practice for now.

    The TOpenGLControl is only used from CASTLE_WINDOW_FORM in practice,
    which always calls HandleNeeded manually after creation anyway. }
  HandleNeeded;

  if Assigned(OnPaint) then
    OnPaint(Self);

  // inherited not needed, and possibly causes something unnecessary
end;

procedure TOpenGLControl.HandleNeeded;
begin
  FGLUtility.HandleNeeded;
end;

function TOpenGLControl.DefinePresentationName: String;
begin
  Result := 'OpenGLControl-' + GetPresentationSuffix;
end;

procedure TOpenGLControl.Invalidate;
begin
  InvalidateRect(TRectF.Create(0, 0, Width, Height));
end;

procedure TOpenGLControl.SwapBuffers;
begin
  FPlatformContext.SwapBuffers;
end;

procedure TOpenGLControl.MakeCurrent;
begin
  FPlatformContext.MakeCurrent;
end;

procedure TOpenGLControl.Update;
begin
  FGLUtility.Update;
end;

function TOpenGLControl.PixelsWidth: Integer;
begin
  Result := Round(Width * FGLUtility.Scale);
end;

function TOpenGLControl.PixelsHeight: Integer;
begin
  Result := Round(Height * FGLUtility.Scale);
end;

function TOpenGLControl.MousePosScale: Single;
begin
  { Note that FGLUtility.Scale seems always 1.0 when this is used
    with Windows from CASTLE_WINDOW_FORM.
    Though Windows usage through TCastleControl shows that we need to scale
    mouse coordinates and sizes by FGLUtility.Scale.
    See also src\window\castlewindow_form_fmx_scaling.md .

    On Linux, both usage here (from CASTLE_WINDOW_FORM)
    and in TCastleControl may actually show non-1.0 scaling (configure scaling
    in GNOME to make it happen) and has to be accounted.

    It seems that correct and cross-platform approach is just to always
    scale, if we want to convert FMX sizes/coordinates to pixel sizes/coordinates
    needed by CGE.
  }

  Result := FGLUtility.Scale;
end;

procedure TOpenGLControl.SetVisible(const AValue: Boolean);
begin
  inherited;
  { This happens e.g. when developer does "CastleControl.Visible := false".
    (Not actually used by castlewindow_form.inc for now.)
    We will recreate this handle later by FGLUtility.HandleNeeded,
    at first paint. }
  if not AValue then
    FGLUtility.HandleRelease;
end;

procedure TOpenGLControl.DoRootChanging(const NewRoot: IRoot);
begin
  inherited;
  { This happens e.g. when developer does "CastleControl.Parent := nil"
    or when TOpenGLControl is destroyed.
    We will recreate this handle later by FGLUtility.HandleNeeded,
    at first paint. }
  FGLUtility.HandleRelease;
end;

function TOpenGLControl.PixelsPerInch: Single;
const
  { Default value for container's Dpi, as is usually set on desktops. }
  DefaultDpi = 96.0;
begin
  Result := FGLUtility.Scale * DefaultDpi;
end;

{$ifdef MSWINDOWS}
initialization
  { Make TWinNativeOpenGLControl used
    for TOpenGLControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
{$endif}
end.
