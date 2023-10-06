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
  CastleInternalContextBase;

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
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure CreateContext;
    procedure DestroyContext;
  protected
    function DefinePresentationName: String; override;
  public
    OnPaint: TNotifyEvent;

    constructor Create(AOwner: TComponent); override;
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

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;
  end;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics,
  CastleLog, CastleUtils, CastleInternalFmxUtils, CastleInternalDelphiUtils;

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

procedure TOpenGLControl.CreateContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;
    // TODO: implement sharing of OpenGL contexts in this case
    // In CGE, all open contexts should share GL resources
    // FPlatformContext.SharedContext := AnyOtherOpenContext;
    ContextAdjustEarly(Self, FPlatformContext);
    FPlatformContext.ContextCreate(FRequirements);
    // Invalidate; // would be too early, CASTLE_WINDOW_FORM will do it later
  end;
end;

procedure TOpenGLControl.DestroyContext;
begin
  if FGLInitialized then
  begin
    FGLInitialized := false;
    FPlatformContext.ContextDestroy;
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

  TabStop := true;
  CanFocus := True;
  Assert(not (csDesigning in ComponentState)); // this is not ready for design-time
  ControlType := TControlType.Platform;
end;

procedure TOpenGLControl.CreateHandle;
begin
  CreateContext;
end;

procedure TOpenGLControl.DestroyHandle;
begin
  DestroyContext;
end;

procedure TOpenGLControl.Paint;
begin
  // We must have OpenGL context at this point,
  // and on Delphi/Linux there seems no way to register "on native handle creation".
  // TODO: We should make sure we get handle before some other events,
  // like update or mouse/key press.
  HandleNeeded;

  if Assigned(OnPaint) then
    OnPaint(Self);

  // inherited not needed, and possibly causes something unnecessary
end;

procedure TOpenGLControl.HandleNeeded;
begin
  ControlHandleNeeded(Self);

  {$if defined(LINUX)}
  { There seems to be no way to create a handle for something else
    than entire TCastleForm on FMXLinux.
    So here we just initialize the context immediately (it will use
    form's handle). }
  CreateHandle;

  // TODO: Where to call DestroyHandle
  {$endif}
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

{$ifdef MSWINDOWS}
initialization
  { Make TWinNativeOpenGLControl used
    for TOpenGLControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TOpenGLControl, TControlType.Platform, TWinPresentationProxy<TWinNativeOpenGLControl>);
{$endif}
end.
