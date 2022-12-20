{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Control with OpenGL context on a Delphi FMX form. }
unit CastleFmxOpenGlControl;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Windows,
  FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Win, FMX.Memo,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  { Control rendering "Castle Game Engine" on FMX form. }
  TCastleFmxControl = class(TPresentedControl)
  private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FOnGlOpen, FOnGlPaint: TNotifyEvent;
    procedure CreateHandle;
    procedure DestroyHandle;
    procedure SetOnGlOpen(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;

    { Render callback. }
    property OnGlPaint: TNotifyEvent read FOnGlPaint write FOnGlPaint;

    { When OpenGL context is initialized. }
    property OnGlOpen: TNotifyEvent read FOnGlOpen write SetOnGlOpen;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleFmxControl
  ]);
end;

{ TWinNativeGLControl -------------------------------------------------------- }

type
  { Presentation for TCastleFmxControl.
    This class is necessary to manage WinAPI HWND associated with FMX control. }
  TWinNativeGLControl = class(TWinPresentation)
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    function CastleControl: TCastleFmxControl;
  end;

function TWinNativeGLControl.CastleControl: TCastleFmxControl;
begin
  Result := Control as TCastleFmxControl;
end;

procedure TWinNativeGLControl.CreateHandle;
begin
  inherited;
  { Looking at TWinNativeMemo.CreateHandle confirms this can be called with Handle = null }
  if Handle <> NullHWnd then
    CastleControl.CreateHandle;
end;

procedure TWinNativeGLControl.DestroyHandle;
begin
  if Handle <> NullHWnd then
    CastleControl.DestroyHandle;
  inherited;
end;

{ TCastleFmxControl ---------------------------------------------------- }

constructor TCastleFmxControl.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK

  { Makes the Presentation be TWinNativeGLControl, which has HWND.
    Do this after FContext is initialized, as it may call CreateHandle. }
  ControlType := TControlType.Platform;
end;

destructor TCastleFmxControl.Destroy;
begin
  FreeAndNil(FContext);
  inherited;
end;

procedure TCastleFmxControl.CreateHandle;
begin
  { Thanks to TWinNativeGLControl, we have Windows HWND for this control.
    This is necessary to create OpenGL context that only renders to this control.

    Note: The only other way in FMX to get HWND seems to be to get form HWND,
      WindowHandleToPlatform(Handle).Wnd
    but this is not useful for us (we don't want to always render to full window).
  }
  FContext.WndPtr := (Presentation as TWinNativeGLControl).Handle;
  FContext.h_Dc := GetWindowDC(FContext.WndPtr);

  FContext.ContextCreate(FRequirements);

  // TODO: below should be done by TCastleContainer

  // initialize CGE OpenGL resources
  FContext.MakeCurrent;
  ApplicationProperties._GLContextEarlyOpen;
  ApplicationProperties._GLContextOpen;
  GLInformationInitialize;

  // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;

  if Assigned(OnGlOpen) then
    OnGlOpen(Self);
end;

procedure TCastleFmxControl.DestroyHandle;
begin
  if FContext <> nil then
    FContext.ContextDestroy;
  inherited;
end;

procedure TCastleFmxControl.Paint;
begin
  inherited;
  if (FContext <> nil) and Assigned(OnGlPaint) then
  begin
    FContext.MakeCurrent;
    RenderContext.Viewport := Rectangle(0, 0, Round(Width), Round(Height));
    OnGlPaint(Self);
    FContext.SwapBuffers;
  end;
end;

procedure TCastleFmxControl.SetOnGlOpen(const Value: TNotifyEvent);
begin
  if not SameMethods(TMethod(FOnGlOpen), TMethod(Value)) then
  begin
    FOnGlOpen := Value;
    { Call OnOpen, if context already initialized when it is set\ }
    if FContext.h_GLRc <> 0 then
      if Assigned(OnGlOpen) then
        OnGlOpen(Self);
  end;
end;

initialization
  { Make TWinNativeGLControl used
    for TCastleFmxControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TCastleFmxControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TCastleFmxControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
end.
