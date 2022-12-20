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
  { Control rendering OpenGL on FMX form.

    This control must always have "native style", which means
    it has ControlType = Platform. See FMX docs about native controls:
    https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
    Native controls are always on top of non-native controls.
  }
  TCastleFmxOpenGlControl = class(TPresentedControl)
  private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FOnGlPaint: TNotifyEvent;
    procedure SetOnGlPaint(const Value: TNotifyEvent);
  public
    property OnGlPaint: TNotifyEvent read FOnGlPaint write SetOnGlPaint;
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Requirements: TGLContextRequirements read FRequirements;
    procedure GLContextOpen;

    // todo
    //procedure AfterPaint; override;
//    procedure Painting; virtual;
//    procedure Paint; virtual;
//    procedure DoPaint; virtual;
//    procedure AfterPaint; virtual;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleFmxOpenGlControl
  ]);
end;

type
  { Presentation for TCastleFmxOpenGlControl.
    For now we don't need anything beyond base TWinPresentation. }
  TWinNativeGLControl = class(TWinPresentation)
  end;

constructor TCastleFmxOpenGlControl.Create(AOwner: TComponent);
begin
  inherited;

  // Makes the Presentation be TWinNativeGLControl, which has HWnd
  ControlType := TControlType.Platform;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  { Thanks to TWinNativeGLControl, we have Windows HWND for this control.
    This is necessary to create OpenGL context that only renders to this control.

    Note: The only other way in FMX to get HWND seems to be to get form HWND,
      WindowHandleToPlatform(Handle).Wnd
    but this is not useful for us (we don't want to always render to full window).
  }
  FContext.WndPtr := (Presentation as TWinNativeGLControl).Handle;
  FContext.h_Dc := GetWindowDC(FContext.WndPtr);
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK
end;

procedure TCastleFmxOpenGlControl.GLContextOpen;
begin
  FContext.ContextCreate(FRequirements);

  // initialize CGE OpenGL resources
  FContext.MakeCurrent;
  ApplicationProperties._GLContextEarlyOpen;
  ApplicationProperties._GLContextOpen;
  GLInformationInitialize;

  // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;
  RenderContext.Viewport := Rectangle(0, 0, Round(Width), Round(Height));
end;

procedure TCastleFmxOpenGlControl.Paint;
begin
  inherited;
  if Assigned(OnGlPaint) then
  begin
    FContext.MakeCurrent;
    OnGlPaint(Self);
    FContext.SwapBuffers;
  end;
end;

procedure TCastleFmxOpenGlControl.SetOnGlPaint(const Value: TNotifyEvent);
begin
  FOnGlPaint := Value;
end;

initialization
  { Make TWinNativeGLControl used
    for TCastleFmxOpenGlControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TCastleFmxOpenGlControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TCastleFmxOpenGlControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
end.
