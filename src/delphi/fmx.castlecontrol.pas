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
unit Fmx.CastleControl;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Windows,
  FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Win, FMX.Memo,
  CastleGLVersion, CastleGLUtils, CastleVectors, CastleKeysMouse,
  CastleInternalContextWgl, CastleInternalContainer;

type
  { Control rendering "Castle Game Engine" on FMX form. }
  TCastleControl = class(TPresentedControl)
  strict private
    type
      { Non-abstract implementation of TCastleContainer that cooperates with
        TCastleControl. }
      TContainer = class(TCastleContainerEasy)
      private
        Parent: TCastleControl;
      protected
        function GetMousePosition: TVector2; override;
        procedure SetMousePosition(const Value: TVector2); override;
        procedure AdjustContext(const AContext: TGLContextWGL); override;
      public
        constructor Create(AParent: TCastleControl); reintroduce;
        procedure Invalidate; override;
        function Width: Integer; override;
        function Height: Integer; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
        function Dpi: Single; override;
      end;

    var
      FContainer: TContainer;
  private
    procedure CreateHandle;
    procedure DestroyHandle;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { Access Castle Game Engine container properties and events,
      not specific for FMX. }
    property Container: TContainer read FContainer;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory, Types,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils, CastleUIControls;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

{ TWinNativeGLControl -------------------------------------------------------- }

type
  { Presentation for TCastleControl.
    This class is necessary to manage WinAPI HWND associated with FMX control. }
  TWinNativeGLControl = class(TWinPresentation)
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    function CastleControl: TCastleControl;
  end;

function TWinNativeGLControl.CastleControl: TCastleControl;
begin
  Result := Control as TCastleControl;
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

{ TCastleControl.TContainer ---------------------------------------------------}

constructor TCastleControl.TContainer.Create(AParent: TCastleControl);
begin
  inherited Create(AParent); // AParent must be a component Owner to show published properties of container in LFM
  Parent := AParent;
end;

procedure TCastleControl.TContainer.AdjustContext(const AContext: TGLContextWGL);
begin
  inherited;
  AContext.WndPtr := (Parent.Presentation as TWinNativeGLControl).Handle;
  AContext.h_Dc := GetWindowDC(AContext.WndPtr);
end;

function TCastleControl.TContainer.Dpi: Single;
begin
  Result := DefaultDpi;
end;

function TCastleControl.TContainer.GetMousePosition: TVector2;
begin
  // TODO
  Result := TVector2.Zero;
end;

procedure TCastleControl.TContainer.SetMousePosition(const Value: TVector2);
begin
  // TODO
end;

function TCastleControl.TContainer.Width: Integer;
begin
  Result := Round(Parent.Width);
end;

function TCastleControl.TContainer.Height: Integer;
begin
  Result := Round(Parent.Height);
end;

procedure TCastleControl.TContainer.Invalidate;
begin
  Parent.InvalidateRect(TRectF.Create(0, 0, Width, Height));
end;

procedure TCastleControl.TContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  // TODO
end;

{ TCastleControl ---------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;

  FContainer := TContainer.Create(Self);

  { Makes the Presentation be TWinNativeGLControl, which has HWND.
    Do this after FContainer is initialized, as it may call CreateHandle. }
  ControlType := TControlType.Platform;
end;

destructor TCastleControl.Destroy;
begin
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  { Thanks to TWinNativeGLControl, we have Windows HWND for this control now in
      (Presentation as TWinNativeGLControl).Handle
    This is used in AdjustContext and
    is necessary to create OpenGL context that only renders to this control.

    Note: The only other way in FMX to get HWND seems to be to get form HWND,
      WindowHandleToPlatform(Handle).Wnd
    but this is not useful for us (we don't want to always render to full window).
  }
  FContainer.CreateContext;
end;

procedure TCastleControl.DestroyHandle;
begin
  FContainer.DestroyContext;
  inherited;
end;

procedure TCastleControl.Paint;
begin
  inherited;
  FContainer.DoRender;
end;

initialization
  { Make TWinNativeGLControl used
    for TCastleControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
end.
