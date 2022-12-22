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
  FMX.Types, UITypes,
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
        class var
          UpdatingTimer: TTimer;
        class procedure UpdatingTimerEvent(Sender: TObject);
      protected
        function GetMousePosition: TVector2; override;
        procedure SetMousePosition(const Value: TVector2); override;
        procedure AdjustContext(const AContext: TGLContextWGL); override;
        class procedure UpdatingEnable; override;
        class procedure UpdatingDisable; override;
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
      FMousePosition: TVector2;

    { Call whenever you have new knowledge about new shift state.

      Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, call this method when Shift state is known,
      to update Pressed when needed. }
    procedure UpdateShiftState(const Shift: TShiftState);
  private
    procedure CreateHandle;
    procedure DestroyHandle;
  protected
    { // TODO
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { This control must always have "native style", which means
      it has ControlType = Platform. See FMX docs about native controls:
      https://docwiki.embarcadero.com/RADStudio/Sydney/en/FireMonkey_Native_Windows_Controls
      Native controls are always on top of non-native controls. }
    property ControlType default TControlType.Platform;
  published
    { Access Castle Game Engine container properties and events,
      not specific for FMX. }
    property Container: TContainer read FContainer;

    property Align;
    property Anchors;
    property OnClick;
    property OnDblClick;
    property Height;
    property Width;
    property Size;
    property Position;
    property Margins;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory, Types,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils, CastleUIControls, CastleInternalDelphiUtils;

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
  Result := Parent.FMousePosition;
end;

procedure TCastleControl.TContainer.SetMousePosition(const Value: TVector2);
begin
  // TODO
end;

function TCastleControl.TContainer.Width: Integer;
{ // Using LocalToScreen doesn't help to counteract the FMX scale
var
  P: TPointF;
begin
  P := Parent.LocalToScreen(TPointF.Create(Parent.Width, 0));
  Result := Round(P.X);
end;
}
var
  Scale: Single;
begin
  // this may be called at units finalization, when Handle is no longer available
  if Parent.Presentation <> nil then
    Scale := (Parent.Presentation as TWinNativeGLControl).Scale
  else
    Scale := 1;

  Result := Round(Parent.Width * Scale);
end;

function TCastleControl.TContainer.Height: Integer;
var
  Scale: Single;
begin
  // this may be called at units finalization, when Handle is no longer available
  if Parent.Presentation <> nil then
    Scale := (Parent.Presentation as TWinNativeGLControl).Scale
  else
    Scale := 1;

  Result := Round(Parent.Height * Scale);
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
  FContainer.SetSubComponent(true);
  FContainer.Name := 'Container';

  { Makes the Presentation be TWinNativeGLControl, which has HWND.
    Do this after FContainer is initialized, as it may call CreateHandle. }
  ControlType := TControlType.Platform;

  // TODO: is this necessary to receive keys?
  TabStop := true;
end;

destructor TCastleControl.Destroy;
begin
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  inherited;

  { For now, FMX TCastleControl doesn't create a context at design-time, because
    - painting doesn't work correctly at design-time, it is shifted
    - there are weird errors (infinite range check error or wglMakeCurrent errors),
      if you close a weird nameless non-modal window that appears when FMX project is open.
  }
  if not (csDesigning in ComponentState) then
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
  if not (csDesigning in ComponentState) then
    FContainer.DestroyContext;
  inherited;
end;

procedure TCastleControl.Paint;
begin
  // TODO: at design-time, FMX TCastleControl is displayed at wrong position
  if csDesigning in ComponentState then
  begin
    inherited;
  end else
  begin
    // inherited not needed, and possibly causes something unnecessary
    FContainer.DoRender;
  end;
end;

class procedure TCastleControl.TContainer.UpdatingEnable;
begin
  inherited;
  UpdatingTimer := TTimer.Create(nil);
  UpdatingTimer.Interval := 1;
  UpdatingTimer.OnTimer := {$ifdef FPC}@{$endif} UpdatingTimerEvent;
end;

class procedure TCastleControl.TContainer.UpdatingDisable;
begin
  FreeAndNil(UpdatingTimer);
  inherited;
end;

class procedure TCastleControl.TContainer.UpdatingTimerEvent(Sender: TObject);
begin
  DoUpdateEverything;
end;

procedure TCastleControl.UpdateShiftState(const Shift: TShiftState);
begin
  Container.Pressed.Keys[keyShift] := ssShift in Shift;
  Container.Pressed.Keys[keyAlt  ] := ssAlt   in Shift;
  Container.Pressed.Keys[keyCtrl ] := ssCtrl  in Shift;
end;

procedure TCastleControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  MyButton: TCastleMouseButton;
begin
  if not IsFocused then // TODO: doesn't seem to help with focus
    SetFocus;

  inherited; { FMX OnMouseDown before our callbacks }

  FMousePosition := Vector2(X, Height - 1 - Y);

  if MouseButtonToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed + [MyButton];

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  if MouseButtonToCastle(Button, MyButton) then
    Container.EventPress(InputMouseButton(FMousePosition, MyButton, 0,
      ModifiersDown(Container.Pressed)));
end;

procedure TCastleControl.MouseMove(Shift: TShiftState; NewX, NewY: Single);
begin
  inherited;

  Container.EventMotion(InputMotion(FMousePosition,
    Vector2(NewX, Height - 1 - NewY), Container.MousePressed, 0));

  // change FMousePosition *after* EventMotion, callbacks may depend on it
  FMousePosition := Vector2(NewX, Height - 1 - NewY);

  UpdateShiftState(Shift);
end;

procedure TCastleControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  MyButton: TCastleMouseButton;
begin
  inherited; { FMX OnMouseUp before our callbacks }

  FMousePosition := Vector2(X, Height - 1 - Y);

  if MouseButtonToCastle(Button, MyButton) then
    Container.MousePressed := Container.MousePressed - [MyButton];

  UpdateShiftState(Shift); { do this after Pressed update above, and before *Event }

  if MouseButtonToCastle(Button, MyButton) then
    Container.EventRelease(InputMouseButton(FMousePosition, MyButton, 0));
end;

procedure TCastleControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  if not Handled then
    Handled := Container.EventPress(InputMouseWheel(
      FMousePosition, WheelDelta / 120, true, ModifiersDown(Container.Pressed)));

  inherited;
end;

procedure TCastleControl.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  UpdateShiftState(Shift);

  if KeyToCastle(Key, Shift, CastleKey, CastleKeyString) then
  begin
    CastleEvent := InputKey(FMousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    if KeyChar <> #0 then
      CastleEvent.KeyString := KeyChar;

    // check this before updating Container.Pressed
    CastleEvent.KeyRepeated :=
      // Key already pressed
      ((CastleEvent.Key = keyNone) or Container.Pressed.Keys[CastleEvent.Key]) and
      // KeyString already pressed
      ((CastleEvent.KeyString = '') or Container.Pressed.Strings[CastleEvent.KeyString]);

    Container.Pressed.KeyDown(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventPress(CastleEvent) then
    begin
      Key := 0;
      KeyChar := #0;
    end;
  end;
end;

procedure TCastleControl.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  UpdateShiftState(Shift);

  if KeyToCastle(Key, Shift, CastleKey, CastleKeyString) then
  begin
    CastleEvent := InputKey(FMousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    if KeyChar <> #0 then
      CastleEvent.KeyString := KeyChar;

    Container.Pressed.KeyUp(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventRelease(CastleEvent) then
    begin
      Key := 0;
      KeyChar := #0;
    end;
  end;
end;

initialization
  { Make TWinNativeGLControl used
    for TCastleControl with ControlType = TControlType.Platform. }
  TPresentationProxyFactory.Current.Register(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
finalization
  TPresentationProxyFactory.Current.Unregister(TCastleControl, TControlType.Platform, TWinPresentationProxy<TWinNativeGLControl>);
end.
