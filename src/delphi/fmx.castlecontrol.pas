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

{ Control with OpenGL context on a Delphi FMX form. }
unit Fmx.CastleControl;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Windows,
  FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Win, FMX.Memo,
  FMX.Types, UITypes,
  CastleGLVersion, CastleGLUtils, CastleVectors, CastleKeysMouse,
  CastleInternalContextBase, CastleInternalContextWgl, CastleInternalContainer;

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
        procedure AdjustContext(const PlatformContext: TGLContext); override;
        class procedure UpdatingEnable; override;
        class procedure UpdatingDisable; override;
      public
        constructor Create(AParent: TCastleControl); reintroduce;
        procedure Invalidate; override;
        function Width: Integer; override;
        function Height: Integer; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
      end;

    var
      FContainer: TContainer;
      FMousePosition: TVector2;

    function GetCurrentShift: TShiftState;
    procedure SetCurrentShift(const Value: TShiftState);

    { Current knowledge about shift state, based on Container.Pressed.

      Call this whenever you have new new shift state.

      Sometimes, releasing shift / alt / ctrl keys will not be reported
      properly to KeyDown / KeyUp. Example: opening a menu
      through Alt+F for "_File" will make keydown for Alt,
      but not keyup for it, and DoExit will not be called,
      so ReleaseAllKeysAndMouse will not be called.

      To counteract this, set this whenever Shift state is known,
      to update Container.Pressed when needed. }
    property CurrentShift: TShiftState
      read GetCurrentShift write SetCurrentShift;
  private
    procedure CreateHandle;
    procedure DestroyHandle;
  protected
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
    property TabStop default true;
    property TabOrder;
    property CanFocus default True;
  end;

procedure Register;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils, CastleUIControls, CastleInternalDelphiUtils,
  CastleLog;

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

procedure TCastleControl.TContainer.AdjustContext(const PlatformContext: TGLContext);
{$ifdef MSWINDOWS}
var
  WinContext: TGLContextWGL;
begin
  inherited;
  WinContext := PlatformContext as TGLContextWGL;
  WinContext.WndPtr :=
    (Parent.Presentation as TWinNativeGLControl).Handle;
  if WinContext.WndPtr = 0 then
    raise Exception.Create('Native handle not ready when calling TCastleControl.TContainer.AdjustContext');
  WinContext.h_Dc := GetWindowDC(WinContext.WndPtr);
{$else}
begin
{$endif}
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

  { In FMX, this causes adding WS_TABSTOP to Params.Style
    in TWinPresentation.CreateParams. So it is more efficient to call
    before we actually create window by setting ControlType. }
  TabStop := true;

  CanFocus := True;

  { Makes the Presentation be TWinNativeGLControl, which has HWND.
    Do this after FContainer is initialized, as it may call CreateHandle,
    which in turn requires FContainer to be created.

    Note that we cannnot do this at design-time (in Delphi IDE):

    - Switching to TControlType.Platform at design-time
      creates additional weird (visible in task bar, detached from form designer)
      window on Windows, alongside main Delphi IDE window.

      User can even close this window, causing crashes
      (later when closing the FMX form, there will be exception,
      because the Windows handle went away and FMX is not prepared for it).

    - We *can* create OpenGL context in this weird window, and render there...
      But all my experiments to attach it to form designer in Delphi IDE failed.
      Overriding TWinNativeGLControl.CreateParams to

      1. Params.Style := Params.Style or WS_CHILD
      2. Params.WndParent := ContainerHandle;
      3. CreateSubClass(Params, 'CastleControl');

      .. yield nothing.

      1 and 2 should indeed not be necessary, this is done by default by FMX,
      we have WS_CHILD by default.

      None of the above cause our rendering to be attached to Delphi IDE
      as you would expect.

    - FMX controls cannot render in native style at design-time it seems.

      That is also the case for TMemo or TEdit,
      their rendering can be native only at runtime (if you set their ControlType
      to platform), at design-time they just display the "styled" (non-native)
      along with an icon informing they will be native at runtime.

      See
      https://docwiki.embarcadero.com/RADStudio/Alexandria/en/FireMonkey_Native_Windows_Controls#Visual_Changes_to_Native_Windows_Controls
  }
  if not (csDesigning in ComponentState) then
    ControlType := TControlType.Platform;
end;

destructor TCastleControl.Destroy;
begin
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  { Do not create context at design-time.
    We don't even set "ControlType := TControlType.Platform" at design-time now
    (see constructor comments),
    this line only secures in case user would set ControlType in a TCastleControl
    descendant at design-time. }
  if csDesigning in ComponentState then
    Exit;

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
end;

procedure TCastleControl.Paint;
var
  R: TRectF;
begin
  { See our constructor comments:
    looks like native drawing at design-time in FMX is just not possible reliably. }

  if csDesigning in ComponentState then
  begin
    inherited;
    R := LocalRect;

    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := $A0909090;
    Canvas.FillRect(R, 1.0);

    Canvas.Fill.Color := TAlphaColors.Yellow;
    Canvas.FillText(R,
      'Run the project to see actual rendering of ' + Name + ' (' + ClassName + ')',
      true, 1.0, [], TTextAlign.Center);
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

function TCastleControl.GetCurrentShift: TShiftState;
begin
  Result := [];
  if Container.Pressed.Keys[keyShift] then
    Include(Result, ssShift);
  if Container.Pressed.Keys[keyAlt] then
    Include(Result, ssAlt);
  if Container.Pressed.Keys[keyCtrl] then
    Include(Result, ssCtrl);
end;

procedure TCastleControl.SetCurrentShift(const Value: TShiftState);
begin
  Container.Pressed.Keys[keyShift] := ssShift in Value;
  Container.Pressed.Keys[keyAlt  ] := ssAlt   in Value;
  Container.Pressed.Keys[keyCtrl ] := ssCtrl  in Value;
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

  CurrentShift := Shift; { do this after Pressed update above, and before *Event }

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

  CurrentShift := Shift;
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

  CurrentShift := Shift; { do this after Pressed update above, and before *Event }

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
  CurrentShift := Shift;

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
  CurrentShift := Shift;

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
