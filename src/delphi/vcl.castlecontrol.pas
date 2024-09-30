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

{ Control with OpenGL context on a Delphi VCL form. }
unit Vcl.CastleControl;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Vcl.Controls, Vcl.ExtCtrls, Types,  WinApi.Messages,
  CastleGLVersion, CastleGLUtils,
  CastleInternalContextBase, CastleInternalContextWgl, CastleInternalContainer,
  CastleVectors, CastleKeysMouse;

type
  { Control rendering OpenGL on VCL form. }
  TCastleControl = class(TCustomControl)
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
        procedure SystemSetMousePosition(const Value: TVector2); override;
        procedure AdjustContext(const PlatformContext: TGLContext); override;
        class procedure UpdatingEnable; override;
        class procedure UpdatingDisable; override;
      public
        constructor Create(AParent: TCastleControl); reintroduce;
        procedure Invalidate; override;
        function PixelsWidth: Integer; override;
        function PixelsHeight: Integer; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
      end;

    var
      FContainer: TContainer;

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
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    { // TODO
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; NewX, NewY: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    { To handle some special keys, set on form KeyPreview
      and call these methods from VCL form's OnKeyDown / OnKeyUp. }
    procedure PreviewFormKeyDown(var Key: Word; Shift: TShiftState);
    procedure PreviewFormKeyUp(var Key: Word; Shift: TShiftState);
  published
    { Access Castle Game Engine container properties and events,
      not specific for FMX. }
    property Container: TContainer read FContainer;

    property Align;
    property Anchors;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
  end;

procedure Register;

implementation

uses Windows,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUIControls, CastleInternalDelphiUtils;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
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
  WinContext.WndPtr := Parent.Handle;
  WinContext.h_Dc := GetWindowDC(WinContext.WndPtr);
{$else}
begin
{$endif}
end;

class procedure TCastleControl.TContainer.UpdatingEnable;
begin
  inherited;
  UpdatingTimer := TTimer.Create(nil);
  if csDesigning in UpdatingTimer.ComponentState then
    { At design-time, limit FPS.
      Otherwise even Delphi IDE may become sluggish on very old GPUs.
      Observed on old laptops with Delphi 10.2.3 and 10.4 on Win64. }
    UpdatingTimer.Interval := Round(1000 / 60)
  else
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

function TCastleControl.TContainer.PixelsWidth: Integer;
begin
  Result := Parent.Width;
end;

procedure TCastleControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

function TCastleControl.TContainer.PixelsHeight: Integer;
begin
  Result := Parent.Height;
end;

procedure TCastleControl.TContainer.Invalidate;
begin
  Parent.Invalidate;
end;

procedure TCastleControl.TContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  // TODO
end;

procedure TCastleControl.TContainer.SystemSetMousePosition(const Value: TVector2);
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

  // needed to receive keys, like AWSD, in TCastleControl
  TabStop := true;
end;

destructor TCastleControl.Destroy;
begin
  { Looks like VCL doesn't (always?) call DestroyHandle.
    While FContainer would be destroyed anyway,
    but we need TCastleContainerEasy.DestroyContext call,
    to e.g. make ApplicationProperties.OnGLContextClose call,
    that frees various CGE things when last GL context is released. }

  if FContainer.GLInitialized then
    FContainer.FinalizeContext;
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  inherited;
  { Handle is only available now, in CreateHandle.
    So only now call FContainer.CreateContext that does FContainer.AdjustContext. }
  FContainer.InitializeContext;
end;

procedure TCastleControl.DestroyHandle;
begin
  FContainer.FinalizeContext;
  inherited;
end;

function TCastleControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Scroll: Single;
  Vertical: Boolean;
begin
  Scroll := WheelDelta / 120;
  Vertical := true; // only vertical scrolling is reported here
  Result := Container.EventPress(InputMouseWheel(
    Container.MousePosition, Scroll, Vertical, ModifiersDown(Container.Pressed)));
  if Result then Exit;

  Result := inherited;
end;

procedure TCastleControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  CastleButton: TCastleMouseButton;
begin
  { Focus on click, to receive e.g. AWSD keys in TCastleControl.
    Testcase: examples/delphi/vcl with TCastleWalkNavigation in 3D view,
    should handle AWSD without the need to do anything else. }
  if not Focused then
    SetFocus;

  inherited; { VCL OnMouseDown before our callbacks }

  { This updates Container.Pressed.
    Do this before using ModifiersDown(Container.Pressed) below. }
  UpdateShiftState(Shift);

  if MouseButtonToCastle(Button, CastleButton) then
  begin
    Container.EventPress(InputMouseButton(Vector2(X, Height - 1 - Y),
      CastleButton, 0, ModifiersDown(Container.Pressed)));
  end;
end;

procedure TCastleControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  CastleButton: TCastleMouseButton;
begin
  inherited; { VCL OnMouseUp before our callbacks }

  { This updates Container.Pressed.
    Do this before using ModifiersDown(Container.Pressed) below. }
  UpdateShiftState(Shift);

  if MouseButtonToCastle(Button, CastleButton) then
  begin
    Container.EventRelease(InputMouseButton(Vector2(X, Height - 1 - Y),
      CastleButton, 0, ModifiersDown(Container.Pressed)));
  end;
end;

procedure TCastleControl.MouseMove(Shift: TShiftState; NewX, NewY: Integer);
begin
  inherited;

  { This updates Container.Pressed.
    Do this before using ModifiersDown(Container.Pressed) below. }
  UpdateShiftState(Shift);

  Container.EventMotion(InputMotion(Container.MousePosition,
    Vector2(NewX, Height - 1 - NewY), Container.MousePressed, 0));
end;

procedure TCastleControl.Paint;
begin
  //inherited; // inherited not needed, and possibly causes something unnecessary
  FContainer.DoRender;
end;

procedure TCastleControl.PreviewFormKeyDown(var Key: Word; Shift: TShiftState);
begin
  if //Focused and // TODO: It seems we are never Focused
     (
       (Key = VK_Down) or
       (Key = VK_Up) or
       (Key = VK_Left) or
       (Key = VK_Right) or
       (Key = VK_Space)
     ) then
  begin
    KeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TCastleControl.PreviewFormKeyUp(var Key: Word; Shift: TShiftState);
begin
  if //Focused and // TODO: It seems we are never Focused
     (
       (Key = VK_Down) or
       (Key = VK_Up) or
       (Key = VK_Left) or
       (Key = VK_Right) or
       (Key = VK_Space)
     ) then
  begin
    KeyUp(Key, Shift);
    Key := 0;
  end;
end;

procedure TCastleControl.Resize;
begin
  inherited;
  if Container.GLInitialized then
  begin
    Container.MakeContextCurrent;
    Container.EventResize;
  end;
end;

procedure TCastleControl.UpdateShiftState(const Shift: TShiftState);
begin
  Container.Pressed.Keys[keyShift] := ssShift in Shift;
  Container.Pressed.Keys[keyAlt  ] := ssAlt   in Shift;
  Container.Pressed.Keys[keyCtrl ] := ssCtrl  in Shift;
end;

procedure TCastleControl.KeyDown(var Key: Word; Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  UpdateShiftState(Shift);

  CastleKey := KeyToCastle(Key, Shift);
  if CastleKey <> keyNone then
  begin
    CastleKeyString := SimpleKeyToString(CastleKey, Shift);

    CastleEvent := InputKey(Container.MousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    // check this before updating Container.Pressed
    CastleEvent.KeyRepeated :=
      // Key already pressed
      ((CastleEvent.Key = keyNone) or Container.Pressed.Keys[CastleEvent.Key]) and
      // KeyString already pressed
      ((CastleEvent.KeyString = '') or Container.Pressed.Strings[CastleEvent.KeyString]);

    Container.Pressed.KeyDown(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventPress(CastleEvent) then
      Key := 0; // handled
  end;
end;

procedure TCastleControl.KeyUp(var Key: Word; Shift: TShiftState);
var
  CastleKey: TKey;
  CastleKeyString: String;
  CastleEvent: TInputPressRelease;
begin
  inherited;
  UpdateShiftState(Shift);

  CastleKey := KeyToCastle(Key, Shift);
  if CastleKey <> keyNone then
  begin
    CastleKeyString := SimpleKeyToString(CastleKey, Shift);

    CastleEvent := InputKey(Container.MousePosition, CastleKey, CastleKeyString,
      ModifiersDown(Container.Pressed));

    Container.Pressed.KeyUp(CastleEvent.Key, CastleEvent.KeyString);
    if Container.EventRelease(CastleEvent) then
      Key := 0; // handled
  end;
end;

procedure TCastleControl.KeyPress(var Key: Char);
begin
  // TODO ignored now
  inherited;
end;

end.
