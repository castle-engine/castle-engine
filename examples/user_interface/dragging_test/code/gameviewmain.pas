{
  Copyright 2019-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main user interface class.
  This implements the majority of this application functionality. }
unit GameViewMain;

interface

uses CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleUIControls;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TViewMain = class(TCastleView)
  private
    Buttons: TCastleVerticalGroup;
    ButtonDragSimple: TCastleButton;
    ButtonDragPointerLock: TCastleButton;
    DraggingSimple: Boolean;
    { State of buttons ButtonDragSimple and ButtonDragPointerLock:
      does user want to drag in a simple way (without pointer lock)
      or with pointer lock? }
    WantsDraggingPointerLock: Boolean;
    DraggedRect: TCastleRectangleControl;
    Status: TCastleLabel;
    procedure ClickDragSimple(Sender: TObject);
    procedure ClickDragPointerLock(Sender: TObject);
    procedure RefreshButtonsPressed;
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleVectors;

{ TViewMain ----------------------------------------------------------------- }

procedure TViewMain.Start;
begin
  inherited;

  DraggedRect := TCastleRectangleControl.Create(FreeAtStop);
  DraggedRect.Color := Yellow;
  DraggedRect.Anchor(vpMiddle);
  DraggedRect.Anchor(hpMiddle);
  InsertFront(DraggedRect);

  Status := TCastleLabel.Create(FreeAtStop);
  Status.Color := Yellow;
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpLeft, 10);
  InsertFront(Status);

  Buttons := TCastleVerticalGroup.Create(FreeAtStop);
  Buttons.Anchor(vpBottom);
  Buttons.Anchor(hpLeft);
  Buttons.Padding := 10;
  Buttons.Spacing := 10;
  InsertFront(Buttons);

  ButtonDragSimple := TCastleButton.Create(FreeAtStop);
  ButtonDragSimple.Caption := 'Drag by simple Motion events';
  ButtonDragSimple.OnClick := {$ifdef FPC}@{$endif}ClickDragSimple;
  ButtonDragSimple.Toggle := true;
  Buttons.InsertFront(ButtonDragSimple);

  ButtonDragPointerLock := TCastleButton.Create(FreeAtStop);
  ButtonDragPointerLock.Caption := 'Drag by Pointer Lock logic (hide mouse, pretend drag area is unbounded)';
  ButtonDragPointerLock.OnClick := {$ifdef FPC}@{$endif}ClickDragPointerLock;
  ButtonDragPointerLock.Toggle := true;
  Buttons.InsertFront(ButtonDragPointerLock);

  WantsDraggingPointerLock := false;
  RefreshButtonsPressed;
end;

procedure TViewMain.RefreshButtonsPressed;
begin
  ButtonDragSimple.Pressed := not WantsDraggingPointerLock;
  ButtonDragPointerLock.Pressed := WantsDraggingPointerLock;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    if WantsDraggingPointerLock then
    begin
      Cursor := mcForceNone; // TODO: why is this necessary here
      Container.PointerLock.Active := true
    end else
      DraggingSimple := true;
    Exit(true);
  end;
end;

function TViewMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    DraggingSimple := false;
    Container.PointerLock.Active := false;
    Cursor := mcDefault; // TODO: why is this necessary here
    Exit(true);
  end;
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;

  procedure Drag(const Delta: TVector2);
  begin
    DraggedRect.Translation := DraggedRect.Translation + Delta / UIScale;
    Status.Caption := Format('Dragged rect position: %s', [DraggedRect.Translation.ToString]);
  end;

begin
  Result := inherited;
  if Result then Exit;

  if DraggingSimple then
  begin
    Drag(Event.Position - Event.OldPosition);
    Exit(true);
  end else
  if Container.PointerLock.Active then
  begin
    Drag(Container.PointerLock.Delta(Event));
    Exit(true);
  end;
end;

procedure TViewMain.ClickDragSimple(Sender: TObject);
begin
  WantsDraggingPointerLock := false;
  RefreshButtonsPressed;
end;

procedure TViewMain.ClickDragPointerLock(Sender: TObject);
begin
  WantsDraggingPointerLock := true;
  RefreshButtonsPressed;
end;

end.
