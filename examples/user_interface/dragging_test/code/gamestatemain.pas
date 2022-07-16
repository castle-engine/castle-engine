{
  Copyright 2019-2022 Michalis Kamburelis.

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
unit GameStateMain;

interface

uses CastleUIState, CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleUIControls;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TStateMain = class(TUIState)
  private
    Buttons: TCastleVerticalGroup;
    ButtonDragSimple: TCastleButton;
    ButtonDragMouseLook: TCastleButton;
    Dragging, MouseLook: Boolean;
    DraggedRect: TCastleRectangleControl;
    Status: TCastleLabel;
    procedure ClickDragSimple(Sender: TObject);
    procedure ClickDragMouseLook(Sender: TObject);
    procedure RefreshButtonsPressed;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, CastleVectors;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
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
  ButtonDragSimple.Caption := 'Drag by tracking Motion events';
  ButtonDragSimple.OnClick := {$ifdef FPC}@{$endif}ClickDragSimple;
  ButtonDragSimple.Toggle := true;
  Buttons.InsertFront(ButtonDragSimple);

  ButtonDragMouseLook := TCastleButton.Create(FreeAtStop);
  ButtonDragMouseLook.Caption := 'Drag by MouseLook logic (hide mouse, pretend drag area is unbounded)';
  ButtonDragMouseLook.OnClick := {$ifdef FPC}@{$endif}ClickDragMouseLook;
  ButtonDragMouseLook.Toggle := true;
  Buttons.InsertFront(ButtonDragMouseLook);

  MouseLook := false;
  RefreshButtonsPressed;
end;

procedure TStateMain.RefreshButtonsPressed;
begin
  ButtonDragSimple.Pressed := not MouseLook;
  ButtonDragMouseLook.Pressed := MouseLook;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if Dragging and MouseLook then
    Container.MouseLookUpdate;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    Dragging := true;
    if MouseLook then
    begin
      Cursor := mcForceNone;
      Container.MouseLookPress;
    end;
    Exit(ExclusiveEvents);
  end;
end;

function TStateMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(buttonLeft) then
  begin
    Dragging := false;
    if MouseLook then
      Cursor := mcDefault;
    Exit(ExclusiveEvents);
  end;
end;

function TStateMain.Motion(const Event: TInputMotion): Boolean;
var
  Delta: TVector2;
begin
  Result := inherited;
  if Result then Exit;

  if Dragging then
  begin
    if MouseLook then
      Delta := Container.MouseLookDelta(Event, RenderRect)
    else
      Delta := Event.Position - Event.OldPosition;
    DraggedRect.AnchorDelta := DraggedRect.AnchorDelta + Delta / UIScale;
    Status.Caption := Format('Dragged rect position: %s', [DraggedRect.AnchorDelta.ToString]);
    Exit(ExclusiveEvents);
  end;
end;

procedure TStateMain.ClickDragSimple(Sender: TObject);
begin
  MouseLook := false;
  RefreshButtonsPressed;
end;

procedure TStateMain.ClickDragMouseLook(Sender: TObject);
begin
  MouseLook := true;
  RefreshButtonsPressed;
end;

end.
