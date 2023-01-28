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

{ Define custom cursor as TCustomCursor class. }
unit GameCustomCursor;

interface

uses SysUtils, Classes,
  CastleUIControls, CastleKeysMouse, CastleVectors, CastleControls;

type
  TCustomCursorStyle = (csImage, csAnimated);

  { User interface that shows custom cursor, and automatically adjusts its own
    position (anchors) to Container.MousePosition.

    Assumes that the parent UI is a container or some FullSize UI control
    (like some TCastleView).
    The TCustomCursor must fill the entire container, to capture motion
    events from the entire container, and for UpdateCursorPosition
    to work correctly. }
  TCustomCursor = class(TCastleUserInterface)
  strict private
    { To avoid clashes in component names between image_cursor.castle-user-interface
      and animated_cursor.castle-user-interface, it's best if each of them has
      a separate owner component. }
    FImageCursorOwner: TComponent;
    FAnimatedCursorOwner: TComponent;
    FImageCursorUi: TCastleUserInterface;
    FAnimatedCursorUi: TCastleUserInterface;
    FCursorUi: TCastleUserInterface;
    FCursorDebugDot: TCastleRectangleControl;
    FStyle: TCustomCursorStyle;
    procedure SetStyle(const Value: TCustomCursorStyle);
    procedure UpdateStyle;
    procedure UpdateCursorPosition(const V: TVector2);
  public
    constructor Create(AOwner: TComponent); override;
    property Style: TCustomCursorStyle read FStyle write SetStyle default csAnimated;
    function Motion(const Event: TInputMotion): Boolean; override;
    procedure InternalSetContainer(const Value: TCastleContainer); override;
    procedure VisibleChange(const Changes: TCastleUserInterfaceChanges;
      const ChangeInitiatedByChildren: boolean = false); override;
  end;

var
  { Single instance of TCustomCursor. }
  CustomCursor: TCustomCursor;

implementation

uses CastleColors;

constructor TCustomCursor.Create(AOwner: TComponent);
begin
  inherited;
  KeepInFront := true;
  FullSize := true;

  FImageCursorOwner := TComponent.Create(Self);
  FImageCursorUi := UserInterfaceLoad('castle-data:/image_cursor.castle-user-interface', FImageCursorOwner);
  { Shift, to make cursor pivot (where the cursor is visually pointing) perfect.
    Note that this assumes that RootGroup in image_cursor.castle-user-interface has FullSize=false. }
  FImageCursorUi.Anchor(hpLeft, -10);
  FImageCursorUi.Anchor(vpBottom, -100);

  FAnimatedCursorOwner := TComponent.Create(Self);
  FAnimatedCursorUi := UserInterfaceLoad('castle-data:/animated_cursor.castle-user-interface', FAnimatedCursorOwner);
  { Shift, to make cursor pivot (where the cursor is visually pointing) perfect.
    Note that this assumes that RootGroup in animated_cursor.castle-user-interface has FullSize=false. }
  FAnimatedCursorUi.Anchor(vpBottom, -100);

  { FCursorUi will contain, as child, FImageCursorUi or FAnimatedCursorUi.
    This makes it easy to implement UpdateCursorPosition that just moves FCursorUi. }
  FCursorUi := TCastleUserInterface.Create(Self);
  InsertFront(FCursorUi);

  { FCursorDebugDot is a debug UI.
    It shows you where is the (0,0) point of the FCursorUi,
    which corresponds to the Container.MousePosition.
    Looking at it, you should adjust shift of FAnimatedCursorUi and FImageCursorUi,
    such that their "visual pivot" (where they point at) is at the red dot.
    For actual game, you should hide this. }
  FCursorDebugDot := TCastleRectangleControl.Create(Self);
  //FCursorDebugDot.Exists := false; // hide this for the actual game
  FCursorDebugDot.Color := Red;
  FCursorDebugDot.Width := 1;
  FCursorDebugDot.Height := 1;
  //FCursorUi.InsertFront(FCursorDebugDot); this will be done in each UpdateStyle

  // initialize default Style
  FStyle := csAnimated;
  UpdateStyle;
end;

procedure TCustomCursor.SetStyle(const Value: TCustomCursorStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    UpdateStyle;
  end;
end;

procedure TCustomCursor.UpdateStyle;
begin
  FCursorUi.ClearControls;
  FCursorUi.InsertFront(FCursorDebugDot);
  case FStyle of
    csImage: FCursorUi.InsertFront(FImageCursorUi);
    csAnimated: FCursorUi.InsertFront(FAnimatedCursorUi);
  end;
end;

procedure TCustomCursor.VisibleChange(const Changes: TCastleUserInterfaceChanges;
  const ChangeInitiatedByChildren: boolean);
begin
  inherited;
  // show cursor at proper place when you toggle Exists to true
  if (chExists in Changes) and (Container <> nil) then
  begin
    UpdateCursorPosition(Container.MousePosition);
  end;
end;

procedure TCustomCursor.InternalSetContainer(const Value: TCastleContainer);
begin
  inherited;
  if Value <> nil then
  begin
    // show cursor at proper place before even it is moved
    UpdateCursorPosition(Value.MousePosition);
  end;
end;

function TCustomCursor.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  UpdateCursorPosition(Event.Position);
end;

procedure TCustomCursor.UpdateCursorPosition(const V: TVector2);
begin
  FCursorUi.Anchor(hpLeft, V.X / UIScale);
  FCursorUi.Anchor(vpBottom, V.Y / UIScale);
end;

end.
