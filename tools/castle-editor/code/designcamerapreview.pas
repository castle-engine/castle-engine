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

{ Preview camera at design-time. }
unit DesignCameraPreview;

interface

uses Classes,
  CastleViewport, CastleControls, CastleScene, CastleTransform, CastleUIControls;

type
  TCameraPreview = class
  strict private
    Viewport: TCastleViewport;
    EmptyItems: TCastleRootTransform;
    LabelCaption: TCastleLabel;
    Rect: TCastleRectangleControl;
  public
    constructor Create(const DesignOwner: TComponent);
    { Add this to design to make camera preview potentially visible. }
    function UiRoot: TCastleUserInterface;
    { Call when selection changed.
      T is current selection as TCastleTransform (or nil if none),
      V is the current selection as viewport or viewport containing current selection (or nil if none). }
    procedure SelectedChanged(const T: TCastleTransform; const V: TCastleViewport);
  end;

implementation

uses CastleColors;

constructor TCameraPreview.Create(const DesignOwner: TComponent);
const
  LabelFontSize = 20;
  Margin = 5;
begin
  inherited Create;

  Rect := TCastleRectangleControl.Create(DesignOwner);
  Rect.Border.AllSides := Margin;
  Rect.Border.Top := Margin + LabelFontSize + Margin;
  Rect.BorderColor := Gray;
  //Rect.Color := Gray;
  Rect.WidthFraction := 0.33;
  Rect.HeightFraction := 0.33;
  Rect.SetTransient;
  Rect.Anchor(hpRight, -Margin);
  Rect.Anchor(vpBottom, Margin);
  Rect.Exists := false; // initially

  LabelCaption := TCastleLabel.Create(DesignOwner);
  LabelCaption.Color := Yellow;
  LabelCaption.FontSize := LabelFontSize;
  //LabelCaption.Caption := 'Test Caption';
  LabelCaption.Anchor(vpBottom, vpTop, Margin);
  LabelCaption.Anchor(hpMiddle);
  LabelCaption.SetTransient;
  Rect.InsertFront(LabelCaption);

  Viewport := TCastleViewport.Create(DesignOwner);
  Viewport.FullSize := true;
  Viewport.SetTransient;
  Rect.InsertFront(Viewport);

  EmptyItems := Viewport.Items;
end;

function TCameraPreview.UiRoot: TCastleUserInterface;
begin
  Result := Rect;
end;

procedure TCameraPreview.SelectedChanged(const T: TCastleTransform; const V: TCastleViewport);
begin
  { Show Rect if selected a camera
    that is *not* the current camera in this viewport. }
  Rect.Exists := (T is TCastleCamera) and
    (V <> nil) and (T.World = V.Items) and (V.Camera <> T);

  if Rect.Exists then
  begin
    Viewport.Items := V.Items;
    Viewport.Camera := T as TCastleCamera;
    LabelCaption.Caption := T.Name;
  end else
  begin
    { Assign "empty" values for Items/Camera.
      This is esp. important for Viewport.Items,
      because TCastleViewport.SetItems doesn't set up any observer right now
      to be notified when given Items are freed by something. }
    Viewport.Items := EmptyItems;
    Viewport.Camera := nil;
  end;
end;

end.
