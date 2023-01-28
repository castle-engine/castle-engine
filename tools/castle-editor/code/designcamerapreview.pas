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

{ Preview camera at design-time. }
unit DesignCameraPreview;

interface

uses Classes,
  CastleViewport, CastleControls, CastleScene, CastleTransform, CastleUIControls,
  CastleClassUtils;

type
  TCameraPreview = class
  strict private
    type
      TMyViewport = class(TCastleViewport)
      protected
        function InternalOverride2DProjectionSizing: TCastleUserInterface; override;
      public
        Preview: TCameraPreview;
        SelectedViewport: TCastleViewport;
        function InternalHeadlightCamera: TCastleCamera; override;
        procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
      end;
    var
      Viewport: TMyViewport;
      LabelCaption: TCastleLabel;
      Rect: TCastleRectangleControl;
      ButtonLarger, ButtonSmaller, ButtonPin, ButtonClose: TCastleButton;
      Pinned: Boolean;
      Size: Integer;
      SelectedCameraObserver: TFreeNotificationObserver;
      SelectedViewportObserver: TFreeNotificationObserver;
    procedure ClickLarger(Sender: TObject);
    procedure ClickSmaller(Sender: TObject);
    procedure ClickPin(Sender: TObject);
    procedure ClickClose(Sender: TObject);
    procedure Show(const T: TCastleTransform; const V: TCastleViewport);
    procedure SizeChange(const Change: Integer);
    procedure SelectedCameraFreeNotification(const Sender: TFreeNotificationObserver);
    procedure SelectedViewportFreeNotification(const Sender: TFreeNotificationObserver);
    { Synchronize properties of selected viewport/camera that can change at any point. }
    procedure SynchronizeSelectedProperties;
  public
    constructor Create(const DesignOwner: TComponent);
    { Add this to design to make camera preview potentially visible. }
    function UiRoot: TCastleUserInterface;
    { Call when selection changed.
      T is current selection as TCastleTransform (or nil if none),
      V is the current selection as viewport or viewport containing current selection (or nil if none). }
    procedure SelectedChanged(const T: TCastleTransform; const V: TCastleViewport);
    function SelectedCamera: TCastleCamera;
  end;

implementation

uses Math,
  CastleColors, CastleUtils;

{ TCameraPreview.TMyViewport ------------------------------------------------- }

function TCameraPreview.TMyViewport.InternalHeadlightCamera: TCastleCamera;
begin
  { Inherited implementation of InternalHeadlightCamera would mean we use
    Items.MainCamera, as InternalDesignManipulation = false in TMyViewport.
    This would be wrong: Items.MainCamera is now the *design-time camera
    of main (non-preview) viewport*.

    Using our Camera would also be wrong: in case we preview non-current
    camera, i.e. SelectedViewport.Camera=Camera1 but we preview Camera2.

    The correct behavior is to follow SelectedViewport.InternalHeadlightCamera. }

  Result := SelectedViewport.InternalHeadlightCamera;
end;

procedure TCameraPreview.TMyViewport.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  Preview.SynchronizeSelectedProperties;
end;

function TCameraPreview.TMyViewport.InternalOverride2DProjectionSizing: TCastleUserInterface;
begin
  Result := SelectedViewport;
end;

{ TCameraPreview ------------------------------------------------------------- }

const
  MinSize = 1;
  MaxSize = 5;
  Sizes: array [MinSize..MaxSize] of Single = (
    0.2,
    0.33,
    0.5,
    0.66,
    0.8
  );

constructor TCameraPreview.Create(const DesignOwner: TComponent);
var
  ButtonsLayout: TCastleHorizontalGroup;
const
  ButtonsFontSize = 16;
  LabelFontSize = 30;
  Margin = 5;
begin
  inherited Create;

  SelectedCameraObserver := TFreeNotificationObserver.Create(DesignOwner);
  SelectedCameraObserver.OnFreeNotification := {$ifdef FPC}@{$endif} SelectedCameraFreeNotification;

  SelectedViewportObserver := TFreeNotificationObserver.Create(DesignOwner);
  SelectedViewportObserver.OnFreeNotification := {$ifdef FPC}@{$endif} SelectedViewportFreeNotification;

  Size := 2;

  Rect := TCastleRectangleControl.Create(DesignOwner);
  Rect.Color := Gray;
  Rect.WidthFraction := Sizes[Size];
  Rect.HeightFraction := Sizes[Size];
  Rect.SetTransient;
  Rect.Anchor(hpRight, -Margin);
  Rect.Anchor(vpBottom, Margin);
  Rect.Exists := false; // initially

  LabelCaption := TCastleLabel.Create(DesignOwner);
  LabelCaption.Color := Yellow;
  LabelCaption.FontSize := LabelFontSize;
  //LabelCaption.Caption := 'Test Caption';
  LabelCaption.Anchor(vpTop, -Margin);
  LabelCaption.Anchor(hpLeft, Margin);
  LabelCaption.SetTransient;
  Rect.InsertFront(LabelCaption);

  ButtonsLayout := TCastleHorizontalGroup.Create(DesignOwner);
  ButtonsLayout.Anchor(vpTop, -Margin);
  ButtonsLayout.Anchor(hpRight, -Margin);
  //ButtonsLayout.Spacing := 4;
  Rect.InsertFront(ButtonsLayout);

  ButtonLarger := TCastleButton.Create(DesignOwner);
  ButtonLarger.Caption := '+';
  ButtonLarger.OnClick := @ClickLarger;
  ButtonLarger.FontSize := ButtonsFontSize;
  ButtonsLayout.InsertFront(ButtonLarger);

  ButtonSmaller := TCastleButton.Create(DesignOwner);
  ButtonSmaller.Caption := '-';
  ButtonSmaller.OnClick := @ClickSmaller;
  ButtonSmaller.FontSize := ButtonsFontSize;
  ButtonsLayout.InsertFront(ButtonSmaller);

  ButtonPin := TCastleButton.Create(DesignOwner);
  ButtonPin.Caption := 'pin';
  ButtonPin.OnClick := @ClickPin;
  ButtonPin.Toggle := true;
  ButtonPin.Pressed := Pinned;
  ButtonPin.FontSize := ButtonsFontSize;
  ButtonsLayout.InsertFront(ButtonPin);

  ButtonClose := TCastleButton.Create(DesignOwner);
  ButtonClose.Caption := 'x';
  ButtonClose.OnClick := @ClickClose;
  ButtonClose.FontSize := ButtonsFontSize;
  ButtonsLayout.InsertFront(ButtonClose);

  Viewport := TMyViewport.InternalCreateNonDesign(DesignOwner);
  Viewport.Preview := Self;
  Viewport.Border.AllSides := Margin;
  Viewport.Border.Top := Margin +
    Max(LabelCaption.EffectiveHeight, ButtonsLayout.EffectiveHeight) + Margin;
  Viewport.FullSize := true;
  Viewport.SetTransient;
  Rect.InsertFront(Viewport);

  ForceFallbackLook(Rect);
end;

procedure TCameraPreview.SizeChange(const Change: Integer);
begin
  Size := Clamped(Size + Change, MinSize, MaxSize);

  Rect.WidthFraction := Sizes[Size];
  Rect.HeightFraction := Sizes[Size];
end;

procedure TCameraPreview.SelectedCameraFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  // This is useful when selected current camera was pinned, and then freed
  Show(nil, nil);
end;

procedure TCameraPreview.SelectedViewportFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  Show(nil, nil);
end;

procedure TCameraPreview.ClickLarger(Sender: TObject);
begin
  SizeChange(1);
end;

procedure TCameraPreview.ClickSmaller(Sender: TObject);
begin
  SizeChange(-1);
end;

procedure TCameraPreview.ClickPin(Sender: TObject);
begin
  Pinned := not Pinned;
  ButtonPin.Pressed := Pinned;
end;

procedure TCameraPreview.ClickClose(Sender: TObject);
begin
  Show(nil, nil)
end;

function TCameraPreview.UiRoot: TCastleUserInterface;
begin
  Result := Rect;
end;

procedure TCameraPreview.SelectedChanged(const T: TCastleTransform; const V: TCastleViewport);
begin
  // when Pinned, merely changing selection doesn't change what is displayed
  if not Pinned then
    Show(T, V);
end;

procedure TCameraPreview.Show(const T: TCastleTransform; const V: TCastleViewport);
begin
  { Show Rect if selected a camera
    that is *not* the current camera in this viewport. }
  Rect.Exists := (T is TCastleCamera) and
    (V <> nil) and (T.World = V.Items) and (V.InternalCamera <> T);

  if Rect.Exists then
  begin
    Viewport.Items := V.Items;
    Viewport.Camera := T as TCastleCamera;

    SelectedCameraObserver.Observed := T;

    Viewport.SelectedViewport := V;
    SelectedViewportObserver.Observed := Viewport.SelectedViewport;

    SynchronizeSelectedProperties;
  end else
  begin
    { Assign "empty" values for Items/Camera,
      to avoid needlessly refering to them when they could be destroyed
      (although we have observers to detect it anyway). }
    Viewport.Items := nil;
    Viewport.Camera := nil;
    Viewport.Background := nil;
    Viewport.Fog := nil;

    // unpin when hiding, this is most natural
    Pinned := false;
    ButtonPin.Pressed := Pinned;

    SelectedCameraObserver.Observed := nil;

    Viewport.SelectedViewport := nil;
    SelectedViewportObserver.Observed := Viewport.SelectedViewport;
  end;
end;

function TCameraPreview.SelectedCamera: TCastleCamera;
begin
  Result := Viewport.Camera;
end;

procedure TCameraPreview.SynchronizeSelectedProperties;
begin
  if Viewport.SelectedViewport <> nil then
  begin
    Assert(SelectedCamera <> nil);
    LabelCaption.Caption := SelectedCamera.Name;

    Viewport.Background := Viewport.SelectedViewport.Background;
    Viewport.BackgroundColor := Viewport.SelectedViewport.BackgroundColor;
    Viewport.Fog := Viewport.SelectedViewport.Fog;
    Viewport.Transparent := Viewport.SelectedViewport.Transparent;
  end;
end;

end.
