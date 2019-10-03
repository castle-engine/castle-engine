{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Frame to choose UI anchors (left/middle/right, top/middle/bottom). }
unit FrameAnchors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, Buttons,
  CastleRectangles;

type
  TAnchorsFrame = class(TFrame)
    ButtonLeftTop: TSpeedButton;
    ButtonLeftMiddle: TSpeedButton;
    ButtonLeftBottom: TSpeedButton;
    ButtonMiddleMiddle: TSpeedButton;
    ButtonMiddleBottom: TSpeedButton;
    ButtonMiddleTop: TSpeedButton;
    ButtonRightTop: TSpeedButton;
    ButtonRightMiddle: TSpeedButton;
    ButtonRightBottom: TSpeedButton;
    GroupBox1: TGroupBox;
    procedure AnyButtonClick(Sender: TObject);
  private
    FHorizontalAnchor: THorizontalPosition;
    FVerticalAnchor: TVerticalPosition;
    Buttons: array [THorizontalPosition, TVerticalPosition] of TSpeedButton;
    procedure SetHorizontalAnchor(const AValue: THorizontalPosition);
    procedure SetVerticalAnchor(const AValue: TVerticalPosition);
    { Make buttons state reflect current FHorizontalAnchor, FVerticalAnchor. }
    procedure UpdateButtons;
  public
    { Called when change happens because user pressed a button.
      Not called when XxxAnchor changed by code. }
    OnAnchorChange: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    property HorizontalAnchor: THorizontalPosition read FHorizontalAnchor write SetHorizontalAnchor;
    property VerticalAnchor: TVerticalPosition read FVerticalAnchor write SetVerticalAnchor;
  end;

implementation

uses Dialogs,
  CastleUtils;

{$R *.lfm}

constructor TAnchorsFrame.Create(AOwner: TComponent);
begin
  inherited;
  Buttons[hpLeft  , vpBottom] := ButtonLeftBottom;
  Buttons[hpMiddle, vpBottom] := ButtonMiddleBottom;
  Buttons[hpRight , vpBottom] := ButtonRightBottom;
  Buttons[hpLeft  , vpMiddle] := ButtonLeftMiddle;
  Buttons[hpMiddle, vpMiddle] := ButtonMiddleMiddle;
  Buttons[hpRight , vpMiddle] := ButtonRightMiddle;
  Buttons[hpLeft  , vpTop   ] := ButtonLeftTop;
  Buttons[hpMiddle, vpTop   ] := ButtonMiddleTop;
  Buttons[hpRight , vpTop   ] := ButtonRightTop;

  // make buttons reflect default value of FHorizontalAnchor, FVerticalAnchor
  UpdateButtons;
end;

procedure TAnchorsFrame.UpdateButtons;
//var
//  H: THorizontalPosition;
//  V: TVerticalPosition;
begin
  Buttons[FHorizontalAnchor, FVerticalAnchor].Down := true;

  { The rest of the buttons will automatically have Down=false.
  for H in THorizontalPosition do
    for V in TVerticalPosition do
      Buttons[H, V].Down := (H = FHorizontalAnchor) and (V = FVerticalAnchor);
  }
end;

procedure TAnchorsFrame.AnyButtonClick(Sender: TObject);

  procedure FindButton(const Sender: TSpeedButton;
    out H: THorizontalPosition; out V: TVerticalPosition);
  begin
    for H in THorizontalPosition do
      for V in TVerticalPosition do
        if Buttons[H, V] = Sender then
          Exit;
    raise EInternalError.Create('Cannot find sender button');
  end;

var
  H: THorizontalPosition;
  V: TVerticalPosition;
begin
  FindButton(Sender as TSpeedButton, H, V);

  Buttons[H, V].Down := true;
  // The rest of the buttons will automatically have Down=false.
  FHorizontalAnchor := H;
  FVerticalAnchor := V;

  if Assigned(OnAnchorChange) then
    OnAnchorChange(Self);
end;

procedure TAnchorsFrame.SetHorizontalAnchor(const AValue: THorizontalPosition);
begin
  if FHorizontalAnchor = AValue then Exit;
  FHorizontalAnchor := AValue;
  UpdateButtons;
end;

procedure TAnchorsFrame.SetVerticalAnchor(const AValue: TVerticalPosition);
begin
  if FVerticalAnchor = AValue then Exit;
  FVerticalAnchor := AValue;
  UpdateButtons;
end;

end.
