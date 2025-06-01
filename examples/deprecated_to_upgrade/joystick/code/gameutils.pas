{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Dice Throwing".

  "Dice Throwing" is free software; see the file LICENSE,
  included in this distribution, for details about the copyright.

  "Dice Throwing" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utitlities for gamepad example. }
unit GameUtils;

interface

uses Classes,
  CastleUiControls, CastleVectors, CastleControls, CastleJoysticks;

type
  { Visualize joystick axis (2D vector) state. }
  TJoyAxisVisualize = class(TCastleRectangleControl)
  strict private
    FAxis: TVector2;
    Shape: TCastleShape;
    Lab: TCastleLabel;
    procedure SetAxis(const Value: TVector2);
    function GetCaption: String;
    procedure SetCaption(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    property Axis: TVector2 read FAxis write SetAxis;
    property Caption: String read GetCaption write SetCaption;
  end;

const
  AxisNames: array [JOY_AXIS_X..JOY_POVY] of string =
  ( 'JOY_AXIS_X',
    'JOY_AXIS_Y',
    'JOY_AXIS_Z',
    'JOY_AXIS_R',
    'JOY_AXIS_U',
    'JOY_AXIS_V',
    'JOY_POVX',
    'JOY_POVY'
  );

implementation

uses SysUtils,
  CastleColors;

constructor TJoyAxisVisualize.Create(AOwner: TComponent);
begin
  inherited;
  Color := Vector4(0, 0.5, 0, 1); // dark green
  Border.AllSides := 2;
  BorderColor := Yellow;
  Width := 256;
  Height := 256;

  Shape := TCastleShape.Create(Self);
  Shape.ShapeType := stCircle;
  Shape.Color := Yellow;
  Shape.Anchor(hpMiddle);
  Shape.Anchor(vpMiddle);
  Shape.Width := 16;
  Shape.Height := 16;
  InsertFront(Shape);

  Lab := TCastleLabel.Create(Self);
  Lab.Color := Yellow;
  Lab.Anchor(hpLeft);
  Lab.Anchor(vpBottom);
  InsertFront(Lab);
end;

procedure TJoyAxisVisualize.SetAxis(const Value: TVector2);
begin
  FAxis := Value;
  Shape.Translation := Value * Vector2(Width - Shape.Width, Height - Shape.Height) / 2;
end;

function TJoyAxisVisualize.GetCaption: String;
begin
  Result := Lab.Caption;
end;

procedure TJoyAxisVisualize.SetCaption(const Value: String);
begin
  Lab.Caption := Value;
end;

end.