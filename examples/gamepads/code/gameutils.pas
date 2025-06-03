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

function InternalAxisName(const Axis: TInternalGamepadAxis): String;

implementation

uses SysUtils, TypInfo,
  CastleColors;

constructor TJoyAxisVisualize.Create(AOwner: TComponent);
var
  CircleFill: TCastleShape;
begin
  inherited;
  Color := Vector4(0.5, 0.5, 0.5, 1); // gray
  Border.AllSides := 2;
  BorderColor := White;
  Width := 256;
  Height := 256;

  { Show circle inside, just to debug how the axis coordinates fit within
    circle. They generally *do not* fit within the circle (testing
    with  XBox Controller now) although they also don't go "all the way"
    into rect corners. }
  CircleFill := TCastleShape.Create(Self);
  CircleFill.ShapeType := stCircle;
  CircleFill.Width := 256 - Border.AllSides * 2;
  CircleFill.Height := 256 - Border.AllSides * 2;
  CircleFill.Color := Vector4(0.4, 0.4, 0.4, 1); // minimally darker gray than background
  CircleFill.Anchor(hpMiddle);
  CircleFill.Anchor(vpMiddle);
  InsertFront(CircleFill);

  Shape := TCastleShape.Create(Self);
  Shape.ShapeType := stCircle;
  Shape.Color := Yellow;
  Shape.Anchor(hpMiddle);
  Shape.Anchor(vpMiddle);
  Shape.Width := 16;
  Shape.Height := 16;
  InsertFront(Shape);

  Lab := TCastleLabel.Create(Self);
  Lab.Color := White;
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

function InternalAxisName(const Axis: TInternalGamepadAxis): String;
begin
  Result := GetEnumName(TypeInfo(TInternalGamepadAxis), Ord(Axis));
end;

end.