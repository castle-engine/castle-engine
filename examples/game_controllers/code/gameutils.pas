{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utitlities for game_controllers example. }
unit GameUtils;

interface

uses Classes,
  CastleUiControls, CastleVectors, CastleControls, CastleGameControllers;

type
  { Visualize controller 2D axis state. }
  T2DAxisVisualize = class(TCastleRectangleControl)
  strict private
    FAxis: TVector2;
    Shape: TCastleShape;
    Lab: TCastleLabel;
    FCaption: String;
    procedure SetAxis(const Value: TVector2);
    procedure SetCaption(const Value: String);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
    property Axis: TVector2 read FAxis write SetAxis;
    property Caption: String read FCaption write SetCaption;
  end;

  { Visualize controller 1D axis state. }
  T1DAxisVisualize = class(TCastleRectangleControl)
  strict private
    FAxis: Single;
    Shape: TCastleShape;
    Lab: TCastleLabel;
    FCaption: String;
    procedure SetAxis(const Value: Single);
    procedure SetCaption(const Value: String);
    procedure UpdateLabel;
  public
    constructor Create(AOwner: TComponent); override;
    property Axis: Single read FAxis write SetAxis;
    property Caption: String read FCaption write SetCaption;
  end;

function InternalAxisName(const Axis: TInternalGameControllerAxis): String;

implementation

uses SysUtils, TypInfo,
  CastleColors, CastleUtils;

{ T2DAxisVisualize ----------------------------------------------------------- }

constructor T2DAxisVisualize.Create(AOwner: TComponent);
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

procedure T2DAxisVisualize.SetAxis(const Value: TVector2);
begin
  FAxis := Value;
  Shape.Translation := Value * Vector2(Width - Shape.Width, Height - Shape.Height) / 2;
  UpdateLabel;
end;

procedure T2DAxisVisualize.SetCaption(const Value: String);
begin
  FCaption := Value;
  UpdateLabel;
end;

procedure T2DAxisVisualize.UpdateLabel;
begin
  Lab.Caption := FormatDot('%s' + NL + '(%f, %f)', [
    FCaption,
    FAxis.X,
    FAxis.Y
  ]);
end;

{ T1DAxisVisualize ----------------------------------------------------------- }

constructor T1DAxisVisualize.Create(AOwner: TComponent);
begin
  inherited;
  Color := Vector4(0.5, 0.5, 0.5, 1); // gray
  Border.AllSides := 2;
  BorderColor := White;
  Width := 256;
  Height := 32;

  Shape := TCastleShape.Create(Self);
  Shape.ShapeType := stCircle;
  Shape.Color := Yellow;
  Shape.Anchor(hpMiddle);
  Shape.Anchor(vpMiddle);
  Shape.Width := 16 - Border.AllSides * 2;
  Shape.Height := 16 - Border.AllSides * 2;
  InsertFront(Shape);

  Lab := TCastleLabel.Create(Self);
  Lab.Color := White;
  Lab.Anchor(hpLeft);
  Lab.Anchor(vpBottom);
  InsertFront(Lab);
end;

procedure T1DAxisVisualize.SetAxis(const Value: Single);
begin
  FAxis := Value;
  // Note: Invert -1 and 1, to be more natural for trigger visualization
  // (because AxisTrigger = 1.0 is "fully left").
  Shape.Translation := Vector2(MapRange(Value, 1, -1, -126, 126), 0);
  UpdateLabel;
end;

procedure T1DAxisVisualize.SetCaption(const Value: String);
begin
  FCaption := Value;
  UpdateLabel;
end;

procedure T1DAxisVisualize.UpdateLabel;
begin
  Lab.Caption := FormatDot('%s (%f)', [
    FCaption,
    FAxis
  ]);
end;

{ routines ------------------------------------------------------------------- }

function InternalAxisName(const Axis: TInternalGameControllerAxis): String;
begin
  Result := GetEnumName(TypeInfo(TInternalGameControllerAxis), Ord(Axis));
end;

end.