{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Interpolate (animate) a set of 2D positions,
    using cubic Bezier curve instead of linear interpolation.
    See http://castle-engine.sourceforge.net/x3d_implementation_interpolation_extensions.php . }
  TCubicBezier2DOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    private FFdControlPoints: TMFVec4f;
    public property FdControlPoints: TMFVec4f read FFdControlPoints;

    private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    { Event out } { }
    private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;
  end;

  { Interpolate (animate) a set of 3D positions,
    using cubic Bezier curve instead of linear interpolation.
    See http://castle-engine.sourceforge.net/x3d_implementation_interpolation_extensions.php . }
  TCubicBezierPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    private FFdControlPoints: TMFVec4f;
    public property FdControlPoints: TMFVec4f read FFdControlPoints;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ Lerp on angle value in radians. }
function AngleLerp(T, Angle1, Angle2: Single): Single;
begin
  Angle1 := FloatModulo(Angle1, 2 * Pi);
  Angle2 := FloatModulo(Angle2, 2 * Pi);

  if Angle1 > Angle2 then
  begin
    T := 1 - T;
    SwapValues(Angle1, Angle2);
  end;
  { rest of the implementation can assume Angle1 <= Angle2 }

  if Angle1 + 2 * Pi - Angle2 >= Angle2 - Angle1 then
    Result := Lerp(T, Angle1, Angle2) else
    Result := FloatModulo(Lerp(T, Angle1 + 2 * Pi, Angle2), 2 * Pi);
end;

function CubicBezierPercent(const T: Single;
  const ControlPoints: TVector4Single): Single;
var
  {CX1, }CY1, {CX2, }CY2: Single;
begin
//  CX1 := ControlPoints[0];
  CY1 := ControlPoints[1];
//  CX2 := ControlPoints[2];
  CY2 := ControlPoints[3];

  { TODO: this is just a poor-mans approximation of how to calculate a point
    on a Bezier curve. We simply use 1 dimension as a curve parameter,
    which is totally incorrect (we should calculate curve in 2D,
    with some parameter, and then use T as X; instead we use T as curve
    parameter directly).
    But this looks acceptable, and is really better than doing nothing (that is,
    doing linear, which means returning T).

    The better solution would be to actually precalculate
    such Bezier curve (Spine C# runtime just precalculates 10 points along
    the way, and makes linear interpolation between them, it seems). }

  Result := 3 * CY1 * Sqr(1 - T) * T +
            3 * CY2 * Sqr(T) * (1 - T) +
                      Sqr(T) * T;
end;

procedure TCubicBezier2DOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);

  FFdControlPoints := TMFVec4f.Create(Self, 'controlPoints', []);
  AddField(FFdControlPoints);

  FFdAxis := TSFVec3f.Create(Self, 'axis', Vector3Single(0, 0, 1));
  AddField(FFdAxis);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCubicBezier2DOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'CubicBezier2DOrientationInterpolator';
end;

class function TCubicBezier2DOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCubicBezier2DOrientationInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);

  function KeyValue(Index: Integer): Single;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.L[Index] else
      Result := 0;
  end;

var
  T: Single;
  OutputValueAngle: Single;
  KeyCount, Range: Integer;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    Exit;
  end else
  begin
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValueAngle := KeyValue(0) else
    if Range = KeyCount then
      OutputValueAngle := KeyValue(KeyCount - 1) else
    begin
      if Range - 1 < FdControlPoints.Items.Count then
        T := CubicBezierPercent(T, FdControlPoints.Items.L[Range - 1]);
      OutputValueAngle := AngleLerp(T, KeyValue(Range - 1), KeyValue(Range));
    end;
  end;
  EventValue_Changed.Send(Vector4Single(FdAxis.Value, OutputValueAngle), Time);
end;

procedure TCubicBezierPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdControlPoints := TMFVec4f.Create(Self, 'controlPoints', []);
  AddField(FFdControlPoints);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCubicBezierPositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'CubicBezierPositionInterpolator';
end;

class function TCubicBezierPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCubicBezierPositionInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);

  function KeyValue(Index: Integer): TVector3Single;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.L[Index] else
      Result := ZeroVector3Single;
  end;

var
  T: Single;
  OutputValue: TVector3Single;
  KeyCount, Range: Integer;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    Exit;
  end else
  begin
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
    begin
      if Range - 1 < FdControlPoints.Items.Count then
        T := CubicBezierPercent(T, FdControlPoints.Items.L[Range - 1]);
      OutputValue := Lerp(T, KeyValue(Range - 1), KeyValue(Range));
    end;
  end;
  EventValue_Changed.Send(OutputValue, Time);
end;

procedure RegisterInterpolationCubicBezierNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCubicBezier2DOrientationInterpolatorNode,
    TCubicBezierPositionInterpolatorNode
  ]);
end;

{$endif read_implementation}
