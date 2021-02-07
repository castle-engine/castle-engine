{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  TAbstractCubicBezierInterpolatorNode = class(TAbstractInterpolatorNode)
  strict private
    type
      TCalculatedCurve2D = array [0..10] of TVector2;
      TCalculatedCurve2DList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TStructList<TCalculatedCurve2D>;
    var
      FCalculatedCurves: TCalculatedCurve2DList;
      CubicBezierValueWarningDone: Boolean;

    { Like KeyRange, but looking at X coordinates of the points in Key array. }
    class function KeyRangeArray2D(Key: array of TVector2;
      const Fraction: Single; out T: Single): Integer; static;

    class function CubicBezierPercent(const T: Single;
      const ControlPoints: TVector4;
      const Curve: TCalculatedCurve2D): Single; static;

    { For a Bezier curve segment, expressed as 4D vector with the meaning
      like in Spine "curve" specification (it specifies XY values of
      2 middle cubic Bezier curve points), calculate the curve. }
    class procedure CalculatedCurve2D(const KeyValue: TVector4;
      out Curve: TCalculatedCurve2D); static;

  strict protected
    { Calculate curve value for the range inside
      FdControlPoints[ControlPointsIndex]. }
    function CubicBezierValue(
      const T: Single; const ControlPointsIndex: Integer): Single;

  protected
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;

  public
    procedure CreateNode; override;
    destructor Destroy; override;

    strict private FFdControlPoints: TMFVec4f;
    public property FdControlPoints: TMFVec4f read FFdControlPoints;

    { Once the FdControlPoints value is settled, call this to precalculate
      curve parameters. This makes the following interpolation using this node
      work fast. }
    procedure OptimizeControlPoints;

    {$I auto_generated_node_helpers/x3dnodes_x3dcubicbezierinterpolator.inc}
  end;

  { Interpolate (animate) a set of 2D rotations,
    using cubic Bezier curve instead of linear interpolation.
    See https://castle-engine.io/x3d_implementation_interpolation_extensions.php . }
  TCubicBezier2DOrientationInterpolatorNode = class(TAbstractCubicBezierInterpolatorNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    strict private FFdAxis: TSFVec3f;
    public property FdAxis: TSFVec3f read FFdAxis;

    { Event out } { }
    strict private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_cubicbezier2dorientationinterpolator.inc}
  end;

  { Interpolate (animate) a set of 3D positions,
    using cubic Bezier curve instead of linear interpolation.
    See https://castle-engine.io/x3d_implementation_interpolation_extensions.php . }
  TCubicBezierPositionInterpolatorNode = class(TAbstractCubicBezierInterpolatorNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event out } { }
    strict private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_cubicbezierpositioninterpolator.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractCubicBezierInterpolatorNode --------------------------------------- }

class function TAbstractCubicBezierInterpolatorNode.KeyRangeArray2D(
  Key: array of TVector2;
  const Fraction: Single; out T: Single): Integer; static;
var
  KeyCount, A, B: Integer;
begin
  KeyCount := High(Key) + 1;

  Assert(KeyCount > 0);
  Assert(not IsNan(Fraction));

  if Fraction <= Key[0].Data[0] then
    Result := 0 else
  if Fraction >= Key[KeyCount - 1].Data[0] then
    Result := KeyCount else
  begin
    { Then for sure we're between two Key values.
      Note that we know that KeyCount > 1 (otherwise, Key.First = Key.Last
      so one of <= or >= comparisons above would occur; we check
      IsNan(Fraction) at the beginning to eliminate Fraction=NaN case). }
    Assert(KeyCount > 1);

    { Always A < B.
      We're considering range from Key[A].X to Key[B].X.

      Remember that we cannot actually depend on the fact that
      Key values are non-decreasing. They should be non-decreasing,
      and we have to output correct result only when they are non-decreasing,
      but we also have to terminate (with any result) in any situation.
      Reason: Key values are supplied in X3D file, so they may be broken
      in every possible way. }

    A := 0;
    B := KeyCount - 1;
    while B - A > 1 do
    begin
      Result := (A + B) div 2;
      { A < B => (A + B) < 2B => (A + B) div 2 < B => Result < B.
        Also, Result > A (the only way how Result could be = A
        would be when B = A + 1, but we eliminated this case by "while"
        condition".

        This is good, it means A < Result < B, so Result is good candidate
        for next A or B, it will for sure shorten the distance
        between A and B. }
      Assert(A < Result);
      Assert(Result < B);
      if Fraction <= Key[Result].Data[0] then
        B := Result else
        A := Result;
    end;
    Result := B;

    if Key[B].Data[0] > Key[A].Data[0] then
      T := (Fraction - Key[A].Data[0]) / (Key[B].Data[0] - Key[A].Data[0])
    else
      T := 0;
  end;
end;

class function TAbstractCubicBezierInterpolatorNode.CubicBezierPercent(
  const T: Single;
  const ControlPoints: TVector4;
  const Curve: TCalculatedCurve2D): Single; static;
var
  Range: Integer;
  FractionBetweenCurvePoints: Single;
begin
  { use T as the X value, to search appropriate Y on the curve graph }
  Range := KeyRangeArray2D(Curve, T, FractionBetweenCurvePoints);

  if Range = 0 then
    Result := Curve[0].Data[1]
  else
  if Range = High(Curve) + 1 then
    Result := Curve[High(Curve)].Data[1]
  else
    Result := Lerp(FractionBetweenCurvePoints,
      Curve[Range - 1].Data[1],
      Curve[Range].Data[1]);
end;

class procedure TAbstractCubicBezierInterpolatorNode.CalculatedCurve2D(
  const KeyValue: TVector4;
  out Curve: TCalculatedCurve2D); static;
var
  CurvePoints: TCubicBezier2DPoints;
  I: Integer;
begin
  CurvePoints[0] := Vector2(0, 0);
  CurvePoints[1] := Vector2(KeyValue[0], KeyValue[1]);
  CurvePoints[2] := Vector2(KeyValue[2], KeyValue[3]);
  CurvePoints[3] := Vector2(1, 1);

  { calculate Curve in 2D for curve parameter (T parameter of CubicBezier2D())
    in 0..1 }
  for I := 0 to High(Curve) do
    Curve[I] := CubicBezier2D(I / High(Curve), CurvePoints);
end;

procedure TAbstractCubicBezierInterpolatorNode.OptimizeControlPoints;
var
  I: Integer;
begin
  FreeAndNil(FCalculatedCurves);
  FCalculatedCurves := TCalculatedCurve2DList.Create;
  FCalculatedCurves.Count := FdControlPoints.Items.Count;
  for I := 0 to FdControlPoints.Items.Count - 1 do
    CalculatedCurve2D(FdControlPoints.Items.List^[I], FCalculatedCurves.List^[I]);
end;

procedure TAbstractCubicBezierInterpolatorNode.CreateNode;
begin
  inherited;

  FFdControlPoints := TMFVec4f.Create(Self, true, 'controlPoints', []);
  AddField(FFdControlPoints);
end;

function TAbstractCubicBezierInterpolatorNode.CubicBezierValue(
  const T: Single; const ControlPointsIndex: Integer): Single;
var
  ControlPointPtr: PVector4;
  Curve: TCalculatedCurve2D;
begin
  { Call CubicBezierPercent, using ready FCalculatedCurves
    if possible. }
  ControlPointPtr := FdControlPoints.Items.Ptr(ControlPointsIndex);
  if (FCalculatedCurves <> nil) and
     (FCalculatedCurves.Count = FdControlPoints.Count) then
  begin
    Result := CubicBezierPercent(T, ControlPointPtr^,
      FCalculatedCurves.List^[ControlPointsIndex]);
  end else
  begin
    if not CubicBezierValueWarningDone then
    begin
      WritelnWarning('Using CubicBezierXxxInterpolator without calling OptimizeControlPoints beforehand, this is slow. Better call OptimizeControlPoints once the FdControlPoints is settled.');
      // use CubicBezierValueWarningDone to avoid spamming with warnings
      CubicBezierValueWarningDone := true;
    end;
    CalculatedCurve2D(ControlPointPtr^, Curve);
    Result := CubicBezierPercent(T, ControlPointPtr^, Curve);
  end;
end;

destructor TAbstractCubicBezierInterpolatorNode.Destroy;
begin
  FreeAndNil(FCalculatedCurves);
  inherited;
end;

function TAbstractCubicBezierInterpolatorNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  Res: TAbstractCubicBezierInterpolatorNode;
begin
  Result := inherited;
  Res := Result as TAbstractCubicBezierInterpolatorNode;
  Res.OptimizeControlPoints;
end;

{ TCubicBezier2DOrientationInterpolatorNode ---------------------------------- }

procedure TCubicBezier2DOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FFdAxis := TSFVec3f.Create(Self, true, 'axis', Vector3(0, 0, 1));
  AddField(FFdAxis);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

class function TCubicBezier2DOrientationInterpolatorNode.ClassX3DType: String;
begin
  Result := 'CubicBezier2DOrientationInterpolator';
end;

procedure TCubicBezier2DOrientationInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);

  function KeyValue(Index: Integer): Single;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.List^[Index] else
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
        T := CubicBezierValue(T, Range - 1);
      OutputValueAngle := AngleLerp(T, KeyValue(Range - 1), KeyValue(Range));
    end;
  end;
  EventValue_Changed.Send(Vector4(FdAxis.Value, OutputValueAngle), Time, Value.InternalPartialSend);
end;

{ TCubicBezierPositionInterpolatorNode --------------------------------------- }

procedure TCubicBezierPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

class function TCubicBezierPositionInterpolatorNode.ClassX3DType: String;
begin
  Result := 'CubicBezierPositionInterpolator';
end;

procedure TCubicBezierPositionInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);

  function KeyValue(Index: Integer): TVector3;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.List^[Index] else
      Result := TVector3.Zero;
  end;

var
  T: Single;
  OutputValue: TVector3;
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
        T := CubicBezierValue(T, Range - 1);
      OutputValue := Lerp(T, KeyValue(Range - 1), KeyValue(Range));
    end;
  end;
  EventValue_Changed.Send(OutputValue, Time, Value.InternalPartialSend);
end;

{ registration --------------------------------------------------------------- }

procedure RegisterInterpolationCubicBezierNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCubicBezier2DOrientationInterpolatorNode,
    TCubicBezierPositionInterpolatorNode
  ]);
end;

{$endif read_implementation}
