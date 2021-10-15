{
  Copyright 2002-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Interpolation mode, for @link(TAbstractInterpolatorNode.Interpolation). }
  TInterpolation = (
    { The value changes linearly from previous to next. }
    inLinear,
    { The value changes immediately from previous to next,
      at the time of next key.
      Before the time of 1st key, the value is equal to the 1st value.

      E.g. key = [ 0.1, 0.2 ], keyValue = [ 1 2 ] =>
      value is 1 when time is [0, 0.2),
      value is 2 when time is [0.2, ...].

      This is consistent with glTF "step" interpolation,
      see https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#appendix-c-interpolation . }
    inStep
  );


  { Basis for all nodes interpolating (animating) values. }
  TAbstractInterpolatorNode = class(TAbstractChildNode)
  strict protected
    function KeyRange(const Fraction: Single; out T: Single): Integer;
  protected
    function DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
  public
    { TODO: Interpolation is now a regular Pascal class field,
      not X3D field, to conserve memory use.
      This matters greatly in case of e.g. https://github.com/castle-engine/castle-engine/issues/285 }
    Interpolation: TInterpolation;

  {$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
  end;

  { Basis for all nodes interpolating (animating) a single value. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TAbstractSingleInterpolatorNode<
    TInterpolatedValue,
    TKeyValue,
    TKeyValueEvent> = class(TAbstractInterpolatorNode)
  strict private
    function KeyValue(Index: Integer): TInterpolatedValue;
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  strict protected
    function InterpolatorDefault: TInterpolatedValue; virtual;
    function InterpolatorLerp(const A: Single;
      const V1, V2: TInterpolatedValue): TInterpolatedValue; virtual; abstract;
  public
    procedure CreateNode; override;

    strict private FFdKeyValue: TKeyValue;
    public property FdKeyValue: TKeyValue read FFdKeyValue;

    { Event out } { }
    strict private FEventValue_changed: TKeyValueEvent;
    public property EventValue_changed: TKeyValueEvent read FEventValue_changed;
  end;

  { Basis for all nodes interpolating (animating) an array of values. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TAbstractArrayInterpolatorNode<
    TKeyValue,
    TKeyValueEvent> = class(TAbstractInterpolatorNode)
  strict private
    function KeyValue(const Index, KeyCount: Integer): TKeyValue;
    function KeyValueLerp(const T: Single; const Index1, Index2, KeyCount: Integer): TKeyValue;
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  strict protected
    procedure InterpolatorLerp(const Target: TKeyValue;
      const Fraction: Single; const F1, F2: TKeyValue;
      const Index1, Index2, ACount: Integer); virtual;
  public
    procedure CreateNode; override;

    strict private FFdKeyValue: TKeyValue;
    public property FdKeyValue: TKeyValue read FFdKeyValue;

    { Event out } { }
    strict private FEventValue_changed: TKeyValueEvent;
    public property EventValue_changed: TKeyValueEvent read FEventValue_changed;
  end;

  { Interpolate (animate) a single color. }
  TColorInterpolatorNode = class(specialize TAbstractSingleInterpolatorNode<
    TVector3,
    TMFColor,
    TSFColorEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector3): TVector3; override;

  {$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
  end;

  { Interpolate (animate) a set of colors,
    for example to animate a set of @link(TBackgroundNode)
    or @link(TColorNode) colors. }
  TColorSetInterpolatorNode = class(specialize TAbstractArrayInterpolatorNode<
    TMFColor,
    TMFColorEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TMFColor;
      const Fraction: Single; const F1, F2: TMFColor;
      const Index1, Index2, ACount: Integer); override;

  {$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
  end;

  { Interpolate (animate) a set of 3D positions,
    for example to animate @link(TCoordinateNode coordinates) of a mesh. }
  TCoordinateInterpolatorNode = class(specialize TAbstractArrayInterpolatorNode<
    TMFVec3f,
    TMFVec3fEvent>)

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
  end;

  { Interpolate (animate) a set of 2D positions,
    for example to animate 2D texture coordinates. }
  TCoordinateInterpolator2DNode = class(specialize TAbstractArrayInterpolatorNode<
    TMFVec2f,
    TMFVec2fEvent>)

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
  end;

  { Support controlled gradual transitions by modifying
    TimeSensor node fractions.
    @bold(Not implemented yet.) }
  TEaseInEaseOutNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
  end;

  { Interpolate (animate) a set of 3D directions, for example normal vectors. }
  TNormalInterpolatorNode = class(specialize TAbstractArrayInterpolatorNode<
    TMFVec3f,
    TMFVec3fEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TMFVec3f;
      const Fraction: Single; const F1, F2: TMFVec3f;
      const Index1, Index2, ACount: Integer); override;

  {$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
  end;

  { Interpolate (animate) an orientation,
    for example to animate @link(TTransformNode.Rotation). }
  TOrientationInterpolatorNode = class(specialize TAbstractSingleInterpolatorNode<
    TVector4,
    TMFRotation,
    TSFRotationEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector4): TVector4; override;

  {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
  end;

  { Interpolate (animate) a 3D position,
    for example to animate @link(TTransformNode.Translation). }
  TPositionInterpolatorNode = class(specialize TAbstractSingleInterpolatorNode<
    TVector3,
    TMFVec3f,
    TSFVec3fEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector3): TVector3; override;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
  end;

  { Interpolate (animate) a 2D position. }
  TPositionInterpolator2DNode = class(specialize TAbstractSingleInterpolatorNode<
    TVector2,
    TMFVec2f,
    TSFVec2fEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector2): TVector2; override;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
  end;

  { Interpolate (animate) a single floating-point value,
    for example to animate @link(TMaterialNode.Transparency). }
  TScalarInterpolatorNode = class(specialize TAbstractSingleInterpolatorNode<
    Single,
    TMFFloat,
    TSFFloatEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: Single): Single; override;

  {$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 3D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 2D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
  end;

  { Non-linearly interpolate (animate) a single floating-point value.
    @bold(Not implemented yet.) }
  TSplineScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a rotation.
    @bold(Not implemented yet.) }
  TSquadOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
  end;

  { Interpolate (animate) a set of floats, for example to animate
    @link(TElevationGridNode) heights. }
  TVectorInterpolatorNode = class(specialize TAbstractArrayInterpolatorNode<
    TMFFloat,
    TMFFloatEvent>)

  {$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  InterpolationNames: array [TInterpolation] of String = (
    'LINEAR',
    'STEP'
  );

{ TAbstractInterpolatorNode -------------------------------------------------- }

function TAbstractInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
begin
  Result := X3DNodes.KeyRange(FdKey.Items, Fraction, T);
end;

function TAbstractInterpolatorNode.DeepCopyCore(CopyState: TX3DNodeDeepCopyState): TX3DNode;
var
  ResultI: TAbstractInterpolatorNode;
begin
  Result := inherited DeepCopyCore(CopyState);
  ResultI := Result as TAbstractInterpolatorNode;

  { TODO: As Interpolation is now a regular Pascal class field, not X3D field,
    it needs to be copied manually. }
  ResultI.Interpolation := Interpolation;
end;

{ TAbstractSingleInterpolatorNode -------------------------------------------- }

function TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValue(Index: Integer): TInterpolatedValue;
begin
  if Index < FdKeyValue.Items.Count then
    Result := FdKeyValue.Items.List^[Index]
  else
    Result := InterpolatorDefault;
end;

procedure TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  TKeyValue,
  TKeyValueEvent>{$endif} .
  EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  T: Single;
  OutputValue: TInterpolatedValue;
  KeyCount, Range: Integer;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    Exit;
  end;

  Assert(Value is TSFFloat, 'EventSet_FractionReceive (for single-value field) can only process SFFloat input events');
  Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive (for single-value field) cannot handle fraction = NaN');
  Range := KeyRange((Value as TSFFloat).Value, T);
  if Range = 0 then
    OutputValue := KeyValue(0)
  else
  if Range = KeyCount then
    OutputValue := KeyValue(KeyCount - 1)
  else
  begin
    case Interpolation of
      inLinear: OutputValue := InterpolatorLerp(T, KeyValue(Range - 1), KeyValue(Range));
      inStep  : OutputValue := KeyValue(Range - 1);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TAbstractSingleInterpolatorNode:Interpolation?');
      {$endif}
    end;
  end;

  EventValue_Changed.Send(OutputValue, Time, Value.InternalPartialSend);
end;

procedure TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  TKeyValue,
  TKeyValueEvent>{$endif} .
  CreateNode;
begin
  inherited;

  FFdKeyValue := TKeyValue.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventValue_changed := TKeyValueEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

function TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  TKeyValue,
  TKeyValueEvent>{$endif}
  .InterpolatorDefault: TInterpolatedValue;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

// function TAbstractSingleInterpolatorNode.InterpolatorLerp(const A: Single;
//   const V1, V2: TInterpolatedValue): TInterpolatedValue;
// begin
//   Result := Lerp(A, V1, V2);
// end;

{ TAbstractArrayInterpolatorNode --------------------------------------------- }

function TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValue(const Index, KeyCount: Integer): TKeyValue;
var
  FrameValues: Integer;
begin
  FrameValues := FdKeyValue.Items.Count div KeyCount;

  // We pass Self as ParentNode to
  // TKeyValue.CreateUndefined.
  // This is not required, ParentNode could be @nil and everything
  // will work Ok, but setting ParentNode allows InstantReality Logger node
  // to display "sending node" information, useful for debugging.
  Result := TKeyValue.CreateUndefined(Self, false, '');

  if (Index + 1) * FrameValues <= FdKeyValue.Items.Count then
  begin
    Result.Items.AddSubRange(FdKeyValue.Items, Index * FrameValues, FrameValues);
  end else
    WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
      [X3DType]));
end;

function TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValueLerp(const T: Single; const Index1, Index2, KeyCount: Integer): TKeyValue;
var
  FrameValues: Integer;
begin
  // This actually knows that Index2 = Index1 + 1, but assumes only
  // Index2 > Index1

  FrameValues := FdKeyValue.Items.Count div KeyCount;
  Result := TKeyValue.CreateUndefined(Self, false, '');

  if (Index2 + 1) * FrameValues <= FdKeyValue.Items.Count then
  begin
    InterpolatorLerp(Result, T, FdKeyValue, FdKeyValue,
      Index1 * FrameValues, Index2 * FrameValues, FrameValues);
  end else
    WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
      [X3DType]));
end;

procedure TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TKeyValue,
  TKeyValueEvent>{$endif} .
  EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  KeyCount: Integer;
  T: Single;
  OutputValue: TKeyValue;
  Range: Integer;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    // Moreover, "div KeyCount" would cause errors if we would try
    // to get some KeyValue or KeyValueLerp.
    Exit;
  end;

  Assert(Value is TSFFloat, 'EventSet_FractionReceive (for multiple-value field) can only process SFFloat input events');
  Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive (for multiple-value field) cannot handle fraction = NaN');
  Range := KeyRange((Value as TSFFloat).Value, T);
  if Range = 0 then
    OutputValue := KeyValue(0, KeyCount)
  else
  if Range = KeyCount then
    OutputValue := KeyValue(KeyCount - 1, KeyCount)
  else
  begin
    case Interpolation of
      inLinear: OutputValue := KeyValueLerp(T, Range - 1, Range, KeyCount);
      inStep  : OutputValue := KeyValue(Range - 1, KeyCount);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TAbstractSingleInterpolatorNode:Interpolation?');
      {$endif}
    end;
  end;

  OutputValue.InternalPartialSend := Value.InternalPartialSend;
  try
    EventValue_Changed.Send(OutputValue, Time);
  finally
    FreeAndNil(OutputValue);
  end;
end;

procedure TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TKeyValue,
  TKeyValueEvent>{$endif} .
  CreateNode;
begin
  inherited;

  FFdKeyValue := TKeyValue.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventValue_changed := TKeyValueEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

procedure TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TKeyValue,
  TKeyValueEvent>{$endif} .
  InterpolatorLerp(const Target: TKeyValue;
  const Fraction: Single; const F1, F2: TKeyValue;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerp(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

{ TColorInterpolatorNode ----------------------------------------------------- }

function TColorInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector3): TVector3;
begin
  Result := LerpRgbInHsv(A, V1, V2);
end;

{ TColorSetInterpolatorNode -------------------------------------------------- }

procedure TColorSetInterpolatorNode.InterpolatorLerp(const Target: TMFColor;
  const Fraction: Single; const F1, F2: TMFColor;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpRgbInHsv(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

{ TNormalInterpolatorNode -------------------------------------------------- }

procedure TNormalInterpolatorNode.InterpolatorLerp(
  const Target: TMFVec3f;
  const Fraction: Single; const F1, F2: TMFVec3f;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpNormalize(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

{ TOrientationInterpolatorNode -------------------------------------------------- }

function TOrientationInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector4): TVector4;
begin
  { In my tests, NLerp works as good as SLerp here.
    Although NLerp doesn't guarantee constant velocity, and should be
    faster --- neither of these qualities seem to be noticeable
    in practice. }
  Result := SLerp(A, V1, V2);
end;

{ TPositionInterpolatorNode -------------------------------------------------- }

function TPositionInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector3): TVector3;
begin
  Result := Lerp(A, V1, V2);
end;

{ TPositionInterpolator2DNode -------------------------------------------------- }

function TPositionInterpolator2DNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector2): TVector2;
begin
  Result := Lerp(A, V1, V2);
end;

{ TScalarInterpolatorNode -------------------------------------------------- }

function TScalarInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: Single): Single;
begin
  Result := Lerp(A, V1, V2);
end;

procedure RegisterInterpolationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TColorInterpolatorNode,
    TColorSetInterpolatorNode,
    TCoordinateInterpolatorNode,
    TCoordinateInterpolator2DNode,
    TEaseInEaseOutNode,
    TNormalInterpolatorNode,
    TOrientationInterpolatorNode,
    TPositionInterpolatorNode,
    TPositionInterpolator2DNode,
    TScalarInterpolatorNode,
    TSplinePositionInterpolatorNode,
    TSplinePositionInterpolator2DNode,
    TSplineScalarInterpolatorNode,
    TSquadOrientationInterpolatorNode,
    TVectorInterpolatorNode
  ]);
end;

{$endif read_implementation}
