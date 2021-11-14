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
    TSingleField{$ifndef FPC}: TX3DSingleField{$endif};
    TKeyValue{$ifndef FPC}: TX3DField{$endif};
    TKeyValueEvent{$ifndef FPC}:TX3DEvent{$endif} > = class(TAbstractInterpolatorNode)
  strict private
    FKeyValue: TKeyValue;
    FEventValue_changed: TKeyValueEvent;
    FTempFieldToSend: TSingleField;
    FTempFieldToSendUsed: Boolean;
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  strict protected
    { Set Target.Value to Lerp(A, KeyValue[Index1], KeyValue[Index2]).
      Index1 and Index2 are for sure valid (within KeyValue counts). }
    procedure InterpolatorLerp(const Target: TSingleField;
      const A: Single; const Index1, Index2: Integer); virtual; abstract;
    { Set Target.Value to KeyValue[Index], if Index is valid. }
    procedure InterpolatorSet(const Target: TSingleField; const Index: Integer); virtual; abstract;
    { Create TSingleField instance, to send it by EventValue_Changed. }
    function CreateOutputField: TSingleField; virtual; abstract;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
  end;

  { Basis for all nodes interpolating (animating) an array of values. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    TInterpolatedValue;
    TSingleField: TX3DSingleField;
    TInternalItemList: class;
    TItemList: ICastleItemList<TInterpolatedValue, TInternalItemList>, class, constructor;
    TEvent: TX3DEvent, constructor;
    {$endif}
    TKeyValue{$ifndef FPC}: TX3DSimpleMultField<TInterpolatedValue, TSingleField, TInternalItemList, TItemList, TEvent>{$endif};
    TKeyValueEvent{$ifndef FPC}:TX3DEvent{$endif}> = class(TAbstractInterpolatorNode)
  strict private
    FKeyValue: TKeyValue;
    FEventValue_changed: TKeyValueEvent;
    function KeyValue(const Index, KeyCount: Integer): TKeyValue;
    function KeyValueLerp(const T: Single; const Index1, Index2, KeyCount: Integer): TKeyValue;
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  strict protected
    { Set Target array contents to Lerp(Fraction, F1, F2).

      We use from F1 a subrange starting from Index1, counting ACount.

      We use from F2 a subrange starting from Index2, counting ACount.

      We place in Target a ACount values. }
    procedure InterpolatorLerp(const Target: TKeyValue;
      const Fraction: Single; const F1, F2: TKeyValue;
      const Index1, Index2, ACount: Integer); virtual;
    { Create TKeyValue instance, to send it by EventValue_Changed. }
    function CreateOutputField: TKeyValue; virtual; abstract;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  end;

  { Interpolate (animate) a single color. }
  TColorInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TAbstractSingleInterpolatorNode<
    TSFVec3f,
    TMFColor,
    TSFColorEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TSFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec3f; const Index: Integer); override;
    function CreateOutputField: TSFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
  end;

  { Interpolate (animate) a set of colors,
    for example to animate a set of @link(TBackgroundNode)
    or @link(TColorNode) colors. }
  TColorSetInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    TVector3,
    TSFVec3f,
    TStructList<TVector3>,
    TVector3List,
    TMFVec3fEvent,
    {$endif}
    TMFColor,
    TMFColorEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TMFColor;
      const Fraction: Single; const F1, F2: TMFColor;
      const Index1, Index2, ACount: Integer); override;
    function CreateOutputField: TMFColor; override;

  {$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
  end;

  { Interpolate (animate) a set of 3D positions,
    for example to animate @link(TCoordinateNode coordinates) of a mesh. }
  TCoordinateInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    TVector3,
    TSFVec3f,
    TStructList<TVector3>,
    TVector3List,
    TMFVec3fEvent,
    {$endif}
    TMFVec3f,
    TMFVec3fEvent>)

    function CreateOutputField: TMFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
  end;

  { Interpolate (animate) a set of 2D positions,
    for example to animate 2D texture coordinates. }
  TCoordinateInterpolator2DNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    TVector2,
    TSFVec2f,
    TStructList<TVector2>,
    TVector2List,
    TMFVec2fEvent,
    {$endif}
    TMFVec2f,
    TMFVec2fEvent>)

    function CreateOutputField: TMFVec2f; override;

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
  end;

  { Support controlled gradual transitions by modifying
    TimeSensor node fractions.
    @bold(Not implemented yet.) }
  TEaseInEaseOutNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
  end;

  { Interpolate (animate) a set of 3D directions, for example normal vectors. }
  TNormalInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    TVector3,
    TSFVec3f,
    TStructList<TVector3>,
    TVector3List,
    TMFVec3fEvent,
    {$endif}
    TMFVec3f,
    TMFVec3fEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TMFVec3f;
      const Fraction: Single; const F1, F2: TMFVec3f;
      const Index1, Index2, ACount: Integer); override;
    function CreateOutputField: TMFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
  end;

  { Interpolate (animate) an orientation,
    for example to animate @link(TTransformNode.Rotation). }
  TOrientationInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TSFRotation,
    TMFRotation,
    TSFRotationEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TSFRotation; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFRotation; const Index: Integer); override;
    function CreateOutputField: TSFRotation; override;

  {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
  end;

  { Interpolate (animate) a 3D position,
    for example to animate @link(TTransformNode.Translation). }
  TPositionInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TSFVec3f,
    TMFVec3f,
    TSFVec3fEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TSFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec3f; const Index: Integer); override;
    function CreateOutputField: TSFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
  end;

  { Interpolate (animate) a 2D position. }
  TPositionInterpolator2DNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TSFVec2f,
    TMFVec2f,
    TSFVec2fEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TSFVec2f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec2f; const Index: Integer); override;
    function CreateOutputField: TSFVec2f; override;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
  end;

  { Interpolate (animate) a single floating-point value,
    for example to animate @link(TMaterialNode.Transparency). }
  TScalarInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TSFFloat,
    TMFFloat,
    TSFFloatEvent>)
  strict protected
    procedure InterpolatorLerp(const Target: TSFFloat; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFFloat; const Index: Integer); override;
    function CreateOutputField: TSFFloat; override;

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
  TVectorInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractArrayInterpolatorNode<
    {$ifndef FPC}
    Single,
    TSFFloat,
    TStructList<Single>,
    TSingleList,
    TMFFloatEvent,
    {$endif}
    TMFFloat,
    TMFFloatEvent>)

    function CreateOutputField: TMFFloat; override;

  {$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ // unused for now
const
  InterpolationNames: array [TInterpolation] of String = (
    'LINEAR',
    'STEP'
  );
}

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

constructor TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TSingleField,
  TKeyValue,
  TKeyValueEvent>{$endif} .Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FKeyValue := Field('keyValue', true) as TKeyValue;
  FEventValue_changed := AnyEvent('value_changed', true) as TKeyValueEvent;
  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

destructor TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TSingleField,
  TKeyValue,
  TKeyValueEvent>{$endif} .Destroy;
begin
  FreeAndNil(FTempFieldToSend);
  inherited;
end;

procedure TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TSingleField,
  TKeyValue,
  TKeyValueEvent>{$endif} .
  EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  T: Single;
  KeyCount, Range: Integer;
  OutputField: TSingleField;
begin
  if not FEventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    // Interpolator nodes containing no keys in the key field
    // shall not produce any events.
    Exit;
  end;

  if not FTempFieldToSendUsed then
  begin
    FTempFieldToSendUsed := true;
    // create FTempFieldToSend when it is needed for the 1st time
    if FTempFieldToSend = nil then
      FTempFieldToSend := CreateOutputField;
    OutputField := FTempFieldToSend;
  end else
    OutputField := CreateOutputField;

  Assert(Value is TSFFloat, 'EventSet_FractionReceive (for single-value field) can only process SFFloat input events');
  Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive (for single-value field) cannot handle fraction = NaN');
  Range := KeyRange((Value as TSFFloat).Value, T);
  if Range = 0 then
    InterpolatorSet(OutputField, 0)
  else
  if Range = KeyCount then
    InterpolatorSet(OutputField, KeyCount - 1)
  else
  begin
    case Interpolation of
      inLinear:
        begin
          {$ifdef FPC} // Delphi doesn't understand we have Count
          Assert(Between(Range - 1, 0, FKeyValue.Count - 1));
          Assert(Between(Range, 0, FKeyValue.Count - 1));
          {$endif}
          InterpolatorLerp(OutputField, T, Range - 1, Range);
        end;
      inStep  : InterpolatorSet(OutputField, Range - 1);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TAbstractSingleInterpolatorNode:Interpolation?');
      {$endif}
    end;
  end;

  OutputField.InternalPartialSend := Value.InternalPartialSend;
  FEventValue_Changed.Send(OutputField, Time);
  if OutputField = FTempFieldToSend then
    FTempFieldToSendUsed := false
  else
    FreeAndNil(OutputField);
end;

{ TAbstractArrayInterpolatorNode --------------------------------------------- }

constructor TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  {$ifndef FPC}
  TInterpolatedValue,
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FKeyValue := Field('keyValue', true) as TKeyValue;
  FEventValue_changed := AnyEvent('value_changed', true) as TKeyValueEvent;
  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

function TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  {$ifndef FPC}
  TInterpolatedValue,
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValue(const Index, KeyCount: Integer): TKeyValue;
var
  FrameValues: Integer;
begin
  FrameValues := FKeyValue.Items.Count div KeyCount;
  Result := CreateOutputField;

  if (Index + 1) * FrameValues <= FKeyValue.Items.Count then
  begin
  {$ifdef FPC}
    Result.Items.AddSubRange(FKeyValue.Items, Index * FrameValues, FrameValues);
  {$else}
    Result.Items.AddSubRange(FKeyValue.GetInternalItems, Index * FrameValues, FrameValues);
  {$endif}
  end else
    WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
      [X3DType]));
end;

function TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  {$ifndef FPC}
  TInterpolatedValue,
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValueLerp(const T: Single; const Index1, Index2, KeyCount: Integer): TKeyValue;
var
  FrameValues: Integer;
begin
  // This actually knows that Index2 = Index1 + 1, but assumes only
  // Index2 > Index1

  FrameValues := FKeyValue.Items.Count div KeyCount;
  Result := CreateOutputField;

  if (Index2 + 1) * FrameValues <= FKeyValue.Items.Count then
  begin
    InterpolatorLerp(Result, T, FKeyValue, FKeyValue,
      Index1 * FrameValues, Index2 * FrameValues, FrameValues);
  end else
    WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
      [X3DType]));
end;

procedure TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  {$ifndef FPC}
  TInterpolatedValue,
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  KeyCount: Integer;
  T: Single;
  OutputValue: TKeyValue;
  Range: Integer;
begin
  if not FEventValue_Changed.SendNeeded then Exit;

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
    FEventValue_Changed.Send(OutputValue, Time);
  finally
    FreeAndNil(OutputValue);
  end;
end;

procedure TAbstractArrayInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  {$ifndef FPC}
  TInterpolatedValue,
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  InterpolatorLerp(const Target: TKeyValue;
  const Fraction: Single; const F1, F2: TKeyValue;
  const Index1, Index2, ACount: Integer);
begin
  {$ifdef FPC}
  Target.Items.AssignLerpRange(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
  {$else}
  Target.Items.AssignLerpRange(Fraction, F1.GetInternalItems, F2.GetInternalItems, Index1, Index2, ACount);
  {$endif}
end;

{ TColorInterpolatorNode ----------------------------------------------------- }

procedure TColorInterpolatorNode.InterpolatorLerp(const Target: TSFVec3f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := LerpRgbInHsv(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TColorInterpolatorNode.InterpolatorSet(const Target: TSFVec3f; const Index: Integer);
begin
  if Index < FdKeyValue.Items.Count then
    Target.Value := FdKeyValue.Items.List^[Index]
  else
    Target.Value := TCastleColorRGB.Zero;
end;

function TColorInterpolatorNode.CreateOutputField: TSFVec3f;
begin
  // We pass Self as ParentNode to
  // TKeyValue.CreateUndefined.
  // This is not required, ParentNode could be @nil and everything
  // will work Ok, but setting ParentNode allows InstantReality Logger node
  // to display "sending node" information, useful for debugging.

  Result := TSFColor.CreateUndefined(Self, false, '');
end;

{ TColorSetInterpolatorNode -------------------------------------------------- }

procedure TColorSetInterpolatorNode.InterpolatorLerp(const Target: TMFColor;
  const Fraction: Single; const F1, F2: TMFColor;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpRgbInHsv(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

function TColorSetInterpolatorNode.CreateOutputField: TMFColor;
begin
  Result := TMFColor.CreateUndefined(Self, false, '');
end;

{ TCoordinateInterpolatorNode ---------------------------------------------------- }

function TCoordinateInterpolatorNode.CreateOutputField: TMFVec3f;
begin
  Result := TMFVec3f.CreateUndefined(Self, false, '');
end;

{ TCoordinateInterpolator2DNode ---------------------------------------------------- }

function TCoordinateInterpolator2DNode.CreateOutputField: TMFVec2f;
begin
  Result := TMFVec2f.CreateUndefined(Self, false, '');
end;

{ TNormalInterpolatorNode -------------------------------------------------- }

procedure TNormalInterpolatorNode.InterpolatorLerp(
  const Target: TMFVec3f;
  const Fraction: Single; const F1, F2: TMFVec3f;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpNormalize(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

function TNormalInterpolatorNode.CreateOutputField: TMFVec3f;
begin
  Result := TMFVec3f.CreateUndefined(Self, false, '');
end;

{ TOrientationInterpolatorNode -------------------------------------------------- }

procedure TOrientationInterpolatorNode.InterpolatorLerp(const Target: TSFRotation;
  const A: Single; const Index1, Index2: Integer);
begin
  { In my tests, NLerp works as good as SLerp here.
    Although NLerp doesn't guarantee constant velocity, and should be
    faster --- neither of these qualities seem to be noticeable
    in practice. }
  Target.Value := SLerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TOrientationInterpolatorNode.InterpolatorSet(const Target: TSFRotation; const Index: Integer);
begin
  if Index < FdKeyValue.Items.Count then
    Target.Value := FdKeyValue.Items.List^[Index]
  else
    Target.Value := TVector4.Zero;
end;

function TOrientationInterpolatorNode.CreateOutputField: TSFRotation;
begin
  Result := TSFRotation.CreateUndefined(Self, false, '');
end;

{ TPositionInterpolatorNode -------------------------------------------------- }

procedure TPositionInterpolatorNode.InterpolatorLerp(const Target: TSFVec3f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TPositionInterpolatorNode.InterpolatorSet(const Target: TSFVec3f; const Index: Integer);
begin
  if Index < FdKeyValue.Items.Count then
    Target.Value := FdKeyValue.Items.List^[Index]
  else
    Target.Value := TVector3.Zero;
end;

function TPositionInterpolatorNode.CreateOutputField: TSFVec3f;
begin
  Result := TSFVec3f.CreateUndefined(Self, false, '');
end;

{ TPositionInterpolator2DNode -------------------------------------------------- }

procedure TPositionInterpolator2DNode.InterpolatorLerp(const Target: TSFVec2f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TPositionInterpolator2DNode.InterpolatorSet(const Target: TSFVec2f; const Index: Integer);
begin
  if Index < FdKeyValue.Items.Count then
    Target.Value := FdKeyValue.Items.List^[Index]
  else
    Target.Value := TVector2.Zero;
end;

function TPositionInterpolator2DNode.CreateOutputField: TSFVec2f;
begin
  Result := TSFVec2f.CreateUndefined(Self, false, '');
end;

{ TScalarInterpolatorNode -------------------------------------------------- }

procedure TScalarInterpolatorNode.InterpolatorLerp(const Target: TSFFloat;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TScalarInterpolatorNode.InterpolatorSet(const Target: TSFFloat; const Index: Integer);
begin
  if Index < FdKeyValue.Items.Count then
    Target.Value := FdKeyValue.Items.List^[Index]
  else
    Target.Value := 0;
end;

function TScalarInterpolatorNode.CreateOutputField: TSFFloat;
begin
  Result := TSFFloat.CreateUndefined(Self, false, '');
end;

{ TVectorInterpolatorNode ---------------------------------------------------- }

function TVectorInterpolatorNode.CreateOutputField: TMFFloat;
begin
  Result := TMFFloat.CreateUndefined(Self, false, '');
end;

{ registration --------------------------------------------------------------- }

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
