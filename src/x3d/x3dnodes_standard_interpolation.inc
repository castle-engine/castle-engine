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
  { Basis for all nodes interpolating (animating) values. }
  TAbstractInterpolatorNode = class(TAbstractChildNode)
  strict protected
    function KeyRange(const Fraction: Single; out T: Single): Integer;
  public
    procedure CreateNode; override;

    { Event in } { }
    strict private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    strict private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    {$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
  end;

  { Basis for all nodes interpolating (animating) a single value. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TAbstractSingleInterpolatorNode<
    TInterpolatedValue;
    {$ifndef FPC}
    TSingleField: TX3DSingleField;
    TInternalItemList: class;
    TItemList: ICastleItemList<TInterpolatedValue, TInternalItemList>;
    TEvent;
    {$endif}
///    {$ifndef FPC}
///    TInternalItemList: class; // neded to make conversion from TItemList available
///    TItemList: ICastleItemList<TInterpolatedValue, TInternalItemList>;
///    {$endif}
    TKeyValue{$ifndef FPC}: TX3DSimpleMultField<TInterpolatedValue, TSingleField, TInternalItemList, TItemList, TEvent>{$endif};///{$ifndef FPC}: IX3DSimpleMultField<TInterpolatedValue, TInternalItemList, TItemList>{$endif};
    TKeyValueEvent{$ifndef FPC}{:TX3DEvent}: IX3DEvent<TInterpolatedValue>{$endif} > = class(TAbstractInterpolatorNode)
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
    {$ifndef FPC}
    TInterpolatedValue;
    TSingleField: TX3DSingleField;
    TInternalItemList: class;
    TItemList: ICastleItemList<TInterpolatedValue, TInternalItemList>;
    TEvent;
    {$endif}
///    {$ifndef FPC}
///    TInterpolatedValue;
///    TInternalItemList;
///    TItemList: ICastleItemList<TInterpolatedValue, TInternalItemList>;
///    {$endif}
    TKeyValue{$ifndef FPC}: TX3DSimpleMultField<TInterpolatedValue, TSingleField, TInternalItemList, TItemList, TEvent>{$endif}; ///{$ifndef FPC}: IX3DSimpleMultField<TInterpolatedValue, TInternalItemList, TItemList>{$endif};
    TKeyValueEvent{$ifndef FPC}:TX3DEvent{$endif}> = class(TAbstractInterpolatorNode)
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
  TColorInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TAbstractSingleInterpolatorNode<
    TVector3,
    {$ifndef FPC}
    TSFVec3f,
    TStructList<TVector3>,
    TVector3List,
    TMFVec3fEvent,
    {$endif}
///    {$ifndef FPC}
///    TStructList<TVector3>,
///    TVector3List,
///    {$endif}
    TMFColor,
    TSFColorEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector3): TVector3; override;
  public
    class function ClassX3DType: string; override;

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
  public
    class function ClassX3DType: string; override;

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
  public
    class function ClassX3DType: string; override;

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
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
  end;

  { Support controlled gradual transitions by modifying
    TimeSensor node fractions.
    @bold(Not implemented yet.) }
  TEaseInEaseOutNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    strict private FFdEaseInEaseOut: TMFVec2f;
    public property FdEaseInEaseOut: TMFVec2f read FFdEaseInEaseOut;

    strict private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    { Event out } { }
    strict private FEventModifiedFraction_changed: TSFFloatEvent;
    public property EventModifiedFraction_changed: TSFFloatEvent read FEventModifiedFraction_changed;

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
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
  end;

  { Interpolate (animate) an orientation,
    for example to animate @link(TTransformNode.Rotation). }
  TOrientationInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TVector4,
    {$ifndef FPC}
    TSFRotation,
    TStructList<TVector4>,
    TVector4List,
    TMFRotationEvent,
    {$endif}
    TMFRotation,
    TSFRotationEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector4): TVector4; override;
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
  end;

  { Interpolate (animate) a 3D position,
    for example to animate @link(TTransformNode.Translation). }
  TPositionInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TVector3,
    {$ifndef FPC}
    TSFVec3f,
    TStructList<TVector3>,
    TVector3List,
    TMFVec3fEvent,
    {$endif}
    TMFVec3f,
    TSFVec3fEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector3): TVector3; override;
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
  end;

  { Interpolate (animate) a 2D position. }
  TPositionInterpolator2DNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    TVector2,
    {$ifndef FPC}
    TSFVec2f,
    TStructList<TVector2>,
    TVector2List,
    TMFVec2fEvent,
    {$endif}
    TMFVec2f,
    TSFVec2fEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: TVector2): TVector2; override;
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
  end;

  { Interpolate (animate) a single floating-point value,
    for example to animate @link(TMaterialNode.Transparency). }
  TScalarInterpolatorNode = class({$ifdef CASTLE_OBJFPC}specialize{$endif}
    TAbstractSingleInterpolatorNode<
    Single,
    {$ifndef FPC}
    TSFFloat,
    TStructList<Single>,
    TSingleList,
    TMFFloatEvent,
    {$endif}
    TMFFloat,
    TSFFloatEvent>)
  strict protected
    function InterpolatorLerp(const A: Single;
      const V1, V2: Single): Single; override;
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 3D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    strict private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    strict private FFdKeyVelocity: TMFVec3f;
    public property FdKeyVelocity: TMFVec3f read FFdKeyVelocity;

    strict private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    strict private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 2D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    strict private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    strict private FFdKeyVelocity: TMFVec2f;
    public property FdKeyVelocity: TMFVec2f read FFdKeyVelocity;

    strict private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    strict private FEventValue_changed: TSFVec2fEvent;
    public property EventValue_changed: TSFVec2fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
  end;

  { Non-linearly interpolate (animate) a single floating-point value.
    @bold(Not implemented yet.) }
  TSplineScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    strict private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    strict private FFdKeyVelocity: TMFFloat;
    public property FdKeyVelocity: TMFFloat read FFdKeyVelocity;

    strict private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    strict private FEventValue_changed: TSFFloatEvent;
    public property EventValue_changed: TSFFloatEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a rotation.
    @bold(Not implemented yet.) }
  TSquadOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdKeyValue: TMFRotation;
    public property FdKeyValue: TMFRotation read FFdKeyValue;

    strict private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    strict private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

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
  public
    class function ClassX3DType: string; override;

    {$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractInterpolatorNode -------------------------------------------------- }

procedure TAbstractInterpolatorNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdKey := TMFFloat.Create(Self, {$ifdef CASTLE_SLIM_NODES} false {$else} true {$endif}, 'key', []);
  AddField(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TAbstractInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
begin
  Result := X3DNodes.KeyRange(FdKey.Items, Fraction, T);
end;

{ TAbstractSingleInterpolatorNode -------------------------------------------- }

function TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  {$ifndef FPC}
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  KeyValue(Index: Integer): TInterpolatedValue;
begin
  if Index < FdKeyValue.Items.Count then
  {$ifdef FPC}
    Result := FdKeyValue.Items.List^[Index]
  {$else}
    Result := FdKeyValue.Items.InternalGetItem(Index)
  {$endif}
  else
    Result := InterpolatorDefault;
end;

procedure TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  {$ifndef FPC}
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
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
    OutputValue := InterpolatorLerp(T, KeyValue(Range - 1), KeyValue(Range));

  EventValue_Changed.Send(OutputValue, Time, Value.InternalPartialSend);
end;

procedure TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  {$ifndef FPC}
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
  TKeyValue,
  TKeyValueEvent>{$endif} .
  CreateNode;
{$ifndef FPC}
var
  AValue: TValue;
  RttiContext: TRttiContext;
  KeyType: TRttiType;
  AMethCreate: TRttiMethod;
  InstanceType: TRttiInstanceType;
  ArrayOfTInterpolatedValue: TArray<TInterpolatedValue>;
  ArrayValue: TValue;
{$endif}
begin
  inherited;
  {$ifdef FPC}
  FFdKeyValue := TKeyValue.Create(Self, true, 'keyValue', []);

  AddField(FFdKeyValue);

  FEventValue_changed := TKeyValueEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);
  {$else}
  SetLength(ArrayOfTInterpolatedValue, 0);

  { https://stackoverflow.com/questions/8355456/creating-object-instance-based-on-unconstrained-generic-type }
  RttiContext := TRttiContext.Create;
  try
    KeyType := RttiContext.GetType(TypeInfo(TKeyValue));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 4) then
      begin
        InstanceType := KeyType.AsInstance;
        ArrayValue := TValue.From<TArray<TInterpolatedValue>>(ArrayOfTInterpolatedValue);
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, true, 'keyValue',
          ArrayValue]);
        FFdKeyValue := AValue.AsType<TKeyValue>;

        break;
      end;
    end;

    AddField(FFdKeyValue as TX3DField);

    KeyType := RttiContext.GetType(TypeInfo(TKeyValueEvent));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 3) then
      begin
        InstanceType := KeyType.AsInstance;
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, 'value_changed', false]);
        FEventValue_changed := AValue.AsType<TKeyValueEvent>;
        AddEvent(FEventValue_changed as TX3DEvent);
        break;
      end;
    end;
  finally
    RttiContext.Free;
  end;
  {$endif}

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

function TAbstractSingleInterpolatorNode {$ifndef CASTLE_OBJFPC} <
  TInterpolatedValue,
  {$ifndef FPC}
  TSingleField,
  TInternalItemList,
  TItemList,
  TEvent,
  {$endif}
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
  {$ifndef FPC}
  AValue: TValue;
  RttiContext: TRttiContext;
  KeyType: TRttiType;
  AMethCreate: TRttiMethod;
  InstanceType: TRttiInstanceType;
  {$endif}
begin
  FrameValues := FdKeyValue.Items.Count div KeyCount;

  // We pass Self as ParentNode to
  // TKeyValue.CreateUndefined.
  // This is not required, ParentNode could be @nil and everything
  // will work Ok, but setting ParentNode allows InstantReality Logger node
  // to display "sending node" information, useful for debugging.
  {$ifdef FPC}
  Result := TKeyValue.CreateUndefined(Self, false, '');
  {$else}
  { https://stackoverflow.com/questions/8355456/creating-object-instance-based-on-unconstrained-generic-type }
  RttiContext := TRttiContext.Create;
  try
    KeyType := RttiContext.GetType(TypeInfo(TKeyValue));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 3) then
      begin
        InstanceType := KeyType.AsInstance;
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, false, '']);
        Result := AValue.AsType<TKeyValue>;
        break;
      end;
    end;
  finally
    RttiContext.Free;
  end;
  {$endif}

  if (Index + 1) * FrameValues <= FdKeyValue.Items.Count then
  begin
  {$ifdef FPC}
    Result.Items.AddSubRange(FdKeyValue.Items, Index * FrameValues, FrameValues);
  {$else}
    Result.Items.AddSubRange(FdKeyValue.GetInternalItems, Index * FrameValues, FrameValues);
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
  {$ifndef FPC}
  AValue: TValue;
  RttiContext: TRttiContext;
  KeyType: TRttiType;
  AMethCreate: TRttiMethod;
  InstanceType: TRttiInstanceType;
  {$endif}
begin
  // This actually knows that Index2 = Index1 + 1, but assumes only
  // Index2 > Index1

  FrameValues := FdKeyValue.Items.Count div KeyCount;
  {$ifdef FPC}
  Result := TKeyValue.CreateUndefined(Self, false, '');
  {$else}
  { https://stackoverflow.com/questions/8355456/creating-object-instance-based-on-unconstrained-generic-type }
  RttiContext := TRttiContext.Create;
  try
    KeyType := RttiContext.GetType(TypeInfo(TKeyValue));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 3) then
      begin
        InstanceType := KeyType.AsInstance;
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, false, '']);
        Result := AValue.AsType<TKeyValue>;
        break;
      end;
    end;
  finally
    RttiContext.Free;
  end;
  {$endif}

  if (Index2 + 1) * FrameValues <= FdKeyValue.Items.Count then
  begin
    InterpolatorLerp(Result, T, FdKeyValue, FdKeyValue,
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
    OutputValue := KeyValueLerp(T, Range - 1, Range, KeyCount);
  OutputValue.InternalPartialSend := Value.InternalPartialSend;
  try
    EventValue_Changed.Send(OutputValue, Time);
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
  CreateNode;
{$ifndef FPC}
var
  AValue: TValue;
  RttiContext: TRttiContext;
  KeyType: TRttiType;
  AMethCreate: TRttiMethod;
  InstanceType: TRttiInstanceType;
  ArrayOfTInterpolatedValue: TArray<TInterpolatedValue>;
  ArrayValue: TValue;
{$endif}
begin
  inherited;

  {$ifdef FPC}
  FFdKeyValue := TKeyValue.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventValue_changed := TKeyValueEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);
  {$else}
  SetLength(ArrayOfTInterpolatedValue, 0);

  { https://stackoverflow.com/questions/8355456/creating-object-instance-based-on-unconstrained-generic-type }
  RttiContext := TRttiContext.Create;
  try
    KeyType := RttiContext.GetType(TypeInfo(TKeyValue));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 4) then
      begin
        InstanceType := KeyType.AsInstance;
        ArrayValue := TValue.From<TArray<TInterpolatedValue>>(ArrayOfTInterpolatedValue);
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, true, 'keyValue',
          ArrayValue]);
        FFdKeyValue := AValue.AsType<TKeyValue>;

        break;
      end;
    end;

    AddField(FFdKeyValue as TX3DField);

    KeyType := RttiContext.GetType(TypeInfo(TKeyValueEvent));
    for AMethCreate in KeyType.GetMethods do
    begin
      if (AMethCreate.IsConstructor) and (Length(AMethCreate.GetParameters) = 3) then
      begin
        InstanceType := KeyType.AsInstance;
        AValue := AMethCreate.Invoke(InstanceType.MetaclassType, [Self, 'value_changed', false]);
        FEventValue_changed := AValue.AsType<TKeyValueEvent>;
        AddEvent(FEventValue_changed as TX3DEvent);
        break;
      end;
    end;
  finally
    RttiContext.Free;
  end;
  {$endif}


  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification(
    {$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
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
  Target.Items.AssignLerp(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
  {$else}
  Target.Items.AssignLerp(Fraction, F1.GetInternalItems, F2.GetInternalItems, Index1, Index2, ACount);
  {$endif}
end;

{ TColorInterpolatorNode ----------------------------------------------------- }

class function TColorInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ColorInterpolator';
end;

function TColorInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector3): TVector3;
begin
  Result := LerpRgbInHsv(A, V1, V2);
end;

{ TColorSetInterpolatorNode -------------------------------------------------- }

class function TColorSetInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ColorSetInterpolator';
end;

procedure TColorSetInterpolatorNode.InterpolatorLerp(const Target: TMFColor;
  const Fraction: Single; const F1, F2: TMFColor;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpRgbInHsv(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

{ TCoordinateInterpolatorNode ------------------------------------------------ }

class function TCoordinateInterpolatorNode.ClassX3DType: string;
begin
  Result := 'CoordinateInterpolator';
end;

class function TCoordinateInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'CoordinateInterpolator2D';
end;

procedure TEaseInEaseOutNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdEaseInEaseOut := TMFVec2f.Create(Self, true, 'easeInEaseOut', []);
  AddField(FFdEaseInEaseOut);
  { X3D specification comment: (-Inf,Inf) }

  FFdKey := TMFFloat.Create(Self, true, 'key', []);
  AddField(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventModifiedFraction_changed := TSFFloatEvent.Create(Self, 'modifiedFraction_changed', false);
  AddEvent(FEventModifiedFraction_changed);

  DefaultContainerField := 'children';
end;

class function TEaseInEaseOutNode.ClassX3DType: string;
begin
  Result := 'EaseInEaseOut';
end;

class function TNormalInterpolatorNode.ClassX3DType: string;
begin
  Result := 'NormalInterpolator';
end;

procedure TNormalInterpolatorNode.InterpolatorLerp(
  const Target: TMFVec3f;
  const Fraction: Single; const F1, F2: TMFVec3f;
  const Index1, Index2, ACount: Integer);
begin
  Target.Items.AssignLerpNormalize(Fraction, F1.Items, F2.Items, Index1, Index2, ACount);
end;

class function TOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'OrientationInterpolator';
end;

function TOrientationInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector4): TVector4;
begin
  { In my tests, NLerp works as good as SLerp here.
    Although NLerp doesn't guarantee constant velocity, and should be
    faster --- neither of these qualities seem to be noticeable
    in practice. }
  Result := SLerp(A, V1, V2);
end;

class function TPositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'PositionInterpolator';
end;

function TPositionInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector3): TVector3;
begin
  Result := Lerp(A, V1, V2);
end;

class function TPositionInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'PositionInterpolator2D';
end;

function TPositionInterpolator2DNode.InterpolatorLerp(const A: Single;
  const V1, V2: TVector2): TVector2;
begin
  Result := Lerp(A, V1, V2);
end;

class function TScalarInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ScalarInterpolator';
end;

function TScalarInterpolatorNode.InterpolatorLerp(const A: Single;
  const V1, V2: Single): Single;
begin
  Result := Lerp(A, V1, V2);
end;

procedure TSplinePositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, true, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFVec3f.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec3f.Create(Self, true, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, true, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SplinePositionInterpolator';
end;

procedure TSplinePositionInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, true, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFVec2f.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec2f.Create(Self, true, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, true, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'SplinePositionInterpolator2D';
end;

procedure TSplineScalarInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, true, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFFloat.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFFloat.Create(Self, true, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, true, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFFloatEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplineScalarInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SplineScalarInterpolator';
end;

procedure TSquadOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFRotation.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, true, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSquadOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SquadOrientationInterpolator';
end;

class function TVectorInterpolatorNode.ClassX3DType: string;
begin
  Result := 'VectorInterpolator';
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
