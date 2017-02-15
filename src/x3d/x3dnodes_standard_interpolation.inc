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
  { Basis for all nodes interpolating (animating) values. }
  TAbstractInterpolatorNode = class(TAbstractChildNode)
  protected
    function KeyRange(const Fraction: Single; out T: Single): Integer;
  public
    procedure CreateNode; override;

    { Event in } { }
    private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    {$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
  end;

  { Interpolate (animate) a single color. }
  TColorInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFColor;
    public property FdKeyValue: TMFColor read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TSFColorEvent;
    public property EventValue_changed: TSFColorEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
  end;

  { Interpolate (animate) a set of colors,
    for example to animate a set of @link(TBackgroundNode)
    or @link(TColorNode) colors. }
  TColorSetInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFColor;
    public property FdKeyValue: TMFColor read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TMFColorEvent;
    public property EventValue_changed: TMFColorEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
  end;

  { Interpolate (animate) a set of 3D positions,
    for example to animate @link(TCoordinateNode coordinates) of a mesh. }
  TCoordinateInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TMFVec3fEvent;
    public property EventValue_changed: TMFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
  end;

  { Interpolate (animate) a set of 2D positions,
    for example to animate 2D texture coordinates. }
  TCoordinateInterpolator2DNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TMFVec2fEvent;
    public property EventValue_changed: TMFVec2fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
  end;

  { Support controlled gradual transitions by modifying
    TimeSensor node fractions.
    @bold(Not implemented yet.) }
  TEaseInEaseOutNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    private FFdEaseInEaseOut: TMFVec2f;
    public property FdEaseInEaseOut: TMFVec2f read FFdEaseInEaseOut;

    private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    { Event out } { }
    private FEventModifiedFraction_changed: TSFFloatEvent;
    public property EventModifiedFraction_changed: TSFFloatEvent read FEventModifiedFraction_changed;

    {$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
  end;

  { Interpolate (animate) a set of 3D directions, for example normal vectors. }
  TNormalInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TMFVec3fEvent;
    public property EventValue_changed: TMFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
  end;

  { Interpolate (animate) an orientation,
    for example to animate @link(TTransformNode.Rotation). }
  TOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFRotation;
    public property FdKeyValue: TMFRotation read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
  end;

  { Interpolate (animate) a 3D position,
    for example to animate @link(TTransformNode.Translation). }
  TPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
  end;

  { Interpolate (animate) a 2D position. }
  TPositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TSFVec2fEvent;
    public property EventValue_changed: TSFVec2fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
  end;

  { Interpolate (animate) a single floating-point value,
    for example to animate @link(TMaterialNode.Transparency). }
  TScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TSFFloatEvent;
    public property EventValue_changed: TSFFloatEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 3D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    private FFdKeyVelocity: TMFVec3f;
    public property FdKeyVelocity: TMFVec3f read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 2D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    private FFdKeyVelocity: TMFVec2f;
    public property FdKeyVelocity: TMFVec2f read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    private FEventValue_changed: TSFVec2fEvent;
    public property EventValue_changed: TSFVec2fEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
  end;

  { Non-linearly interpolate (animate) a single floating-point value.
    @bold(Not implemented yet.) }
  TSplineScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    private FFdKeyVelocity: TMFFloat;
    public property FdKeyVelocity: TMFFloat read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    private FEventValue_changed: TSFFloatEvent;
    public property EventValue_changed: TSFFloatEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a rotation.
    @bold(Not implemented yet.) }
  TSquadOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFRotation;
    public property FdKeyValue: TMFRotation read FFdKeyValue;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event out } { }
    private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
  end;

  { Interpolate (animate) a set of floats, for example to animate
    @link(TElevationGridNode) heights. }
  TVectorInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    { Event out } { }
    private FEventValue_changed: TMFFloatEvent;
    public property EventValue_changed: TMFFloatEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$define SingleInterpolator :=

  function KeyValue(Index: Integer): SingleInterpolatorType;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.L[Index] else
      Result := SingleInterpolatorDefault;
  end;

var
  T: Single;
  OutputValue: SingleInterpolatorType;
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
    Assert(Value is TSFFloat, 'EventSet_FractionReceive (for single-value field) can only process SFFloat input events');
    Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive (for single-value field) cannot handle fraction = NaN');
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
      OutputValue := SingleInterpolatorLerp(T, KeyValue(Range - 1), KeyValue(Range));
  end;
  EventValue_Changed.Send(OutputValue, Time);
end;}

{$define MultipleInterpolator :=
var
  KeyCount: Integer;

  // We pass Self as ParentNode to
  // MultipleInterpolatorFieldType.CreateUndefined.
  // This is not required, ParentNode could be @nil and everything
  // will work Ok, but setting ParentNode allows InstantReality Logger node
  // to display "sending node" information, useful for debugging.

  function KeyValue(Index: Integer): MultipleInterpolatorFieldType;
  var
    FrameValues: Integer;
  begin
    FrameValues := FdKeyValue.Items.Count div KeyCount;
    Result := MultipleInterpolatorFieldType.CreateUndefined(Self, '', false);

    if (Index + 1) * FrameValues <= FdKeyValue.Items.Count then
    begin
      Result.Items.AddListRange(FdKeyValue.Items, Index * FrameValues, FrameValues);
    end else
      WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
        [X3DType]));
  end;

  // This actually knows that Index2 = Index1 + 1, but assumes only
  // Index2 > Index1
  function KeyValueLerp(const T: Single;
    Index1, Index2: Integer): MultipleInterpolatorFieldType;
  var
    FrameValues: Integer;
  begin
    FrameValues := FdKeyValue.Items.Count div KeyCount;
    Result := MultipleInterpolatorFieldType.CreateUndefined(Self, '', false);

    if (Index2 + 1) * FrameValues <= FdKeyValue.Items.Count then
    begin
      Result.Items.MultipleInterpolatorAssignLerp(
        T, FdKeyValue.Items, FdKeyValue.Items,
        Index1 * FrameValues, Index2 * FrameValues, FrameValues);
    end else
      WritelnWarning('VRML/X3D', Format('Not enough keyValue items for %s interpolator',
        [X3DType]));
  end;

var
  T: Single;
  OutputValue: MultipleInterpolatorFieldType;
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
  end else
  begin
    Assert(Value is TSFFloat, 'EventSet_FractionReceive (for multiple-value field) can only process SFFloat input events');
    Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive (for multiple-value field) cannot handle fraction = NaN');
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
      OutputValue := KeyValueLerp(T, Range - 1, Range);
  end;
  try
    EventValue_Changed.Send(OutputValue, Time);
  finally
    FreeAndNil(OutputValue);
  end;
end;}

procedure TAbstractInterpolatorNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdKey := TMFFloat.Create(Self, 'key', []);
  AddField(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TAbstractInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
begin
  Result := X3DNodes.KeyRange(FdKey.Items, Fraction, T);
end;

procedure TColorInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFColor.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: [0,1] }

  FEventValue_changed := TSFColorEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TColorInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ColorInterpolator';
end;

class function TColorInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TColorInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
{$define SingleInterpolatorLerp := LerpRgbInHsv}
SingleInterpolator

procedure TColorSetInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFColor.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TMFColorEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TColorSetInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ColorSetInterpolator';
end;

class function TColorSetInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType) or
    URNMatchingCastle(URN, ClassX3DType);
end;

procedure TColorSetInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define MultipleInterpolatorFieldType := TMFColor}
{$define MultipleInterpolatorAssignLerp := AssignLerpRgbInHsv}
MultipleInterpolator

procedure TCoordinateInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TMFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCoordinateInterpolatorNode.ClassX3DType: string;
begin
  Result := 'CoordinateInterpolator';
end;

class function TCoordinateInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCoordinateInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorAssignLerp := AssignLerp}
MultipleInterpolator

procedure TCoordinateInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TMFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCoordinateInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'CoordinateInterpolator2D';
end;

class function TCoordinateInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCoordinateInterpolator2DNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define MultipleInterpolatorFieldType := TMFVec2f}
{$define MultipleInterpolatorAssignLerp := AssignLerp}
MultipleInterpolator

procedure TEaseInEaseOutNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdEaseInEaseOut := TMFVec2f.Create(Self, 'easeInEaseOut', []);
  AddField(FFdEaseInEaseOut);
  { X3D specification comment: (-Inf,Inf) }

  FFdKey := TMFFloat.Create(Self, 'key', []);
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

class function TEaseInEaseOutNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNormalInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TMFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNormalInterpolatorNode.ClassX3DType: string;
begin
  Result := 'NormalInterpolator';
end;

class function TNormalInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNormalInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorAssignLerp := AssignLerpNormalize}
MultipleInterpolator

procedure TOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFRotation.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'OrientationInterpolator';
end;

class function TOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TOrientationInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define SingleInterpolatorType := TVector4Single}
{$define SingleInterpolatorDefault := ZeroVector4Single}

{ In my tests, NLerp works as good as SLerp here.
  Although NLerp doesn't guarantee constant velocity, and should be
  faster --- neither of these qualities seem to be noticeable
  in practice. }

{$define SingleInterpolatorLerp := SLerp}
SingleInterpolator

procedure TPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TPositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'PositionInterpolator';
end;

class function TPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TPositionInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TPositionInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'PositionInterpolator2D';
end;

class function TPositionInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionInterpolator2DNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define SingleInterpolatorType := TVector2Single}
{$define SingleInterpolatorDefault := ZeroVector2Single}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TScalarInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFFloatEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TScalarInterpolatorNode.ClassX3DType: string;
begin
  Result := 'ScalarInterpolator';
end;

class function TScalarInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TScalarInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define SingleInterpolatorType := Single}
{$define SingleInterpolatorDefault := 0}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TSplinePositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec3f.Create(Self, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SplinePositionInterpolator';
end;

class function TSplinePositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSplinePositionInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec2f.Create(Self, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolator2DNode.ClassX3DType: string;
begin
  Result := 'SplinePositionInterpolator2D';
end;

class function TSplinePositionInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSplineScalarInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  AddField(FFdClosed);

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFFloat.Create(Self, 'keyVelocity', []);
  AddField(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFFloatEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplineScalarInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SplineScalarInterpolator';
end;

class function TSplineScalarInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TSquadOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFRotation.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  AddField(FFdNormalizeVelocity);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSquadOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'SquadOrientationInterpolator';
end;

class function TSquadOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TVectorInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TMFFloatEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TVectorInterpolatorNode.ClassX3DType: string;
begin
  Result := 'VectorInterpolator';
end;

class function TVectorInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType) or
    URNMatchingCastle(URN, ClassX3DType);
end;

procedure TVectorInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
{$define MultipleInterpolatorFieldType := TMFFloat}
{$define MultipleInterpolatorAssignLerp := AssignLerp}
MultipleInterpolator

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
