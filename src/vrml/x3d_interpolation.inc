{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractInterpolatorNode = class(TAbstractChildNode)
  protected
    function KeyRange(const Fraction: Single; out T: Single): Integer;
  public
    procedure CreateNode; override;

    { Event: SFFloat, in } { }
    private FEventSet_fraction: TVRMLEvent;
    public property EventSet_fraction: TVRMLEvent read FEventSet_fraction;

    private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;
  end;

  TColorInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFColor;
    public property FdKeyValue: TMFColor read FFdKeyValue;

    { Event: SFColor, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TColorSetInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFColor;
    public property FdKeyValue: TMFColor read FFdKeyValue;

    { Event: MFColor, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TCoordinateInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event: MFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TCoordinateInterpolator2DNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    { Event: MFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TEaseInEaseOutNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventSet_fraction: TVRMLEvent;
    public property EventSet_fraction: TVRMLEvent read FEventSet_fraction;

    private FFdEaseInEaseOut: TMFVec2f;
    public property FdEaseInEaseOut: TMFVec2f read FFdEaseInEaseOut;

    private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    { Event: SFFloat, out } { }
    private FEventModifiedFraction_changed: TVRMLEvent;
    public property EventModifiedFraction_changed: TVRMLEvent read FEventModifiedFraction_changed;
  end;

  TNormalInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event: MFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFRotation;
    public property FdKeyValue: TMFRotation read FFdKeyValue;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TPositionInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TPositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    { Event: SFFloat, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TSplinePositionInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFVec3f;
    public property FdKeyValue: TMFVec3f read FFdKeyValue;

    private FFdKeyVelocity: TMFVec3f;
    public property FdKeyVelocity: TMFVec3f read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TSplinePositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFVec2f;
    public property FdKeyValue: TMFVec2f read FFdKeyValue;

    private FFdKeyVelocity: TMFVec2f;
    public property FdKeyVelocity: TMFVec2f read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TSplineScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    private FFdKeyVelocity: TMFFloat;
    public property FdKeyVelocity: TMFFloat read FFdKeyVelocity;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event: SFFloat, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TSquadOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFRotation;
    public property FdKeyValue: TMFRotation read FFdKeyValue;

    private FFdNormalizeVelocity: TSFBool;
    public property FdNormalizeVelocity: TSFBool read FFdNormalizeVelocity;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
  end;

  TVectorInterpolatorNode = class(TAbstractInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFFloat;
    public property FdKeyValue: TMFFloat read FFdKeyValue;

    { Event: MFFloat, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;
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
  // will work Ok, but setting ParentNode allows Avalon Logger node
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
      OnWarning(wtMajor, 'VRML/X3D', Format('Not enough keyValue items for %s interpolator',
        [NodeTypeName]));
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
      Result.Items.AssignLerp(T, FdKeyValue.Items, FdKeyValue.Items,
        Index1 * FrameValues, Index2 * FrameValues, FrameValues);
    end else
      OnWarning(wtMajor, 'VRML/X3D', Format('Not enough keyValue items for %s interpolator',
        [NodeTypeName]));
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
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
    begin
      OutputValue := KeyValueLerp(T, Range - 1, Range);
      MultipleInterpolatorFieldTypePostprocess
    end;
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

  FEventSet_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventSet_fraction);

  FFdKey := TMFFloat.Create(Self, 'key', []);
  Fields.Add(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TAbstractInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
begin
  Result := VRMLNodes.KeyRange(FdKey.Items, Fraction, T);
end;

procedure TColorInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFColor.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: [0,1] }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFColor, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TColorInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'ColorInterpolator';
end;

class function TColorInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TColorInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TColorSetInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFColor.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFColor, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TColorSetInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'ColorSetInterpolator';
end;

class function TColorSetInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TColorSetInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define MultipleInterpolatorFieldType := TMFColor}
{$define MultipleInterpolatorFieldTypePostprocess := }
MultipleInterpolator

procedure TCoordinateInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFVec3f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCoordinateInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator';
end;

class function TCoordinateInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCoordinateInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorFieldTypePostprocess := }
MultipleInterpolator

procedure TCoordinateInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFVec2f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TCoordinateInterpolator2DNode.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator2D';
end;

class function TCoordinateInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCoordinateInterpolator2DNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define MultipleInterpolatorFieldType := TMFVec2f}
{$define MultipleInterpolatorFieldTypePostprocess := }
MultipleInterpolator

procedure TEaseInEaseOutNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventSet_fraction);

  FFdEaseInEaseOut := TMFVec2f.Create(Self, 'easeInEaseOut', []);
  Fields.Add(FFdEaseInEaseOut);
  { X3D specification comment: (-Inf,Inf) }

  FFdKey := TMFFloat.Create(Self, 'key', []);
  Fields.Add(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventModifiedFraction_changed := TVRMLEvent.Create(Self, 'modifiedFraction_changed', TSFFloat, false);
  Events.Add(FEventModifiedFraction_changed);

  DefaultContainerField := 'children';
end;

class function TEaseInEaseOutNode.ClassNodeTypeName: string;
begin
  Result := 'EaseInEaseOut';
end;

class function TEaseInEaseOutNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNormalInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFVec3f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNormalInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'NormalInterpolator';
end;

class function TNormalInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNormalInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorFieldTypePostprocess := OutputValue.Items.Normalize;}
MultipleInterpolator

procedure TOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFRotation.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TOrientationInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'OrientationInterpolator';
end;

class function TOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TOrientationInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
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
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TPositionInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator';
end;

class function TPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TPositionInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TPositionInterpolator2DNode.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator2D';
end;

class function TPositionInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionInterpolator2DNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define SingleInterpolatorType := TVector2Single}
{$define SingleInterpolatorDefault := ZeroVector2Single}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TScalarInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFFloat, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TScalarInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'ScalarInterpolator';
end;

class function TScalarInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TScalarInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define SingleInterpolatorType := Single}
{$define SingleInterpolatorDefault := 0}
{$define SingleInterpolatorLerp := Lerp}
SingleInterpolator

procedure TSplinePositionInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  Fields.Add(FFdClosed);

  FFdKeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec3f.Create(Self, 'keyVelocity', []);
  Fields.Add(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  Fields.Add(FFdNormalizeVelocity);

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator';
end;

class function TSplinePositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TSplinePositionInterpolator2DNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  Fields.Add(FFdClosed);

  FFdKeyValue := TMFVec2f.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFVec2f.Create(Self, 'keyVelocity', []);
  Fields.Add(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  Fields.Add(FFdNormalizeVelocity);

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplinePositionInterpolator2DNode.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator2D';
end;

class function TSplinePositionInterpolator2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TSplineScalarInterpolatorNode.CreateNode;
begin
  inherited;

  FFdClosed := TSFBool.Create(Self, 'closed', false);
  Fields.Add(FFdClosed);

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdKeyVelocity := TMFFloat.Create(Self, 'keyVelocity', []);
  Fields.Add(FFdKeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  Fields.Add(FFdNormalizeVelocity);

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFFloat, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSplineScalarInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'SplineScalarInterpolator';
end;

class function TSplineScalarInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TSquadOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFRotation.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdNormalizeVelocity := TSFBool.Create(Self, 'normalizeVelocity', false);
  Fields.Add(FFdNormalizeVelocity);

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TSquadOrientationInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'SquadOrientationInterpolator';
end;

class function TSquadOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TVectorInterpolatorNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFFloat.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFFloat, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TVectorInterpolatorNode.ClassNodeTypeName: string;
begin
  Result := 'VectorInterpolator';
end;

class function TVectorInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure TVectorInterpolatorNode.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
{$define MultipleInterpolatorFieldType := TMFFloat}
{$define MultipleInterpolatorFieldTypePostprocess := }
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
