{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractSequencerNode = class(TAbstractChildNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
    procedure EventNextReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
    procedure EventPreviousReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  private
    FCurrentKeyRange: Integer;
  protected
    { Analogous to TAbstractInterpolatorNode.KeyRange. }
    function KeyRange(const Fraction: Single): Integer;

    { In what key range we currently are. In other words, which
      keyValue was last send (passed to SendValueChanged).

      We have to keep track of this information, per each node,
      as specification clearly says that value_changed should not be
      generated continously on each set_fraction, but only when value
      actually changes.

      Valid values here are between -1 and FdKey.Count - 1.
      Values between 0 and FdKey.Count - 1 indicate which keyValue was
      last send. Value -1 means that SendValueChanged was not yet called,
      this is the default value (after construction). }
    property CurrentKeyRange: Integer read FCurrentKeyRange;
    procedure SetCurrentKeyRange(const Value: Integer; const Time: TX3DTime);

    { Override this is descendants to actually send value_changed event
      witk keyValue[ValueIndex]. You are guaranteed that ValueIndex
      is between 0 and FdKey.Count - 1, but you have to check yourself
      whether it's also between 0 and FdKeyValue.Count - 1
      (although spec requires that key and keyValue fields have the same size,
      we have to protect against invalid files). }
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime); virtual; abstract;
  public
    procedure CreateNode; override;

    { Event: SFBool, in } { }
    private FEventNext: TX3DEvent;
    public property EventNext: TX3DEvent read FEventNext;

    { Event: SFBool, in } { }
    private FEventPrevious: TX3DEvent;
    public property EventPrevious: TX3DEvent read FEventPrevious;

    { Event: SFFloat, in } { }
    private FEventSet_fraction: TX3DEvent;
    public property EventSet_fraction: TX3DEvent read FEventSet_fraction;

    private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;
  end;

  TAbstractTriggerNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;
  end;

  TBooleanFilterNode = class(TAbstractChildNode)
  private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventSet_boolean: TX3DEvent;
    public property EventSet_boolean: TX3DEvent read FEventSet_boolean;

    { Event: SFBool, out } { }
    private FEventInputFalse: TX3DEvent;
    public property EventInputFalse: TX3DEvent read FEventInputFalse;

    { Event: SFBool, out } { }
    private FEventInputNegate: TX3DEvent;
    public property EventInputNegate: TX3DEvent read FEventInputNegate;

    { Event: SFBool, out } { }
    private FEventInputTrue: TX3DEvent;
    public property EventInputTrue: TX3DEvent read FEventInputTrue;
  end;

  TBooleanSequencerNode = class(TAbstractSequencerNode)
  protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFBool;
    public property FdKeyValue: TMFBool read FFdKeyValue;

    { Event: SFBool, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;
  end;

  TBooleanToggleNode = class(TAbstractChildNode)
  private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventSet_boolean: TX3DEvent;
    public property EventSet_boolean: TX3DEvent read FEventSet_boolean;

    private FFdToggle: TSFBool;
    public property FdToggle: TSFBool read FFdToggle;
  end;

  TBooleanTriggerNode = class(TAbstractTriggerNode)
  private
    procedure EventSet_triggerTimeReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFTime, in } { }
    private FEventSet_triggerTime: TX3DEvent;
    public property EventSet_triggerTime: TX3DEvent read FEventSet_triggerTime;

    { Event: SFBool, out } { }
    private FEventTriggerTrue: TX3DEvent;
    public property EventTriggerTrue: TX3DEvent read FEventTriggerTrue;
  end;

  TIntegerSequencerNode = class(TAbstractSequencerNode)
  protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdKeyValue: TMFInt32;
    public property FdKeyValue: TMFInt32 read FFdKeyValue;

    { Event: SFInt32, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;
  end;

  TIntegerTriggerNode = class(TAbstractTriggerNode)
  private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventSet_boolean: TX3DEvent;
    public property EventSet_boolean: TX3DEvent read FEventSet_boolean;

    private FFdIntegerKey: TSFInt32;
    public property FdIntegerKey: TSFInt32 read FFdIntegerKey;

    { Event: SFInt32, out } { }
    private FEventTriggerValue: TX3DEvent;
    public property EventTriggerValue: TX3DEvent read FEventTriggerValue;
  end;

  TTimeTriggerNode = class(TAbstractTriggerNode)
  private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventSet_boolean: TX3DEvent;
    public property EventSet_boolean: TX3DEvent read FEventSet_boolean;

    { Event: SFTime, out } { }
    private FEventTriggerTime: TX3DEvent;
    public property EventTriggerTime: TX3DEvent read FEventTriggerTime;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractSequencerNode.CreateNode;
begin
  inherited;

  FCurrentKeyRange := -1;

  FEventNext := TX3DEvent.Create(Self, 'next', TSFBool, true);
  Events.Add(FEventNext);
  EventNext.OnReceive.Add(@EventNextReceive);

  FEventPrevious := TX3DEvent.Create(Self, 'previous', TSFBool, true);
  Events.Add(FEventPrevious);
  EventPrevious.OnReceive.Add(@EventPreviousReceive);

  FEventSet_fraction := TX3DEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventSet_fraction);
  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);

  FFdKey := TMFFloat.Create(Self, 'key', []);
  Fields.Add(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TAbstractSequencerNode.KeyRange(
  const Fraction: Single): Integer;
var
  T: Single;
begin
  { T is simply ignored, as it's useless for sequences (since they generate
    discrete events from float fraction, then they are not interested
    in T value). }

  Result := X3DNodes.KeyRange(FdKey.Items, Fraction, T);
end;

procedure TAbstractSequencerNode.SetCurrentKeyRange(const Value: Integer;
  const Time: TX3DTime);
begin
  if FCurrentKeyRange <> Value then
  begin
    Assert(Value >= -1);
    Assert(Value < FdKey.Count);
    FCurrentKeyRange := Value;
    SendValueChanged(Value, Time);
  end;
end;

procedure TAbstractSequencerNode.EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
var
  KeyCount: Integer;
  NewKeyRange: Integer;
begin
  KeyCount := FdKey.Items.Count;
  if KeyCount <> 0 then { for KeyCount = 0, do nothing }
  begin
    NewKeyRange := KeyRange((Value as TSFFloat).Value);

    { For NewKeyRange = 0 (before 1st key), leave it as 0.
      For NewKeyRange = KeyCount, this is after last key, so we want
      NewKeyRange = KeyCount - 1.
      For other values, we're within some range, and NewKeyRange is index
      of the right value --- we want to decrease it, so become index of the left
      value. }
    if NewKeyRange <> 0 then
      Dec(NewKeyRange);

    { Set CurrentKeyRange and eventually send value_changed }
    SetCurrentKeyRange(NewKeyRange, Time);
  end;
end;

procedure TAbstractSequencerNode.EventNextReceive(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
var
  KeyCount, NewKeyRange: Integer;
begin
  KeyCount := FdKey.Items.Count;
  if (Value as TSFBool).Value and (KeyCount <> 0) then
  begin
    { Note that CurrentKeyRange may be -1. Code below will still work then
      (as CurrentKeyRange + 1 is always >= 0).
      This also means that even when KeyCount = 1, calling "next" event
      may be meaningfull: it will cause generation of the first keyvalue. }

    NewKeyRange := CurrentKeyRange + 1;
    if NewKeyRange = KeyCount then
      NewKeyRange := 0;

    { Set CurrentKeyRange and eventually send value_changed }
    SetCurrentKeyRange(NewKeyRange, Time);
  end;
end;

procedure TAbstractSequencerNode.EventPreviousReceive(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
var
  KeyCount, NewKeyRange: Integer;
begin
  KeyCount := FdKey.Items.Count;
  if (Value as TSFBool).Value and (KeyCount <> 0) then
  begin
    { Note that CurrentKeyRange may be -1. Code below will still work then
      (as CurrentKeyRange - 1 is then -2, so we will jump to last item).
      This also means that even when KeyCount = 1, calling "previous" event
      may be meaningfull: it will cause generation of the last keyvalue. }

    NewKeyRange := CurrentKeyRange - 1;
    if NewKeyRange < 0 then
      NewKeyRange := KeyCount - 1;

    { Set CurrentKeyRange and eventually send value_changed }
    SetCurrentKeyRange(NewKeyRange, Time);
  end;
end;

procedure TAbstractTriggerNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TBooleanFilterNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TX3DEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventSet_boolean);
  FEventSet_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FEventInputFalse := TX3DEvent.Create(Self, 'inputFalse', TSFBool, false);
  Events.Add(FEventInputFalse);

  FEventInputNegate := TX3DEvent.Create(Self, 'inputNegate', TSFBool, false);
  Events.Add(FEventInputNegate);

  FEventInputTrue := TX3DEvent.Create(Self, 'inputTrue', TSFBool, false);
  Events.Add(FEventInputTrue);

  DefaultContainerField := 'children';
end;

procedure TBooleanFilterNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    EventInputTrue.Send(B, Time) else
    EventInputFalse.Send(B, Time);
  EventInputNegate.Send(not B, Time);
end;

class function TBooleanFilterNode.ClassNodeTypeName: string;
begin
  Result := 'BooleanFilter';
end;

class function TBooleanFilterNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TBooleanSequencerNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFBool.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFBool, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TBooleanSequencerNode.ClassNodeTypeName: string;
begin
  Result := 'BooleanSequencer';
end;

class function TBooleanSequencerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TBooleanSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime);
begin
  if FdKeyValue.Count <> FdKey.Count then
    OnWarning(wtMajor, 'VRML/X3D', 'BooleanSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time);
end;

procedure TBooleanToggleNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TX3DEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventSet_boolean);
  FEventSet_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FFdToggle := TSFBool.Create(Self, 'toggle', false);
  Fields.Add(FFdToggle);

  DefaultContainerField := 'children';
end;

procedure TBooleanToggleNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    FdToggle.ExposedEvents[true].Send(not FdToggle.Value, Time);
end;

class function TBooleanToggleNode.ClassNodeTypeName: string;
begin
  Result := 'BooleanToggle';
end;

class function TBooleanToggleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TBooleanTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_triggerTime := TX3DEvent.Create(Self, 'set_triggerTime', TSFTime, true);
  Events.Add(FEventSet_triggerTime);
  FEventSet_triggerTime.OnReceive.Add(@EventSet_triggerTimeReceive);

  FEventTriggerTrue := TX3DEvent.Create(Self, 'triggerTrue', TSFBool, false);
  Events.Add(FEventTriggerTrue);

  DefaultContainerField := 'children';
end;

procedure TBooleanTriggerNode.EventSet_triggerTimeReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTrue.Send(true, Time);
end;

class function TBooleanTriggerNode.ClassNodeTypeName: string;
begin
  Result := 'BooleanTrigger';
end;

class function TBooleanTriggerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TIntegerSequencerNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFInt32.Create(Self, 'keyValue', []);
  Fields.Add(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFInt32, false);
  Events.Add(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TIntegerSequencerNode.ClassNodeTypeName: string;
begin
  Result := 'IntegerSequencer';
end;

class function TIntegerSequencerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TIntegerSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime);
begin
  if FdKeyValue.Count <> FdKey.Count then
    OnWarning(wtMajor, 'VRML/X3D', 'IntegerSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time);
end;

procedure TIntegerTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TX3DEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventSet_boolean);
  FEventSet_boolean.OnReceive.Add(@EventSet_booleanReceive);

  { X3D specification mistakenly omitted default value for this field...
    I guess it's 0? }
  FFdIntegerKey := TSFInt32.Create(Self, 'integerKey', 0);
  Fields.Add(FFdIntegerKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventTriggerValue := TX3DEvent.Create(Self, 'triggerValue', TSFInt32, false);
  Events.Add(FEventTriggerValue);

  DefaultContainerField := 'children';
end;

procedure TIntegerTriggerNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventtriggerValue.Send(FdintegerKey.Value, Time);
end;

class function TIntegerTriggerNode.ClassNodeTypeName: string;
begin
  Result := 'IntegerTrigger';
end;

class function TIntegerTriggerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTimeTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TX3DEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventSet_boolean);
  FEventSet_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FEventTriggerTime := TX3DEvent.Create(Self, 'triggerTime', TSFTime, false);
  Events.Add(FEventTriggerTime);

  DefaultContainerField := 'children';
end;

class function TTimeTriggerNode.ClassNodeTypeName: string;
begin
  Result := 'TimeTrigger';
end;

class function TTimeTriggerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTimeTriggerNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTime.Send(Time.Seconds, Time);
end;

procedure RegisterEventUtilitiesNodes;
begin
  NodesManager.RegisterNodeClasses([
    TBooleanFilterNode,
    TBooleanSequencerNode,
    TBooleanToggleNode,
    TBooleanTriggerNode,
    TIntegerSequencerNode,
    TIntegerTriggerNode,
    TTimeTriggerNode
  ]);
end;

{$endif read_implementation}
