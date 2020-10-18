{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base node type from which all sequencers (like @link(TIntegerSequencerNode)
    and @link(TBooleanSequencerNode)) are derived. }
  TAbstractSequencerNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
    procedure EventNextReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
    procedure EventPreviousReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  strict private
    FCurrentKeyRange: Integer;
  strict protected
    { Analogous to TAbstractInterpolatorNode.KeyRange. }
    function KeyRange(const Fraction: Single): Integer;

    { In what key range we currently are. In other words, which
      keyValue was last send (passed to SendValueChanged).

      We have to keep track of this information, per each node,
      as specification clearly says that value_changed should not be
      generated continuously on each set_fraction, but only when value
      actually changes. (Although we make an exception for this
      with our extension @link(ForceContinuousValue_Changed)).

      Valid values here are between -1 and FdKey.Count - 1.
      Values between 0 and FdKey.Count - 1 indicate which keyValue was
      last send. Value -1 means that SendValueChanged was not yet called,
      this is the default value (after construction). }
    property CurrentKeyRange: Integer read FCurrentKeyRange;
    procedure SetCurrentKeyRange(const Value: Integer; const Time: TX3DTime;
      const PartialSend: TPartialSend);

    { Override this is descendants to actually send value_changed event
      witk keyValue[ValueIndex]. You are guaranteed that ValueIndex
      is between 0 and FdKey.Count - 1, but you have to check yourself
      whether it's also between 0 and FdKeyValue.Count - 1
      (although spec requires that key and keyValue fields have the same size,
      we have to protect against invalid files). }
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime; const PartialSend: TPartialSend); virtual; abstract;
  public
    procedure CreateNode; override;

    { Event in } { }
    strict private FEventNext: TSFBoolEvent;
    public property EventNext: TSFBoolEvent read FEventNext;

    { Event in } { }
    strict private FEventPrevious: TSFBoolEvent;
    public property EventPrevious: TSFBoolEvent read FEventPrevious;

    { Event in } { }
    strict private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    strict private FFdKey: TMFFloat;
    public property FdKey: TMFFloat read FFdKey;

    strict private FFdForceContinuousValue_Changed: TSFBool;
    public property FdForceContinuousValue_Changed: TSFBool read FFdForceContinuousValue_Changed;

    {$I auto_generated_node_helpers/x3dnodes_x3dsequencernode.inc}
  end;

  { Base type from which all trigger nodes are derived. }
  TAbstractTriggerNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dtriggernode.inc}
  end;

  { Filters boolean events, allowing for selective routing
    of TRUE or FALSE values and negation. }
  TBooleanFilterNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_boolean: TSFBoolEvent;
    public property EventSet_boolean: TSFBoolEvent read FEventSet_boolean;

    { Event out } { }
    strict private FEventInputFalse: TSFBoolEvent;
    public property EventInputFalse: TSFBoolEvent read FEventInputFalse;

    { Event out } { }
    strict private FEventInputNegate: TSFBoolEvent;
    public property EventInputNegate: TSFBoolEvent read FEventInputNegate;

    { Event out } { }
    strict private FEventInputTrue: TSFBoolEvent;
    public property EventInputTrue: TSFBoolEvent read FEventInputTrue;

    {$I auto_generated_node_helpers/x3dnodes_booleanfilter.inc}
  end;

  { Generate sequential boolean events. }
  TBooleanSequencerNode = class(TAbstractSequencerNode)
  strict protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime; const PartialSend: TPartialSend); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdKeyValue: TMFBool;
    public property FdKeyValue: TMFBool read FFdKeyValue;

    { Event out } { }
    strict private FEventValue_changed: TSFBoolEvent;
    public property EventValue_changed: TSFBoolEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_booleansequencer.inc}
  end;

  { Stores a boolean value for toggling on/off. }
  TBooleanToggleNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_boolean: TSFBoolEvent;
    public property EventSet_boolean: TSFBoolEvent read FEventSet_boolean;

    strict private FFdToggle: TSFBool;
    public property FdToggle: TSFBool read FFdToggle;

    {$I auto_generated_node_helpers/x3dnodes_booleantoggle.inc}
  end;

  { Trigger node that generates Boolean events upon receiving time events. }
  TBooleanTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_triggerTimeReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_triggerTime: TSFTimeEvent;
    public property EventSet_triggerTime: TSFTimeEvent read FEventSet_triggerTime;

    { Event out } { }
    strict private FEventTriggerTrue: TSFBoolEvent;
    public property EventTriggerTrue: TSFBoolEvent read FEventTriggerTrue;

    {$I auto_generated_node_helpers/x3dnodes_booleantrigger.inc}
  end;

  { Generate sequential integer events.
    For example, it can be driven by a TimeSensor and iterate over a series
    of nodes by animating Switch.whichChoice. }
  TIntegerSequencerNode = class(TAbstractSequencerNode)
  strict protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime; const PartialSend: TPartialSend); override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdKeyValue: TMFInt32;
    public property FdKeyValue: TMFInt32 read FFdKeyValue;

    { Event out } { }
    strict private FEventValue_changed: TSFInt32Event;
    public property EventValue_changed: TSFInt32Event read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_integersequencer.inc}
  end;

  { Generate an integer upon receiving any boolean value. }
  TIntegerTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_boolean: TSFBoolEvent;
    public property EventSet_boolean: TSFBoolEvent read FEventSet_boolean;

    strict private FFdIntegerKey: TSFInt32;
    public property FdIntegerKey: TSFInt32 read FFdIntegerKey;

    { Event out } { }
    strict private FEventTriggerValue: TSFInt32Event;
    public property EventTriggerValue: TSFInt32Event read FEventTriggerValue;

    {$I auto_generated_node_helpers/x3dnodes_integertrigger.inc}
  end;

  { Generate time events upon receiving any boolean value. }
  TTimeTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_BooleanReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventSet_boolean: TSFBoolEvent;
    public property EventSet_boolean: TSFBoolEvent read FEventSet_boolean;

    { Event out } { }
    strict private FEventTriggerTime: TSFTimeEvent;
    public property EventTriggerTime: TSFTimeEvent read FEventTriggerTime;

    {$I auto_generated_node_helpers/x3dnodes_timetrigger.inc}
  end;

  { Generate events upon receiving a boolean TRUE value. }
  TValueTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventTriggerReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_valuetrigger.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractSequencerNode.CreateNode;
begin
  inherited;

  FCurrentKeyRange := -1;

  FEventNext := TSFBoolEvent.Create(Self, 'next', true);
  AddEvent(FEventNext);
  EventNext.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventNextReceive);

  FEventPrevious := TSFBoolEvent.Create(Self, 'previous', true);
  AddEvent(FEventPrevious);
  EventPrevious.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventPreviousReceive);

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);
  EventSet_Fraction.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);

  FFdKey := TMFFloat.Create(Self, true, 'key', []);
  AddField(FFdKey);
  { X3D specification comment: (-Inf,Inf) }

  FFdForceContinuousValue_Changed := TSFBool.Create(Self, true, 'forceContinuousValue_changed', false);
  AddField(FFdForceContinuousValue_Changed);

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
  const Time: TX3DTime; const PartialSend: TPartialSend);
begin
  if (FCurrentKeyRange <> Value) or FdForceContinuousValue_Changed.Value then
  begin
    Assert(Value >= -1);
    Assert(Value < FdKey.Count);
    FCurrentKeyRange := Value;
    SendValueChanged(Value, Time, PartialSend);
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
    Assert(Value is TSFFloat);
    Assert(not IsNan((Value as TSFFloat).Value));
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
    SetCurrentKeyRange(NewKeyRange, Time, Value.InternalPartialSend);
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
    SetCurrentKeyRange(NewKeyRange, Time, Value.InternalPartialSend);
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
    SetCurrentKeyRange(NewKeyRange, Time, Value.InternalPartialSend);
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

  FEventSet_boolean := TSFBoolEvent.Create(Self, 'set_boolean', true);
  AddEvent(FEventSet_boolean);
  FEventSet_boolean.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BooleanReceive);

  FEventInputFalse := TSFBoolEvent.Create(Self, 'inputFalse', false);
  AddEvent(FEventInputFalse);

  FEventInputNegate := TSFBoolEvent.Create(Self, 'inputNegate', false);
  AddEvent(FEventInputNegate);

  FEventInputTrue := TSFBoolEvent.Create(Self, 'inputTrue', false);
  AddEvent(FEventInputTrue);

  DefaultContainerField := 'children';
end;

procedure TBooleanFilterNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    EventInputTrue.Send(B, Time, Value.InternalPartialSend)
  else
    EventInputFalse.Send(B, Time, Value.InternalPartialSend);
  EventInputNegate.Send(not B, Time, Value.InternalPartialSend);
end;

class function TBooleanFilterNode.ClassX3DType: string;
begin
  Result := 'BooleanFilter';
end;

procedure TBooleanSequencerNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFBool.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);

  FEventValue_changed := TSFBoolEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TBooleanSequencerNode.ClassX3DType: string;
begin
  Result := 'BooleanSequencer';
end;

procedure TBooleanSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime; const PartialSend: TPartialSend);
begin
  if FdKeyValue.Count <> FdKey.Count then
    WritelnWarning('VRML/X3D', 'BooleanSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time, PartialSend);
end;

procedure TBooleanToggleNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TSFBoolEvent.Create(Self, 'set_boolean', true);
  AddEvent(FEventSet_boolean);
  FEventSet_boolean.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BooleanReceive);

  FFdToggle := TSFBool.Create(Self, true, 'toggle', false);
  AddField(FFdToggle);

  DefaultContainerField := 'children';
end;

procedure TBooleanToggleNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    FdToggle.Send(not FdToggle.Value, Value.InternalPartialSend);
end;

class function TBooleanToggleNode.ClassX3DType: string;
begin
  Result := 'BooleanToggle';
end;

procedure TBooleanTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_triggerTime := TSFTimeEvent.Create(Self, 'set_triggerTime', true);
  AddEvent(FEventSet_triggerTime);
  FEventSet_triggerTime.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_triggerTimeReceive);

  FEventTriggerTrue := TSFBoolEvent.Create(Self, 'triggerTrue', false);
  AddEvent(FEventTriggerTrue);

  DefaultContainerField := 'children';
end;

procedure TBooleanTriggerNode.EventSet_triggerTimeReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTrue.Send(true, Time, Value.InternalPartialSend);
end;

class function TBooleanTriggerNode.ClassX3DType: string;
begin
  Result := 'BooleanTrigger';
end;

procedure TIntegerSequencerNode.CreateNode;
begin
  inherited;

  FFdKeyValue := TMFInt32.Create(Self, true, 'keyValue', []);
  AddField(FFdKeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFInt32Event.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';
end;

class function TIntegerSequencerNode.ClassX3DType: string;
begin
  Result := 'IntegerSequencer';
end;

procedure TIntegerSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime; const PartialSend: TPartialSend);
begin
  if FdKeyValue.Count <> FdKey.Count then
    WritelnWarning('VRML/X3D', 'IntegerSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time, PartialSend);
end;

procedure TIntegerTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TSFBoolEvent.Create(Self, 'set_boolean', true);
  AddEvent(FEventSet_boolean);
  FEventSet_boolean.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_booleanReceive);

  { The default is not given in X3D 3.2 spec, but X3D 3.0 and 3.3 specs specify default as -1.
    See
    https://www.web3d.org/documents/specifications/19775-1/V3.0/Part01/components/utils.html#IntegerTrigger
    https://www.web3d.org/documents/specifications/19775-1/V3.2/Part01/components/utils.html#IntegerTrigger
    https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/utils.html#IntegerTrigger
  }
  FFdIntegerKey := TSFInt32.Create(Self, true, 'integerKey', -1);
  AddField(FFdIntegerKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventTriggerValue := TSFInt32Event.Create(Self, 'triggerValue', false);
  AddEvent(FEventTriggerValue);

  DefaultContainerField := 'children';
end;

procedure TIntegerTriggerNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventtriggerValue.Send(FdintegerKey.Value, Time, Value.InternalPartialSend);
end;

class function TIntegerTriggerNode.ClassX3DType: string;
begin
  Result := 'IntegerTrigger';
end;

procedure TTimeTriggerNode.CreateNode;
begin
  inherited;

  FEventSet_boolean := TSFBoolEvent.Create(Self, 'set_boolean', true);
  AddEvent(FEventSet_boolean);
  FEventSet_boolean.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_BooleanReceive);

  FEventTriggerTime := TSFTimeEvent.Create(Self, 'triggerTime', false);
  AddEvent(FEventTriggerTime);

  DefaultContainerField := 'children';
end;

class function TTimeTriggerNode.ClassX3DType: string;
begin
  Result := 'TimeTrigger';
end;

procedure TTimeTriggerNode.EventSet_BooleanReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTime.Send(Time.Seconds, Time, Value.InternalPartialSend);
end;

{ TValueTriggerNode ---------------------------------------------------------- }

constructor TValueTriggerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventTrigger.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventTriggerReceive);
  HasInterfaceDeclarations := AllAccessTypes;
end;

procedure TValueTriggerNode.EventTriggerReceive(Event: TX3DEvent; Value: TX3DField;
  const Time: TX3DTime);
var
  B: Boolean;
  I: Integer;
  F: TX3DField;
begin
  B := (Value as TSFBool).Value;
  if B and Enabled then
  begin
    for I := 0 to FieldsCount - 1 do
    begin
      F := Fields[I];
      { For "dynamic" (coming from "interface declaration") and exposed fields,
        send an event with their current value. }
      if (F.ParentInterfaceDeclaration <> nil) and
         (F.EventOut <> nil) then
      begin
        // since we send F, it must have Value.InternalPartialSend set, to account for cross-fading
        F.InternalPartialSend := Value.InternalPartialSend;
        F.EventOut.Send(F, Time);
        F.InternalPartialSend := nil;
      end;
    end;
  end;
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
    TTimeTriggerNode,
    TValueTriggerNode
  ]);
end;

{$endif read_implementation}
