{%MainUnit x3dnodes.pas}
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
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
    procedure EventNextReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
    procedure EventPreviousReceive(const Event: TX3DEvent; const Value: TX3DField;
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
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    {$I auto_generated_node_helpers/x3dnodes_x3dsequencernode.inc}
  end;

  { Base type from which all trigger nodes are derived. }
  TAbstractTriggerNode = class(TAbstractChildNode)
  public
    {$I auto_generated_node_helpers/x3dnodes_x3dtriggernode.inc}
  end;

  { Filters boolean events, allowing for selective routing
    of TRUE or FALSE values and negation. }
  TBooleanFilterNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_BooleanReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_booleanfilter.inc}
  end;

  { Generate sequential boolean events. }
  TBooleanSequencerNode = class(TAbstractSequencerNode)
  strict protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TX3DTime; const PartialSend: TPartialSend); override;
  public
    {$I auto_generated_node_helpers/x3dnodes_booleansequencer.inc}
  end;

  { Stores a boolean value for toggling on/off. }
  TBooleanToggleNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_BooleanReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_booleantoggle.inc}
  end;

  { Trigger node that generates Boolean events upon receiving time events. }
  TBooleanTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_triggerTimeReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
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
    {$I auto_generated_node_helpers/x3dnodes_integersequencer.inc}
  end;

  { Generate an integer upon receiving any boolean value. }
  TIntegerTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_BooleanReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_integertrigger.inc}
  end;

  { Generate time events upon receiving any boolean value. }
  TTimeTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventSet_BooleanReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_timetrigger.inc}
  end;

  { Generate events upon receiving a boolean TRUE value. }
  TValueTriggerNode = class(TAbstractTriggerNode)
  strict private
    procedure EventTriggerReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    {$I auto_generated_node_helpers/x3dnodes_valuetrigger.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractSequencerNode ----------------------------------------------------- }

constructor TAbstractSequencerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;

  FCurrentKeyRange := -1;

  EventNext.AddNotification({$ifdef FPC}@{$endif} EventNextReceive);
  EventPrevious.AddNotification({$ifdef FPC}@{$endif} EventPreviousReceive);
  EventSet_Fraction.AddNotification({$ifdef FPC}@{$endif} EventSet_FractionReceive);
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

procedure TAbstractSequencerNode.EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
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

procedure TAbstractSequencerNode.EventNextReceive(const Event: TX3DEvent; const Value: TX3DField;
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

procedure TAbstractSequencerNode.EventPreviousReceive(const Event: TX3DEvent; const Value: TX3DField;
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

{ TBooleanFilterNode --------------------------------------------------------- }

constructor TBooleanFilterNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventSet_boolean.AddNotification({$ifdef FPC}@{$endif} EventSet_BooleanReceive);
end;

procedure TBooleanFilterNode.EventSet_BooleanReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
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

{ TBooleanSequencerNode ------------------------------------------------------ }

procedure TBooleanSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime; const PartialSend: TPartialSend);
begin
  if FdKeyValue.Count <> FdKey.Count then
    WritelnWarning('VRML/X3D', 'BooleanSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time, PartialSend);
end;

{ TBooleanToggleNode --------------------------------------------------------- }

constructor TBooleanToggleNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventSet_boolean.AddNotification({$ifdef FPC}@{$endif} EventSet_BooleanReceive);
end;

procedure TBooleanToggleNode.EventSet_BooleanReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    FdToggle.Send(not FdToggle.Value, Value.InternalPartialSend);
end;

{ TBooleanTriggerNode -------------------------------------------------------- }

constructor TBooleanTriggerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventSet_triggerTime.AddNotification({$ifdef FPC}@{$endif} EventSet_triggerTimeReceive);
end;

procedure TBooleanTriggerNode.EventSet_triggerTimeReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTrue.Send(true, Time, Value.InternalPartialSend);
end;

{ TIntegerSequencerNode ------------------------------------------------------ }

procedure TIntegerSequencerNode.SendValueChanged(const ValueIndex: Integer;
  const Time: TX3DTime; const PartialSend: TPartialSend);
begin
  if FdKeyValue.Count <> FdKey.Count then
    WritelnWarning('VRML/X3D', 'IntegerSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time, PartialSend);
end;

{ TIntegerTriggerNode -------------------------------------------------------- }

constructor TIntegerTriggerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventSet_boolean.AddNotification({$ifdef FPC}@{$endif} EventSet_booleanReceive);
end;

procedure TIntegerTriggerNode.EventSet_BooleanReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  EventtriggerValue.Send(FdintegerKey.Value, Time, Value.InternalPartialSend);
end;

{ TTimeTriggerNode ----------------------------------------------------------- }

constructor TTimeTriggerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventSet_boolean.AddNotification({$ifdef FPC}@{$endif} EventSet_BooleanReceive);
end;

procedure TTimeTriggerNode.EventSet_BooleanReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  EventTriggerTime.Send(Time.Seconds, Time, Value.InternalPartialSend);
end;

{ TValueTriggerNode ---------------------------------------------------------- }

constructor TValueTriggerNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FEventTrigger.AddNotification({$ifdef FPC}@{$endif} EventTriggerReceive);
  HasInterfaceDeclarations := AllAccessTypes;
end;

procedure TValueTriggerNode.EventTriggerReceive(const Event: TX3DEvent; const Value: TX3DField;
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
