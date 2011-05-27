{
  Copyright 2008-2011 Michalis Kamburelis.

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
  TNodeX3DSequencerNode = class(TNodeX3DChildNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
    procedure EventNextReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
    procedure EventPreviousReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  private
    FCurrentKeyRange: Integer;
  protected
    { Analogous to TNodeX3DInterpolatorNode.KeyRange. }
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
    procedure SetCurrentKeyRange(const Value: Integer; const Time: TVRMLTime);

    { Override this is descendants to actually send value_changed event
      witk keyValue[ValueIndex]. You are guaranteed that ValueIndex
      is between 0 and FdKey.Count - 1, but you have to check yourself
      whether it's also between 0 and FdKeyValue.Count - 1
      (although spec requires that key and keyValue fields have the same size,
      we have to protect against invalid files). }
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TVRMLTime); virtual; abstract;
  public
    procedure CreateNode; override;

    { Event: SFBool, in } { }
    private FEventnext: TVRMLEvent;
    public property Eventnext: TVRMLEvent read FEventnext;

    { Event: SFBool, in } { }
    private FEventprevious: TVRMLEvent;
    public property Eventprevious: TVRMLEvent read FEventprevious;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;
  end;

  TNodeX3DTriggerNode = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
  end;

  TNodeBooleanFilter = class(TNodeX3DChildNode)
  private
    procedure EventSet_BooleanReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    { Event: SFBool, out } { }
    private FEventinputFalse: TVRMLEvent;
    public property EventinputFalse: TVRMLEvent read FEventinputFalse;

    { Event: SFBool, out } { }
    private FEventinputNegate: TVRMLEvent;
    public property EventinputNegate: TVRMLEvent read FEventinputNegate;

    { Event: SFBool, out } { }
    private FEventinputTrue: TVRMLEvent;
    public property EventinputTrue: TVRMLEvent read FEventinputTrue;
  end;

  TNodeBooleanSequencer = class(TNodeX3DSequencerNode)
  protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TVRMLTime); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFBool;
    public property FdkeyValue: TMFBool read FFdkeyValue;

    { Event: SFBool, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeBooleanToggle = class(TNodeX3DChildNode)
  private
    procedure EventSet_BooleanReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    private FFdtoggle: TSFBool;
    public property Fdtoggle: TSFBool read FFdtoggle;
  end;

  TNodeBooleanTrigger = class(TNodeX3DTriggerNode)
  private
    procedure EventSet_triggerTimeReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFTime, in } { }
    private FEventset_triggerTime: TVRMLEvent;
    public property Eventset_triggerTime: TVRMLEvent read FEventset_triggerTime;

    { Event: SFBool, out } { }
    private FEventtriggerTrue: TVRMLEvent;
    public property EventtriggerTrue: TVRMLEvent read FEventtriggerTrue;
  end;

  TNodeIntegerSequencer = class(TNodeX3DSequencerNode)
  protected
    procedure SendValueChanged(const ValueIndex: Integer;
      const Time: TVRMLTime); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFInt32;
    public property FdkeyValue: TMFInt32 read FFdkeyValue;

    { Event: SFInt32, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeIntegerTrigger = class(TNodeX3DTriggerNode)
  private
    procedure EventSet_BooleanReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    private FFdintegerKey: TSFInt32;
    public property FdintegerKey: TSFInt32 read FFdintegerKey;

    { Event: SFInt32, out } { }
    private FEventtriggerValue: TVRMLEvent;
    public property EventtriggerValue: TVRMLEvent read FEventtriggerValue;
  end;

  TNodeTimeTrigger = class(TNodeX3DTriggerNode)
  private
    procedure EventSet_BooleanReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    { Event: SFTime, out } { }
    private FEventtriggerTime: TVRMLEvent;
    public property EventtriggerTime: TVRMLEvent read FEventtriggerTime;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DSequencerNode.CreateNode;
begin
  inherited;

  FCurrentKeyRange := -1;

  FEventnext := TVRMLEvent.Create(Self, 'next', TSFBool, true);
  Events.Add(FEventnext);
  EventNext.OnReceive.Add(@EventNextReceive);

  FEventprevious := TVRMLEvent.Create(Self, 'previous', TSFBool, true);
  Events.Add(FEventprevious);
  EventPrevious.OnReceive.Add(@EventPreviousReceive);

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);
  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);

  FFdkey := TMFFloat.Create(Self, 'key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TNodeX3DSequencerNode.KeyRange(
  const Fraction: Single): Integer;
var
  T: Single;
begin
  { T is simply ignored, as it's useless for sequences (since they generate
    discrete events from float fraction, then they are not interested
    in T value). }

  Result := VRMLNodes.KeyRange(FdKey.Items, Fraction, T);
end;

procedure TNodeX3DSequencerNode.SetCurrentKeyRange(const Value: Integer;
  const Time: TVRMLTime);
begin
  if FCurrentKeyRange <> Value then
  begin
    Assert(Value >= -1);
    Assert(Value < FdKey.Count);
    FCurrentKeyRange := Value;
    SendValueChanged(Value, Time);
  end;
end;

procedure TNodeX3DSequencerNode.EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
  const Time: TVRMLTime);
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

procedure TNodeX3DSequencerNode.EventNextReceive(Event: TVRMLEvent; Value: TVRMLField;
  const Time: TVRMLTime);
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

procedure TNodeX3DSequencerNode.EventPreviousReceive(Event: TVRMLEvent; Value: TVRMLField;
  const Time: TVRMLTime);
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

procedure TNodeX3DTriggerNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TNodeBooleanFilter.CreateNode;
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);
  FEventset_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FEventinputFalse := TVRMLEvent.Create(Self, 'inputFalse', TSFBool, false);
  Events.Add(FEventinputFalse);

  FEventinputNegate := TVRMLEvent.Create(Self, 'inputNegate', TSFBool, false);
  Events.Add(FEventinputNegate);

  FEventinputTrue := TVRMLEvent.Create(Self, 'inputTrue', TSFBool, false);
  Events.Add(FEventinputTrue);

  DefaultContainerField := 'children';
end;

procedure TNodeBooleanFilter.EventSet_BooleanReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    EventInputTrue.Send(B, Time) else
    EventInputFalse.Send(B, Time);
  EventInputNegate.Send(not B, Time);
end;

class function TNodeBooleanFilter.ClassNodeTypeName: string;
begin
  Result := 'BooleanFilter';
end;

function TNodeBooleanFilter.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeBooleanFilter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeBooleanSequencer.CreateNode;
begin
  inherited;

  FFdkeyValue := TMFBool.Create(Self, 'keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFBool, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeBooleanSequencer.ClassNodeTypeName: string;
begin
  Result := 'BooleanSequencer';
end;

function TNodeBooleanSequencer.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeBooleanSequencer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeBooleanSequencer.SendValueChanged(const ValueIndex: Integer;
  const Time: TVRMLTime);
begin
  if FdKeyValue.Count <> FdKey.Count then
    VRMLWarning(vwSerious, 'BooleanSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time);
end;

procedure TNodeBooleanToggle.CreateNode;
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);
  FEventset_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FFdtoggle := TSFBool.Create(Self, 'toggle', false);
  Fields.Add(FFdtoggle);

  DefaultContainerField := 'children';
end;

procedure TNodeBooleanToggle.EventSet_BooleanReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  B: boolean;
begin
  B := (Value as TSFBool).Value;
  if B then
    FdToggle.ExposedEvents[true].Send(not FdToggle.Value, Time);
end;

class function TNodeBooleanToggle.ClassNodeTypeName: string;
begin
  Result := 'BooleanToggle';
end;

function TNodeBooleanToggle.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeBooleanToggle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeBooleanTrigger.CreateNode;
begin
  inherited;

  FEventset_triggerTime := TVRMLEvent.Create(Self, 'set_triggerTime', TSFTime, true);
  Events.Add(FEventset_triggerTime);
  FEventset_triggerTime.OnReceive.Add(@EventSet_triggerTimeReceive);

  FEventtriggerTrue := TVRMLEvent.Create(Self, 'triggerTrue', TSFBool, false);
  Events.Add(FEventtriggerTrue);

  DefaultContainerField := 'children';
end;

procedure TNodeBooleanTrigger.EventSet_triggerTimeReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  EventTriggerTrue.Send(true, Time);
end;

class function TNodeBooleanTrigger.ClassNodeTypeName: string;
begin
  Result := 'BooleanTrigger';
end;

function TNodeBooleanTrigger.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeBooleanTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeIntegerSequencer.CreateNode;
begin
  inherited;

  FFdkeyValue := TMFInt32.Create(Self, 'keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFInt32, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeIntegerSequencer.ClassNodeTypeName: string;
begin
  Result := 'IntegerSequencer';
end;

function TNodeIntegerSequencer.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeIntegerSequencer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeIntegerSequencer.SendValueChanged(const ValueIndex: Integer;
  const Time: TVRMLTime);
begin
  if FdKeyValue.Count <> FdKey.Count then
    VRMLWarning(vwSerious, 'IntegerSequencer node: key and keyValue fields should have the same size');
  if ValueIndex < FdKeyValue.Count then
    EventValue_Changed.Send(FdKeyValue.Items.Items[ValueIndex], Time);
end;

procedure TNodeIntegerTrigger.CreateNode;
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);
  FEventset_boolean.OnReceive.Add(@EventSet_booleanReceive);

  { X3D specification mistakenly omitted default value for this field...
    I guess it's 0? }
  FFdintegerKey := TSFInt32.Create(Self, 'integerKey', 0);
  Fields.Add(FFdintegerKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventtriggerValue := TVRMLEvent.Create(Self, 'triggerValue', TSFInt32, false);
  Events.Add(FEventtriggerValue);

  DefaultContainerField := 'children';
end;

procedure TNodeIntegerTrigger.EventSet_BooleanReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  EventtriggerValue.Send(FdintegerKey.Value, Time);
end;

class function TNodeIntegerTrigger.ClassNodeTypeName: string;
begin
  Result := 'IntegerTrigger';
end;

function TNodeIntegerTrigger.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeIntegerTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTimeTrigger.CreateNode;
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create(Self, 'set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);
  FEventset_boolean.OnReceive.Add(@EventSet_BooleanReceive);

  FEventtriggerTime := TVRMLEvent.Create(Self, 'triggerTime', TSFTime, false);
  Events.Add(FEventtriggerTime);

  DefaultContainerField := 'children';
end;

class function TNodeTimeTrigger.ClassNodeTypeName: string;
begin
  Result := 'TimeTrigger';
end;

function TNodeTimeTrigger.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeTimeTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTimeTrigger.EventSet_BooleanReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  EventTriggerTime.Send(Time.Seconds, Time);
end;

procedure RegisterEventUtilitiesNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBooleanFilter,
    TNodeBooleanSequencer,
    TNodeBooleanToggle,
    TNodeBooleanTrigger,
    TNodeIntegerSequencer,
    TNodeIntegerTrigger,
    TNodeTimeTrigger
  ]);
end;

{$endif read_implementation}
