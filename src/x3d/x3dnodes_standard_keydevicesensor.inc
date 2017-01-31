{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base type for all sensor node types that operate using key devices. }
  TAbstractKeyDeviceSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;

    { React to key down/up events.

      This is used by TCastleSceneCore to communicate with this key sensor.

      Semantics follow TInputPressRelease behavior for EventType = itKey.
      This means that Key may be K_None (if not
      representable as TKey) and C may be #0 (if not representable as char),
      but not both --- at least one of Key or C must contains something useful.
      Also, key presses are affected
      by the "key repeat" feature of the OS (windowing system actually), so when
      user holds down a key --- we get many key down messages.

      @groupBegin }
    procedure KeyDown(Key: TKey; C: char; const Time: TX3DTime); virtual;
    procedure KeyUp(Key: TKey; C: char; const Time: TX3DTime); virtual;
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_x3dkeydevicesensornode.inc}
  end;

  { Generate events when user key presses or releases keys on the keyboard. }
  TKeySensorNode = class(TAbstractKeyDeviceSensorNode)
  private
    AltKey, ControlKey, ShiftKey: boolean;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event out } { }
    private FEventActionKeyPress: TSFInt32Event;
    public property EventActionKeyPress: TSFInt32Event read FEventActionKeyPress;

    { Event out } { }
    private FEventActionKeyRelease: TSFInt32Event;
    public property EventActionKeyRelease: TSFInt32Event read FEventActionKeyRelease;

    { Event out } { }
    private FEventAltKey: TSFBoolEvent;
    public property EventAltKey: TSFBoolEvent read FEventAltKey;

    { Event out } { }
    private FEventControlKey: TSFBoolEvent;
    public property EventControlKey: TSFBoolEvent read FEventControlKey;

    { Event out } { }
    private FEventKeyPress: TSFStringEvent;
    public property EventKeyPress: TSFStringEvent read FEventKeyPress;

    { Event out } { }
    private FEventKeyRelease: TSFStringEvent;
    public property EventKeyRelease: TSFStringEvent read FEventKeyRelease;

    { Event out } { }
    private FEventShiftKey: TSFBoolEvent;
    public property EventShiftKey: TSFBoolEvent read FEventShiftKey;

    procedure KeyDown(Key: TKey; C: char; const Time: TX3DTime); override;
    procedure KeyUp(Key: TKey; C: char; const Time: TX3DTime); override;

    {$I auto_generated_node_helpers/x3dnodes_keysensor.inc}
  end;

  { Generates events as the user inputs a longer string on the keyboard,
    useful for example to simulate "input fields" in 3D. }
  TStringSensorNode = class(TAbstractKeyDeviceSensorNode)
  private
    EnteredText: string;
    Active: boolean;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDeletionAllowed: TSFBool;
    public property FdDeletionAllowed: TSFBool read FFdDeletionAllowed;

    { Event out } { }
    private FEventEnteredText: TSFStringEvent;
    public property EventEnteredText: TSFStringEvent read FEventEnteredText;

    { Event out } { }
    private FEventFinalText: TSFStringEvent;
    public property EventFinalText: TSFStringEvent read FEventFinalText;

    procedure KeyDown(Key: TKey; C: char; const Time: TX3DTime); override;

    {$I auto_generated_node_helpers/x3dnodes_stringsensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractKeyDeviceSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TAbstractKeyDeviceSensorNode.KeyDown(Key: TKey; C: char; const Time: TX3DTime);
begin
  { Nothing to do in this class. }
end;

procedure TAbstractKeyDeviceSensorNode.KeyUp(Key: TKey; C: char; const Time: TX3DTime);
begin
  { Nothing to do in this class. }
end;

{ Convert TKey to VRML "action" key code.
  As defined by X3D KeySensor node specification. }
function KeyToActionKey(Key: TKey; out ActionKey: Integer): boolean;
begin
  Result := true;
  case Key of
    K_F1 .. K_F12 : ActionKey := Ord(Key) - Ord(K_F1) + 1;
    K_Home     : ActionKey := 13;
    K_End      : ActionKey := 14;
    K_PageUp   : ActionKey := 15;
    K_PageDown : ActionKey := 16;
    K_Up       : ActionKey := 17;
    K_Down     : ActionKey := 18;
    K_Left     : ActionKey := 19;
    K_Right    : ActionKey := 20;
    else Result := false;
  end;
end;

procedure TKeySensorNode.CreateNode;
begin
  inherited;

  FEventActionKeyPress := TSFInt32Event.Create(Self, 'actionKeyPress', false);
  AddEvent(FEventActionKeyPress);

  FEventActionKeyRelease := TSFInt32Event.Create(Self, 'actionKeyRelease', false);
  AddEvent(FEventActionKeyRelease);

  FEventAltKey := TSFBoolEvent.Create(Self, 'altKey', false);
  AddEvent(FEventAltKey);

  FEventControlKey := TSFBoolEvent.Create(Self, 'controlKey', false);
  AddEvent(FEventControlKey);

  FEventKeyPress := TSFStringEvent.Create(Self, 'keyPress', false);
  AddEvent(FEventKeyPress);

  FEventKeyRelease := TSFStringEvent.Create(Self, 'keyRelease', false);
  AddEvent(FEventKeyRelease);

  FEventShiftKey := TSFBoolEvent.Create(Self, 'shiftKey', false);
  AddEvent(FEventShiftKey);

  DefaultContainerField := 'children';
end;

class function TKeySensorNode.ClassX3DType: string;
begin
  Result := 'KeySensor';
end;

class function TKeySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TKeySensorNode.KeyDown(Key: TKey; C: char; const Time: TX3DTime);

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TSFBoolEvent);
  begin
    if not SpecialPressed then
    begin
      SpecialEvent.Send(true, Time);
      EventIsActive.Send(true, Time);
      SpecialPressed := true;
    end;
  end;

var
  ActionKey: Integer;
begin
  inherited;
  if FdEnabled.Value then
  begin
    { For now, actionKeyPress and keyPress are simply always accompanied by isActive := true.
      We ignore the trouble with key repeat, as spec doesn't say what to do
      (suggesting that we should make event only on the actual press, but this
      would be poor --- key repeat is useful for users).

      We track ctrl / alt / shift state and avoid key repeat for them.
      Reason: 1. for them, key repeat seems really not useful.
      2. on Unix, windowing system (or at least GTK) doesn't even do key repeat
      for them. So this makes Windows and Unix behave more commonly. }
    if KeyToActionKey(Key, ActionKey) then
    begin
      EventActionKeyPress.Send(ActionKey, Time);
      EventIsActive.Send(true, Time);
    end;
    if C <> #0 then
    begin
      EventKeyPress.Send(C, Time);
      EventIsActive.Send(true, Time);
    end;
    case Key of
      K_Alt  : HandleSpecial(AltKey, EventAltKey);
      K_Ctrl : HandleSpecial(ControlKey, EventControlKey);
      K_Shift: HandleSpecial(ShiftKey, EventShiftKey);
    end;
  end;
end;

procedure TKeySensorNode.KeyUp(Key: TKey; C: char; const Time: TX3DTime);

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TSFBoolEvent);
  begin
    if SpecialPressed then
    begin
      SpecialEvent.Send(false, Time);
      EventIsActive.Send(false, Time);
      SpecialPressed := false;
    end;
  end;

var
  ActionKey: Integer;
begin
  if FdEnabled.Value then
  begin
    if KeyToActionKey(Key, ActionKey) then
    begin
      EventActionKeyRelease.Send(ActionKey, Time);
      EventIsActive.Send(false, Time);
    end;
    if C <> #0 then
    begin
      EventKeyRelease.Send(C, Time);
      EventIsActive.Send(false, Time);
    end;
    case Key of
      K_Alt  : HandleSpecial(AltKey, EventAltKey);
      K_Ctrl : HandleSpecial(ControlKey, EventControlKey);
      K_Shift: HandleSpecial(ShiftKey, EventShiftKey);
    end;
  end;
  inherited;
end;

procedure TStringSensorNode.CreateNode;
begin
  inherited;

  FFdDeletionAllowed := TSFBool.Create(Self, 'deletionAllowed', true);
  AddField(FFdDeletionAllowed);

  FEventEnteredText := TSFStringEvent.Create(Self, 'enteredText', false);
  AddEvent(FEventEnteredText);

  FEventFinalText := TSFStringEvent.Create(Self, 'finalText', false);
  AddEvent(FEventFinalText);

  DefaultContainerField := 'children';
end;

class function TStringSensorNode.ClassX3DType: string;
begin
  Result := 'StringSensor';
end;

class function TStringSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TStringSensorNode.KeyDown(Key: TKey; C: char; const Time: TX3DTime);
begin
  inherited;

  if FdEnabled.Value and ( (C <> #0) or (Key in [K_BackSpace, K_Enter]) ) then
  begin
    if not Active then
    begin
      Active := true;
      EventIsActive.Send(Active, Time);
    end;

    { Catch both CharBackspace and K_BackSpace.
      Catching CharBackspace is not enough, because it's also equal to Ctrl+H.
      Same for CharEnter: it's also equal to Ctrl+M.
      And Ctrl+... may be often used by some menu items, see e.g. view3dscene. }

    if (C = CharBackspace) or (Key = K_BackSpace) then
    begin
      if FdDeletionAllowed.Value and (EnteredText <> '') then
      begin
        SetLength(EnteredText, Length(EnteredText) - 1);
        EventEnteredText.Send(EnteredText, Time);
      end;
    end else
    if (C = CharEnter) or (Key = K_Enter) then
    begin
      EventFinalText.Send(EnteredText, Time);
      { X3D spec says explicitly that in this case, enteredText event
        is not generated although internal value is empty. }
      EnteredText := '';
      Active := false;
      EventIsActive.Send(Active, Time);
    end else
    begin
      EnteredText := EnteredText + C;
      EventEnteredText.Send(EnteredText, Time);
    end;
  end;
end;

procedure RegisterKeyDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TKeySensorNode,
    TStringSensorNode
  ]);
end;

{$endif read_implementation}
