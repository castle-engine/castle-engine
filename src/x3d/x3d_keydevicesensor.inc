{
  Copyright 2008-2012 Michalis Kamburelis.

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
  end;

  TKeySensorNode = class(TAbstractKeyDeviceSensorNode)
  private
    AltKey, ControlKey, ShiftKey: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, out } { }
    private FEventActionKeyPress: TX3DEvent;
    public property EventActionKeyPress: TX3DEvent read FEventActionKeyPress;

    { Event: SFInt32, out } { }
    private FEventActionKeyRelease: TX3DEvent;
    public property EventActionKeyRelease: TX3DEvent read FEventActionKeyRelease;

    { Event: SFBool, out } { }
    private FEventAltKey: TX3DEvent;
    public property EventAltKey: TX3DEvent read FEventAltKey;

    { Event: SFBool, out } { }
    private FEventControlKey: TX3DEvent;
    public property EventControlKey: TX3DEvent read FEventControlKey;

    { Event: SFString, out } { }
    private FEventKeyPress: TX3DEvent;
    public property EventKeyPress: TX3DEvent read FEventKeyPress;

    { Event: SFString, out } { }
    private FEventKeyRelease: TX3DEvent;
    public property EventKeyRelease: TX3DEvent read FEventKeyRelease;

    { Event: SFBool, out } { }
    private FEventShiftKey: TX3DEvent;
    public property EventShiftKey: TX3DEvent read FEventShiftKey;

    procedure KeyDown(Key: TKey; C: char; const Time: TX3DTime); override;
    procedure KeyUp(Key: TKey; C: char; const Time: TX3DTime); override;
  end;

  TStringSensorNode = class(TAbstractKeyDeviceSensorNode)
  private
    EnteredText: string;
    Active: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDeletionAllowed: TSFBool;
    public property FdDeletionAllowed: TSFBool read FFdDeletionAllowed;

    { Event: SFString, out } { }
    private FEventEnteredText: TX3DEvent;
    public property EventEnteredText: TX3DEvent read FEventEnteredText;

    { Event: SFString, out } { }
    private FEventFinalText: TX3DEvent;
    public property EventFinalText: TX3DEvent read FEventFinalText;

    procedure KeyDown(Key: TKey; C: char; const Time: TX3DTime); override;
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

  FEventActionKeyPress := TX3DEvent.Create(Self, 'actionKeyPress', TSFInt32, false);
  Events.Add(FEventActionKeyPress);

  FEventActionKeyRelease := TX3DEvent.Create(Self, 'actionKeyRelease', TSFInt32, false);
  Events.Add(FEventActionKeyRelease);

  FEventAltKey := TX3DEvent.Create(Self, 'altKey', TSFBool, false);
  Events.Add(FEventAltKey);

  FEventControlKey := TX3DEvent.Create(Self, 'controlKey', TSFBool, false);
  Events.Add(FEventControlKey);

  FEventKeyPress := TX3DEvent.Create(Self, 'keyPress', TSFString, false);
  Events.Add(FEventKeyPress);

  FEventKeyRelease := TX3DEvent.Create(Self, 'keyRelease', TSFString, false);
  Events.Add(FEventKeyRelease);

  FEventShiftKey := TX3DEvent.Create(Self, 'shiftKey', TSFBool, false);
  Events.Add(FEventShiftKey);

  DefaultContainerField := 'children';
end;

class function TKeySensorNode.ClassNodeTypeName: string;
begin
  Result := 'KeySensor';
end;

class function TKeySensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TKeySensorNode.KeyDown(Key: TKey; C: char; const Time: TX3DTime);

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TX3DEvent);
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

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TX3DEvent);
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
  Fields.Add(FFdDeletionAllowed);

  FEventEnteredText := TX3DEvent.Create(Self, 'enteredText', TSFString, false);
  Events.Add(FEventEnteredText);

  FEventFinalText := TX3DEvent.Create(Self, 'finalText', TSFString, false);
  Events.Add(FEventFinalText);

  DefaultContainerField := 'children';
end;

class function TStringSensorNode.ClassNodeTypeName: string;
begin
  Result := 'StringSensor';
end;

class function TStringSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
