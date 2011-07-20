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
  TNodeX3DKeyDeviceSensorNode = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;

    { React to key down/up events.

      This is used by TVRMLScene to communicate with this key sensor.

      Semantics follow TUIControl.KeyDown,
      TUIControl.KeyUp behavior which in turn follow TGLWindow.OnKeyDown,
      TGLWindow.OnKeyUp behavior. This means that Key may be K_None (if not
      representable as TKey) and C may be #0 (if not representable as char),
      although not both at the same time (that is, at least one of Key or C
      parameters contains some useful value). Also, key presses are affected
      by the "key repeat" feature of the OS (windowing system actually), so when
      user holds down a key --- we get many key down messages.

      @groupBegin }
    procedure KeyDown(Key: TKey; C: char; const Time: TVRMLTime); virtual;
    procedure KeyUp(Key: TKey; C: char; const Time: TVRMLTime); virtual;
    { @groupEnd }
  end;

  TNodeKeySensor = class(TNodeX3DKeyDeviceSensorNode)
  private
    AltKey, ControlKey, ShiftKey: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, out } { }
    private FEventactionKeyPress: TVRMLEvent;
    public property EventactionKeyPress: TVRMLEvent read FEventactionKeyPress;

    { Event: SFInt32, out } { }
    private FEventactionKeyRelease: TVRMLEvent;
    public property EventactionKeyRelease: TVRMLEvent read FEventactionKeyRelease;

    { Event: SFBool, out } { }
    private FEventaltKey: TVRMLEvent;
    public property EventaltKey: TVRMLEvent read FEventaltKey;

    { Event: SFBool, out } { }
    private FEventcontrolKey: TVRMLEvent;
    public property EventcontrolKey: TVRMLEvent read FEventcontrolKey;

    { Event: SFString, out } { }
    private FEventkeyPress: TVRMLEvent;
    public property EventkeyPress: TVRMLEvent read FEventkeyPress;

    { Event: SFString, out } { }
    private FEventkeyRelease: TVRMLEvent;
    public property EventkeyRelease: TVRMLEvent read FEventkeyRelease;

    { Event: SFBool, out } { }
    private FEventshiftKey: TVRMLEvent;
    public property EventshiftKey: TVRMLEvent read FEventshiftKey;

    procedure KeyDown(Key: TKey; C: char; const Time: TVRMLTime); override;
    procedure KeyUp(Key: TKey; C: char; const Time: TVRMLTime); override;
  end;

  TNodeStringSensor = class(TNodeX3DKeyDeviceSensorNode)
  private
    EnteredText: string;
    Active: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddeletionAllowed: TSFBool;
    public property FddeletionAllowed: TSFBool read FFddeletionAllowed;

    { Event: SFString, out } { }
    private FEvententeredText: TVRMLEvent;
    public property EvententeredText: TVRMLEvent read FEvententeredText;

    { Event: SFString, out } { }
    private FEventfinalText: TVRMLEvent;
    public property EventfinalText: TVRMLEvent read FEventfinalText;

    procedure KeyDown(Key: TKey; C: char; const Time: TVRMLTime); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DKeyDeviceSensorNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TNodeX3DKeyDeviceSensorNode.KeyDown(Key: TKey; C: char; const Time: TVRMLTime);
begin
  { Nothing to do in this class. }
end;

procedure TNodeX3DKeyDeviceSensorNode.KeyUp(Key: TKey; C: char; const Time: TVRMLTime);
begin
  { Nothing to do in this class. }
end;

{ Convert TKey to VRML "action" key code.
  As defined by X3D KeySensor node specification. }
function KeyToActionKey(Key: TKey; out ActionKey: Integer): boolean;
begin
  Result := true;
  case Key of
    K_F1 .. K_F12 : ActionKey := Key - K_F1 + 1;
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

procedure TNodeKeySensor.CreateNode;
begin
  inherited;

  FEventactionKeyPress := TVRMLEvent.Create(Self, 'actionKeyPress', TSFInt32, false);
  Events.Add(FEventactionKeyPress);

  FEventactionKeyRelease := TVRMLEvent.Create(Self, 'actionKeyRelease', TSFInt32, false);
  Events.Add(FEventactionKeyRelease);

  FEventaltKey := TVRMLEvent.Create(Self, 'altKey', TSFBool, false);
  Events.Add(FEventaltKey);

  FEventcontrolKey := TVRMLEvent.Create(Self, 'controlKey', TSFBool, false);
  Events.Add(FEventcontrolKey);

  FEventkeyPress := TVRMLEvent.Create(Self, 'keyPress', TSFString, false);
  Events.Add(FEventkeyPress);

  FEventkeyRelease := TVRMLEvent.Create(Self, 'keyRelease', TSFString, false);
  Events.Add(FEventkeyRelease);

  FEventshiftKey := TVRMLEvent.Create(Self, 'shiftKey', TSFBool, false);
  Events.Add(FEventshiftKey);

  DefaultContainerField := 'children';
end;

class function TNodeKeySensor.ClassNodeTypeName: string;
begin
  Result := 'KeySensor';
end;

class function TNodeKeySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeKeySensor.KeyDown(Key: TKey; C: char; const Time: TVRMLTime);

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TVRMLEvent);
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

procedure TNodeKeySensor.KeyUp(Key: TKey; C: char; const Time: TVRMLTime);

  procedure HandleSpecial(var SpecialPressed: boolean; SpecialEvent: TVRMLEvent);
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

procedure TNodeStringSensor.CreateNode;
begin
  inherited;

  FFddeletionAllowed := TSFBool.Create(Self, 'deletionAllowed', true);
  Fields.Add(FFddeletionAllowed);

  FEvententeredText := TVRMLEvent.Create(Self, 'enteredText', TSFString, false);
  Events.Add(FEvententeredText);

  FEventfinalText := TVRMLEvent.Create(Self, 'finalText', TSFString, false);
  Events.Add(FEventfinalText);

  DefaultContainerField := 'children';
end;

class function TNodeStringSensor.ClassNodeTypeName: string;
begin
  Result := 'StringSensor';
end;

class function TNodeStringSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeStringSensor.KeyDown(Key: TKey; C: char; const Time: TVRMLTime);
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
    TNodeKeySensor,
    TNodeStringSensor
  ]);
end;

{$endif read_implementation}
