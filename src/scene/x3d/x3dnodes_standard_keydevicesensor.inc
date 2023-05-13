{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2021 Michalis Kamburelis.

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
    { React to key down/up events.

      This is used by TCastleSceneCore to communicate with this key sensor.

      Semantics follow TInputPressRelease behavior for EventType = itKey.
      This means that Key may be keyNone (if not
      representable as TKey) and C may be #0 (if not representable as char),
      but not both --- at least one of Key or C must contains something useful.
      Also, key presses are affected
      by the "key repeat" feature of the OS (windowing system actually), so when
      user holds down a key --- we get many key down messages.

      @groupBegin }
    procedure KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime); virtual;
    procedure KeyUp(const Key: TKey; const C: Char; const Time: TX3DTime); virtual;
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_x3dkeydevicesensornode.inc}
  end;

  { Generate events when user key presses or releases keys on the keyboard. }
  TKeySensorNode = class(TAbstractKeyDeviceSensorNode)
  strict private
    AltKey, ControlKey, ShiftKey: boolean;
  public
    procedure KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime); override;
    procedure KeyUp(const Key: TKey; const C: Char; const Time: TX3DTime); override;
  {$I auto_generated_node_helpers/x3dnodes_keysensor.inc}
  end;

  { Generates events as the user inputs a longer string on the keyboard,
    useful for example to simulate "input fields" in 3D. }
  TStringSensorNode = class(TAbstractKeyDeviceSensorNode)
  strict private
    EnteredText: string;
    Active: boolean;
  public
    procedure KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime); override;
  {$I auto_generated_node_helpers/x3dnodes_stringsensor.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ internal utils ------------------------------------------------------------- }

{ Convert TKey to X3D "action" key code.
  As defined by X3D KeySensor node specification. }
function KeyToActionKey(const Key: TKey; out ActionKey: Integer): boolean;
begin
  Result := true;
  case Key of
    keyF1 .. keyF12 : ActionKey := Ord(Key) - Ord(keyF1) + 1;
    keyHome       : ActionKey := 13;
    keyEnd        : ActionKey := 14;
    keyPageUp     : ActionKey := 15;
    keyPageDown   : ActionKey := 16;
    keyArrowUp    : ActionKey := 17;
    keyArrowDown  : ActionKey := 18;
    keyArrowLeft  : ActionKey := 19;
    keyArrowRight : ActionKey := 20;
    else Result := false;
  end;
end;

{ TAbstractKeyDeviceSensorNode ----------------------------------------------- }

procedure TAbstractKeyDeviceSensorNode.KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime);
begin
  { Nothing to do in this class. }
end;

procedure TAbstractKeyDeviceSensorNode.KeyUp(const Key: TKey; const C: Char; const Time: TX3DTime);
begin
  { Nothing to do in this class. }
end;

{ TKeySensorNode ------------------------------------------------------------- }

procedure TKeySensorNode.KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime);

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
      keyAlt  : HandleSpecial(AltKey, EventAltKey);
      keyCtrl : HandleSpecial(ControlKey, EventControlKey);
      keyShift: HandleSpecial(ShiftKey, EventShiftKey);
      else ;
    end;
  end;
end;

procedure TKeySensorNode.KeyUp(const Key: TKey; const C: Char; const Time: TX3DTime);

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
      keyAlt  : HandleSpecial(AltKey, EventAltKey);
      keyCtrl : HandleSpecial(ControlKey, EventControlKey);
      keyShift: HandleSpecial(ShiftKey, EventShiftKey);
      else ;
    end;
  end;
  inherited;
end;

{ TStringSensorNode ---------------------------------------------------------- }

procedure TStringSensorNode.KeyDown(const Key: TKey; const C: Char; const Time: TX3DTime);
begin
  inherited;

  if FdEnabled.Value and ( (C <> #0) or (Key in [keyBackSpace, keyEnter]) ) then
  begin
    if not Active then
    begin
      Active := true;
      EventIsActive.Send(Active, Time);
    end;

    { Catch both CharBackspace and keyBackSpace.
      Catching CharBackspace is not enough, because it's also equal to Ctrl+H.
      Same for CharEnter: it's also equal to Ctrl+M.
      And Ctrl+... may be often used by some menu items, see e.g. view3dscene. }

    if (C = CharBackspace) or (Key = keyBackSpace) then
    begin
      if FdDeletionAllowed.Value and (EnteredText <> '') then
      begin
        SetLength(EnteredText, Length(EnteredText) - 1);
        EventEnteredText.Send(EnteredText, Time);
      end;
    end else
    if (C = CharEnter) or (Key = keyEnter) then
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
