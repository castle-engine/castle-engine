{
  Copyright 2008-2010 Michalis Kamburelis.

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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeKeySensor = class(TNodeX3DKeyDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    procedure KeyDown(Key: TKey; C: char; const Time: TVRMLTime);
    procedure KeyUp(Key: TKey; C: char; const Time: TVRMLTime);
    { @groupEnd }
  end;

  TNodeStringSensor = class(TNodeX3DKeyDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddeletionAllowed: TSFBool;
    public property FddeletionAllowed: TSFBool read FFddeletionAllowed;

    { Event: SFString, out } { }
    private FEvententeredText: TVRMLEvent;
    public property EvententeredText: TVRMLEvent read FEvententeredText;

    { Event: SFString, out } { }
    private FEventfinalText: TVRMLEvent;
    public property EventfinalText: TVRMLEvent read FEventfinalText;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DKeyDeviceSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
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

constructor TNodeKeySensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
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

function TNodeKeySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeKeySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeKeySensor.KeyDown(Key: TKey; C: char; const Time: TVRMLTime);
var
  ActionKey: Integer;
begin
  if FdEnabled.Value then
  begin
    EventIsActive.Send(true, Time);
    if KeyToActionKey(Key, ActionKey) then
      EventActionKeyPress.Send(ActionKey, Time);
    if C <> #0 then
      EventKeyPress.Send(C, Time);
    case Key of
      K_Alt: EventAltKey.Send(true, Time);
      K_Ctrl: EventControlKey.Send(true, Time);
      K_Shift: EventShiftKey.Send(true, Time);
    end;
  end;
end;

procedure TNodeKeySensor.KeyUp(Key: TKey; C: char; const Time: TVRMLTime);
var
  ActionKey: Integer;
begin
  if FdEnabled.Value then
  begin
    EventIsActive.Send(false, Time);
    if KeyToActionKey(Key, ActionKey) then
      EventActionKeyRelease.Send(ActionKey, Time);
    if C <> #0 then
      EventKeyRelease.Send(C, Time);
    case Key of
      K_Alt: EventAltKey.Send(false, Time);
      K_Ctrl: EventControlKey.Send(false, Time);
      K_Shift: EventShiftKey.Send(false, Time);
    end;
  end;
end;

constructor TNodeStringSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
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

function TNodeStringSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeStringSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterKeyDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeKeySensor,
    TNodeStringSensor
  ]);
end;

{$endif read_implementation}
