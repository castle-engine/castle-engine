{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ KambiScript utilities for usage as VRML scripts. }
unit VRMLKambiScript;

interface

uses VRMLFields, KambiScript, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TKamScriptVRMLValuesList = class(TKamScriptValuesList)
  private
    FFieldOrEvents: TVRMLFieldOrEventsList;
  public
    constructor Create;
    destructor Destroy; override;

    { List of field/events associated with this list's KamScript variables.
      This list is read-only (use @link(Add) to add here).
      It always has the same Count as our own count. }
    property FieldOrEvents: TVRMLFieldOrEventsList read FFieldOrEvents;

    { Create TKamScriptValue descendant suitable to hold FieldOrEvent
      value, and add it to Items.
      FieldOrEvent is added to FieldOrEvents list, so we keep
      all information. }
    procedure Add(FieldOrEvent: TVRMLFieldOrEvent);

    procedure BeforeExecute;
    procedure AfterExecute;
  end;

function VRMLKamScriptCreateValue(FieldOrEvent: TVRMLFieldOrEvent): TKamScriptValue;

{ Do common things before VRML script with this variable is executed.
  This resets ValueAssigned (will be used in AfterExecute),
  and sets current variable's value from FieldOrEvent (if this is a field). }
procedure VRMLKamScriptBeforeExecute(Value: TKamScriptValue;
  FieldOrEvent: TVRMLFieldOrEvent);

{ Do common things after VRML script with this variable is executed.
  This checks ValueAssigned, and propagates value change to appropriate
  field/event, sending event/setting field. }
procedure VRMLKamScriptAfterExecute(Value: TKamScriptValue;
  FieldOrEvent: TVRMLFieldOrEvent);

{$undef read_interface}

implementation

uses SysUtils, KambiTimeUtils, VRMLNodes, VRMLScene, VRMLErrors;

{$define read_implementation}

{ general utils -------------------------------------------------------- }

function VRMLKamScriptCreateValue(FieldOrEvent: TVRMLFieldOrEvent): TKamScriptValue;
var
  FieldClass: TVRMLFieldClass;
begin
  if FieldOrEvent is TVRMLEvent then
    FieldClass := TVRMLEvent(FieldOrEvent).FieldClass else
    FieldClass := TVRMLFieldClass(FieldOrEvent.ClassType);

  if FieldClass.InheritsFrom(TSFEnum) or
     FieldClass.InheritsFrom(TSFLong) or
     FieldClass.InheritsFrom(TMFLong) then
    Result := TKamScriptInteger.Create else
  if FieldClass.InheritsFrom(TSFFloat) or
     FieldClass.InheritsFrom(TSFDouble) or
     FieldClass.InheritsFrom(TMFFloat) or
     FieldClass.InheritsFrom(TMFDouble) then
    Result := TKamScriptFloat.Create else
  if FieldClass.InheritsFrom(TSFBool) or
     FieldClass.InheritsFrom(TMFBool) then
    Result := TKamScriptBoolean.Create else
  if FieldClass.InheritsFrom(TSFString) or
     FieldClass.InheritsFrom(TMFString) then
    Result := TKamScriptBoolean.Create else
  begin
    VRMLNonFatalError('Note that KambiScript is not yet suitable to process values of type ' + FieldClass.VrmlTypeName);
    Result := TKamScriptFloat.Create;
  end;

  Result.Name := FieldOrEvent.Name;
end;

procedure VRMLKamScriptBeforeExecute(Value: TKamScriptValue;
  FieldOrEvent: TVRMLFieldOrEvent);

  procedure AssignVRMLFieldValue(Field: TVRMLField);
  begin
    if Field is TSFEnum then
      TKamScriptInteger(Value).Value := TSFEnum(Field).Value else
    if Field is TSFLong then
      TKamScriptInteger(Value).Value := TSFLong(Field).Value else
    if Field is TMFLong then
    begin
      if TMFLong(Field).Items.Count >= 1 then
        TKamScriptInteger(Value).Value := TMFLong(Field).Items.Items[0] else
        TKamScriptInteger(Value).Value := 0; { anything predictable }
    end else

    if Field is TSFFloat then
      TKamScriptFloat(Value).Value := TSFFloat(Field).Value else
    if Field is TSFDouble then
      TKamScriptFloat(Value).Value := TSFDouble(Field).Value else
    if Field is TMFFloat then
    begin
      if TMFFloat(Field).Items.Count >= 1 then
        TKamScriptFloat(Value).Value := TMFFloat(Field).Items.Items[0] else
        TKamScriptFloat(Value).Value := 0.0; { anything predictable }
    end else
    if Field is TMFDouble then
    begin
      if TMFDouble(Field).Items.Count >= 1 then
        TKamScriptFloat(Value).Value := TMFDouble(Field).Items.Items[0] else
        TKamScriptFloat(Value).Value := 0.0; { anything predictable }
    end else

    if Field is TSFBool then
      TKamScriptBoolean(Value).Value := TSFBool(Field).Value else
    if Field is TMFBool then
    begin
      if TMFBool(Field).Items.Count >= 1 then
        TKamScriptBoolean(Value).Value := TMFBool(Field).Items.Items[0] else
        TKamScriptBoolean(Value).Value := false; { anything predictable }
    end else

    if Field is TSFString then
      TKamScriptString(Value).Value := TSFString(Field).Value else
    if Field is TMFString then
    begin
      if TMFString(Field).Items.Count >= 1 then
        TKamScriptString(Value).Value := TMFString(Field).Items.Items[0] else
        TKamScriptString(Value).Value := ''; { anything predictable }
    end else

      { No sensible way to convert, just fall back to predictable 0.0. }
      TKamScriptFloat(Value).Value := 0.0;
  end;

begin
  if FieldOrEvent is TVRMLField then
    AssignVRMLFieldValue(TVRMLField(FieldOrEvent));

  Value.ValueAssigned := false;
end;

procedure VRMLKamScriptAfterExecute(Value: TKamScriptValue;
  FieldOrEvent: TVRMLFieldOrEvent);

  function GetTimestamp(out Time: TKamTime): boolean;
  begin
    { In practice, this should always return true, as script is
      run only when ProcessEvents := true, script node always has
      ParentNode assigned, and when ProcessEvents = true then
      ParentEventsProcessor is also assigned.
      But we secure here, and work like this wasn't required.

      This way you could use scripting even without ProcessEvents
      (just to set initializeOnly). Useless, but possible. }

    Result := (FieldOrEvent.ParentNode <> nil) and
      (TVRMLNode(FieldOrEvent.ParentNode).ParentEventsProcessor <> nil) and
      (TVRMLNode(FieldOrEvent.ParentNode).ParentEventsProcessor is TVRMLScene);
    if Result then
    begin
      Time := TVRMLScene(TVRMLNode(FieldOrEvent.ParentNode).
        ParentEventsProcessor).WorldTime;
    end;
  end;

  function CreateTempField(Event: TVRMLEvent): TVRMLField;
  begin
    { We have to create temporary field to send.
      This is Ok, TVRMLEvent.Send shortcuts would do the same thing
      after all.

      We use Event.ParentNode, Event.Name for this temporary field
      for the same reasons TVRMLEvent.Send shortcuts do this:
      to get nice information in Logger node reports. }

    Result := Event.FieldClass.CreateUndefined(Event.ParentNode, Event.Name);
  end;

var
  AbortSending: boolean;
  Field: TVRMLField;
  Timestamp: TKamTime;
begin
  if Value.ValueAssigned then
  begin
    { calculate Field, will be set based on our current Value }

    if FieldOrEvent is TVRMLEvent then
    begin
      Field := CreateTempField(TVRMLEvent(FieldOrEvent));
    end else
    if (FieldOrEvent as TVRMLField).Exposed then
    begin
      Field := CreateTempField(TVRMLField(FieldOrEvent).EventIn);
    end else
    begin
      Assert(FieldOrEvent is TVRMLField);
      Assert(not TVRMLField(FieldOrEvent).Exposed);
      { For initializeOnly fields, we will set them directly. }
      Field := TVRMLField(FieldOrEvent);
    end;

    AbortSending := false;

    { set Field value (and leave AbortSending as false),
      or leave Field value (and set AbortSending as true). }

    if Field is TSFEnum then
      TSFEnum(Field).Value := TKamScriptInteger(Value).Value else
    if Field is TSFLong then
      TSFLong(Field).Value := TKamScriptInteger(Value).Value else
    if Field is TMFLong then
    begin
      TMFLong(Field).Items.Count := 1;
      TMFLong(Field).Items.Items[0] := TKamScriptInteger(Value).Value;
    end else

    if Field is TSFFloat then
      TSFFloat(Field).Value := TKamScriptFloat(Value).Value else
    if Field is TSFDouble then
      TSFDouble(Field).Value := TKamScriptFloat(Value).Value else
    if Field is TMFFloat then
    begin
      TMFFloat(Field).Items.Count := 1;
      TMFFloat(Field).Items.Items[0] := TKamScriptFloat(Value).Value;
    end else
    if Field is TMFDouble then
    begin
      TMFDouble(Field).Items.Count := 1;
      TMFDouble(Field).Items.Items[0] := TKamScriptFloat(Value).Value;
    end else

    if Field is TSFBool then
      TSFBool(Field).Value := TKamScriptBoolean(Value).Value else
    if Field is TMFBool then
    begin
      TMFBool(Field).Items.Count := 1;
      TMFBool(Field).Items.Items[0] := TKamScriptBoolean(Value).Value;
    end else

    if Field is TSFString then
      TSFString(Field).Value := TKamScriptString(Value).Value else
    if Field is TMFString then
    begin
      TMFString(Field).Items.Count := 1;
      TMFString(Field).Items.Items[0] := TKamScriptString(Value).Value;
    end else

    begin
      { No sensible way to convert, just do nothing, don't set/send anything. }
      AbortSending := true;
    end;

    { send and free Field, for output events or exposed fields }

    if FieldOrEvent is TVRMLEvent then
    begin
      if (not AbortSending) and GetTimestamp(Timestamp) then
        TVRMLEvent(FieldOrEvent).Send(Field, Timestamp);
      FreeAndNil(Field);
    end else
    if (FieldOrEvent as TVRMLField).Exposed then
    begin
      if (not AbortSending) and GetTimestamp(Timestamp) then
        TVRMLField(FieldOrEvent).EventIn.Send(Field, Timestamp);
      FreeAndNil(Field);
    end;

    { Theoretically, we should call now Scene.ChangedFields now
      if FieldOrEvent was initializeOnly field (and not AbortSending).
      But in this case, we know FieldOrEvent comes from a Script node,
      and in this situation ChangedFields doesn't do anything anyway. }
  end;
end;

{ TKamScriptVRMLValuesList -------------------------------------------------- }

constructor TKamScriptVRMLValuesList.Create;
begin
  inherited Create;
  FFieldOrEvents := TVRMLFieldOrEventsList.Create;
end;

destructor TKamScriptVRMLValuesList.Destroy;
begin
  SysUtils.FreeAndNil(FFieldOrEvents);
  inherited;
end;

procedure TKamScriptVRMLValuesList.Add(FieldOrEvent: TVRMLFieldOrEvent);
begin
  inherited Add(VRMLKamScriptCreateValue(FieldOrEvent));
  FieldOrEvents.Add(FieldOrEvent);
end;

procedure TKamScriptVRMLValuesList.BeforeExecute;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VRMLKamScriptBeforeExecute(Items[I], FieldOrEvents[I]);
end;

procedure TKamScriptVRMLValuesList.AfterExecute;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    VRMLKamScriptAfterExecute(Items[I], FieldOrEvents[I]);
end;

end.
