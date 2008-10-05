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

uses VRMLFields, KambiScript, KambiUtils, KambiClassUtils, KambiTimeUtils;

{$define read_interface}

type
  TKamScriptVRMLValuesList = class(TKamScriptValuesList)
  private
    FFieldOrEvents: TVRMLFieldOrEventsList;
    FLastEventTimes: TDynDoubleArray;
    InsideAfterExecute: boolean;
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

    { Clear the memory when the last events were generated from this script.
      For each script output-capable field (inputOutput or outputOnly)
      on this list, we keep track of when it last generated an event.

      This is needed to avoid loops.

      @italic(Some background info why this is needed:)

      X3D spec says (4.4.8.3 Execution model):
      Nodes that contain output events shall produce at most
      one event per field per timestamp. [...] This also applies to scripts.

      "[...]" above talks about ROUTE breaking rule, we handle this
      generally in TVRMLRoute. The trouble with script is that it can
      make loops even without the help of ROUTEs: script receives it's inputs,
      and sends it's outputs, so when you send an output that causes your own
      input you made a loop... The most trivial example of this
      is when on receiving inputOutput you will set the very same inputOutput
      field, thus making a loop.

      Scripts with directOutput (although not implemented yet) can
      cause even more trouble, as you can generate input event on another node
      that will get back to you. If two scripts generate events on each
      other, this can again make a loop, without the help of ROUTEs.

      So we have to secure against this. This is done pretty much like
      with ROUTEs: we just remember the last timestamp, and ignore sending
      events again. }
    procedure ResetLastEventTimes;
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
  FieldOrEvent: TVRMLFieldOrEvent; var LastEventTime: TKamTime);

{$undef read_interface}

implementation

uses SysUtils, VRMLNodes, VRMLScene, VRMLErrors, KambiLog, KambiScriptVectors,
  VectorMath;

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
    Result := TKamScriptInteger.Create(true) else
  if FieldClass.InheritsFrom(TSFFloat) or
     FieldClass.InheritsFrom(TSFDouble) or
     FieldClass.InheritsFrom(TMFFloat) or
     FieldClass.InheritsFrom(TMFDouble) then
    Result := TKamScriptFloat.Create(true) else
  if FieldClass.InheritsFrom(TSFBool) or
     FieldClass.InheritsFrom(TMFBool) then
    Result := TKamScriptBoolean.Create(true) else
  if FieldClass.InheritsFrom(TSFString) or
     FieldClass.InheritsFrom(TMFString) then
    Result := TKamScriptString.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2f) or
     FieldClass.InheritsFrom(TMFVec2f) then
    Result := TKamScriptVec2f.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3f) or
     FieldClass.InheritsFrom(TMFVec3f) then
    Result := TKamScriptVec3f.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4f) or
     FieldClass.InheritsFrom(TMFVec4f) then
    Result := TKamScriptVec4f.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2d) or
     FieldClass.InheritsFrom(TMFVec2d) then
    Result := TKamScriptVec2d.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3d) or
     FieldClass.InheritsFrom(TMFVec3d) then
    Result := TKamScriptVec3d.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4d) or
     FieldClass.InheritsFrom(TMFVec4d) then
    Result := TKamScriptVec4d.Create(true) else
  begin
    VRMLNonFatalError('Note that KambiScript is not yet suitable to process values of type ' + FieldClass.VrmlTypeName);
    Result := TKamScriptFloat.Create(true);
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

    if Field is TSFVec2f then
      TKamScriptVec2f(Value).Value := TSFVec2f(Field).Value else
    if Field is TMFVec2f then
    begin
      if TMFVec2f(Field).Items.Count >= 1 then
        TKamScriptVec2f(Value).Value := TMFVec2f(Field).Items.Items[0] else
        TKamScriptVec2f(Value).Value := ZeroVector2Single; { anything predictable }
    end else

    if Field is TSFVec3f then
      TKamScriptVec3f(Value).Value := TSFVec3f(Field).Value else
    if Field is TMFVec3f then
    begin
      if TMFVec3f(Field).Items.Count >= 1 then
        TKamScriptVec3f(Value).Value := TMFVec3f(Field).Items.Items[0] else
        TKamScriptVec3f(Value).Value := ZeroVector3Single; { anything predictable }
    end else

    if Field is TSFVec4f then
      TKamScriptVec4f(Value).Value := TSFVec4f(Field).Value else
    if Field is TMFVec4f then
    begin
      if TMFVec4f(Field).Items.Count >= 1 then
        TKamScriptVec4f(Value).Value := TMFVec4f(Field).Items.Items[0] else
        TKamScriptVec4f(Value).Value := ZeroVector4Single; { anything predictable }
    end else

    if Field is TSFVec2d then
      TKamScriptVec2d(Value).Value := TSFVec2d(Field).Value else
    if Field is TMFVec2d then
    begin
      if TMFVec2d(Field).Items.Count >= 1 then
        TKamScriptVec2d(Value).Value := TMFVec2d(Field).Items.Items[0] else
        TKamScriptVec2d(Value).Value := ZeroVector2Double; { anything predictable }
    end else

    if Field is TSFVec3d then
      TKamScriptVec3d(Value).Value := TSFVec3d(Field).Value else
    if Field is TMFVec3d then
    begin
      if TMFVec3d(Field).Items.Count >= 1 then
        TKamScriptVec3d(Value).Value := TMFVec3d(Field).Items.Items[0] else
        TKamScriptVec3d(Value).Value := ZeroVector3Double; { anything predictable }
    end else

    if Field is TSFVec4d then
      TKamScriptVec4d(Value).Value := TSFVec4d(Field).Value else
    if Field is TMFVec4d then
    begin
      if TMFVec4d(Field).Items.Count >= 1 then
        TKamScriptVec4d(Value).Value := TMFVec4d(Field).Items.Items[0] else
        TKamScriptVec4d(Value).Value := ZeroVector4Double; { anything predictable }
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
  FieldOrEvent: TVRMLFieldOrEvent; var LastEventTime: TKamTime);

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
  Time: TKamTime;
  SendToEvent: TVRMLEvent;
begin
  if Value.ValueAssigned then
  begin
    { calculate Field, will be set based on our current Value }

    if FieldOrEvent is TVRMLEvent then
    begin
      SendToEvent := TVRMLEvent(FieldOrEvent);
      Field := CreateTempField(SendToEvent);
    end else
    if (FieldOrEvent as TVRMLField).Exposed then
    begin
      SendToEvent := TVRMLField(FieldOrEvent).EventIn;
      Field := CreateTempField(SendToEvent);
    end else
    begin
      SendToEvent := nil;
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

    if Field is TSFVec2f then
      TSFVec2f(Field).Value := TKamScriptVec2f(Value).Value else
    if Field is TMFVec2f then
    begin
      TMFVec2f(Field).Items.Count := 1;
      TMFVec2f(Field).Items.Items[0] := TKamScriptVec2f(Value).Value;
    end else

    if Field is TSFVec3f then
      TSFVec3f(Field).Value := TKamScriptVec3f(Value).Value else
    if Field is TMFVec3f then
    begin
      TMFVec3f(Field).Items.Count := 1;
      TMFVec3f(Field).Items.Items[0] := TKamScriptVec3f(Value).Value;
    end else

    if Field is TSFVec4f then
      TSFVec4f(Field).Value := TKamScriptVec4f(Value).Value else
    if Field is TMFVec4f then
    begin
      TMFVec4f(Field).Items.Count := 1;
      TMFVec4f(Field).Items.Items[0] := TKamScriptVec4f(Value).Value;
    end else

    if Field is TSFVec2d then
      TSFVec2d(Field).Value := TKamScriptVec2d(Value).Value else
    if Field is TMFVec2d then
    begin
      TMFVec2d(Field).Items.Count := 1;
      TMFVec2d(Field).Items.Items[0] := TKamScriptVec2d(Value).Value;
    end else

    if Field is TSFVec3d then
      TSFVec3d(Field).Value := TKamScriptVec3d(Value).Value else
    if Field is TMFVec3d then
    begin
      TMFVec3d(Field).Items.Count := 1;
      TMFVec3d(Field).Items.Items[0] := TKamScriptVec3d(Value).Value;
    end else

    if Field is TSFVec4d then
      TSFVec4d(Field).Value := TKamScriptVec4d(Value).Value else
    if Field is TMFVec4d then
    begin
      TMFVec4d(Field).Items.Count := 1;
      TMFVec4d(Field).Items.Items[0] := TKamScriptVec4d(Value).Value;
    end else

    begin
      { No sensible way to convert, just do nothing, don't set/send anything. }
      AbortSending := true;
    end;

    { send and free Field, for output events or exposed fields. }

    if SendToEvent <> nil then
    begin
      if (not AbortSending) and
        GetTimestamp(Time) then
      begin
        if Time > LastEventTime then
        begin
          LastEventTime := Time;
          SendToEvent.Send(Field, Time);
        end else
        if Log then
          WritelnLog('VRML Script', Format(
            'Sending event %s from Script ignored at <= timestamp (%f, while last event was on %f). Potential loop avoided',
            [ SendToEvent.Name, Time, LastEventTime ]));
      end;
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
  FLastEventTimes := TDynDoubleArray.Create;
end;

destructor TKamScriptVRMLValuesList.Destroy;
begin
  SysUtils.FreeAndNil(FFieldOrEvents);
  SysUtils.FreeAndNil(FLastEventTimes);
  inherited;
end;

procedure TKamScriptVRMLValuesList.Add(FieldOrEvent: TVRMLFieldOrEvent);
begin
  inherited Add(VRMLKamScriptCreateValue(FieldOrEvent));
  FieldOrEvents.Add(FieldOrEvent);
  FLastEventTimes.AppendItem(OldestTime);
end;

procedure TKamScriptVRMLValuesList.BeforeExecute;
var
  I: Integer;
begin
  if not InsideAfterExecute then
  begin
    for I := 0 to Count - 1 do
      VRMLKamScriptBeforeExecute(Items[I], FieldOrEvents[I]);
  end;
end;

procedure TKamScriptVRMLValuesList.AfterExecute;
var
  I: Integer;
begin
  if not InsideAfterExecute then
  begin
    { Note that VRMLKamScriptAfterExecute may send events when given
      fields changed. These events may cause yet another execution
      of the same script. For example,
      - setting inputOutput field of the script causes another execution
        of the same script
      - setting anything may cause route to another input event.
        It may be another field/event of the same script,
        or it may be field/event of different node (possibly, different
        script node) that will cause execution back to our script
        by another route...

      So we have to secure against this. We don't want to allow
      resetting of ValueAssigned at the beginning of recursive script calls.
      Actually, we also don't want to reinitialize the KambiScript field
      from VRML field value, to not lose new value.
      What to do about changed values? If recursive script execution
      will change again the same field, we lose the old value, but that's
      somewhat Ok.

      My approach: using InsideAfterExecute.
      If InsideAfterExecute is already true, then Before/AfterExecute
      do nothing. Actual script execution will possibly change ValueAssigned
      to true for some fields, and change their value, that's Ok.
      Only Before/AfterExecute with InsideAfterExecute = false will cause actual
      sending of fields/events. }

    InsideAfterExecute := true;
    try
      for I := 0 to Count - 1 do
        VRMLKamScriptAfterExecute(Items[I], FieldOrEvents[I], FLastEventTimes.Items[I]);
    finally InsideAfterExecute := false end;
  end;
end;

procedure TKamScriptVRMLValuesList.ResetLastEventTimes;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FLastEventTimes.Items[I] := OldestTime;
end;

end.
