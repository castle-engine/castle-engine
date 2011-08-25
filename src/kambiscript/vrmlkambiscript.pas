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

{ KambiScript utilities for usage as VRML scripts. }
unit VRMLKambiScript;

interface

uses VRMLFields, KambiScript, KambiUtils, KambiClassUtils, VRMLTime;

{$define read_interface}

type
  TKamScriptVRMLValueList = class(TKamScriptValueList)
  private
    FFieldOrEvents: TVRMLFieldOrEventList;
    FLastEventTimes: TVRMLTimeList;
    InsideAfterExecute: boolean;
  public
    constructor Create(AFreeObjects: boolean);
    destructor Destroy; override;

    { List of field/events associated with this list's KamScript variables.
      This list is read-only (use @link(Add) to add here).
      It always has the same Count as our own count. }
    property FieldOrEvents: TVRMLFieldOrEventList read FFieldOrEvents;

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
  FieldOrEvent: TVRMLFieldOrEvent; var LastEventTime: TVRMLTime);

{$undef read_interface}

implementation

uses SysUtils, VRMLNodes, KambiLog, KambiScriptVectors, KambiWarnings,
  VectorMath, KambiScriptImages, KambiScriptArrays;

{$define read_implementation}

type
  { We use TKamScriptVec4f to represent VRML rotations.
    TKamScriptRotation is just for notation convenience. }
  TKamScriptRotation = TKamScriptVec4f;
  TKamScriptRotationArray = TKamScriptVec4fArray;

{ general utils -------------------------------------------------------- }

function VRMLKamScriptCreateValue(FieldOrEvent: TVRMLFieldOrEvent): TKamScriptValue;
var
  FieldClass: TVRMLFieldClass;
begin
  if FieldOrEvent is TVRMLEvent then
    FieldClass := TVRMLEvent(FieldOrEvent).FieldClass else
    FieldClass := TVRMLFieldClass(FieldOrEvent.ClassType);

  if FieldClass.InheritsFrom(TSFEnum) or
     FieldClass.InheritsFrom(TSFLong) then
    Result := TKamScriptInteger.Create(true) else
  if FieldClass.InheritsFrom(TMFLong) then
    Result := TKamScriptLongIntArray.Create(true) else
  if FieldClass.InheritsFrom(TSFFloat) or
     FieldClass.InheritsFrom(TSFDouble) then
    Result := TKamScriptFloat.Create(true) else
  if FieldClass.InheritsFrom(TMFFloat) then
    Result := TKamScriptSingleArray.Create(true) else
  if FieldClass.InheritsFrom(TMFDouble) then
    Result := TKamScriptDoubleArray.Create(true) else
  if FieldClass.InheritsFrom(TSFBool) then
    Result := TKamScriptBoolean.Create(true) else
  if FieldClass.InheritsFrom(TMFBool) then
    Result := TKamScriptBooleanArray.Create(true) else
  if FieldClass.InheritsFrom(TSFString) then
    Result := TKamScriptString.Create(true) else
  if FieldClass.InheritsFrom(TMFString) then
    Result := TKamScriptStringArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2f) then
    Result := TKamScriptVec2f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec2f) then
    Result := TKamScriptVec2fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3f) then
    Result := TKamScriptVec3f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec3f) then
    Result := TKamScriptVec3fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4f) then
    Result := TKamScriptVec4f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec4f) then
    Result := TKamScriptVec4fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2d) then
    Result := TKamScriptVec2d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec2d) then
    Result := TKamScriptVec2dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3d) then
    Result := TKamScriptVec3d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec3d) then
    Result := TKamScriptVec3dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4d) then
    Result := TKamScriptVec4d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec4d) then
    Result := TKamScriptVec4dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFRotation) then
    Result := TKamScriptRotation.Create(true) else
  if FieldClass.InheritsFrom(TMFRotation) then
    Result := TKamScriptRotationArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix3f) then
    Result := TKamScriptMatrix3f.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix3f) then
    Result := TKamScriptMatrix3fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix4f) then
    Result := TKamScriptMatrix4f.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix4f) then
    Result := TKamScriptMatrix4fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix3d) then
    Result := TKamScriptMatrix3d.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix3d) then
    Result := TKamScriptMatrix3dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix4d) then
    Result := TKamScriptMatrix4d.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix4d) then
    Result := TKamScriptMatrix4dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFImage) {or
     FieldClass.InheritsFrom(TMFImage) }then
    Result := TKamScriptImage.Create(true) else
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'Note that KambiScript is not yet suitable to process values of type ' + FieldClass.VrmlTypeName);
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
      TKamScriptLongIntArray(Value).Value := TMFLong(Field).Items else

    if Field is TSFFloat then
      TKamScriptFloat(Value).Value := TSFFloat(Field).Value else
    if Field is TSFDouble then
      TKamScriptFloat(Value).Value := TSFDouble(Field).Value else
    if Field is TMFFloat then
      TKamScriptSingleArray(Value).Value := TMFFloat(Field).Items else
    if Field is TMFDouble then
      TKamScriptDoubleArray(Value).Value := TMFDouble(Field).Items else

    if Field is TSFBool then
      TKamScriptBoolean(Value).Value := TSFBool(Field).Value else
    if Field is TMFBool then
      TKamScriptBooleanArray(Value).Value := TMFBool(Field).Items else

    if Field is TSFString then
      TKamScriptString(Value).Value := TSFString(Field).Value else
    if Field is TMFString then
      TKamScriptStringArray(Value).Value := TMFString(Field).Items else

    if Field is TSFVec2f then
      TKamScriptVec2f(Value).Value := TSFVec2f(Field).Value else
    if Field is TMFVec2f then
      TKamScriptVec2fArray(Value).Value := TMFVec2f(Field).Items else

    if Field is TSFVec3f then
      TKamScriptVec3f(Value).Value := TSFVec3f(Field).Value else
    if Field is TMFVec3f then
      TKamScriptVec3fArray(Value).Value := TMFVec3f(Field).Items else

    if Field is TSFVec4f then
      TKamScriptVec4f(Value).Value := TSFVec4f(Field).Value else
    if Field is TMFVec4f then
      TKamScriptVec4fArray(Value).Value := TMFVec4f(Field).Items else

    if Field is TSFVec2d then
      TKamScriptVec2d(Value).Value := TSFVec2d(Field).Value else
    if Field is TMFVec2d then
      TKamScriptVec2dArray(Value).Value := TMFVec2d(Field).Items else

    if Field is TSFVec3d then
      TKamScriptVec3d(Value).Value := TSFVec3d(Field).Value else
    if Field is TMFVec3d then
      TKamScriptVec3dArray(Value).Value := TMFVec3d(Field).Items else

    if Field is TSFVec4d then
      TKamScriptVec4d(Value).Value := TSFVec4d(Field).Value else
    if Field is TMFVec4d then
      TKamScriptVec4dArray(Value).Value := TMFVec4d(Field).Items else

    if Field is TSFRotation then
      TKamScriptRotation(Value).Value := TSFRotation(Field).Value else
    if Field is TMFRotation then
      TKamScriptRotationArray(Value).Value := TMFRotation(Field).Items else

    if Field is TSFMatrix3f then
      TKamScriptMatrix3f(Value).Value := TSFMatrix3f(Field).Value else
    if Field is TMFMatrix3f then
      TKamScriptMatrix3fArray(Value).Value := TMFMatrix3f(Field).Items else

    if Field is TSFMatrix4f then
      TKamScriptMatrix4f(Value).Value := TSFMatrix4f(Field).Value else
    if Field is TMFMatrix4f then
      TKamScriptMatrix4fArray(Value).Value := TMFMatrix4f(Field).Items else

    if Field is TSFMatrix3d then
      TKamScriptMatrix3d(Value).Value := TSFMatrix3d(Field).Value else
    if Field is TMFMatrix3d then
      TKamScriptMatrix3dArray(Value).Value := TMFMatrix3d(Field).Items else

    if Field is TSFMatrix4d then
      TKamScriptMatrix4d(Value).Value := TSFMatrix4d(Field).Value else
    if Field is TMFMatrix4d then
      TKamScriptMatrix4dArray(Value).Value := TMFMatrix4d(Field).Items else

    if Field is TSFImage then
      TKamScriptImage(Value).Value := TSFImage(Field).Value else
    {if Field is TMFImage then
      TKamScriptImageArray(Value).Value := TMFImage(Field).Items else}

      { No sensible way to convert, just fall back to predictable 0.0. }
      TKamScriptFloat(Value).Value := 0.0;
  end;

begin
  if FieldOrEvent is TVRMLField then
    AssignVRMLFieldValue(TVRMLField(FieldOrEvent));

  Value.ValueAssigned := false;
end;

procedure VRMLKamScriptAfterExecute(Value: TKamScriptValue;
  FieldOrEvent: TVRMLFieldOrEvent; var LastEventTime: TVRMLTime);

  function GetTimestamp(out Time: TVRMLTime): boolean;
  begin
    { In practice, this should always return true, as script is
      run only when ProcessEvents := true, script node always has
      ParentNode assigned, and when ProcessEvents = true then
      EventsEngine is also assigned.
      But we secure here, and work like this wasn't required.

      This way you could use scripting even without ProcessEvents
      (just to set initializeOnly). Useless, but possible. }

    Result := (FieldOrEvent.ParentNode <> nil) and
      (TX3DNode(FieldOrEvent.ParentNode).Scene <> nil);
    if Result then
    begin
      Time := TX3DNode(FieldOrEvent.ParentNode).Scene.GetTime;
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

    Result := Event.FieldClass.CreateUndefined(Event.ParentNode, Event.Name, false);
  end;

var
  AbortSending: boolean;
  Field: TVRMLField;
  Time: TVRMLTime;
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
      TMFLong(Field).Items := TKamScriptLongIntArray(Value).Value else

    if Field is TSFFloat then
      TSFFloat(Field).Value := TKamScriptFloat(Value).Value else
    if Field is TSFDouble then
      TSFDouble(Field).Value := TKamScriptFloat(Value).Value else
    if Field is TMFFloat then
      TMFFloat(Field).Items := TKamScriptSingleArray(Value).Value else
    if Field is TMFDouble then
      TMFDouble(Field).Items := TKamScriptDoubleArray(Value).Value else

    if Field is TSFBool then
      TSFBool(Field).Value := TKamScriptBoolean(Value).Value else
    if Field is TMFBool then
      TMFBool(Field).Items := TKamScriptBooleanArray(Value).Value else

    if Field is TSFString then
      TSFString(Field).Value := TKamScriptString(Value).Value else
    if Field is TMFString then
      TMFString(Field).Items := TKamScriptStringArray(Value).Value else

    if Field is TSFVec2f then
      TSFVec2f(Field).Value := TKamScriptVec2f(Value).Value else
    if Field is TMFVec2f then
      TMFVec2f(Field).Items := TKamScriptVec2fArray(Value).Value else

    if Field is TSFVec3f then
      TSFVec3f(Field).Value := TKamScriptVec3f(Value).Value else
    if Field is TMFVec3f then
      TMFVec3f(Field).Items := TKamScriptVec3fArray(Value).Value else

    if Field is TSFVec4f then
      TSFVec4f(Field).Value := TKamScriptVec4f(Value).Value else
    if Field is TMFVec4f then
      TMFVec4f(Field).Items := TKamScriptVec4fArray(Value).Value else

    if Field is TSFVec2d then
      TSFVec2d(Field).Value := TKamScriptVec2d(Value).Value else
    if Field is TMFVec2d then
      TMFVec2d(Field).Items := TKamScriptVec2dArray(Value).Value else

    if Field is TSFVec3d then
      TSFVec3d(Field).Value := TKamScriptVec3d(Value).Value else
    if Field is TMFVec3d then
      TMFVec3d(Field).Items := TKamScriptVec3dArray(Value).Value else

    if Field is TSFVec4d then
      TSFVec4d(Field).Value := TKamScriptVec4d(Value).Value else
    if Field is TMFVec4d then
      TMFVec4d(Field).Items := TKamScriptVec4dArray(Value).Value else

    if Field is TSFRotation then
      TSFRotation(Field).Value := TKamScriptRotation(Value).Value else
    if Field is TMFRotation then
      TMFRotation(Field).Items := TKamScriptRotationArray(Value).Value else

    if Field is TSFMatrix3f then
      TSFMatrix3f(Field).Value := TKamScriptMatrix3f(Value).Value else
    if Field is TMFMatrix3f then
      TMFMatrix3f(Field).Items := TKamScriptMatrix3fArray(Value).Value else

    if Field is TSFMatrix4f then
      TSFMatrix4f(Field).Value := TKamScriptMatrix4f(Value).Value else
    if Field is TMFMatrix4f then
      TMFMatrix4f(Field).Items := TKamScriptMatrix4fArray(Value).Value else

    if Field is TSFMatrix3d then
      TSFMatrix3d(Field).Value := TKamScriptMatrix3d(Value).Value else
    if Field is TMFMatrix3d then
      TMFMatrix3d(Field).Items := TKamScriptMatrix3dArray(Value).Value else

    if Field is TSFMatrix4d then
      TSFMatrix4d(Field).Value := TKamScriptMatrix4d(Value).Value else
    if Field is TMFMatrix4d then
      TMFMatrix4d(Field).Items := TKamScriptMatrix4dArray(Value).Value else

    if Field is TSFImage then
    begin
      FreeAndNil(TSFImage(Field).Value);
      TSFImage(Field).Value := TKamScriptImage(Value).Value.MakeCopy;
    end else
    {if Field is TMFImage then
    begin
      TMFImage(Field).Items := TKamScriptImageArray(Value).Value
    end else}

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
            [ SendToEvent.Name, Time.Seconds, LastEventTime.Seconds ]));
      end;
      FreeAndNil(Field);
    end;

    { Theoretically, we should call Scene.ChangedField now
      if FieldOrEvent was initializeOnly field (and not AbortSending).
      But in this case, we know FieldOrEvent comes from a Script node,
      and in this situation ChangedField doesn't do anything anyway. }

    { This is needed for TKamScriptVRMLValueList.AfterExecute trick.
      We handled this change, so we mark it by ValueAssigned = false. }
    Value.ValueAssigned := false;
  end;
end;

{ TKamScriptVRMLValueList -------------------------------------------------- }

constructor TKamScriptVRMLValueList.Create(AFreeObjects: boolean);
begin
  inherited;
  FFieldOrEvents := TVRMLFieldOrEventList.Create(false);
  FLastEventTimes := TVRMLTimeList.Create;
end;

destructor TKamScriptVRMLValueList.Destroy;
begin
  SysUtils.FreeAndNil(FFieldOrEvents);
  SysUtils.FreeAndNil(FLastEventTimes);
  inherited;
end;

procedure TKamScriptVRMLValueList.Add(FieldOrEvent: TVRMLFieldOrEvent);
begin
  inherited Add(VRMLKamScriptCreateValue(FieldOrEvent));
  FieldOrEvents.Add(FieldOrEvent);
  FLastEventTimes.Add(OldestVRMLTime);
end;

procedure TKamScriptVRMLValueList.BeforeExecute;
var
  I: Integer;
begin
  if not InsideAfterExecute then
  begin
    for I := 0 to Count - 1 do
      VRMLKamScriptBeforeExecute(Items[I], FieldOrEvents[I]);
  end;
end;

procedure TKamScriptVRMLValueList.AfterExecute;
var
  I: Integer;
  WasSomeValueAssigned: boolean;
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
      sending of fields/events.

      Note that execution of recursive script may cause setting ValueAssigned
      := true on earlier value. So we have to reiterate over Items,
      until no ValueAssigned needs to be handled. Don't worry, this will
      not cause loops, FLastEventTimes prevents the loops. So eventually
      all fields will send their out values, and then WasSomeValueAssigned
      will have to be false and we will finish. }

    InsideAfterExecute := true;
    try
      repeat
        WasSomeValueAssigned := false;
        for I := 0 to Count - 1 do
        begin
          if Items[I].ValueAssigned then
            WasSomeValueAssigned := true;
          VRMLKamScriptAfterExecute(Items[I], FieldOrEvents[I], FLastEventTimes.L[I]);
        end;
      until not WasSomeValueAssigned;
    finally InsideAfterExecute := false end;
  end;
end;

procedure TKamScriptVRMLValueList.ResetLastEventTimes;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FLastEventTimes.L[I] := OldestVRMLTime;
end;

end.
