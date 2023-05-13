{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript utilities for usage as X3D scripts. }
unit CastleInternalX3DScript;

{$I castleconf.inc}

interface

uses X3DFields, CastleScript, CastleUtils, CastleClassUtils, X3DTime;

{$define read_interface}

type
  TCasScriptX3DValueList = class(TCasScriptValueList)
  private
    FFieldOrEvents: TX3DFieldOrEventList;
    FLastEventTimes: TX3DTimeList;
    InsideAfterExecute: boolean;
  public
    constructor Create(AFreeObjects: boolean);
    destructor Destroy; override;

    { List of field/events associated with this list's CasScript variables.
      This list is read-only (use @link(Add) to add here).
      It always has the same Count as our own count. }
    property FieldOrEvents: TX3DFieldOrEventList read FFieldOrEvents;

    { Create TCasScriptValue descendant suitable to hold FieldOrEvent
      value, and add it to Items.
      FieldOrEvent is added to FieldOrEvents list, so we keep
      all information. }
    procedure Add(FieldOrEvent: TX3DFieldOrEvent); reintroduce;

    procedure BeforeExecute;
    procedure AfterExecute(const Time: TX3DTime);

    { Clear the memory when the last events were generated from this script.
      For each script output-capable field (inputOutput or outputOnly)
      on this list, we keep track of when it last generated an event.

      This is needed to avoid loops.

      @italic(Some background info why this is needed:)

      X3D spec says (4.4.8.3 Execution model):
      Nodes that contain output events shall produce at most
      one event per field per timestamp. [...] This also applies to scripts.

      "[...]" above talks about ROUTE breaking rule, we handle this
      generally in TX3DRoute. The trouble with script is that it can
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

function X3DCasScriptCreateValue(FieldOrEvent: TX3DFieldOrEvent): TCasScriptValue;

{ Do common things before VRML/X3D script with this variable is executed.
  This resets ValueAssigned (will be used in AfterExecute),
  and sets current variable's value from FieldOrEvent (if this is a field). }
procedure X3DCasScriptBeforeExecute(Value: TCasScriptValue;
  FieldOrEvent: TX3DFieldOrEvent);

{ Do common things after VRML/X3D script with this variable is executed.
  This checks ValueAssigned, and propagates value change to appropriate
  field/event, sending event/setting field. }
procedure X3DCasScriptAfterExecute(Value: TCasScriptValue;
  FieldOrEvent: TX3DFieldOrEvent; var LastEventTime: TX3DTime;
  const Time: TX3DTime);

{$undef read_interface}

implementation

uses SysUtils, X3DNodes, CastleLog, CastleScriptVectors,
  CastleVectors, CastleScriptImages, CastleScriptArrays;

{$define read_implementation}

{$ifdef CASTLE_SCRIPT_FPC}
type
  { We use TCasScriptVec4f to represent VRML/X3D rotations.
    TCasScriptRotation is just for notation convenience. }
  TCasScriptRotation = TCasScriptVec4f;
  TCasScriptRotationArray = TCasScriptVec4fArray;
{$endif CASTLE_SCRIPT_FPC}

{ general utils -------------------------------------------------------- }

function X3DCasScriptCreateValue(FieldOrEvent: TX3DFieldOrEvent): TCasScriptValue;
var
  FieldClass: TX3DFieldClass;
begin
  if FieldOrEvent is TX3DEvent then
    FieldClass := TX3DEvent(FieldOrEvent).FieldClass else
    FieldClass := TX3DFieldClass(FieldOrEvent.ClassType);

  if FieldClass.InheritsFrom(TSFEnum) or
     FieldClass.InheritsFrom(TSFLong) then
    Result := TCasScriptInteger.Create(true) else
  {$ifdef CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TMFLong) then
    Result := TCasScriptInt32Array.Create(true) else
  {$endif CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TSFFloat) or
     FieldClass.InheritsFrom(TSFDouble) then
    Result := TCasScriptFloat.Create(true) else
  {$ifdef CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TMFFloat) then
    Result := TCasScriptSingleArray.Create(true) else
  if FieldClass.InheritsFrom(TMFDouble) then
    Result := TCasScriptDoubleArray.Create(true) else
  {$endif CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TSFBool) then
    Result := TCasScriptBoolean.Create(true) else
  {$ifdef CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TMFBool) then
    Result := TCasScriptBooleanArray.Create(true) else
  {$endif CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TSFString) then
    Result := TCasScriptString.Create(true) else
  {$ifdef CASTLE_SCRIPT_FPC}
  if FieldClass.InheritsFrom(TMFString) then
    Result := TCasScriptStringArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2f) then
    Result := TCasScriptVec2f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec2f) then
    Result := TCasScriptVec2fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3f) then
    Result := TCasScriptVec3f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec3f) then
    Result := TCasScriptVec3fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4f) then
    Result := TCasScriptVec4f.Create(true) else
  if FieldClass.InheritsFrom(TMFVec4f) then
    Result := TCasScriptVec4fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec2d) then
    Result := TCasScriptVec2d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec2d) then
    Result := TCasScriptVec2dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec3d) then
    Result := TCasScriptVec3d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec3d) then
    Result := TCasScriptVec3dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFVec4d) then
    Result := TCasScriptVec4d.Create(true) else
  if FieldClass.InheritsFrom(TMFVec4d) then
    Result := TCasScriptVec4dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFRotation) then
    Result := TCasScriptRotation.Create(true) else
  if FieldClass.InheritsFrom(TMFRotation) then
    Result := TCasScriptRotationArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix3f) then
    Result := TCasScriptMatrix3f.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix3f) then
    Result := TCasScriptMatrix3fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix4f) then
    Result := TCasScriptMatrix4f.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix4f) then
    Result := TCasScriptMatrix4fArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix3d) then
    Result := TCasScriptMatrix3d.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix3d) then
    Result := TCasScriptMatrix3dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFMatrix4d) then
    Result := TCasScriptMatrix4d.Create(true) else
  if FieldClass.InheritsFrom(TMFMatrix4d) then
    Result := TCasScriptMatrix4dArray.Create(true) else
  if FieldClass.InheritsFrom(TSFImage) {or
     FieldClass.InheritsFrom(TMFImage) }then
    Result := TCasScriptImage.Create(true) else
  {$endif CASTLE_SCRIPT_FPC}
  begin
    WritelnWarning('X3D', 'Note that CastleScript is not yet suitable to process values of type ' + FieldClass.X3DType);
    Result := TCasScriptFloat.Create(true);
  end;

  Result.Name := FieldOrEvent.X3DName;
end;

procedure X3DCasScriptBeforeExecute(Value: TCasScriptValue;
  FieldOrEvent: TX3DFieldOrEvent);

  procedure AssignX3DFieldValue(Field: TX3DField);
  begin
    if Field is TSFEnum then
      TCasScriptInteger(Value).Value := TSFEnum(Field).Value else
    if Field is TSFLong then
      TCasScriptInteger(Value).Value := TSFLong(Field).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFLong then
      TCasScriptInt32Array(Value).Value := TMFLong(Field).Items else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFFloat then
      TCasScriptFloat(Value).Value := TSFFloat(Field).Value else
    if Field is TSFDouble then
      TCasScriptFloat(Value).Value := TSFDouble(Field).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFFloat then
      TCasScriptSingleArray(Value).Value := TMFFloat(Field).Items else
    if Field is TMFDouble then
      TCasScriptDoubleArray(Value).Value := TMFDouble(Field).Items else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFBool then
      TCasScriptBoolean(Value).Value := TSFBool(Field).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFBool then
      TCasScriptBooleanArray(Value).Value := TMFBool(Field).Items else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFString then
      TCasScriptString(Value).Value := TSFString(Field).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFString then
      TCasScriptStringArray(Value).Value := TMFString(Field).Items else

    if Field is TSFVec2f then
      TCasScriptVec2f(Value).Value := TSFVec2f(Field).Value else
    if Field is TMFVec2f then
      TCasScriptVec2fArray(Value).Value := TMFVec2f(Field).Items else

    if Field is TSFVec3f then
      TCasScriptVec3f(Value).Value := TSFVec3f(Field).Value else
    if Field is TMFVec3f then
      TCasScriptVec3fArray(Value).Value := TMFVec3f(Field).Items else

    if Field is TSFVec4f then
      TCasScriptVec4f(Value).Value := TSFVec4f(Field).Value else
    if Field is TMFVec4f then
      TCasScriptVec4fArray(Value).Value := TMFVec4f(Field).Items else

    if Field is TSFVec2d then
      TCasScriptVec2d(Value).Value := TSFVec2d(Field).Value else
    if Field is TMFVec2d then
      TCasScriptVec2dArray(Value).Value := TMFVec2d(Field).Items else

    if Field is TSFVec3d then
      TCasScriptVec3d(Value).Value := TSFVec3d(Field).Value else
    if Field is TMFVec3d then
      TCasScriptVec3dArray(Value).Value := TMFVec3d(Field).Items else

    if Field is TSFVec4d then
      TCasScriptVec4d(Value).Value := TSFVec4d(Field).Value else
    if Field is TMFVec4d then
      TCasScriptVec4dArray(Value).Value := TMFVec4d(Field).Items else

    if Field is TSFRotation then
      TCasScriptRotation(Value).Value := TSFRotation(Field).Value else
    if Field is TMFRotation then
      TCasScriptRotationArray(Value).Value := TMFRotation(Field).Items else

    if Field is TSFMatrix3f then
      TCasScriptMatrix3f(Value).Value := TSFMatrix3f(Field).Value else
    if Field is TMFMatrix3f then
      TCasScriptMatrix3fArray(Value).Value := TMFMatrix3f(Field).Items else

    if Field is TSFMatrix4f then
      TCasScriptMatrix4f(Value).Value := TSFMatrix4f(Field).Value else
    if Field is TMFMatrix4f then
      TCasScriptMatrix4fArray(Value).Value := TMFMatrix4f(Field).Items else

    if Field is TSFMatrix3d then
      TCasScriptMatrix3d(Value).Value := TSFMatrix3d(Field).Value else
    if Field is TMFMatrix3d then
      TCasScriptMatrix3dArray(Value).Value := TMFMatrix3d(Field).Items else

    if Field is TSFMatrix4d then
      TCasScriptMatrix4d(Value).Value := TSFMatrix4d(Field).Value else
    if Field is TMFMatrix4d then
      TCasScriptMatrix4dArray(Value).Value := TMFMatrix4d(Field).Items else

    if Field is TSFImage then
      TCasScriptImage(Value).Value := TSFImage(Field).Value else
    {if Field is TMFImage then
      TCasScriptImageArray(Value).Value := TMFImage(Field).Items else}
    {$endif CASTLE_SCRIPT_FPC}

      { No sensible way to convert, just fall back to predictable 0.0. }
      TCasScriptFloat(Value).Value := 0.0;
  end;

begin
  if FieldOrEvent is TX3DField then
    AssignX3DFieldValue(TX3DField(FieldOrEvent));

  Value.ValueAssigned := false;
end;

procedure X3DCasScriptAfterExecute(Value: TCasScriptValue;
  FieldOrEvent: TX3DFieldOrEvent; var LastEventTime: TX3DTime;
  const Time: TX3DTime);

  function CreateTempField(Event: TX3DEvent): TX3DField;
  begin
    { We have to create temporary field to send.
      This is Ok, TX3DEvent.Send shortcuts would do the same thing
      after all.

      We use Event.ParentNode, Event.Name for this temporary field
      for the same reasons TX3DEvent.Send shortcuts do this:
      to get nice information in Logger node reports. }

    Result := Event.FieldClass.CreateUndefined(Event.ParentNode, false, Event.X3DName);
  end;

var
  AbortSending: boolean;
  Field: TX3DField;
  SendToEvent: TX3DEvent;
begin
  if Value.ValueAssigned then
  begin
    { calculate Field, will be set based on our current Value }

    if FieldOrEvent is TX3DEvent then
    begin
      SendToEvent := TX3DEvent(FieldOrEvent);
      Field := CreateTempField(SendToEvent);
    end else
    if (FieldOrEvent as TX3DField).Exposed then
    begin
      SendToEvent := TX3DField(FieldOrEvent).EventIn;
      Field := CreateTempField(SendToEvent);
    end else
    begin
      SendToEvent := nil;
      Assert(FieldOrEvent is TX3DField);
      Assert(not TX3DField(FieldOrEvent).Exposed);
      { For initializeOnly fields, we will set them directly. }
      Field := TX3DField(FieldOrEvent);
    end;

    AbortSending := false;

    { set Field value (and leave AbortSending as false),
      or leave Field value (and set AbortSending as true). }

    if Field is TSFEnum then
      TSFEnum(Field).Value := TCasScriptInteger(Value).Value else
    if Field is TSFLong then
      TSFLong(Field).Value := TCasScriptInteger(Value).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFLong then
      TMFLong(Field).Items := TCasScriptInt32Array(Value).Value else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFFloat then
      TSFFloat(Field).Value := TCasScriptFloat(Value).Value else
    if Field is TSFDouble then
      TSFDouble(Field).Value := TCasScriptFloat(Value).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFFloat then
      TMFFloat(Field).Items := TCasScriptSingleArray(Value).Value else
    if Field is TMFDouble then
      TMFDouble(Field).Items := TCasScriptDoubleArray(Value).Value else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFBool then
      TSFBool(Field).Value := TCasScriptBoolean(Value).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFBool then
      TMFBool(Field).Items := TCasScriptBooleanArray(Value).Value else
    {$endif CASTLE_SCRIPT_FPC}

    if Field is TSFString then
      TSFString(Field).Value := TCasScriptString(Value).Value else
    {$ifdef CASTLE_SCRIPT_FPC}
    if Field is TMFString then
      TMFString(Field).Items := TCasScriptStringArray(Value).Value else

    if Field is TSFVec2f then
      TSFVec2f(Field).Value := TCasScriptVec2f(Value).Value else
    if Field is TMFVec2f then
      TMFVec2f(Field).Items := TCasScriptVec2fArray(Value).Value else

    if Field is TSFVec3f then
      TSFVec3f(Field).Value := TCasScriptVec3f(Value).Value else
    if Field is TMFVec3f then
      TMFVec3f(Field).Items := TCasScriptVec3fArray(Value).Value else

    if Field is TSFVec4f then
      TSFVec4f(Field).Value := TCasScriptVec4f(Value).Value else
    if Field is TMFVec4f then
      TMFVec4f(Field).Items := TCasScriptVec4fArray(Value).Value else

    if Field is TSFVec2d then
      TSFVec2d(Field).Value := TCasScriptVec2d(Value).Value else
    if Field is TMFVec2d then
      TMFVec2d(Field).Items := TCasScriptVec2dArray(Value).Value else

    if Field is TSFVec3d then
      TSFVec3d(Field).Value := TCasScriptVec3d(Value).Value else
    if Field is TMFVec3d then
      TMFVec3d(Field).Items := TCasScriptVec3dArray(Value).Value else

    if Field is TSFVec4d then
      TSFVec4d(Field).Value := TCasScriptVec4d(Value).Value else
    if Field is TMFVec4d then
      TMFVec4d(Field).Items := TCasScriptVec4dArray(Value).Value else

    if Field is TSFRotation then
      TSFRotation(Field).Value := TCasScriptRotation(Value).Value else
    if Field is TMFRotation then
      TMFRotation(Field).Items := TCasScriptRotationArray(Value).Value else

    if Field is TSFMatrix3f then
      TSFMatrix3f(Field).Value := TCasScriptMatrix3f(Value).Value else
    if Field is TMFMatrix3f then
      TMFMatrix3f(Field).Items := TCasScriptMatrix3fArray(Value).Value else

    if Field is TSFMatrix4f then
      TSFMatrix4f(Field).Value := TCasScriptMatrix4f(Value).Value else
    if Field is TMFMatrix4f then
      TMFMatrix4f(Field).Items := TCasScriptMatrix4fArray(Value).Value else

    if Field is TSFMatrix3d then
      TSFMatrix3d(Field).Value := TCasScriptMatrix3d(Value).Value else
    if Field is TMFMatrix3d then
      TMFMatrix3d(Field).Items := TCasScriptMatrix3dArray(Value).Value else

    if Field is TSFMatrix4d then
      TSFMatrix4d(Field).Value := TCasScriptMatrix4d(Value).Value else
    if Field is TMFMatrix4d then
      TMFMatrix4d(Field).Items := TCasScriptMatrix4dArray(Value).Value else

    if Field is TSFImage then
      TSFImage(Field).Value := TCasScriptImage(Value).Value.MakeCopy else
    {if Field is TMFImage then
    begin
      TMFImage(Field).Items := TCasScriptImageArray(Value).Value
    end else}
    {$endif CASTLE_SCRIPT_FPC}

    begin
      { No sensible way to convert, just do nothing, don't set/send anything. }
      AbortSending := true;
    end;

    { send and free Field, for output events or exposed fields. }

    if SendToEvent <> nil then
    begin
      if not AbortSending then
      begin
        if Time > LastEventTime then
        begin
          LastEventTime := Time;
          SendToEvent.Send(Field, Time);
        end else
          WritelnLog('X3D Script', Format(
            'Sending event %s from Script ignored at <= timestamp (%f, while last event was on %f). Potential loop avoided',
            [ SendToEvent.X3DName, Time.Seconds, LastEventTime.Seconds ]));
      end;
      FreeAndNil(Field);
    end;

    { Theoretically, we should call Scene.InternalChangedField now
      if FieldOrEvent was initializeOnly field (and not AbortSending).
      But in this case, we know FieldOrEvent comes from a Script node,
      and in this situation InternalChangedField doesn't do anything anyway. }

    { This is needed for TCasScriptX3DValueList.AfterExecute trick.
      We handled this change, so we mark it by ValueAssigned = false. }
    Value.ValueAssigned := false;
  end;
end;

{ TCasScriptX3DValueList -------------------------------------------------- }

constructor TCasScriptX3DValueList.Create(AFreeObjects: boolean);
begin
  inherited;
  FFieldOrEvents := TX3DFieldOrEventList.Create(false);
  FLastEventTimes := TX3DTimeList.Create;
end;

destructor TCasScriptX3DValueList.Destroy;
begin
  SysUtils.FreeAndNil(FFieldOrEvents);
  SysUtils.FreeAndNil(FLastEventTimes);
  inherited;
end;

procedure TCasScriptX3DValueList.Add(FieldOrEvent: TX3DFieldOrEvent);
begin
  inherited Add(X3DCasScriptCreateValue(FieldOrEvent));
  FieldOrEvents.Add(FieldOrEvent);
  FLastEventTimes.Add(TX3DTime.Oldest);
end;

procedure TCasScriptX3DValueList.BeforeExecute;
var
  I: Integer;
begin
  if not InsideAfterExecute then
  begin
    for I := 0 to Count - 1 do
      X3DCasScriptBeforeExecute(Items[I], FieldOrEvents[I]);
  end;
end;

procedure TCasScriptX3DValueList.AfterExecute(const Time: TX3DTime);
var
  I: Integer;
  WasSomeValueAssigned: boolean;
begin
  if not InsideAfterExecute then
  begin
    { Note that X3DCasScriptAfterExecute may send events when given
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
      Actually, we also don't want to reinitialize the CastleScript field
      from VRML/X3D field value, to not lose new value.
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
          X3DCasScriptAfterExecute(Items[I], FieldOrEvents[I], FLastEventTimes.L[I], Time);
        end;
      until not WasSomeValueAssigned;
    finally InsideAfterExecute := false end;
  end;
end;

procedure TCasScriptX3DValueList.ResetLastEventTimes;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FLastEventTimes.L[I] := TX3DTime.Oldest;
end;

end.
