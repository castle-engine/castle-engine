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
  TKamScriptVRMLValue = class(TKamScriptFloat)
  private
    FFieldOrEvent: TVRMLFieldOrEvent;
  public
    { TKamScriptVRMLValue constructor.

      We reintroduce this constructor, hiding virtual constructor for
      TKamScriptValue, which means that you cannot construct this class
      by virtual constructor in descendant. This is Ok, as you must
      provide FieldOrEvent when constructing this. This class cannot
      be created by calculating expression (like CreateValueIfNeeded). }
    constructor Create(AFieldOrEvent: TVRMLFieldOrEvent); reintroduce;

    property FieldOrEvent: TVRMLFieldOrEvent read FFieldOrEvent;

    { Do common things before script with this variable is executed.
      This resets ValueAssigned (will be used in AfterExecute),
      and sets current variable's value from FieldOrEvent (if this is a field). }
    procedure BeforeExecute;

    { Do common things after script with this variable is executed.
      This checks ValueAssigned, and propagates value change to appropriate
      field/event, sending event/setting field. }
    procedure AfterExecute;
  end;

  TKamScriptVRMLValuesList = class(TKamScriptValuesList)
  private
    function GetItems(I: Integer): TKamScriptVRMLValue;
  public
    property Items[I: Integer]: TKamScriptVRMLValue read GetItems; default;
    procedure BeforeExecute;
    procedure AfterExecute;
  end;

{$undef read_interface}

implementation

uses SysUtils, KambiTimeUtils, VRMLNodes, VRMLScene;

{$define read_implementation}

{ TKamScriptVRMLValue -------------------------------------------------------- }

{ TODO: For now we descend TKamScriptVRMLValue from TKamScriptFloat.
  This means that every VRML field value is encoded/decoded
  as a simple Float... Clearly, this just doesn't work for many types
  (string, image, vectors, matrices), and for other types may be suboptimal/
  non-precise (single,double,int,bool).

  Intention was to have other TKamScriptValue descendants for
  various field classes. }

constructor TKamScriptVRMLValue.Create(AFieldOrEvent: TVRMLFieldOrEvent);
begin
  inherited Create;
  FFieldOrEvent := AFieldOrEvent;
  Name := FieldOrEvent.Name;
end;

procedure TKamScriptVRMLValue.BeforeExecute;

  procedure AssignVRMLFieldValue(Field: TVRMLField);
  begin
    if Field is TSFFloat then
      Value := TSFFloat(Field).Value else
    if Field is TSFDouble then
      Value := TSFDouble(Field).Value else
    if Field is TSFBool then
      AsBoolean := TSFBool(Field).Value else
    if Field is TSFEnum then
      Value := TSFEnum(Field).Value else
    if Field is TSFLong then
      Value := TSFLong(Field).Value else

    if (Field is TMFFloat) and
       (TMFFloat(Field).Items.Count = 1) then
      Value := TMFFloat(Field).Items.Items[0] else
    if (Field is TMFDouble) and
       (TMFDouble(Field).Items.Count = 1) then
      Value := TMFDouble(Field).Items.Items[0] else
    if (Field is TMFBool) and
       (TMFBool(Field).Items.Count = 1) then
      AsBoolean := TMFBool(Field).Items.Items[0] else
    if (Field is TMFLong) and
       (TMFLong(Field).Items.Count = 1) then
      Value := TMFLong(Field).Items.Items[0] else

      { No sensible way to convert, just fall back to 0.0. }
      Value := 0.0;
  end;

begin
  ValueAssigned := false;

  if FieldOrEvent is TVRMLField then
    AssignVRMLFieldValue(TVRMLField(FieldOrEvent));
end;

procedure TKamScriptVRMLValue.AfterExecute;

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

var
  AbortSending: boolean;
  Field: TVRMLField;
  Timestamp: TKamTime;
begin
  if ValueAssigned then
  begin
    { calculate Field, will be set based on our current Value }

    if (FieldOrEvent is TVRMLEvent) or
       (FieldOrEvent as TVRMLField).Exposed then
    begin
      { We have to create temporary field to send.
        This is Ok, TVRMLEvent.Send shortcuts would do the same thing
        after all.

        We use Event.ParentNode, Event.Name for this temporary field
        for the same reasons TVRMLEvent.Send shortcuts do this:
        to get nice information in Logger node reports. }

      Field := TVRMLEvent(FieldOrEvent).FieldClass.CreateUndefined(
        FieldOrEvent.ParentNode, FieldOrEvent.Name);
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

    if Field is TSFFloat then
      TSFFloat(Field).Value := Value else
    if Field is TSFDouble then
      TSFDouble(Field).Value := Value else
    if Field is TSFBool then
      TSFBool(Field).Value := AsBoolean else
    if Field is TSFEnum then
      TSFEnum(Field).Value := Round(Value) else
    if Field is TSFLong then
      TSFLong(Field).Value := Round(Value) else

    if Field is TMFFloat then
    begin
      TMFFloat(Field).Items.Count := 1;
      TMFFloat(Field).Items.Items[0] := Value;
    end else
    if Field is TMFDouble then
    begin
      TMFDouble(Field).Items.Count := 1;
      TMFDouble(Field).Items.Items[0] := Value;
    end else
    if Field is TMFBool then
    begin
      TMFBool(Field).Items.Count := 1;
      TMFBool(Field).Items.Items[0] := AsBoolean;
    end else
    if Field is TMFLong then
    begin
      TMFLong(Field).Items.Count := 1;
      TMFLong(Field).Items.Items[0] := Round(Value);
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
  end;
end;

{ TKamScriptVRMLValuesList -------------------------------------------------- }

function TKamScriptVRMLValuesList.GetItems(I: Integer): TKamScriptVRMLValue;
begin
  Result := (inherited Items[I]) as TKamScriptVRMLValue;
end;

procedure TKamScriptVRMLValuesList.BeforeExecute;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BeforeExecute;
end;

procedure TKamScriptVRMLValuesList.AfterExecute;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].AfterExecute;
end;

end.
