{%MainUnit x3dfields.pas}
{
  Copyright 2007-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { X3D event. }
  TX3DEvent = class(TX3DFieldOrEvent)
  strict private
    type
      TX3DEventReceiveList = class({$ifdef FPC}specialize{$endif} TList<TX3DEventReceive>)
      public
        { Call all functions. }
        procedure ExecuteAll(const Event: TX3DEvent; const Value: TX3DField;
          const Time: TX3DTime);
      end;

    var
    FFieldClass: TX3DFieldClass;
    FInEvent: boolean;
    { To avoid allocating memory for FNotifications list in the most-common case
      when 0 or 1 notification is enough, store 1st notification in special
      field instead. }
    FFirstNotification: TX3DEventReceive;
    FNotifications: TX3DEventReceiveList;
    FParentExposedField: TX3DField;
    Temp: TX3DField;
    TempUsed: boolean;

    SendLevel: Cardinal;
    NotificationsToRemove: TX3DEventReceiveList;

    procedure RemoveNotificationNow(const Notification: TX3DEventReceive);
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AFieldClass: TX3DFieldClass; const AInEvent: boolean);
    destructor Destroy; override;

    { Internal helper utility for SendXxx implementations. @exclude }
    function CreateTemp(const PartialSend: TPartialSend): TX3DField;
    { Internal helper utility for SendXxx implementations. @exclude }
    procedure FreeTemp(const Field: TX3DField);

    property FieldClass: TX3DFieldClass read FFieldClass;

    { @abstract(Is it "in" or "out" event ?) }
    property InEvent: boolean read FInEvent;

    { This only reads (optional) "IS" clause of the event, as may occur
      in VRML nodeBodyStatement. }
    procedure Parse(Lexer: TX3DLexer);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function SaveToXml: TSaveToXmlMethod; override;

    { If this event is an exposed event belonging to some field,
      this references parent field. Otherwise it's @nil. }
    property ParentExposedField: TX3DField
      read FParentExposedField write FParentExposedField;

    { Send a value to event. For input fields, this is used by routes, scripts
      etc. to send an event to a field. For output fields, this is used by
      node itself to send event to routes, scripts etc.

      Field must be non-nil, of class FieldClass.

      The Field instance doesn't become owned in any way by the TX3DEvent.
      That is, it's the caller responsibility to free the Field instance
      at any comfortable time, possibly right after calling Send.

      Overloaded versions without explicit Time parameter just take time
      from ParentNode.Scene.GetTime. If ParentNode is @nil
      (which should not happen with normal fields within nodes)
      or if ParentNode.Scene is @nil (which may happen only if events
      processing is not turned on, that is TCastleSceneCore.ProcessEvents is @false)
      then event is not send. }
    procedure Send(Field: TX3DField; const Time: TX3DTime); overload;
    procedure Send(Field: TX3DField); overload;

    { Add notification about receiving new value. For input events,
      these are used by node itself to listen to events send to it.
      For output events, these are used by routes, scripts to listen
      to events occuring.

      When you handle this callback to send another X3D event,
      you should pass around the same @code(Time) value.
      This makes sure that X3D route loop detection mechanism works fine.
      For example, here's how handling TimeSensor.elapsedTime_receive event
      sends fraction_changed event:

      @longCode(#
        procedure TTimeSensorNode.EventElapsedTimeReceive(
          const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
        var
          Fraction: Single;
        begin
          if FdEnabled.Value then
          begin
            Fraction := ...; // doesn't matter here how this is calculated
            // Pass "Time" parameter further by a call below
            Eventfraction_changed.Send(Fraction, Time);
          end;
        end;
      #)

      All callbacks registered here are simply called by @link(Send) method. }
    procedure AddNotification(const Notification: TX3DEventReceive);

    { Remove notification (previously added by @link(AddNotification))
      about receiving new value.

      This is guaranteed to work correctly even if we're right now
      in the middle of this event's processing (which means that we may
      be iterating over a list of notifications).
      The handler may be actually removed a bit later (when it's safe).

      Note that adding a notification two-times makes it called two times.
      Removing a notification removes one copy of a notification,
      so it should then be removed two-times too. }
    procedure RemoveNotification(const Notification: TX3DEventReceive);

    { @abstract(Is anything actually listening on this events @link(Send)?)

      Sometimes, even preparing a value to Send is quite time-consuming
      (example: CoordinateInterpolator, where a long MFVec3f value
      has to be computer). Then checking SendNeeded is useful:
      if SendNeeded = @false, you know that there's no point in preparing
      Value to send, because actually Send will do nothing.

      For example, event out to which no ROUTE is connected, and no Script
      can listen to it.

      For now, this simply returns whether any callback is registered
      by AddNotification. }
    function SendNeeded: boolean;

    { Assign from another event. The FieldClass and InEvent is copied.

      The notifications (from @link(AddNotification))
      and ParentExposedField are not copied, since they shouldn't
      be copied so easily (ParentExposedField is related to hierarchy
      of containers, copying notification would be surprising to the
      notification listener). }
    procedure Assign(Source: TPersistent); override;
  end;

  TX3DEventList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DEvent>)
  strict private
    function GetByName(const AName: string): TX3DEvent;
  public
    { Access event by name.
      Raises EX3DNotFound if the given Name doesn't exist. }
    property ByName[const AName: string]: TX3DEvent read GetByName;

    { Find event index on the list. -1 if not found. }
    function IndexOfName(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DEvent.TX3DEventReceiveList -------------------------------------------------- }

procedure TX3DEvent.TX3DEventReceiveList.ExecuteAll(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I](Event, Value, Time);
end;

{ TX3DEvent ----------------------------------------------------------------- }

constructor TX3DEvent.Create(AParentNode: TX3DFileItem;
  const AName: string; const AFieldClass: TX3DFieldClass;
  const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName);
  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
end;

destructor TX3DEvent.Destroy;
begin
  FreeAndNil(Temp);
  FreeAndNil(NotificationsToRemove);
  FreeAndNil(FNotifications);
  inherited;
end;

procedure TX3DEvent.Parse(Lexer: TX3DLexer);
begin
  ParseIsClause(Lexer);
end;

procedure TX3DEvent.SaveToStream(Writer: TX3DWriter);
begin
  { saves nothing. IS clauses are saved separately by SaveToStreamIsClauses }
end;

function TX3DEvent.SaveToXml: TSaveToXmlMethod;
begin
  Result := sxNone;
end;

procedure TX3DEvent.Send(Field: TX3DField; const Time: TX3DTime);

  procedure RemoveNotifications;
  var
    I: Integer;
  begin
    for I := 0 to NotificationsToRemove.Count - 1 do
      RemoveNotificationNow(NotificationsToRemove.Items[I]);
    // clear NotificationsToRemove
    NotificationsToRemove.Count := 0;
  end;

begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);

  Inc(SendLevel);
  try
    if Assigned(FFirstNotification) then
      FFirstNotification(Self, Field, Time);
    if FNotifications <> nil then
      FNotifications.ExecuteAll(Self, Field, Time);
  finally
    Dec(SendLevel);
    if (NotificationsToRemove <> nil) and
       (NotificationsToRemove.Count <> 0) and
       (SendLevel = 0) then
      RemoveNotifications;
  end;
end;

procedure TX3DEvent.Send(Field: TX3DField);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Field, TX3DNode(ParentNode).Scene.NextEventTime);
end;

function TX3DEvent.SendNeeded: boolean;
begin
  Result :=
    Assigned(FFirstNotification) or
    ((FNotifications <> nil) and (FNotifications.Count <> 0));
end;

procedure TX3DEvent.Assign(Source: TPersistent);
begin
  if Source is TX3DEvent then
  begin
    FieldOrEventAssignCommon(TX3DEvent(Source));

    FFieldClass := TX3DEvent(Source).FieldClass;
    FInEvent := TX3DEvent(Source).InEvent;
  end;
end;

function TX3DEvent.CreateTemp(const PartialSend: TPartialSend): TX3DField;
begin
  if not TempUsed then
  begin
    if Temp = nil then
      Temp := FieldClass.CreateUndefined(ParentNode, false, X3DName);
    Result := Temp;
    TempUsed := true;
  end else
    Result := FieldClass.CreateUndefined(ParentNode, false, X3DName);
  Result.InternalPartialSend := PartialSend;
end;

procedure TX3DEvent.FreeTemp(const Field: TX3DField);
begin
  if Field = Temp then
    TempUsed := false else
    Field.Free;
end;

procedure TX3DEvent.AddNotification(const Notification: TX3DEventReceive);
begin
  if not Assigned(FFirstNotification) then
  begin
    FFirstNotification := Notification;
  end else
  begin
    { create FNotifications on-demand, as in cases of models with a large number
      of nodes (or using prototypes a lot, which also creates a large number of nodes)
      we have a lot of TX3DEvent instances, and they eat a significant part of memory }
    if FNotifications = nil then
      FNotifications := TX3DEventReceiveList.Create;
    FNotifications.Add(Notification);
  end;
end;

procedure TX3DEvent.RemoveNotificationNow(const Notification: TX3DEventReceive);
begin
  { The "else" below is important to remove only the first notification present,
    if you added multiple notifications. }
  if SameMethods(TMethod(FFirstNotification), TMethod(Notification)) then
    FFirstNotification := nil
  else
  if FNotifications <> nil then
    FNotifications.Remove(Notification);
end;

procedure TX3DEvent.RemoveNotification(const Notification: TX3DEventReceive);
begin
  if SendLevel = 0 then
  begin
    RemoveNotificationNow(Notification);
  end else
  begin
    if NotificationsToRemove = nil then
      NotificationsToRemove := TX3DEventReceiveList.Create;
    NotificationsToRemove.Add(Notification);
  end;
end;

{ TX3DEventList ------------------------------------------------------------ }

function TX3DEventList.IndexOfName(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].IsName(Name) then Exit;
  Result := -1;
end;

function TX3DEventList.GetByName(const AName: string): TX3DEvent;
var
  I: integer;
begin
  I := IndexOfName(AName);
  if I <> -1 then
    Result := Items[I] else
    raise EX3DNotFound.CreateFmt('Event name "%s" not found', [AName]);
end;

{$endif read_implementation}
