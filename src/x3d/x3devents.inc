{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Callback type for @link(TX3DEvent.OnReceive). }
  TX3DEventReceive = procedure (Event: TX3DEvent; Value: TX3DField;
    const Time: TX3DTime) of object;

  TX3DEventReceiveList = class(specialize TGenericStructList<TX3DEventReceive>)
  public
    { Call all functions. }
    procedure ExecuteAll(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  end;

  { X3D event. }
  TX3DEvent = class(TX3DFieldOrEvent)
  private
    FFieldClass: TX3DFieldClass;
    FInEvent: boolean;
    FOnReceive: TX3DEventReceiveList;
    FParentExposedField: TX3DField;
    Temp: TX3DField;
    TempUsed: boolean;

    SendLevel: Cardinal;
    HandlersToRemove: TX3DEventReceiveList;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const AFieldClass: TX3DFieldClass; const AInEvent: boolean);
    destructor Destroy; override;

    { Internal helper utility for SendXxx implementations. }
    function CreateTemp: TX3DField;
    { Internal helper utility for SendXxx implementations. }
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
    procedure Send(Field: TX3DField; const Time: TX3DTime);
    procedure Send(Field: TX3DField);

    { Notifications about receiving an item. For input fields,
      these are used by node itself to listen to events send to it.
      For output fields, these are used by routes, scripts to listen
      to events occuring.

      When you handle this callback to send another X3D event,
      you should pass around the same @code(Time) value.
      This makes sure that X3D route loop detection mechanism works fine.
      For example, here's how handling TimeSensor.elapsedTime_receive event
      sends fraction_changed event:

      @longCode(#
        procedure TTimeSensorNode.EventElapsedTimeReceive(
          Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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

      All callbacks here are simply called by @link(Send) method. }
    property OnReceive: TX3DEventReceiveList read FOnReceive;

    { Safely remove a callback from OnReceive list.

      Contrary to direct @code(OnReceive.Remove) call, this works correctly
      even if we're right now in the middle of this event's processing.
      In the latter case, the handler will be actually removed with some delay
      (when it's safe). }
    procedure RemoveHandler(Handler: TX3DEventReceive);

    { @abstract(Is anything actually listening on this events @link(Send)?)

      Sometimes, even preparing a value to Send is quite time-consuming
      (example: CoordinateInterpolator, where a long MFVec3f value
      has to be computer). Then checking SendNeeded is useful:
      if SendNeeded = @false, you know that there's no point in preparing
      Value to send, because actually Send will do nothing.

      For example, event out to which no ROUTE is connected, and no Script
      can listen to it.

      For now, this simply returns whether any OnReceive callback is registered. }

    function SendNeeded: boolean;

    { Assign from another event.
      Note that FieldClass, InEvent is copied.

      OnReceive, ParentExposedField are not copied, since they shouldn't
      be copied so easily (ParentExposedField is related to hierarchy
      of containers, and OnReceive requires some knowledge of the caller
      when his callback is copied). }
    procedure Assign(Source: TPersistent); override;
  end;

  TX3DEventList = class(specialize TFPGObjectList<TX3DEvent>)
  private
    function GetByName(const AName: string): TX3DEvent;
  public
    { Access event by name.
      Raises EX3DNotFound if the given Name doesn't exist. }
    property ByName[const AName: string]: TX3DEvent read GetByName;

    { Find event index on the list. -1 if not found. }
    function IndexOf(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DEventReceiveList -------------------------------------------------- }

procedure TX3DEventReceiveList.ExecuteAll(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I](Event, Value, Time);
end;

{ TX3DEvent ----------------------------------------------------------------- }

constructor TX3DEvent.Create(AParentNode: TX3DFileItem;
  const AName: string; const AFieldClass: TX3DFieldClass;
  const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName);

  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
  FOnReceive := TX3DEventReceiveList.Create;
end;

destructor TX3DEvent.Destroy;
begin
  FreeAndNil(Temp);
  FreeAndNil(HandlersToRemove);
  FreeAndNil(FOnReceive);
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

  procedure RemoveHandlers;
  var
    I: Integer;
  begin
    for I := 0 to HandlersToRemove.Count - 1 do
      OnReceive.Remove(HandlersToRemove.L[I]);
    HandlersToRemove.Count := 0;
  end;

begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);

  Inc(SendLevel);
  try
    FOnReceive.ExecuteAll(Self, Field, Time);
  finally
    Dec(SendLevel);
    if (HandlersToRemove <> nil) and
       (HandlersToRemove.Count <> 0) and
       (SendLevel = 0) then
      RemoveHandlers;
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
  Result := OnReceive.Count <> 0;
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

function TX3DEvent.CreateTemp: TX3DField;
begin
  if not TempUsed then
  begin
    if Temp = nil then
      Temp := FieldClass.CreateUndefined(ParentNode, X3DName, false);
    Result := Temp;
    TempUsed := true;
  end else
    Result := FieldClass.CreateUndefined(ParentNode, X3DName, false);
end;

procedure TX3DEvent.FreeTemp(const Field: TX3DField);
begin
  if Field = Temp then
    TempUsed := false else
    Field.Free;
end;

procedure TX3DEvent.RemoveHandler(Handler: TX3DEventReceive);
begin
  if SendLevel = 0 then
    OnReceive.Remove(Handler) else
  begin
    if HandlersToRemove = nil then
      HandlersToRemove := TX3DEventReceiveList.Create;
    HandlersToRemove.Add(Handler);
  end;
end;

{ TX3DEventList ------------------------------------------------------------ }

function TX3DEventList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].IsName(Name) then Exit;
  Result := -1;
end;

function TX3DEventList.GetByName(const AName: string): TX3DEvent;
var
  I: integer;
begin
  I := IndexOf(AName);
  if I <> -1 then
    Result := Items[I] else
    raise EX3DNotFound.CreateFmt('Event name "%s" not found', [AName]);
end;

{$endif read_implementation}
