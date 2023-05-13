{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Route makes a connection between two X3D events,
    making the @italic(destination) event occur when the @italic(source) event happened. }
  TX3DRoute = class(TX3DFileItem)
  strict private
    FSourceNode: TX3DNode;
    FSourceEvent: TX3DEvent;

    FDestinationNode: TX3DNode;
    FDestinationEvent: TX3DEvent;

    LastEventTime: TX3DTime;
    FInternal: boolean;

    procedure DestructionNotification(const Node: TX3DNode);

    procedure UnsetEnding(
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean;
      const RemoveFromDestructionNotification: boolean = true);

    procedure SetEnding(const NodeName, FieldOrEventName: string;
      const Reader: TX3DReaderNames;
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean);

    { Set Event, based on FieldOrEvent (may be actual event,
      or exposed field containing it) and DestEnding.
      Assumes that Event is clear on enter (cleared by UnsetEnding). }
    procedure SetEndingInternal(
      const Node: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
      var Event: TX3DEvent;
      const DestEnding: boolean);

    procedure SetEndingDirectly(
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
      var Node: TX3DNode; var Event: TX3DEvent;
      const DestEnding: boolean);

    procedure EventReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    constructor Create;
    destructor Destroy; override;

    { Source event properties. Either all three are @nil, or:

      @unorderedList(
        @item(SourceEvent is assigned, meaning is self-explanatory.

          Note: if you want to get it's exposed field, remember this
          is available in SourceEvent.ParentExposedField.)

        @item(SourceNode must also be assigned and
          this must be the node enclosing SourceEvent. That is, the node that
          has SourceEvent as one of explicit (on TX3DNode.Events list) or
          implicit (exposed by some field) event.)
      )

      @groupBegin }
    property SourceNode: TX3DNode read FSourceNode;
    property SourceEvent: TX3DEvent read FSourceEvent;
    { @groupEnd }

    { Destination event properties.
      Analogous to SourceEvent, SourceNode.

      @groupBegin }
    property DestinationNode: TX3DNode read FDestinationNode;
    property DestinationEvent: TX3DEvent read FDestinationEvent;
    { @groupEnd }

    { Set source/destination of the route.

      This does everything that VRML parser should
      do when parsed VRML route. It looks for given node name
      (in Names.Nodes, then Names.Imported),
      then it looks for field/event within this node,
      and if everything is successful --- sets route properties.

      If something goes wrong, WritelnWarning is generated
      and route ending is left unset.

      @groupBegin }
    procedure SetSource(
      const SourceNodeName, SourceFieldOrEventName: string;
      Reader: TX3DReaderNames);

    procedure SetDestination(
      const DestinationNodeName, DestinationFieldOrEventName: string;
      Reader: TX3DReaderNames);
    { @groupEnd }

    { These set source/destination of the route in more direct way.

      FieldOrEvent is used to set SourceEvent (or DestinationEvent).
      FieldOrEvent may be the actual event to set,
      or exposed field containing this event.

      You specify explictly NewNode, which is not checked in any way.
      We don't check whether it exists, whether it contains given
      FieldOrEvent,  etc. --- you have to guarantee this yourself.
      Also, remember that normal SetSource actually look for events
      inside PrototypeInstanceSourceNode, if exists --- if you want this,
      you have to do this yourself when using these SetXxxDirectly.
      It is used to set SourceNode (or DestinationNode).
      Overloaded versions that don't take NewNode parameter just assume
      that NewNode can be taken from FieldOrEvent.ParentNode.

      @groupBegin }
    procedure SetSourceDirectly(
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent); overload;

    procedure SetDestinationDirectly(
      const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent); overload;

    procedure SetSourceDirectly(const FieldOrEvent: TX3DFieldOrEvent); overload;
    procedure SetDestinationDirectly(const FieldOrEvent: TX3DFieldOrEvent); overload;
    { @groupEnd }

    { Parse the route (classic VRML encoding).
      Implementation should be able to safely assume that current token
      is ROUTE. }
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the route (XML encoding).
      Given Element here must have TagName = 'ROUTE'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    { Save a ROUTE to VRML file.

      Will generate WritelnWarning when route cannot be saved.
      This can happen when SourceNode or SourceEvent
      or DestinationNode or DestinationEvent are @nil.
      Also, if SourceNode and DestinationNode are without a name,
      or the name is not currently bound in Writer.NodeNames.
    }
    procedure SaveToStream(Writer: TX3DWriter); override;

    { Clear the memory when the last event passed through this route.
      Route must remember such thing, to avoid loops in routes.
      This is following VRML 2.0 / X3D specifications, that explicitly
      say that only one event per ROUTE per timestamp is allowed.

      Use ResetLastEventTime when you really want to reset this memory.
      In practice, this should be used only by TCastleSceneCore.ResetTime
      implementation. }
    procedure ResetLastEventTime;

    { Internal routes are created by PROTO expansion code, which
      needs to create internal routes to implement "IS" clauses for events.

      These routes work exactly like normal routes, except:
      @unorderedList(
        @item(They are not saved to file (SaveToStream will ignore
          internal route).)
        @item(It's allowed (in fact, this will always happen for current
          internal routes) to route one input event to another input event.)
      )
    }
    property Internal: boolean read FInternal write FInternal default false;

    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DRoute;
  end;

  TX3DRouteList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DRoute>);

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DRoute ----------------------------------------------------------------- }

constructor TX3DRoute.Create;
begin
  inherited;
  ResetLastEventTime;
end;

destructor TX3DRoute.Destroy;
begin
  { We have to unset, to call
    RemoveDestructionNotification(...) on our nodes before
    we get destroyed. Otherwise nodes would have invalid references
    on TX3DNode.FDestructionNotifications list. }

  UnsetEnding(FSourceNode     , FSourceEvent     , false);
  UnsetEnding(FDestinationNode, FDestinationEvent, true);
  inherited;
end;

procedure TX3DRoute.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  SourceNodeName, SourceEventName: string;
  DestinationNodeName, DestinationEventName: string;
begin
  { We don't use NextTokenForceVTName here, as then the dot "."
    is treated like part the VRML name. So this assumes that VRML node names
    are really correct VRML names. }

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  SourceNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  SourceEventName := Lexer.TokenX3DName;

  Lexer.NextToken;
  Lexer.CheckTokenIsKeyword(vkTO);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML node name');
  DestinationNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'VRML field/event name');
  DestinationEventName := Lexer.TokenX3DName;

  Lexer.NextToken;

  SetSource     (SourceNodeName     , SourceEventName     , Reader);
  SetDestination(DestinationNodeName, DestinationEventName, Reader);
end;

procedure TX3DRoute.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

  function RequiredX3DName(const AttrName: string): string;
  begin
    if not XMLAttributeX3DName(Element, AttrName, Result) then
    begin
      WritelnWarning('X3D', 'Missing ROUTE ' + AttrName + ' attribute');
      Result := '';
    end;
  end;

var
  SourceNodeName, SourceEventName: string;
  DestinationNodeName, DestinationEventName: string;
begin
  SourceNodeName := RequiredX3DName('fromNode');
  SourceEventName := RequiredX3DName('fromField');
  DestinationNodeName := RequiredX3DName('toNode');
  DestinationEventName := RequiredX3DName('toField');

  SetSource     (SourceNodeName     , SourceEventName     , Reader);
  SetDestination(DestinationNodeName, DestinationEventName, Reader);
end;

procedure TX3DRoute.UnsetEnding(
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean;
  const RemoveFromDestructionNotification: boolean);
begin
  if (Event <> nil) and (not DestEnding) then
    Event.RemoveNotification({$ifdef FPC}@{$endif} EventReceive);

  if Node <> nil then
  begin
    if RemoveFromDestructionNotification then
      Node.RemoveDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification);
    Node := nil;
  end;

  Event := nil;
end;

procedure TX3DRoute.EventReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  Assert(Event = SourceEvent);

  { Follow ROUTE only when LastEventTime is older than current Time.
    This avoids loops in ROUTEs, following VRML / X3D specs. }
  if Time > LastEventTime then
  begin
    LastEventTime := Time;
    if DestinationEvent <> nil then
      DestinationEvent.Send(Value, Time);
  end else
    WritelnLog('X3D', Format(
      'Route %s.%s -> %s.%s ignored another event at <= timestamp (%f.%d, while last event was on %f.%d). Potential routes loop avoided',
      [ SourceNode.X3DName, SourceEvent.X3DName,
        DestinationNode.X3DName, DestinationEvent.X3DName,
        Time.Seconds, Time.PlusTicks,
        LastEventTime.Seconds, LastEventTime.PlusTicks ]));
end;

procedure TX3DRoute.ResetLastEventTime;
begin
  LastEventTime := TX3DTime.Oldest;
end;

type
  ERouteSetEndingError = class(EX3DError);

const
  DestEndingNames: array [boolean] of string =
  ('source', 'destination');

procedure TX3DRoute.SetEndingInternal(
  const Node: TX3DNode;
  const FieldOrEvent: TX3DFieldOrEvent;
  var Event: TX3DEvent;
  const DestEnding: boolean);
var
  ExposedField: TX3DField;
begin
  if FieldOrEvent is TX3DField then
  begin
    ExposedField := TX3DField(FieldOrEvent);
    if not ExposedField.Exposed then
      raise ERouteSetEndingError.CreateFmt('Route %s specifies field "%s" (for node "%s"), but this is not an exposed field (cannot generate/receive events)',
        [ DestEndingNames[DestEnding], FieldOrEvent.X3DName, Node.X3DName ]);
    Event := ExposedField.ExposedEvents[DestEnding];
  end else
  begin
    Assert(FieldOrEvent is TX3DEvent);
    Event := TX3DEvent(FieldOrEvent);
  end;

  if (not Internal) and (Event.InEvent <> DestEnding) then
  begin
    if DestEnding then
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: destination of the route (%s, type %s) can only be output event',
        [ Event.X3DName, Event.FieldClass.X3DType ]) else
      raise ERouteSetEndingError.CreateFmt('Route uses wrong event: source of the route (%s, type %s) can only be input event',
        [ Event.X3DName, Event.FieldClass.X3DType ]);
  end;

  if (SourceEvent <> nil) and
     (DestinationEvent <> nil) and
     (SourceEvent.FieldClass <> DestinationEvent.FieldClass) and
     { destination field can be XFAny (for some InstantReality nodes) as an exception. }
     (not (DestinationEvent.FieldClass = TX3DField)) then
    raise ERouteSetEndingError.CreateFmt('Route has different event types for source (%s, type %s) and destination (%s, type %s)',
      [ SourceEvent     .X3DName, SourceEvent     .FieldClass.X3DType,
        DestinationEvent.X3DName, DestinationEvent.FieldClass.X3DType ]);

  if (Event <> nil) and (not DestEnding) then
    Event.AddNotification({$ifdef FPC}@{$endif} EventReceive);
end;

procedure TX3DRoute.SetEnding(const NodeName, FieldOrEventName: string;
  const Reader: TX3DReaderNames;
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean);
var
  N: TX3DNode;
  FieldOrEvent: TX3DFieldOrEvent;
  IgnoreNodeFinished: boolean;
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    N := Reader.Bound(NodeName, IgnoreNodeFinished, false);
    if N = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s node name "%s" not found',
        [ DestEndingNames[DestEnding], NodeName ]);

    Node := N;
    if Node.PrototypeInstanceSourceNode <> nil then
    begin
      Node := Node.PrototypeInstanceSourceNode;
      { Actually, SearchNode.PrototypeInstanceSourceNode may also be non-nil,
        in case we have nested prototype. But it doesn't matter,
        that is we don't want to go all the way down to find the
        final PrototypeInstanceSourceNode --- we only want to see the first
        PrototypeInstanceSourceNode. }
    end;

    Node.AddDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification);

    FieldOrEvent := Node.FieldOrEvent(FieldOrEventName);
    if FieldOrEvent = nil then
      raise ERouteSetEndingError.CreateFmt('Route %s field/event name "%s" (for node "%s", type "%s") not found',
        [ DestEndingNames[DestEnding], FieldOrEventName, NodeName, Node.X3DType ]);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      WritelnWarning('X3D', E.Message);
    end;
  end;
end;

procedure TX3DRoute.SetSource(
  const SourceNodeName, SourceFieldOrEventName: string;
  Reader: TX3DReaderNames);
begin
  SetEnding(SourceNodeName, SourceFieldOrEventName,
    Reader, FSourceNode, FSourceEvent, false);
end;

procedure TX3DRoute.SetDestination(
  const DestinationNodeName, DestinationFieldOrEventName: string;
  Reader: TX3DReaderNames);
begin
  SetEnding(DestinationNodeName, DestinationFieldOrEventName,
    Reader, FDestinationNode, FDestinationEvent, true);
end;

procedure TX3DRoute.SetEndingDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent;
  var Node: TX3DNode; var Event: TX3DEvent;
  const DestEnding: boolean);
begin
  UnsetEnding(Node, Event, DestEnding);

  try
    Node := NewNode;
    Node.AddDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification);

    SetEndingInternal(Node, FieldOrEvent, Event, DestEnding);
  except
    on E: ERouteSetEndingError do
    begin
      UnsetEnding(Node, Event, DestEnding);
      WritelnWarning('X3D', E.Message);
    end;
  end;
end;

procedure TX3DRoute.SetSourceDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FSourceNode, FSourceEvent,
    false);
end;

procedure TX3DRoute.SetDestinationDirectly(
  const NewNode: TX3DNode; const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetEndingDirectly(NewNode, FieldOrEvent,
    FDestinationNode, FDestinationEvent,
    true);
end;

procedure TX3DRoute.SetSourceDirectly(const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetSourceDirectly(FieldOrEvent.ParentNode as TX3DNode, FieldOrEvent);
end;

procedure TX3DRoute.SetDestinationDirectly(
  const FieldOrEvent: TX3DFieldOrEvent);
begin
  SetDestinationDirectly(FieldOrEvent.ParentNode as TX3DNode, FieldOrEvent);
end;

type
  EX3DRouteSaveError = class(EX3DError);

procedure TX3DRoute.SaveToStream(Writer: TX3DWriter);

  procedure Ending(Node: TX3DNode; Event: TX3DEvent; const S: string;
    out NodeName, EventName: string);
  var
    BoundNode: TX3DNode;
    IgnoreNodeFinished: boolean;
  begin
    { Check Node }
    if Node = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node not assigned (look for warnings when reading this VRML file)', [S]);
    if Node.X3DName = '' then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node not named', [S]);

    BoundNode := (Writer as TX3DWriterNames).NodeNames.Bound(Node.X3DName, IgnoreNodeFinished);
    if BoundNode = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound',
        [S, Node.X3DName]);

    { Just like when setting node by TX3DRoute.SetEnding:
      we actually keep the Node that contains the route, which is
      sometimes TX3DPrototypeNode hidden inside PrototypeInstanceSourceNode. }
    if BoundNode.PrototypeInstanceSourceNode <> nil then
      BoundNode := BoundNode.PrototypeInstanceSourceNode;
    if BoundNode <> Node then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s node name "%s" not bound (another node bound to the same name)',
        [S, Node.X3DName]);

    NodeName := Node.X3DName;

    { Check Event }
    if Event = nil then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s event not assigned', [S]);

    { Check we have a name. }
    if Event.X3DName = '' then
      raise EX3DRouteSaveError.CreateFmt('Cannot save VRML route: %s event not named', [S]);
    EventName := Event.X3DName;
  end;

var
  SourceNodeName, SourceEventName, DestinationNodeName, DestinationEventName: string;
begin
  if Internal then Exit;

  try
    Ending(SourceNode     , SourceEvent     , 'source'     , SourceNodeName     , SourceEventName     );
    Ending(DestinationNode, DestinationEvent, 'destination', DestinationNodeName, DestinationEventName);

    case Writer.Encoding of
      xeClassic:
        Writer.WritelnIndent(Format('ROUTE %s.%s TO %s.%s', [
          EncodeX3DName(SourceNodeName),
          EncodeX3DName(SourceEventName),
          EncodeX3DName(DestinationNodeName),
          EncodeX3DName(DestinationEventName)
        ]));
      xeXML:
        Writer.WritelnIndent(Format('<ROUTE fromNode=%s fromField=%s toNode=%s toField=%s />', [
          StringToX3DXml(EncodeX3DName(SourceNodeName)),
          StringToX3DXml(EncodeX3DName(SourceEventName)),
          StringToX3DXml(EncodeX3DName(DestinationNodeName)),
          StringToX3DXml(EncodeX3DName(DestinationEventName))
        ]));
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TX3DRoute.SaveToStream Encoding?');
      {$endif}
    end;
  except
    on E: EX3DRouteSaveError do
      WritelnWarning('X3D', E.Message);
  end;
end;

procedure TX3DRoute.DestructionNotification(const Node: TX3DNode);
begin
  { UnsetEnding is called with RemoveFromDestructionNotification = false.
    Reason:
    1. Removing from DestructionNotification is not needed,
       since the Node is destroyed anyway, along with it's
       DestructionNotification list.
    2. Moreover, right now we probably iterate over DestructionNotification,
       so removing items from it is very bad idea (indexes shift,
       pointers change if reallocation occurs). }

  if Node = FSourceNode then
    UnsetEnding(FSourceNode     , FSourceEvent     , false, false);

  if Node = FDestinationNode then
    UnsetEnding(FDestinationNode, FDestinationEvent, true , false);
end;

function TX3DRoute.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DRoute;
var
  NewSourceNode, NewDestinationNode: TX3DNode;
  NewSourceEvent, NewDestinationEvent: TX3DEvent;
begin
  Result := TX3DRoute.Create;
  Result.Internal := Internal;

  if (SourceNode <> nil) and
     (SourceEvent <> nil) then
  begin
    NewSourceNode := CopyState.DeepCopy(SourceNode);
    NewSourceEvent := NewSourceNode.AnyEvent(SourceEvent.X3DName);
    if NewSourceEvent = nil then
      raise EInternalError.CreateFmt('Route source node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewSourceNode.X3DName,
          NewSourceNode.X3DType,
          NewSourceEvent.X3DName ]);
    Result.SetSourceDirectly(NewSourceNode, NewSourceEvent);
  end;

  if (DestinationNode <> nil) and
     (DestinationEvent <> nil) then
  begin
    NewDestinationNode := CopyState.DeepCopy(DestinationNode);
    NewDestinationEvent := NewDestinationNode.AnyEvent(DestinationEvent.X3DName);
    if NewDestinationEvent = nil then
      raise EInternalError.CreateFmt('Route destination node "%s" (%s) has event "%s", which is not found in this node''s deep copy',
        [ NewDestinationNode.X3DName,
          NewDestinationNode.X3DType,
          NewDestinationEvent.X3DName ]);
    Result.SetDestinationDirectly(NewDestinationNode, NewDestinationEvent);
  end;
end;

{$endif read_implementation}
