{
  Copyright 2007-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}


{$ifdef read_interface}

  { }
  TVRMLEventReceive = procedure (Event: TVRMLEvent; Value: TVRMLField;
    const Time: TX3DTime) of object;

  TVRMLEventReceiveList = class(specialize TGenericStructList<TVRMLEventReceive>)
  public
    { Call all functions. }
    procedure ExecuteAll(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TX3DTime);
  end;

  { VRML event. }
  TVRMLEvent = class(TVRMLFieldOrEvent)
  private
    FFieldClass: TVRMLFieldClass;
    FInEvent: boolean;
    FOnReceive: TVRMLEventReceiveList;
    FParentExposedField: TVRMLField;
    Temp: TVRMLField;
    TempUsed: boolean;

    SendLevel: Cardinal;
    HandlersToRemove: TVRMLEventReceiveList;

    function CreateTemp: TVRMLField;
    procedure FreeTemp(const Field: TVRMLField);
  public
    constructor Create(AParentNode: TVRMLFileItem;
      const AName: string;
      const AFieldClass: TVRMLFieldClass; const AInEvent: boolean);
    destructor Destroy; override;

    property FieldClass: TVRMLFieldClass read FFieldClass;

    { @abstract(Is it "in" or "out" event ?) }
    property InEvent: boolean read FInEvent;

    { This only reads (optional) "IS" clause of the event, as may occur
      in VRML nodeBodyStatement. }
    procedure Parse(Lexer: TX3DLexer);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function SaveToXml: TSaveToXmlMethod; override;

    { If this event is an exposed event belonging to some field,
      this references parent field. Otherwise it's @nil. }
    property ParentExposedField: TVRMLField
      read FParentExposedField write FParentExposedField;

    { Send a value to event. For input fields, this is used by routes, scripts
      etc. to send an event to a field. For output fields, this is used by
      node itself to send event to routes, scripts etc.

      Field must be non-nil, of class FieldClass.

      The Field instance doesn't become owned in any way by the TVRMLEvent.
      That is, it's the caller responsibility to free the Field instance
      at any comfortable time, possibly right after calling Send.

      Overloaded versions without explicit Time parameter just take time
      from ParentNode.Scene.GetTime. If ParentNode is @nil
      (which should not happen with normal fields within nodes)
      or if ParentNode.Scene is @nil (which may happen only if events
      processing is not turned on, that is T3DSceneCore.ProcessEvents is @false)
      then event is not send. }
    procedure Send(Field: TVRMLField; const Time: TX3DTime);
    procedure Send(Field: TVRMLField);

    { Send a value to event.

      See the @link(Send Send(TVRMLField, TX3DTime)) method.
      These shortcuts just create temporary appropriate TVRMLField instance,
      and call Send(TVRMLField) method.

      The temporary field instance has Name initialized from this event Name,
      to ease debugging (you can see e.g. by Logger node the originating
      event name then).

      We have separate names for single-value and multiple-value fields
      (Send and SendArray), otherwise overloads could get confused
      (as vector is also just an array of floats, and matrix is just an
      array of vectors).

      @groupBegin }
    procedure Send(const Value: LongInt; const Time: TX3DTime);
    procedure Send(const Value: boolean; const Time: TX3DTime);
    procedure Send(const Value: string; const Time: TX3DTime);
    procedure Send(const Value: TVector2Single; const Time: TX3DTime);
    procedure Send(const Value: TVector3Single; const Time: TX3DTime);
    procedure Send(const Value: TVector4Single; const Time: TX3DTime);
    procedure Send(const Value: Single; const Time: TX3DTime);
    procedure Send(const Value: Double; const Time: TX3DTime);
    procedure Send(const Value: TMatrix3Single; const Time: TX3DTime);
    procedure Send(const Value: TMatrix4Single; const Time: TX3DTime);
    procedure Send(const Value: TMatrix3Double; const Time: TX3DTime);
    procedure Send(const Value: TMatrix4Double; const Time: TX3DTime);

    procedure SendArray(const Value: array of string; const Time: TX3DTime);
    procedure SendArray(const Value: array of TVector2Single; const Time: TX3DTime);
    procedure SendArray(const Value: array of TVector3Single; const Time: TX3DTime);
    procedure SendArray(const Value: array of TVector4Single; const Time: TX3DTime);

    procedure Send(const Value: LongInt);
    procedure Send(const Value: boolean);
    procedure Send(const Value: string);
    procedure Send(const Value: TVector2Single);
    procedure Send(const Value: TVector3Single);
    procedure Send(const Value: TVector4Single);
    procedure Send(const Value: Single);
    procedure Send(const Value: Double);
    procedure Send(const Value: TMatrix3Single);
    procedure Send(const Value: TMatrix4Single);
    procedure Send(const Value: TMatrix3Double);
    procedure Send(const Value: TMatrix4Double);

    procedure SendArray(const Value: array of string);
    procedure SendArray(const Value: array of TVector2Single);
    procedure SendArray(const Value: array of TVector3Single);
    procedure SendArray(const Value: array of TVector4Single);
    { @groupEnd }

    { Notifications about receiving an item. For input fields,
      these are used by node itself to listen to events send to it.
      For output fields, these are used by routes, scripts to listen
      to events occuring.

      All callbacks here are simply called by @link(Send) method. }
    property OnReceive: TVRMLEventReceiveList read FOnReceive;

    { Safely remove a callback from OnReceive list.

      Contrary to direct @code(OnReceive.Remove) call, this works correctly
      even if we're right now in the middle of this event's processing.
      In the latter case, the handler will be actually removed with some delay
      (when it's safe). }
    procedure RemoveHandler(Handler: TVRMLEventReceive);

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

  TVRMLEventList = class(specialize TFPGObjectList<TVRMLEvent>)
  public
    function IndexOf(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVRMLEventReceiveList -------------------------------------------------- }

procedure TVRMLEventReceiveList.ExecuteAll(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TX3DTime);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    L[I](Event, Value, Time);
end;

{ TVRMLEvent ----------------------------------------------------------------- }

constructor TVRMLEvent.Create(AParentNode: TVRMLFileItem;
  const AName: string; const AFieldClass: TVRMLFieldClass;
  const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName);

  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
  FOnReceive := TVRMLEventReceiveList.Create;
end;

destructor TVRMLEvent.Destroy;
begin
  FreeAndNil(Temp);
  FreeAndNil(HandlersToRemove);
  FreeAndNil(FOnReceive);
  inherited;
end;

procedure TVRMLEvent.Parse(Lexer: TX3DLexer);
begin
  ParseIsClause(Lexer);
end;

procedure TVRMLEvent.SaveToStream(Writer: TX3DWriter);
begin
  { saves nothing. IS clauses are saved separately by SaveToStreamIsClauses }
end;

function TVRMLEvent.SaveToXml: TSaveToXmlMethod;
begin
  Result := sxNone;
end;

procedure TVRMLEvent.Send(Field: TVRMLField; const Time: TX3DTime);

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

procedure TVRMLEvent.Send(const Value: LongInt; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFLong).Value := Value; // handles also SFInt32
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: boolean; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFBool).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: string; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFString).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector2Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFVec2f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector3Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFVec3f).Value := Value; // handles also SFColor
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector4Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  if FieldClass = TSFRotation then
    (Field as TSFRotation).Value := Value else
    (Field as TSFVec4f).Value := Value; // handles also SFColorRGBA
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFFloat).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Double; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFDouble).Value := Value; // handles also SFTime
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TMatrix3Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFMatrix3f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TMatrix4Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFMatrix4f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TMatrix3Double; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFMatrix3d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TMatrix4Double; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TSFMatrix4d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.SendArray(const Value: array of string; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TMFString).Items.AssignArray(Value);
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector2Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TMFVec2f).Items.AssignArray(Value);
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector3Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  (Field as TMFVec3f).Items.AssignArray(Value); // handles also MFColor
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector4Single; const Time: TX3DTime);
var
  Field: TVRMLField;
begin
  Field := CreateTemp;
  if FieldClass = TMFRotation then
    (Field as TMFRotation).Items.AssignArray(Value) else
    (Field as TMFVec4f).Items.AssignArray(Value); // handles also MFColorRGBA
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TVRMLEvent.Send(Field: TVRMLField);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Field, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: LongInt);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: boolean);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: string);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TVector2Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TVector3Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TVector4Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TMatrix3Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TMatrix4Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TMatrix3Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.Send(const Value: TMatrix4Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.SendArray(const Value: array of string);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    SendArray(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector2Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    SendArray(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector3Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    SendArray(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

procedure TVRMLEvent.SendArray(const Value: array of TVector4Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    SendArray(Value, TX3DNode(ParentNode).Scene.GetTime);
end;

function TVRMLEvent.SendNeeded: boolean;
begin
  Result := OnReceive.Count <> 0;
end;

procedure TVRMLEvent.Assign(Source: TPersistent);
begin
  if Source is TVRMLEvent then
  begin
    FieldOrEventAssignCommon(TVRMLEvent(Source));

    FFieldClass := TVRMLEvent(Source).FieldClass;
    FInEvent := TVRMLEvent(Source).InEvent;
  end;
end;

function TVRMLEvent.CreateTemp: TVRMLField;
begin
  if not TempUsed then
  begin
    if Temp = nil then
      Temp := FieldClass.CreateUndefined(ParentNode, Name, false);
    Result := Temp;
    TempUsed := true;
  end else
    Result := FieldClass.CreateUndefined(ParentNode, Name, false);
end;

procedure TVRMLEvent.FreeTemp(const Field: TVRMLField);
begin
  if Field = Temp then
    TempUsed := false else
    Field.Free;
end;

procedure TVRMLEvent.RemoveHandler(Handler: TVRMLEventReceive);
begin
  if SendLevel = 0 then
    OnReceive.Remove(Handler) else
  begin
    if HandlersToRemove = nil then
      HandlersToRemove := TVRMLEventReceiveList.Create;
    HandlersToRemove.Add(Handler);
  end;
end;

{ TVRMLEventList ------------------------------------------------------------ }

function TVRMLEventList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].IsName(Name) then Exit;
  Result := -1;
end;

{$endif read_implementation}
