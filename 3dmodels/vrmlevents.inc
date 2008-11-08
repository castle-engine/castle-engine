{
  Copyright 2007 Michalis Kamburelis.

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


{$ifdef read_interface}

  { VRML event. }
  TVRMLEventReceive = procedure (Event: TVRMLEvent; Value: TVRMLField;
    const Time: TVRMLTime) of object;

  TDynArrayItem_1 = TVRMLEventReceive;
  PDynArrayItem_1 = ^TVRMLEventReceive;
  {$define DYNARRAY_1_IS_FUNCTION}
  {$define DYNARRAY_1_IS_FUNCTION_METHOD}
  {$define DYNARRAY_1_USE_EQUALITY}
  {$I dynarray_1.inc}
  TDynVRMLEventReceiveArray = class(TDynArray_1)
  public
    { This calls all functions (all Items) passing them
      (Event, Value) parameters. }
    procedure ExecuteAll(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  end;

  TVRMLEvent = class(TVRMLFieldOrEvent)
  private
    FFieldClass: TVRMLFieldClass;
    FInEvent: boolean;
    FOnReceive: TDynVRMLEventReceiveArray;
    FParentExposedField: TVRMLField;
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
    procedure Parse(Lexer: TVRMLLexer);

    { For each IS clause, writeln event name followed by "IS" clause.

      If WriteExactlyOnce then we will write exactly one line:
      only name (if no IS clauses), or just name + first IS clause.
      Assertion checks that we cannot have more than 1 IS clause in this case.
      This is suitable for saving VRML interface declaration
      containing an event. }
    procedure EventSaveToStream(SaveProperties: TVRMLSaveToStreamProperties;
      WriteExactlyOnce: boolean);

    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

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
      at any comfortable time, possibly right after calling Send. }
    procedure Send(Field: TVRMLField; const Time: TVRMLTime);

    { Send a value to event.

      See the @link(Send(TVRMLField)) method.
      These shortcuts just create temporary appropriate TVRMLField instance,
      and call Send(TVRMLField) method.

      The temporary field instance has Name initialized from this event Name,
      to ease debugging (you can see e.g. by Logger node the originating
      event name then).

      @groupBegin }
    procedure Send(const Value: LongInt; const Time: TVRMLTime);
    procedure Send(const Value: boolean; const Time: TVRMLTime);
    procedure Send(const Value: string; const Time: TVRMLTime);
    procedure Send(const Value: TVector2Single; const Time: TVRMLTime);
    procedure Send(const Value: TVector3Single; const Time: TVRMLTime);
    procedure Send(const Value: TVector4Single; const Time: TVRMLTime);
    procedure Send(const Value: Single; const Time: TVRMLTime);
    procedure Send(const Value: Double; const Time: TVRMLTime);

    procedure Send(const Value: array of TVector2Single; const Time: TVRMLTime);
    procedure Send(const Value: array of TVector3Single; const Time: TVRMLTime);
    procedure Send(const Value: array of TVector4Single; const Time: TVRMLTime);
    { @groupEnd }

    { Notifications about receiving an item. For input fields,
      these are used by node itself to listen to events send to it.
      For output fields, these are used by routes, scripts to listen
      to events occuring.

      All callbacks here are simply called by @link(Send) method. }
    property OnReceive: TDynVRMLEventReceiveArray read FOnReceive;

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

  TObjectsListItem_3 = TVRMLEvent;
  {$I objectslist_3.inc}
  TVRMLEventsList = class(TObjectsList_3)
  public
    function IndexOf(const Name: string): Integer;
  end;

{$endif read_interface}

{$ifdef read_implementation}
{$I objectslist_3.inc}
{$I dynarray_1.inc}

{ TDynVRMLEventReceiveArray -------------------------------------------------- }

procedure TDynVRMLEventReceiveArray.ExecuteAll(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
    Items[I](Event, Value, Time);
end;

{ TVRMLEvent ----------------------------------------------------------------- }

constructor TVRMLEvent.Create(AParentNode: TVRMLFileItem;
  const AName: string; const AFieldClass: TVRMLFieldClass;
  const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName);

  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
  FOnReceive := TDynVRMLEventReceiveArray.Create;
end;

destructor TVRMLEvent.Destroy;
begin
  FreeAndNil(FOnReceive);
  inherited;
end;

procedure TVRMLEvent.Parse(Lexer: TVRMLLexer);
begin
  ParseIsClause(Lexer);
end;

procedure TVRMLEvent.EventSaveToStream(
  SaveProperties: TVRMLSaveToStreamProperties;
  WriteExactlyOnce: boolean);
var
  N: string;
  I: Integer;
begin
  N := NameForVersion(SaveProperties);

  if WriteExactlyOnce then
  begin
    { Saving interface declaration doesn't produce sensible results for
      Event.IsClauseNames.Count > 1, there's no way to save it
      from single interface decl. }
    Assert(IsClauseNames.Count <= 1, 'saving event with many IS clauses from single interface decl not possible');

    if N <> '' then
      SaveProperties.WriteIndent(N + ' ');

    if IsClauseNames.Count = 1 then
      SaveProperties.Write('IS ' + IsClauseNames.Items[0]);

    SaveProperties.Writeln;
  end else
  begin
    { Actually, when N = '', we assume that event has only one "IS" clause.
      Otherwise results don't make any sense. }

    for I := 0 to IsClauseNames.Count - 1 do
    begin
      if N <> '' then
        SaveProperties.WriteIndent(N + ' ');
      SaveProperties.Writeln('IS ' + IsClauseNames.Items[I]);
    end;
  end;
end;

procedure TVRMLEvent.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
begin
  EventSaveToStream(SaveProperties, false);
end;

procedure TVRMLEvent.Send(Field: TVRMLField; const Time: TVRMLTime);
begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);
  FOnReceive.ExecuteAll(Self, Field, Time);
end;

procedure TVRMLEvent.Send(const Value: LongInt; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFInt32 then
    Field := TSFInt32.Create(ParentNode, Name, Value) else
    Field := TSFLong.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: boolean; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  Field := TSFBool.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: string; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  Field := TSFString.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector2Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  Field := TSFVec2f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector3Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColor then
    Field := TSFColor.Create(ParentNode, Name, Value) else
    Field := TSFVec3f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector4Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColorRGBA then
    Field := TSFColorRGBA.Create(ParentNode, Name, Value) else
  if FieldClass = TSFRotation then
    Field := TSFRotation.Create(ParentNode, Name, Value) else
    Field := TSFVec4f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  Field := TSFFloat.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Double; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFTime then
    Field := TSFTime.Create(ParentNode, Name, Value) else
    Field := TSFDouble.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: array of TVector2Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  Field := TMFVec2f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: array of TVector3Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColor then
    Field := TMFColor.Create(ParentNode, Name, Value) else
    Field := TMFVec3f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: array of TVector4Single; const Time: TVRMLTime);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColorRGBA then
    Field := TMFColorRGBA.Create(ParentNode, Name, Value) else
  if FieldClass = TSFRotation then
    Field := TMFRotation.Create(ParentNode, Name, Value) else
    Field := TMFVec4f.Create(ParentNode, Name, Value);
  try
    Send(Field, Time);
  finally FreeAndNil(Field) end;
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

{ TVRMLEventsList ------------------------------------------------------------ }

function TVRMLEventsList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = Name then Exit;
  Result := -1;
end;

{$endif read_implementation}
