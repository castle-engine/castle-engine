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
  TVRMLEventReceive = procedure (Event: TVRMLEvent; Value: TVRMLField) of object;

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
    procedure ExecuteAll(Event: TVRMLEvent; Value: TVRMLField);
  end;

  TVRMLEvent = class(TVRMLFieldOrEvent)
  private
    FFieldClass: TVRMLFieldClass;
    FInEvent: boolean;
    FOnReceive: TDynVRMLEventReceiveArray;
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

    { Write event name, and (if present) "IS" clause of the event. }
    procedure SaveToStream(SaveProperties: TVRMLSaveToStreamProperties); override;

    { Send a value to event. For input fields, this is used by routes, scripts
      etc. to send an event to a field. For output fields, this is used by
      node itself to send event to routes, scripts etc.

      Field must be non-nil, of class FieldClass, and must have a value
      (that is, Field.IsClause must be @false).

      The Field instance doesn't become owned in any way by the TVRMLEvent.
      That is, it's the caller responsibility to free the Field instance
      at any comfortable time, possibly right after calling Send. }
    procedure Send(Field: TVRMLField);

    { Send a value to event.

      See the @link(Send(TVRMLField)) method.
      These shortcuts just create temporary appropriate TVRMLField instance,
      and call Send(TVRMLField) method.

      The temporary field instance has Name initialized from this event Name,
      to ease debugging (you can see e.g. by Logger node the originating
      event name then).

      @groupBegin }
    procedure Send(const Value: LongInt);
    procedure Send(const Value: boolean);
    procedure Send(const Value: string);
    procedure Send(const Value: TVector2Single);
    procedure Send(const Value: TVector3Single);
    procedure Send(const Value: TVector4Single);
    procedure Send(const Value: Single);
    procedure Send(const Value: Double);
    { @groupEnd }

    { Notifications about receiving an item. For input fields,
      these are used by node itself to listen to events send to it.
      For output fields, these are used by routes, scripts to listen
      to events occuring.

      All callbacks here are simply called by @link(Send) method. }
    property OnReceive: TDynVRMLEventReceiveArray read FOnReceive;
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

procedure TDynVRMLEventReceiveArray.ExecuteAll(Event: TVRMLEvent; Value: TVRMLField);
var
  I: Integer;
begin
  for I := 0 to Length - 1 do
    Items[I](Event, Value);
end;

{ TVRMLEvent ----------------------------------------------------------------- }

constructor TVRMLEvent.Create(AParentNode: TVRMLFileItem;
  const AName: string; const AFieldClass: TVRMLFieldClass;
  const AInEvent: boolean);
begin
  inherited Create;
  FName := AName;
  FParentNode := AParentNode;

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

procedure TVRMLEvent.SaveToStream(SaveProperties: TVRMLSaveToStreamProperties);
begin
  SaveProperties.WriteIndent(NameForVersion(SaveProperties) + ' ');
  if IsClause then
    SaveProperties.Write('IS ' + IsClauseName);
  SaveProperties.Writeln;
end;

procedure TVRMLEvent.Send(Field: TVRMLField);
begin
  Assert(Field <> nil);
  Assert(Field is FieldClass);
  Assert(not Field.IsClause);
  FOnReceive.ExecuteAll(Self, Field);
end;

procedure TVRMLEvent.Send(const Value: LongInt);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFInt32 then
    Field := TSFInt32.Create(ParentNode, Name, Value) else
    Field := TSFLong.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: boolean);
var
  Field: TVRMLField;
begin
  Field := TSFBool.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: string);
var
  Field: TVRMLField;
begin
  Field := TSFString.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector2Single);
var
  Field: TVRMLField;
begin
  Field := TSFVec2f.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector3Single);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColor then
    Field := TSFColor.Create(ParentNode, Name, Value) else
    Field := TSFVec3f.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: TVector4Single);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFColorRGBA then
    Field := TSFColorRGBA.Create(ParentNode, Name, Value) else
  if FieldClass = TSFRotation then
    Field := TSFRotation.Create(ParentNode, Name, Value) else
    Field := TSFVec4f.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Single);
var
  Field: TVRMLField;
begin
  Field := TSFFloat.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

procedure TVRMLEvent.Send(const Value: Double);
var
  Field: TVRMLField;
begin
  if FieldClass = TSFTime then
    Field := TSFTime.Create(ParentNode, Name, Value) else
    Field := TSFDouble.Create(ParentNode, Name, Value);
  try
    Send(Field);
  finally FreeAndNil(Field) end;
end;

{ TVRMLEventsList ------------------------------------------------------------ }

function TVRMLEventsList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = Name then Exit;
  Result := -1;
end;

{$endif read_implementation}
