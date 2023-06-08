{%MainUnit x3dfields.pas}
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

  { Base class for VRML/X3D field or event. }
  TX3DFieldOrEvent = class(TX3DFileItem)
  strict private
    { To optimize memory usage (which may really be huge in case of VRML/X3D
      with many nodes, esp. when VRML/X3D uses a lot of prototypes),
      this is only created when needed (when it is not empty).
      Otherwise it is @nil. }
    FIsClauseNames: TCastleStringList;

    FParentNode: TX3DFileItem;
    FParentInterfaceDeclaration: TX3DFileItem;

    function GetIsClauseNames(const Index: Integer): string;

    { Return FIsClauseNames, initializing it if necessary to not be nil. }
    function IsClauseNamesCreate: TCastleStringList;
  private
    FX3DName: string;

    { Implementation of AddAlternativeName uses it like this:
      - FAlternativeNames[0] is alternative for any VRML/X3D version
      - FAlternativeNames[1], [2], [3] is alternative for VRML 1.0, VRML 2.0 (97) and X3D
      - any FAlternativeNames[...] that is '' should be ignored
    }
    FAlternativeNames: array of string;
  strict protected
    procedure FieldOrEventAssignCommon(Source: TX3DFieldOrEvent);
  public
    constructor Create(const AParentNode: TX3DFileItem; const AX3DName: string);
    destructor Destroy; override;

    { Name of the field or event.

      Most fields/events are inside some X3D node, and then
      they have a non-empty name. But in some special cases we
      also use temporary fields with an empty name.

      Note that you cannot change this after object creation, since
      Name is used for various purposes (like to generate names for
      TX3DField.ExposedEvents).

      Note that this property is deliberately not called @code(Name).
      In the future, this class may descend from the standard TComponent
      class, that defines a @code(Name) field with a special restrictions
      (it must be a valid Pascal identifier), which cannot apply to X3D node names
      (that can have quite free names, see
      http://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/grammar.html ).
      We don't want to confuse these two properties. }
    property X3DName: string read FX3DName;
    {$ifdef FPC} property Name: string read FX3DName; deprecated 'use X3DName';{$endif}

    { X3D node containing this field/event.
      This must always contain an instance
      of TX3DNode class (although it cannot be declared such, since X3DFields
      unit cannot depend on X3DNodes interface).

      It may be @nil for special fields/events when parent node is unknown. }
    property ParentNode: TX3DFileItem read FParentNode write FParentNode;

    { "IS" clauses of this field/event, used when this field/event
      is inside prototype definition.

      This is an array, as one item may have many "IS" clauses (for a field,
      only one "IS" clause should refer to another field;
      but you can have many "IS" clauses connecting events,
      also exposedField may have "IS" clause that should be interpreted
      actually as links to it's exposed events).
      See e.g. @code(demo_models/x3d/proto_events_test_3.x3dv).

      Note that having "IS" clauses doesn't mean that the field should
      be considered "without any value". This is not a good way of thinking,
      as an exposed field may have an "IS" clause, but linking it to an event,
      and thus such field has it's value (default value, if not specified
      in the file), event though it also has an "IS" clause.
      Although there is TX3DField.ValueFromIsClause, which indicates
      whether current value was obtained from "IS" clause.

      To be able to significantly optimize memory, we do not expose IsClauseNames
      as TCastleStringList. Instead operate on them only using below functions.
      Note that IsClauseNamesAssign can also accept @nil as parameter.

      @groupBegin }
    property IsClauseNames[const Index: Integer]: string read GetIsClauseNames;
    function IsClauseNamesCount: Integer;
    procedure IsClauseNamesAssign(const SourceIsClauseNames: TCastleStringList);
    procedure IsClauseNamesAdd(const S: string);
    { @groupEnd }

    { Parse only "IS" clause, if it's not present --- don't do nothing.
      For example, for the TX3DField descendant, this does not try to parse
      field value. }
    procedure ParseIsClause(Lexer: TX3DLexer);

    { Add alternative name for the same field/event, to be used in different
      VRML version.

      When VRML major version is exactly equal X3DMajorVersion,
      the AlternativeName should be used --- for both reading and writing
      of this field/event. In some cases, when reading, we may also allow
      all versions (both original and alternative), but this is mostly
      for implementation simplicity --- don't count on it.

      A special value 0 for X3DMajorVersion means that this is just
      an alternative name, that should be allowed when reading (as alternative
      to normal Name), and never used when writing.

      Alternative names is a very handy mechanism for cases when
      the only thing that changed between VRML versions is the field
      name. Example: Switch node's children/choice, LOD node's children/level,
      Polyline2D lineSegments/point.

      Note that this also works for ExposedEvents with exposed TX3DField:
      if a field has alternative names, then it's exposed events always also
      have appropriate alternative names. }
    procedure AddAlternativeName(const AlternativeName: string;
      const X3DMajorVersion: Integer); virtual;
    procedure RemoveAlternativeName(const X3DMajorVersion: Integer);

    { Returns if S matches current Name or one of the alternative names.
      Think about it like simple test "Name = S", but actually this
      checks also names added by AddAlternativeName method. }
    function IsName(const S: string): boolean;

    { Return how this field should be named for given VRML version.
      In almost all cases, this simply returns current Name.
      But it can also return a name added by AddAlternativeName method. }
    function NameForVersion(Version: TX3DVersion): string; overload;
    function NameForVersion(Writer: TX3DWriter): string; overload;

    { For fields contained in TX3DInterfaceDeclaration.

      This should always be @nil (if the field is normal, standard field,
      not coming from interface declaration in X3D file) or an instance of
      TX3DInterfaceDeclaration. (But it cannot be declared such,
      since TX3DInterfaceDeclaration is not known in this unit). }
    property ParentInterfaceDeclaration: TX3DFileItem
      read FParentInterfaceDeclaration write FParentInterfaceDeclaration;

    { Nice and concise field description for user.
      Describes parent node type, name and field/event's name. }
    function NiceName: string;

    { Save IS clauses to stream, only for classic encoding.
      For each IS clause, writeln field/event name followed by "IS" clause. }
    procedure SaveToStreamClassicIsClauses(Writer: TX3DWriter);

    { Similar to Assign, but for SFNode / MFNode fields they are copied by DeepCopy,
      not merely by copying references.
      Given CopyState is always TX3DNodeDeepCopyState,
      but it cannot be declared as such here.
      @exclude }
    procedure InternalAssignDeepCopy(const Source: TX3DFieldOrEvent;
      const CopyState: TObject); virtual;
  end;

  TX3DFieldOrEventList = {$ifdef FPC}specialize{$endif} TObjectList<TX3DFieldOrEvent>;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DFieldOrEvent ---------------------------------------------------------- }

constructor TX3DFieldOrEvent.Create(const AParentNode: TX3DFileItem;
  const AX3DName: string);
begin
  inherited Create;
  FParentNode := AParentNode;
  FX3DName := AX3DName;
end;

destructor TX3DFieldOrEvent.Destroy;
begin
  FreeAndNil(FIsClauseNames);
  inherited;
end;

function TX3DFieldOrEvent.IsClauseNamesCreate: TCastleStringList;
begin
  if FIsClauseNames = nil then
    FIsClauseNames := TCastleStringList.Create;
  Result := FIsClauseNames;
end;

function TX3DFieldOrEvent.GetIsClauseNames(const Index: Integer): string;
begin
  if FIsClauseNames = nil then
    raise Exception.CreateFmt('IsClauseNames item index %d does not exist, because IsClauseNames is empty',
      [Index]);
  Result := FIsClauseNames[Index];
end;

function TX3DFieldOrEvent.IsClauseNamesCount: Integer;
begin
  if FIsClauseNames = nil then
    Result := 0 else
    Result := FIsClauseNames.Count;
end;

procedure TX3DFieldOrEvent.IsClauseNamesAssign(
  const SourceIsClauseNames: TCastleStringList);
begin
  if (SourceIsClauseNames <> nil) and
     (SourceIsClauseNames.Count <> 0) then
    IsClauseNamesCreate.Assign(SourceIsClauseNames) else
    FreeAndNil(FIsClauseNames);
end;

procedure TX3DFieldOrEvent.IsClauseNamesAdd(const S: string);
begin
  IsClauseNamesCreate.Add(S);
end;

procedure TX3DFieldOrEvent.ParseIsClause(Lexer: TX3DLexer);
begin
  if Lexer.TokenIsKeyword(vkIS) then
  begin
    Lexer.NextToken;
    IsClauseNamesCreate.Add(Lexer.TokenName);
    Lexer.NextToken;
  end;
end;

procedure TX3DFieldOrEvent.AddAlternativeName(const AlternativeName: string;
  const X3DMajorVersion: Integer);
begin
  if X3DMajorVersion >= Length(FAlternativeNames) then
    SetLength(FAlternativeNames, X3DMajorVersion + 1);

  FAlternativeNames[X3DMajorVersion] := AlternativeName;
end;

procedure TX3DFieldOrEvent.RemoveAlternativeName(const X3DMajorVersion: Integer);
begin
  if X3DMajorVersion < Length(FAlternativeNames) then
    FAlternativeNames[X3DMajorVersion] := '';
end;

function TX3DFieldOrEvent.IsName(const S: string): boolean;
var
  I: Integer;
begin
  { No field is ever named ''.
    Actually, we sometimes use '' for special "unnamed fields",
    in this case it's Ok that no name matches their name.
    Besides, we don't want empty FAlternativeNames to match when
    searching for S = ''. }

  if S = '' then
    Exit(false);

  for I := Low(FAlternativeNames) to High(FAlternativeNames) do
    if FAlternativeNames[I] = S then
      Exit(true);

  Result := X3DName = S;
end;

function TX3DFieldOrEvent.NameForVersion(Version: TX3DVersion): string;
begin
  if Version.Major < Length(FAlternativeNames) then
    Result := FAlternativeNames[Version.Major]
  else
    Result := '';

  if Result = '' then
    Result := X3DName;
end;

function TX3DFieldOrEvent.NameForVersion(
  Writer: TX3DWriter): string;
begin
  Result := NameForVersion(Writer.Version);
end;

procedure TX3DFieldOrEvent.FieldOrEventAssignCommon(Source: TX3DFieldOrEvent);
begin
  FX3DName := Source.X3DName;
  IsClauseNamesAssign(Source.FIsClauseNames);
  FPositionInParent := Source.PositionInParent;
  FAlternativeNames := Source.FAlternativeNames;
end;

function TX3DFieldOrEvent.NiceName: string;
begin
  Result := '';

  if ParentNode <> nil then
    Result := Result + TX3DNode(ParentNode).NiceName + '.';

  if X3DName <> '' then
    Result := Result + X3DName
  else
    Result := Result + '<not named field>';
end;

procedure TX3DFieldOrEvent.SaveToStreamClassicIsClauses(Writer: TX3DWriter);
var
  N: string;
  I: Integer;
begin
  N := NameForVersion(Writer);

  { When N = '', we assume that field/event has only one "IS" clause.
    Otherwise results don't make any sense. }
  for I := 0 to IsClauseNamesCount - 1 do
  begin
    if N <> '' then
      Writer.WriteIndent(N + ' ');
    Writer.Writeln('IS ' + IsClauseNames[I]);
  end;
end;

procedure TX3DFieldOrEvent.InternalAssignDeepCopy(const Source: TX3DFieldOrEvent;
  const CopyState: TObject);
begin
  Assign(Source);
end;

{$endif read_implementation}
