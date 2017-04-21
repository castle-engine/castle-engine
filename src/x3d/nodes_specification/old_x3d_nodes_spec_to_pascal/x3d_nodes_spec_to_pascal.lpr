{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reads nodes definition from text file on stdin,
  in a format like X3D specification
  (see castle_game_engine/src/x3d/nodes_specification/components/*.txt files)
  and outputs on stdout Pascal include
  file that for each X3D node defines appropriate Pascal class
  (with appropriate fields, events, field default values, etc.).

  Param $1 is the X3D component name (for now used only to name the
  RegisterXxxNodes procedure, that you should paste call at
  initialization).

  Example call is:

    ./x3d_nodes_spec_to_pascal Lighting <  ../components/Lighting.txt

  Generated Pascal class can be directly used inside unit like X3DNodes
  to allow our engine to recognize and parse all fields of given node. }
program x3d_nodes_spec_to_pascal;

uses SysUtils, Classes, CastleClassUtils, X3DFields,
  CastleStringUtils, X3DNodes, CastleUtils, CastleParameters;

var
  InterfaceLines: TMemoryStream;
  ImplementationLines: TMemoryStream;
  RegistrationLines: TMemoryStream;

var
  WasSomeNode: boolean;
  NodeType: string;
  IsInterface: boolean;
  IsAbstract: boolean;

procedure ParseNodeStartLine(const Line: string);
const
  Delimiters = WhiteSpaces + [','];
var
  SeekPos: Integer;
  SomeAncestorFound: boolean;
  Token: string;
begin
  SeekPos := 1;

  Token := NextToken(Line, SeekPos, Delimiters);
  NodeType := Token;
  IsInterface := IsSuffix('Object', NodeType, false);
  IsAbstract := IsPrefix('X3D', NodeType, false);

  if IsInterface then
    WriteStr(InterfaceLines, '  I' + NodeType + 'Node = interface(') else
    WriteStr(InterfaceLines, '  T' + NodeType + 'Node = class(');

  SomeAncestorFound := false;

  Token := NextToken(Line, SeekPos, Delimiters);
  if Token = ':' then
  begin
    repeat
      Token := NextToken(Line, SeekPos, Delimiters);
      if (Token = '') or (Token = '{') then break;

      if SomeAncestorFound then
      begin
        WriteStr(InterfaceLines, ', I' + Token + 'Node');
      end else
      begin
        if IsInterface then
          WriteStr(InterfaceLines, 'I' + Token + 'Node') else
          WriteStr(InterfaceLines, 'T' + Token + 'Node');
        SomeAncestorFound := true;
      end;
    until false;
  end;

  if not SomeAncestorFound then
    if IsInterface then
      WriteStr(InterfaceLines, 'IX3DNode') else
      WriteStr(InterfaceLines, 'TX3DNode');

  Assert(Token = '{');

  WritelnStr(InterfaceLines, ')');

  if not IsInterface then
  begin
    WritelnStr(InterfaceLines,
      '  public' + NL +
      '    constructor Create(const AX3DName: string; const ABaseUrl: string); override;');

    if not IsAbstract then
      WritelnStr(InterfaceLines,
        '    class function ClassX3DType: string; override;' + NL +
        '    class function URNMatching(const URN: string): boolean; override;');

    WritelnStr(ImplementationLines,
      'constructor TNode' + NodeType + '.Create(const AX3DName: string;' + NL +
      '  const ABaseUrl: string);' + NL +
      'begin' + NL +
      '  inherited;');

    if not IsAbstract then
    begin
      if WasSomeNode then
        WriteStr(RegistrationLines, ',' + NL) else
        WasSomeNode := true;
      WriteStr(RegistrationLines, '    TNode' + NodeType);
    end;
  end;
end;

procedure ParseNodeInsideLine(const Line: string);
const
  BoolToStrLowerCase: array[boolean] of string=('false','true');
var
  FieldType, FieldName, AccessTypeName, EventInOrOut: string;
  AccessType: TX3DAccessType;
  SeekPos, I: Integer;
  NodeField: boolean;
  NodeFieldAllowedChildren, FieldDefaultValue, FieldComment, FieldExposedLine: string;
begin
  SeekPos := 1;

  FieldType := NextToken(Line, SeekPos, WhiteSpaces);
  NodeField := (FieldType = 'SFNode') or (FieldType = 'MFNode');

  if FieldType = '#' then
  begin
    { special case, this is not a field, just abort now }
    WritelnStr(InterfaceLines, NL + '    { ' + Line + '}');
    Exit;
  end;

  AccessTypeName := NextToken(Line, SeekPos, WhiteSpaces);
  if AccessTypeName = '[]' then
    AccessType := atInitializeOnly else
  if AccessTypeName = '[in]' then
    AccessType := atInputOnly else
  if AccessTypeName = '[out]' then
    AccessType := atOutputOnly else
  if AccessTypeName = '[in,out]' then
    AccessType := atInputOutput else
    Assert(false, 'unknown AccessTypeName "' + AccessTypeName + '"');

  FieldName := NextToken(Line, SeekPos, WhiteSpaces);
  { TODO: uppercase first letter of FieldName. While compiler ignores case,
    it's still better to use nice case, as users will see this at auto-completion. }

  while SCharIs(Line, SeekPos, WhiteSpaces) do Inc(SeekPos);

  { Parsing field's default value if the most tricky and ad-hoc implementation
    here. That's because we don't want to just parse the field using
    parsing routines in TX3DField unit --- this would loose some things
    like "Pi/2" expressions. So instead we employ text tricks that often
    work good enough.

    For example, for MF fields, it's enough to parse to matching "]" character.
    For SF fields (or MF fields with single init value, i.e. not starting
    with "[") field types gives us token count for NextToken.

    In case of problems, the unparsed field's value will be in "X3D spec comment",
    and manual fixing of Pascal include file will be needed.  }

  if SCharIs(Line, SeekPos, '[') then
  begin
    FieldDefaultValue := '[';
    Inc(SeekPos);
    while (SeekPos <= Length(Line)) and (Line[SeekPos] <> ']') do
    begin
      FieldDefaultValue += Line[SeekPos];
      Inc(SeekPos);
    end;
    FieldDefaultValue += ']';
    Inc(SeekPos);
  end else
  begin
    if (FieldType = 'SFVec2d') or
       (FieldType = 'SFVec2f') or
       (FieldType = 'MFVec2d') or
       (FieldType = 'MFVec2f') then
    begin
      FieldDefaultValue := 'Vector2Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (FieldType = 'SFColor') or
       (FieldType = 'SFVec3d') or
       (FieldType = 'SFVec3f') or
       (FieldType = 'MFColor') or
       (FieldType = 'MFVec3d') or
       (FieldType = 'MFVec3f') then
    begin
      FieldDefaultValue := 'Vector3Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (FieldType = 'SFColorRGBA') or
       (FieldType = 'SFVec4d') or
       (FieldType = 'SFVec4f') or
       (FieldType = 'MFColorRGBA') or
       (FieldType = 'MFVec4d') or
       (FieldType = 'MFVec4f') then
    begin
      FieldDefaultValue := 'Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (FieldType = 'SFMatrix4f') or
       (FieldType = 'SFMatrix4d') or
       (FieldType = 'MFMatrix4f') or
       (FieldType = 'MFMatrix4d') then
    begin
      FieldDefaultValue := 'Matrix4Single(';

      for I := 1 to 3 do
      begin
        FieldDefaultValue += '    Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
        FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '),' + NL;
      end;

      FieldDefaultValue += '    Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '));';
    end else
    begin
    if (FieldType = 'SFRotation') or
       (FieldType = 'MFRotation') then
    begin
      FieldDefaultValue := 'Vector3Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      FieldDefaultValue += '), ' + NextToken(Line, SeekPos, WhiteSpaces);
    end else
      FieldDefaultValue := NextToken(Line, SeekPos, WhiteSpaces);
    end;
  end;

  StringReplaceAllVar(FieldDefaultValue, '"', '''', false);
  { make booleans lowecase, I like them more lowercase }
  StringReplaceAllVar(FieldDefaultValue, 'FALSE', 'false', false);
  StringReplaceAllVar(FieldDefaultValue, 'TRUE', 'true', false);

  FieldComment := Trim(SEnding(Line, SeekPos));

  if AccessType in [atInputOnly, atOutputOnly] then
  begin
    if AccessType = atInputOnly then
      EventInOrOut := 'in' else
      EventInOrOut := 'out';
    if IsInterface then
      WritelnStr(InterfaceLines,
        '    { Event: ' + FieldType + ', ' + EventInOrOut + ' } { }' + NL +
        '    property Event' + FieldName + ': TX3DEvent { read GetEvent' + FieldName + ' };') else
    begin
      WritelnStr(InterfaceLines,
        NL +
        '    { Event: ' + FieldType + ', ' + EventInOrOut + ' } { }' + NL +
        '    private FEvent' + FieldName + ': TX3DEvent;' + NL +
        '    public property Event' + FieldName + ': TX3DEvent read FEvent' + FieldName + ';');

      WritelnStr(ImplementationLines,
        NL +
        '  FEvent' + FieldName + ' := TX3DEvent.Create(''' + FieldName + ''', T' + FieldType + ', ' + BoolToStrLowerCase[AccessType = atInputOnly] + ');' + NL +
        '  AddEvent(FEvent' + FieldName + ');');
    end;
  end else
  begin
    if IsInterface then
      WritelnStr(InterfaceLines,
        '    property Fd' + FieldName + ': T' + FieldType + ' { read GetFd' + FieldName + ' }; { }') else
    begin
      WritelnStr(InterfaceLines,
        NL +
        '    private FFd' + FieldName + ': T' + FieldType + ';' + NL +
        '    public property Fd' + FieldName + ': T' + FieldType + ' read FFd' + FieldName + ';');

      if AccessType = atInitializeOnly then
        FieldExposedLine := '  FFd' + FieldName + '.Exposed := false;' + NL else
        FieldExposedLine := '';

      if NodeField then
      begin
        { Although NULL is sensible only for SFNode and [] is sensible only
          for MFNode, X3D specification switches them in many places
          --- too many to fix them, it's easier to just ignore
          the difference here. }
        Assert( (FieldDefaultValue = 'NULL') or
                (FieldDefaultValue = '[]') );

        Assert(FieldComment <> '');
        Assert(FieldComment[1] = '[');
        Assert(FieldComment[Length(FieldComment)] = ']');
        NodeFieldAllowedChildren := Copy(FieldComment, 2, Length(FieldComment) - 2);

        WritelnStr(ImplementationLines,
          NL +
          '  FFd' + FieldName + ' := T' + FieldType + '.Create(Self, ''' + FieldName + ''', [' + NodeFieldAllowedChildren + ']);' + NL +
          FieldExposedLine +
          '  AddField(FFd' + FieldName + ');');
      end else
      begin
        WritelnStr(ImplementationLines,
          NL +
          '  FFd' + FieldName + ' := T' + FieldType + '.Create(''' + FieldName + ''', ' + FieldDefaultValue + ');' + NL +
          FieldExposedLine +
          '  AddField(FFd' + FieldName + ');');
        if FieldComment <> '' then
          WritelnStr(ImplementationLines,
            '  { X3D specification comment: ' + FieldComment + ' }');
      end;
    end;
  end;
end;

procedure ParseNodeEnd;
begin
  WritelnStr(InterfaceLines, '  end;' + NL);

  if not IsInterface then
  begin
    WritelnStr(ImplementationLines,
      NL +
      '  DefaultContainerField := ''children'';' + NL +
      'end;' + NL);
    if not IsAbstract then
      WritelnStr(ImplementationLines,
        'class function TNode' + NodeType + '.ClassX3DType: string;' + NL +
        'begin' + NL +
        '  Result := ''' + NodeType + ''';' + NL +
        'end;' + NL +
        NL +
        'class function TNode' + NodeType + '.URNMatching(const URN: string): boolean;' + NL +
        'begin' + NL +
        '  Result := (inherited URNMatching(URN)) or' + NL +
        '    (URN = URNX3DNodes + ClassX3DType);' + NL +
        'end;' + NL);
  end;
end;

var
  Line: string;
  ComponentName: string;
begin
  Parameters.CheckHigh(1);
  ComponentName := Parameters[1];

  try
    RegistrationLines := TMemoryStream.Create;
    InterfaceLines := TMemoryStream.Create;
    ImplementationLines := TMemoryStream.Create;

    WasSomeNode := false;

    while not Eof do
    begin
      Readln(Line);
      Line := Trim(Line);
      if Line <> '' then
      begin
        ParseNodeStartLine(Line);
        while not Eof do
        begin
          Readln(Line);
          Line := Trim(Line);
          if Line <> '}' then
            ParseNodeInsideLine(Line) else
            Break;
        end;
        ParseNodeEnd;
      end;
    end;

    WritelnStr({$I x3d_nodes_spec_to_pascal_preamble.inc});
    WritelnStr('{$ifdef read_interface}');
    InterfaceLines.Position := 0;
    InterfaceLines.SaveToStream(StdOutStream);
    WritelnStr('{$endif read_interface}' + NL +
               NL +
               '{$ifdef read_implementation}');
    ImplementationLines.Position := 0;
    ImplementationLines.SaveToStream(StdOutStream);
    WritelnStr('procedure Register' + ComponentName + 'Nodes;' + NL +
               'begin' + NL +
               '  NodesManager.RegisterNodeClasses([');
    RegistrationLines.Position := 0;
    RegistrationLines.SaveToStream(StdOutStream);
    WritelnStr(NL + '  ]);' + NL +
               'end;' + NL);
    WritelnStr('{$endif read_implementation}');
  finally
    FreeAndNil(RegistrationLines);
    FreeAndNil(InterfaceLines);
    FreeAndNil(ImplementationLines);
  end;
end.
