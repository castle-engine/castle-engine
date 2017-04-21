{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Processors that do the actual job of reading X3D nodes
  specification and generating output. }
unit GenerateProcessors;

interface

uses SysUtils, FGL,
  CastleClassUtils, CastleStringUtils, CastleTimeUtils, CastleLog,
  CastleColors, CastleUtils, CastleApplicationProperties;

type
  { X3D field/event access type.
    Copies from X3DNodes, not used, this is a litle more comfortable.
    Because when the x3d-nodes-to-pascal is tested, the X3DNodes unit
    is sometimes temporarily broken:) }
  TX3DAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);

  EInvalidSpecificationFile = class(Exception);

  TX3DNodeInformationList = class;

  TX3DNodeInformation = class
    X3DType: string;
    Ancestors: TX3DNodeInformationList;
    constructor Create;
    destructor Destroy; override;
    function PascalType(const ForceAsInterface: boolean = false): string;
    function IsInterface: boolean;
    function IsAbstract: boolean;
  end;

  TX3DNodeInformationList = class(specialize TFPGObjectList<TX3DNodeInformation>);

  TX3DFieldInformation = class
    X3DType: string;
    X3DName, PascalName, PascalNamePrefixed: string;
    X3DAccessType: string;
    IsEnumString: boolean;
    DefaultValue, Comment: string;
    function AccessType: TX3DAccessType;
    function PascalClass: string;
    { The type for a helper property, which can be something simple
      like "Single" for SFFloat. Can be '' if no helper property. }
    function PascalHelperType: string;
    function IsNode: boolean;
  end;

  TProcessor = class abstract
  strict private
    { Field is SFString with a strictly limited set of values. }
    class function FieldIsEnumString(const Line: string; const X3DFieldType: string): boolean;
  public
    procedure ProcessFile(const InputFileName: string);
    procedure NodeBegin(const Node: TX3DNodeInformation); virtual;
    procedure NodeField(const Node: TX3DNodeInformation;
      const Field: TX3DFieldInformation); virtual;
    procedure NodeEnd(const Node: TX3DNodeInformation); virtual;
    procedure ComponentEnd(const ComponentName: string); virtual;
  end;

  THelperProcessor = class(TProcessor)
  strict private
    OutputPrivateInterface, OutputPublicInterface, OutputImplementation: string;
  public
    procedure NodeField(const Node: TX3DNodeInformation;
      const Field: TX3DFieldInformation); override;
    procedure NodeEnd(const Node: TX3DNodeInformation); override;
  end;

  { Output Pascal code that for each X3D node defines appropriate Pascal class
    (with appropriate fields, events, field default values, etc.).
    Generated Pascal class can be directly used inside unit like X3DNodes
    to allow our engine to recognize and parse all fields of given node. }
  TTemplateProcessor = class(TProcessor)
  strict private
    OutputInterface, OutputImplementation, OutputRegistration: string;
    SomeNodeRegistered: boolean;
    IsInterface: boolean;
    IsAbstract: boolean;
  public
    procedure NodeBegin(const Node: TX3DNodeInformation); override;
    procedure NodeField(const Node: TX3DNodeInformation;
      const Field: TX3DFieldInformation); override;
    procedure NodeEnd(const Node: TX3DNodeInformation); override;
    procedure ComponentEnd(const ComponentName: string); override;
  end;

var
  Verbose: boolean;

implementation

procedure WritelnVerbose(const S: string);
begin
  if Verbose then
    Writeln(ErrOutput, 'VERBOSE NOTE: ' + S);
end;

{ TX3DNodeInformation -------------------------------------------------------- }

constructor TX3DNodeInformation.Create;
begin
  inherited;
  Ancestors := TX3DNodeInformationList.Create;
end;

destructor TX3DNodeInformation.Destroy;
begin
  FreeAndNil(Ancestors);
  inherited;
end;

function TX3DNodeInformation.IsInterface: boolean;
begin
  Result := IsSuffix('Object', X3DType);
end;

function TX3DNodeInformation.IsAbstract: boolean;
begin
  Result := IsPrefix('X3D', X3DType);
end;

function TX3DNodeInformation.PascalType(const ForceAsInterface: boolean): string;
begin
  Result := X3DType;

  // replace X3D prefix with Abstract prefix
  if IsAbstract then
  begin
    { On X3DViewpointNode, we have both
      TAbstractX3DViewpointNode and TAbstractViewpointNode,
      to support also older VRML versions. Similar for grouping. }
    if (X3DType <> 'X3DViewpointNode') and
       (X3DType <> 'X3DGroupingNode') then
      Result := PrefixRemove('X3D', Result, true);
    Result := 'Abstract' + Result;
  end;

  // always end with Node suffix
  Result := SuffixRemove('Node', Result, true);
  Result := Result + 'Node';

  if ForceAsInterface or IsInterface then
    Result := 'I' + Result
  else
    Result := 'T' + Result;
end;

{ TX3DFieldInformation ------------------------------------------------------- }

function TX3DFieldInformation.AccessType: TX3DAccessType;
begin
  if X3DAccessType = '[]' then
    AccessType := atInitializeOnly
  else
  if X3DAccessType = '[in]' then
    AccessType := atInputOnly
  else
  if X3DAccessType = '[out]' then
    AccessType := atOutputOnly
  else
  if X3DAccessType = '[in,out]' then
    AccessType := atInputOutput
  else
    raise EInvalidSpecificationFile.Create('Unrecognized field access type "' + X3DAccessType + '"');
end;

function TX3DFieldInformation.IsNode: boolean;
begin
  Result := (X3DType = 'SFNode') or (X3DType = 'MFNode');
end;

function TX3DFieldInformation.PascalClass: string;
begin
  Result := 'T' + X3DType;
end;

function TX3DFieldInformation.PascalHelperType: string;
begin
  if X3DType = 'SFFloat' then
    Result := 'Single' else
  if X3DType = 'SFDouble' then
    Result := 'Double' else
  if X3DType = 'SFTime' then
    Result := 'TFloatTime' else
  if X3DType = 'SFVec2f' then
    Result := 'TVector2Single' else
  if X3DType = 'SFVec3f' then
    Result := 'TVector3Single' else
  if X3DType = 'SFVec4f' then
    Result := 'TVector4Single' else
  if X3DType = 'SFVec2d' then
    Result := 'TVector2Double' else
  if X3DType = 'SFVec3d' then
    Result := 'TVector3Double' else
  if X3DType = 'SFVec4d' then
    Result := 'TVector4Double' else
  if X3DType = 'SFInt32' then
    Result := 'Integer' else
  if X3DType = 'SFBool' then
    Result := 'boolean' else
  if X3DType = 'SFRotation' then
    Result := 'TVector4Single' else
  if X3DType = 'SFColor' then
    Result := 'TCastleColorRGB' else
  if X3DType = 'SFColorRGBA' then
    Result := 'TCastleColor' else
  // Note that many SFString are enums, and they should be converted to enums
  // in ObjectPascal. We capture enums outside of this function.
  if X3DType = 'SFString' then
    Result := 'string' else
  if X3DType = 'SFMatrix3f' then
    Result := 'TMatrix3Single' else
  if X3DType = 'SFMatrix4f' then
    Result := 'TMatrix4Single' else
//  if X3DType = 'SFNode' then // nope, because these should be typed accordingly in ObjectPascal
//    Result := 'TXxx' else
    Result := '';
end;

{ TProcessor ----------------------------------------------------------------- }

{ Field is SFString with a strictly limited set of values. }
class function TProcessor.FieldIsEnumString(const Line: string; const X3DFieldType: string): boolean;
begin
  Result :=
    (X3DFieldType = 'SFString') and
    (Pos('["', Line) <> 0) and
   ((Pos('"]', Line) <> 0) or (Pos('...]', Line) <> 0));
end;

procedure TProcessor.ProcessFile(const InputFileName: string);

  { Parse field information.
    Note that the Line parameters should receive the LineWithComment value. }
  procedure ParseField(const Field: TX3DFieldInformation; const Line: string);
  var
    SeekPos, I: Integer;
  begin
    SeekPos := 1;

    Field.X3DType := NextToken(Line, SeekPos, WhiteSpaces);
    if Field.X3DType = '' then
      raise EInvalidSpecificationFile.Create('Missing X3D field type on line: ' + Line);

    Field.X3DAccessType := NextToken(Line, SeekPos, WhiteSpaces);
    if Field.X3DAccessType = '' then
      raise EInvalidSpecificationFile.Create('Missing X3D field access type on line: ' + Line);

    Field.X3DName := NextToken(Line, SeekPos, WhiteSpaces);
    if Field.X3DName = '' then
      raise EInvalidSpecificationFile.Create('Missing X3D field name on line: ' + Line);

    Field.PascalName := Field.X3DName;
    { rename some field names to avoid collisions }
    if Field.PascalName = 'on' then
      Field.PascalName := 'IsOn'
    else
    if Field.PascalName = 'name' then
      Field.PascalName := 'NameField';
    Field.PascalName[1] := UpCase(Field.PascalName[1]);

    Field.PascalNamePrefixed := Field.X3DName;
    Field.PascalNamePrefixed[1] := UpCase(Field.PascalNamePrefixed[1]);
    if Field.AccessType in [atInputOnly, atOutputOnly] then
      Field.PascalNamePrefixed := 'Event' + Field.PascalNamePrefixed
    else
      Field.PascalNamePrefixed := 'Fd' + Field.PascalNamePrefixed;

    Field.IsEnumString := FieldIsEnumString(Line, Field.X3DType);

    { Parsing field's default value is a bit tricky.
      We can't just parse the field using parsing routines in TX3DField unit,
      as we want to preserve expressions like "Pi/2" unevaluated.

      For MF fields, it's enough to parse to matching "]" character.
      For SF fields (or MF fields with single init value, i.e. not starting
      with "["), the field type determines the token count for default value.

      In case of problems, the unparsed field's value will be in Field.Comment.  }

    if SCharIs(Line, SeekPos, '[') then
    begin
      Field.DefaultValue := '[';
      Inc(SeekPos);
      while (SeekPos <= Length(Line)) and (Line[SeekPos] <> ']') do
      begin
        Field.DefaultValue += Line[SeekPos];
        Inc(SeekPos);
      end;
      Field.DefaultValue += ']';
      Inc(SeekPos);
    end else
    if (Field.X3DType = 'SFVec2d') or
       (Field.X3DType = 'SFVec2f') or
       (Field.X3DType = 'MFVec2d') or
       (Field.X3DType = 'MFVec2f') then
    begin
      Field.DefaultValue := 'Vector2Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFColor') or
       (Field.X3DType = 'SFVec3d') or
       (Field.X3DType = 'SFVec3f') or
       (Field.X3DType = 'MFColor') or
       (Field.X3DType = 'MFVec3d') or
       (Field.X3DType = 'MFVec3f') then
    begin
      Field.DefaultValue := 'Vector3Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFColorRGBA') or
       (Field.X3DType = 'SFVec4d') or
       (Field.X3DType = 'SFVec4f') or
       (Field.X3DType = 'MFColorRGBA') or
       (Field.X3DType = 'MFVec4d') or
       (Field.X3DType = 'MFVec4f') then
    begin
      Field.DefaultValue := 'Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFMatrix4f') or
       (Field.X3DType = 'SFMatrix4d') or
       (Field.X3DType = 'MFMatrix4f') or
       (Field.X3DType = 'MFMatrix4d') then
    begin
      Field.DefaultValue := 'Matrix4Single(';

      for I := 1 to 3 do
      begin
        Field.DefaultValue += '    Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '),' + NL;
      end;

      Field.DefaultValue += '    Vector4Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '));';
    end else
    begin
    if (Field.X3DType = 'SFRotation') or
       (Field.X3DType = 'MFRotation') then
    begin
      Field.DefaultValue := 'Vector3Single(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += '), ' + NextToken(Line, SeekPos, WhiteSpaces);
    end else
      Field.DefaultValue := NextToken(Line, SeekPos, WhiteSpaces);
    end;

    StringReplaceAllVar(Field.DefaultValue, '"', '''', false);
    { make booleans lowecase, I like them more lowercase }
    StringReplaceAllVar(Field.DefaultValue, 'FALSE', 'false', false);
    StringReplaceAllVar(Field.DefaultValue, 'TRUE', 'true', false);

    Field.Comment := Trim(SEnding(Line, SeekPos));
  end;

var
  F: TTextReader;
  PosComment, I: Integer;
  Tokens: TCastleStringList;
  Line, LineWithComment: string;
  Node, Ancestor: TX3DNodeInformation;
  Field: TX3DFieldInformation;
begin
  Node := nil;

  F := TTextReader.Create(InputFileName);
  try
    while not F.Eof do
    begin
      LineWithComment := F.Readln;
      Line := LineWithComment;
      { remove comments }
      PosComment := Pos('#', Line);
      if PosComment <> 0 then
        SetLength(Line, PosComment - 1);
      { avoid empty lines (after comment removal) }
      if Trim(Line) <> '' then
      begin
        Tokens := CreateTokens(Line);
        try
          { node start }
          if (Tokens.Count >= 2) and
             (Tokens[Tokens.Count - 1] = '{') and
             ((Tokens.Count = 2) or (Tokens[1] = ':')) then
          begin
            FreeAndNil(Node);
            Node := TX3DNodeInformation.Create;
            Node.X3DType := Tokens[0];
            for I := 2 to Tokens.Count - 2 do
            begin
              Ancestor := TX3DNodeInformation.Create;
              Ancestor.X3DType := SuffixRemove(',', Tokens[I], true);
              Node.Ancestors.Add(Ancestor);
            end;
            NodeBegin(Node);
          end else

          { node end }
          if (Tokens.Count = 1) and
             (Tokens[0] = '}') then
          begin
            NodeEnd(Node);
            FreeAndNil(Node);
          end else

          { field/event inside node }
          begin
            Field := TX3DFieldInformation.Create;
            try
              ParseField(Field, LineWithComment);
              if Node = nil then
              begin
                WritelnWarning('Input', 'Field found, but not inside a node: ' + Field.X3DName);
                Continue;
              end;
              NodeField(Node, Field);
            finally FreeAndNil(Field) end;
          end;
        finally FreeAndNil(Tokens) end;
      end;
    end;
  finally FreeAndNil(F) end;

  FreeAndNil(Node);

  ComponentEnd(DeleteFileExt(ExtractFileName(InputFileName)));
end;

procedure TProcessor.NodeBegin(const Node: TX3DNodeInformation);
begin
end;

procedure TProcessor.NodeField(const Node: TX3DNodeInformation;
  const Field: TX3DFieldInformation);
begin
end;

procedure TProcessor.NodeEnd(const Node: TX3DNodeInformation);
begin
end;

procedure TProcessor.ComponentEnd(const ComponentName: string);
begin
end;

{ THelperProcessor ----------------------------------------------------------- }

procedure THelperProcessor.NodeField(const Node: TX3DNodeInformation;
  const Field: TX3DFieldInformation);
begin
  if Field.IsEnumString then
    Exit;
  if (Field.X3DName = 'solid') or
     (Field.X3DName = 'repeatS') or
     (Field.X3DName = 'repeatT') or
     (Field.X3DName = 'cycleInterval') or
     ((Field.X3DName = 'position') and (Node.X3DType = 'Viewpoint')) or
     ((Field.X3DName = 'position') and (Node.X3DType = 'OrthoViewpoint')) or
     ((Field.X3DName = 'position') and (Node.X3DType = 'GeoViewpoint')) or
     ((Field.X3DName = 'orientation') and (Node.X3DType = 'X3DViewpointNode')) or
     ((Field.X3DName = 'magnificationFilter') and (Node.X3DType = 'TextureProperties')) or
     ((Field.X3DName = 'minificationFilter') and (Node.X3DType = 'TextureProperties')) or
     (Field.X3DName = 'linetype')
     // TODO: bboxCenter and bboxSize should also be removed from here someday,
     // we should convert them manually to BBox: TBox3D to support our TBox3D type.
     then
  begin
    WritelnVerbose('Not processing, this field has special implementation: ' + Field.X3DName);
    Exit;
  end;
  if (Node.X3DType = 'X3DMetadataObject') or
     (Node.X3DType = 'X3DFogObject') or
     (Node.X3DType = 'X3DPickableObject') or
     (Node.X3DType = 'LOD') then
  begin
    WritelnVerbose('Not processing, this node has special implementation: ' + Node.X3DType);
    Exit;
  end;
  if (Field.X3DAccessType <> '[in,out]') and
     (Field.X3DAccessType <> '[]') then
  begin
    WritelnVerbose('Only fields (inputOutput or initializeOnly) are supported now: ' + Field.X3DName);
    Exit;
  end;
  if Field.PascalHelperType = '' then
    Exit;

  OutputPrivateInterface +=
    '    function Get' + Field.PascalName + ': ' + Field.PascalHelperType + ';' + NL +
    '    procedure Set' + Field.PascalName + '(const Value: ' + Field.PascalHelperType + ');' + NL;
  OutputPublicInterface +=
    '    property ' + Field.PascalName + ': ' + Field.PascalHelperType + ' read Get' + Field.PascalName + ' write Set' + Field.PascalName + ';' + NL;
  OutputImplementation +=
    'function ' + Node.PascalType + '.Get' + Field.PascalName + ': ' + Field.PascalHelperType + ';' + NL +
    'begin' + NL +
    '  Result := ' + Field.PascalNamePrefixed + '.Value;' + NL +
    'end;' + NL +
    NL +
    'procedure ' + Node.PascalType + '.Set' + Field.PascalName + '(const Value: ' + Field.PascalHelperType + ');' + NL +
    'begin' + NL +
    '  ' + Field.PascalNamePrefixed + '.Send(Value);' + NL +
    'end;' + NL +
    NL;
end;

procedure THelperProcessor.NodeEnd(const Node: TX3DNodeInformation);

  procedure GenerateOutput(const OutputInterface, OutputImplementation: string);
  var
    OutputFileName: string;
  begin
    OutputFileName := '../../auto_generated_node_helpers/x3dnodes_' +
      LowerCase(Node.X3DType) + '.inc';

    StringToFile(OutputFileName,
      '{ -*- buffer-read-only: t -*-' + NL +
      '' + NL +
      '  Copyright 2015-2017 Michalis Kamburelis.' + NL +
      '' + NL +
      '  This file is part of "Castle Game Engine".' + NL +
      '' + NL +
      '  "Castle Game Engine" is free software; see the file COPYING.txt,' + NL +
      '  included in this distribution, for details about the copyright.' + NL +
      '' + NL +
      '  "Castle Game Engine" is distributed in the hope that it will be useful,' + NL +
      '  but WITHOUT ANY WARRANTY; without even the implied warranty of' + NL +
      '  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + NL +
      '' + NL +
      '  ----------------------------------------------------------------------------' + NL +
      '}' + NL +
      '' + NL +
      '{ Automatically generated node properties.' + NL +
      '' + NL +
      '  Do not edit this file manually!' + NL +
      '  To add new properties:' + NL +
      '  - add them to the text files in nodes_specification/components/ ,' + NL +
      '  - and regenerate include files by running' + NL +
      '    nodes_specification/x3d-nodes-to-pascal/x3d-nodes-to-pascal.lpr .' + NL +
      '' + NL +
      '  The documentation for properties should go to x3dnodes_documentation.txt . }' + NL +
      '' + NL +
      '{$ifdef read_interface}' + NL +
      NL +
      // 'type' + NL +
      OutputInterface +
      '{$endif read_interface}' + NL +
      '' + NL +
      '{$ifdef read_implementation}' + NL +
      NL +
      OutputImplementation +
      '{$endif read_implementation}' + NL
    );
  end;

begin
  if (OutputPrivateInterface = '') or
     (OutputPublicInterface = '') or
     (OutputImplementation = '') then
    WritelnVerbose('Node does not have any helpers (for now), generating empty include file: ' + Node.X3DType);

  if OutputPrivateInterface <> '' then
    OutputPrivateInterface := '  private' + NL + OutputPrivateInterface;
  if OutputPublicInterface <> '' then
    OutputPublicInterface := '  public' + NL + OutputPublicInterface;

  // no helpers for interfaces
  if Node.IsInterface then
    Exit;

  GenerateOutput(
    // '  ' + Node.PascalType + 'Helper = class helper for ' + Node.PascalType + NL +
    OutputPrivateInterface +
    OutputPublicInterface +
    // '  end;' + NL +
    NL,
    '{ ' + Node.PascalType + ' ----------------------------------------------- }' + NL +
    NL +
    OutputImplementation);

  OutputPrivateInterface := '';
  OutputPublicInterface := '';
  OutputImplementation := '';
end;

{ TTemplateProcessor ----------------------------------------------------------- }

procedure TTemplateProcessor.NodeBegin(const Node: TX3DNodeInformation);
var
  I: Integer;
begin
  OutputInterface += '  ' + Node.PascalType + ' = ';
  if Node.IsInterface then
    OutputInterface += 'interface'
  else
    OutputInterface += 'class';
  OutputInterface += '(';

  if Node.Ancestors.Count = 0 then
  begin
    if Node.IsInterface then
      OutputInterface += 'IX3DNode'
    else
      OutputInterface += 'TX3DNode';
  end else
  begin
    for I := 0 to Node.Ancestors.Count - 1 do
    begin
      if I <> 0 then
        OutputInterface += ', ';
      OutputInterface += Node.Ancestors[I].PascalType(Node.IsInterface or (I <> 0));
    end;
  end;

  OutputInterface += ')' + NL;

  if not IsInterface then
  begin
    OutputInterface +=
      '  public' + NL +
      '    constructor Create(const AX3DName: string; const ABaseUrl: string); override;' + NL;

    if not IsAbstract then
      OutputInterface +=
        '    class function ClassX3DType: string; override;' + NL +
        '    class function URNMatching(const URN: string): boolean; override;' + NL;

    OutputImplementation +=
      'constructor ' + Node.PascalType + '.Create(const AX3DName: string;' + NL +
      '  const ABaseUrl: string);' + NL +
      'begin' + NL +
      '  inherited;' + NL;

    if not IsAbstract then
    begin
      if SomeNodeRegistered then
        OutputRegistration += ',' + NL
      else
        SomeNodeRegistered := true;
      OutputRegistration += '    ' + Node.PascalType;
    end;
  end;
end;

procedure TTemplateProcessor.NodeField(const Node: TX3DNodeInformation;
  const Field: TX3DFieldInformation);
var
  EventInOrOut: string;
  NodeFieldAllowedChildren, FieldExposedLine: string;
begin
  if Field.AccessType in [atInputOnly, atOutputOnly] then
  begin
    if Field.AccessType = atInputOnly then
      EventInOrOut := 'in'
    else
      EventInOrOut := 'out';
    if IsInterface then
      OutputInterface +=
        '    { Event: ' + Field.X3DType + ', ' + EventInOrOut + ' } { }' + NL +
        '    property ' + Field.PascalNamePrefixed + ': TX3DEvent { read Get' + Field.PascalNamePrefixed + ' };' + NL else
    begin
      OutputInterface +=
        NL +
        '    { Event: ' + Field.X3DType + ', ' + EventInOrOut + ' } { }' + NL +
        '    private F' + Field.PascalNamePrefixed + ': TX3DEvent;' + NL +
        '    public property ' + Field.PascalNamePrefixed + ': TX3DEvent read F' + Field.PascalNamePrefixed + ';' + NL;

      OutputImplementation +=
        NL +
        '  F' + Field.PascalNamePrefixed + ' := TX3DEvent.Create(''' + Field.X3DName + ''', ' + Field.PascalClass + ', ' + LowerCase(BoolToStr(Field.AccessType = atInputOnly, true)) + ');' + NL +
        '  AddEvent(F' + Field.PascalNamePrefixed + ');' + NL;
    end;
  end else
  begin
    if IsInterface then
      OutputInterface +=
        '    property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ' { read Get' + Field.PascalNamePrefixed + ' }; { }' + NL else
    begin
      OutputInterface +=
        NL +
        '    private F' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ';' + NL +
        '    public property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ' read F' + Field.PascalNamePrefixed + ';' + NL;

      if Field.AccessType = atInitializeOnly then
        FieldExposedLine := '  F' + Field.PascalNamePrefixed + '.Exposed := false;' + NL else
        FieldExposedLine := '';

      if Field.IsNode then
      begin
        { Although NULL is sensible only for SFNode and [] is sensible only
          for MFNode, X3D specification switches them in many places
          --- too many to fix them, it's easier to just ignore
          the difference here. }
        Assert( (Field.DefaultValue = 'NULL') or
                (Field.DefaultValue = '[]') );

        Assert(Field.Comment <> '');
        Assert(Field.Comment[1] = '[');
        Assert(Field.Comment[Length(Field.Comment)] = ']');
        NodeFieldAllowedChildren := Copy(Field.Comment, 2, Length(Field.Comment) - 2);

        OutputImplementation +=
          NL +
          '  F' + Field.PascalNamePrefixed + ' := ' + Field.PascalClass + '.Create(Self, ''' + Field.X3DName + ''', [' + NodeFieldAllowedChildren + ']);' + NL +
          FieldExposedLine +
          '  AddField(F' + Field.PascalNamePrefixed + ');' + NL;
      end else
      begin
        OutputImplementation +=
          NL +
          '  F' + Field.PascalNamePrefixed + ' := ' + Field.PascalClass + '.Create(''' + Field.X3DName + ''', ' + Field.DefaultValue + ');' + NL +
          FieldExposedLine +
          '  AddField(F' + Field.PascalNamePrefixed + ');' + NL;
        if Field.Comment <> '' then
          OutputImplementation +=
            '  { X3D specification comment: ' + Field.Comment + ' }' + NL;
      end;
    end;
  end;
end;

procedure TTemplateProcessor.NodeEnd(const Node: TX3DNodeInformation);
begin
  OutputInterface +='  end;' + NL + NL;

  if not Node.IsInterface then
  begin
    OutputImplementation +=
      NL +
      '  DefaultContainerField := ''children'';' + NL +
      'end;' + NL + NL;
    if not Node.IsAbstract then
      OutputImplementation +=
        'class function ' + Node.PascalType + '.ClassX3DType: string;' + NL +
        'begin' + NL +
        '  Result := ''' + Node.X3DType + ''';' + NL +
        'end;' + NL +
        NL +
        'class function ' + Node.PascalType + '.URNMatching(const URN: string): boolean;' + NL +
        'begin' + NL +
        '  Result := (inherited URNMatching(URN)) or' + NL +
        '    (URN = URNX3DNodes + ClassX3DType);' + NL +
        'end;' + NL + NL;
  end;
end;

procedure TTemplateProcessor.ComponentEnd(const ComponentName: string);
begin
  Writeln(
    '{$ifdef read_interface}' + NL +
    OutputInterface + NL +
    '{$endif read_interface}' + NL +
    NL +
    '{$ifdef read_implementation}' + NL +
    OutputImplementation + NL +
    'procedure Register' + ComponentName + 'Nodes;' + NL +
    'begin' + NL +
    '  NodesManager.RegisterNodeClasses([' + NL +
    OutputRegistration + NL +
    '  ]);' + NL +
    'end;' + NL +
    '{$endif read_implementation}');

  OutputInterface := '';
  OutputImplementation := '';
  OutputRegistration := '';
  SomeNodeRegistered := false;
end;

end.
