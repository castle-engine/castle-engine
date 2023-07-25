// -*- compile-command: "castle-engine compile && cd .. && ./run.sh" -*-
{
  Copyright 2015-2023 Michalis Kamburelis.

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

uses SysUtils, Generics.Collections, Classes,
  CastleClassUtils, CastleStringUtils, CastleTimeUtils, CastleLog,
  CastleColors, CastleUtils, CastleApplicationProperties, CastleFilesUtils,
  CastleDownload;

type
  { X3D field/event access type.
    Copies from X3DNodes, not used, this is a litle more comfortable.
    Because when the x3d-nodes-to-pascal is tested, the X3DNodes unit
    is sometimes temporarily broken:) }
  TX3DAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);

  EInvalidSpecificationFile = class(Exception);

  TX3DNodeInformationList = class;
  TX3DFieldInformationList = class;

  TX3DNodeInformation = class
    X3DType: string;
    Ancestors: TX3DNodeInformationList;
    { DefaultContainerField for XML encoding.
      Leave empty to use ancestor's DefaultContainerField value. }
    DefaultContainerField: string;
    { All fields within this node. }
    Fields: TX3DFieldInformationList;
    Vrml1: Boolean;
    constructor Create;
    destructor Destroy; override;
    function PascalType(const ForceAsFunctionality: boolean = false): string;
    function IsFunctionality: boolean;
    function IsAbstract: boolean;
  end;

  TX3DNodeInformationList = class(specialize TObjectList<TX3DNodeInformation>)
    function PascalTypesList: string;
  end;

  TX3DFieldInformation = class
    X3DType: string;
    X3DName, PascalName, PascalNamePrefixed: string;
    X3DAccessType: string;
    EnumType, EnumNames, EnumDefault: String;
    DefaultValue, Comment: string;
    AllowedChildrenNodes: TX3DNodeInformationList;
    NotSlim: Boolean;
    ChangeAlways: String; //< Value of ChangeAlways for this field, as String
    Range: String; //< Allowed field values, in format specific to given field type
    Documentation: String;
    SetterBefore: String;
    { Set TSFFloat/TMFFloat.Angle (to make it treated as an angle for UNIT conversions. }
    Angle: Boolean;
    WeakLink: Boolean;
    Vrml1EnumNames: String;
    Vrml1BitMaskNames: String;
    constructor Create;
    destructor Destroy; override;
    function AccessType: TX3DAccessType;
    function PascalClass: string;

    { Type of a helper property to get and set this field.
      It can be something simple like 'Single' for SFFloat.
      Can be '' if no helper property. }
    function PascalHelperType: string;

    { Types of a helper public setter method to set this field. }
    procedure PascalSetterTypes(const Names: TCastleStringList);

    { Field is of SFNode or MFNode type. }
    function IsNode: boolean;

    { Pascal preprocessor conditional expression to include/exclude this field.
      These methods either return empty string, or a line terminated by NL. }
    function ConditionsBegin: String;
    function ConditionsEnd: String;

    { Field is SFString / MFString with a strictly limited set of values. }
    function IsEnumString: Boolean;

    { Field type is numeric, and range indicates it must be >= 0. }
    function MustBeNonnegative: Boolean;

    { Call when the field is completely parsed, e.g. Range is known. }
    procedure Finished;
  end;

  TX3DFieldInformationList = class(specialize TObjectList<TX3DFieldInformation>)
  end;

  TProcessor = class abstract
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
    OutputPrivateInterface, OutputPublicInterface,
      OutputImplementation, OutputCreateImplementation: string;
  public
    procedure NodeBegin(const Node: TX3DNodeInformation); override;
    procedure NodeField(const Node: TX3DNodeInformation;
      const Field: TX3DFieldInformation); override;
    procedure NodeEnd(const Node: TX3DNodeInformation); override;
  end;

var
  Verbose: Boolean;

  { When will the .inc files be generated.
    May but doesn't have to end with PathDelim. }
  OutputPath: String = '../../../src/scene/x3d/auto_generated_node_helpers/';

implementation

uses DateUtils;

function DocToPascal(const S: String): String;
begin
  if Pos('}', S) <> 0 then
    raise Exception.Create('Node documentation cannot use "}" character (Pascal comment closing char) for now');
  Result := Trim(S);
end;

procedure WritelnVerbose(const S: string);
begin
  if Verbose then
    Writeln(ErrOutput, 'VERBOSE NOTE: ' + S);
end;

procedure AddAutoGeneratedField(
  const Node: TX3DNodeInformation;
  const Field: TX3DFieldInformation;
  var OutputInterface, OutputImplementation: string);
var
  FieldImplementationComment, AllowedChildrenNodesStr: String;
  FieldConfigure, FieldExposed, FieldDefaultValue: string;
begin
  FieldConfigure := '';

  if Field.AccessType in [atInputOnly, atOutputOnly] then
  begin
    if Node.IsFunctionality then
    begin
      OutputInterface +=
        Field.ConditionsBegin +
        '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
        '    // property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ';' + NL +
        Field.ConditionsEnd;
    end else
    begin
      OutputInterface +=
        NL +
        Field.ConditionsBegin +
        '    strict private F' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ';' + NL +
        '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
        '    public property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ' read F' + Field.PascalNamePrefixed + ';' + NL +
        Field.ConditionsEnd;

      OutputImplementation +=
        NL +
        Field.ConditionsBegin +
        '  F' + Field.PascalNamePrefixed + ' := ' + Field.PascalClass + '.Create(Self, ''' + Field.X3DName + ''', ' + LowerCase(BoolToStr(Field.AccessType = atInputOnly, true)) + ');' + NL +
        '  AddEvent(F' + Field.PascalNamePrefixed + ');' + NL +
        Field.ConditionsEnd;
    end;
  end else
  begin
    if Node.IsFunctionality then
    begin
      OutputInterface +=
        Field.ConditionsBegin +
        '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
        '    // property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ';' + NL +
        Field.ConditionsEnd;
    end else
    begin
      OutputInterface +=
        NL +
        Field.ConditionsBegin +
        '    strict private F' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ';' + NL +
        '    { Internal wrapper for property @code(' + Field.PascalName + '). This wrapper API may change, we advise to access simpler @code(' + Field.PascalName + ') instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }' + NL +
        '    public property ' + Field.PascalNamePrefixed + ': ' + Field.PascalClass + ' read F' + Field.PascalNamePrefixed + ';' + NL +
        Field.ConditionsEnd;

      FieldConfigure += '   ' + Field.PascalNamePrefixed + '.ChangeAlways := ' + Field.ChangeAlways + ';' + NL;
      if Field.Angle then
        FieldConfigure += '   ' + Field.PascalNamePrefixed + '.Angle := true;' + NL;
      if Field.MustBeNonnegative then
        FieldConfigure += '   ' + Field.PascalNamePrefixed + '.MustBeNonnegative := true;' + NL;
      if Field.WeakLink then
        FieldConfigure += '   ' + Field.PascalNamePrefixed + '.WeakLink := true;' + NL;
      if Field.SetterBefore <> '' then
        FieldConfigure += '   ' + Field.PascalNamePrefixed + '.OnBeforeValueChange := {$ifdef FPC}@{$endif}' + Field.SetterBefore + ';' + NL;

      FieldExposed := BoolToStr(Field.AccessType = atInputOutput, true);

      if Field.Comment <> '' then
        FieldImplementationComment := '  { X3D specification comment: ' + SReplaceChars(Field.Comment, ['{', '}'], '_') + ' }' + NL
      else
        FieldImplementationComment := '';

      if Field.IsNode then
      begin
        { range: X3DUrlObject means that we limit the node by TUrlFunctionality,
          and TMFNode.Create accepts only single argument (not a list) in this case.
          For now just hardcode this case. }
        if (Field.AllowedChildrenNodes.Count = 1) and
           (Field.AllowedChildrenNodes[0].X3DType = 'X3DUrlObject') then
          AllowedChildrenNodesStr := Field.AllowedChildrenNodes.PascalTypesList
        else
          AllowedChildrenNodesStr := '[' + Field.AllowedChildrenNodes.PascalTypesList + ']';
        OutputImplementation +=
          NL +
          Field.ConditionsBegin +
          '  F' + Field.PascalNamePrefixed + ' := ' + Field.PascalClass + '.Create(Self, ' + FieldExposed + ', ''' + Field.X3DName + ''', ' + AllowedChildrenNodesStr + ');' + NL +
          FieldConfigure +
          '  AddField(F' + Field.PascalNamePrefixed + ');' + NL +
          FieldImplementationComment +
          Field.ConditionsEnd;
      end else
      begin
        { calculate FieldDefaultValue }
        if Field.X3DType = 'SFEnum' then
          FieldDefaultValue := '@' + Field.Vrml1EnumNames + ', Ord(High(' + Field.Vrml1EnumNames + ')) + 1, ' + Field.DefaultValue
        else
        if Field.X3DType = 'SFBitMask' then
          FieldDefaultValue := Field.Vrml1BitMaskNames + ', ' + Field.DefaultValue
        else
        if Field.IsEnumString then
          FieldDefaultValue := '@' + Field.EnumNames + ', Ord(High(' + Field.EnumNames + ')) + 1, Ord(' + Field.EnumDefault + ')'
        else
          FieldDefaultValue := Field.DefaultValue;

        OutputImplementation +=
          NL +
          Field.ConditionsBegin +
          '  F' + Field.PascalNamePrefixed + ' := ' + Field.PascalClass + '.Create(Self, ' + FieldExposed + ', ''' + Field.X3DName + ''', ' + FieldDefaultValue + ');' + NL +
          FieldConfigure +
          '  AddField(F' + Field.PascalNamePrefixed + ');' + NL +
          FieldImplementationComment +
          Field.ConditionsEnd;
      end;
    end;
  end;
end;

{ TX3DNodeInformation -------------------------------------------------------- }

constructor TX3DNodeInformation.Create;
begin
  inherited;
  Ancestors := TX3DNodeInformationList.Create;
  Fields := TX3DFieldInformationList.Create;
end;

destructor TX3DNodeInformation.Destroy;
begin
  FreeAndNil(Ancestors);
  FreeAndNil(Fields);
  inherited;
end;

function TX3DNodeInformation.IsFunctionality: boolean;
begin
  Result := IsSuffix('Object', X3DType);
end;

function TX3DNodeInformation.IsAbstract: boolean;
begin
  Result := IsPrefix('X3D', X3DType) or IsPrefix('AbstractVrml1', X3DType);
end;

function TX3DNodeInformation.PascalType(const ForceAsFunctionality: boolean): string;
begin
  Result := X3DType;

  { If this looks like a TXxxNode, then assume it is already a Pascal name spelled explicitly
    in txt file. }
  if IsPrefix('T', X3DType, false) and
     (
       IsSuffix('Node', X3DType, false) or
       IsSuffix('Node_1', X3DType, false) or
       IsSuffix('Node_2', X3DType, false)
     ) then
    Exit;

  if ForceAsFunctionality or IsFunctionality then
  begin
    // change 'X3DUrlObject' into 'TUrlFunctionality'
    Result := PrefixRemove('X3D', Result, true);
    // Result := SuffixRemove('Node', Result, true); // not needed
    Result := SuffixRemove('Object', Result, true);
    Result := 'T' + Result + 'Functionality';
  end else
  begin
    // replace X3D / AbstractVrml1 prefix with Abstract prefix
    if IsAbstract then
    begin
      Result := PrefixRemove('X3D', Result, true);
      Result := PrefixRemove('AbstractVrml1', Result, true);
      Result := 'Abstract' + Result;
    end;

    // always end with Node suffix
    Result := SuffixRemove('Node', Result, true);
    Result := Result + 'Node';

    // avoid TAbstractMetadataObjectNode, make it just TAbstractMetadataNode
    if IsSuffix('ObjectNode', Result) then
      Result := SuffixRemove('ObjectNode', Result, true) + 'Node';

    // add _1 to VRML 1 nodes, to not conflict with node names from VRML 2 / X3D
    if Vrml1 then
      Result := Result + '_1';

    Result := 'T' + Result;
  end;
end;

{ TX3DNodeInformationList ---------------------------------------------------- }

function TX3DNodeInformationList.PascalTypesList: string;
var
  I: Integer;
begin
  Result := '';
  if Count <> 0 then
  begin
    for I := 0 to Count - 2 do
      Result += Items[I].PascalType + ', ';
    Result += Items[Count - 1].PascalType;
  end;
end;

{ TX3DFieldInformation ------------------------------------------------------- }

constructor TX3DFieldInformation.Create;
begin
  inherited;
  AllowedChildrenNodes := TX3DNodeInformationList.Create;
  ChangeAlways := 'chVisibleNonGeometry'; // default
end;

destructor TX3DFieldInformation.Destroy;
begin
  FreeAndNil(AllowedChildrenNodes);
  inherited;
end;

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
  if AccessType in [atInputOnly, atOutputOnly] then
    Result := 'T' + X3DType + 'Event'
  else
  if (X3DType = 'SFString') and IsEnumString then
    Result := 'TSFStringEnum'
  else
  if (X3DType = 'MFString') and IsEnumString then
    Result := 'TMFStringEnum' // for things like TFontStyleNode.Justify
  else
    Result := 'T' + X3DType;
end;

function TX3DFieldInformation.PascalHelperType: string;
begin
  if (X3DType = 'SFString') and IsEnumString and (EnumType <> '') then
    Result := EnumType
  else
  if X3DType = 'SFFloat' then
    Result := 'Single'
  else
  if X3DType = 'SFDouble' then
    Result := 'Double'
  else
  if X3DType = 'SFTime' then
    Result := 'TFloatTime'
  else
  if X3DType = 'SFVec2f' then
    Result := 'TVector2'
  else
  if X3DType = 'SFVec3f' then
    Result := 'TVector3'
  else
  if X3DType = 'SFVec4f' then
    Result := 'TVector4'
  else
  if X3DType = 'SFVec2d' then
    Result := 'TVector2Double'
  else
  if X3DType = 'SFVec3d' then
    Result := 'TVector3Double'
  else
  if X3DType = 'SFVec4d' then
    Result := 'TVector4Double'
  else
  if X3DType = 'SFInt32' then
    Result := 'Integer'
  else
  if X3DType = 'SFBool' then
    Result := 'Boolean'
  else
  if X3DType = 'SFRotation' then
    Result := 'TVector4'
  else
  if X3DType = 'SFColor' then
    Result := 'TCastleColorRGB'
  else
  if X3DType = 'SFColorRGBA' then
    Result := 'TCastleColor'
  else
  if X3DType = 'SFString'then
    Result := 'String'
  else
  if X3DType = 'SFMatrix3f' then
    Result := 'TMatrix3'
  else
  if X3DType = 'SFMatrix4f' then
    Result := 'TMatrix4'
  else
  if X3DType = 'SFMatrix3d' then
    Result := 'TMatrix3Double'
  else
  if X3DType = 'SFMatrix4d' then
    Result := 'TMatrix4Double'
  else
//  if X3DType = 'SFNode' then // these are converted to Pascal using AllowedPascalClass
//    Result := 'TXxx'
//  else
    Result := '';
end;

procedure TX3DFieldInformation.PascalSetterTypes(const Names: TCastleStringList);
begin
  if (X3DType = 'MFString') and IsEnumString and (EnumType <> '') then
  begin
    Names.Add('array of ' + EnumType);
    Names.Add('T' + EnumType + 'List');
  end else
  if X3DType = 'MFFloat' then
  begin
    Names.Add('array of Single');
    Names.Add('TSingleList');
  end else
  if X3DType = 'MFDouble' then
  begin
    Names.Add('array of Double');
    Names.Add('TDoubleList');
  end else
  if X3DType = 'MFTime' then
  begin
    Names.Add('array of TFloatTime');
    Names.Add('TDoubleList');
  end else
  if X3DType = 'MFVec2f' then
  begin
    Names.Add('array of TVector2');
    Names.Add('TVector2List');
  end else
  if X3DType = 'MFVec3f' then
  begin
    Names.Add('array of TVector3');
    Names.Add('TVector3List');
  end else
  if X3DType = 'MFVec4f' then
  begin
    Names.Add('array of TVector4');
    Names.Add('TVector4List');
  end else
  if X3DType = 'MFVec2d' then
  begin
    Names.Add('array of TVector2Double');
    Names.Add('TVector2DoubleList');
  end else
  if X3DType = 'MFVec3d' then
  begin
    Names.Add('array of TVector3Double');
    Names.Add('TVector3DoubleList');
  end else
  if X3DType = 'MFVec4d' then
  begin
    Names.Add('array of TVector4Double');
    Names.Add('TVector4DoubleList');
  end else
  if X3DType = 'MFInt32' then
  begin
    Names.Add('array of Int32');
    Names.Add('TInt32List');
  end else
  if X3DType = 'MFBool' then
  begin
    Names.Add('array of boolean');
    Names.Add('TBooleanList');
  end else
  if X3DType = 'MFRotation' then
  begin
    Names.Add('array of TVector4');
    Names.Add('TVector4List');
  end else
  if X3DType = 'MFColor' then
  begin
    Names.Add('array of TCastleColorRGB');
    Names.Add('TVector3List');
  end else
  if X3DType = 'MFColorRGBA' then
  begin
    Names.Add('array of TCastleColor');
    Names.Add('TVector4List');
  end else
  if X3DType = 'MFString' then
  begin
    Names.Add('array of string');
    Names.Add('TCastleStringList');
  end else
  if X3DType = 'MFMatrix3f' then
  begin
    Names.Add('array of TMatrix3');
    Names.Add('TMatrix3List');
  end else
  if X3DType = 'MFMatrix4f' then
  begin
    Names.Add('array of TMatrix4');
    Names.Add('TMatrix4List');
  end else
  if X3DType = 'MFMatrix3d' then
  begin
    Names.Add('array of TMatrix3Double');
    Names.Add('TMatrix3DoubleList');
  end else
  if X3DType = 'MFMatrix4d' then
  begin
    Names.Add('array of TMatrix4Double');
    Names.Add('TMatrix4DoubleList');
  end;
end;

function TX3DFieldInformation.ConditionsBegin: String;
begin
  if NotSlim then
    Result := '{$ifndef CASTLE_SLIM_NODES}' + NL
  else
    Result := '';
end;

function TX3DFieldInformation.ConditionsEnd: String;
begin
  if NotSlim then
    Result := '{$endif not CASTLE_SLIM_NODES}' + NL
  else
    Result := '';
end;

function TX3DFieldInformation.IsEnumString: Boolean;
begin
  { TODO: MFString is not yet detected as IsEnumString,
    as our TMFStringEnum is not yet implemented. }
  Result :=
   ((X3DType = 'SFString') {or (X3DType = 'MFString')}) and
    IsPrefix('["', Range, false) and
   (IsSuffix('"]', Range, false) or
    IsSuffix('...]', Range, false));
end;

procedure TX3DFieldInformation.Finished;
var
  I: Integer;
  AllowedChildrenNodesSplitted: TCastleStringList;
  AllowedChildren: TX3DNodeInformation;
begin
  if IsNode and
     (AccessType in [atInitializeOnly, atInputOutput]) then
  begin
    { Although NULL is sensible only for SFNode and [] is sensible only
      for MFNode, X3D specification switches them in many places
      --- too many to fix them, it's easier to just ignore
      the difference here. }
    if (DefaultValue <> 'NULL') and
       (DefaultValue <> '[]') then
      raise EInvalidSpecificationFile.CreateFmt('Invalid default SFNode / MFNode value for "%s": %s', [
        X3DName,
        DefaultValue
      ]);

    if IsPrefix('[', Range, false) or
       IsSuffix(']', Range, false) then
      raise EInvalidSpecificationFile.Create('Do not surround SFNode / MFNode range in [...] anymore: ' + X3DName);

    { in case of SFNode / MFNode, convert the Range into
      NodeAllowedChildren }
    AllowedChildrenNodesSplitted := CreateTokens(Range, WhiteSpaces + [',', '|']);
    try
      for I := 0 to AllowedChildrenNodesSplitted.Count - 1 do
      begin
        AllowedChildren := TX3DNodeInformation.Create;
        AllowedChildren.X3DType := AllowedChildrenNodesSplitted[I];
        AllowedChildrenNodes.Add(AllowedChildren);
      end;
    finally FreeAndNil(AllowedChildrenNodesSplitted) end;
  end;
end;

function TX3DFieldInformation.MustBeNonnegative: Boolean;
begin
  Result :=
    (
      (X3DType = 'SFFloat') or
      (X3DType = 'SFLong') or
      (X3DType = 'SFInt32')
    ) and (
      (Range = '(0,Inf)') or
      (Range = '[0,Inf)') or
      (Range = '(0,1)') or
      (Range = '[0,1)') or
      (Range = '(0,1]') or
      (Range = '[0,1]')
    );
end;

{ TProcessor ----------------------------------------------------------------- }

procedure TProcessor.ProcessFile(const InputFileName: string);

  { Parse field information.
    Note that the Line parameters should receive the LineWithComment value.
    Node contents are read-only in this routine. }
  procedure ParseField(const Field: TX3DFieldInformation;
    const Node: TX3DNodeInformation; const Line: string);
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
    if (Field.PascalName = 'type') and
       ( (Node.X3DType = 'ShaderPart') or
         (Node.X3DType = 'EffectPart') or
         (Node.X3DType = 'ShaderProgram') ) then
      Field.PascalName := 'ShaderType'
    else
    if Field.PascalName = 'name' then
      Field.PascalName := 'NameField'
    else
    if Field.PascalName = 'string' then
      Field.PascalName := 'Text' { standard Pascal name for such properties };
    Field.PascalName[1] := UpCase(Field.PascalName[1]);

    Field.PascalNamePrefixed := Field.X3DName;
    Field.PascalNamePrefixed[1] := UpCase(Field.PascalNamePrefixed[1]);
    if Field.AccessType in [atInputOnly, atOutputOnly] then
      Field.PascalNamePrefixed := 'Event' + Field.PascalNamePrefixed
    else
      Field.PascalNamePrefixed := 'Fd' + Field.PascalNamePrefixed;

    { Parsing field's default value is a bit tricky.
      We can't just parse the field using parsing routines in TX3DField unit,
      as we want to preserve expressions like "Pi/2" unevaluated.

      For MF fields, it's enough to parse to matching "]" character.
      For SF fields (or MF fields with single init value, i.e. not starting
      with "["), the field type determines the token count for default value.

      In case of problems, the unparsed field's value will be in Field.Comment.  }

    // skip whitespace, to make following test for '[' useful
    while SCharIs(Line, SeekPos, WhiteSpaces) do
      Inc(SeekPos);

    if SCharIs(Line, SeekPos, '[') then
    { parse from [ to matching ] }
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
    { parse from " to matching " }
    if SCharIs(Line, SeekPos, '"') then
    begin
      Field.DefaultValue := '"';
      Inc(SeekPos);
      while (SeekPos <= Length(Line)) and (Line[SeekPos] <> '"') do
      begin
        Field.DefaultValue += Line[SeekPos];
        Inc(SeekPos);
      end;
      Field.DefaultValue += '"';
      Inc(SeekPos);
    end else
    if (Field.X3DType = 'SFVec2f') or
       (Field.X3DType = 'MFVec2f') then
    begin
      Field.DefaultValue := 'Vector2(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFVec2d') or
       (Field.X3DType = 'MFVec2d') then
    begin
      Field.DefaultValue := 'Vector2Double(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFColor') or
       (Field.X3DType = 'SFVec3f') or
       (Field.X3DType = 'MFColor') or
       (Field.X3DType = 'MFVec3f') then
    begin
      Field.DefaultValue := 'Vector3(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFVec3d') or
       (Field.X3DType = 'MFVec3d') then
    begin
      Field.DefaultValue := 'Vector3Double(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFColorRGBA') or
       (Field.X3DType = 'SFVec4f') or
       (Field.X3DType = 'MFColorRGBA') or
       (Field.X3DType = 'MFVec4f') then
    begin
      Field.DefaultValue := 'Vector4(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFVec4d') or
       (Field.X3DType = 'MFVec4d') then
    begin
      Field.DefaultValue := 'Vector4Double(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + ')';
    end else
    if (Field.X3DType = 'SFMatrix3f') or
       (Field.X3DType = 'SFMatrix3d') or
       (Field.X3DType = 'MFMatrix3f') or
       (Field.X3DType = 'MFMatrix3d') then
    begin
      Field.DefaultValue := 'Matrix3(';

      if NextTokenOnce(Line, SeekPos, WhiteSpaces) = 'identity' then
      begin
        case Field.X3DType of
          'SFMatrix3f': Field.DefaultValue := 'TMatrix3.Identity';
          'SFMatrix3d': Field.DefaultValue := 'TMatrix3Double.Identity';
        end;

        // just to advance SeekPos
        NextToken(Line, SeekPos, WhiteSpaces);
      end else
      begin
        for I := 1 to 2 do
        begin
          Field.DefaultValue += '    Vector3(' + NextToken(Line, SeekPos, WhiteSpaces);
          Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
          Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '),' + NL;
        end;

        Field.DefaultValue += '    Vector3(' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '));';
      end;
    end else
    if (Field.X3DType = 'SFMatrix4f') or
       (Field.X3DType = 'SFMatrix') or // VRML 1 matrix type
       (Field.X3DType = 'SFMatrix4d') or
       (Field.X3DType = 'MFMatrix4f') or
       (Field.X3DType = 'MFMatrix4d') then
    begin
      Field.DefaultValue := 'Matrix4(';

      if NextTokenOnce(Line, SeekPos, WhiteSpaces) = 'identity' then
      begin
        case Field.X3DType of
          'SFMatrix4f': Field.DefaultValue := 'TMatrix4.Identity';
          'SFMatrix': Field.DefaultValue := 'TMatrix4.Identity';
          'SFMatrix4d': Field.DefaultValue := 'TMatrix4Double.Identity';
        end;

        // just to advance SeekPos
        NextToken(Line, SeekPos, WhiteSpaces);
      end else
      begin
        for I := 1 to 3 do
        begin
          Field.DefaultValue += '    Vector4(' + NextToken(Line, SeekPos, WhiteSpaces);
          Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
          Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
          Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '),' + NL;
        end;

        Field.DefaultValue += '    Vector4(' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
        Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces) + '));';
      end;
    end else
    if (Field.X3DType = 'SFRotation') or
       (Field.X3DType = 'MFRotation') then
    begin
      Field.DefaultValue := 'Vector3(' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += ', ' + NextToken(Line, SeekPos, WhiteSpaces);
      Field.DefaultValue += '), ' + NextToken(Line, SeekPos, WhiteSpaces);
    end else
    begin
      Field.DefaultValue := NextToken(Line, SeekPos, WhiteSpaces);
    end;

    StringReplaceAllVar(Field.DefaultValue, '"', '''', false);
    { make booleans lowecase, I like them more lowercase }
    StringReplaceAllVar(Field.DefaultValue, 'FALSE', 'false', false);
    StringReplaceAllVar(Field.DefaultValue, 'TRUE', 'true', false);

    Field.Comment := Trim(SEnding(Line, SeekPos));

    // cut off initial '# ' from Field.Comment
    if SCharIs(Field.Comment, 1, '#') then
      Field.Comment := Trim(SEnding(Field.Comment, 2));

    if Field.Comment <> '' then
      raise EInvalidSpecificationFile.CreateFmt('Comments at field lines are not allowed, as they usually indicate you should split this into separate lines for change,not-slim etc.: %s.%s', [
        Node.X3DType,
        Field.X3DName
      ]);
  end;

  { Generate output and free given Node. }
  procedure FinishPreviousNode(var Node: TX3DNodeInformation);
  var
    Field: TX3DFieldInformation;
  begin
    if Node <> nil then
    begin
      NodeBegin(Node);
      for Field in Node.Fields do
      begin
        Field.Finished;
        NodeField(Node, Field);
      end;
      NodeEnd(Node);
    end;
    FreeAndNil(Node);
  end;

var
  Node: TX3DNodeInformation;
  LastField: TX3DFieldInformation; // last field processed (in the current Node)

  procedure ValidatePerNodeCommand(const Command: String);
  begin
    if Node = nil then
      raise EInvalidSpecificationFile.CreateFmt('"%s" can only be used within a node', [Command]);
    if LastField <> nil then
      raise EInvalidSpecificationFile.CreateFmt('"%s" can only be used before any fields are declared', [Command]);
  end;

  procedure ValidatePerFieldCommand(const Command: String);
  begin
    if Node = nil then
      raise EInvalidSpecificationFile.CreateFmt('"%s" can only be used within a node', [Command]);
    if LastField = nil then
      raise EInvalidSpecificationFile.CreateFmt('"%s" can only be used when a field was declared earlier', [Command]);
  end;

  { Simple approach to read multiline string from text file.
    Assumes that it must end with a line with only """ (optionally surrounded by whitespaces,
    but nothing more, not even comments). }
  function ReadMultilineString(const F: TTextReader): String;
  var
    S: String;
  begin
    Result := '';
    repeat
      if F.Eof then
        raise EInvalidSpecificationFile.Create('Unexpected end of file in the middle of multiline string (""")');
      S := F.Readln;
      if Trim(S) = '"""' then
        Exit;
      { Note: do not Trim(S) below, to not break indent in @longCode
        used within X3D fields docs, like BlendingSort,
        file:///home/michalis/sources/castle-engine/castle-engine/doc/reference/X3DNodes.TNavigationInfoNode.html#BlendingSort }
      Result := Result + S + NL;
    until false;
  end;

var
  F: TTextReader;
  PosComment, I: Integer;
  Tokens: TCastleStringList;
  Line, LineWithComment: string;
  Ancestor: TX3DNodeInformation;
begin
  Node := nil;

  WritelnVerbose('Processing ' + InputFileName);
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
            FinishPreviousNode(Node);
            Node := TX3DNodeInformation.Create;
            Node.X3DType := Tokens[0];
            for I := 2 to Tokens.Count - 2 do
            begin
              Ancestor := TX3DNodeInformation.Create;
              Ancestor.X3DType := SuffixRemove(',', Tokens[I], true);
              Node.Ancestors.Add(Ancestor);
            end;
            LastField := nil; // reset at node start
          end else

          { node end }
          if (Tokens.Count = 1) and
             (Tokens[0] = '}') then
          begin
            FinishPreviousNode(Node);
          end else

          { per-node commands }
          if (Tokens.Count = 2) and
             (Tokens[0] = 'default-container-field:') then
          begin
            ValidatePerNodeCommand('default-container-field');
            Node.DefaultContainerField := Tokens[1];
          end else

          if (Tokens.Count = 1) and
             (Tokens[0] = 'vrml1') then
          begin
            ValidatePerNodeCommand('vrml1');
            Node.Vrml1 := true;
          end else

          { per-field commands }
          if (Tokens.Count = 1) and
             (Tokens[0] = 'not-slim') then
          begin
            ValidatePerFieldCommand('not-slim');
            LastField.NotSlim := true;
          end else

          if (Tokens.Count = 1) and
             (Tokens[0] = 'angle') then
          begin
            ValidatePerFieldCommand('angle');
            LastField.Angle := true;
          end else

          if (Tokens.Count = 1) and
             (Tokens[0] = 'weak-link') then
          begin
            ValidatePerFieldCommand('weak-link');
            LastField.WeakLink := true;
          end else

          if (Tokens.Count = 2) and
             (Tokens[0] = 'change:') then
          begin
            ValidatePerFieldCommand('change');
            LastField.ChangeAlways := Tokens[1];
          end else

          if Tokens[0] = 'range:' then
          begin
            ValidatePerFieldCommand('range');
            LastField.Range := Trim(PrefixRemove('range:', Trim(Line), false));
          end else

          if (Tokens.Count = 4) and
             (Tokens[0] = 'enumerated-type:') then
          begin
            ValidatePerFieldCommand('enumerated-type');
            if not LastField.IsEnumString then
              raise EInvalidSpecificationFile.CreateFmt('enumerated-type only available for SFString / MFString fields detected as enumerated, but used with %s', [
                LastField.X3DName
              ]);
            LastField.EnumType := Tokens[1];
            LastField.EnumNames := Tokens[2];
            LastField.EnumDefault := Tokens[3];
          end else

          if (Tokens.Count = 2) and
             (Tokens[0] = 'enumerated-type-vrml1:') then
          begin
            ValidatePerFieldCommand('enumerated-type-vrml1');
            if LastField.X3DType <> 'SFEnum' then
              raise EInvalidSpecificationFile.CreateFmt('enumerated-type-vrml1 only available for SFEnum fields, but used with %s', [
                LastField.X3DName
              ]);
            LastField.Vrml1EnumNames := Tokens[1];
          end else

          if Tokens[0] = 'bit-mask-vrml1:' then
          begin
            ValidatePerFieldCommand('bit-mask-vrml1');
            if LastField.X3DType <> 'SFBitMask' then
              raise EInvalidSpecificationFile.CreateFmt('bit-mask-vrml1 only available for SFBitMask fields, but used with %s', [
                LastField.X3DName
              ]);
            LastField.Vrml1BitMaskNames := Trim(PrefixRemove('bit-mask-vrml1:', Trim(Line), false));
          end else

          if Tokens[0] = 'doc:' then
          begin
            ValidatePerFieldCommand('doc');
            LastField.Documentation := Trim(PrefixRemove('doc:', Trim(Line), false));
            if LastField.Documentation = '"""' then
              LastField.Documentation := ReadMultilineString(F);
          end else

          if Tokens[0] = 'setter-before:' then
          begin
            ValidatePerFieldCommand('setter-before');
            LastField.SetterBefore := Trim(PrefixRemove('setter-before:', Trim(Line), false));
          end else

          { field/event inside node }
          begin
            if Node = nil then
              raise EInvalidSpecificationFile.Create('Found line that declares a field/event (as it was not recognized as anything else) but we are not inside any node ' + LineWithComment);
            LastField := TX3DFieldInformation.Create;
            Node.Fields.Add(LastField);
            ParseField(LastField, Node, LineWithComment);
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

procedure THelperProcessor.NodeBegin(const Node: TX3DNodeInformation);
begin
  OutputPublicInterface +=
    { CreateNode needs some doc, to avoid using template
      "Automatically generated node properties."
      as accidental documentation comment. }
    '    { Create node fields and events. }' + NL +
    '    procedure CreateNode; override;' + NL +
    '    class function ClassX3DType: string; override;' + NL;

  if Node.Vrml1 then
  begin
    OutputPublicInterface +=
      '    class function ForVRMLVersion(const Version: TX3DVersion): Boolean; override;' + NL;
    OutputCreateImplementation +=
      'class function ' + Node.PascalType + '.ForVRMLVersion(const Version: TX3DVersion): Boolean;' + NL +
      'begin' + NL +
      '  Result := Version.Major <= 1; // parse this node only in VRML 1.0 and Inventor files' + NL +
      'end;' + NL +
      NL;
  end;

  OutputCreateImplementation +=
    'class function ' + Node.PascalType + '.ClassX3DType: String;' + NL +
    'begin' + NL +
    '  Result := ''' + Node.X3DType + ''';' + NL +
    'end;' + NL +
    NL;

  OutputCreateImplementation +=
    'procedure ' + Node.PascalType + '.CreateNode;' + NL +
    'begin' + NL +
    '  inherited;' + NL;
end;

procedure THelperProcessor.NodeField(const Node: TX3DNodeInformation;
  const Field: TX3DFieldInformation);
var
  AllowedPascalClass, SetterType: string;
  SetterTypes: TCastleStringList;
begin
  AddAutoGeneratedField(Node, Field, OutputPublicInterface, OutputCreateImplementation);

  if Field.IsEnumString and (Field.EnumType = '') then
    Exit;
  if (Field.X3DName = 'solid') or
     (Field.X3DName = 'linetype') or
     (Field.X3DName = 'bboxSize') or // this is accounted already by writing out bboxCenter as public BBox

     (Node.X3DType + '.' + Field.X3DName = 'Viewpoint.position') or
     (Node.X3DType + '.' + Field.X3DName = 'OrthoViewpoint.position') or
     (Node.X3DType + '.' + Field.X3DName = 'GeoViewpoint.position') or
     (Node.X3DType + '.' + Field.X3DName = 'TextureProperties.magnificationFilter') or
     (Node.X3DType + '.' + Field.X3DName = 'TextureProperties.minificationFilter') or
     (Node.X3DType + '.' + Field.X3DName = 'HAnimHumanoid.skinCoord') or
     (Node.X3DType + '.' + Field.X3DName = 'FontStyle.family') or // in both VRML = 1 and >= 2, these have special helpers
     (Node.Vrml1 and (Node.X3DType + '.' + Field.X3DName = 'FontStyle.style')) or // in VRML = 1, this has special helpers
     ((not Node.Vrml1) and (Node.X3DType + '.' + Field.X3DName = 'FontStyle.justify')) or // in VRML >= 2, this has special helpers
     // ScreenFontStyle does not have better helpers yet, but it will, just like FontStyle
     (Node.X3DType + '.' + Field.X3DName = 'ScreenFontStyle.family') or
     (Node.X3DType + '.' + Field.X3DName = 'ScreenFontStyle.justify') or
     // These are deprecated, and a bit confusing (these are arrays that should have 0 or 1 items, to override "orientation")
     (Node.X3DType + '.' + Field.X3DName = 'X3DViewpointNode.direction') or
     (Node.X3DType + '.' + Field.X3DName = 'X3DViewpointNode.up') or

     false // keep this line, to allow easily rearranging lines above
     then
  begin
    WritelnVerbose('Not processing, this field has special implementation: ' + Node.X3DType + '.' + Field.X3DName);
    Exit;
  end;
  if (Node.X3DType = 'X3DFogObject') or
     (Node.X3DType = 'X3DPickableObject') or
     (Node.X3DType = 'LOD') then
  begin
    WritelnVerbose('Not processing, this entire node has special implementation: ' + Node.X3DType);
    Exit;
  end;
  if (Field.X3DAccessType <> '[in,out]') and
     (Field.X3DAccessType <> '[]') then
  begin
    { We don't generate any special helpers for events now, and they don't seem necessary. }
    // too verbose, and this is normal -- not an alarming situation
    //WritelnVerbose('Getters / setters are only generated for fields (inputOutput or initializeOnly) now, omitting event: ' + Field.X3DName);
    Exit;
  end;

  if Field.IsNode then
  begin
    { All the conditions below may be eventually removed.
      We're just not ready for it yet, the generated code is not ready for them. }
    if (Field.AllowedChildrenNodes.Count = 1) and
       (not Field.AllowedChildrenNodes[0].IsFunctionality) then
    begin
      AllowedPascalClass := Field.AllowedChildrenNodes[0].PascalType;
      if Field.X3DType = 'SFNode' then
      begin
        OutputPrivateInterface +=
          Field.ConditionsBegin +
          '    function Get' + Field.PascalName + ': ' + AllowedPascalClass + ';' + NL +
          '    procedure Set' + Field.PascalName + '(const Value: ' + AllowedPascalClass + ');' + NL +
          Field.ConditionsEnd;
        OutputPublicInterface +=
          Field.ConditionsBegin +
          '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
          '    property ' + Field.PascalName + ': ' + AllowedPascalClass + ' read Get' + Field.PascalName + ' write Set' + Field.PascalName + ';' + NL +
          Field.ConditionsEnd;
        OutputImplementation +=
          Field.ConditionsBegin +
          'function ' + Node.PascalType + '.Get' + Field.PascalName + ': ' + AllowedPascalClass + ';' + NL +
          'begin' + NL +
          '  if ' + Field.PascalNamePrefixed + '.Value is ' + AllowedPascalClass + ' then' + NL +
          '    Result := ' + AllowedPascalClass + '(' + Field.PascalNamePrefixed + '.Value)' + NL +
          '  else' + NL +
          '    Result := nil;' + NL +
          'end;' + NL +
          NL +
          'procedure ' + Node.PascalType + '.Set' + Field.PascalName + '(const Value: ' + AllowedPascalClass + ');' + NL +
          'begin' + NL +
          '  ' + Field.PascalNamePrefixed + '.Send(Value);' + NL +
          'end;' + NL +
          NL +
          Field.ConditionsEnd;
      end else
      if Field.X3DType = 'MFNode' then
      begin
        OutputPublicInterface +=
          Field.ConditionsBegin +
          '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
          '    procedure Set' + Field.PascalName + '(const Value: array of ' + AllowedPascalClass + ');' + NL +
          Field.ConditionsEnd;
        OutputImplementation +=
          Field.ConditionsBegin +
          'procedure ' + Node.PascalType + '.Set' + Field.PascalName + '(const Value: array of ' + AllowedPascalClass + ');' + NL +
          'var' + NL +
          '  L: Integer;' + NL +
          '  A: array of TX3DNode;' + NL +
          'begin' + NL +
          Iff(Field.SetterBefore <> '', '  ' + Field.SetterBefore + '(Value);' + NL, '') +
          '  L := High(Value) + 1;' + NL +
          '  SetLength(A, L);' + NL +
          '  if L > 0 then' + NL +
          '    Move(Value[0], A[0], L * SizeOf(' + AllowedPascalClass + '));' + NL +
          '  ' + Field.PascalNamePrefixed + '.Send(A);' + NL +
          'end;' + NL +
          NL +
          Field.ConditionsEnd;
      end;
    end;
  end else
  if Field.X3DName = 'bboxCenter' then
  begin
    { Define BBox property that gets/sets both bboxCenter and bboxSize at the same time }
    OutputPrivateInterface +=
      Field.ConditionsBegin +
      '    function GetBBox: TBox3D;' + NL +
      '    procedure SetBBox(const Value: TBox3D);' + NL +
      Field.ConditionsEnd;
    OutputPublicInterface +=
      Field.ConditionsBegin +
      '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
      '    property BBox: TBox3D read GetBBox write SetBBox;' + NL +
      Field.ConditionsEnd;
    OutputImplementation +=
      Field.ConditionsBegin +
      'function ' + Node.PascalType + '.GetBBox: TBox3D;' + NL +
      'begin' + NL +
      '  Result := TBox3D.FromCenterSize(FdBBoxCenter.Value, FdBBoxSize.Value);' + NL +
      'end;' + NL +
      NL +
      'procedure ' + Node.PascalType + '.SetBBox(const Value: TBox3D);' + NL +
      'var' + NL +
      '  ValueCenter, ValueSize: TVector3;' + NL +
      'begin' + NL +
      Iff(Field.SetterBefore <> '', '  ' + Field.SetterBefore + '(Value);' + NL, '') +
      '  Value.ToCenterSize(ValueCenter, ValueSize);' + NL +
      '  FdBBoxCenter.Send(ValueCenter);' + NL +
      '  FdBBoxSize.Send(ValueSize);' + NL +
      'end;' + NL +
      NL +
      Field.ConditionsEnd;
  end else
  begin
    if Field.PascalHelperType <> '' then
    begin
      OutputPrivateInterface +=
        Field.ConditionsBegin +
        '    function Get' + Field.PascalName + ': ' + Field.PascalHelperType + ';' + NL +
        '    procedure Set' + Field.PascalName + '(const Value: ' + Field.PascalHelperType + ');' + NL +
        Field.ConditionsEnd;
      OutputPublicInterface +=
        Field.ConditionsBegin +
        '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
        '    property ' + Field.PascalName + ': ' + Field.PascalHelperType + ' read Get' + Field.PascalName + ' write Set' + Field.PascalName + ';' + NL +
        Field.ConditionsEnd;
      OutputImplementation +=
        Field.ConditionsBegin +
        'function ' + Node.PascalType + '.Get' + Field.PascalName + ': ' + Field.PascalHelperType + ';' + NL +
        'begin' + NL +
        Iff(Field.IsEnumString,
        '  Result := ' + Field.EnumType + '(' + Field.PascalNamePrefixed + '.EnumValue);',
        '  Result := ' + Field.PascalNamePrefixed + '.Value;') + NL +
        'end;' + NL +
        NL +
        'procedure ' + Node.PascalType + '.Set' + Field.PascalName + '(const Value: ' + Field.PascalHelperType + ');' + NL +
        'begin' + NL +
        Iff(Field.SetterBefore <> '', '  ' + Field.SetterBefore + '(Value);' + NL, '') +
        Iff(Field.IsEnumString,
        '  ' + Field.PascalNamePrefixed + '.SendEnumValue(Ord(Value));',
        '  ' + Field.PascalNamePrefixed + '.Send(Value);') + NL +
        'end;' + NL +
        NL +
        Field.ConditionsEnd;
    end;

    SetterTypes := TCastleStringList.Create;
    try
      Field.PascalSetterTypes(SetterTypes);
      for SetterType in SetterTypes do
      begin
        OutputPublicInterface +=
          Field.ConditionsBegin +
          '    { ' + DocToPascal(Field.Documentation) + ' }' + NL +
          '    procedure Set' + Field.PascalName + '(const Value: ' + SetterType + ');' +
          Iff(SetterTypes.Count > 1,' overload;', '')  + NL + // Delphi need overload when we have more than one setter wit hte same name
          Field.ConditionsEnd;
        OutputImplementation +=
          Field.ConditionsBegin +
          'procedure ' + Node.PascalType + '.Set' + Field.PascalName + '(const Value: ' + SetterType + ');' + NL +
          'begin' + NL +
          Iff(Field.SetterBefore <> '', '  ' + Field.SetterBefore + '(Value);' + NL, '') +
          '  ' + Field.PascalNamePrefixed + '.Send(Value);' + NL +
          'end;' + NL +
          NL +
          Field.ConditionsEnd;
      end;
    finally FreeAndNil(SetterTypes) end;
  end;
end;

function CopyrightYears: string;
const
  YearBegin = '2015-';
var
  BuildDate: TDateTime;
  SourceDateEpoch: string;
begin
  { Look at SOURCE_DATE_EPOCH to support reproducible builds,
    https://wiki.debian.org/ReproducibleBuilds/TimestampsProposal
    https://reproducible-builds.org/specs/source-date-epoch/ }
  SourceDateEpoch := GetEnvironmentVariable('SOURCE_DATE_EPOCH');
  if SourceDateEpoch = '' then
    BuildDate := Now
  else
    BuildDate := UnixToDateTime(StrToInt(SourceDateEpoch));
  Result := YearBegin + IntToStr(YearOf(BuildDate));
end;

procedure THelperProcessor.NodeEnd(const Node: TX3DNodeInformation);

  procedure GenerateOutput(const OutputInterface, OutputImplementation: string);
  var
    OutputFileName: string;
  begin
    OutputFileName := InclPathDelim(OutputPath) +
      'x3dnodes_' + LowerCase(Node.X3DType) +
      Iff(Node.Vrml1, '_1', '') + '.inc';

    StringToFile(OutputFileName,
      '{ -*- buffer-read-only: t -*-' + NL +
      '' + NL +
      '  Copyright ' + CopyrightYears + ' Michalis Kamburelis.' + NL +
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
      '  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,' + NL +
      '  - and regenerate include files by running x3d-nodes-to-pascal }' + NL +
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
  if Node.DefaultContainerField <> '' then
    OutputCreateImplementation +=
      NL +
      '  DefaultContainerField := ''' + Node.DefaultContainerField + ''';' + NL;
  OutputCreateImplementation +=
    'end;' + NL +
    NL;
  OutputImplementation += OutputCreateImplementation;

  if (OutputPrivateInterface = '') or
     (OutputPublicInterface = '') or
     (OutputImplementation = '') then
  begin
    // too verbose, and this is normal -- not an alarming situation
    // WritelnVerbose('Node does not have any helpers (for now), generating empty include file: ' + Node.X3DType);
  end;

  if OutputPrivateInterface <> '' then
    OutputPrivateInterface := '  strict private' + NL + OutputPrivateInterface;
  if OutputPublicInterface <> '' then
    OutputPublicInterface := '  public' + NL + OutputPublicInterface;

  // no helpers for functionalities, for now
  if not Node.IsFunctionality then
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
  OutputCreateImplementation := '';
end;

end.
