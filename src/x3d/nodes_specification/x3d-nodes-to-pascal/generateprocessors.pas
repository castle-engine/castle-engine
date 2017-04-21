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

uses SysUtils, CastleClassUtils, CastleStringUtils,
  CastleTimeUtils, CastleLog, CastleColors, CastleUtils,
  CastleApplicationProperties;

type
  TProcessor = class abstract
  strict private
    class function FieldTypeX3DToPascal(const X3DName: string): string;
    { Field is SFString with a strictly limited set of values. }
    class function FieldIsEnumString(const Line: string; const Tokens: TCastleStringList): boolean;
    class function NodeTypeX3DToPascal(const X3DName: string): string;
  public
    procedure ProcessFile(const InputFileName: string);
    procedure NodeField(
      const X3DNodeType, PascalNodeType: string;
      const X3DFieldType, PascalFieldType: string;
      const X3DFieldName, PascalFieldName, PascalFieldNamePrefixed: string;
      const X3DFieldAccessType: string;
      const IsEnumString: boolean); virtual; abstract;
    procedure NodeEnd(const X3DNodeType, PascalNodeType: string); virtual; abstract;
  end;

  THelperProcessor = class(TProcessor)
  strict private
    NodePrivateInterface, NodePublicInterface, NodeImplementation: string;
  public
    procedure NodeField(
      const X3DNodeType, PascalNodeType: string;
      const X3DFieldType, PascalFieldType: string;
      const X3DFieldName, PascalFieldName, PascalFieldNamePrefixed: string;
      const X3DFieldAccessType: string;
      const IsEnumString: boolean); override;
    procedure NodeEnd(const X3DNodeType, PascalNodeType: string); override;
  end;

  TTemplateProcessor = class(TProcessor)
  public
    // procedure NodeField(
    //   const X3DNodeType, PascalNodeType: string;
    //   const X3DFieldType, PascalFieldType: string;
    //   const X3DFieldName, PascalFieldName, PascalFieldNamePrefixed: string;
    //   const X3DFieldAccessType: string;
    //   const IsEnumString: boolean); override;
    // procedure NodeEnd(const X3DNodeType, PascalNodeType: string); override;
  end;

var
  Verbose: boolean;

implementation

procedure WritelnVerbose(const S: string);
begin
  if Verbose then
    Writeln(ErrOutput, 'VERBOSE NOTE: ' + S);
end;

{ TProcessor ----------------------------------------------------------------- }

class function TProcessor.FieldTypeX3DToPascal(const X3DName: string): string;
begin
  if X3DName = 'SFFloat' then
    Result := 'Single' else
  if X3DName = 'SFDouble' then
    Result := 'Double' else
  if X3DName = 'SFTime' then
    Result := 'TFloatTime' else
  if X3DName = 'SFVec2f' then
    Result := 'TVector2Single' else
  if X3DName = 'SFVec3f' then
    Result := 'TVector3Single' else
  if X3DName = 'SFVec4f' then
    Result := 'TVector4Single' else
  if X3DName = 'SFVec2d' then
    Result := 'TVector2Double' else
  if X3DName = 'SFVec3d' then
    Result := 'TVector3Double' else
  if X3DName = 'SFVec4d' then
    Result := 'TVector4Double' else
  if X3DName = 'SFInt32' then
    Result := 'Integer' else
  if X3DName = 'SFBool' then
    Result := 'boolean' else
  if X3DName = 'SFRotation' then
    Result := 'TVector4Single' else
  if X3DName = 'SFColor' then
    Result := 'TCastleColorRGB' else
  if X3DName = 'SFColorRGBA' then
    Result := 'TCastleColor' else
  // Note that many SFString are enums, and they should be converted to enums
  // in ObjectPascal. We capture enums outside of this function.
  if X3DName = 'SFString' then
    Result := 'string' else
  if X3DName = 'SFMatrix3f' then
    Result := 'TMatrix3Single' else
  if X3DName = 'SFMatrix4f' then
    Result := 'TMatrix4Single' else
//  if X3DName = 'SFNode' then // nope, because these should be typed accordingly in ObjectPascal
//    Result := 'TXxx' else
    Result := '';
end;

{ Field is SFString with a strictly limited set of values. }
class function TProcessor.FieldIsEnumString(const Line: string; const Tokens: TCastleStringList): boolean;
var
  X3DFieldType{, X3DFieldName}: string;
begin
  X3DFieldType := Tokens[0];
  //X3DFieldName := Tokens[2];
  Result :=
    (X3DFieldType = 'SFString') and
    (Pos('["', Line) <> 0) and
   ((Pos('"]', Line) <> 0) or (Pos('...]', Line) <> 0));
end;

class function TProcessor.NodeTypeX3DToPascal(const X3DName: string): string;
begin
  Result := X3DName;
  if IsPrefix('X3D', X3DName) then
  begin
    { On X3DViewpointNode, we have both
      TAbstractX3DViewpointNode and TAbstractViewpointNode,
      to support also older VRML versions. Similar for grouping. }
    if (X3DName <> 'X3DViewpointNode') and
       (X3DName <> 'X3DGroupingNode') then
      Result := PrefixRemove('X3D', Result, true);
    Result := 'Abstract' + Result;
  end;
  Result := SuffixRemove('Node', Result, true);
  Result := 'T' + Result + 'Node';
end;

procedure TProcessor.ProcessFile(const InputFileName: string);
var
  F: TTextReader;
  PosComment: Integer;
  Tokens: TCastleStringList;
  Line, LineWithComment,
    X3DNodeType, PascalNodeType,
    X3DFieldName, PascalFieldName, PascalFieldNamePrefixed,
    X3DFieldType, PascalFieldType,
    X3DFieldAccessType: string;
begin
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
            X3DNodeType := Tokens[0];
            PascalNodeType := NodeTypeX3DToPascal(X3DNodeType);
            // from 2 to Tokens.Count - 2 are ancestor names, just strip comma
            // X3DAncestorType... := SuffixRemove(',', Tokens[...]);
          end else

          { node end }
          if (Tokens.Count = 1) and
             (Tokens[0] = '}') then
          begin
            NodeEnd(X3DNodeType, PascalNodeType);
            X3DNodeType := '';
            PascalNodeType := '';
          end else

          { field/event inside node }
          if (Tokens.Count >= 3) and
             (FieldTypeX3DToPascal(Tokens[0]) <> '') then
          begin
            X3DFieldType := Tokens[0];
            PascalFieldType := FieldTypeX3DToPascal(X3DFieldType);

            X3DFieldAccessType := Tokens[1];

            X3DFieldName := Tokens[2];

            if X3DNodeType = '' then
            begin
              WritelnWarning('Input', 'Field found, but not inside a node: ' + X3DFieldName);
              Continue;
            end;

            PascalFieldName := X3DFieldName;
            { rename some field names to avoid collisions }
            if PascalFieldName = 'on' then
              PascalFieldName := 'IsOn'
            else
            if PascalFieldName = 'name' then
              PascalFieldName := 'NameField';
            PascalFieldName[1] := UpCase(PascalFieldName[1]);

            PascalFieldNamePrefixed := X3DFieldName;
            PascalFieldNamePrefixed[1] := UpCase(PascalFieldNamePrefixed[1]);
            PascalFieldNamePrefixed := 'Fd' + PascalFieldNamePrefixed;

            NodeField(
              X3DNodeType, PascalNodeType,
              X3DFieldType, PascalFieldType,
              X3DFieldName, PascalFieldName, PascalFieldNamePrefixed,
              X3DFieldAccessType,
              FieldIsEnumString(LineWithComment, Tokens));
          end else
          begin
            WritelnWarning('Input', 'Line not understood, possibly field type not handled: ' + Line);
            Continue;
          end;
        finally FreeAndNil(Tokens) end;
      end;
    end;
  finally FreeAndNil(F) end;
end;

{ THelperProcessor ----------------------------------------------------------- }

procedure THelperProcessor.NodeField(
  const X3DNodeType, PascalNodeType: string;
  const X3DFieldType, PascalFieldType: string;
  const X3DFieldName, PascalFieldName, PascalFieldNamePrefixed: string;
  const X3DFieldAccessType: string;
  const IsEnumString: boolean);
begin
  if IsEnumString then
    Exit;
  if (X3DFieldName = 'solid') or
     (X3DFieldName = 'repeatS') or
     (X3DFieldName = 'repeatT') or
     (X3DFieldName = 'cycleInterval') or
     ((X3DFieldName = 'position') and (X3DNodeType = 'Viewpoint')) or
     ((X3DFieldName = 'position') and (X3DNodeType = 'OrthoViewpoint')) or
     ((X3DFieldName = 'position') and (X3DNodeType = 'GeoViewpoint')) or
     ((X3DFieldName = 'orientation') and (X3DNodeType = 'X3DViewpointNode')) or
     ((X3DFieldName = 'magnificationFilter') and (X3DNodeType = 'TextureProperties')) or
     ((X3DFieldName = 'minificationFilter') and (X3DNodeType = 'TextureProperties')) or
     (X3DFieldName = 'linetype')
     // TODO: bboxCenter and bboxSize should also be removed from here someday,
     // we should convert them manually to BBox: TBox3D to support our TBox3D type.
     then
  begin
    WritelnVerbose('Not processing, this field has special implementation: ' + X3DFieldName);
    Exit;
  end;
  if (X3DNodeType = 'X3DMetadataObject') or
     (X3DNodeType = 'X3DFogObject') or
     (X3DNodeType = 'X3DPickableObject') or
     (X3DNodeType = 'LOD') then
  begin
    WritelnVerbose('Not processing, this node has special implementation: ' + X3DNodeType);
    Exit;
  end;
  if (X3DFieldAccessType <> '[in,out]') and
     (X3DFieldAccessType <> '[]') then
  begin
    WritelnVerbose('Only fields (inputOutput or initializeOnly) are supported now: ' + X3DFieldName);
    Exit;
  end;

  NodePrivateInterface +=
    '    function Get' + PascalFieldName + ': ' + PascalFieldType + ';' + NL +
    '    procedure Set' + PascalFieldName + '(const Value: ' + PascalFieldType + ');' + NL;
  NodePublicInterface +=
    '    property ' + PascalFieldName + ': ' + PascalFieldType + ' read Get' + PascalFieldName + ' write Set' + PascalFieldName + ';' + NL;
  NodeImplementation +=
    'function ' + PascalNodeType + '.Get' + PascalFieldName + ': ' + PascalFieldType + ';' + NL +
    'begin' + NL +
    '  Result := ' + PascalFieldNamePrefixed + '.Value;' + NL +
    'end;' + NL +
    NL +
    'procedure ' + PascalNodeType + '.Set' + PascalFieldName + '(const Value: ' + PascalFieldType + ');' + NL +
    'begin' + NL +
    '  ' + PascalFieldNamePrefixed + '.Send(Value);' + NL +
    'end;' + NL +
    NL;
end;

procedure THelperProcessor.NodeEnd(const X3DNodeType, PascalNodeType: string);

  procedure GenerateOutput(const OutputInterface, OutputImplementation: string);
  var
    OutputFileName: string;
  begin
    OutputFileName := '../../auto_generated_node_helpers/x3dnodes_' +
      LowerCase(X3DNodeType) + '.inc';

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
  if (NodePrivateInterface = '') or
     (NodePublicInterface = '') or
     (NodeImplementation = '') then
    WritelnVerbose('Node does not have any helpers (for now), generating empty include file: ' + X3DNodeType);

  if NodePrivateInterface <> '' then
    NodePrivateInterface := '  private' + NL + NodePrivateInterface;
  if NodePublicInterface <> '' then
    NodePublicInterface := '  public' + NL + NodePublicInterface;

  GenerateOutput(
    // '  ' + PascalNodeType + 'Helper = class helper for ' + PascalNodeType + NL +
    NodePrivateInterface +
    NodePublicInterface +
    // '  end;' + NL +
    NL,
    '{ ' + PascalNodeType + ' ----------------------------------------------- }' + NL +
    NL +
    NodeImplementation);

  NodePrivateInterface := '';
  NodePublicInterface := '';
  NodeImplementation := '';
end;

end.
