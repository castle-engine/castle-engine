{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  TX3DImport = class(TX3DFileItem)
  public
    InlineNodeName, ImportedNodeName, ImportedNodeAlias: string;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the IMPORT declaration (XML encoding).
      Given Element here must have TagName = 'IMPORT'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DImport;
  end;

  TX3DExport = class(TX3DFileItem)
  strict private
    FExportedNode: TX3DNode;
    procedure DestructionNotification(const Node: TX3DNode);
    procedure SetExportedNode(const Value: TX3DNode);
  public
    { Name under which the EXPORTed node is available in outer files,
      i.e. IMPORT statements should refer to this alias.
      If empty, just use ExportedNode.X3DName. }
    ExportedAlias: String;

    destructor Destroy; override;

    { Exported node.
      Note that we store node reference, not just ExportedNodeName,
      this way renaming the ExportedNode (e.g. by RenameUniquely during saving)
      is properly reflected in wrtitten EXPORT statement. }
    property ExportedNode: TX3DNode read FExportedNode write SetExportedNode;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the EXPORT declaration (XML encoding).
      Given Element here must have TagName = 'EXPORT'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DExport;
  end;

  TX3DExportList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DExport>)
  public
    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DExportList;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DImport ---------------------------------------------------------------- }

procedure TX3DImport.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'Inline node name');
  InlineNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtPeriod);

  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'imported node name');
  ImportedNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  if Lexer.TokenIsKeyword(vkAS) then
  begin
    Lexer.NextToken;
    Lexer.CheckTokenIs(vtName, 'alias for imported node name');
    ImportedNodeAlias := Lexer.TokenX3DName;

    Lexer.NextToken;
  end else
    ImportedNodeAlias := ImportedNodeName;

  Reader.DoImport(Self);
end;

procedure TX3DImport.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
begin
  if not XMLAttributeX3DName(Element, 'inlineDEF', InlineNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing IMPORT "inlineDEF" attribute');
    Exit;
  end;

  if not XMLAttributeX3DName(Element, 'importedDEF', ImportedNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing IMPORT "importedDEF" attribute, looking for older "exportedDEF"');
    if not XMLAttributeX3DName(Element, 'exportedDEF', ImportedNodeName) then
    begin
      WritelnWarning('VRML/X3D', 'Missing IMPORT attribute: neighter "importedDEF" nor older "exportedDEF" found');
      Exit;
    end;
  end;

  if not XMLAttributeX3DName(Element, 'AS', ImportedNodeAlias) then
    ImportedNodeAlias := ImportedNodeName;

  Reader.DoImport(Self);
end;

procedure TX3DImport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('IMPORT ' + EncodeX3DName(InlineNodeName) + '.' + EncodeX3DName(ImportedNodeName));
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS ' + EncodeX3DName(ImportedNodeAlias));
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent(Format('<IMPORT inlineDEF=%s importedDEF=%s',
          [ StringToX3DXml(EncodeX3DName(InlineNodeName)),
            StringToX3DXml(EncodeX3DName(ImportedNodeName)) ]));
        if ImportedNodeName <> ImportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(EncodeX3DName(ImportedNodeAlias)));
        Writer.Writeln(' />');
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DImport.SaveToStream Encoding?');
    {$endif}
  end;
end;

function TX3DImport.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DImport;
begin
  Result := TX3DImport.Create;
  Result.InlineNodeName := InlineNodeName;
  Result.ImportedNodeName := ImportedNodeName;
  Result.ImportedNodeAlias := ImportedNodeAlias;
end;

{ TX3DExport ---------------------------------------------------------------- }

procedure TX3DExport.SetExportedNode(const Value: TX3DNode);
begin
  if FExportedNode <> Value then
  begin
    if FExportedNode <> nil then
      FExportedNode.RemoveDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification);
    FExportedNode := Value;
    if FExportedNode <> nil then
      FExportedNode.AddDestructionNotification({$ifdef FPC}@{$endif} DestructionNotification);
  end;
end;

procedure TX3DExport.DestructionNotification(const Node: TX3DNode);
begin
  { If FExportedNode is freed, make sure to set our FExportedNode to @nil.
    Testcase:
    - open some glTF model in view3dscene
    - select shape in view3dscene
    - remove a shape using view3dscene "Edit -> Remove Shape..."
    - save result to X3D using view3dscene "File -> Save..."
    Without TX3DExport.DestructionNotification, this would crash, as TX3DExport
    would contain invalid FExportedNode pointer at saving time. }

  if FExportedNode = Node then
    FExportedNode := nil;
end;

destructor TX3DExport.Destroy;
begin
  ExportedNode := nil; // make sure to unregister DestructionNotification
  inherited;
end;

procedure TX3DExport.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
var
  ExportedNodeName: String;
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'exported node name');
  ExportedNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  if Lexer.TokenIsKeyword(vkAS) then
  begin
    Lexer.NextToken;
    Lexer.CheckTokenIs(vtName, 'alias for exported node name');
    ExportedAlias := Lexer.TokenX3DName;

    Lexer.NextToken;
  end else
    ExportedAlias := '';

  Reader.DoExport(Self, ExportedNodeName);
end;

procedure TX3DExport.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
var
  ExportedNodeName: String;
begin
  if not XMLAttributeX3DName(Element, 'localDEF', ExportedNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing EXPORT "localDEF" attribute');
    Exit;
  end;

  if not XMLAttributeX3DName(Element, 'AS', ExportedAlias) then
    ExportedAlias := '';

  Reader.DoExport(Self, ExportedNodeName);
end;

procedure TX3DExport.SaveToStream(Writer: TX3DWriter);
begin
  if ExportedNode = nil then
  begin
    WritelnWarning('Cannot save EXPORT statement to X3D: Exported node not set');
    Exit;
  end;

  if ExportedNode.X3DName = '' then
  begin
    WritelnWarning('Cannot save EXPORT statement to X3D: Exported node %s has empty name', [ExportedNode.NiceName]);
    Exit;
  end;

  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXPORT ' + EncodeX3DName(ExportedNode.X3DName));
        if (ExportedAlias <> '') and
           (ExportedNode.X3DName <> ExportedAlias) then
          Writer.Write(' AS ' + EncodeX3DName(ExportedAlias));
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent('<EXPORT localDEF=' + StringToX3DXml(EncodeX3DName(ExportedNode.X3DName)));
        if (ExportedAlias <> '') and
           (ExportedNode.X3DName <> ExportedAlias) then
          Writer.Write(' AS=' + StringToX3DXml(EncodeX3DName(ExportedAlias)));
        Writer.Writeln(' />');
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DExport.SaveToStream Encoding?');
    {$endif}
  end;
end;

function TX3DExport.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DExport;
begin
  { Use CopyState to reuse existing TX3DExport reference.

    Useful, because:

    TX3DRootNode.ExportedNodes, which is TX3DExportList,
    carries references to the same TX3DExport instances that are also
    listed in TX3DNode.Exports.
    So TX3DExportList is created with OwnsObjects=false,
    because it should not own TX3DExport instances.
    All TX3DExport instances should be on TX3DNode.Exports list, which already
    owns them.
    And TX3DExportList should only have additional references to them.

    This is what happens when reading the file in normal case, without DeepCopy.

    In TX3DExportList.DeepCopy, for consistency,
    also TX3DExportList is created with OwnsObjects=false.
    So it should also carry the same references (not have new TX3DExport instances).
  }

  if not CopyState.ExportOriginalToCopy.TryGetValue(Self, Result) then
  begin
    Result := TX3DExport.Create;
    Result.ExportedNode := CopyState.DeepCopy(ExportedNode);
    Result.ExportedAlias := ExportedAlias;

    CopyState.ExportOriginalToCopy.Add(Self, Result);
  end;
end;

{ TX3DExportList ------------------------------------------------------------- }

function TX3DExportList.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DExportList;
var
  I: Integer;
begin
  Result := TX3DExportList.Create(false); // do not own TX3DExport instances, they are owned by TX3DNode.Exports
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result[I] := Items[I].DeepCopy(CopyState);
end;

{$endif read_implementation}
