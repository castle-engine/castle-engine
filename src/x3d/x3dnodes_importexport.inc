{
  Copyright 2002-2018 Michalis Kamburelis.

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
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
  end;

  TX3DExport = class(TX3DFileItem)
  public
    ExportedNodeName, ExportedNodeAlias: string;

    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

    { Parse the EXPORT declaration (XML encoding).
      Given Element here must have TagName = 'EXPORT'. }
    procedure ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);

    procedure SaveToStream(Writer: TX3DWriter); override;
    function DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
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

function TX3DImport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DImport;
begin
  Result := TX3DImport.Create;
  Result.InlineNodeName := InlineNodeName;
  Result.ImportedNodeName := ImportedNodeName;
  Result.ImportedNodeAlias := ImportedNodeAlias;
end;

{ TX3DExport ---------------------------------------------------------------- }

procedure TX3DExport.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);
begin
  Lexer.NextToken;
  Lexer.CheckTokenIs(vtName, 'exported node name');
  ExportedNodeName := Lexer.TokenX3DName;

  Lexer.NextToken;
  if Lexer.TokenIsKeyword(vkAS) then
  begin
    Lexer.NextToken;
    Lexer.CheckTokenIs(vtName, 'alias for exported node name');
    ExportedNodeAlias := Lexer.TokenX3DName;

    Lexer.NextToken;
  end else
    ExportedNodeAlias := ExportedNodeName;

  Reader.DoExport(Self);
end;

procedure TX3DExport.ParseXML(Element: TDOMElement; Reader: TX3DReaderNames);
begin
  if not Element.AttributeString('localDEF', ExportedNodeName) then
  begin
    WritelnWarning('VRML/X3D', 'Missing EXPORT "localDEF" attribute');
    Exit;
  end;

  if not XMLAttributeX3DName(Element, 'AS', ExportedNodeAlias) then
    ExportedNodeAlias := ExportedNodeName;

  Reader.DoExport(Self);
end;

procedure TX3DExport.SaveToStream(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic:
      begin
        Writer.WriteIndent('EXPORT ' + EncodeX3DName(ExportedNodeName));
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS ' + EncodeX3DName(ExportedNodeAlias));
        Writer.Writeln('');
      end;
    xeXML:
      begin
        Writer.WriteIndent('<EXPORT localDEF=' + StringToX3DXml(EncodeX3DName(ExportedNodeName)));
        if ExportedNodeName <> ExportedNodeAlias then
          Writer.Write(' AS=' + StringToX3DXml(EncodeX3DName(ExportedNodeAlias)));
        Writer.Writeln(' />');
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TX3DExport.SaveToStream Encoding?');
    {$endif}
  end;
end;

function TX3DExport.DeepCopy(CopyState: TX3DNodeDeepCopyState): TX3DExport;
begin
  Result := TX3DExport.Create;
  Result.ExportedNodeName := ExportedNodeName;
  Result.ExportedNodeAlias := ExportedNodeAlias;
end;

{$endif read_implementation}
