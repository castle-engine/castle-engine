{
    This file is part of the Free Component Library

    XML writing routines
    Copyright (c) 1999-2003 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit XMLWrite;

interface

uses Classes, DOM;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text); overload;
procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream); overload;

procedure WriteXML(Node: TDOMNode; const AFileName: String); overload;
procedure WriteXML(Node: TDOMNode; var AFile: Text); overload;
procedure WriteXML(Node: TDOMNode; AStream: TStream); overload;


// ===================================================================

implementation

uses SysUtils;

// -------------------------------------------------------------------
//   Writers for the different node types
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode); forward;
procedure WriteAttribute(node: TDOMNode); forward;
procedure WriteText(node: TDOMNode); forward;
procedure WriteCDATA(node: TDOMNode); forward;
procedure WriteEntityRef(node: TDOMNode); forward;
procedure WriteEntity(node: TDOMNode); forward;
procedure WritePI(node: TDOMNode); forward;
procedure WriteComment(node: TDOMNode); forward;
procedure WriteDocument(node: TDOMNode); forward;
procedure WriteDocumentType(node: TDOMNode); forward;
procedure WriteDocumentFragment(node: TDOMNode); forward;
procedure WriteNotation(node: TDOMNode); forward;


type
  TWriteNodeProc = procedure(node: TDOMNode);

const
  WriteProcs: array[ELEMENT_NODE..NOTATION_NODE] of TWriteNodeProc =
{$IFDEF FPC}
    (@WriteElement, @WriteAttribute, @WriteText, @WriteCDATA, @WriteEntityRef,
     @WriteEntity, @WritePI, @WriteComment, @WriteDocument, @WriteDocumentType,
     @WriteDocumentFragment, @WriteNotation);
{$ELSE}
    (WriteElement, WriteAttribute, WriteText, WriteCDATA, WriteEntityRef,
     WriteEntity, WritePI, WriteComment, WriteDocument, WriteDocumentType,
     WriteDocumentFragment, WriteNotation);
{$ENDIF}

procedure WriteNode(node: TDOMNode);
begin
  WriteProcs[node.NodeType](node);
end;


// -------------------------------------------------------------------
//   Text file and TStream support
// -------------------------------------------------------------------

type
  TOutputProc = procedure(const s: String);

var
  f: ^Text;
  stream: TStream;
  wrt, wrtln: TOutputProc;
  InsideTextNode: Boolean;


procedure Text_Write(const s: String);
begin
  Write(f^, s);
end;

procedure Text_WriteLn(const s: String);
begin
  WriteLn(f^, s);
end;

procedure Stream_Write(const s: String);
begin
  if Length(s) > 0 then
    Stream.Write(s[1], Length(s));
end;

procedure Stream_WriteLn(const s: String);
const
  LF: Char = #10;
begin
  if Length(s) > 0 then
    Stream.Write(s[1], Length(s));
  Stream.Write(LF, 1);
end;


// -------------------------------------------------------------------
//   Indent handling
// -------------------------------------------------------------------

var
  Indent: String;


procedure IncIndent;
begin
  Indent := Indent + '  ';
end;

procedure DecIndent;
begin
  if Length(Indent) >= 2 then
    SetLength(Indent, Length(Indent) - 2);
end;


// -------------------------------------------------------------------
//   String conversion
// -------------------------------------------------------------------

type
  TCharacters = set of Char;
  TSpecialCharCallback = procedure(c: Char);

const
  AttrSpecialChars = ['<', '>', '"', '&'];
  TextSpecialChars = ['<', '>', '&'];


procedure ConvWrite(const s: String; const SpecialChars: TCharacters;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if s[EndPos] in SpecialChars then
    begin
      wrt(Copy(s, StartPos, EndPos - StartPos));
      SpecialCharCallback(s[EndPos]);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if EndPos > StartPos then
    wrt(Copy(s, StartPos, EndPos - StartPos));
end;

procedure AttrSpecialCharCallback(c: Char);
begin
  if c = '<' then
    wrt('&lt;')
  else if c = '>' then
    wrt('&gt;')
  else if c = '"' then
    wrt('&quot;')
  else if c = '&' then
    wrt('&amp;')
  else
    wrt(c);
end;

procedure TextnodeSpecialCharCallback(c: Char);
begin
  if c = '<' then
    wrt('&lt;')
  else if c = '>' then
    wrt('&gt;')
  else if c = '&' then
    wrt('&amp;')
  else
    wrt(c);
end;


// -------------------------------------------------------------------
//   Node writers implementations
// -------------------------------------------------------------------

procedure WriteElement(node: TDOMNode);
var
  i: Integer;
  attr, child: TDOMNode;
  SavedInsideTextNode: Boolean;
  s: String;
begin
  if not InsideTextNode then
    wrt(Indent);
  wrt('<' + node.NodeName);
  for i := 0 to node.Attributes.Length - 1 do
  begin
    attr := node.Attributes.Item[i];
    wrt(' ' + attr.NodeName + '=');
    s := attr.NodeValue;
    wrt('"');
    ConvWrite(s, AttrSpecialChars, @AttrSpecialCharCallback);
    wrt('"');
  end;
  Child := node.FirstChild;
  if Child = nil then
    if InsideTextNode then
      wrt('/>')
    else
      wrtln('/>')
  else
  begin
    SavedInsideTextNode := InsideTextNode;
    if InsideTextNode or Child.InheritsFrom(TDOMText) then
      wrt('>')
    else
      wrtln('>');
    IncIndent;
    repeat
      if Child.InheritsFrom(TDOMText) then
        InsideTextNode := True;
      WriteNode(Child);
      Child := Child.NextSibling;
    until child = nil;
    DecIndent;
    if not InsideTextNode then
      wrt(Indent);
    InsideTextNode := SavedInsideTextNode;
    s := '</' + node.NodeName + '>';
    if InsideTextNode then
      wrt(s)
    else
      wrtln(s);
  end;
end;

procedure WriteAttribute(node: TDOMNode);
begin
  WriteLn('WriteAttribute');
end;

procedure WriteText(node: TDOMNode);
begin
  ConvWrite(node.NodeValue, TextSpecialChars, @TextnodeSpecialCharCallback);
end;

procedure WriteCDATA(node: TDOMNode);
begin
  if InsideTextNode then
    wrt('<![CDATA[' + node.NodeValue + ']]>')
  else
    wrtln(Indent + '<![CDATA[' + node.NodeValue + ']]>')
end;

procedure WriteEntityRef(node: TDOMNode);
begin
  wrt('&' + node.NodeName + ';');
end;

procedure WriteEntity(node: TDOMNode);
begin
  WriteLn('WriteEntity');
end;

procedure WritePI(node: TDOMNode);
var
  s: String;
begin
  s := '<!' + TDOMProcessingInstruction(node).Target + ' ' +
    TDOMProcessingInstruction(node).Data + '>';
  if InsideTextNode then
    wrt(s)
  else
    wrtln(Indent + s);
end;

procedure WriteComment(node: TDOMNode);
begin
  if InsideTextNode then
    wrt('<!--' + node.NodeValue + '-->')
  else
    wrtln(Indent + '<!--' + node.NodeValue + '-->')
end;

procedure WriteDocument(node: TDOMNode);
begin
  WriteLn('WriteDocument');
end;

procedure WriteDocumentType(node: TDOMNode);
begin
  WriteLn('WriteDocumentType');
end;

procedure WriteDocumentFragment(node: TDOMNode);
begin
  WriteLn('WriteDocumentFragment');
end;

procedure WriteNotation(node: TDOMNode);
begin
  WriteLn('WriteNotation');
end;


procedure InitWriter;
begin
  InsideTextNode := False;
  SetLength(Indent, 0);
end;

procedure RootWriter(doc: TXMLDocument);
var
  Child: TDOMNode;
begin
  InitWriter;
  wrt('<?xml version="');
  if Length(doc.XMLVersion) > 0 then
    ConvWrite(doc.XMLVersion, AttrSpecialChars, @AttrSpecialCharCallback)
  else
    wrt('1.0');
  wrt('"');
  if Length(doc.Encoding) > 0 then
  begin
    wrt(' encoding="');
    ConvWrite(doc.Encoding, AttrSpecialChars, @AttrSpecialCharCallback);
    wrt('"');
  end;
  wrtln('?>');

  if Length(doc.StylesheetType) > 0 then
  begin
    wrt('<?xml-stylesheet type="');
    ConvWrite(doc.StylesheetType, AttrSpecialChars, @AttrSpecialCharCallback);
    wrt('" href="');
    ConvWrite(doc.StylesheetHRef, AttrSpecialChars, @AttrSpecialCharCallback);
    wrtln('"?>');
  end;

  SetLength(Indent, 0);

  child := doc.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;


// -------------------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------------------

{$IFDEF FPC}
  {$IFNDEF VER1_0}
    {$DEFINE UsesFPCWidestrings}
  {$ENDIF}
{$ENDIF}

{$IFDEF UsesFPCWidestrings}

procedure SimpleWide2AnsiMove(source:pwidechar;dest:pchar;len:sizeint);
var
  i : sizeint;
begin
  for i:=1 to len do
   begin
     if word(source^)<256 then
      dest^:=char(word(source^))
     else
      dest^:='?';
     inc(dest);
     inc(source);
   end;
end;

procedure SimpleAnsi2WideMove(source:pchar;dest:pwidechar;len:sizeint);
var
  i : sizeint;
begin
  for i:=1 to len do
   begin
     dest^:=widechar(byte(source^));
     inc(dest);
     inc(source);
   end;
end;

const
  WideStringManager: TWideStringManager = (
    Wide2AnsiMove: @SimpleWide2AnsiMove;
    Ansi2WideMove: @SimpleAnsi2WideMove
  );

{$ENDIF}

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    Stream := TFileStream.Create(AFileName, fmCreate);
    wrt := @Stream_Write;
    wrtln := @Stream_WriteLn;
    RootWriter(doc);
    Stream.Free;
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    f := @AFile;
    wrt := @Text_Write;
    wrtln := @Text_WriteLn;
    RootWriter(doc);
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    Stream := AStream;
    wrt := @Stream_Write;
    wrtln := @Stream_WriteLn;
    RootWriter(doc);
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;


procedure WriteXML(Node: TDOMNode; const AFileName: String);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    Stream := TFileStream.Create(AFileName, fmCreate);
    wrt := @Stream_Write;
    wrtln := @Stream_WriteLn;
    InitWriter;
    WriteNode(Node);
    Stream.Free;
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;

procedure WriteXML(Node: TDOMNode; var AFile: Text);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    f := @AFile;
    wrt := @Text_Write;
    wrtln := @Text_WriteLn;
    InitWriter;
    WriteNode(Node);
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;

procedure WriteXML(Node: TDOMNode; AStream: TStream);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    stream := AStream;
    wrt := @Stream_Write;
    wrtln := @Stream_WriteLn;
    InitWriter;
    WriteNode(Node);
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;


end.


{
  Revision 1.14  2004/05/02 20:17:53  peter
    * use sizeint

  Revision 1.13  2004/01/20 12:27:19  sg
  * "<" and ">" are now written as "&lt;" and "&gt;"

  Revision 1.12  2003/12/01 23:59:12  sg
  * Added support for main branch to be able to read and write at least
    ISO8859-1 encoded files correctly. A much improved solution will be
    provided when the mainbranch RTL fully supports Unicode/WideStrings.

  Revision 1.11  2003/01/15 21:59:55  sg
  * the units DOM, XMLRead and XMLWrite now compile with Delphi without
    modifications as well

  Revision 1.10  2002/11/30 16:04:34  sg
  * Stream parameters are not "var" anymore (stupid copy&paste bug)

  Revision 1.9  2002/09/20 11:36:51  sg
  * Argument escaping improvements
  * Indent fixed for consecutive WriteXML calls

  Revision 1.8  2002/09/20 11:04:21  michael
  + Changed writexml type to TDomNode instead of TDomeElement

  Revision 1.7  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
