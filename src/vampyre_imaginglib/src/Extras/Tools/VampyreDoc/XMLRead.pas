{
    This file is part of the Free Component Library

    XML reading routines.
    Copyright (c) 1999-2003 by Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit XMLRead;

interface

uses SysUtils, Classes, DOM;

type

  EXMLReadError = class(Exception);


procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
  overload;
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: File); overload;
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
  overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream;
  const AFilename: String); overload;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
  overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String); overload;


// =======================================================

implementation

const

  Letter = ['A'..'Z', 'a'..'z'];
  Digit = ['0'..'9'];
  PubidChars: set of Char = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];
  WhitespaceChars: set of Char = [#9, #10, #13, ' '];

  NmToken: set of Char = Letter + Digit + ['.', '-', '_', ':'];

type

  TXMLReaderDocument = class(TXMLDocument)
  public
    procedure SetDocType(ADocType: TDOMDocumentType);
  end;

  TXMLReaderDocumentType = class(TDOMDocumentType)
  public
    constructor Create(ADocument: TXMLReaderDocument);
    property Name: DOMString read FNodeName write FNodeName;
  end;


  TSetOfChar = set of Char;

  TXMLReader = class
  protected
    buf, BufStart: PChar;
    Filename: String;

    procedure RaiseExc(descr: String);
    function  SkipWhitespace: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(s: String);
    function  CheckFor(s: PChar): Boolean;
    function  GetString(ValidChars: TSetOfChar): String;

    function  GetName(var s: String): Boolean;
    function  ExpectName: String;                                       // [5]
    procedure ExpectAttValue(attr: TDOMAttr);                           // [10]
    function  ExpectPubidLiteral: String;                               // [12]
    function  ParseComment(AOwner: TDOMNode): Boolean;                  // [15]
    function  ParsePI: Boolean;                                         // [16]
    procedure ExpectProlog;                                             // [22]
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseMisc(AOwner: TDOMNode);                              // [27]
    function  ParseMarkupDecl: Boolean;                                 // [29]
    function  ParseCharData(AOwner: TDOMNode): Boolean;			// [14]
    function  ParseCDSect(AOwner: TDOMNode): Boolean;    		// [18]
    function  ParseElement(AOwner: TDOMNode): Boolean;                  // [39]
    procedure ExpectElement(AOwner: TDOMNode);
    function  ParseReference(AOwner: TDOMNode): Boolean;                // [67]
    procedure ExpectReference(AOwner: TDOMNode);
    function  ParsePEReference: Boolean;                                // [69]
    function  ParseExternalID: Boolean;                                 // [75]
    procedure ExpectExternalID;
    function  ParseEncodingDecl: String;                                // [80]

    procedure ResolveEntities(RootNode: TDOMNode);
  public
    doc: TDOMDocument;
    procedure ProcessXML(ABuf: PChar; AFilename: String);  // [1]
    procedure ProcessFragment(AOwner: TDOMNode; ABuf: PChar; AFilename: String);
    procedure ProcessDTD(ABuf: PChar; AFilename: String);  // ([29])
  end;



procedure TXMLReaderDocument.SetDocType(ADocType: TDOMDocumentType);
begin
  FDocType := ADocType;
end;


constructor TXMLReaderDocumentType.Create(ADocument: TXMLReaderDocument);
begin
  inherited Create(ADocument);
end;



procedure TXMLReader.RaiseExc(descr: String);
var
  apos: PChar;
  x, y: Integer;
begin
  // find out the line in which the error occured
  apos := BufStart;
  x := 1;
  y := 1;
  while apos < buf do begin
    if apos[0] = #10 then begin
      Inc(y);
      x := 1;
    end else
      Inc(x);
    Inc(apos);
  end;

  raise EXMLReadError.Create('In ' + Filename + ' (line ' + IntToStr(y) + ' pos ' +
    IntToStr(x) + '): ' + descr);
end;

function TXMLReader.SkipWhitespace: Boolean;
begin
  Result := False;
  while buf[0] in WhitespaceChars do
  begin
    Inc(buf);
    Result := True;
  end;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    RaiseExc('Expected whitespace');
end;

procedure TXMLReader.ExpectString(s: String);
var
  i: Integer;
  s2: PChar;
  s3: String;
begin
  for i := 1 to Length(s) do
    if buf[i - 1] <> s[i] then begin
      GetMem(s2, Length(s) + 1);
      StrLCopy(s2, buf, Length(s));
      s3 := StrPas(s2);
      FreeMem(s2);
      RaiseExc('Expected "' + s + '", found "' + s3 + '"');
    end;
  Inc(buf, Length(s));
end;

function TXMLReader.CheckFor(s: PChar): Boolean;
begin
  if buf[0] = #0 then begin
    Result := False;
    exit;
  end;
  if StrLComp(buf, s, StrLen(s)) = 0 then begin
    Inc(buf, StrLen(s));
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.GetString(ValidChars: TSetOfChar): String;
begin
  SetLength(Result, 0);
  while buf[0] in ValidChars do begin
    Result := Result + buf[0];
    Inc(buf);
  end;
end;

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

procedure TXMLReader.ProcessXML(ABuf: PChar; AFilename: String);    // [1]
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    doc := TXMLReaderDocument.Create;
    ExpectProlog;
    ExpectElement(doc);
    ParseMisc(doc);
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}

  if buf[0] <> #0 then
    RaiseExc('Text after end of document element found');
end;

procedure TXMLReader.ProcessFragment(AOwner: TDOMNode; ABuf: PChar;
  AFilename: String);
{$IFDEF UsesFPCWidestrings}
var
  OldWideStringManager: TWideStringManager;
{$ENDIF}
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  {$IFDEF UsesFPCWidestrings}
  SetWideStringManager(WideStringManager, OldWideStringManager);
  try
  {$ENDIF}
    SkipWhitespace;
    while ParseCharData(AOwner) or ParseCDSect(AOwner) or ParsePI or
      ParseComment(AOwner) or ParseElement(AOwner) or
      ParseReference(AOwner) do
      SkipWhitespace;
  {$IFDEF UsesFPCWidestrings}
  finally
    SetWideStringManager(OldWideStringManager);
  end;
  {$ENDIF}
end;


function TXMLReader.GetName(var s: String): Boolean;    // [5]
begin
  SetLength(s, 0);
  if not (buf[0] in (Letter + ['_', ':'])) then begin
    Result := False;
    exit;
  end;

  s := buf[0];
  Inc(buf);
  s := s + GetString(Letter + ['0'..'9', '.', '-', '_', ':']);
  Result := True;
end;

function TXMLReader.ExpectName: String;    // [5]
begin
  if not (buf[0] in (Letter + ['_', ':'])) then
    RaiseExc('Expected letter, "_" or ":" for name, found "' + buf[0] + '"');

  Result := buf[0];
  Inc(buf);
  Result := Result + GetString(Letter + ['0'..'9', '.', '-', '_', ':']);
end;

procedure TXMLReader.ExpectAttValue(attr: TDOMAttr);    // [10]
var
  s: String;

  procedure FlushStringBuffer;
  begin
    if Length(s) > 0 then
    begin
      attr.AppendChild(doc.CreateTextNode(s));
      SetLength(s, 0);
    end;
  end;

var
  StrDel: array[0..1] of Char;  // String delimiter
begin
  if (buf[0] <> '''') and (buf[0] <> '"') then
    RaiseExc('Expected quotation marks');
  StrDel[0] := buf[0];
  StrDel[1] := #0;
  Inc(buf);
  SetLength(s, 0);
  while not CheckFor(StrDel) do
    if buf[0] = '&' then
    begin
      FlushStringBuffer;
      ParseReference(attr);
    end else
    begin
      s := s + buf[0];
      Inc(buf);
    end;
  FlushStringBuffer;
  ResolveEntities(Attr);
end;

function TXMLReader.ExpectPubidLiteral: String;
begin
  SetLength(Result, 0);
  if CheckFor('''') then begin
    GetString(PubidChars - ['''']);
    ExpectString('''');
  end else if CheckFor('"') then begin
    GetString(PubidChars - ['"']);
    ExpectString('"');
  end else
    RaiseExc('Expected quotation marks');
end;

function TXMLReader.ParseComment(AOwner: TDOMNode): Boolean;    // [15]
var
  comment: String;
begin
  if CheckFor('<!--') then begin
    SetLength(comment, 0);
    while (buf[0] <> #0) and (buf[1] <> #0) and
      ((buf[0] <> '-') or (buf[1] <> '-')) do begin
      comment := comment + buf[0];
      Inc(buf);
    end;
    AOwner.AppendChild(doc.CreateComment(comment));
    ExpectString('-->');
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParsePI: Boolean;    // [16]
var
  checkbuf: array[0..3] of char;
begin
  if CheckFor('<?') then begin

  {  StrLCopy(checkbuf, buf, 4);
    if UpperCase(StrPas(checkbuf)) = 'XML ' then
      RaiseExc('"<?xml" processing instruction not allowed here');
   } ExpectName;
    if SkipWhitespace then
      while (buf[0] <> #0) and (buf[1] <> #0) and not
        ((buf[0] = '?') and (buf[1] = '>')) do Inc(buf);
    ExpectString('?>');
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectProlog;    // [22]

  procedure ParseVersionNum;
  begin
    if doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).XMLVersion :=
        GetString(['a'..'z', 'A'..'Z', '0'..'9', '_', '.', ':', '-']);
  end;

  procedure ParseDoctypeDecls;
  begin
    repeat
      SkipWhitespace;
    until not (ParseMarkupDecl or ParsePEReference);
    ExpectString(']');
  end;


var
  DocType: TXMLReaderDocumentType;

begin
  if CheckFor('<?xml') then
  begin
    // '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'

    // VersionInfo: S 'version' Eq (' VersionNum ' | " VersionNum ")
    SkipWhitespace;
    ExpectString('version');
    ParseEq;
    if buf[0] = '''' then
    begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('''');
    end else if buf[0] = '"' then
    begin
      Inc(buf);
      ParseVersionNum;
      ExpectString('"');
    end else
      RaiseExc('Expected single or double quotation mark');

    // EncodingDecl?
    ParseEncodingDecl;

    // SDDecl?
    SkipWhitespace;
    if CheckFor('standalone') then
    begin
      ExpectEq;
      if buf[0] = '''' then
      begin
        Inc(buf);
        if not (CheckFor('yes''') or CheckFor('no''')) then
          RaiseExc('Expected ''yes'' or ''no''');
      end else if buf[0] = '''' then
      begin
        Inc(buf);
        if not (CheckFor('yes"') or CheckFor('no"')) then
          RaiseExc('Expected "yes" or "no"');
      end;
      SkipWhitespace;
    end;

    ExpectString('?>');
  end;

  // Check for "Misc*"
  ParseMisc(doc);

  // Check for "(doctypedecl Misc*)?"    [28]
  if CheckFor('<!DOCTYPE') then
  begin
    DocType := TXMLReaderDocumentType.Create(doc as TXMLReaderDocument);
    if doc.InheritsFrom(TXMLReaderDocument) then
      TXMLReaderDocument(doc).SetDocType(DocType);
    SkipWhitespace;
    DocType.Name := ExpectName;
    SkipWhitespace;
    if CheckFor('[') then
    begin
      ParseDoctypeDecls;
      SkipWhitespace;
      ExpectString('>');
    end else if not CheckFor('>') then
    begin
      ParseExternalID;
      SkipWhitespace;
      if CheckFor('[') then
      begin
        ParseDoctypeDecls;
        SkipWhitespace;
      end;
      ExpectString('>');
    end;
    ParseMisc(doc);
  end;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
var
  savedbuf: PChar;
begin
  savedbuf := buf;
  SkipWhitespace;
  if buf[0] = '=' then begin
    Inc(buf);
    SkipWhitespace;
    Result := True;
  end else begin
    buf := savedbuf;
    Result := False;
  end;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    RaiseExc('Expected "="');
end;


// Parse "Misc*":
//   Misc ::= Comment | PI | S

procedure TXMLReader.ParseMisc(AOwner: TDOMNode);    // [27]
begin
  repeat
    SkipWhitespace;
  until not (ParseComment(AOwner) or ParsePI);
end;

function TXMLReader.ParseMarkupDecl: Boolean;    // [29]

  function ParseElementDecl: Boolean;    // [45]

    procedure ExpectChoiceOrSeq;    // [49], [50]

      procedure ExpectCP;    // [48]
      begin
        if CheckFor('(') then
          ExpectChoiceOrSeq
        else
          ExpectName;
        if CheckFor('?') then
        else if CheckFor('*') then
        else if CheckFor('+') then;
      end;

    var
      delimiter: Char;
    begin
      SkipWhitespace;
      ExpectCP;
      SkipWhitespace;
      delimiter := #0;
      while not CheckFor(')') do begin
        if delimiter = #0 then begin
          if (buf[0] = '|') or (buf[0] = ',') then
            delimiter := buf[0]
          else
            RaiseExc('Expected "|" or ","');
          Inc(buf);
        end else
          ExpectString(delimiter);
        SkipWhitespace;
        ExpectCP;
      end;
    end;

  begin
    if CheckFor('<!ELEMENT') then begin
      ExpectWhitespace;
      ExpectName;
      ExpectWhitespace;

      // Get contentspec [46]

      if CheckFor('EMPTY') then
      else if CheckFor('ANY') then
      else if CheckFor('(') then begin
        SkipWhitespace;
        if CheckFor('#PCDATA') then begin
          // Parse Mixed section [51]
          SkipWhitespace;
          if not CheckFor(')') then
            repeat
              ExpectString('|');
              SkipWhitespace;
              ExpectName;
            until CheckFor(')*');
        end else begin
          // Parse Children section [47]

          ExpectChoiceOrSeq;

          if CheckFor('?') then
          else if CheckFor('*') then
          else if CheckFor('+') then;
        end;
      end else
        RaiseExc('Invalid content specification');

      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

  function ParseAttlistDecl: Boolean;    // [52]
  var
    attr: TDOMAttr;
  begin
    if CheckFor('<!ATTLIST') then begin
      ExpectWhitespace;
      ExpectName;
      SkipWhitespace;
      while not CheckFor('>') do begin
        ExpectName;
        ExpectWhitespace;

        // Get AttType [54], [55], [56]
        if CheckFor('CDATA') then
        else if CheckFor('ID') then
        else if CheckFor('IDREF') then
        else if CheckFor('IDREFS') then
        else if CheckFor('ENTITTY') then
        else if CheckFor('ENTITIES') then
        else if CheckFor('NMTOKEN') then
        else if CheckFor('NMTOKENS') then
        else if CheckFor('NOTATION') then begin   // [57], [58]
          ExpectWhitespace;
          ExpectString('(');
          SkipWhitespace;
          ExpectName;
          SkipWhitespace;
          while not CheckFor(')') do begin
            ExpectString('|');
            SkipWhitespace;
            ExpectName;
            SkipWhitespace;
          end;
        end else if CheckFor('(') then begin    // [59]
          SkipWhitespace;
          GetString(Nmtoken);
          SkipWhitespace;
          while not CheckFor(')') do begin
            ExpectString('|');
            SkipWhitespace;
            GetString(Nmtoken);
            SkipWhitespace;
          end;
        end else
          RaiseExc('Invalid tokenized type');

        ExpectWhitespace;

        // Get DefaultDecl [60]
        if CheckFor('#REQUIRED') then
        else if CheckFor('#IMPLIED') then
        else begin
          if CheckFor('#FIXED') then
            SkipWhitespace;
          attr := doc.CreateAttribute('');
          ExpectAttValue(attr);
        end;

        SkipWhitespace;
      end;
      Result := True;
    end else
      Result := False;
  end;

  function ParseEntityDecl: Boolean;    // [70]
  var
    NewEntity: TDOMEntity;

    function ParseEntityValue: Boolean;    // [9]
    var
      strdel: array[0..1] of Char;
    begin
      if (buf[0] <> '''') and (buf[0] <> '"') then begin
        Result := False;
        exit;
      end;
      strdel[0] := buf[0];
      strdel[1] := #0;
      Inc(buf);
      while not CheckFor(strdel) do
        if ParsePEReference then
        else if ParseReference(NewEntity) then
        else begin
          Inc(buf);             // Normal haracter
        end;
      Result := True;
    end;

  begin
    if CheckFor('<!ENTITY') then begin
      ExpectWhitespace;
      if CheckFor('%') then begin    // [72]
        ExpectWhitespace;
        NewEntity := doc.CreateEntity(ExpectName);
        ExpectWhitespace;
        // Get PEDef [74]
        if ParseEntityValue then
        else if ParseExternalID then
        else
          RaiseExc('Expected entity value or external ID');
      end else begin    // [71]
        NewEntity := doc.CreateEntity(ExpectName);
        ExpectWhitespace;
        // Get EntityDef [73]
        if ParseEntityValue then
        else begin
          ExpectExternalID;
          // Get NDataDecl [76]
          ExpectWhitespace;
          ExpectString('NDATA');
          ExpectWhitespace;
          ExpectName;
        end;
      end;
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

  function ParseNotationDecl: Boolean;    // [82]
  begin
    if CheckFor('<!NOTATION') then begin
      ExpectWhitespace;
      ExpectName;
      ExpectWhitespace;
      if ParseExternalID then
      else if CheckFor('PUBLIC') then begin    // [83]
        ExpectWhitespace;
        ExpectPubidLiteral;
      end else
        RaiseExc('Expected external or public ID');
      SkipWhitespace;
      ExpectString('>');
      Result := True;
    end else
      Result := False;
  end;

begin
  Result := False;
  while ParseElementDecl or ParseAttlistDecl or ParseEntityDecl or
    ParseNotationDecl or ParsePI or ParseComment(doc) or SkipWhitespace do
    Result := True;
end;

procedure TXMLReader.ProcessDTD(ABuf: PChar; AFilename: String);
begin
  buf := ABuf;
  BufStart := ABuf;
  Filename := AFilename;

  doc := TXMLReaderDocument.Create;
  ParseMarkupDecl;

  {
  if buf[0] <> #0 then begin
    WriteLn('=== Unparsed: ===');
    //WriteLn(buf);
    WriteLn(StrLen(buf), ' chars');
  end;
  }
end;

function TXMLReader.ParseCharData(AOwner: TDOMNode): Boolean;    // [14]
var
  s: String;
  i: Integer;
begin
  SetLength(s, 0);
  while not (buf[0] in [#0, '<', '&']) do
  begin
    s := s + buf[0];
    Inc(buf);
  end;
  if Length(s) > 0 then
  begin
    // Check if s has non-whitespace content
    i := Length(s);
    while (i > 0) and (s[i] in WhitespaceChars) do
      Dec(i);
    if i > 0 then
      AOwner.AppendChild(doc.CreateTextNode(s));
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParseCDSect(AOwner: TDOMNode): Boolean;    // [18]
var
  cdata: String;
begin
  if CheckFor('<![CDATA[') then
  begin
    SetLength(cdata, 0);
    while not CheckFor(']]>') do
    begin
      cdata := cdata + buf[0];
      Inc(buf);
    end;
    AOwner.AppendChild(doc.CreateCDATASection(cdata));
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParseElement(AOwner: TDOMNode): Boolean;    // [39] [40] [44]
var
  NewElem: TDOMElement;
var
  IsEmpty: Boolean;
  name: String;
  oldpos: PChar;

  attr: TDOMAttr;
begin
  oldpos := buf;
  if CheckFor('<') then
  begin
    if not GetName(name) then
    begin
      buf := oldpos;
      Result := False;
      exit;
    end;

    NewElem := doc.CreateElement(name);
    AOwner.AppendChild(NewElem);

    SkipWhitespace;
    IsEmpty := False;
    while True do
    begin
      if CheckFor('/>') then
      begin
        IsEmpty := True;
        break;
      end;
      if CheckFor('>') then
        break;

      // Get Attribute [41]
      attr := doc.CreateAttribute(ExpectName);
      NewElem.Attributes.SetNamedItem(attr);
      ExpectEq;
      ExpectAttValue(attr);

      SkipWhitespace;
    end;

    if not IsEmpty then
    begin
      // Get content
      SkipWhitespace;
      while ParseCharData(NewElem) or ParseCDSect(NewElem) or ParsePI or
        ParseComment(NewElem) or ParseElement(NewElem) or
        ParseReference(NewElem) do;

      // Get ETag [42]
      ExpectString('</');
      if ExpectName <> name then
        RaiseExc('Unmatching element end tag (expected "</' + name + '>")');
      SkipWhitespace;
      ExpectString('>');
    end;

    ResolveEntities(NewElem);



    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectElement(AOwner: TDOMNode);
begin
  if not ParseElement(AOwner) then
    RaiseExc('Expected element');
end;

function TXMLReader.ParsePEReference: Boolean;    // [69]
begin
  if CheckFor('%') then begin
    ExpectName;
    ExpectString(';');
    Result := True;
  end else
    Result := False;
end;

function TXMLReader.ParseReference(AOwner: TDOMNode): Boolean;    // [67] [68]
begin
  if not CheckFor('&') then begin
    Result := False;
    exit;
  end;
  if CheckFor('#') then begin    // Test for CharRef [66]
    if CheckFor('x') then begin
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9', 'a'..'f', 'A'..'F'] do Inc(buf);
    end else
      // !!!: there must be at least one digit
      while buf[0] in ['0'..'9'] do Inc(buf);
  end else
    AOwner.AppendChild(doc.CreateEntityReference(ExpectName));
  ExpectString(';');
  Result := True;
end;

procedure TXMLReader.ExpectReference(AOwner: TDOMNode);
begin
  if not ParseReference(AOwner) then
    RaiseExc('Expected reference ("&Name;" or "%Name;")');
end;


function TXMLReader.ParseExternalID: Boolean;    // [75]

  function GetSystemLiteral: String;
  begin
    SetLength(Result, 0);
    if buf[0] = '''' then begin
      Inc(buf);
      while (buf[0] <> '''') and (buf[0] <> #0) do begin
        Result := Result + buf[0];
        Inc(buf);
      end;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      while (buf[0] <> '"') and (buf[0] <> #0) do begin
        Result := Result + buf[0];
        Inc(buf);
      end;
      ExpectString('"');
    end;
  end;

begin
  if CheckFor('SYSTEM') then begin
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else if CheckFor('PUBLIC') then begin
    ExpectWhitespace;
    ExpectPubidLiteral;
    ExpectWhitespace;
    GetSystemLiteral;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.ExpectExternalID;
begin
  if not ParseExternalID then
    RaiseExc('Expected external ID');
end;

function TXMLReader.ParseEncodingDecl: String;    // [80]

  function ParseEncName: String;
  begin
    if not (buf[0] in ['A'..'Z', 'a'..'z']) then
      RaiseExc('Expected character (A-Z, a-z)');
    Result := buf[0];
    Inc(buf);
    Result := Result + GetString(['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']);
  end;

begin
  SetLength(Result, 0);
  SkipWhitespace;
  if CheckFor('encoding') then begin
    ExpectEq;
    if buf[0] = '''' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('''');
    end else if buf[0] = '"' then begin
      Inc(buf);
      Result := ParseEncName;
      ExpectString('"');
    end;
  end;
end;


{ Currently, this method will only resolve the entities which are
  predefined in XML: }

procedure TXMLReader.ResolveEntities(RootNode: TDOMNode);

  procedure ReplaceEntityRef(EntityNode: TDOMNode; const Replacement: String);
  var
    PrevSibling, NextSibling: TDOMNode;
  begin
    PrevSibling := EntityNode.PreviousSibling;
    NextSibling := EntityNode.NextSibling;
    if Assigned(PrevSibling) and (PrevSibling.NodeType = TEXT_NODE) then
    begin
      TDOMCharacterData(PrevSibling).AppendData(Replacement);
      RootNode.RemoveChild(EntityNode);
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        TDOMCharacterData(PrevSibling).AppendData(
          TDOMCharacterData(NextSibling).Data);
        RootNode.RemoveChild(NextSibling);
      end
    end else
      if Assigned(NextSibling) and (NextSibling.NodeType = TEXT_NODE) then
      begin
        TDOMCharacterData(NextSibling).InsertData(0, Replacement);
        RootNode.RemoveChild(EntityNode);
      end else
        RootNode.ReplaceChild(Doc.CreateTextNode(Replacement), EntityNode);
  end;

var
  Node, NextSibling: TDOMNode;
begin
  Node := RootNode.FirstChild;
  while Assigned(Node) do
  begin
    NextSibling := Node.NextSibling;
    if Node.NodeType = ENTITY_REFERENCE_NODE then
      if Node.NodeName = 'amp' then
        ReplaceEntityRef(Node, '&')
      else if Node.NodeName = 'apos' then
        ReplaceEntityRef(Node, '''')
      else if Node.NodeName = 'gt' then
        ReplaceEntityRef(Node, '>')
      else if Node.NodeName = 'lt' then
        ReplaceEntityRef(Node, '<')
      else if Node.NodeName = 'quot' then
        ReplaceEntityRef(Node, '"');
    Node := NextSibling;
  end;
end;



procedure ReadXMLFile(var ADoc: TXMLDocument; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, TFileRec(f).name);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessXML(buf, AFilename);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; var f: TStream);
begin
  ReadXMLFile(ADoc, f, '<Stream>');
end;

procedure ReadXMLFile(var ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadXMLFile(ADoc, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


procedure ReadXMLFragment(AParentNode: TDOMNode; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.Doc := AParentNode.OwnerDocument;
      Reader.ProcessFragment(AParentNode, buf, TFileRec(f).name);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream;
  const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    Reader.Doc := AParentNode.OwnerDocument;
    try
      Reader.ProcessFragment(AParentNode, buf, AFilename);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  ReadXMLFragment(AParentNode, f, '<Stream>');
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadXMLFragment(AParentNode, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


procedure ReadDTDFile(var ADoc: TXMLDocument; var f: File);
var
  Reader: TXMLReader;
  buf: PChar;
  BufSize: LongInt;
begin
  ADoc := nil;
  BufSize := FileSize(f) + 1;
  if BufSize <= 1 then
    exit;

  GetMem(buf, BufSize);
  try
    BlockRead(f, buf^, BufSize - 1);
    buf[BufSize - 1] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, TFileRec(f).name);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream;
  const AFilename: String);
var
  Reader: TXMLReader;
  buf: PChar;
begin
  ADoc := nil;
  if f.Size = 0 then
    exit;

  GetMem(buf, f.Size + 1);
  try
    f.Read(buf^, f.Size);
    buf[f.Size] := #0;
    Reader := TXMLReader.Create;
    try
      Reader.ProcessDTD(buf, AFilename);
      ADoc := TXMLDocument(Reader.doc);
    finally
      Reader.Free;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, '<Stream>');
end;

procedure ReadDTDFile(var ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    ReadDTDFile(ADoc, Stream, AFilename);
  finally
    Stream.Free;
  end;
end;


end.


{
  Revision 1.11  2004/05/02 20:17:52  peter
    * use sizeint

  Revision 1.10  2003/12/01 23:59:12  sg
  * Added support for main branch to be able to read and write at least
    ISO8859-1 encoded files correctly. A much improved solution will be
    provided when the mainbranch RTL fully supports Unicode/WideStrings.

  Revision 1.9  2003/11/04 20:00:46  michael
  + Fixed processing instruction parsing. <?xml is not allowed but <?xml-XXX is

  Revision 1.8  2003/01/15 21:59:55  sg
  * the units DOM, XMLRead and XMLWrite now compile with Delphi without
    modifications as well

  Revision 1.7  2002/09/21 19:22:38  sg
  * Added procedures to process XML fragments only (e.g. for merging them
    into an existing DOM document)

  Revision 1.6  2002/09/07 15:15:29  peter
    * old logs removed and tabs fixed

}
