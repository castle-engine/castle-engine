{ This file is copied from FPC SVN on 2008-08-24 and patched to fix
  [http://bugs.freepascal.org/view.php?id=11957].
  
  It is needed when using FPC 2.2.2, under FPC <= 2.2.0 the bug is not
  present and in FPC > 2.2.2 my patch should be already applied.
  It will be eventually dropped in the far future, when FPC 2.2.2
  support will be gone.
}

{
    This file is part of the Free Component Library

    XML reading routines.
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
    Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit KambiXMLRead;

{$ifdef fpc}
{$MODE objfpc}{$H+}
{$endif}

interface

uses
  SysUtils, Classes, DOM;

type
  TErrorSeverity = (esWarning, esError, esFatal);

  EXMLReadError = class(Exception)
  private
    FSeverity: TErrorSeverity;
    FErrorMessage: string;
    FLine: Integer;
    FLinePos: Integer;
  public
    property Severity: TErrorSeverity read FSeverity;
    property ErrorMessage: string read FErrorMessage;
    property Line: Integer read FLine;
    property LinePos: Integer read FLinePos;
  end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String); overload;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: Text); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream); overload;
procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const ABaseURI: String); overload;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);  overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: Text); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream); overload;
procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String); overload;

type
  TDOMParseOptions = class(TObject)
  private
    FValidate: Boolean;
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;
    FResolveExternals: Boolean;
    FNamespaces: Boolean;
  public
    property Validate: Boolean read FValidate write FValidate;
    property PreserveWhitespace: Boolean read FPreserveWhitespace write FPreserveWhitespace;
    property ExpandEntities: Boolean read FExpandEntities write FExpandEntities;
    property IgnoreComments: Boolean read FIgnoreComments write FIgnoreComments;
    property CDSectionsAsText: Boolean read FCDSectionsAsText write FCDSectionsAsText;
    property ResolveExternals: Boolean read FResolveExternals write FResolveExternals;
    property Namespaces: Boolean read FNamespaces write FNamespaces;
  end;

  // NOTE: DOM 3 LS ACTION_TYPE enumeration starts at 1
  TXMLContextAction = (
    xaAppendAsChildren = 1,
    xaReplaceChildren,
    xaInsertBefore,
    xaInsertAfter,
    xaReplace);

  TXMLErrorEvent = procedure(Error: EXMLReadError) of object;

  TXMLInputSource = class(TObject)
  private
    FStream: TStream;
    FStringData: string;
//    FBaseURI: WideString;
    FSystemID: WideString;
    FPublicID: WideString;
//    FEncoding: string;
  public
    constructor Create(AStream: TStream); overload;
    constructor Create(const AStringData: string); overload;
    property Stream: TStream read FStream;
    property StringData: string read FStringData;
//    property BaseURI: WideString read FBaseURI write FBaseURI;
    property SystemID: WideString read FSystemID write FSystemID;
    property PublicID: WideString read FPublicID write FPublicID;
//    property Encoding: string read FEncoding write FEncoding;
  end;

  TDOMParser = class(TObject)
  private
    FOptions: TDOMParseOptions;
    FOnError: TXMLErrorEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(Src: TXMLInputSource; out ADoc: TXMLDocument);
    procedure ParseUri(const URI: WideString; out ADoc: TXMLDocument);
    function ParseWithContext(Src: TXMLInputSource; Context: TDOMNode;
      Action: TXMLContextAction): TDOMNode;
    property Options: TDOMParseOptions read FOptions;
    property OnError: TXMLErrorEvent read FOnError write FOnError;
  end;


// =======================================================

implementation

uses
  UriParser, xmlutils;

const
  PubidChars: TSetOfChar = [' ', #13, #10, 'a'..'z', 'A'..'Z', '0'..'9',
    '-', '''', '(', ')', '+', ',', '.', '/', ':', '=', '?', ';', '!', '*',
    '#', '@', '$', '_', '%'];

type
  TDOMNotationEx = class(TDOMNotation);
  TDOMDocumentTypeEx = class(TDOMDocumentType);
  TDOMElementDef = class;
  TDOMAttrDef = class;

  TDTDSubsetType = (dsNone, dsInternal, dsExternal);

  // This may be augmented with ByteOffset, UTF8Offset, etc.
  TLocation = record
    Line: Integer;
    LinePos: Integer;
  end;

  TDOMEntityEx = class(TDOMEntity)
  protected
    FExternallyDeclared: Boolean;
    FResolved: Boolean;
    FOnStack: Boolean;
    FBetweenDecls: Boolean;
    FReplacementText: DOMString;
    FStartLocation: TLocation;
  end;

  TXMLCharSource = class(TObject)
  private
    FBuf: PWideChar;
    FBufEnd: PWideChar;
    FReader: TObject;   // weak reference
    FParent: TXMLCharSource;
    FEntity: TObject;   // weak reference
    FCursor: TObject;   // weak reference
    FLocation: TLocation;
    LFPos: PWideChar;
    FXML11Rules: Boolean;
    FSystemID: WideString;
    FPublicID: WideString;
    FReloadHook: procedure of object;
    function GetSystemID: WideString;
    function GetPublicID: WideString;
  protected
    function Reload: Boolean; virtual;
  public
    DTDSubsetType: TDTDSubsetType;
    constructor Create(const AData: WideString);
    function NextChar: WideChar;
    procedure Initialize; virtual;
    function SetEncoding(const AEncoding: string): Boolean; virtual;
    property SystemID: WideString read GetSystemID write FSystemID;
    property PublicID: WideString read GetPublicID write FPublicID;
  end;

  TXMLDecodingSource = class;
  TDecoder = function(Src: TXMLDecodingSource): WideChar;
  TXMLDecodingSource = class(TXMLCharSource)
  private
    FCharBuf: PChar;
    FCharBufEnd: PChar;
    FBufStart: PWideChar;
    FDecoder: TDecoder;
    FSeenCR: Boolean;
    FFixedUCS2: string;
    FBufSize: Integer;
    FSurrogate: WideChar;
    procedure DecodingError(const Msg: string);
  protected
    function Reload: Boolean; override;
    procedure FetchData; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    function SetEncoding(const AEncoding: string): Boolean; override;
    procedure Initialize; override;
  end;

  TXMLStreamInputSource = class(TXMLDecodingSource)
  private
    FAllocated: PChar;
    FStream: TStream;
    FCapacity: Integer;
    FOwnStream: Boolean;
  public
    constructor Create(AStream: TStream; AOwnStream: Boolean);
    destructor Destroy; override;
    procedure FetchData; override;
  end;

  TXMLFileInputSource = class(TXMLDecodingSource)
  private
    FFile: ^Text;
    FString: string;
  public
    constructor Create(var AFile: Text);
    procedure FetchData; override;
  end;

  PWideCharBuf = ^TWideCharBuf;
  TWideCharBuf = record
    Buffer: PWideChar;
    Length: Integer;
    MaxLength: Integer;
  end;

  PForwardRef = ^TForwardRef;
  TForwardRef = record
    Value: WideString;
    Loc: TLocation;
  end;

  TCPType = (ctName, ctChoice, ctSeq);
  TCPQuant = (cqOnce, cqZeroOrOnce, cqZeroOrMore, cqOnceOrMore);

  TContentParticle = class(TObject)
  private
    FParent: TContentParticle;
    FChildren: TFPList;
    FIndex: Integer;
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TContentParticle;
  public
    CPType: TCPType;
    CPQuant: TCPQuant;
    Def: TDOMElementDef;
    destructor Destroy; override;
    function Add: TContentParticle;
    function IsRequired: Boolean;
    function FindFirst(aDef: TDOMElementDef): TContentParticle;
    function FindNext(aDef: TDOMElementDef; ChildIdx: Integer): TContentParticle;
    function MoreRequired(ChildIdx: Integer): Boolean;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TContentParticle read GetChild;
  end;

  TElementValidator = object
    FElementDef: TDOMElementDef;
    FCurCP: TContentParticle;
    FFailed: Boolean;
    function IsElementAllowed(Def: TDOMElementDef): Boolean;
    function Incomplete: Boolean;
  end;

  TXMLReadState = (rsProlog, rsDTD, rsRoot, rsEpilog);

  TAttrDefault = (
    adImplied,
    adDefault,
    adRequired,
    adFixed
  );

  TElementContentType = (
    ctAny,
    ctEmpty,
    ctMixed,
    ctChildren
  );

  TXMLReader = class
  private
    FSource: TXMLCharSource;
    FCtrl: TDOMParser;
    FCurChar: WideChar;
    FXML11: Boolean;
    FState: TXMLReadState;
    FRecognizePE: Boolean;
    FHavePERefs: Boolean;
    FInsideDecl: Boolean;
    FDocNotValid: Boolean;
    FValue: TWideCharBuf;
    FName: TWideCharBuf;
    FTokenStart: TLocation;
    FStandalone: Boolean;          // property of Doc ?
    FNamePages: PByteArray;
    FDocType: TDOMDocumentTypeEx;  // a shortcut
    FPEMap: TDOMNamedNodeMap;
    FIDRefs: TFPList;
    FNotationRefs: TFPList;
    FCurrContentType: TElementContentType;
    FSaViolation: Boolean;
    FDTDStartPos: PWideChar;
    FIntSubset: TWideCharBuf;

    FValidate: Boolean;            // parsing options, copy of FCtrl.Options
    FPreserveWhitespace: Boolean;
    FExpandEntities: Boolean;
    FIgnoreComments: Boolean;
    FCDSectionsAsText: Boolean;
    FResolveExternals: Boolean;
    FNamespaces: Boolean;

    procedure RaiseExpectedQmark;
    procedure GetChar;
    procedure Initialize(ASource: TXMLCharSource);
    procedure DoParseAttValue(Delim: WideChar);
    procedure DoParseFragment;
    function ContextPush(AEntity: TDOMEntityEx): Boolean; overload;
    procedure ContextPush(ASrc: TXMLCharSource); overload;
    function ContextPop: Boolean;
    procedure XML11_BuildTables;
    function  XML11_CheckName: Boolean;
    procedure ParseQuantity(CP: TContentParticle);
    procedure StoreLocation(out Loc: TLocation);
    function ValidateAttrSyntax(AttrDef: TDOMAttrDef; const aValue: WideString): Boolean;
    procedure AddForwardRef(aList: TFPList; Buf: PWideChar; Length: Integer);
    procedure ClearRefs(aList: TFPList);
    procedure ValidateIdRefs;
    procedure StandaloneError(LineOffs: Integer = 0);
    procedure CallErrorHandler(E: EXMLReadError);
    function  FindOrCreateElDef: TDOMElementDef;
  protected
    FCursor: TDOMNode;
    FNesting: Integer;
    FValidator: array of TElementValidator;

    procedure DoError(Severity: TErrorSeverity; const descr: string; LineOffs: Integer=0);
    procedure DoErrorPos(Severity: TErrorSeverity; const descr: string;
      const ErrPos: TLocation);
    procedure FatalError(const descr: String; LineOffs: Integer=0); overload;
    procedure FatalError(const descr: string; const args: array of const; LineOffs: Integer=0); overload;
    procedure FatalError(Expected: WideChar); overload;
    function  SkipWhitespace(PercentAloneIsOk: Boolean = False): Boolean;
    function  SkipWhitespaceRaw: Boolean;
    procedure ExpectWhitespace;
    procedure ExpectString(const s: String);
    procedure ExpectChar(wc: WideChar);
    function  CheckForChar(c: WideChar): Boolean;
    procedure SkipString(const ValidChars: TSetOfChar);
    function  GetString(const ValidChars: TSetOfChar): WideString;
    function  NameIs(const Arg: WideString): Boolean;

    procedure RaiseNameNotFound;
    function  CheckName: Boolean;
    function  CheckNmToken: Boolean;
    function  ExpectName: WideString;                                   // [5]
    function SkipQuotedLiteral: Boolean;
    procedure ExpectAttValue;                                           // [10]
    procedure SkipPubidLiteral;                                         // [12]
    procedure SkipSystemLiteral(out Literal: WideString);
    procedure ParseComment;                                             // [15]
    procedure ParsePI;                                                  // [16]
    procedure ParseCDSect;                                              // [18]
    procedure ParseXmlOrTextDecl(TextDecl: Boolean);
    function  ParseEq: Boolean;                                         // [25]
    procedure ExpectEq;
    procedure ParseDoctypeDecl;                                         // [28]
    procedure ParseMarkupDecl;                                          // [29]
    procedure ParseElement;                                             // [39]
    procedure ParseContent;                                             // [43]
    function  ResolvePredefined: Boolean;
    procedure IncludeEntity(InAttr: Boolean);
    procedure StartPE;
    function  ParseCharRef: Boolean;                                    // [66]
    function  ParseExternalID(out SysID, PubID: WideString;             // [75]
      SysIdOptional: Boolean): Boolean;
    procedure ProcessTextAndRefs;

    procedure BadPENesting(S: TErrorSeverity = esError);
    procedure ParseEntityDecl;
    function  ParseEntityDeclValue(Delim: WideChar): Boolean;
    procedure ParseAttlistDecl;
    procedure ExpectChoiceOrSeq(CP: TContentParticle);
    procedure ParseElementDecl;
    procedure ParseNotationDecl;
    function ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLCharSource): Boolean;
    procedure ProcessDefaultAttributes(Element: TDOMElement; ElDef: TDOMElementDef);

    procedure PushVC(aElDef: TDOMElementDef);
    procedure PopVC;
    procedure UpdateConstraints;
    procedure ValidateDTD;
    procedure ValidateRoot;
    procedure ValidationError(const Msg: string; const args: array of const; LineOffs: Integer = -1);
    procedure DoAttrText(ch: PWideChar; Count: Integer);    
    procedure DTDReloadHook;
    procedure ConvertSource(SrcIn: TXMLInputSource; out SrcOut: TXMLCharSource);
    // Some SAX-alike stuff (at a very early stage)
    procedure DoText(ch: PWideChar; Count: Integer; Whitespace: Boolean=False);
    procedure DoComment(ch: PWideChar; Count: Integer);
    procedure DoCDSect(ch: PWideChar; Count: Integer);
    procedure DoNotationDecl(const aName, aPubID, aSysID: WideString);
  public
    doc: TDOMDocument;
    constructor Create; overload;
    constructor Create(AParser: TDOMParser); overload;
    destructor Destroy; override;
    procedure ProcessXML(ASource: TXMLCharSource);                // [1]
    procedure ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
    procedure ProcessDTD(ASource: TXMLCharSource);               // ([29])
  end;

  // Attribute/Element declarations

  TDOMAttrDef = class(TDOMAttr)
  protected
    FExternallyDeclared: Boolean;
    FDefault: TAttrDefault;
    FEnumeration: array of WideString;
    function AddEnumToken(const aValue: WideString): Boolean;
    function HasEnumToken(const aValue: WideString): Boolean;
  end;

  TDOMElementDef = class(TDOMElement)
  public
    FExternallyDeclared: Boolean;
    ContentType: TElementContentType;
    HasElementDecl: Boolean;
    IDAttr: TDOMAttrDef;
    NotationAttr: TDOMAttrDef;
    RootCP: TContentParticle;
    constructor Create(aOwner: TDOMDocument);
    destructor Destroy; override;
  end;

const
  NullLocation: TLocation = (Line: 0; LinePos: 0);

function Decode_UCS2(Src: TXMLDecodingSource): WideChar;
begin
  Result := PWideChar(Src.FCharBuf)^;
  Inc(Src.FCharBuf, sizeof(WideChar));
end;

function Decode_UCS2_Swapped(Src: TXMLDecodingSource): WideChar;
begin
  Result := WideChar((ord(Src.FCharBuf^) shl 8) or ord(Src.FCharBuf[1]));
  Inc(Src.FCharBuf, sizeof(WideChar));
end;


function Decode_UTF8_mb(Src: TXMLDecodingSource; First: WideChar): WideChar;
const
  MaxCode: array[0..3] of Cardinal = ($7F, $7FF, $FFFF, $1FFFFF);
var
  Value: Cardinal;
  I, bc: Integer;
begin
  if ord(First) and $40 = 0 then
    Src.DecodingError('Invalid UTF-8 sequence start byte');
  bc := 1;
  if ord(First) and $20 <> 0 then
  begin
    Inc(bc);
    if ord(First) and $10 <> 0 then
    begin
      Inc(bc);
      if ord(First) and $8 <> 0 then
        Src.DecodingError('UCS4 character out of supported range');
    end;
  end;
  // DONE: (?) check that bc bytes available
  if Src.FCharBufEnd-Src.FCharBuf < bc then
    Src.FetchData;

  Value := ord(First);
  I := bc;  // note: I is never zero
  while bc > 0 do
  begin
    if Src.FCharBuf^ in [#$80..#$BF] then
      Value := (Value shl 6) or (Cardinal(Src.FCharBuf^) and $3F)
    else
      Src.DecodingError('Invalid byte in UTF-8 sequence');
    Inc(Src.FCharBuf);
    Dec(bc);
  end;
  Value := Value and MaxCode[I];
  // RFC2279 check
  if Value <= MaxCode[I-1] then
    Src.DecodingError('Invalid UTF-8 sequence');
  case Value of
    0..$D7FF, $E000..$FFFF:
      begin
        Result := WideChar(Value);
        Exit;
      end;
    $10000..$10FFFF:
      begin
        Result := WideChar($D7C0 + (Value shr 10));
        Src.FSurrogate := WideChar($DC00 xor (Value and $3FF));
        Exit;
      end;
  end;
  Src.DecodingError('UCS4 character out of supported range');
  Result := #0; // supress warning
end;

function Decode_UTF8(Src: TXMLDecodingSource): WideChar;
begin
  Result := WideChar(byte(Src.FCharBuf^));
  Inc(Src.FCharBuf);
  if Result >= #$80 then
    Result := Decode_UTF8_mb(Src, Result);
end;

function Decode_8859_1(Src: TXMLDecodingSource): WideChar;
begin
  Result := WideChar(ord(Src.FCharBuf^));
  Inc(Src.FCharBuf);
end;

function Is_8859_1(const AEncoding: string): Boolean;
begin
  Result := SameText(AEncoding, 'ISO-8859-1') or
            SameText(AEncoding, 'ISO_8859-1') or
            SameText(AEncoding, 'latin1') or
            SameText(AEncoding, 'iso-ir-100') or
            SameText(AEncoding, 'l1') or
            SameText(AEncoding, 'IBM819') or
            SameText(AEncoding, 'CP819') or
            SameText(AEncoding, 'csISOLatin1') or
// This one is not in character-sets.txt, but used in most FPC documentation...
            SameText(AEncoding, 'ISO8859-1');
end;

// TODO: List of registered/supported decoders
function FindDecoder(const Encoding: string): TDecoder;
begin
  if Is_8859_1(Encoding) then
    Result := @Decode_8859_1
  else
    Result := nil;
end;


procedure BufAllocate(var ABuffer: TWideCharBuf; ALength: Integer);
begin
  ABuffer.MaxLength := ALength;
  ABuffer.Length := 0;
  ABuffer.Buffer := AllocMem(ABuffer.MaxLength*SizeOf(WideChar));
end;

procedure BufAppend(var ABuffer: TWideCharBuf; wc: WideChar);
begin
  if ABuffer.Length >= ABuffer.MaxLength then
  begin
    ReallocMem(ABuffer.Buffer, ABuffer.MaxLength * 2 * SizeOf(WideChar));
    FillChar(ABuffer.Buffer[ABuffer.MaxLength], ABuffer.MaxLength * SizeOf(WideChar),0);
    ABuffer.MaxLength := ABuffer.MaxLength * 2;
  end;
  ABuffer.Buffer[ABuffer.Length] := wc;
  Inc(ABuffer.Length);
end;

procedure BufAppendChunk(var ABuf: TWideCharBuf; p: PWideChar; Len: Integer);
begin
  if Len + ABuf.Length >= ABuf.MaxLength then
  begin
    ABuf.MaxLength := (Len + ABuf.Length)*2;
    // note: memory clean isn't necessary here.
    // To avoid garbage, control Length field.
    ReallocMem(ABuf.Buffer, ABuf.MaxLength * sizeof(WideChar));
  end;
  Move(p^, ABuf.Buffer[ABuf.Length], Len * sizeof(WideChar));
  Inc(ABuf.Length, Len);
end;

{ TXMLInputSource }

constructor TXMLInputSource.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

constructor TXMLInputSource.Create(const AStringData: string);
begin
  inherited Create;
  FStringData := AStringData;
end;

{ TDOMParser }

constructor TDOMParser.Create;
begin
  FOptions := TDOMParseOptions.Create;
end;

destructor TDOMParser.Destroy;
begin
  FOptions.Free;
  inherited Destroy;
end;

procedure TDOMParser.Parse(Src: TXMLInputSource; out ADoc: TXMLDocument);
var
  InputSrc: TXMLCharSource;
begin
  with TXMLReader.Create(Self) do
  try
    ConvertSource(Src, InputSrc);  // handles 'no-input-specified' case
    ProcessXML(InputSrc)
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

procedure TDOMParser.ParseUri(const URI: WideString; out ADoc: TXMLDocument);
var
  Src: TXMLCharSource;
begin
  ADoc := nil;
  with TXMLReader.Create(Self) do
  try
    if ResolveEntity(URI, '', Src) then
      ProcessXML(Src)
    else
      DoErrorPos(esFatal, 'The specified URI could not be resolved', NullLocation);
  finally
    ADoc := TXMLDocument(doc);
    Free;
  end;
end;

function TDOMParser.ParseWithContext(Src: TXMLInputSource;
  Context: TDOMNode; Action: TXMLContextAction): TDOMNode;
var
  InputSrc: TXMLCharSource;
  Frag: TDOMDocumentFragment;
  node: TDOMNode;
begin
  if Action in [xaInsertBefore, xaInsertAfter, xaReplace] then
    node := Context.ParentNode
  else
    node := Context;
  // TODO: replacing document isn't yet supported  
  if (Action = xaReplaceChildren) and (node.NodeType = DOCUMENT_NODE) then
    raise EDOMNotSupported.Create('DOMParser.ParseWithContext');

  if not (node.NodeType in [ELEMENT_NODE, DOCUMENT_FRAGMENT_NODE]) then
    raise EDOMHierarchyRequest.Create('DOMParser.ParseWithContext');

  with TXMLReader.Create(Self) do
  try
    ConvertSource(Src, InputSrc);    // handles 'no-input-specified' case
    Frag := Context.OwnerDocument.CreateDocumentFragment;
    try
      ProcessFragment(InputSrc, Frag);
      Result := Frag.FirstChild;
      case Action of
        xaAppendAsChildren: Context.AppendChild(Frag);

        xaReplaceChildren: begin
          Context.TextContent := '';     // removes children
          Context.ReplaceChild(Frag, Context.FirstChild);
        end;
        xaInsertBefore: node.InsertBefore(Frag, Context);
        xaInsertAfter:  node.InsertBefore(Frag, Context.NextSibling);
        xaReplace:      node.ReplaceChild(Frag, Context);
      end;
    finally
      Frag.Free;
    end;
  finally
    Free;
  end;
end;

// TODO: These classes still cannot be considered as the final solution...

{ TXMLInputSource }

constructor TXMLCharSource.Create(const AData: WideString);
begin
  inherited Create;
  FLocation.Line := 1;
  FBuf := PWideChar(AData);
  FBufEnd := FBuf + Length(AData);
  LFPos := FBuf-1;
end;

procedure TXMLCharSource.Initialize;
begin
end;

function TXMLCharSource.NextChar: WideChar;
begin
  Inc(FBuf);
  Result := FBuf^;
  if Result = #0 then
  begin
    if FBuf < FBufEnd then
      Exit;
    if Reload then
      Result := FBuf^;
  end;
end;

function TXMLCharSource.SetEncoding(const AEncoding: string): Boolean;
begin
  Result := True; // always succeed
end;

function TXMLCharSource.GetPublicID: WideString;
begin
  if FPublicID <> '' then
    Result := FPublicID
  else if Assigned(FParent) then
    Result := FParent.PublicID
  else
    Result := '';
end;

function TXMLCharSource.GetSystemID: WideString;
begin
  if FSystemID <> '' then
    Result := FSystemID
  else if Assigned(FParent) then
    Result := FParent.SystemID
  else
    Result := '';
end;

function TXMLCharSource.Reload: Boolean;
begin
  Result := False;
end;

{ TXMLDecodingSource }

procedure TXMLDecodingSource.AfterConstruction;
begin
  inherited AfterConstruction;
  FBufStart := AllocMem(4096);
  FBuf := FBufStart;
  FBufEnd := FBuf;
  LFPos := FBuf-1;
end;

destructor TXMLDecodingSource.Destroy;
begin
  FreeMem(FBufStart);
  inherited Destroy;
end;

procedure TXMLDecodingSource.FetchData;
begin
end;

procedure TXMLDecodingSource.DecodingError(const Msg: string);
var
  p: PWideChar;
begin
  p := FBuf;
  while p < FBufEnd do
  begin
    if p^ = #10 then
    begin
      LFPos := p;
      Inc(FLocation.Line);
    end;
    Inc(p);
  end;
  FBuf := FBufEnd;
  TXMLReader(FReader).FatalError(Msg);
end;

function TXMLDecodingSource.Reload: Boolean;
var
  c: WideChar;
  r: Integer;
begin
  if Assigned(FReloadHook) then
    FReloadHook;
  r := FBufEnd - FBuf;
  if r > 0 then
    Move(FBuf^, FBufStart^, r * sizeof(WideChar));
  Dec(LFPos, FBuf-FBufStart);
  FBuf := FBufStart;
  FBufEnd := FBufStart + r;

  while FBufEnd < FBufStart + FBufSize do
  begin
    if FCharBufEnd <= FCharBuf then
    begin
      FetchData;
      if FCharBufEnd <= FCharBuf then
        Break;
    end;
    if FSurrogate <> #0 then
    begin
      c := FSurrogate;
      FSurrogate := #0;
    end
    else
    begin
      c := FDecoder(Self);
      case c of
      #9: ;
      #10: if FSeenCR then
           begin
             FSeenCR := False;
             Continue;
           end;
      #13: begin
             FSeenCR := True;
             c := #10;
           end;
      #$85, #$2028: if FXML11Rules then
           begin
             if FSeenCR and (c = #$85) then
             begin
               FSeenCR := False;
               Continue;
             end;
             c := #10;
           end;
      else
        if (c < #32) or (c >= #$FFFE) or
         (FXML11Rules and (c >= #$7F) and (c <= #$9F)) then
        DecodingError('Invalid character');
      end; //case
    end;

    FBufEnd^ := c;
    Inc(FBufEnd);
  end;
  FBufEnd^ := #0;
  Result := FBuf < FBufEnd;
end;

const
  XmlSign: array [0..4] of WideChar = ('<', '?', 'x', 'm', 'l');

procedure TXMLDecodingSource.Initialize;
begin
  inherited;
  FLocation.Line := 1;
  FXml11Rules := TXMLReader(FReader).FXML11;
  FDecoder := @Decode_UTF8;
  FFixedUCS2 := '';
  if FCharBufEnd-FCharBuf > 1 then
  begin
    if (FCharBuf[0] = #$FE) and (FCharBuf[1] = #$FF) then
    begin
      FFixedUCS2 := 'UTF-16BE';
      FDecoder := {$IFNDEF ENDIAN_BIG} @Decode_UCS2_Swapped {$ELSE} @Decode_UCS2 {$ENDIF};
    end
    else if (FCharBuf[0] = #$FF) and (FCharBuf[1] = #$FE) then
    begin
      FFixedUCS2 := 'UTF-16LE';
      FDecoder := {$IFDEF ENDIAN_BIG} @Decode_UCS2_Swapped {$ELSE} @Decode_UCS2 {$ENDIF};
    end;
  end;
  FBufSize := 6;             //  possible BOM and '<?xml'
  Reload;
  if FBuf^ = #$FEFF then
    Inc(FBuf);
  LFPos := FBuf-1;
  if CompareMem(FBuf, @XmlSign[0], sizeof(XmlSign)) then
  begin
    FBufSize := 3;           // don't decode past XML declaration
    Inc(FBuf, 4);
    TXMLReader(FReader).ParseXmlOrTextDecl(FParent <> nil);
  end;
  FBufSize := 2047;
end;

function TXMLDecodingSource.SetEncoding(const AEncoding: string): Boolean;
var
  NewDecoder: TDecoder;
begin
  Result := True;
  if (FFixedUCS2 = '') and SameText(AEncoding, 'UTF-8') then
    Exit;
  if FFixedUCS2 <> '' then
  begin
    Result := SameText(AEncoding, FFixedUCS2) or
       SameText(AEncoding, 'UTF-16') or
       SameText(AEncoding, 'unicode');
    Exit;
  end;
  NewDecoder := FindDecoder(AEncoding);
  if Assigned(NewDecoder) then
    FDecoder := NewDecoder
  else
    Result := False;
end;


{ TXMLStreamInputSource }

const
  Slack = 16;

constructor TXMLStreamInputSource.Create(AStream: TStream; AOwnStream: Boolean);
begin
  FStream := AStream;
  FCapacity := 4096;
  GetMem(FAllocated, FCapacity+Slack);
  FCharBuf := FAllocated+(Slack-4);
  FCharBufEnd := FCharBuf;
  FOwnStream := AOwnStream;
  FetchData;
end;

destructor TXMLStreamInputSource.Destroy;
begin
  FreeMem(FAllocated);
  if FOwnStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TXMLStreamInputSource.FetchData;
var
  Remainder, BytesRead: Integer;
  OldBuf: PChar;
begin
  Assert(FCharBufEnd - FCharBuf < Slack-4);

  OldBuf := FCharBuf;
  Remainder := FCharBufEnd - FCharBuf;
  if Remainder < 0 then
    Remainder := 0;
  FCharBuf := FAllocated+Slack-4-Remainder;
  Move(OldBuf^, FCharBuf^, Remainder);
  BytesRead := FStream.Read(FAllocated[Slack-4], FCapacity);
  FCharBufEnd := FAllocated + (Slack-4) + BytesRead;
  PWideChar(FCharBufEnd)^ := #0;
end;

{ TXMLFileInputSource }

constructor TXMLFileInputSource.Create(var AFile: Text);
begin
  FFile := @AFile;
  FetchData;
end;

procedure TXMLFileInputSource.FetchData;
begin
  if not Eof(FFile^) then
  begin
    ReadLn(FFile^, FString);
    FString := FString + #10;    // bad solution...
    FCharBuf := PChar(FString);
    FCharBufEnd := FCharBuf + Length(FString);
  end;
end;

{ helper that closes handle upon destruction }
type
  THandleOwnerStream = class(THandleStream)
  public
    destructor Destroy; override;
  end;

destructor THandleOwnerStream.Destroy;
begin
  if Handle >= 0 then FileClose(Handle);
  inherited Destroy;
end;

{ TXMLReader }

procedure TXMLReader.ConvertSource(SrcIn: TXMLInputSource; out SrcOut: TXMLCharSource);
begin
  SrcOut := nil;
  if Assigned(SrcIn) then
  begin
    if Assigned(SrcIn.FStream) then
      SrcOut := TXMLStreamInputSource.Create(SrcIn.FStream, False)
    else if SrcIn.FStringData <> '' then
      SrcOut := TXMLStreamInputSource.Create(TStringStream.Create(SrcIn.FStringData), True)
    else if (SrcIn.SystemID <> '') then
      ResolveEntity(SrcIn.SystemID, SrcIn.PublicID, SrcOut);
  end;
  if (SrcOut = nil) and (FSource = nil) then
    DoErrorPos(esFatal, 'No input source specified', NullLocation);
end;

procedure TXMLReader.StoreLocation(out Loc: TLocation);
begin
  Loc.Line := FSource.FLocation.Line;
  Loc.LinePos := FSource.FBuf-FSource.LFPos;
end;

function TXMLReader.ResolveEntity(const SystemID, PublicID: WideString; out Source: TXMLCharSource): Boolean;
var
  AbsSysID: WideString;
  Filename: string;
  Stream: TStream;
  fd: THandle;
begin
  Source := nil;
  Result := False;
  if not Assigned(FSource) then
    AbsSysID := SystemID
  else
    if not ResolveRelativeURI(FSource.SystemID, SystemID, AbsSysID) then
      Exit;
  { TODO: alternative resolvers
    These may be 'internal' resolvers or a handler set by application.
    Internal resolvers should probably produce a TStream
    ( so that internal classes need not be exported ).
    External resolver will produce TXMLInputSource that should be converted.
    External resolver must NOT be called for root entity.
    External resolver can return nil, in which case we do the default }
  if URIToFilename(AbsSysID, Filename) then
  begin
    fd := FileOpen(Filename, fmOpenRead + fmShareDenyWrite);
    if fd <> THandle(-1) then
    begin
      Stream := THandleOwnerStream.Create(fd);
      Source := TXMLStreamInputSource.Create(Stream, True);
      Source.SystemID := AbsSysID;    // <- Revisit: Really need absolute sysID?
      Source.PublicID := PublicID;
    end;
  end;
  Result := Assigned(Source);
end;

procedure TXMLReader.Initialize(ASource: TXMLCharSource);
begin
  FSource := ASource;
  FSource.FReader := Self;
  FSource.Initialize;
  FCurChar := FSource.FBuf^;
end;

procedure TXMLReader.GetChar;
begin
  if FCurChar = #10 then
  begin
    Inc(FSource.FLocation.Line);
    FSource.LFPos := FSource.FBuf;
  end;
  FCurChar := FSource.NextChar;
end;

procedure TXMLReader.RaiseExpectedQmark;
begin
  FatalError('Expected single or double quote');
end;

procedure TXMLReader.FatalError(Expected: WideChar);
begin
// FIX: don't output what is found - anything may be found, including exploits...
  FatalError('Expected "%1s"', [string(Expected)]);
end;

procedure TXMLReader.FatalError(const descr: String; LineOffs: Integer);
begin
  DoError(esFatal, descr, LineOffs);
end;

procedure TXMLReader.FatalError(const descr: string; const args: array of const; LineOffs: Integer);
begin
  DoError(esFatal, Format(descr, args), LineOffs);
end;

procedure TXMLReader.ValidationError(const Msg: string; const Args: array of const; LineOffs: Integer);
begin
  FDocNotValid := True;
  if FValidate then
    DoError(esError, Format(Msg, Args), LineOffs);
end;

procedure TXMLReader.DoError(Severity: TErrorSeverity; const descr: string; LineOffs: Integer);
begin
  FSource.FLocation.LinePos := FSource.FBuf - FSource.LFPos;
  if LineOffs >= 0 then
  begin
    Dec(FSource.FLocation.LinePos, LineOffs);
    DoErrorPos(Severity, descr, FSource.FLocation);
  end
  else
    DoErrorPos(Severity, descr, FTokenStart);
end;

procedure TXMLReader.DoErrorPos(Severity: TErrorSeverity; const descr: string; const ErrPos: TLocation);
var
  E: EXMLReadError;
begin
  if Assigned(FSource) then
    E := EXMLReadError.CreateFmt('In ''%s'' (line %d pos %d): %s', [FSource.SystemID, ErrPos.Line, ErrPos.LinePos, descr])
  else
    E := EXMLReadError.Create(descr);
  E.FSeverity := Severity;
  E.FErrorMessage := descr;
  E.FLine := ErrPos.Line;
  E.FLinePos := ErrPos.LinePos;
  CallErrorHandler(E);
  // No 'finally'! If user handler raises exception, control should not get here
  // and the exception will be freed in CallErrorHandler (below)
  E.Free;
end;

function TXMLReader.SkipWhitespace(PercentAloneIsOk: Boolean): Boolean;
begin
  Result := False;
  repeat
    case FCurChar of
      #0: begin
        Result := True;
        if ContextPop then
          Continue;
        Exit;
      end;

      #9, #10, #13, #32:
        Result := True;

      '%': begin
        if not FRecognizePE then
          Exit;
// This is the only case where look-ahead is needed
        if FSource.FBuf > FSource.FBufEnd-2 then
          FSource.Reload;
        if (not PercentAloneIsOk) or
          (Byte(FSource.FBuf[1]) in NamingBitmap[FNamePages^[hi(Word(FSource.FBuf[1]))]]) or
          (FXML11 and (FSource.FBuf[1] >= #$D800) and (FSource.FBuf[1] <= #$DB7F)) then
        begin
          Inc(FSource.FBuf);    // skip '%'
          FCurChar := FSource.FBuf^;
          if not CheckName then
            RaiseNameNotFound;
          ExpectChar(';');
          StartPE;
          Result := True;        // report whitespace upon entering the PE
          Continue;
        end
        else Break;
      end
    else
      Exit;
    end;  
    GetChar;
  until False;
end;

function TXMLReader.SkipWhitespaceRaw: Boolean;
begin
  Result := False;
  while (FCurChar = #32) or (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) do
  begin
    GetChar;
    Result := True;
  end;
end;

procedure TXMLReader.ExpectWhitespace;
begin
  if not SkipWhitespace then
    FatalError('Expected whitespace');
end;

procedure TXMLReader.ExpectChar(wc: WideChar);
begin
  if FCurChar = wc then
    GetChar
  else
    FatalError(wc);
end;

procedure TXMLReader.ExpectString(const s: String);
var
  I: Integer;
begin
  for I := 1 to Length(s) do
  begin
    if FCurChar <> WideChar(ord(s[i])) then
      FatalError('Expected "%s"', [s], i-1);
    GetChar;
  end;
end;

function TXMLReader.CheckForChar(c: WideChar): Boolean;
begin
  Result := (FCurChar = c);
  if Result then
    GetChar;
end;

procedure TXMLReader.SkipString(const ValidChars: TSetOfChar);
begin
  FValue.Length := 0;
  while (ord(FCurChar) < 256) and (char(ord(FCurChar)) in ValidChars) do
  begin
    BufAppend(FValue, FCurChar);
    GetChar;
  end;
end;

function TXMLReader.GetString(const ValidChars: TSetOfChar): WideString;
begin
  SkipString(ValidChars);
  SetString(Result, FValue.Buffer, FValue.Length);
end;

function TXMLReader.NameIs(const Arg: WideString): Boolean;
begin
  Result := (FName.Length = Length(Arg)) and
    CompareMem(FName.Buffer, Pointer(Arg), FName.Length*2);
end;

constructor TXMLReader.Create;
begin
  inherited Create;
  BufAllocate(FName, 128);
  BufAllocate(FValue, 512);
  FIDRefs := TFPList.Create;
  FNotationRefs := TFPList.Create;

  // Set char rules to XML 1.0
  FNamePages := @NamePages;
  SetLength(FValidator, 16);
end;

constructor TXMLReader.Create(AParser: TDOMParser);
begin
  Create;
  FCtrl := AParser;
  FValidate := FCtrl.Options.Validate;
  FPreserveWhitespace := FCtrl.Options.PreserveWhitespace;
  FExpandEntities := FCtrl.Options.ExpandEntities;
  FCDSectionsAsText := FCtrl.Options.CDSectionsAsText;
  FIgnoreComments := FCtrl.Options.IgnoreComments;
  FResolveExternals := FCtrl.Options.ResolveExternals;
  FNamespaces := FCtrl.Options.Namespaces;
end;

destructor TXMLReader.Destroy;
begin
  FreeMem(FName.Buffer);
  FreeMem(FValue.Buffer);
  if Assigned(FSource) then
    while ContextPop do;     // clean input stack
  FSource.Free;
  FPEMap.Free;
  ClearRefs(FNotationRefs);
  ClearRefs(FIDRefs);
  FNotationRefs.Free;
  FIDRefs.Free;
  inherited Destroy;
end;

procedure TXMLReader.XML11_BuildTables;
begin
  FNamePages := Xml11NamePages;
  FXML11 := True;
  FSource.FXml11Rules := True;
end;

procedure TXMLReader.ProcessXML(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  FCursor := doc;
  FState := rsProlog;
  FNesting := 0;
  Initialize(ASource);
  DoParseFragment;              // case FCurChar <> #0 is handled

  if FState < rsRoot then
    FatalError('Root element is missing');

  if FValidate and Assigned(FDocType) then
    ValidateIdRefs;
end;

procedure TXMLReader.ProcessFragment(ASource: TXMLCharSource; AOwner: TDOMNode);
begin
  doc := AOwner.OwnerDocument;
  FCursor := AOwner;
  FState := rsRoot;
  Initialize(ASource);
  FXML11 := doc.InheritsFrom(TXMLDocument) and (TXMLDocument(doc).XMLVersion = '1.1');
  DoParseFragment;
end;

// XML 1.1 allowed range $10000..$EFFFF is [D800..DB7F] followed by [DC00..DFFF]
function TXMLReader.XML11_CheckName: Boolean;
begin
  if (FCurChar >= #$D800) and (FCurChar <= #$DB7F) then
  begin
    BufAppend(FName, FCurChar);
    // TODO: do I need to update Location here???
    FCurChar := FSource.NextChar;
    Result := (FCurChar >= #$DC00) and (FCurChar <= #$DFFF);
  end
  else
    Result := False;
end;

function TXMLReader.CheckName: Boolean;
begin
  FName.Length := 0;
  Result := (Byte(FCurChar) in NamingBitmap[FNamePages^[hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName);
  if Result then
  repeat
    BufAppend(FName, FCurChar);
    GetChar;
  until not ((Byte(FCurChar) in NamingBitmap[FNamePages^[$100+hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName));
end;

function TXMLReader.CheckNmToken: Boolean;
begin
  FName.Length := 0;
  Result := False;
  while (Byte(FCurChar) in NamingBitmap[FNamePages^[$100+hi(Word(FCurChar))]]) or
    (FXML11 and XML11_CheckName) do
  begin
    BufAppend(FName, FCurChar);
    GetChar;
    Result := True;
  end;
end;

procedure TXMLReader.RaiseNameNotFound;
begin
  // Coming at no cost, this allows more user-friendly error messages
  if (FCurChar = #32) or (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) then
    FatalError('Whitespace is not allowed here')
  else
    FatalError('Name starts with invalid character');
end;

function TXMLReader.ExpectName: WideString;
begin
  if not CheckName then
    RaiseNameNotFound;

  SetString(Result, FName.Buffer, FName.Length);
end;

function TXMLReader.ResolvePredefined: Boolean;
var
  wc: WideChar;
begin
  Result := False;
  if NameIs('amp') then
    wc := '&'
  else if NameIs('apos') then
    wc := ''''
  else if NameIs('gt') then
    wc := '>'
  else if NameIs('lt') then
    wc := '<'
  else if NameIs('quot') then
    wc := '"'
  else
    Exit;
  BufAppend(FValue, wc);
  Result := True;
end;

function TXMLReader.ParseCharRef: Boolean;           // [66]
var
  Value: Integer;
begin
  StoreLocation(FTokenStart);
  GetChar;   // skip '&'
  Result := FCurChar = '#';
  if Result then
  begin
    GetChar;
    Value := 0;
    if CheckForChar('x') then
    repeat
      case FCurChar of
        '0'..'9': Value := Value * 16 + Ord(FCurChar) - Ord('0');
        'a'..'f': Value := Value * 16 + Ord(FCurChar) - (Ord('a') - 10);
        'A'..'F': Value := Value * 16 + Ord(FCurChar) - (Ord('A') - 10);
      else
        Break;
      end;
      GetChar;
    until False
    else
    repeat
      case FCurChar of
        '0'..'9': Value := Value * 10 + Ord(FCurChar) - Ord('0');
      else
        Break;
      end;
      GetChar;
    until False;

    case Value of
      $01..$08, $0B..$0C, $0E..$1F:
        if FXML11 then
          BufAppend(FValue, WideChar(Value))
        else
          FatalError('Invalid character reference');
      $09, $0A, $0D, $20..$D7FF, $E000..$FFFD:
        BufAppend(FValue, WideChar(Value));
      $10000..$10FFFF:
        begin
          BufAppend(FValue, WideChar($D7C0 + (Value shr 10)));
          BufAppend(FValue, WideChar($DC00 xor (Value and $3FF)));
        end;
    else
      FatalError('Invalid character reference');
    end;
  end
  else
  begin
    if not CheckName then
      RaiseNameNotFound;
  end;
  ExpectChar(';');
end;

procedure TXMLReader.DoParseAttValue(Delim: WideChar);
begin
  FValue.Length := 0;
  while (FCurChar <> Delim) and (FCurChar <> #0) do
  begin
    if FCurChar = '<' then
      FatalError('Character ''<'' is not allowed in attribute value')
    else if FCurChar <> '&' then
    begin
      if (FCurChar = #10) or (FCurChar = #9) or (FCurChar = #13) then
        BufAppend(FValue, #32)  // don't change FCurChar, needed for correct location reporting
      else
        BufAppend(FValue, FCurChar);  
      GetChar;
    end
    else
    begin
      if ParseCharRef or ResolvePredefined then
        Continue;
      // have to insert entity or reference
      if FValue.Length > 0 then
      begin
        DoAttrText(FValue.Buffer, FValue.Length);
        FValue.Length := 0;
      end;
      IncludeEntity(True);
    end;
  end; // while
  if FValue.Length > 0 then
  begin
    DoAttrText(FValue.Buffer, FValue.Length);
    FValue.Length := 0;
  end;
end;

procedure TXMLReader.DoParseFragment;
begin
  // SAX: ContentHandler.StartDocument() - here?
  ParseContent;
  if FCurChar <> #0 then
    FatalError('End-tag is not allowed here');
  // SAX: ContentHandler.EndDocument() - here? or somewhere in destructor?  
end;

function TXMLReader.ContextPush(AEntity: TDOMEntityEx): Boolean;
var
  Src: TXMLCharSource;
begin
  if AEntity.SystemID <> '' then
  begin
    Result := ResolveEntity(AEntity.SystemID, AEntity.PublicID, Src);
    if not Result then
    begin
      // TODO: a detailed message like SysErrorMessage(GetLastError) would be great here 
      ValidationError('Unable to resolve external entity ''%s''', [AEntity.NodeName]);
      Exit;
    end;
  end
  else
  begin
    Src := TXMLCharSource.Create(AEntity.FReplacementText);
    Src.FLocation.Line := AEntity.FStartLocation.Line;
    Src.LFPos := Src.FBuf - AEntity.FStartLocation.LinePos;
  end;

  AEntity.FOnStack := True;
  Src.FEntity := AEntity;

  ContextPush(Src);
  Result := True;
end;

procedure TXMLReader.ContextPush(ASrc: TXMLCharSource);
begin
  ASrc.FParent := FSource;
  ASrc.FCursor := FCursor;
  Initialize(ASrc);
end;

function TXMLReader.ContextPop: Boolean;
var
  Src: TXMLCharSource;
  Error: Boolean;
begin
  Result := Assigned(FSource.FParent) and (FSource.DTDSubsetType = dsNone);
  if Result then
  begin
    Src := FSource.FParent;
    Error := False;
    if Assigned(FSource.FEntity) then
    begin
      TDOMEntityEx(FSource.FEntity).FOnStack := False;
// [28a] PE that was started between MarkupDecls may not end inside MarkupDecl
      Error := TDOMEntityEx(FSource.FEntity).FBetweenDecls and FInsideDecl;
    end;
    FCursor := TDOMNode(FSource.FCursor);
    FSource.Free;
    FSource := Src;
    FCurChar := FSource.FBuf^;
// correct position of this error is after PE reference      
    if Error then
      BadPENesting(esFatal);
  end;
end;

procedure TXMLReader.IncludeEntity(InAttr: Boolean);
var
  AEntity: TDOMEntityEx;
  RefName: WideString;
  Child: TDOMNode;
begin
  AEntity := nil;
  SetString(RefName, FName.Buffer, FName.Length);

  if Assigned(FDocType) then
    AEntity := FDocType.Entities.GetNamedItem(RefName) as TDOMEntityEx;

  if AEntity = nil then
  begin
    if FStandalone or (FDocType = nil) or not (FHavePERefs or (FDocType.SystemID <> '')) then
      FatalError('Reference to undefined entity ''%s''', [RefName], FName.Length+2)
    else
      ValidationError('Undefined entity ''%s'' referenced', [RefName], FName.Length+2);
    FCursor.AppendChild(doc.CreateEntityReference(RefName));
    Exit;
  end;

  if InAttr and (AEntity.SystemID <> '') then
    FatalError('External entity reference is not allowed in attribute value', FName.Length+2);
  if FStandalone and AEntity.FExternallyDeclared then
    FatalError('Standalone constraint violation', FName.Length+2);
  if AEntity.NotationName <> '' then
    FatalError('Reference to unparsed entity ''%s''', [RefName], FName.Length+2);

  if not AEntity.FResolved then
  begin
    if AEntity.FOnStack then
      FatalError('Entity ''%s'' recursively references itself', [AEntity.NodeName]);

    if ContextPush(AEntity) then
    begin
      FCursor := AEntity;         // build child node tree for the entity
      try
        if InAttr then
          DoParseAttValue(#0)
        else
          DoParseFragment;
        AEntity.FResolved := True;
      finally
        ContextPop;               // FCursor restored
        FValue.Length := 0;
      end;
    end;
  end;
  if (not FExpandEntities) or (not AEntity.FResolved) then
  begin
    // This will clone Entity children
    FCursor.AppendChild(doc.CreateEntityReference(RefName));
    Exit;
  end;

  Child := AEntity.FirstChild;  // clone the entity node tree
  while Assigned(Child) do
  begin
    FCursor.AppendChild(Child.CloneNode(True));
    Child := Child.NextSibling;
  end;
end;

procedure TXMLReader.StartPE;
var
  PEName: WideString;
  PEnt: TDOMEntityEx;
begin
  SetString(PEName, FName.Buffer, FName.Length);
  PEnt := nil;
  if Assigned(FPEMap) then
    PEnt := FPEMap.GetNamedItem(PEName) as TDOMEntityEx;
  if PEnt = nil then    // TODO -cVC: Referencing undefined PE
  begin                 // (These are classified as 'optional errors'...)
//    ValidationError('Undefined parameter entity referenced: %s', [PEName]);
    Exit;
  end;

  if PEnt.FOnStack then
    FatalError('Entity ''%%%s'' recursively references itself', [PEnt.NodeName]);

  PEnt.FBetweenDecls := not FInsideDecl;
  ContextPush(PEnt);
  FHavePERefs := True;
end;

procedure TXMLReader.ProcessTextAndRefs;
var
  nonWs: Boolean;
begin
  FValue.Length := 0;
  nonWs := False;
  StoreLocation(FTokenStart);
  while (FCurChar <> '<') and (FCurChar <> #0) do
  begin
    if FCurChar <> '&' then
    begin
      if (FCurChar <> #32) and (FCurChar <> #10) and (FCurChar <> #9) and (FCurChar <> #13) then
        nonWs := True;
      BufAppend(FValue, FCurChar);
      if FCurChar = '>' then
        with FValue do
          if (Length >= 3) and (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
            FatalError('Literal '']]>'' is not allowed in text', 2);
      GetChar;
    end
    else
    begin
      if FState <> rsRoot then
        FatalError('Illegal at document level');

      if FCurrContentType = ctEmpty then
          ValidationError('References are illegal in EMPTY elements', []);

      if ParseCharRef or ResolvePredefined then
        nonWs := True // CharRef to whitespace is not considered whitespace
      else
      begin
        if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
        begin
          // 'Reference illegal at root' is checked above, no need to check here
          DoText(FValue.Buffer, FValue.Length, not nonWs);
          FValue.Length := 0;
        end;
        IncludeEntity(False);
      end;
    end;
  end; // while
  if FState = rsRoot then
  begin
    if (nonWs or FPreserveWhitespace) and (FValue.Length > 0)  then
    begin
      DoText(FValue.Buffer, FValue.Length, not nonWs);
      FValue.Length := 0;
    end;
  end
  else if nonWs then
    FatalError('Illegal at document level', -1);
end;

procedure TXMLReader.ExpectAttValue;    // [10]
var
  Delim: WideChar;
begin
  if (FCurChar <> '''') and (FCurChar <> '"') then
    RaiseExpectedQmark;
  Delim := FCurChar;
  GetChar;  // skip quote
  DoParseAttValue(Delim);
  GetChar;
end;

function TXMLReader.SkipQuotedLiteral: Boolean;
var
  Delim: WideChar;
begin
  Result := (FCurChar = '''') or (FCurChar = '"');
  if Result then
  begin
    Delim := FCurChar;
    GetChar;  // skip quote
    StoreLocation(FTokenStart);
    FValue.Length := 0;
    while (FCurChar <> Delim) and (FCurChar <> #0) do
    begin
      BufAppend(FValue, FCurChar);
      GetChar;
    end;
    if not CheckForChar(Delim) then
      FatalError('Literal has no closing quote', -1);
  end;
end;

procedure TXMLReader.SkipPubidLiteral;                 // [12]
var
  I: Integer;
  wc: WideChar;
begin
  if SkipQuotedLiteral then
  begin
    for I := 0 to FValue.Length-1 do
    begin
      wc := FValue.Buffer[I];
      if (wc > #255) or not (Char(ord(wc)) in PubidChars) then
        FatalError('Illegal Public ID literal', -1);
      if (wc = #10) or (wc = #13) then
        FValue.Buffer[I] := #32;
    end;
  end
  else
    RaiseExpectedQMark;
end;

procedure TXMLReader.SkipSystemLiteral(out Literal: WideString);
begin
  if SkipQuotedLiteral then
    SetString(Literal, FValue.Buffer, FValue.Length)
  else
    RaiseExpectedQMark;
end;

procedure TXMLReader.ParseComment;    // [15]
begin
  ExpectString('--');
  StoreLocation(FTokenStart);
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetChar;
    with FValue do
      if (Length >= 2) and (Buffer[Length-1] = '-') and
      (Buffer[Length-2] = '-') then
      begin
        ExpectChar('>');
        Dec(Length, 2);
        DoComment(Buffer, Length);
        Exit;
      end;
  until FCurChar = #0;
  FatalError('Unterminated comment', -1);
end;

procedure TXMLReader.ParsePI;                    // [16]
var
  Name, Value: WideString;
  PINode: TDOMProcessingInstruction;
begin
  GetChar;      // skip '?'
  Name := ExpectName;

  with FName do
    if (Length = 3) and
     ((Buffer[0] = 'X') or (Buffer[0] = 'x')) and
     ((Buffer[1] = 'M') or (Buffer[1] = 'm')) and
     ((Buffer[2] = 'L') or (Buffer[2] = 'l')) then
  begin
    if Name <> 'xml' then
      FatalError('''xml'' is a reserved word; it must be lowercase', FName.Length)
    else
      FatalError('XML declaration is not allowed here', FName.Length);
  end;

  if FCurChar <> '?' then
    ExpectWhitespace;

  FValue.Length := 0;
  StoreLocation(FTokenStart);
  repeat
    BufAppend(FValue, FCurChar);
    GetChar;
    with FValue do
      if (Length >= 2) and (Buffer[Length-1] = '>') and
        (Buffer[Length-2] = '?') then
      begin
        Dec(Length, 2);
        SetString(Value, Buffer, Length);
        // SAX: ContentHandler.ProcessingInstruction(Name, Value);

        if FCurrContentType = ctEmpty then
            ValidationError('Processing instructions are not allowed within EMPTY elements', []);

        PINode := Doc.CreateProcessingInstruction(Name, Value);
        if Assigned(FCursor) then
          FCursor.AppendChild(PINode)
        else  // to comply with certain tests, insert PI from DTD before DTD
          Doc.InsertBefore(PINode, FDocType);
        Exit;
      end;
  until FCurChar = #0;
  FatalError('Unterminated processing instruction', -1);
end;

procedure TXMLReader.ParseXmlOrTextDecl(TextDecl: Boolean);
var
  TmpStr: WideString;
  IsXML11: Boolean;
begin
  FCurChar := FSource.NextChar;  // don't update location here 
  ExpectWhitespace;
  // VersionInfo: optional in TextDecl, required in XmlDecl
  if (not TextDecl) or (FCurChar = 'v') then
  begin
    ExpectString('version');                              // [24]
    ExpectEq;
    SkipSystemLiteral(TmpStr);
    IsXML11 := False;
    if TmpStr = '1.1' then     // Checking for bad chars is implied
      IsXML11 := True
    else if TmpStr <> '1.0' then
    { should be no whitespace in these literals, but that isn't checked now }
      FatalError('Illegal version number', -1);

    if not TextDecl then
    begin
      if doc.InheritsFrom(TXMLDocument) then
        TXMLDocument(doc).XMLVersion := TmpStr;
      if IsXML11 then
        XML11_BuildTables;
    end
    else   // parsing external entity
      if IsXML11 and not FXML11 then
        FatalError('XML 1.0 document cannot invoke XML 1.1 entities', -1);

    if FCurChar <> '?' then
      ExpectWhitespace;
  end;

  // EncodingDecl: required in TextDecl, optional in XmlDecl
  if TextDecl or (FCurChar = 'e') then                    // [80]
  begin
    ExpectString('encoding');
    ExpectEq;
    SkipSystemLiteral(TmpStr);

    if not IsValidXmlEncoding(TmpStr) then
      FatalError('Illegal encoding name', -1);

    if not FSource.SetEncoding(TmpStr) then  // <-- Wide2Ansi conversion here
      FatalError('Encoding ''%s'' is not supported', [TmpStr], -1);
    // getting here means that specified encoding is supported
    // TODO: maybe assign the 'preferred' encoding name?
    if not TextDecl and doc.InheritsFrom(TXMLDocument) then
      TXMLDocument(doc).Encoding := TmpStr;

    if FCurChar <> '?' then
      ExpectWhitespace;
  end;

  // SDDecl: forbidden in TextDecl, optional in XmlDecl
  if (not TextDecl) and (FCurChar = 's') then
  begin
    ExpectString('standalone');
    ExpectEq;
    SkipSystemLiteral(TmpStr);
    if TmpStr = 'yes' then
      FStandalone := True
    else if TmpStr <> 'no' then
      FatalError('Only "yes" or "no" are permitted as values of "standalone"', -1);
    SkipWhitespaceRaw;
  end;

  ExpectString('?>');
end;

procedure TXMLReader.DTDReloadHook;
begin
  BufAppendChunk(FIntSubset, FDTDStartPos, FSource.FBuf-FDTDStartPos);
  FDTDStartPos := TXMLDecodingSource(FSource).FBufStart + (FSource.FBufEnd-FSource.FBuf);
end;

procedure TXMLReader.ParseDoctypeDecl;    // [28]
var
  Src: TXMLCharSource;
begin
  if FState >= rsDTD then
    FatalError('Markup declaration is not allowed here');

  ExpectString('DOCTYPE');
  ExpectWhitespace;

  FDocType := TDOMDocumentTypeEx(TDOMDocumentType.Create(doc));
  FState := rsDTD;
  try
    FDocType.FName := ExpectName;
    ExpectWhitespace;
    ParseExternalID(FDocType.FSystemID, FDocType.FPublicID, False);
    SkipWhitespaceRaw;
  finally
    // DONE: append node after its name has been set; always append to avoid leak
    Doc.AppendChild(FDocType);
    FCursor := nil;
  end;

  if CheckForChar('[') then
  begin
    BufAllocate(FIntSubset, 256);
    FSource.DTDSubsetType := dsInternal;
    FSource.FReloadHook := {$IFDEF FPC}@{$ENDIF}DTDReloadHook;
    try
      FDTDStartPos := FSource.FBuf;
      ParseMarkupDecl;
      DTDReloadHook;     // fetch last chunk
      SetString(FDocType.FInternalSubset, FIntSubset.Buffer, FIntSubset.Length);
    finally
      FSource.FReloadHook := nil;
      FreeMem(FIntSubset.Buffer);
      FSource.DTDSubsetType := dsNone;
    end;
    ExpectChar(']');
    SkipWhitespaceRaw;
  end;
  ExpectChar('>');

  if (FDocType.SystemID <> '') then
  begin
    if ResolveEntity(FDocType.SystemID, FDocType.PublicID, Src) then
    begin
      ContextPush(Src);
      try
        Src.DTDSubsetType := dsExternal;
        ParseMarkupDecl;
      finally
        Src.DTDSubsetType := dsNone;
        ContextPop;
      end;
    end
    else
      ValidationError('Unable to resolve external DTD subset', []);
  end;
  FCursor := Doc;
  ValidateDTD;
end;

function TXMLReader.ParseEq: Boolean;    // [25]
begin
  SkipWhitespaceRaw;
  Result := FCurChar = '=';
  if Result then
  begin
    GetChar;
    SkipWhitespaceRaw;
  end;
end;

procedure TXMLReader.ExpectEq;
begin
  if not ParseEq then
    FatalError('Expected "="');
end;


{ DTD stuff }

procedure TXMLReader.BadPENesting(S: TErrorSeverity);
begin
  if (S = esFatal) or FValidate then
    DoError(S, 'Parameter entities must be properly nested');
end;

procedure TXMLReader.StandaloneError(LineOffs: Integer);
begin
  ValidationError('Standalone constriant violation', [], LineOffs);
end;

procedure TXMLReader.ParseQuantity(CP: TContentParticle);
begin
  if CheckForChar('?') then
    CP.CPQuant := cqZeroOrOnce
  else if CheckForChar('*') then
    CP.CPQuant := cqZeroOrMore
  else if CheckForChar('+') then
    CP.CPQuant := cqOnceOrMore;
end;

function TXMLReader.FindOrCreateElDef: TDOMElementDef;
var
  Token: WideString;
begin
  Token := ExpectName;
  Result := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(Token));
  if Result = nil then
  begin
    Result := TDOMElementDef.Create(doc);
    Result.FNodeName := Token;
    FDocType.ElementDefs.SetNamedItem(Result);
  end;
end;

procedure TXMLReader.ExpectChoiceOrSeq(CP: TContentParticle);                  // [49], [50]
var
  Delim: WideChar;
  CurrentEntity: TObject;
  CurrentCP: TContentParticle;
begin
  Delim := #0;
  repeat
    CurrentCP := CP.Add;
    SkipWhitespace;
    if CheckForChar('(') then
    begin
      CurrentEntity := FSource.FEntity;
      ExpectChoiceOrSeq(CurrentCP);
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
    end
    else
      CurrentCP.Def := FindOrCreateElDef;

    ParseQuantity(CurrentCP);

    SkipWhitespace;
    if FCurChar = ')' then
      Break;
    if Delim = #0 then
    begin
      if (FCurChar = '|') or (FCurChar = ',') then
        Delim := FCurChar
      else
        FatalError('Expected "|" or ","');
    end
    else
      if FCurChar <> Delim then
        FatalError(Delim);
    GetChar; // skip delimiter
  until False;
  if Delim = '|' then
    CP.CPType := ctChoice
  else
    CP.CPType := ctSeq;    // '(foo)' is a sequence!
end;

procedure TXMLReader.ParseElementDecl;            // [45]
var
  ElDef: TDOMElementDef;
  NeedAsterisk: Boolean;
  CurrentCP: TContentParticle;
  CurrentEntity: TObject;
  I: Integer;
begin
  ExpectWhitespace;
  ElDef := FindOrCreateElDef;
  if ElDef.HasElementDecl then
    ValidationError('Duplicate declaration of element ''%s''', [ElDef.TagName], FName.Length);

  ElDef.FExternallyDeclared := FSource.DTDSubsetType <> dsInternal;
  ElDef.HasElementDecl := True;

  ExpectWhitespace;
  if FCurChar = 'E' then
  begin
    ExpectString('EMPTY');
    ElDef.ContentType := ctEmpty;
  end
  else if FCurChar = 'A' then
  begin
    ExpectString('ANY');
    ElDef.ContentType := ctAny;
  end
  else if CheckForChar('(') then
  begin
    CurrentEntity := FSource.FEntity;
    SkipWhitespace;
    if FCurChar = '#' then       // Mixed section [51]
    begin
      ExpectString('#PCDATA');
      SkipWhitespace;
      ElDef.ContentType := ctMixed;
      NeedAsterisk := False;
      while FCurChar <> ')' do
      begin
        ExpectChar('|');
        NeedAsterisk := True;
        SkipWhitespace;

        CurrentCP := ElDef.RootCP.Add;
        CurrentCP.Def := FindOrCreateElDef;
        // DONE: comparing pointers instead of strings
        for I := ElDef.RootCP.ChildCount-2 downto 0 do
          if CurrentCP.Def = ElDef.RootCP.Children[I].Def then
            ValidationError('Duplicate token in mixed section', [], FName.Length);
        SkipWhitespace;
      end;
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
      // TODO: does this asterisk have any real meaning?
      if NeedAsterisk then
      begin
        ExpectChar('*');
        ElDef.RootCP.CPQuant := cqZeroOrMore;
      end
      else
        if CheckForChar('*') then
          ElDef.RootCP.CPQuant := cqZeroOrMore;
    end
    else       // Children section [47]
    begin
      ElDef.ContentType := ctChildren;
      ExpectChoiceOrSeq(ElDef.RootCP);
      if CurrentEntity <> FSource.FEntity then
        BadPENesting;
      GetChar;
      ParseQuantity(ElDef.RootCP);
    end;
  end
  else
    FatalError('Invalid content specification');
  // SAX: DeclHandler.ElementDecl(name, model);
end;


procedure TXMLReader.ParseNotationDecl;        // [82]
var
  Name, SysID, PubID: WideString;
begin
  ExpectWhitespace;
  Name := ExpectName;
  ExpectWhitespace;
  if not ParseExternalID(SysID, PubID, True) then
    FatalError('Expected external or public ID');
  DoNotationDecl(Name, PubID, SysID);
end;

procedure TXMLReader.ParseAttlistDecl;         // [52]
var
  ValueRequired: Boolean;
  Token: WideString;
  ElDef: TDOMElementDef;
  AttDef: TDOMAttrDef;
begin
  ExpectWhitespace;
  ElDef := FindOrCreateElDef;
  SkipWhitespace;
  while FCurChar <> '>' do
  begin
    AttDef := TDOMAttrDef.Create(doc);
    AttDef.FExternallyDeclared := FSource.DTDSubsetType <> dsInternal;
    try
      AttDef.FName := ExpectName;
      ExpectWhitespace;
      StoreLocation(FTokenStart);
      Token := GetString(['A'..'Z']);     // Get AttType [54], [55], [56]
      if Token = 'CDATA' then
        AttDef.FDataType := dtCdata
      else if Token = 'ID' then
        AttDef.FDataType := dtId
      else if Token = 'IDREF' then
        AttDef.FDataType := dtIdRef
      else if Token = 'IDREFS' then
        AttDef.FDataType := dtIdRefs
      else if Token = 'ENTITY' then
        AttDef.FDataType := dtEntity
      else if Token = 'ENTITIES' then
        AttDef.FDataType := dtEntities
      else if Token = 'NMTOKEN' then
        AttDef.FDataType := dtNmToken
      else if Token = 'NMTOKENS' then
        AttDef.FDataType := dtNmTokens
      else if Token = 'NOTATION' then     // [57], [58]
      begin
        AttDef.FDataType := dtNotation;
        if Assigned(ElDef.NotationAttr) then
          ValidationError('Only one attribute of type NOTATION is allowed per element',[])
        else
          ElDef.NotationAttr := AttDef;
        if ElDef.ContentType = ctEmpty then
          ValidationError('NOTATION attributes are not allowed on EMPTY elements',[]);

        ExpectWhitespace;
        ExpectChar('(');
        repeat
          SkipWhitespace;
          if not AttDef.AddEnumToken(ExpectName) then
            ValidationError('Duplicate token in NOTATION attribute declaration',[], FName.Length);
          AddForwardRef(FNotationRefs, FName.Buffer, FName.Length);
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end
      else
      if CheckForChar('(') then     // [59]
      begin
        AttDef.FDataType := dtNmToken;
        repeat
          SkipWhitespace;
          if not CheckNmToken then
            RaiseNameNotFound;      // not completely correct error message
          SetString(Token, FName.Buffer, FName.Length);
          if not AttDef.AddEnumToken(Token) then
            ValidationError('Duplicate token in enumerated attibute declaration', [], FName.Length);
          SkipWhitespace;
        until not CheckForChar('|');
        ExpectChar(')');
      end else
        FatalError('Illegal attribute type for ''%s''', [AttDef.Name], FValue.Length);

      if AttDef.DataType = dtID then
      begin
        if Assigned(ElDef.IDAttr) then
          ValidationError('Only one attribute of type ID is allowed per element',[]);
        ElDef.IDAttr := AttDef;
      end;
      ExpectWhitespace;

      ValueRequired := False;
      StoreLocation(FTokenStart);
      if CheckForChar('#') then
      begin
        Token := GetString(['A'..'Z']);
        if Token = 'REQUIRED' then
          AttDef.FDefault := adRequired
        else if Token = 'IMPLIED' then
          AttDef.FDefault := adImplied
        else if Token = 'FIXED' then
        begin
          AttDef.FDefault := adFixed;
          ExpectWhitespace;
          ValueRequired := True;
        end
        else
          FatalError('Expecting ''#REQUIRED'', ''#IMPLIED'' or ''#FIXED''', [], FValue.Length+1);
      end
      else
      begin
        AttDef.FDefault := adDefault;
        ValueRequired := True;
      end;

      if ValueRequired then
      begin
        if AttDef.FDataType = dtId then
          ValidationError('An attribute of type ID cannot have a default value',[]);

        FCursor := AttDef;
        // TODO: move this to ExpectAttValue?
        StoreLocation(FTokenStart);
        Inc(FTokenStart.LinePos);
// See comments to valid-sa-094: PE expansion should be disabled in AttDef.
// ExpectAttValue() does not recognize PEs anyway, so setting FRecognizePEs isn't needed
// Saving/restoring FCursor is also redundant because it is always nil here.
        ExpectAttValue;
        FCursor := nil;
        if not ValidateAttrSyntax(AttDef, AttDef.NodeValue) then
          ValidationError('Default value for attribute ''%s'' has wrong syntax', [AttDef.Name]);
      end;
      // SAX: DeclHandler.AttributeDecl(...)

      // First declaration is binding, subsequent should be ignored
      if Assigned(ElDef.GetAttributeNode(AttDef.Name)) then
        AttDef.Free
      else
        ElDef.SetAttributeNode(AttDef);
    except
      AttDef.Free;
      raise;
    end;
    SkipWhitespace;
  end;
end;

function TXMLReader.ParseEntityDeclValue(Delim: WideChar): Boolean;   // [9]
var
  CurrentEntity: TObject;
begin
  CurrentEntity := FSource.FEntity;
  // "Included in literal": process until delimiter hit IN SAME context
  while not ((FSource.FEntity = CurrentEntity) and CheckForChar(Delim)) do
  if CheckForChar('%') then
  begin
    if not CheckName then
      RaiseNameNotFound;
    ExpectChar(';');  
    if FSource.DTDSubsetType = dsInternal then
      FatalError('PE reference not allowed here in internal subset', FName.Length+2);
    StartPE;
  end
  else if FCurChar = '&' then  // CharRefs: include, EntityRefs: bypass
  begin
    if not ParseCharRef then
    begin
      BufAppend(FValue, '&');
      BufAppendChunk(FValue, FName.Buffer, FName.Length);
      BufAppend(FValue, ';');
    end;
  end
  else if FCurChar <> #0 then         // Regular character
  begin
    BufAppend(FValue, FCurChar);
    GetChar;
  end
  else if not ContextPop then         // #0
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure TXMLReader.ParseEntityDecl;        // [70]
var
  NDataAllowed: Boolean;
  Delim: WideChar;
  Entity: TDOMEntityEx;
  Map: TDOMNamedNodeMap;
begin
  if not SkipWhitespace(True) then
    FatalError('Expected whitespace');
  NDataAllowed := True;
  Map := FDocType.Entities;
  if CheckForChar('%') then                  // [72]
  begin
    ExpectWhitespace;
    NDataAllowed := False;
    if FPEMap = nil then
      FPEMap := TDOMNamedNodeMap.Create(FDocType, ENTITY_NODE);
    Map := FPEMap;
  end;

  Entity := TDOMEntityEx.Create(Doc);
  try
    Entity.FExternallyDeclared := FSource.DTDSubsetType <> dsInternal;
    Entity.FName := ExpectName;
    ExpectWhitespace;

    if (FCurChar = '"') or (FCurChar = '''') then
    begin
      NDataAllowed := False;
      Delim := FCurChar;
      GetChar;
      StoreLocation(Entity.FStartLocation);
      FValue.Length := 0;
      if not ParseEntityDeclValue(Delim) then
        DoErrorPos(esFatal, 'Literal has no closing quote', Entity.FStartLocation);
      SetString(Entity.FReplacementText, FValue.Buffer, FValue.Length);
    end
    else
      if not ParseExternalID(Entity.FSystemID, Entity.FPublicID, False) then
        FatalError('Expected entity value or external ID');

    if NDataAllowed then                // [76]
    begin
      if FCurChar <> '>' then
        ExpectWhitespace;
      if FCurChar = 'N' then
      begin
        ExpectString('NDATA');
        ExpectWhitespace;
        Entity.FNotationName := ExpectName;
        AddForwardRef(FNotationRefs, FName.Buffer, FName.Length);
        // SAX: DTDHandler.UnparsedEntityDecl(...);
      end;
    end;
  except
    Entity.Free;
    raise;
  end;

  // Repeated declarations of same entity are legal but must be ignored
  if Map.GetNamedItem(Entity.NodeName) = nil then
    Map.SetNamedItem(Entity)
  else
    Entity.Free;
end;


procedure TXMLReader.ParseMarkupDecl;        // [29]
var
  Token: WideString;
  IncludeLevel: Integer;
  IgnoreLevel: Integer;
  CurrentEntity: TObject;
  IncludeLoc: TLocation;
  IgnoreLoc: TLocation;
  ploc: ^TLocation;
begin
  IncludeLevel := 0;
  IgnoreLevel := 0;
  repeat
    FRecognizePE := True;      // PERef between declarations should always be recognized
    SkipWhitespace;
    FRecognizePE := False;

    if (FCurChar = ']') and (IncludeLevel > 0) then
    begin
      ExpectString(']]>');
      Dec(IncludeLevel);
      Continue;
    end;

    if not CheckForChar('<') then
      Break;

    CurrentEntity := FSource.FEntity;

    if FCurChar = '?' then
      ParsePI
    else
    begin
      ExpectChar('!');
      if FCurChar = '-' then
        ParseComment
      else if FCurChar = '[' then
      begin
        if FSource.DTDSubsetType = dsInternal then
          FatalError('Conditional sections are not allowed in internal subset');

        FRecognizePE := True;
        GetChar; // skip '['
        SkipWhitespace;
        Token := GetString(['A'..'Z']);
        SkipWhitespace;

        ploc := nil;
        if Token = 'INCLUDE' then
        begin
          if IncludeLevel = 0 then
            ploc := @IncludeLoc;
          Inc(IncludeLevel);
        end
        else if Token = 'IGNORE' then
        begin
          ploc := @IgnoreLoc;
          IgnoreLevel := 1;
        end
        else
          FatalError('Expected "INCLUDE" or "IGNORE"');
        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        ExpectChar('[');
        if Assigned(ploc) then
          StoreLocation(ploc^);
        if IgnoreLevel > 0 then
        repeat
          FRecognizePE := False;    // PEs not recognized in IGNORE section
          if CheckForChar('<') and CheckForChar('!') and CheckForChar('[') then
            Inc(IgnoreLevel)
          else if CheckForChar(']') and CheckForChar(']') and CheckForChar('>') then
            Dec(IgnoreLevel)
          else GetChar;
        until (IgnoreLevel=0) or (FCurChar = #0);
      end
      else
      begin
        FRecognizePE := FSource.DTDSubsetType <> dsInternal;
        FInsideDecl := True;
        Token := GetString(['A'..'Z']);
        if Token = 'ELEMENT' then
          ParseElementDecl
        else if Token = 'ENTITY' then
          ParseEntityDecl
        else if Token = 'ATTLIST' then
          ParseAttlistDecl
        else if Token = 'NOTATION' then
          ParseNotationDecl
        else
          FatalError('Illegal markup declaration', FValue.Length);

        SkipWhitespace;
        FRecognizePE := False;

        if CurrentEntity <> FSource.FEntity then
          BadPENesting;
        ExpectChar('>');
        FInsideDecl := False;
      end;
    end;
  until False;
  FRecognizePE := False;
  if (IncludeLevel > 0) or (IgnoreLevel > 0) then
  begin
    if IncludeLevel > 0 then
      FTokenStart := IncludeLoc
    else
      FTokenStart := IgnoreLoc;
    FatalError('Conditional section is not closed', -1);
  end;
  if (FSource.DTDSubsetType = dsInternal) and (FCurChar = ']') then
    Exit;
  if FCurChar <> #0 then
    FatalError('Illegal character in DTD');
end;

procedure TXMLReader.ProcessDTD(ASource: TXMLCharSource);
begin
  doc := TXMLDocument.Create;
  FDocType := TDOMDocumentTypeEx.Create(doc);
  // TODO: DTD labeled version 1.1 will be rejected - must set FXML11 flag
  // DONE: It's ok to have FCursor=nil now
  doc.AppendChild(FDocType);
  Initialize(ASource);
  ParseMarkupDecl;
end;

procedure TXMLReader.ParseCDSect;               // [18]
begin
  ExpectString('[CDATA[');
  StoreLocation(FTokenStart);
  if FState <> rsRoot then
    FatalError('Illegal at document level');
  FValue.Length := 0;
  repeat
    BufAppend(FValue, FCurChar);
    GetChar;
    with FValue do
      if (Length >= 3) and (Buffer[Length-1] = '>') and
      (Buffer[Length-2] = ']') and (Buffer[Length-3] = ']') then
    begin
      DoCDSect(Buffer, Length-3);
      Exit;
    end;
  until FCurChar = #0;
  FatalError('Unterminated CDATA section', -1);
end;

procedure TXMLReader.ParseContent;
begin
  repeat
    if FCurChar = '<' then
    begin
      GetChar;
      if CheckName then
        ParseElement
      else if FCurChar = '!' then
      begin
        GetChar;
        if FCurChar = '[' then
          ParseCDSect
        else if FCurChar = '-' then
          ParseComment
        else
          ParseDoctypeDecl;
      end
      else if FCurChar = '?' then
        ParsePI
      else
        Exit;
    end
    else
      ProcessTextAndRefs;
  until FCurChar = #0;
end;

// Element name already in FNameBuffer
procedure TXMLReader.ParseElement;    // [39] [40] [44]
var
  NewElem: TDOMElement;
  ElDef: TDOMElementDef;
  IsEmpty: Boolean;
  attr, OldAttr: TDOMNode;
begin
  if FState > rsRoot then
    FatalError('Only one top-level element allowed', FName.Length)
  else if FState < rsRoot then
  begin
    if FValidate then
      ValidateRoot;
    FState := rsRoot;
  end;

  NewElem := doc.CreateElementBuf(FName.Buffer, FName.Length);
  FCursor.AppendChild(NewElem);

  // Find declaration for this element
  ElDef := nil;
  if Assigned(FDocType) then
  begin
    ElDef := TDOMElementDef(FDocType.ElementDefs.GetNamedItem(NewElem.TagName));
    if (ElDef = nil) or (not ElDef.HasElementDecl) then
      ValidationError('Using undeclared element ''%s''',[NewElem.TagName], FName.Length);
  end;

  // Check if new element is allowed in current context
  if FValidate and not FValidator[FNesting].IsElementAllowed(ElDef) then
    ValidationError('Element ''%s'' is not allowed in this context',[NewElem.TagName], FName.Length);

  IsEmpty := False;
  if SkipWhitespaceRaw then
  begin
    while (FCurChar <> '>') and (FCurChar <> '/') do
    begin
      if not CheckName then
        RaiseNameNotFound;
      attr := doc.CreateAttributeBuf(FName.Buffer, FName.Length);

      // !!cannot use TDOMElement.SetAttributeNode because it will free old attribute
      OldAttr := NewElem.Attributes.SetNamedItem(Attr);
      if Assigned(OldAttr) then
      begin
        OldAttr.Free;
        FatalError('Duplicate attribute', FName.Length);
      end;
      ExpectEq;
      FCursor := attr;
      ExpectAttValue;
      if (FCurChar <> '>') and (FCurChar <> '/') then
        ExpectWhitespace;
    end;   // while
  end;
  if FCurChar = '/' then
  begin
    IsEmpty := True;
    GetChar;
  end;
  ExpectChar('>');

  ProcessDefaultAttributes(NewElem, ElDef);

  PushVC(ElDef);
  // SAX: ContentHandler.StartElement(...)
  // SAX: ContentHandler.StartPrefixMapping(...)

  if not IsEmpty then
  begin
    FCursor := NewElem;
    if not FPreserveWhitespace then   // critical for testsuite compliance
      SkipWhitespaceRaw;
    ParseContent;
    if FCurChar = '/' then         // Get ETag [42]
    begin
      GetChar;
      StoreLocation(FTokenStart);
      if not CheckName then
        RaiseNameNotFound;
      if not NameIs(NewElem.TagName) then
        FatalError('Unmatching element end tag (expected "</%s>")', [NewElem.TagName], FName.Length);
      SkipWhitespaceRaw;
      ExpectChar('>');
    end
    else if FCurChar <> #0 then
      RaiseNameNotFound
    else // End of stream in content
      FatalError('End-tag is missing for ''%s''', [NewElem.TagName]);
  end;
  // SAX: ContentHandler.EndElement(...)
  // SAX: ContentHandler.EndPrefixMapping(...)
  FCursor := NewElem.ParentNode;
  if FCursor = doc then
    FState := rsEpilog;

  if FValidate and FValidator[FNesting].Incomplete then
    ValidationError('Element ''%s'' is missing required sub-elements', [NewElem.TagName]);

  PopVC;
end;

procedure TXMLReader.AddForwardRef(aList: TFPList; Buf: PWideChar; Length: Integer);
var
  w: PForwardRef;
begin
  New(w);
  SetString(w^.Value, Buf, Abs(Length));
  StoreLocation(w^.Loc);
  if Length > 0 then
    Dec(w^.Loc.LinePos, Length);
  aList.Add(w);
end;

procedure TXMLReader.ClearRefs(aList: TFPList);
var
  I: Integer;
begin
  for I := 0 to aList.Count-1 do
    Dispose(PForwardRef(aList.List^[I]));
  aList.Clear;
end;

procedure TXMLReader.ValidateIdRefs;
var
  I: Integer;
begin
  for I := 0 to FIDRefs.Count-1 do
    with PForwardRef(FIDRefs.List^[I])^ do
      if Doc.GetElementById(Value) = nil then
        DoErrorPos(esError, Format('The ID ''%s'' does not match any element', [Value]), Loc);
  ClearRefs(FIDRefs);
end;

procedure TXMLReader.ProcessDefaultAttributes(Element: TDOMElement; ElDef: TDOMElementDef);
var
  Map: TDOMNamedNodeMap;
  Attr: TDOMAttr;

procedure DoDefaulting;
var
  AttValue: WideString;
  I, L, StartPos, EndPos: Integer;
  Entity: TDOMEntity;
  AttDef: TDOMAttrDef;
begin
  Map := ElDef.FAttributes;

  for I := 0 to Map.Length-1 do
  begin
    AttDef := Map[I] as TDOMAttrDef;

    Attr := Element.GetAttributeNode(AttDef.Name);
    if Attr = nil then
    begin
      // attribute needs defaulting
      case AttDef.FDefault of
        adDefault, adFixed: begin
          if FStandalone and AttDef.FExternallyDeclared then
            StandaloneError;
          // Cloning TDOMAttrDef creates TDOMAttr. DataType is copied.
          Attr := TDOMAttr(AttDef.CloneNode(True));
          TDOMAttrDef(Attr).FSpecified := False;  // Dirty hack...
          TDOMAttrDef(Attr).FDeclared := True;
          Element.SetAttributeNode(Attr);
        end;
        adRequired:  ValidationError('Required attribute ''%s'' of element ''%s'' is missing',[AttDef.Name, Element.TagName], 0)
      end;
    end
    else
    begin
      TDOMAttrDef(Attr).FDeclared := True;
      // bypass heavyweight operations if possible
      if (AttDef.DataType <> dtCdata) or (AttDef.FDefault = adFixed) then
      begin
        AttValue := Attr.Value; // unnormalized
        // now assign DataType so that value is correctly normalized
        TDOMAttrDef(Attr).FDataType := AttDef.FDataType;
        if FStandalone and AttDef.FExternallyDeclared and (Attr.Value <> AttValue) then
          StandaloneError;
        AttValue := Attr.Value; // recalculate
        // TODO: what about normalization of AttDef.Value? (Currently it IS normalized)
        if (AttDef.FDefault = adFixed) and (AttDef.Value <> AttValue) then
          ValidationError('Value of attribute ''%s'' does not match its #FIXED default',[AttDef.Name], 0);
        if not ValidateAttrSyntax(AttDef, AttValue) then
          ValidationError('Attribute ''%s'' type mismatch', [AttDef.Name], 0);
      end;
    end;

    if Attr = nil then
      Continue;
    L := Length(AttValue);
    case Attr.DataType of
      dtId: if not Doc.AddID(Attr) then
              ValidationError('The ID ''%s'' is not unique', [AttValue], 0);

      dtIdRef, dtIdRefs: begin
        StartPos := 1;
        while StartPos <= L do
        begin
          EndPos := StartPos;
          while (EndPos <= L) and (AttValue[EndPos] <> #32) do
            Inc(EndPos);
          // pass negative Length, so current location is not altered
          AddForwardRef(FIDRefs, @AttValue[StartPos], StartPos-EndPos);
          StartPos := EndPos + 1;
        end;
      end;

      dtEntity, dtEntities: begin
        StartPos := 1;
        while StartPos <= L do
        begin
          EndPos := StartPos;
          while (EndPos <= L) and (AttValue[EndPos] <> #32) do
            Inc(EndPos);
          Entity := TDOMEntity(FDocType.Entities.GetNamedItem(Copy(AttValue, StartPos, EndPos-StartPos)));
          if (Entity = nil) or (Entity.NotationName = '') then
            ValidationError('Attribute ''%s'' type mismatch', [Attr.Name], 0);
          StartPos := EndPos + 1;
        end;
      end;
    end;
  end;
end;

procedure ReportUndeclared;
var
  I: Integer;
begin
  Map := Element.Attributes;
  for I := 0 to Map.Length-1 do
  begin
    Attr := TDOMAttr(Map[I]);
    if not TDOMAttrDef(Attr).FDeclared then
      ValidationError('Using undeclared attribute ''%s'' on element ''%s''',[Attr.Name, Element.TagName], 0);
  end;
end;

begin
  if Assigned(ElDef) and Assigned(ElDef.FAttributes) then
    DoDefaulting;
  // Now report undeclared attributes
  if Assigned(FDocType) and Element.HasAttributes then
    ReportUndeclared;
end;

function TXMLReader.ParseExternalID(out SysID, PubID: WideString;     // [75]
  SysIdOptional: Boolean): Boolean;
begin
  if FCurChar = 'S' then
  begin
    ExpectString('SYSTEM');
    ExpectWhitespace;
    SkipSystemLiteral(SysID);
    Result := True;
  end
  else if FCurChar = 'P' then
  begin
    ExpectString('PUBLIC');
    ExpectWhitespace;
    SkipPubidLiteral;
    SetString(PubID, FValue.Buffer, FValue.Length);
    NormalizeSpaces(PubID);
    if SysIdOptional then
    begin
      SkipWhitespace;
      if SkipQuotedLiteral then
        SetString(SysID, FValue.Buffer, FValue.Length);
    end
    else
    begin
      ExpectWhitespace;
      SkipSystemLiteral(SysID);
    end;
    Result := True;
  end else
    Result := False;
end;

procedure TXMLReader.CallErrorHandler(E: EXMLReadError);
begin
  try
    if Assigned(FCtrl) and Assigned(FCtrl.FOnError) then
      FCtrl.FOnError(E);
    if E.Severity = esFatal then
      raise E;
  except
    if ExceptObject <> E then
      E.Free;
    raise;
  end;
end;

function TXMLReader.ValidateAttrSyntax(AttrDef: TDOMAttrDef; const aValue: WideString): Boolean;
begin
  case AttrDef.FDataType of
    dtId, dtIdRef, dtEntity: Result := IsXmlName(aValue, FXML11);
    dtIdRefs, dtEntities: Result := IsXmlNames(aValue, FXML11);
    dtNmToken: Result := IsXmlNmToken(aValue, FXML11) and AttrDef.HasEnumToken(aValue);
    dtNmTokens: Result := IsXmlNmTokens(aValue, FXML11);
    // IsXmlName() not necessary - enum is never empty and contains valid names
    dtNotation: Result := AttrDef.HasEnumToken(aValue);
  else
    Result := True;
  end;
end;

procedure TXMLReader.ValidateRoot;
begin
  if Assigned(FDocType) then
  begin
    if not NameIs(FDocType.Name) then
      ValidationError('Root element name does not match DTD', [], FName.Length);
  end
  else
    ValidationError('Missing DTD', [], FName.Length);
end;

procedure TXMLReader.ValidateDTD;
var
  I: Integer;
begin
  if FValidate then
    for I := 0 to FNotationRefs.Count-1 do
      with PForwardRef(FNotationRefs[I])^ do
        if FDocType.Notations.GetNamedItem(Value) = nil then
          DoErrorPos(esError, Format('Notation ''%s'' is not declared', [Value]), Loc);
  ClearRefs(FNotationRefs);
end;

procedure TXMLReader.DoText(ch: PWideChar; Count: Integer; Whitespace: Boolean);
var
  TextNode: TDOMText;
begin
  // Validating filter part
  case FCurrContentType of
    ctChildren:
      if not Whitespace then
        ValidationError('Character data is not allowed in element-only content',[])
      else
        if FSaViolation then
          StandaloneError(-1);
    ctEmpty:
      ValidationError('Character data is not allowed in EMPTY elements', []);
  end;

  // Document builder part
  TextNode := Doc.CreateTextNodeBuf(ch, Count);
  TextNode.MayBeIgnorable := Whitespace;
  FCursor.AppendChild(TextNode);
end;

procedure TXMLReader.DoAttrText(ch: PWideChar; Count: Integer);
begin
  FCursor.AppendChild(Doc.CreateTextNodeBuf(ch, Count));
end;

procedure TXMLReader.DoComment(ch: PWideChar; Count: Integer);
var
  Node: TDOMComment;
begin
  // validation filter part
  if FCurrContentType = ctEmpty then
    ValidationError('Comments are not allowed within EMPTY elements', []);

  // DOM builder part
  if (not FIgnoreComments) and Assigned(FCursor) then
  begin
    Node := Doc.CreateCommentBuf(ch, Count);
    FCursor.AppendChild(Node);
  end;
end;

procedure TXMLReader.DoCDSect(ch: PWideChar; Count: Integer);
var
  s: WideString;
begin
  if FCurrContentType = ctChildren then
    ValidationError('CDATA sections are not allowed in element-only content',[]);

  if not FCDSectionsAsText then
  begin
    SetString(s, ch, Count);
    // SAX: LexicalHandler.StartCDATA;
    // SAX: ContentHandler.Characters(...);
    FCursor.AppendChild(doc.CreateCDATASection(s));
    // SAX: LexicalHandler.EndCDATA;
  end
  else
    FCursor.AppendChild(doc.CreateTextNodeBuf(ch, Count));
end;

procedure TXMLReader.DoNotationDecl(const aName, aPubID, aSysID: WideString);
var
  Notation: TDOMNotationEx;
begin
  if FDocType.Notations.GetNamedItem(aName) = nil then
  begin
    Notation := TDOMNotationEx(TDOMNotation.Create(doc));
    Notation.FName := aName;
    Notation.FPublicID := aPubID;
    Notation.FSystemID := aSysID;
    FDocType.Notations.SetNamedItem(Notation);
  end
  else
    ValidationError('Duplicate notation declaration: ''%s''', [aName]);
end;

procedure TXMLReader.PushVC(aElDef: TDOMElementDef);
begin
  Inc(FNesting);
  if FNesting = Length(FValidator) then
    SetLength(FValidator, FNesting * 2);
  FValidator[FNesting].FElementDef := aElDef;
  FValidator[FNesting].FCurCP := nil;
  FValidator[FNesting].FFailed := False;
  UpdateConstraints;
end;

procedure TXMLReader.PopVC;
begin
  if FNesting > 0 then Dec(FNesting);
  UpdateConstraints;
end;

procedure TXMLReader.UpdateConstraints;
begin
  if FValidate and Assigned(FValidator[FNesting].FElementDef) then
  begin
    FCurrContentType := FValidator[FNesting].FElementDef.ContentType;
    FSaViolation := FStandalone and (FValidator[FNesting].FElementDef.FExternallyDeclared);
  end
  else
  begin
    FCurrContentType := ctAny;
    FSaViolation := False;
  end;
end;

{ TDOMAttrDef }

function TDOMAttrDef.AddEnumToken(const aValue: WideString): Boolean;
var
  I, L: Integer;
begin
  // TODO: this implementaion is the slowest possible...
  Result := False;
  L := Length(FEnumeration);
  for I := 0 to L-1 do
  begin
    if aValue = FEnumeration[I] then
      Exit;
  end;
  SetLength(FEnumeration, L+1);
  FEnumeration[L] := aValue;
  Result := True;
end;

function TDOMAttrDef.HasEnumToken(const aValue: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(FEnumeration) = 0 then
    Exit;
  for I := 0 to Length(FEnumeration)-1 do
  begin
    if FEnumeration[I] = aValue then
      Exit;
  end;
  Result := False;
end;

{ TElementValidator }

function TElementValidator.IsElementAllowed(Def: TDOMElementDef): Boolean;
var
  I: Integer;
  Next: TContentParticle;
begin
  Result := True;
  // if element is not declared, non-validity has been already reported, no need to report again...
  if Assigned(Def) and Assigned(FElementDef) then
  begin
    case FElementDef.ContentType of
      ctMixed: begin
        for I := 0 to FElementDef.RootCP.ChildCount-1 do
        begin
          if Def = FElementDef.RootCP.Children[I].Def then
          Exit;
        end;
        Result := False;
      end;

      ctEmpty: Result := False;

      ctChildren: begin
        if FCurCP = nil then
          Next := FElementDef.RootCP.FindFirst(Def)
        else
          Next := FCurCP.FindNext(Def, 0); { second arg ignored here }
        Result := Assigned(Next);
        if Result then
          FCurCP := Next
        else
          FFailed := True;  // used to prevent extra error at the end of element
      end;
      // ctAny: returns True by default
    end;
  end;
end;

function TElementValidator.Incomplete: Boolean;
begin
  if Assigned(FElementDef) and (FElementDef.ContentType = ctChildren) and (not FFailed) then
  begin
    if FCurCP <> nil then
      Result := FCurCP.MoreRequired(0) { arg ignored here }
    else
      Result := FElementDef.RootCP.IsRequired;
  end
  else
    Result := False;
end;

{ TContentParticle }

function TContentParticle.Add: TContentParticle;
begin
  if FChildren = nil then
    FChildren := TFPList.Create;
  Result := TContentParticle.Create;
  Result.FParent := Self;
  Result.FIndex := FChildren.Add(Result);
end;

destructor TContentParticle.Destroy;
var
  I: Integer;
begin
  if Assigned(FChildren) then
    for I := FChildren.Count-1 downto 0 do
      TObject(FChildren[I]).Free;
  FChildren.Free;
  inherited Destroy;
end;

function TContentParticle.GetChild(Index: Integer): TContentParticle;
begin
  Result := TContentParticle(FChildren[Index]);
end;

function TContentParticle.GetChildCount: Integer;
begin
  if Assigned(FChildren) then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TContentParticle.IsRequired: Boolean;
var
  I: Integer;
begin
  Result := (CPQuant = cqOnce) or (CPQuant = cqOnceOrMore);
  // do not return True if all children are optional
  if (CPType <> ctName) and Result then
  begin
    for I := 0 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
end;

function TContentParticle.MoreRequired(ChildIdx: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if CPType = ctSeq then
  begin
    for I := ChildIdx + 1 to ChildCount-1 do
    begin
      Result := Children[I].IsRequired;
      if Result then Exit;
    end;
  end;
  if Assigned(FParent) then
    Result := FParent.MoreRequired(FIndex);
end;

function TContentParticle.FindFirst(aDef: TDOMElementDef): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  case CPType of
    ctSeq:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aDef);
        if Assigned(Result) or IsRequired then
          Exit;
      end;
    ctChoice:
      for I := 0 to ChildCount-1 do with Children[I] do
      begin
        Result := FindFirst(aDef);
        if Assigned(Result) then
          Exit;
      end;
  else // ctName
    if aDef = Self.Def then
      Result := Self
  end;
end;

function TContentParticle.FindNext(aDef: TDOMElementDef;
  ChildIdx: Integer): TContentParticle;
var
  I: Integer;
begin
  Result := nil;
  if CPType = ctSeq then   // search sequence to its end
  begin
    for I := ChildIdx + 1 to ChildCount-1 do with Children[I] do
    begin
      Result := FindFirst(aDef);
      if (Result <> nil) or IsRequired then
        Exit;
    end;
  end;
  if (CPQuant = cqZeroOrMore) or (CPQuant = cqOnceOrMore) then
    Result := FindFirst(aDef);
  if (Result = nil) and Assigned(FParent) then
    Result := FParent.FindNext(aDef, FIndex);
end;

{ TDOMElementDef }

constructor TDOMElementDef.Create(aOwner: TDOMDocument);
begin
  inherited Create(aOwner);
  RootCP := TContentParticle.Create;
end;

destructor TDOMElementDef.Destroy;
begin
  RootCP.Free;
  inherited Destroy;
end;

{ plain calls }

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Src := TXMLFileInputSource.Create(f);
  Src.SystemID := FilenameToURI(TTextRec(f).Name);
  Reader := TXMLReader.Create;
  try
    Reader.ProcessXML(Src);
    ADoc := TXMLDocument(Reader.Doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessXML(Src);
  finally
    ADoc := TXMLDocument(Reader.doc);
    Reader.Free;
  end;
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadXMLFile(ADoc, f, 'stream:');
end;

procedure ReadXMLFile(out ADoc: TXMLDocument; const AFilename: String);
var
  FileStream: TStream;
begin
  ADoc := nil;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFile(ADoc, FileStream, FilenameToURI(AFilename));
  finally
    FileStream.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Src.SystemID := FilenameToURI(TTextRec(f).Name);
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessFragment(Src, AParentNode);
  finally
    Reader.Free;
  end;
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; var f: TStream);
begin
  ReadXMLFragment(AParentNode, f, 'stream:');
end;

procedure ReadXMLFragment(AParentNode: TDOMNode; const AFilename: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadXMLFragment(AParentNode, Stream, FilenameToURI(AFilename));
  finally
    Stream.Free;
  end;
end;


procedure ReadDTDFile(out ADoc: TXMLDocument; var f: Text);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLFileInputSource.Create(f);
    Src.SystemID := FilenameToURI(TTextRec(f).Name);
    Reader.ProcessDTD(Src);
    ADoc := TXMLDocument(Reader.doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream; const ABaseURI: String);
var
  Reader: TXMLReader;
  Src: TXMLCharSource;
begin
  ADoc := nil;
  Reader := TXMLReader.Create;
  try
    Src := TXMLStreamInputSource.Create(f, False);
    Src.SystemID := ABaseURI;
    Reader.ProcessDTD(Src);
    ADoc := TXMLDocument(Reader.doc);
  finally
    Reader.Free;
  end;
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; var f: TStream);
begin
  ReadDTDFile(ADoc, f, 'stream:');
end;

procedure ReadDTDFile(out ADoc: TXMLDocument; const AFilename: String);
var
  Stream: TStream;
begin
  ADoc := nil;
  Stream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    ReadDTDFile(ADoc, Stream, FilenameToURI(AFilename));
  finally
    Stream.Free;
  end;
end;




end.
