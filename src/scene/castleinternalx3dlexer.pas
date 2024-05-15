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

{ VRML/X3D classic lexer (TX3DLexer). }
unit CastleInternalX3DLexer;

{$I castleconf.inc}

interface

uses SysUtils, Classes, CastleUtils, CastleStringUtils, CastleClassUtils,
  Math;

type
  { Valid keywords for all VRML / X3D versions. }
  TX3DKeyword = (vkDEF, vkEXTERNPROTO, vkFALSE, vkIS, vkNULL, vkPROTO, vkROUTE,
    vkTO, vkTRUE, vkUSE, vkEventIn, vkEventOut, vkExposedField, vkField,
    { Below keywords are X3D-only as far as specification is concerned.
      However, we decide to support IMPORT/EXPORT for older VRML versions
      too (the downside is that you cannot name your nodes like this,
      but the upside is that you can use these features in all VRML versions.) } { }
    vkAS, vkEXPORT, vkIMPORT,
    { X3D-only keywords below } { }
    vkCOMPONENT, vkMETA, vkPROFILE, vkUNIT,
    vkInputOnly, vkOutputOnly, vkInputOutput, vkInitializeOnly);

  TX3DKeywords = set of TX3DKeyword;

const
  VRML10Keywords = [vkDEF, vkUSE, vkFALSE, vkTRUE];
  VRML20Keywords = [vkDEF .. vkIMPORT] +
    { META accepted as Titania demos use it } [vkMETA];
  X3DKeywords = [Low(TX3DKeyword) .. High(TX3DKeyword)] -
    [vkEventIn, vkEventOut, vkExposedField, vkField];

type
  { Lexer token. }
  TX3DToken = (
    vtKeyword,
    vtName,

    { Symbols for all VRML versions.
      @groupBegin }
    vtOpenCurlyBracket, vtCloseCurlyBracket,
    vtOpenSqBracket, vtCloseSqBracket,
    { @groupEnd }

    { Symbols below are only for VRML <= 1.0.
      In VRML 2.0, they are no longer valid symbols
      (comma is even considered a whitespace).
      They will never be returned by lexer when reading VRML >= 2.0 files.

      @groupBegin }
    vtOpenBracket, vtCloseBracket, vtBar, vtComma,
    { @groupEnd }

    { Symbols below are only for VRML >= 2.0.
      They will never be returned by lexer when reading VRML < 2.0 files.
      @groupBegin }
    vtPeriod,
    { @groupEnd }

    { Symbols below are only for VRML >= 3.0, that is X3D.
      They will never be returned by lexer when reading VRML < 3.0 files.
      @groupBegin }
    vtColon,
    { @groupEnd }

    { Back to version-neutral tokens, suitable for all VRML / X3D versions.
      @groupBegin }
    vtFloat, vtInteger, vtString,
    { @groupEnd }

    { vtEnd means that we're standing at the end of stream, no more tokens.
      From this point, further reads using NextToken from stream will
      always result in vtEnd (they will not raise an error). }
    vtEnd);
  TX3DTokens = set of TX3DToken;

  { Any error related to VRML/X3D. }
  EX3DError = class(Exception);

  EGzipCompressed = class(Exception);

const
  TokenNumbers : TX3DTokens = [vtFloat, vtInteger];

type
  TX3DEncoding = (xeClassic, xeXML);

  TX3DVersion = record
    Major, Minor: Integer;
  end;

  { VRML/X3D (classic encoding) lexer.

    The lexer always "looks" (i.e. contains in Token and TokenXxx fields)
    at the next not yet interpreted token.

    Remember that VRML is case-sensitive, so TokenName and TokenString
    should be compared in case-sensitive manner. Also note that
    for VRML/X3D >= 2.0 these fields contain UTF-8 encoded strings.

    Note that this lexer can read only from @link(TPeekCharStream), not just
    from any TStream. You may have to wrap your stream in some
    @link(TPeekCharStream) descendant (for example create TFileStream
    and then wrap it inside @link(TBufferedReadStream)). }
  TX3DLexer = class
  private
    fVersion: TX3DVersion;
    fToken: TX3DToken;
    fTokenKeyword: TX3DKeyword;
    fTokenName: string;
    fTokenFloat: Float;
    fTokenInteger: Int64;
    fTokenString: string;

    VRMLWhitespaces, VRMLNoWhitespaces: TSetOfChars;
    VRMLNameChars, VRMLNameFirstChars: TSetOfChars;

    FStream: TPeekCharStream;
    FOwnsStream: boolean;

    { Reads chars from Stream until EOF or some non-white char will
      be approached. Omits VRML comments. Returns as FirstBlack
      -1 (if EOF) or Ord(of this non-white char). (This non-white
      char will be already read from Stream, so usually you MUST do
      something with returned here FirstBlack, you can't ignore him) }
    procedure StreamReadUptoFirstBlack(out FirstBlack: Integer);

    { Read string. Initial " has been already read. Reads everything
      up to (and including) " terminating the string.
      Sets fToken and fTokenString to appropriate values
      (i.e. fToken always to vtString, fTokenString to string contents). }
    procedure ReadString;

    { Helpers for implementing constructors. They do everything, besides
      reading header from the stream (before the first real token)
      and setting VRMLVerXxx fields, you must do it between the call
      to CreateCommonBegin and CreateCommonEnd.

      @groupBegin }
    procedure CreateCommonBegin(AStream: TPeekCharStream;
      AOwnsStream: boolean);
    procedure CreateCommonEnd;
    { @groupEnd }
  public
    { Standard constructor.
      After constructor call, @link(Version) is already set,
      it's checked that file is not compressed by gzip, and the first
      Token is already read.
      @raises(EGzipCompressed If the Stream starts with gzip file header.) }
    constructor Create(AStream: TPeekCharStream; AOwnsStream: boolean);

    { Constructor for the case when you only have part of normal
      VRML tokens stream.

      This is particularly useful to parse fields of X3D in XML encoding.
      Inside XML attributes we have then a text that can parsed with
      a classical VRML lexer, to parse fields contents.

      This creates a lexer that works quite like a normal lexer.
      At creation time it doesn't expect header line (like @code(#VRML 2.0 utf8)),
      that why you have to supply VRML major and minor version as
      parameters here. Also it doesn't try to detect gzip header.
      It simply behaves like we're in the middle of VRML tokens stream.

      Overloaded version with a first parameter as string simply reads
      tokens from this string (wrapping it in TStringStream and TPeekCharStream).

      @groupBegin }
    constructor CreateForPartialStream(
      AStream: TPeekCharStream; AOwnsStream: boolean;
      const AVersion: TX3DVersion); overload;
    constructor CreateForPartialStream(const S: string;
      const AVersion: TX3DVersion); overload;
    { @groupEnd }

    destructor Destroy; override;

    { The stream we're reading.
      This is simply the AStream that you passed to the constructor
      of this class.

      Note that you can't operate on this stream from outside while lexer
      works, this could confuse the lexer. But you're free to read
      some stream properties, e.g. check Stream.Position. }
    property Stream: TPeekCharStream read FStream;

    { VRML/X3D version, as recorded in the file header.

      All VRML 1.0, 2.0, X3D (various 3.x) are recognized correctly.
      For Inventor 1.0 ascii, we set Version.Major to 0
      (as historically Inventor is a predecessor to VRML 1.0). }
    property Version: TX3DVersion read fVersion;

    { Token we're currently standing on.
      TokenKeyword, TokenName, TokenFloat and TokenInteger have defined
      values only when token type is appropriate. }
    property Token: TX3DToken read fToken;

    { When Token = vtKeyword, TokenKeyword points to appropriate keyword.

      When Version.Major = 1, then you can be sure that TokenKeyword is
      in VRML10Keywords. Analogous for VRML20Keywords and X3DKeywords.
      So e.g. in VRML 1.0 "PROTO" will be treated like a normal name,
      not a start of prototype. }
    property TokenKeyword: TX3DKeyword read fTokenKeyword;

    { When Token = vtName, TokenName contains appropriate VRML name.

      Name syntax as in specification on page 24 (really 32 in pdf) of
      vrml97specification.pdf. It can be a user name for something (for a node,
      for example) but it can also be a name of a node type or a node field
      or an enumerated field constant ... it can be @italic(anything)
      except keyword.

      Note that this is supposed to contain UTF-8 encoded string for VRML >= 2.0. }
    property TokenName: string read fTokenName;

    { TokenName, decoded using DecodeX3DName. }
    function TokenX3DName: String;

    { When Token = vtFloat or vtInteger, TokenFloat contains a value of
      this token.

      For vtInteger you have the same thing in TokenInteger,
      TokenFloat is also initialized to the same value for your comfort
      (every integer value is also a float, after all). }
    property TokenFloat: Float read fTokenFloat;

    { When Token = vtInteger, TokenInteger contains appropriate value. }
    property TokenInteger: Int64 read fTokenInteger;

    { When Token = vtString, TokenString contains string value. }
    property TokenString: string read fTokenString;

    { NextToken reads next token from stream, initializing appropriately
      all Token* properties. For comfort, this returs the new value of
      @link(Token) property. }
    function NextToken: TX3DToken;

    { Read the next token, knowing that it @italic(must) be vtName token.
      This is basically a dirty hack to read some incorrect VRML files,
      that use not allowed characters in VRML names. This allows us to
      accept as a vtName some characters that normally (when using normal
      NextToken) would get interpreted as other token.

      For example, mgf2inv can write name @code(0) (a zero, that would be
      read as vtInteger token in normal circumstances), on some WWW page
      I found sample VRML models with node name @code("Crab!") (yes,
      with exclamation mark and double quotes as part of the node name).

      @raises(EX3DParserError When we really really cannot interpret contents
        as vtName token here --- currently this may happen only if end of
        stream is reached. Note that this is reported as a parsing error.) }
    procedure NextTokenForceVTName;

    { Read the next token, knowing that it @italic(must) be vtString token.

      Similiar to NextTokenForceVTName: use this like a shortcut for

      @longCode(#
        NextToken;
        CheckTokenIs(vtString);
      #)

      but it is not equivalent to such instructions. This is because
      VRML 1.0 allowed rather strange thing: string may be not enclosed
      in double quotes if it does not contain a space. This "feature"
      is not present in VRML >= 2.0, but, unfortunately, I'm trying to handle
      VRML 1.0 here so I have to conform to this specification.
      In particular, Blender generates VRML 1.0 files with Texture2.filename
      fields not enclosed in double quotes. So this "feature" is actually
      used by someone... So I have to implement this.

      Usual NextToken will not be able to return vtString if it approaches
      a string not enclosed in double quotes. But THIS function
      will be able to handle it. So always use this function when
      you expect a string, this ensures
      that we will correctly parse any valid VRML 1.0 file.

      (unfortunately I'm not doing this now when parsing MFString,
      this would just require too "unclean" code; I'm using this
      function only before calling parse on SFString field from
      TX3DNode.Parse.) }
    procedure NextTokenForceVTString;

    { Returns if Token is vtKeyword and TokenKeyword is given Keyword. }
    function TokenIsKeyword(const Keyword: TX3DKeyword): boolean; overload;
    function TokenIsKeyword(const Keywords: TX3DKeywords): boolean; overload;

    { Nice textual description of current token, suitable to show to user. }
    function DescribeToken: string;

    { Check is token = Tok, if not -> parser error "expected token 'tok'".
      You can provide your own description for Tok or default desciption
      for token will be used. }
    procedure CheckTokenIs(Tok: TX3DToken); overload;
    procedure CheckTokenIs(Tok: TX3DToken; const TokDescription: string); overload;
    procedure CheckTokenIs(const Toks: TX3DTokens; const ToksDescription: string); overload;
    procedure CheckTokenIsKeyword(const Keyword: TX3DKeyword);
  end;

  { Error when reading VRML/X3D classic encoding. }
  EX3DClassicReadError = class(EX3DError)
  public
    { Standard constructor.

      Lexer instance must be valid for this call, but not longer.
      That is, you can free the lexer after this constructor finished,
      it doesn't need to be valid for the whole lifetime of this object. }
    constructor Create(Lexer: TX3DLexer; const s: string);
  end;

  { Error when reading VRML/X3D. For now, just equal to EX3DClassicReadError,
    later may be an ancestor to EX3DClassicReadError.
    Problems in other encodings (XML) are for now always turned into warnings. }
  EX3DReadError = EX3DClassicReadError;

  EX3DLexerError = class(EX3DClassicReadError);
  EX3DParserError = class(EX3DClassicReadError);

const
  X3DKeywordsName: array [TX3DKeyword] of string = (
    'DEF', 'EXTERNPROTO', 'FALSE', 'IS', 'NULL', 'PROTO', 'ROUTE',
    'TO', 'TRUE', 'USE', 'eventIn', 'eventOut', 'exposedField', 'field',
    'AS', 'EXPORT', 'IMPORT',
    'COMPONENT', 'META', 'PROFILE', 'UNIT',
    'inputOnly', 'outputOnly', 'inputOutput', 'initializeOnly'
    );

{ String encoded for VRML/X3D classic, surrounded by double quotes.
  You can use this when generating VRML/X3D content by hand.

  Simply put, this just adds
  double quotes around and prepends backslash to all " and \ inside.
  Fortunately, no other characters (including newlines) need to be quoted
  in any special way for VRML/X3D.
  For example:

  @longCode(#
    StringToX3DClassic('foo') = '"foo"'
    StringToX3DClassic('say "yes"') = '"say \"yes\""'
  #) }
function StringToX3DClassic(const s: string;
  const SurroundWithQuotes: boolean = true): string;

{ String encoded for X3D XML, surrounded by double quotes.
  You can use this when generating VRML/X3D content by hand.

  Simply put, this just adds double quotes around and replaces
  all special characters (quotes, double quotes and more)
  with appropriate XML entities &xxx;. }
function StringToX3DXml(const s: string): string;

{ String encoded for X3D XML, surrounded by double quotes, to be used
  as part of MFString.

  In addition to what StringToX3DXml does, this additionally
  adds backslash before &quot; (double quotes), and doubles existing backslashes.
  Otherwise XML reader
  may not tell the difference between double quotes delimiting single string
  item and double quotes inside, as it has to (and DOM units do) treat
  " and &quot; exactly the same. X3D XML encoding spec shows example
  confirming this is correct,
  @code(<Text string='"He said, \&quot;Immel did it!\&quot;"' />). }
function StringToX3DXmlMulti(const s: string): string;

implementation

uses CastleLog,
  X3DLoadInternalUtils;

const
  { utf8 specific constants below }
  X3DLineTerm = [#10, #13];

  X3DTokenNames: array [TX3DToken] of string = (
    'keyword', 'name',
    '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."', '":"',
    'float', 'integer', 'string', 'end of stream');

var
  { Log all read tokens. Useful for debugging lexer. }
  LogTokens: boolean = false;

function ArrayPosX3DKeywords(const s: string; out Index: TX3DKeyword): boolean;
var
  I: TX3DKeyword;
begin
  for I {$ifdef FPC}:= Low(X3DKeywords) to High(X3DKeywords) {$else}in X3DKeywords{$endif} do
    if X3DKeywordsName[I] = s then
    begin
      Index := I;
      Result := true;
      Exit;
    end;
  Result := false;
end;

{ TX3DLexer ------------------------------------------------------------- }

procedure TX3DLexer.CreateCommonBegin(AStream: TPeekCharStream;
  AOwnsStream: boolean);
begin
  inherited Create;

  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

procedure TX3DLexer.CreateCommonEnd;
begin
  { calculate VRMLWhitespaces, VRMLNoWhitespaces
    (based on VRMLVerXxx) }
  VRMLWhitespaces := [' ',#9, #10, #13];
  if FVersion.Major >= 2 then
    Include(VRMLWhitespaces, ',');
  VRMLNoWhitespaces := AllChars - VRMLWhitespaces;

  { calculate VRMLNameChars, VRMLNameFirstChars }
  { These are defined according to vrml97specification on page 24. }
  VRMLNameChars := AllChars -
    [#0..#$1f, ' ', '''', '"', '#', ',', '.', '[', ']', '\', '{', '}'];
  if FVersion.Major <= 1 then
    VRMLNameChars := VRMLNameChars - ['(', ')', '|'];
  if FVersion.Major >= 3 then
    { X3D standard has a little less characters allowed.
      In particular, ':' (unicode 0x3a) is not allowed and should not be,
      because component statements are separated by vtColon.
      Detailed spec is in "IdFirstChar" and "IdRestChars" on X3D classic VRML
      spec grammat. }
    VRMLNameChars := VRMLNameChars - [':'];
  VRMLNameFirstChars := VRMLNameChars - ['0'..'9', '-','+'];

  {read first token}
  NextToken;
end;

constructor TX3DLexer.Create(AStream: TPeekCharStream; AOwnsStream: boolean);
const
  InventorHeaderStart = '#Inventor ';

  VRML1HeaderStart = '#VRML V1.0 ';
  EncodingAscii = 'ascii'; { Used by VRML 1.0 }

  VRML2HeaderStart = '#VRML V2.0 ';
  { This is not an official VRML header, but it's used by VRML models on
    [http://www.itl.nist.gov/div897/ctg/vrml/chaco/chaco.html] }
  VRML2DraftHeaderStart = '#VRML Draft #2 V2.0 ';
  EncodingUtf8 = 'utf8'; { Used by VRML >= 2.0 }

  X3DHeaderStart = '#X3D ';

  procedure Utf8HeaderReadRest(const Line: string);
  var
    Encoding: string;
  begin
    Encoding := NextTokenOnce(Line);
    if Encoding <> EncodingUtf8 then
      raise EX3DLexerError.Create(Self,
        'VRML 2.0 / X3D incorrect signature: only utf8 encoding supported');
  end;

  { If Prefix is a prefix of S, then return @true and remove this prefix
    from S. Otherwise return @false (without modifying S).
    Allows also an UTF-8 BOM before a prefix. }
  function CheckRemoveHeader(Prefix: string; var S: string): boolean;
  const
    Utf8Bom = #$EF + #$BB + #$BF;
  begin
    Result := IsPrefix(Prefix, S);
    if Result then
      Delete(S, 1, Length(Prefix)) else
    begin
      Prefix := Utf8Bom + Prefix;
      Result := IsPrefix(Prefix, S);
      if Result then
        Delete(S, 1, Length(Prefix));
    end;
  end;

  { Parse and remove Vmajor.minor version number from VRML header line.

    Note that this is slightly more flexible than VRML / X3D classic
    spec says (they require exactly one space before and after version
    number, we allow any number of whitespaces). }
  procedure ParseVersion(var S: string; out Major, Minor: Integer);
  const
    SIncorrectSignature = 'Inventor / VRML / X3D Incorrect signature: ';
    Digits = ['0' .. '9'];
  var
    NumStart, I: Integer;
  begin
    I := 1;

    { whitespace }
    while SCharIs(S, I, WhiteSpaces) do Inc(I);

    { "V" }
    if not SCharIs(S, I, 'V') then
      raise EX3DLexerError.Create(Self,
        SIncorrectSignature + 'Expected "V" and version number');
    Inc(I);

    { major number }
    NumStart := I;
    while SCharIs(S, I, Digits) do Inc(I);
    try
      Major := StrToInt(CopyPos(S, NumStart, I - 1));
    except
      on E: EConvertError do
        raise EX3DLexerError.Create(Self,
          SIncorrectSignature + 'Incorrect major version number: ' + E.Message);
    end;

    { dot }
    if not SCharIs(S, I, '.') then
      raise EX3DLexerError.Create(Self,
        SIncorrectSignature + 'Expected "." between major and minor version number');
    Inc(I);

    { minor number }
    NumStart := I;
    while SCharIs(S, I, Digits) do Inc(I);
    try
      Minor := StrToInt(CopyPos(S, NumStart, I - 1));
    except
      on E: EConvertError do
        raise EX3DLexerError.Create(Self,
          SIncorrectSignature + 'Incorrect minor version number: ' + E.Message);
    end;

    { whitespace }
    while SCharIs(S, I, WhiteSpaces) do Inc(I);

    Delete(S, 1, I - 1);
  end;

var
  FirstLine: AnsiString;
  Line: String;
begin
  CreateCommonBegin(AStream, AOwnsStream);

  { Read first line = signature. }
  FirstLine := Stream.ReadUpto(X3DLineTerm);

  if Length(FirstLine) < 2 then
    raise EX3DLexerError.Create(Self,
      'Unexpected end of file on the signature line');

  { Conveniently, GzipHeader doesn't contain X3DLineTerm.
    So if Line starts with GzipHeader, we know 100% it's gzip file,
    otherwise we know 100% it's not. }
  if (FirstLine[1] = #$1F) and (FirstLine[2] = #$8B) then
    raise EGzipCompressed.Create('Stream is compressed by gzip');

  Line := FirstLine;

  { Normal (uncompressed) VRML file, continue reading ... }

  if Stream.ReadChar = -1 then
    raise EX3DLexerError.Create(Self,
      'Unexpected end of file on the 1st line');

  { Recognize various Inventor / VRML / X3D headers,
    code below goes chronologically through various VRML etc. versions.

    For now ParseVersion is used only by X3D. But it could be used
    also by other Inventor / VRML versions. }

  if CheckRemoveHeader(InventorHeaderStart, Line) then
  begin
    FVersion.Major := 0;
    FVersion.Minor := 0;

    if not IsPrefix('V1.0 ascii', Line) then
      raise EX3DLexerError.Create(Self, Format(
        'Inventor format detected, version "%s", but only Inventor 1.0 ascii files are supported',
        [TrimRight(Line)]));
  end else
  if CheckRemoveHeader(VRML1HeaderStart, Line) then
  begin
    FVersion.Major := 1;
    FVersion.Minor := 0;

    { then must be 'ascii';
      VRML 1.0 'ascii' may be followed immediately by some black char. }
    if not IsPrefix(EncodingAscii, Line) then
      raise EX3DLexerError.Create(Self, 'Wrong VRML 1.0 signature: '+
        'VRML 1.0 files must have "ascii" encoding');
  end else
  if CheckRemoveHeader(VRML2DraftHeaderStart, Line) or
     CheckRemoveHeader(VRML2HeaderStart, Line) then
  begin
    FVersion.Major := 2;
    FVersion.Minor := 0;
    Utf8HeaderReadRest(Line);
  end else
  if CheckRemoveHeader(X3DHeaderStart, Line) then
  begin
    ParseVersion(Line, FVersion.Major, FVersion.Minor);
    if FVersion.Major < 3 then
      raise EX3DLexerError.Create(Self,
        'Wrong X3D major version number, should be >= 3');
    Utf8HeaderReadRest(Line);
  end else
    raise EX3DLexerError.Create(Self,
      'VRML signature error : unrecognized signature');

  CreateCommonEnd;
end;

constructor TX3DLexer.CreateForPartialStream(
  AStream: TPeekCharStream; AOwnsStream: boolean;
  const AVersion: TX3DVersion);
begin
  CreateCommonBegin(AStream, AOwnsStream);
  FVersion := AVersion;
  CreateCommonEnd;
end;

constructor TX3DLexer.CreateForPartialStream(const S: string;
  const AVersion: TX3DVersion);
var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(S);
  CreateForPartialStream(
    TBufferedReadStream.Create(StringStream, true), true, AVersion);
end;

destructor TX3DLexer.Destroy;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
  inherited;
end;

procedure TX3DLexer.StreamReadUptoFirstBlack(out FirstBlack: Integer);
begin
 repeat
  Stream.ReadUpto(VRMLNoWhitespaces);
  FirstBlack := Stream.ReadChar;

  { TODO: ignore X3D multiline comments also }

  { ignore comments }
  if FirstBlack = Ord('#') then
   Stream.ReadUpto(X3DLineTerm) else
   break;
 until false;
end;

procedure TX3DLexer.ReadString;
var
  endingChar: Integer;
  NextChar: Integer;
begin
 fToken := vtString;
 fTokenString := '';
 repeat
  fTokenString := fTokenString + Stream.ReadUpto(['\', '"']);
  endingChar := Stream.ReadChar;

  if endingChar = -1 then
   raise EX3DLexerError.Create(Self,
     'Unexpected end of file in the middle of string token');

  { When EndingChar is a backslash, and it is followed by " or \,
    we ignore the 1st backslash and the next character
    is interpreted verbatim.
    This way \\ becomes \ and \" becomes \.

    Moreover, for https://sourceforge.net/p/castle-engine/tickets/14/ ,
    we detect (and warn) about invalid sequences after backslash,
    and in this case we keep the backslash. }
  if EndingChar = Ord('\') then
  begin
    NextChar := Stream.ReadChar;
    if NextChar = -1 then
      raise EX3DLexerError.Create(Self,
        'Unexpected end of file in the middle of string token');
    if not CharInSet(Chr(NextChar), ['"', '\']) then
    begin
      WritelnWarning('X3D', 'Invalid X3D file: Invalid sequence in a string: "\%s". Backslash must be followed by another backslash or double quote, for SFString and MFString (in X3D classic (VRML) encoding) and for MFString (in X3D XML encoding).',
        [Chr(NextChar)]);
      FTokenString := FTokenString + '\';
    end;
    FTokenString := FTokenString + Chr(NextChar);
  end;

 until endingChar = Ord('"');
end;

function TX3DLexer.NextToken: TX3DToken;

  procedure ReadNameOrKeyword(FirstLetter: char);
  {read name token. First letter has been already read.}
  var foundKeyword: TX3DKeyword;
  const
    LowerCaseVKTrue = 'true' { LowerCase(X3DKeywords[vkTRUE]) };
    LowerCaseVKFalse = 'false' { LowerCase(X3DKeywords[vkFALSE]) };
  begin
   fTokenName := FirstLetter +Stream.ReadUpto(AllChars - VRMLNameChars);

   { teraz zobacz czy fTokenName nie jest przypadkiem keywordem. }
   if ArrayPosX3DKeywords(fTokenName, foundKeyword) and
      ( ( (FVersion.Major <= 1) and (foundKeyword in VRML10Keywords) ) or
        ( (FVersion.Major  = 2) and (foundKeyword in VRML20Keywords) )  or
        ( (FVersion.Major >= 3) and (foundKeyword in X3DKeywords) )
      ) then
   begin
     FToken := vtKeyword;
     FTokenKeyword := foundKeyword;
   end else
   if FVersion.Major >= 3 then
   begin
     { In X3D XML encoding you should specify SFBool / MFBool values
       as lower-case. From spec:

         Lower-case strings for true and false are used in order
         to maximize interoperability with other XML languages.

       In my engine, I relax this rule: in *any* X3D encoding (XML, classic...),
       you can use either lower-case or upper-case boolean values.
       This way all valid files are handled. }
     if FTokenName = LowerCaseVKTrue then
     begin
       FToken := vtKeyword;
       FTokenKeyword := vkTRUE;
     end else
     if FTokenName = LowerCaseVKFalse then
     begin
       FToken := vtKeyword;
       FTokenKeyword := vkFALSE;
     end else
       FToken := vtName;
   end else
     FToken := vtName;
  end;

  {
    VRML float token corresponds to Pascal Float type,
    in VRML it's expressed in the followin form:

    @preformatted(
      [("-"|"+")]
      (digit+ [ "." digit+ ] | "." digit+)
      [ "e"|"E" [("-"|"+")] digit+ ]
    )

    VRML integer token corresponds to Pascal Int64 type,
    in VRML it's expressed in the following form:

    @preformatted(
      (form : [("-"|"+")] ("0x" digit_hex+ | [1-9]digit_decimal* | 0 digit_octal+) )
    )
  }

  procedure ReadFloatOrInteger(FirstChar: char);
  const
    NoDigits = AllChars - ['0'..'9'];
    NoHexDigits = AllChars - ['0'..'9', 'a'..'f', 'A'..'F'];
    { TODO: octal notation not implemented (I never saw any model using it) }

    procedure ReadAfterE(const AlreadyRead: string);
    var CharAfterE: char;
        RestOfToken: string;
        CharAfterEInt: Integer;
    begin
     fToken := vtFloat;
     { Za "e" musi byc min 1 znak, to moze byc cyfra lub - lub +.
       Odczytujemy go do CharAfterE.
       Potem sa juz tylko cyfry, odczytujemy je do RestOfToken.
       (note: you can't write "Stream.ReadChar(Stream) + Stream.ReadUpto(NoDigits)"
       because it is undefined in what order S1+S2
       will be calculated. See console.testy/test_string_plus) }
     CharAfterEInt := Stream.ReadChar;
     if CharAfterEInt = -1 then
       raise EX3DLexerError.Create(Self,
         'Unexpected end of file in the middle of real constant');
     CharAfterE := Chr(CharAfterEInt);
     RestOfToken := Stream.ReadUpto(NoDigits);
     fTokenFloat := StrToFloatDot(AlreadyRead +'e' +CharAfterE +RestOfToken);
    end;

    procedure ReadAfterDot(const AlreadyRead: string);
    {AlreadyRead zawieraja dotychczas przeczytana liczbe calkowita ze znakiem.
     Wiemy ze potem odczytano kropke - czytamy dalej. }
    var s: string;
        AfterS: integer;
    begin
     s := AlreadyRead +'.' +Stream.ReadUpto(NoDigits);
     AfterS := Stream.PeekChar;
     if (AfterS = Ord('e')) or (AfterS = Ord('E')) then
     begin
      Stream.ReadChar;
      ReadAfterE(s);
     end else
     begin
      fToken := vtFloat;
      fTokenFloat := StrToFloatDot(s);
     end;
    end;

  var Dig1, HexDig: string;
      AfterDig1: integer;
  begin
   try
    if FirstChar = '.' then
     ReadAfterDot('') else
    begin
     Dig1 := FirstChar + Stream.ReadUpto(NoDigits);
     AfterDig1 := Stream.PeekChar;
     if (AfterDig1 = Ord('x')) and (ArrayPosStr(Dig1, ['0', '-0', '+0']) >= 0) then
     begin
      Stream.ReadChar; { consume AfterDig1 }
      HexDig := Stream.ReadUpto(NoHexDigits);
      fToken := vtInteger;
      fTokenInteger := StrHexToInt(HexDig);
      if Dig1[1] = '-' then fTokenInteger := - fTokenInteger;
     end else
     if (AfterDig1 = Ord('.')) then
     begin
      Stream.ReadChar; { consume AfterDig1 }
      { w przypadku liczby postaci -.9 Dig1 byc ponizej rowne '';
        to niczemu nie wadzi }
      ReadAfterDot(Dig1)
     end else
     if (AfterDig1 = Ord('e')) or (AfterDig1 = Ord('E')) then
     begin
      Stream.ReadChar; { consume AfterDig1 }
      ReadAfterE(Dig1)
     end else
     begin
      { odczytalismy zwyklego integera }
      FToken := vtInteger;
      try
        FTokenInteger := StrToInt64(Dig1);
      except
        on E: EConvertError do
        begin
          { We failed to use StrToInt64, but it's possibly a valid
            float value. It's just too large for 64-bit integer... }
          FToken := vtFloat;
          try
            FTokenFloat := StrToFloatDot(Dig1);
          except
            on EFloat: EConvertError do
              { Raise EConvertError with nice error message,
                explaining what we did. }
              raise EConvertError.CreateFmt('Trying to treat "%s" as ' +
                '64-bit integer failed (%s), trying to treat it as ' +
                'a float also failed (%s)', [Dig1, E.Message, EFloat.Message]);
          end;
        end;
      end;
     end;
    end;

    if fToken = vtInteger then fTokenFloat := TokenInteger;
   except
    on E: EConvertError do raise EX3DLexerError.Create(Self, E.Message);
   end;
  end;

  procedure RecognizeCommonTokens(FirstBlackChr: char);
  begin
    case FirstBlackChr of
     '{':fToken := vtOpenCurlyBracket;
     '}':fToken := vtCloseCurlyBracket;
     '[':fToken := vtOpenSqBracket;
     ']':fToken := vtCloseSqBracket;
     '-','+','.','0'..'9':ReadFloatOrInteger(FirstBlackChr);
     '"':ReadString;
     else
      if CharInSet(FirstBlackChr, VRMLNameFirstChars) then
       ReadNameOrKeyword(FirstBlackChr) else
       raise EX3DLexerError.Create(Self, Format('Illegal character in stream : %s (#%d)',
         [FirstBlackChr, Ord(FirstBlackChr)]));
    end;
  end;

var
  FirstBlack: integer;
  FirstBlackChr: char;
begin
  StreamReadUptoFirstBlack(FirstBlack);

  if FirstBlack = -1 then
    fToken := vtEnd else
  begin
    FirstBlackChr := Chr(FirstBlack);

    if FVersion.Major <= 1 then
    begin
      case FirstBlackChr of
        { VRML <= 1.0 symbols }
        '(': fToken := vtOpenBracket;
        ')': fToken := vtCloseBracket;
        '|': fToken := vtBar;
        ',': fToken := vtComma;
        else RecognizeCommonTokens(FirstBlackChr);
      end;
    end else
    begin
      { It's a little unsure lexer moment here. Maybe 12.34 means
        "token integer 12", "token dot", "token integer 34" ?
        Well, our decisions:

        1. Lexer is greedy, so if after 12 we have a dot,
        we assume it's a float. This means that in grammar, you cannot have
        allowed sequence integer + dot.

        2. If we see a dot, then we assume it's a float if after dot we have
        a digit. So ".12" is one token, float number. So you cannot have
        allowed sequence dot + integer in the grammar.

        It's not a problem in practice. "Dot" token is allowed only inside
        ROUTE statements as name + dot + name (and name doesn't start with
        digit), so it's all OK in practice. Valid VRML files may be
        unambiguously tokenized. }
      if (FirstBlackChr = '.') and
         (not Between(Stream.PeekChar, Ord('0'), Ord('9'))) then
        FToken := vtPeriod else
      { X3D only token }
      if ( (FirstBlackChr = ':') and (FVersion.Major >= 3) ) then
        FToken := vtColon else
        RecognizeCommonTokens(FirstBlackChr);
    end;
  end;

  if LogTokens then WritelnLog('X3D lexer', DescribeToken);

  result := Token;
end;

procedure TX3DLexer.NextTokenForceVTName;
var FirstBlack: integer;
begin
 StreamReadUptoFirstBlack(FirstBlack);

 if FirstBlack = -1 then
  fToken := vtEnd else
 begin
   (* Stop tokens include { } [ ], otherwise we risk that because of this
      hack (NextTokenForceVTName is really only a hack to try to read
      even incorrect VRML files) we would fail to read correctly valid
      VRML files. *)
  fTokenName := Chr(FirstBlack) +Stream.ReadUpto(
    VRMLWhitespaces + ['{', '}', '[', ']']);
  fToken := vtName;
 end;

  if LogTokens then WritelnLog('X3D lexer', DescribeToken);

 CheckTokenIs(vtName);
end;

procedure TX3DLexer.NextTokenForceVTString;
var FirstBlack: integer;
begin
 StreamReadUptoFirstBlack(FirstBlack);

 if FirstBlack = -1 then
  fToken := vtEnd else
 if FirstBlack = Ord('"') then
  ReadString else
 begin
  fTokenString := Chr(FirstBlack) + Stream.ReadUpto(VRMLWhitespaces);
  fToken := vtString;
 end;

 if LogTokens then WritelnLog('X3D lexer', DescribeToken);

 CheckTokenIs(vtString);
end;

function TX3DLexer.TokenIsKeyword(const Keyword: TX3DKeyword): boolean;
begin
  Result := (Token = vtKeyword) and (TokenKeyword = Keyword);
end;

function TX3DLexer.TokenIsKeyword(const Keywords: TX3DKeywords): boolean;
begin
  Result := (Token = vtKeyword) and (TokenKeyword in Keywords);
end;

function TX3DLexer.DescribeToken: string;
begin
  Result := X3DTokenNames[Token];
  case Token of
    vtKeyword: Result := Result +' "' + X3DKeywordsName[TokenKeyword] +'"';
    vtName: Result := 'identifier (unquoted in X3D file) "' + TokenName +'"';
    vtFloat: Result := Result +' ' + FloatToStrDot(TokenFloat);
    vtInteger: Result := Result +' ' + IntToStr(TokenInteger);
    vtString: Result := Result +' "' + TokenString + '"';
    else ;
  end;
end;

procedure TX3DLexer.CheckTokenIs(Tok: TX3DToken);
begin
 CheckTokenIs(Tok, X3DTokenNames[Tok]);
end;

procedure TX3DLexer.CheckTokenIs(Tok: TX3DToken; const TokDescription: string);
begin
 if Token <> Tok then
  raise EX3DParserError.Create(Self, 'Expected '+TokDescription
    +', got '+DescribeToken);
end;

procedure TX3DLexer.CheckTokenIs(const Toks: TX3DTokens; const ToksDescription: string);
begin
 if not (Token in Toks) then
  raise EX3DParserError.Create(Self, 'Expected '+ToksDescription
    +', got '+DescribeToken);
end;

procedure TX3DLexer.CheckTokenIsKeyword(const Keyword: TX3DKeyword);
begin
  if not ( (Token = vtKeyword) and (TokenKeyword = Keyword) ) then
    raise EX3DParserError.Create(Self,
      Format('Expected keyword "%s", got %s', [X3DKeywordsName[Keyword],
        DescribeToken]));
end;

function TX3DLexer.TokenX3DName: String;
begin
  Result := DecodeX3DName(TokenName);
end;

{ Exceptions ----------------------------------------------------------------- }

constructor EX3DClassicReadError.Create(Lexer: TX3DLexer; const s: string);
begin
  inherited Create(Format('Error at line %d column %d: ',
    [Lexer.Stream.Line, Lexer.Stream.Column]) + S);
end;

{ global funcs  ------------------------------------------------------------------ }

function StringToX3DClassic(const s: string;
  const SurroundWithQuotes: boolean): string;
const
  Patterns: array [0..1] of string = ('\', '"');
  PatValues: array [0..1] of string = ('\\', '\"');
begin
  { use soMatchCase for speed }
  if SurroundWithQuotes then
    Result := '"' + SReplacePatterns(s, Patterns, PatValues, false) + '"' else
    Result :=       SReplacePatterns(s, Patterns, PatValues, false);
end;

function StringToX3DXml(const s: string): string;
const
  Patterns: array [0..6] of string = ('&', '"', '''', '<', '>', #10, #13);
  PatValues: array [0..6] of string = ('&amp;', '&quot;', '&apos;', '&lt;', '&gt;', '&#xA;', '&#xD;');
begin
  { use soMatchCase for speed }
  Result := '"' + SReplacePatterns(s, Patterns, PatValues, false) + '"';
end;

function StringToX3DXmlMulti(const s: string): string;
const
  Patterns: array [0..7] of string = ('&', '"', '''', '<', '>', #10, #13, '\');
  PatValues: array [0..7] of string = ('&amp;', '\&quot;', '&apos;', '&lt;', '&gt;', '&#xA;', '&#xD;', '\\');
begin
  { use soMatchCase for speed }
  Result := '"' + SReplacePatterns(s, Patterns, PatValues, false) + '"';
end;

end.
