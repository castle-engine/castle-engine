{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ TVRMLLexer class and helpers. }
unit VRMLLexer;

{ Every newly read token will be reported with LogWrite.
  Useful only for debugging this unit. }
{ $define LOG_VRML_TOKENS}

{$I kambiconf.inc}

interface

uses SysUtils, Classes, KambiUtils, KambiStringUtils, KambiClassUtils,
  Math, VRMLErrors {$ifdef LOG_VRML_TOKENS} ,LogFile {$endif};

type
  { Valid keywords for any VRML version. }
  TVRMLKeyword = (vkDEF, vkEXTERNPROTO, vkFALSE, vkIS, vkNULL, vkPROTO, vkROUTE,
    vkTO, vkTRUE, vkUSE, vkEventIn, vkEventOut, vkExposedField, vkField);
  TVRMLKeywords = set of TVRMLKeyword;
const
  VRML10Keywords = [vkDEF, vkUSE, vkFALSE, vkTRUE];

type
  { VRML lexer token }
  TVRMLToken = (
    vtKeyword,
    vtName,

    { Symbols for all VRML versions }
    vtOpenCurlyBracket, vtCloseCurlyBracket,
    vtOpenSqBracket, vtCloseSqBracket,

    { Symbols below are only for VRML <= 1.0.
      In VRML 2.0, they are no longer valid symbols
      (comma is even considered a whitespace).
      They will never be returned by lexer when reading VRML >= 2.0 files. }
    vtOpenBracket, vtCloseBracket, vtBar, vtComma,

    { Symbols below are only for VRML >= 2.0.
      They will never be returned by lexer when reading VRML < 2.0 files.  }
    vtPeriod,

    vtFloat, vtInteger, vtString,

    { vtEnd means that we're standing at the end of stream, no more tokens.
      Subsequent reads NextToken from stream will always result in
      vtEnd (they will not raise an error). }
    vtEnd);
  TVRMLTokens = set of TVRMLToken;

const
  TokenNumbers : TVRMLTokens = [vtFloat, vtInteger];

type
  { VRML unified lexer.

    The lexer always "looks" (i.e. contains in Token and TokenXxx fields)
    at the next not yet interpreted token.

    Remember that VRML is case-sensitive, so TokenName and TokenString
    should be compared in case-sensitive manner. Also note that
    for VRML >= 2.0 these fields contain UTF-8 encoded string.

    Note that it can read only from @link(TPeekCharStream), not just
    from any TStream. You may have to wrap your stream in some
    @link(TPeekCharStream) descendant (see for example at
    @link(TVRMLLexerFileName) implementation,
    that creates TFileStream and then wraps it inside
    @link(TBufferedReadStream)). }
  TVRMLLexer = class
  private
    fVRMLVerMajor, fVRMLVerMinor: integer;
    fToken: TVRMLToken;
    fTokenKeyword: TVRMLKeyword;
    fTokenName: string;
    fTokenFloat: Float;
    fTokenInteger: Int64;
    fTokenString: string;

    VRMLWhitespaces, VRMLNoWhitespaces: TSetOfChars;
    VRMLNameChars, VRMLNameFirstChars: TSetOfChars;

    FStream: TPeekCharStream;

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
  public
    { to po prostu strumien ktory dostalismy jako parametr konstruktora.
      Nie mozesz na nim operowac kiedy juz zainicjowales lexera !
      Ale mozesz np. sprawdzic jego Position aby wiedziec gdzie mniej
      wiecej bylismy w strumieniu gdy wystapil blad lexera. }
    property Stream: TPeekCharStream read FStream;

    { These indicate VRML version, as recorded in VRML file header.

      The only versions allowed by any VRML specifications
      are 1.0 and 2.0. Moreover we handle Inventor 1.0 ascii,
      then we set VRMLVerMajor and VRMLVerMinor both to 0
      (as historically Inventor is a predecessor to VRML 1.0).

      @groupBegin }
    property VRMLVerMajor: integer read fVRMLVerMajor;
    property VRMLVerMinor: integer read fVRMLVerMinor;
    { @groupEnd }

    { Token na jakim aktualnie stoimy. Odpowiednie pola TokenKeyword,
      TokenName, TokenFloat i TokenInteger maja defined wartosci tylko
      jezeli typ tokenu jest odpowiedni. }
    property Token: TVRMLToken read fToken;

    { When Token = vtKeyword, TokenKeyword points to appropriate keyword.
      Jezeli VRMLVersion = 1.0 to na pewno TokenKeyword in VRML10Keywords.
      Innymi slowy, gdy czytamy VRML 1.0 np. string "PROTO" zostanie potraktowany
      jako token Name, nie keyword. I tak jest dobrze. }
    property TokenKeyword: TVRMLKeyword read fTokenKeyword;

    { When Token = vtName, TokenName contains appropriate VRML name.

      Name syntax as in specification on page 24 (really 32 in pdf) of
      vrml97specification.pdf. It can be a user name for something (for a node,
      for example) but it can also be a name of a node type or a node field
      or an enumerated field constant ... it can be @italic(anything)
      except keyword.

      Note that this is supposed to contain UTF-8 encoded string for VRML 2.0. }
    property TokenName: string read fTokenName;

    { When Token = vtFloat or vtInteger, TokenFloat contains a value of
      this token.

      VRML float token corresponds to Pascal Float type,
      in VRML it's expressed in the followin form:
      @preformatted(
        [("-"|"+")]
        (digit+ [ "." digit+ ] | "." digit+)
        [ "e"|"E" [("-"|"+")] digit+ ]
      )

      For vtInteger you have the same thing in TokenInteger,
      TokenFloat is also initialized to the same value for your comfort
      (every integer value is also a float, after all). }
    property TokenFloat: Float read fTokenFloat;

    { When Token = vtInteger, TokenInteger contains appropriate value.

      VRML integer token corresponds to Pascal Int64 type,
      in VRML it's expressed in the followin form:
      @preformatted(
        (form : [("-"|"+")] ("0x" digit_hex+ | [1-9]digit_decimal* | 0 digit_octal+) )
      ) }
    property TokenInteger: Int64 read fTokenInteger;

    property TokenString: string read fTokenString;

    { NextToken reads next token from stream, initializing appropriately
      all Token* properties. For comfort, this returs the Token value. }
    function NextToken: TVRMLToken;

    { uzywaj gdy wiesz ze nastepny token MUSI byc vtName i zeby w zwiazku z
      tym lekser odczytal nastepny token jako vtName. Pamietaj ze moze
      to powodowac odczytanie jako vtName czegos co nie jest poprawna
      nazwa node'a w VRML'u ! Ale moze byc uzyteczne jesli jakis inny
      program zapisywal plik VRML'a nie patrzac na to czy tworzy prawidlowe
      czy nie nazwy VRML'a (np. mgf2inv potrafi zapisac nazwe "0" (tak jest,
      zero, co oczywiscie zostanie odczytane jako vtInteger), gdzie indziej
      znalazlem przykladowe VRMLe z nazwa node'a "Crab!" (tak, z "!" i
      cudzyslowem)).
      
      Uzywajac tej procedury bedziesz w stanie odczytac takie VRMLe ze zlymi
      nazwami node'ow (pamietajac ze zawsze przed nazwa node'a jest USE
      lub DEF wiec wiadomo kiedy nalezy sie spodziewac vtName i wtedy wlasnie
      trzeba uzyc NextTokenForceVTName.)
      
      Wyjatek EParserError (bo to przeciez blad parsowania, nie leksera) jesli
      mimo wszystko nie uda sie odczytac tokenu vtName. }
    procedure NextTokenForceVTName;

    { Similiar to NextTokenForceVTName: use this like a shortcut for
        NextToken;
        CheckTokenIs(vtString);
      but it is NOT equivalent to such instructions. This is because
      VRML 1.0 allowed rather strange thing: string may be not enclosed
      in double quotes if it does not contain a space. This "feature"
      is not present in VRML 97, but, unfortunately, I'm trying to handle
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
      TVRMLNode.Parse.) }
    procedure NextTokenForceVTString;

    { Returns if Token is vtKeyword and TokenKeyword is given Keyword. }
    function TokenIsKeyword(const Keyword: TVRMLKeyword): boolean; overload;
    function TokenIsKeyword(const Keywords: TVRMLKeywords): boolean; overload;

    { skonstruuj tekstowy opis tokenu ktory nadaje sie do pokazania
      userowi. }
    function DescribeToken: string;

    { Check is token = Tok, if not -> parser error "expected token >>tok<<".
      You can provide your own description for Tok or default desciption
      for token will be used. }
    procedure CheckTokenIs(Tok: TVRMLToken); overload;
    procedure CheckTokenIs(Tok: TVRMLToken; const TokDescription: string); overload;
    procedure CheckTokenIs(const Toks: TVRMLTokens; const ToksDescription: string); overload;
    procedure CheckTokenIsKeyword(const Keyword: TVRMLKeyword);

    { po wykonaniu konstruktora VRMLVerMajor i Minor i pierwszy Token
      juz sa odczytane }
    constructor Create(AStream: TPeekCharStream; const AWWWBasePath: string);
    destructor Destroy; override;

    { See TVRMLNode.WWWBasePath for a description of this field.

      This field is not used anywhere in the Lexer but it MUST be defined
      to something sensible. It is just some information
      "carried with" the lexer. We will use it when we parse nodes. }
    WWWBasePath: string;

    { This is used when parsing to keep current namespace for DEF/USE.

      NodeNameBinding jest lista bez duplikatow okreslajaca wszystkie dotychczasowe
      nazwy node'ow razem z ich instancjami. Jezeli kilka instancji mialo takie
      samo NodeName to na liscie znajduje sie ostatni z nich (ostatni w sensie
      pozycji w pliku, czy raczej w strumieniu tokenow Lexera). Tym samym
      jest chyba jasne do czego uzywamy NodeNameBinding : do realizacji
      konstrukcji "USE <nodename>". Procedura ParseNode nie moze modyfikowac
      tej listy, to zadania ma wykonywac TVRMLNode.Parse.

      Notka do mechanizmu DEF/USE : gdy uzywamy DEF nie mozemy odwolac
      sie do nazwy node'a ktory aktualnie parsujemy (osiagamy to
      po prostu dodajac nazwe node do NodeNameBinding dopiero PO
      sparsowaniu node'a). W ten sposob zapewniamy sobie ze graf VRML'a
      nie moze zawierac cykli i jestesmy szczesliwi.

      TODO: above causes failure to read jsTouch VRML file in openvrml/models/
      tests. Looks like cycles are unavoidable after all. }
    NodeNameBinding: TStringList;

    { This is used when parsing to keep current namespace of prototypes. }
    ProtoNameBinding: TStringList;
  end;

  TVRMLLexerFileName = class(TVRMLLexer)
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

  EVRMLLexerError = class(EVRMLError)
    { Lexer object must be valid for this call, it is not needed when
      constructor call finished (i.e. Lexer reference don't need to be
      valid for the lifetime of the exception; it must be valid only for
      constructing the exception, later it can be Freed etc.) }
    constructor Create(Lexer: TVRMLLexer; const s: string);
  end;

  EVRMLParserError = class(EVRMLError)
    { Lexer object must be valid only for this call; look at
      EVRMLLexerError.Create for more detailed comment. }
    constructor Create(Lexer: TVRMLLexer; const s: string);
  end;

const
  VRMLKeywords: array[TVRMLKeyword]of string = (
    'DEF', 'EXTERNPROTO', 'FALSE', 'IS', 'NULL', 'PROTO', 'ROUTE',
    'TO', 'TRUE', 'USE', 'eventIn', 'eventOut', 'exposedField', 'field');

{ otoczy s cudzyslowami i zmieni wnetrze s tak zeby wynik mogl byc
  zapisany jako token vtString o wartosci s. Mowiac wprost,
  zamieni wszystkie " na \" i wszystkie \ na \\. }
function StringToVRMLStringToken(const s: string): string;

implementation

const
  VRMLFirstLineTerm = [#10, #13];

  { utf8 specific constants below }
  VRMLLineTerm = [#10, #13];

  VRMLTokenNames: array[TVRMLToken]of string = (
    'keyword', 'name',
    '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."',
    'float', 'integer', 'string', 'end of file');

{$I macarraypos.inc}
{$define ARRAY_POS_FUNCTION_NAME := ArrayPosVRMLKeywords}
{$define ARRAY_POS_ARRAY_NAME := VRMLKeywords}
{$define ARRAY_POS_INDEX_TYPE := TVRMLKeyword}
IMPLEMENT_ARRAY_POS

{ TVRMLLexer ------------------------------------------------------------- }

constructor TVRMLLexer.Create(AStream: TPeekCharStream;
  const AWWWBasePath: string);
const
  VRML1HeaderStart = '#VRML V1.0 ';
  VRML2HeaderStart = '#VRML V2.0 ';
  { This is not an official VRML header, but it's used by VRML models on
    [http://www.itl.nist.gov/div897/ctg/vrml/chaco/chaco.html] }
  VRML2DraftHeaderStart = '#VRML Draft #2 V2.0 ';
  VRML1HeaderAscii = 'ascii';
  InventorHeaderStart = '#Inventor ';

  procedure VRML2HeaderReadRest(const Line: string);
  var
    Encoding: string;
  begin
    fVRMLVerMajor := 2;
    fVRMLVerMinor := 0;

    Encoding := NextTokenOnce(Line);
    if Encoding <> 'utf8' then
      raise EVRMLLexerError.Create(Self,
        'VRML 2.0 signature : only utf8 encoding supported for now');
  end;

var
  Line: string;
begin
  inherited Create;
  FStream := AStream;
  WWWBasePath := AWWWBasePath;

  NodeNameBinding := TStringListCaseSens.Create;
  ProtoNameBinding := TStringListCaseSens.Create;

  { Read first line = signature. }
  Line := Stream.ReadUpto(VRMLLineTerm);
  if Stream.ReadChar = -1 then
    raise EVRMLLexerError.Create(Self,
      'Unexpected end of file on the 1st line');

  if IsPrefix(VRML1HeaderStart, Line) then
  begin
    Delete(Line, 1, Length(VRML1HeaderStart));
    fVRMLVerMajor := 1;
    fVRMLVerMinor := 0;

    { then must be 'ascii';
      VRML 1.0 'ascii' may be followed immediately by some black char. }
    if not IsPrefix(VRML1HeaderAscii, Line) then
      raise EVRMLLexerError.Create(Self, 'Wrong VRML 1.0 signature : '+
        'VRML 1.0 files must have "ascii" encoding');
  end else
  if IsPrefix(VRML2HeaderStart, Line) then
  begin
    Delete(Line, 1, Length(VRML2HeaderStart));
    VRML2HeaderReadRest(Line);
  end else
  if IsPrefix(VRML2DraftHeaderStart, Line) then
  begin
    Delete(Line, 1, Length(VRML2DraftHeaderStart));
    VRML2HeaderReadRest(Line);
  end else
  if IsPrefix(InventorHeaderStart, Line) then
  begin
    Delete(Line, 1, Length(InventorHeaderStart));
    fVRMLVerMajor := 0;
    fVRMLVerMinor := 0;

    if not IsPrefix('V1.0 ascii', Line) then
      raise EVRMLLexerError.Create(Self,
        'Inventor signature recognized, but only '+
        'Inventor 1.0 ascii files are supported. Sor'+'ry.');
  end else
    raise EVRMLLexerError.Create(Self,
      'VRML signature error : unrecognized signature');

  { calculate VRMLWhitespaces, VRMLNoWhitespaces }
  VRMLWhitespaces := [' ',#9, #10, #13];
  if VRMLVerMajor >= 2 then
    Include(VRMLWhitespaces, ',');
  VRMLNoWhitespaces := AllChars - VRMLWhitespaces;

  { calculate VRMLNameChars, VRMLNameFirstChars }
  { These are defined according to vrml97specification on page 24. }
  VRMLNameChars := AllChars -
    [#0..#$1f, ' ', '''', '"', '#', ',', '.', '[', ']', '\', '{', '}'];
  if VRMLVerMajor <= 1 then
    VRMLNameChars := VRMLNameChars - ['(', ')', '|'];
  VRMLNameFirstChars := VRMLNameChars - ['0'..'9', '-','+'];

  {read first token}
  NextToken;
end;

destructor TVRMLLexer.Destroy;
begin
  FreeAndNil(NodeNameBinding);
  FreeAndNil(ProtoNameBinding);
  inherited;
end;

procedure TVRMLLexer.StreamReadUptoFirstBlack(out FirstBlack: Integer);
begin
 repeat
  Stream.ReadUpto(VRMLNoWhitespaces);
  FirstBlack := Stream.ReadChar;

  { ignore comments }
  if FirstBlack = Ord('#') then
   Stream.ReadUpto(VRMLLineTerm) else
   break;
 until false;
end;

procedure TVRMLLexer.ReadString;
{ String in encoded using the form
  "char*" where char is either not " or \" sequence. }
var
  endingChar: Integer;
  NextChar: Integer;
begin
 fToken := vtString;
 fTokenString := '';
 repeat
  fTokenString += Stream.ReadUpto(['\','"']);
  endingChar := Stream.ReadChar;

  if endingChar = -1 then
   raise EVRMLLexerError.Create(Self,
     'Unexpected end of file in the middle of string token');

  { gdy endingChar = '\' to ignorujemy palke ktora wlasnie przeczytalismy
    i nastepny znak ze strumienia nie jest interpretowany - odczytujemy
    go przez Stream.ReadChar i zawsze dopisujemy do fTokenString. W ten sposob
    \\ zostanie zrozumiane jako \, \" zostanie zrozumiane jako " (i nie bedzie
    oznaczac konca stringu), wszystko inne \? bedzie oznaczac ?. }
  if endingChar = Ord('\') then
  begin
    NextChar := Stream.ReadChar;
    if NextChar = -1 then
      raise EVRMLLexerError.Create(Self,
        'Unexpected end of file in the middle of string token');
    fTokenString += Chr(NextChar);
  end;

 until endingChar = Ord('"');
end;

function TVRMLLexer.NextToken: TVRMLToken;

  procedure ReadNameOrKeyword(FirstLetter: char);
  {read name token. First letter has been already read.}
  var foundKeyword: TVRMLKeyword;
  begin
   fTokenName := FirstLetter +Stream.ReadUpto(AllChars - VRMLNameChars);

   {teraz zobacz czy fTokenName nie jest przypadkiem keywordem.
    Jezeli znalazl keyword VRML'a 2.0 w VRML 1.0 - to "nie liczy sie" }
   if ArrayPosVRMLKeywords(fTokenName, foundKeyword) and
      ( (VRMLVerMajor > 1) or (foundKeyword in VRML10Keywords) ) then
   begin
    fToken := vtKeyword;
    fTokenKeyword := foundKeyword;
   end else
    fToken := vtName;
  end;

  procedure ReadFloatOrInteger(FirstChar: char);
  const NoDigits = AllChars - ['0'..'9'];
        NoHexDigits = AllChars - ['0'..'9', 'a'..'f', 'A'..'F'];
  { TODO: octal notation not implemented (i simply forgot about it) }

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
       will be evaluated. See console.testy/test_string_plus) }
     CharAfterEInt := Stream.ReadChar;
     if CharAfterEInt = -1 then
       raise EVRMLLexerError.Create(Self,
         'Unexpected end of file in the middle of real constant');
     CharAfterE := Chr(CharAfterEInt);
     RestOfToken := Stream.ReadUpto(NoDigits);
     fTokenFloat := StrToFloat(AlreadyRead +'e' +CharAfterE +RestOfToken);
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
      fTokenFloat := StrToFloat(s);
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
      fToken := vtInteger;
      fTokenInteger := StrToInt64(Dig1);
     end;
    end;

    if fToken = vtInteger then fTokenFloat := TokenInteger;
   except
    on E: EConvertError do raise EVRMLLexerError.Create(Self, E.Message);
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
      if FirstBlackChr in VRMLNameFirstChars then
       ReadNameOrKeyword(FirstBlackChr) else
       raise EVRMLLexerError.Create(Self, Format('Illegal character in stream : %s (#%d)',
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

    if VRMLVerMajor <= 1 then
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
        RecognizeCommonTokens(FirstBlackChr);
    end;
  end;

  {$ifdef LOG_VRML_TOKENS} LogWrite('VRML token: ' +DescribeToken); {$endif}

  result := Token;
end;

procedure TVRMLLexer.NextTokenForceVTName;
var FirstBlack: integer;
begin
 StreamReadUptoFirstBlack(FirstBlack);

 if FirstBlack = -1 then
  fToken := vtEnd else
 begin
   (* Stop tokens include { and }, otherwise we risk that because of this
      hack (NextTokenForceVTName is really only a hack to try to read
      even incorrect VRML files) we would fail to read correctly valid
      VRML files. *)
  fTokenName := Chr(FirstBlack) +Stream.ReadUpto(VRMLWhitespaces + ['{', '}']);
  fToken := vtName;
 end;

 {$ifdef LOG_VRML_TOKENS} LogWrite('VRML token: ' +DescribeToken); {$endif}

 CheckTokenIs(vtName);
end;

procedure TVRMLLexer.NextTokenForceVTString;
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

 {$ifdef LOG_VRML_TOKENS} LogWrite('VRML token: ' +DescribeToken); {$endif}

 CheckTokenIs(vtString);
end;

function TVRMLLexer.TokenIsKeyword(const Keyword: TVRMLKeyword): boolean;
begin
  Result := (Token = vtKeyword) and (TokenKeyword = Keyword);
end;

function TVRMLLexer.TokenIsKeyword(const Keywords: TVRMLKeywords): boolean;
begin
  Result := (Token = vtKeyword) and (TokenKeyword in Keywords);
end;

function TVRMLLexer.DescribeToken: string;
begin
 result := VRMLTokenNames[Token];
 case Token of
  vtKeyword: result := result +' "' +VRMLKeywords[TokenKeyword]+'"';
  vtName: result := '"' +TokenName+'"';
  vtFloat: result := result +' ' +FloatToStr(TokenFloat);
  vtInteger: result := result +' ' +IntToStr(TokenInteger);
  vtString: result := result+' "'+TokenString+'"';
 end;
end;

procedure TVRMLLexer.CheckTokenIs(Tok: TVRMLToken);
begin
 CheckTokenIs(Tok, VRMLTokenNames[Tok]);
end;

procedure TVRMLLexer.CheckTokenIs(Tok: TVRMLToken; const TokDescription: string);
begin
 if Token <> Tok then
  raise EVRMLParserError.Create(Self, 'Expected '+TokDescription
    +', got '+DescribeToken);
end;

procedure TVRMLLexer.CheckTokenIs(const Toks: TVRMLTokens; const ToksDescription: string);
begin
 if not (Token in Toks) then
  raise EVRMLParserError.Create(Self, 'Expected '+ToksDescription
    +', got '+DescribeToken);
end;

procedure TVRMLLexer.CheckTokenIsKeyword(const Keyword: TVRMLKeyword);
begin
  if not ( (Token = vtKeyword) and (TokenKeyword = Keyword) ) then
    raise EVRMLParserError.Create(Self,
      Format('Expected keyword "%s", got %s', [VRMLKeywords[TokenKeyword],
        DescribeToken]));
end;

{ TVRMLLexerFileName --------------------------------------------------------- }

constructor TVRMLLexerFileName.Create(const FileName: string);
var FileStream: TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 inherited Create(
   TBufferedReadStream.Create(FileStream, true),
   ExtractFilePath(ExpandFilename(FileName)));
end;

destructor TVRMLLexerFileName.Destroy;
begin
 inherited;
 Stream.Free;
end;

{ EVRMLLexer/ParserError ------------------------------------------------------------ }

constructor EVRMLLexerError.Create(Lexer: TVRMLLexer; const s: string);
begin
 inherited CreateFmt('VRML lexical error at position %d : %s',
   [Lexer.Stream.Position, s]);
end;

constructor EVRMLParserError.Create(Lexer: TVRMLLexer; const s: string);
begin
 inherited CreateFmt('VRML parse error at position %d : %s',
   [Lexer.Stream.Position, s]);
end;

{ global funcs  ------------------------------------------------------------------ }

function StringToVRMLStringToken(const s: string): string;
const
  Patterns: array[0..1]of string = ('\', '"');
  PatValues: array[0..1]of string = ('\\', '\"');
begin
 {uzyj soMatchCase tylko po to zeby bylo szybciej}
 result := '"' + SReplacePatterns(s, Patterns, PatValues, [soMatchCase]) + '"';
end;

end.
