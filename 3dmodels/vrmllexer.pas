{
  Copyright 2002-2005 Michalis Kamburelis.

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

(*
  @abstract(VRML 1.0 and 2.0 unified lexer by Kambi.)

  Also we're trying to read
  Inventor 1.0 ascii format (we treat Inventor 1.0 ascii format as
  VRML 1.0. VerMajor.VerMinor are "0.0" when reading Inventor files.).

  Lexer tokens :
  @definitionList(
    @itemLabel keyword
    @item(
      For VRML 97 :
      DEF, EXTERNPROTO, FALSE, IS, NULL, PROTO, ROUTE, TO, TRUE,
      USE, eventIn, eventOut, exposedField, field.

      For VRML 1.0 only small subset of these (DEF, USE, FALSE, TRUE)

      Additional keyword INCLUDE (intended to exist in VRML 1.0 and 97)
      is my (Kambi) extension.)

    @itemLabel name
    @item(
      Syntax as in specification on page 24 (really 32 in pdf) of
      vrml97specification.pdf. It can be a user name for something (for a node,
      for example) but it can also be a name of a node type or a node field
      or an enumerated field constant ... it can be @italic(anything)
      except keyword.)

    @itemLabel various symbols
    @item(
      @code({ } [ ] ( ) |) @br
      (last four things shouldn't be tokens in VRML 2.0;
      TODO: comma should even be whitespace))

    @itemLabel float
    @item(Pascal Float type, expressed in the followin form:
@preformatted(
  [("-"|"+")]
  (digit+ [ "." digit+ ] | "." digit+)
  [ "e"|"E" [("-"|"+")] digit+ ]
))
    @itemLabel integer
    @item(Pascal Int64 type, expressed in the followin form:
@preformatted(
  (form : [("-"|"+")] ("0x" digit_hex+ | [1-9]digit_decimal* | 0 digit_octal+) )
))

    @itemLabel string
    @item(string (form : "char*" where char is either not " or \" sequence))
  )

  Notki:
  @unorderedList(
    @item VRML jest case-sensitive
    @item(TODO: to jest dopiero planowane:
      Obsluga keyword INCLUDE jest zaimplementowana juz w tym lekserze.
      Znaczy to tyle ze obiekt TVRMLLexer nigdy nie bedzie stal na takim tokenie,
      tzn. nigdy nie bedzie (Token = vtKeyword and TokenKeyword = vkINCLUDE).
      Z punktu widzenia parsera wywolujacego NextToken, caly plik VRMLa
      bedzie podany juz w postaci "resolved", tzn. wszystkie INCLUDE zostana
      juz wstawione na miejsce : tam gdzie w pliku beda dwa tokeny
      INCLUDE "plik.wrl" tam parser bedzie dostawal po kolei tokeny jakie
      beda odczytywane z pliku "plik.wrl" (czyli lekser utworzy sobie na chwile
      pomocniczy lekser do czytania pliku "plik.wrl" i bedzie zwracal tokeny
      z tego pomocniczego leksera; kiedy pomocniczy lekser zwroci vtEnd
      pomocniczy lekser zostanie zakonczony i bedziemy dalej czytac
      nastepne tokeny (za INCLUDE "plik.wrl") przy pomocy zasadniczego leksera).
    )
  )
*)

unit VRMLLexer;

{ Every newly read token will be reported with LogWrite.
  Useful only for debugging this unit. }
{ $define LOG_VRML_TOKENS}

{$I kambiconf.inc}

interface

uses SysUtils, Classes, KambiUtils, KambiClassUtils, Math
  {$ifdef LOG_VRML_TOKENS} ,LogFile {$endif};

{ specification of valid VRML 1.0 and 2.0 keywords }
type
  TVRMLKeyword = (vkDEF, vkEXTERNPROTO, vkFALSE, vkIS, vkNULL, vkPROTO, vkROUTE,
    vkTO, vkTRUE, vkUSE, vkEventIn, vkEventOut, vkExposedField, vkField,
    vkINCLUDE);
  TVRMLKeywords = set of TVRMLKeyword;
const
  VRML10Keywords = [vkDEF, vkUSE, vkFALSE, vkTRUE, vkINCLUDE];

{ VRML lexer token }
type
  TVRMLToken = (vtKeyword, vtName,
    (* special symbols : { } [ ] ( ) | , *)
    vtOpenWasBracket, vtCloseWasBracket,
    vtOpenSqBracket, vtCloseSqBracket, vtOpenBracket, vtCloseBracket,
    vtBar, vtComma,
    vtFloat, vtInteger, vtString,
    { vtEnd = we're standing at the end of stream, no more tokens.
      Subsequent reads NextToken from stream will always result in
      vtEnd (they will not raise an error). }
    vtEnd);
  TVRMLTokens = set of TVRMLToken;
const
  TokenNumbers : TVRMLTokens = [vtFloat, vtInteger];

type
  { VRML lexer. It always "looks" at the next not yet interpreted token.

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

    FStream: TPeekCharStream;

    { Reads chars from Stream until EOF or some non-white char will
      be approached. Omits VRML comments. Returns as FirstBlack
      -1 (if EOF) or Ord(of this non-white char). (This non-white
      char will be already read from Stream, so usually you MUST do
      something with returned here FirstBlack, you can't ignore him) }
    procedure StreamReadUptoFirstBlack(var FirstBlack: Integer);

    { Read string. Initial " has been already read. Reads everything
      up to (and including) " terminating the string.
      Sets fToken and fTokenString to appropriate values
      (i.e. fToken always to vtString, fTokenString to string contents). }
    procedure ReadString;
  public
    { to po prostu strumien ktory dostalismy jako parametr konstruktora.
      Nie mozesz na nim operowac kiedy juz zainicjowales lexera !
      Ale mozesz np. sprawdzic jego Position aby wiedziec gdzie mniej
      wiecej bylismy w strumieniu gdy wystapil blad lexera.
      (sorry - reconsider this after implementing INCLUDE keyword)}
    property Stream: TPeekCharStream read FStream;

    { jedyna wersje VRML'a dopuszczalne przez jakiekolwiek specyfikacje
      to 1.0 lub 2.0. Dodatkowo obslugujemy Inventor'a 1.0 jako wersje "0.0". }
    property VRMLVerMajor: integer read fVRMLVerMajor;
    property VRMLVerMinor: integer read fVRMLVerMinor;

    { Token na jakim aktualnie stoimy. Odpowiednie pola TokenKeyword,
      TokenName, TokenFloat i TokenInteger maja defined wartosci tylko
      jezeli typ tokenu jest odpowiedni. Wyjatkiem (ale pozytywnym,
      wiec nic sie nie stanie jesli o nim zapomnisz) jest vtInteger :
      jest wtedy nie tylko zdefiniowne pole TokenInteger ale tez TokenFloat
      (dla wygody; kazdy integer moze byc tez potraktowany jako liczba;
      mozesz sprawdzac czy token to liczba testujac Token in TokenNumbers) }
    property Token: TVRMLToken read fToken;
    { jezeli VRMLVersion = 1.0 to na pewno TokenKeyword in VRML10Keywords.
      Innymi slowy, gdy czytamy VRML 1.0 np. string "PROTO" zostanie potraktowany
      jako token Name, nie keyword. I tak jest dobrze.
      sorry - gdy tylko zaimplementujemy INCLUDE handling, bedzie mozna
      polegac na fakcie ze nigdy TokenKeyword = vkINCLUDE (no, tzn, gdy
      Token <> vtKeyword TokenKeyword jest ciagle undefined wiec wtedy wszystko
      jest mozliwe) }
    property TokenKeyword: TVRMLKeyword read fTokenKeyword;
    property TokenName: string read fTokenName;
    property TokenFloat: Float read fTokenFloat;
    property TokenInteger: Int64 read fTokenInteger;
    property TokenString: string read fTokenString;

    { NextToken pobiera z pliku nastepny token, inicjujac odpowiednio pola
      Token*. Potem zwraca wartosc pola Token (dla wygody). }
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

    { skonstruuj tekstowy opis tokenu ktory nadaje sie do pokazania
      userowi. }
    function DescribeToken: string;
    { Check is token = Tok, if not -> parser error "expected token >>tok<<".
      You can provide your own description for Tok or default desciption
      for token will be used. }
    procedure CheckTokenIs(Tok: TVRMLToken); overload;
    procedure CheckTokenIs(Tok: TVRMLToken; const TokDescription: string); overload;
    procedure CheckTokenIs(const Toks: TVRMLTokens; const ToksDescription: string); overload;

    { po wykonaniu konstruktora VRMLVerMajor i Minor i pierwszy Token
      juz sa odczytane }
    constructor Create(AStream: TPeekCharStream; const AWWWBasePath: string);
    destructor Destroy; override;

    (* this field is not used anywhere in the Lexer but it MUST be defined
      to something sensible. It is just some information
      "carried with" the lexer. We will use it when we parse nodes.
      Look at TVRMLNode node for a description of this field.
      sorry - when we implement INCLUDE handling here, what shall we do with
        this ? Probably nothing - text INCLUDEd will have same WWWBasePath
        as main file (this is sensible because INCLUDE is not limited to
        contain some nodes or anything like that - it must only contain
        some tokens, e.g. following situation is possible:
          file1.wrl contents:
            #VRML V1.0 ascii
            INCLUDE "file2.wrl" 0 color 1 0 0 }
          file2.wrl contents:
            PointLight { location 0 0
        and "file1.wrl" is considered a valid VRML. I.e. nodes, and even
        fields!, may be "broken" into multiple files using INCLUDE
        keyword.
        I should add this comments to kambi-vrml.php as soon as I implement
        INCLUDE keyword handling.
    *)
    WWWBasePath: string;
  end;

  TVRMLLexerFileName = class(TVRMLLexer)
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

  EVRMLError = class(Exception);
  EVRMLLexerError = class(EVRMLError)
    { Lexer object must be valid for this call, it is not needed when
      constructor call finished (i.e. Lexer reference don't need to be
      valid for the lifetime of the exception; it must be valid only for
      constructing the exception, later it can be Freed etc.)

      sorry - when implementing INCLUDE keyword, specify here what Lexer
      should be given: do we require here to get Lexer reading it's Stream
      (not posredniczacy ?)
    }
    constructor Create(Lexer: TVRMLLexer; const s: string);
  end;
  EVRMLParserError = class(EVRMLError)
    { Lexer object must be valid only for this call; look at
      EVRMLLexerError.Create for more detailed comment.

      sorry - same as EVRMLLexerError.Create }
    constructor Create(Lexer: TVRMLLexer; const s: string);
  end;

const
  VRMLKeywords: array[TVRMLKeyword]of string = (
    'DEF', 'EXTERNPROTO', 'FALSE', 'IS', 'NULL', 'PROTO', 'ROUTE',
    'TO', 'TRUE', 'USE', 'eventIn', 'eventOut', 'exposedField', 'field',
    'INCLUDE');

const
  { to jest pelna sygnatura VRML'a 1.0. Niniejszy lekser tego nie uzywa
    bo tak naprawde mozna to sygnature zapisac tez nieco inaczej.
    Ale moze byc przydatne dla programow zapisujacych VRML'e }
  VRML10SignatureLine = '#VRML V1.0 ascii';

{ otoczy s cudzyslowami i zmieni wnetrze s tak zeby wynik mogl byc
  zapisany jako token vtString o wartosci s. Mowiac wprost,
  zamieni wszystkie " na \" i wszystkie \ na \\. }
function StringToVRMLStringToken(const s: string): string;

implementation

const
  VRMLFirstLineTerm = [#10, #13];

  { utf8 specific constants below }
  VRMLWhitespaces = [
    {sorry - in VRML 2.0 we should have here a comma ',' too}
    ' ',#9, #10, #13];
  VRMLLineTerm = [#10, #13];
  VRMLNoWhitespaces = AllChars - VRMLWhitespaces;
  { NameChars and NameFirstChars defined according to vrml97specification
    on page 24. }
  VRMLNameChars = AllChars - [#0..#$1f, ' ', '''', '"', '#', ',', '.', '[', ']',
    '\', '{', '}',
    '(', ')', '|' {sorry - last three should be here only in VRML 1.0}];
  VRMLNameFirstChars = VRMLNameChars - ['0'..'9', '-','+'];

  VRMLTokenNames: array[TVRMLToken]of string = (
    'keyword', 'name', '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","',
    'float', 'integer', 'string', 'end of file');

{$I macarraypos.inc}
{$define ARRAY_POS_FUNCTION_NAME := ArrayPosVRMLKeywords}
{$define ARRAY_POS_ARRAY_NAME := VRMLKeywords}
{$define ARRAY_POS_INDEX_TYPE := TVRMLKeyword}
IMPLEMENT_ARRAY_POS

{ TVRMLLexer ------------------------------------------------------------- }

procedure TVRMLLexer.StreamReadUptoFirstBlack(var FirstBlack: Integer);
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
var endingChar: Integer;
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
    go przez StreamReadChar i zawsze dopisujemy do fTokenString. W ten sposob
    \\ zostanie zrozumiane jako \, \" zostanie zrozumiane jako " (i nie bedzie
    oznaczac konca stringu), wszystko inne \? bedzie oznaczac ?. }
  if endingChar = Ord('\') then
   fTokenString += StreamReadChar(Stream);

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
  {sorry - octal notation not implemented (i simply forgot about it)}

    procedure ReadAfterE(const AlreadyRead: string);
    var CharAfterE: char;
        RestOfToken: string;
    begin
     fToken := vtFloat;
     { Za "e" musi byc min 1 znak, to moze byc cyfra lub - lub +.
       Odczytujemy go do CharAfterE.
       Potem sa juz tylko cyfry, odczytujemy je do RestOfToken.
       (note: you can't write "StreamReadChar(Stream) + Stream.ReadUpto(NoDigits)"
       because it is undefined in what order S1+S2
       will be evaluated. See console.testy/test_string_plus) }
     CharAfterE := StreamReadChar(Stream);
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

var FirstBlack: integer;
    FirstBlackChr: char;
begin
 StreamReadUptoFirstBlack(FirstBlack);

 if FirstBlack = -1 then
  fToken := vtEnd else
 begin
  FirstBlackChr := Chr(FirstBlack);
  case FirstBlackChr of
   '{':fToken := vtOpenWasBracket;
   '}':fToken := vtCloseWasBracket;
   '[':fToken := vtOpenSqBracket;
   ']':fToken := vtCloseSqBracket;
   '(':fToken := vtOpenBracket;
   ')':fToken := vtCloseBracket;
   '|':fToken := vtBar;
   ',':fToken := vtComma;
   '-','+','.','0'..'9':ReadFloatOrInteger(FirstBlackChr);
   '"':ReadString;
   else
    if FirstBlackChr in VRMLNameFirstChars then
     ReadNameOrKeyword(FirstBlackChr) else
     raise EVRMLLexerError.Create(Self, Format('Illegal character in stream : %s (#%d)',
       [FirstBlackChr, Ord(FirstBlackChr)]));
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
  fTokenName := Chr(FirstBlack) +Stream.ReadUpto(VRMLWhitespaces);
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

constructor TVRMLLexer.Create(AStream: TPeekCharStream;
  const AWWWBasePath: string);

  procedure IgnoreRestOfSignatureLine;
  begin
   { ignore rest of first line }
   Stream.ReadUpto(VRMLLineTerm);
   if Stream.ReadChar = -1 then
    raise EVRMLLexerError.Create(Self,
      'Unexpected end of file on the 1st line');
  end;

var Sign: string;
begin
 inherited Create;
 FStream := AStream;
 WWWBasePath := AWWWBasePath;

 {read first line = signature.
  Must begin with '#VRML V?.0 ' (11 chars)}
 SetLength(Sign, 11);
 Stream.ReadBuffer(Sign[1], Length(Sign));
 if Sign = '#VRML V1.0 ' then
 begin
  fVRMLVerMajor := 1;
  fVRMLVerMinor := 0;
  {then must be 'ascii'; VRML 1.0 'ascii' may be followed immediately by some black char. }
  SetLength(Sign, 5);
  Stream.ReadBuffer(Sign[1], Length(Sign));
  if Sign <> 'ascii' then
   raise EVRMLLexerError.Create(Self, 'Wrong VRML 1.0 signature : '+
     'VRML 1.0 files must have "ascii" encoding');

  IgnoreRestOfSignatureLine;
 end else
 if Sign = '#VRML V2.0 ' then
 begin
  fVRMLVerMajor := 2;
  fVRMLVerMinor := 0;
  Sign := Stream.ReadUpto(Whitespaces);
  if Sign <> 'utf8' then
   raise EVRMLLexerError.Create(Self, 'VRML 2.0 signature : only utf8 encoding '+
     'supported for now');

  IgnoreRestOfSignatureLine;
 end else
 if Sign = '#Inventor V' then
 begin
  fVRMLVerMajor := 0;
  fVRMLVerMinor := 0;

  SetLength(Sign, 9);
  Stream.ReadBuffer(Sign[1], Length(Sign));
  if Sign <> '1.0 ascii' then
   raise EVRMLLexerError.Create(Self, 'Inventor signature recognized, but only '+
     'Inventor 1.0 ascii files are supported. Sor'+'ry.');

  IgnoreRestOfSignatureLine;
 end else
  raise EVRMLLexerError.Create(Self,
    'VRML signature error : unrecognized signature');

 {read first token}
 NextToken;
end;

destructor TVRMLLexer.Destroy;
begin
 inherited;
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
