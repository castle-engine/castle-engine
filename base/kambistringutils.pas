{
  Copyright 2000-2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Operations on strings.

  Also some operations on chars and PChars.
  And various convertions strings<->numbers.

  General comments:
  For all procedures that have parameter like IgnoreCase:
  @unorderedList(
    @item(
      If such parameter has some default value, this default value should be
      @definitionList(
        @itemLabel @true
        @item for procedures that only read processed string
        @itemLabel @false
        @item(for procedures that can modify processed string (for safety,
          so that accidental modification should be harder))
      ))

    @item(
      If I don't write in docs for this procedure whether this
      procedure takes current locale into account (as current locale
      can change the meaning of "ignoring case"), then it means it
      @italic(does) take current locale into account.)
  )
}

unit KambiStringUtils;

interface

uses Variants, SysUtils, KambiUtils;

type
  { }
  TSearchOptions = set of (soMatchCase, soWholeWord, soBackwards);
  { A set of chars. }
  TSetOfChars = SysUtils.TSysCharSet;

const
  AllChars = [Low(Char) .. High(Char)];
  DefaultWordBorders = AllChars - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  WhiteSpaces = [' ', #9, #10, #13];

function RandomString: string;

{ StringReplaceAllTo1st zamienia w stringu s wszystkie wystapienia
  subs_orig na subs_repl. Domyslnie IgnoreCase = true.
    StringReplaceAllTo1st(s, from, to)
  jest rownowazne
    s := StringReplace(s, from, to, [rfReplaceAll, rfIgnoreCase])
  Przy IgnoreCase = false nie ma flagi rfIgnoreCase. Tym samym
  StringReplaceAllTo1st opakowuje bardzo czeste zastosowanie
  StringReplace. }
procedure StringReplaceAllTo1st(var s: string; const fromPat, toPat: string;
  IgnoreCase: boolean = true); overload;

{ BreakLine : bierze s i wstawia w nim znaki nl tak zeby kazda linia
  w stringu miala maksymalnie MaxCol znakow. String s przy tym moze
  byc juz podzielony przy uzyciu dowolnych znakow konca linii : #13, #10,
  #13#10 lub #10#13. Zostana one rozpoznane i uwzglednione jako juz dzielace
  linie ograniczniki.
  Stara sie wstawic nl na ostatnim przed MaxCol znakiem onbreakChars (znak
  sposrod breakChars zostaje przy tym skasowany).}
function BreakLine(const s: string; MaxCol: integer;
  onbreakChars: TSetOfChars = WhiteSpaces): string; overload;

{ Returns S with all chars in ExcludedChars deleted. }
function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;

{ SReplaceChars zamienia wszystkie wystapienia znakow ze stringu / zbioru
  frompos w stringu s na :
   wersja 3 x string - zamienia znak na odpowiedni znak na tej samej pozycji w ToChars,
     np. wywolaj (s, 'ab','cd') aby zamienic wszystkie a na b i wszystkie c na d w s.
     Zawsze powinno byc Length(FromChars) <= Length(ToChars).
   wersja string, TSetOfChars, to char zamienia kazdy znak na ToChar (i dziala szybciej).

  Version with "FromChar: char" replaces just this one character to another. }
function SReplaceChars(const s, FromChars, ToChars: string): string; overload;
function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string; overload;
function SReplaceChars(const s: string; FromChar, ToChar: char): string; overload;

{ E.g. @code(SPad('29',4, '0')) gives '0029' }
function SPad(const s: string; len: integer; c: char = ' '): string; overload;

{ Like SPad with padding char set to '0'. }
function SZeroPad(const s: string; len: integer): string;

{ LoCase: pair for UpCase. Note that this doesn't take current locale
  into account (just like UpCase), it works only on English
  a-z -> A-Z letters. }
function LoCase(c: char): char;

function CharPos(c: char; const s: string): integer;

{ CharsPos : znajdz dowolny sposrod znakow chars w stringu s.
  Czyli to samo co FirstDelimiter ale z parametrem jako TSetOfChars i duzo bardziej
  sensowna nazwa. BackPos dziala w tyl.
  CharsPosEx searches starting from Offset char.
  Zwracaja 0 jesli nie znajda. }
function CharsPos(const chars: TSetOfChars; const s: string): integer;
function CharsPosEx(const chars: TSetOfChars; const s: string;
  Offset: Integer): integer;
function BackCharsPos(const chars: TSetOfChars; const s: string): integer;

{ BackPos jak Pos, ale znajduje ostatnie wystapienie. }
function BackPos(const SubS, S: string): integer;
{ FirstDelimiter : cos jak LastDelimiter, ale skanuje od lewej.
  zwraca 0 jesli nie znajdzie. }
function FirstDelimiter(const Delimiters, S: string): Integer;

{ SEnding returns S contents starting from position P.
  Returns '' if P > length(S).
  Yes, this is simply equivalent to Copy(S, P, MaxInt). }
function SEnding(const s: string; P: integer): string;

function IsPrefix(const Prefix, S: string;
  IgnoreCase: boolean = true): boolean; overload;
function IsSuffix(const Suffix, S: string;
  IgnoreCase: boolean = true): boolean; overload;

{ If IsPrefix(Prefix, S, IgnoreCase) then returns S with this prefix
  removed. Else returns S. }
function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;
{ Like PrefixRemove, but here checks for and removes Suffix }
function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;

{ SAppendData : dopisz do s DataSize bajtow z pamieci wskazywanej przez @@Data }
procedure SAppendData(var s: string; const Data; DataSize: integer);
{ SChar zwraca wskaznik na s[CharNum] czyli @@s[CharNum].
  Ale SChar nie wykonuje przy tym Range Checks ! }
function SChar(const s: string; CharNum: integer): PChar;
{ SCharIs : zwraca czy s[index] = c. ale zwraca false jesli s jest krotsze
  niz index (normalnie porownanie s[index] = c da RangeError (przy $R+)
  lub przypadkowy wynik (przy $R-) }
function SCharIs(const s: string; index: integer; c: char): boolean; overload;
function SCharIs(const s: string; index: integer; const chars: TSetOfChars): boolean; overload;
{ Zamienia spec. znaczki w stringu na #numer.
  Useful for printing strings with some unprintable chars for
  debugging purposes. }
function SReadableForm(const s: string): string;
{ CopyPos zwraca s[StartPoz, .... ,EndPoz] - dziala jak Copy ale ostatni paranetr
  okresla EndPoz zamiast ACount. Podobnie DeletePos dziala jak Delete. }
function CopyPos(const s: string; StartPoz, EndPoz: integer): string;
procedure DeletePos(var S: string; StartPoz, EndPoz: Integer);

{ NextToken szuka w s pierwszego znaku <not in TokenDelims>, poczynajac od SeekPos.
  Potem szuka dalej znaku <in TokenDelims>. Zwraca znaleziony sub-string (wlaczajac w to
  pierwszy znak not in TokDelims i wylaczajac koncowy znak in TokenDelims).

  Modyfikuje jednoczesnie zmienna SeekPos tak ze po wywolaniu wskazuje ona na pierwszy
  znak za koncowym znakiem ktory byl <not in TokDelims>, tzn.
  s[SeekPos-1] jest in TokenDelims (chyba ze SeekPos-1 > Length(s) ) a s[SeekPos-2] to
  ostatni znak w zwroconym tokenie. W rezultacie aby znalezc kolejny token powinienes
  nastepnym razem wywolac ta procedure znowu z SeekPos.

  Zwraca '' jesli skonczyly sie tokeny (wartosc SeekPos jest w tym wypadku nieokreslona).
  Typowe uzycie - iteruj po wszystkich tokenach w stringu :
    SeekPos := 1;
    repeat
     Token := NextToken(s, SeekPos);
     if Token = '' then break;
     ... process_next_token (Token) ...
    until false;

  Note: it's *much* easier to use CreateTokens instead of this procedure.
  This procedure has just a bit more functionality. }
function NextToken(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;
  overload;

{ NextTokenOnce umozliwia przekazanie parametru SeekPos by value.

  Oczywiscie, nie jest to dobre rozwiazanie jezeli chcemy znalezc wszystkie tokeny
  w stringu - ale jezeli zalezy nam tylko na jednym tokenie (jesli SeekPos = 1 to bedzie
  to pierwszy token) to ta procedura bedzie wygodniejsza od NextToken . }
function NextTokenOnce(const s: string; SeekPos: integer = 1;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;
  overload;

{ Returns TDynStringArray with tokens extracted from s.
  Token is something delimited by TokenDelims.
  TokenDelims are not contained in resulting items.
  E.g. CreateTokens('foo, bar', [' ', ',']) returns TDynStringArray
  with 2 items: 'foo' and 'bar'. }
function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars = WhiteSpaces): TDynStringArray;

{ NextTokenRestr to troche bardziej zaawansowane NextToken.

  Podobnie jak NextToken, kiedy NextTokenRestr napotka pierwszy znak not in
  TokenDelims to bedzie czytac string az napotka cos in TokenDelims.
  ALE znaki pomiedzy parami tych samych znakow w RestrAreas beda zawsze
  traktowane jako not in TokenDelims !

  Tym sposobem mozna np. powiedziec mu zeby odczytal nastepny token, gdzie
  tokeny sa oddzielone spacjami, ale zeby ignorowal spacje pojawiajace sie
  pomiedzy ' a ' lub pomiedzy " a ".

  Np. jesli wszystkie parametry default beda mialy defaultowe wartosci, to moze
  zwrocic taki token :
    blabla = 'spacje miedzy apostrofami sa ignorowane'
    hehe = 'po odczytaniu apostrofu czeka na dlugi apostrof, cudzyslow (") ignoruje'
    kuku="jeszcze jeden string"
    'jesli pierwszy napotkany znak na SeekPos in RestrictedAreas, tez to zauwazy'
    oczywiscie_moze_tez_nie_napotkac_nigdzie_niczego_in_RestrAreas
  ALE nie zwroci np. stringu
    'start od apostrofu, koniec na cudzyslowu"
  Przyklady pochodza wprost z rzeczy do ktorej uzywalem tej procedury : do parsowania
    argumentow HTMLowych tagow. }
function NextTokenRestr(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars = WhiteSpaces;
  const RestrAreas: TSetOfChars = ['''','"']): string; overload;

{ FindPos : mocno rozbudowana procedura Pos (kiedys pod nazwa PosX) .
  Szuka SubTekst w Copy(Tekst, StartPoz, Dlugosc) i zwraca pozycje (wzgledem calego Tekstu,
   nie wzgledem StartPoz). Pozycja w stringu pascalowym jest tradycyjnie liczna od 1.
   Jesli soMatchCase w Opcje to szukanie jest case-sensitive (jesli nie jest, to
     utozsamianie malych i duzych znakow uwzglednia locale-chars).
   Jesli soWholeWord, to znalezione wystapienie SubTekstu w Tekst musi dodatkowo
     byc otoczone znakami z WordBorders (albo koncem/poczatkiem calego stringa Tekst,
     np. FindPos('a','ba a',2, 1,[soWholeWord]) zwroci 0 = nie znajdzie - znalezione
       wystapienie 'a' nie jest otoczone wordBorders mimo ze znak nie bedacy WordBorderem
       jest poza przeszukiwanym wycinkiem Tekstu)
   Jesli soBackwards to szuka od tylu (co ma znaczenie bo FindPos zatrzymuje
     sie i zwraca wynik pierwszego znalezionego wystapienia, wiec wartosc
     soBackwards in Opcje okresla czy pierwszego od lewej czy od prawej).
  Zwraca 0 jesli nie znalazl. }
function FindPos(const SubTekst, Tekst: string; StartPoz, Dlugosc: integer;
  opcje: TSearchOptions;
  const WordBorders: TSetOfChars = DefaultWordBorders): integer; overload;

{ MatchingFind : sprawdza czy Copy(Text, MatchStart, MatchLength) jest prawidlowym
  wystapieniem SubText, tzn. nie tylko sprawdza czy jest rowne SubText ale uwzglednia
  tez flagi soMatchCase i soWholeWords w so. Przydatne jesli mamy tekst co do ktorego
  chcemy sprawdzic czy jest prawidlowym wynikiem poprzedniego FindPos.
  Uwaga : MatchingFind jest wygodne i uzyteczne do jednorazowaego wywolania.
  Pamietaj ze realizowanie FindPos jako ciaglych wywolan MatchingFind jest strasznie
  nieefektywne - np. jezeli soMatchCase nie jest ustawione to MatchingFind za kazdym
  razem robiloby czesc stringa od nowa na upper a FindPos zrobi to tylko raz. }
function MatchingFind(const SubText, Text: string;
  MatchStart, MatchLength: integer; so: TSearchOptions;
  const WordBorders: TSetOfChars): boolean; overload;
function MatchingFind(const SubText, Text: string;
  MatchStart, MatchLength: integer; matchCase, wholeWord: boolean;
  const WordBorders: TSetOfChars): boolean; overload;

{ FindWordPos - wykonuje FindPos(SubTekst, Tekst, 1, Length(Tekst),
   [soWholeWord, soMatchCase], WordBorders). Slowem, wykonuje normalne Pos ale
   znajduje tylko osobne slowa oddzielone WordBorders. Czyli robi FindPos ale
   ze specjalnych funkcji realizuje tylko soWholeWords. }
function FindWordPos(const SubTekst, Tekst: string;
  const WordBorders: TSetOfChars = DefaultWordBorders): integer; overload;
{ GetWordAtPos : znajdzie slowo otaczajace pozycje 'pozycja' w 'Tekst'. }
function GetWordAtPos(const tekst: string; pozycja: integer;
  const WordBorders: TSetOfChars = DefaultWordBorders): string; overload;

{ Return rightmost RPart characters from S.
  If RPart > Length(S) then returns S. }
function SRight(const s: string; const rpart: integer): string;

{ If S = '' then returns NextPart, else returns S + PartSeparator + NextPart. }
function SAppendPart(const s, PartSeparator, NextPart: string): string;

function FileToString(const fname: string): string;
procedure StringToFile(const fname, contents: string);

type
  EDeformatError = class(Exception);

{ DeFormat :

  format to ciag bialych znakow, specyfikacji typu %s %d lub %f i czarnych
  znakow. %% oznacza czarny znak '%'. 1 lub wiecej bialych znakow w
  format to 1 lub wiecej bialych znakow w data (ale nie musi ich byc
  tyle samo tu i tu). Dotyczy to takze koncowki i poczatku stringa -
  format = ' %d' oznacza ze przed integerem musi byc przynajmniej jedna
  biala spacja.

  %d to Integer, moze byc ze znakiem.

  %f to Float, moze byc ze znakiem i z kropka.

  %.single. , %.double., %.extended. to odpowiednie typy zmiennoprzec.
  (uwaga - nazwy typow sa case-sensitive, musza byc pisane malymi literami).

  %s to AnsiString, zakonczy sie na pierwszym bialym znaku w data (lub na koncu
  data). Uwaga - %s moze byc utozsamiony z '' jesli np. format = '%d %s'
  data = '123 '.

  Wszystkie pozostale czarne znaki musza wystepowac dokladnie tak samo
  w data jak sa w format. Parametr IgnoreCase kontroluje czy ignorowac
  roznice w wielkosci liter.

  Format musi objac cale data - po przeczytaniu format data powinno sie
  skonczyc, inaczej blad. Jest jeden wyjatek : data moze zawierac nadprogramowe
  biale znaki na poczatku i na koncu.

  TryDeFormat zwraca ile sposrod args zostalo zainicjowanych - a wiec
  dopuszcza sytuacje ze Data skonczylo sie przed Format. DeFormat
  wyrzuca w takiej sytuacji wyjatek.

  Napisane bo zarowno wersje z JclSscanf jak i z modulu scanf dzialaly zle
  - patrz openGL.testy/nehe10.

  DeFormat rzuca wyjatkiem EDeformatError w razie jakiegokolwiek bledu
  - niezgodnosci data z format. Pamietaj ze wtedy niektore sposrod
  args moga byc zmienione a niektore nie. }
procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer; IgnoreCase: boolean = true); overload;
function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer; IgnoreCase: boolean = true): integer; overload;

{ FileFilter jest w postaci 'xxxx|name1.ext1;name2.ext2' albo
  'name1.ext1;name2.ext2' gdzie xxxx to moze byc cokolwiek (nie zawierajace |),
  podobnie nameX (nie moze zawierac tylko . i ;) i extX (nie moze zawierac . i ;)
  i rozszerzen z nazwami moze byc dowolnie wiele.
  W tablicy Extensions (ktora musi byc juz zainicjowanym obiektem) zwraca liste
  stringow [.ext1,.ext2,.ext3 ... ].
  Innymi slowy, wyciaga rozszrzenia z typowego filtra na pliki uzytego jako
  OpenDialog.Filter. }
procedure GetFileFilterExts(const FileFilter: string; var Extensions: TDynStringArray);

{ FileFilter jest postaci 'filter_name', 'filter_name|exts',
  'filter_name(exts)|exts', wszedzie pomiedzy moga wystepowac biale znaki.
  Dla kazdej z tych postaci zwroci Trim(filter_name) (w ostatnim przypadku
  sprawdzi czy oba wystapienia exts sa takie same zeby nie dac sie nabrac
  na pulapke 'Pascal files (really!)|*.pas' -> zwroci 'Pascal files (really!)',
  ale 'Pascal files (*.pas)|*.pas' -> zwroci 'Pascal files'.
  exts w czesci prawej musza byc rozdzielane srednikami, w czesci lewej -
  moga tez byc rozdzielane przecinkami. }
function GetFileFilterName(const FileFilter: string): string;

{ szuka w FileFilter znaku '|' i zwraca wszystko co jest za nim (czyli taka
  prymitywna podstawa do robienia tego co robi GetFileFilterExts).
  Przy okazji - zauwaz ze wynika z tego ze napis bez znaku '|' uznajemy
  za samo Name, bez zadnych Exts, a nie na odwrot. }
function GetFileFilterExtsStr(const FileFilter: string): string;

{ SReplacePatterns : podobnie jak StringReplace, tyle ze mozna na raz
  zamienic wiele OldPattern na NewPattern. (Tablice patterns i values musza
  miec taka sama ilosc elementow. patterns[0] zostana zamienione na values[0] itd.
  Pattern sa zamieniane od lewej, tzn, jezeli wystapienia dwoch patterns
  zachodza na siebie zamieniona zostanie pattern zaczynajaca sie wczesniej.
  Jezeli obie patterns zaczynaja sie w tym samym miejscu zostanie wybrana
  ta o mniejszym indeksie w tablicy patterns[]).

  Ta funkcja unika pulapki ktora powstaje gdy jedna z values[] zawiera jedna
  z patterns - ta funkcja nie zauwazy tego, zamienia tylko wystapienia w
  oryginalnej tresci s. W przeciwienstwie do pozornie poprawnego kodu
   result := s;
   result := StringReplace(result, patterns[0], values[0], [rfReplaceAll]);
   result := StringReplace(result, patterns[1], values[1], [rfReplaceAll]);
   itd.
  Powyzszy kod nie dziala tak jak moznaby tego oczekiwac gdy np. values[0]
  zawiera wystapienie patterns[1].
  Opcje NIE MOZE zawierac flagi soBackwards ! }
function SReplacePatterns(const s: string; const patterns, values: array of string; Options: TSearchOptions): string;

function SCharsCount(const s: string; c: char): Cardinal; overload;
function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal; overload;

{ obcina z s wszystko za pierwszym znakiem '#' w stringu
  (lacznie z tym '#'). Jezeli w s nie ma '#' to zwraca s.
  Przydatne do odczytywania prostych plikow tekstowych z takimi komentarzami.
  Dla plikow tekstowych o nieco bardziej zlozonym formacie - pamietaj
  ze # moze byc np. czescia jakiegos stringa itp. i wtedy niekoniecznie
  oznacza poczatek komentarza. }
function STruncateHash(const s: string): string;

{ Zamien s na takie cos zeby Format(SUnformattable(s), [...]) = s.
  Innymi slowy spraw zeby procedura Format nie widziala zadnego znacznika
  formatowania w stringu s - czyli mowiac wprost, podwoj znaki '%'. }
function SUnformattable(const s: string): string;

{ SAnsiCompare robi AnsiCompareStr lub AnsiCompareText, w zaleznosci od
  IgnoreCase. Wiec porownuje stringi uzywajac current locale i moze
  rozrozniac lub nie duze / male litery (przy czym jezeli ma nie rozrozniac
  liter to current locale tez ma znaczenie, np. na polskim Windowsie
  bedzie uznawal male i duze "a z ogonkiem" za to samo).
  Zwraca <0 wtw. s1 < s2, =0 gdy s1 = s2 i >0 gdy s1 > s2. }
function SAnsiCompare(const s1, s2: string; IgnoreCase: boolean): Integer;

{ SAnsiSame does SAnsiCompare() = 0 }
function SAnsiSame(const s1, s2: string; IgnoreCase: boolean): boolean;

{ Formalnie:
    Kazdy dwuznakowy ciag postaci
      PercentChar + Replaces[i].c (dla dowolnego i)
    jaki znajdziemy w Format zamieniamy na
      Replaces[i].s (dla tego samego i ktore pasowalo wyzej)
    Ponadto ciag PercentChar+PercentChar zamieniamy na pojedyncze PercentChar.
    Jezeli ErrorOnUnknownPercentFormat i znajdziemy w stringu ciag PercentChar+c
      gdzie c nie wystepuje nigdzie jako Replaces[i].c ani nie jest rowne PercentChar
      (przy czym c moze byc takze koncem stringa, tzn. ma to takze zastosowanie gdy
      na koncu Format jest pojedynczy PercentChar)
      to rzucamy wyjatek EUnknownPercentFormat z opisem 'Unknown formatting sequence :
      %c. Allowed formatting sequences are .... (and "%%" for "%")'
    Cala reszta Format pozostaje bez zmian.
    Tak zmodyfikowany Format zwracamy. Oczywiscie zamiany robimy jednokrotnie,
      przesuwajac sie zawsze do przodu, wiec np. '%%%%' zamienimy na '%%' (nie wychwycimy
      tych '%%' ponownie jako pary).
  Nieformalnie: tak funkcja formatuje string Format zamieniajac sekwencje w rodzaju
    '%a' na zadane stringi. Np. podaj Replaces = [(c:'a'; s:'kot'), (c:'b'; s:'pies')]
    a rezultat bedzie taki ze w podanym Format wszystkie ciagi postaci '%a' zostana
    zamienione na 'kot' a '%b' na 'pies'. Jezeli not ErrorOnUnknownPercentFormat
    to ciagi z rodzaju '%d' zostana w tym momencie zostawione jako '%d' (tzn. jezeli
    po '%' wystepuje literka taka jak 'd', ktora nie ma przypisanego formatu, to po prostu
    nie wykonamy zadnego formatowania). Ale zazwyczaj nie jest to zbyt rozsadne bo
    zazwyczaj uzywajac tej funkcji bedziesz chcial przygotowac sie na to ze mozesz
    sobie kiedys rozszerzac Replaces - jezeli np. dodasz wtedy (c:'d'; s:'kura')
    to string '%d' zostanie zamieniony 'kura'. Wiec lepiej uzywaj podwojnego '%%'
    aby uzyskac '%'. Jezeli ErrorOnUnknownPercentFormat = true to jestes do tego nawet
    ZMUSZONY bo wszelki ciag '%d' (lub sam '%' na koncu stringi) gdzie 'd' nie wystepuje
    w Replaces[].c spowoduje wyjatek.

    Ponadto mozesz konfigurowac czym jest PercenChar. Standardowo jest on
    wlasnie procentem, czyli '%', ale mozesz uzyc dowolnego znaku.
    IgnoreCase nie ma znaczeniu przy szukaniu PercentChar, znak PercentChar
    jest zawsze szukany dokladnie, bez jakiegokolwiek ignorowania wielkosci
    liter.

    Nie jest zdefiniowane jak zachowa sie ta procedura kiedy wsrod Replaces jakis
    znak c pojawi sie wiecej niz raz lub jakis znak c bedzie rowny PercentChar.
}
type
  TPercentReplace = record
    { @noAutoLinkHere }
    c: char;
    { @noAutoLinkHere }
    s: string;
  end;

  EUnknownPercentFormat = class(Exception);
function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean = true;
  PercentChar: char ='%';
  IgnoreCase: boolean = false): string; overload;

function AnsiUpperCaseChar(C: char): char;
function AnsiLowerCaseChar(C: char): char;

{ Returns S with S[1] character replaced with AnsiUpperCaseChar(S[1])
  (unless S does not have 1st char, i.e. S = '') }
function SAnsiUpperFirstChar(const S: string): string;

{ convertions ------------------------------------------------------------ }

const
  { I should restrain from adding more similiar BoolToStrXxx constants
    below, since there are *so* many possibilities here (on/off, ON/OFF,
    On/Off, yes/no, YES/NO etc.) that it's quite useless trying to
    gather them all here. }
  BoolToStr: array[boolean] of string=('FALSE','TRUE');
  BoolToStrYesNo: array[boolean]of string = ('No','Yes');
  { BoolToStrNNN: array[boolean]of string = (' not ',' '); }

{ 0 konwertuje na '0' itd. Uzywaj tylko dla b = 0..9 }
function DigitAsChar(b: byte): char;
{ c = '0' .. '9' konwertuje na 0..9 }
function DigitAsByte(c: char): byte;

{ IntToStrZpad = jak IntToStr ale padduje od lewej zerami jesli trzeba }
function IntToStrZPad(n: integer; minLength: integer): string;
{ zamienia n na string w ukladzie poz-pozycyjnym. Poz nie moze byc wieksze
  niz 'Z'-'A'+1+10 (zabrakloby nam wtedy symboli). }
function IntToStrPoz(n: Int64; poz: Byte): string; overload;
{ j.w., ale padded with zeros }
function IntToStrPoz(n: Int64; poz: Byte; minLength: Cardinal): string; overload;
{ Write n in binary.
  MinLength means to left-pad with zeros if necessary.
  You can supply a value for zero digit (default '0'), one digit (default '1')
  and minus sign (default '-'). }
function IntToStr2(n: Int64; minLength: Cardinal = 1): string; overload;
function IntToStr2(n: Int64; MinLength: Cardinal;
  ZeroDigit, OneDigit, MinusSign: char): string; overload;
{ wypisuje n zapisana heksadecymalnie (w ukladnie 16-stkowym) }
function IntToStr16(n: Int64; minLength: Cardinal = 1): string; overload;
function ToStr(const args: array of const): string;
function VarRecToStr(const v: TVarRec): string;

{ This returns IntToStr(Value) with ThousandSeparator inserted to
  separate thousands (only if ThousandSeparator <> #0). }
function IntToStrThousandSep(const Value: Int64): string;

{ zamienia ciag 0 i 1 (z ew. znakiem +/-) na liczbe; wyrzuca EConvertError w
  razie bledu }
function Str2ToInt(const s: string): integer;
{ Convert string in hexadecimal (0-9, a-z, A-Z are digits,
  optional sign -/+ allowed) to Int64.
  Raises EConvertError on problems with conversion }
function StrHexToInt(const s: string): Int64;
function StrToFloatDef(const s: string; DefValue: Extended): Extended;

function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;
 { wypisze Set jakby byl set of NumStart..NumEnd. Uwaga ! Implementacja zaklada ze
   realizacja set'u NumStart..NumEnd to set of byte obciety i dosuniety do lewej,
   co w przypadku realizacji Borlanda moze sie okazac bledne - np.
   set of 1..16 jest realizowany na 3 bajtach (pierwszy bit tracony, marnujemy jeden bajt,
          ale za to mamy szybkosc - badajac i in set nie trzeba robic i := i-1.
          Tak naprawde SizeOf(ten set) = 4 bo mamy dopelnianie do 4 bajtow - adresy podzielne
          na 4 moga byc osiagane szybciej)
   ale SizeOf(set of 200..216) = 4 takze, czyli tutaj juz kompilator jest na tyle sprytny
    ze nie alokuje 216 bitow na zapamietanie 17 bitow, orientuje sie ze jest to tylko
    maly przedzial.
   Podsumowujac, nie mam nic przeciwko tej realizacji set'u przez Borlanda ale powoduje
    ona ze NumStart dla powyzszej procedury powinien byc dobierany ostroznie zeby wynik
    nie zawieral nieznaczacych bitow. Slowem, uzywaj tej funkcji raczej tylko do testow.
    (no, ew. dla set of 0..NumEnd lub set of byte wynik bedzie zawsze ok.)
 }

{ PCharOrNil - cos jak cast PChar(s) ale zwraca nil jesli s = ''.
  Ku mojemu zdziwieniu cast PChar(s) tak nie robi. }
function PCharOrNil(const s: string): PChar;

{ some ASCII funcs / codes -------------------------------------------------- }

const
  CtrlA = Chr(Ord('a') - Ord('a') + 1); { = #1 }
  CtrlB = Chr(Ord('b') - Ord('a') + 1); { = #2 }
  CtrlC = Chr(Ord('c') - Ord('a') + 1); { ... etc. }
  CtrlD = Chr(Ord('d') - Ord('a') + 1);
  CtrlE = Chr(Ord('e') - Ord('a') + 1);
  CtrlF = Chr(Ord('f') - Ord('a') + 1);
  CtrlG = Chr(Ord('g') - Ord('a') + 1);
  CtrlH = Chr(Ord('h') - Ord('a') + 1); { = CharBackspace }
  CtrlI = Chr(Ord('i') - Ord('a') + 1); { = CharTab }
  CtrlJ = Chr(Ord('j') - Ord('a') + 1);
  CtrlK = Chr(Ord('k') - Ord('a') + 1);
  CtrlL = Chr(Ord('l') - Ord('a') + 1);
  CtrlM = Chr(Ord('m') - Ord('a') + 1); { = CharEnter }
  CtrlN = Chr(Ord('n') - Ord('a') + 1);
  CtrlO = Chr(Ord('o') - Ord('a') + 1);
  CtrlP = Chr(Ord('p') - Ord('a') + 1);
  CtrlQ = Chr(Ord('q') - Ord('a') + 1);
  CtrlR = Chr(Ord('r') - Ord('a') + 1);
  CtrlS = Chr(Ord('s') - Ord('a') + 1);
  CtrlT = Chr(Ord('t') - Ord('a') + 1);
  CtrlU = Chr(Ord('u') - Ord('a') + 1);
  CtrlV = Chr(Ord('v') - Ord('a') + 1);
  CtrlW = Chr(Ord('w') - Ord('a') + 1);
  CtrlX = Chr(Ord('x') - Ord('a') + 1);
  CtrlY = Chr(Ord('y') - Ord('a') + 1);
  CtrlZ = Chr(Ord('z') - Ord('a') + 1); { = #26 }

  CharBackSpace = #8;
  CharTab = #9;
  CharEnter = #13;
  CharEscape = #27;

function DescribeKey(c: char): string;

implementation

uses KambiFilesUtils;

function RandomString: string;
var i: integer;
begin
 result := '';
 for i := 1 to random(10) do result := result+char(byte('A')+Random(26));
 for i := 1 to 3 do result := result+char(byte('0')+Random(10));
end;

procedure StringReplaceAllTo1st(var s: string; const fromPat, toPat: string;
  IgnoreCase: boolean);
(*
 { NAIWNA IMPLEMENTACJA : zawsze szuka w nowym s od subs_orig od poczatku
   (w rezultacie poczatek stringa przeszukajac wiele razy niepotrzebnie).
   No i moze sie zapetlic gdy subs_repl zawiera w sobie subs_orig. }
var p: integer;
begin
 {assert( Pos(subs_orig, subs_repl) = 0 , 'blad w ReplaceSubstr !');}
 p := Pos(subs_orig, s); (tutaj powinna byc sterowana IgnoreCase) }
 while p > 0 do
 begin
  Delete(s, p, length(subs_Orig));
  Insert(subs_repl, s, p);
  p := Pos(subs_orig, s); (tutaj powinna byc sterowana IgnoreCase)
 end;
*)
begin
 if IgnoreCase then
  s := StringReplace(s, fromPat, toPat, [rfReplaceAll, rfIgnoreCase]) else
  s := StringReplace(s, fromPat, toPat, [rfReplaceAll]);
end;

function BreakLine(const s: string; MaxCol: integer; onbreakChars: TSetOfChars): string;
var done: integer;
    nowcol, i, brk: integer;
label brokenSuccess;
const breakingstr = nl;
begin
 Done := 0;
 Result := '';

 i := 1;
 while i <= Length(s) do
 begin
  if s[i] in [#10, #13] then
  begin
   { niech i obejmie cale zakonczenie linii ktore moze byc 2-znakowe #13#10 lub #10#13 }
   case s[i] of
    #13 : if SCharIs(s, i+1, #10) then Inc(i);
    #10 : if SCharIs(s, i+1, #13) then Inc(i);
   end;
   Result := Result + CopyPos(s, Done+1, i);
   Done := i;
  end else
  begin
   NowCol := i - Done;
   if NowCol > MaxCol then
   begin
    { we got line s[done+1..i] that we have to break somewhere. }
    for brk := i downto Done + 1 do
     if s[brk] in OnBreakChars then
     begin
      Result := Result + CopyPos(s, Done+1, Brk-1) + BreakingStr;
      Done := brk; { we left the rest : s[brk+1..i] to be done }
      goto brokenSuccess;
     end;
    { ups ! it can't be broken - no onbreakChars found ! so we break after
      done+maxcol position. }
    Result := Result + Copy(s, Done+1, MaxCol) + BreakingStr;
    Done := Done + MaxCol;
    brokenSuccess:;
   end;
  end;

  Inc(i);
 end;

 if Done < Length(S) then
  Result := Result + SEnding(S, Done+1);
end;

function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;
var i, j: integer;
begin
 SetLength(result, length(s));
 j := 1;
 for i := 1 to length(s) do
  if not (s[i] in excludedChars) then
   begin result[j] := s[i]; Inc(j); end;
 SetLength(result, j-1);
end;

function SReplaceChars(const s, FromChars, ToChars: string): string;
var i, p: integer;
begin
 result := s;
 for i := 1 to Length(result) do
 begin
  p := CharPos(result[i], FromChars);
  if p > 0 then result[i] := ToChars[p];
 end;
end;

function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string;
var i: integer;
begin
 result := s;
 for i := 1 to Length(result) do
  if result[i] in FromChars then result[i] := ToChar;
end;

function SReplaceChars(const s: string; FromChar, ToChar: char): string;
var i: Integer;
begin
 Result := S;
 for i := 1 to Length(Result) do
  if Result[i] = FromChar then Result[i] := ToChar;
end;

function Chars(count: integer; c: char): string;
begin
 setLength(result, count);
 fillChar(Pointer(result)^,count, c);
end;

function SPad(const s: string; len: integer; c: char): string;
var lnow: integer;
begin
 lnow := length(s);
 if lnow < len then
  Result := Chars(len-lnow, c) + s else
  Result := s;
end;

function SZeroPad(const s: string; len: integer): string;
begin result := SPad(s, len, '0') end;

function LoCase(c: char): char;
begin
 if c in ['A'..'Z'] then
  result := chr(ord(c)-ord('A')+ord('a')) else
  result := c;
end;

function CharPos(c: char; const s: string): integer;
var i: integer;
begin
 for i := 1 to length(s) do
  if s[i] = c then begin result := i; exit end;
 result := 0;
end;

function CharsPos(const chars: TSetOfChars; const s: string): integer;
begin
 for result := 1 to Length(s) do
  if s[result] in chars then exit;
 result := 0;
end;

function CharsPosEx(const Chars: TSetOfChars; const S: string;
  Offset: Integer): integer;
begin
 for Result := Offset to Length(S) do
   if S[Result] in Chars then Exit;
 Result := 0;
end;

function BackCharsPos(const chars: TSetOfChars; const s: string): integer;
begin
 for result := Length(s) downto 1 do
  if s[result] in chars then exit;
 result := 0;
end;

function BackPos(const SubS, S: string): integer;
begin
 for result := Length(S)-Length(SubS)+1 downto 1 do
  if SubS = Copy(S, result, Length(SubS)) then exit;
 result := 0;
end;

function FirstDelimiter(const Delimiters, S: string): Integer;
begin
 for result := 1 to Length(s) do
  if CharPos(S[result], Delimiters) <> 0 then exit;
 result := 0;
end;

function SEnding(const S: string; P: integer): string;
begin
 result := Copy(S, P, MaxInt)
end;

function IsPrefix(const Prefix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(Copy(S, 1, Length(Prefix)), Prefix) = 0 else
  result := AnsiCompareStr(Copy(S, 1, Length(Prefix)), Prefix) = 0;
end;

function IsSuffix(const Suffix, S: string; IgnoreCase: boolean): boolean;
begin
 if IgnoreCase then
  result := AnsiCompareText(SRight(S, Length(Suffix)), Suffix) = 0 else
  result := AnsiCompareStr(SRight(S, Length(Suffix)), Suffix) = 0;
end;

function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;
begin
 if IsPrefix(Prefix, S, IgnoreCase) then
  Result := SEnding(S, Length(Prefix) + 1) else
  Result := S;
end;

function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;
begin
 Result := S;
 if IsSuffix(Suffix, S, IgnoreCase) then
 begin
  { doing assignment and SetLength should be a little faster
    than doing Result := Copy(S, 1, ...) }
  SetLength(Result, Length(s) - Length(Suffix));
 end;
end;

procedure SAppendData(var s: string; const Data; DataSize: integer);
var OldLen: integer;
begin
 OldLen := Length(s);
 SetLength(s, OldLen+DataSize);
 Move(Data, SChar(s, OldLen+1)^ , DataSize);
end;

{$Include NoRQCheckBegin.inc}
function SChar(const s: string; CharNum: integer): PChar;
begin Result := @s[CharNum] end;
{$Include NoRQCheckEnd.inc}

function SCharIs(const s: string; index: integer; c: char): boolean;
begin result:=(index <= Length(s)) and (s[index] = c) end;

function SCharIs(const s: string; index: integer; const chars: TSetOfChars): boolean;
begin result:=(index <= Length(s)) and (s[index] in chars) end;

function SReadableForm(const s: string): string;
var i: integer;
begin
 result := '';
 for i := 1 to Length(s) do
  if ( Ord(s[i]) < Ord(' ') ) then
   result := result+'#'+IntToStr(Ord(s[i])) else
   result := result+s[i];
end;

function CopyPos(const s: string; StartPoz, EndPoz: integer): string;
begin
 result := Copy(s, StartPoz, EndPoz - StartPoz + 1);
end;

procedure DeletePos(var S: string; StartPoz, EndPoz: Integer);
begin
 Delete(S, StartPoz, EndPoz - StartPoz + 1);
end;

function NextToken(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars): string;
var TokStart: integer;
begin
 repeat
  if SeekPos > Length(s) then begin result := ''; exit end;
  if S[SeekPos] in TokenDelims then Inc(SeekPos) else break;
 until false;
 TokStart := SeekPos; {TokStart := pierwszy znak not in TokenDelims}

 while (SeekPos <= Length(s)) and not(S[SeekPos] in TokenDelims) do Inc(SeekPos);

 result := Copy(s, TokStart, SeekPos-TokStart); {result := s[TokStart, ... , SeekPos-1] }
 Inc(SeekPos); { moglibysmy nie robic tu Inc(seekPos) ale wiadomo ze szukania nastepnego
                 tokenu nie warto zaczynac od SeekPos bo przeciez wiemy ze s[SeekPos] to
                 TokenDelim ! }
end;

function NextTokenOnce(const s: string; SeekPos: integer;
  const TokenDelims: TSetOfChars): string;
begin
 result := Nexttoken(S, SeekPos, TokenDelims);
end;

function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars): TDynStringArray;
var SeekPos: Integer;
    Token: string;
begin
 Result := TDynStringArray.Create;
 try
  SeekPos := 1;
  repeat
   Token := NextToken(s, SeekPos, TokenDelims);
   if Token = '' then break;
   Result.AppendItem(Token);
  until false;
 except Result.Free; raise end;
end;

{$WARNINGS OFF}
function NextTokenRestr(const s: string; var SeekPos: integer;
  const TokenDelims: TSetOfChars; const RestrAreas: TSetOfChars): string;
var TokStart: integer;
    InRestr: boolean;     { czy jestesmy w restricted area }
    RestrBeginChar: char; { znak ktory rozpoczal restricted area w ktorym jestesmy }
begin
 repeat
  if SeekPos > Length(s) then begin result := ''; exit end;
  if S[SeekPos] in TokenDelims then Inc(SeekPos) else break;
 until false;
 TokStart := SeekPos; {TokStart := pierwszy znak not in TokenDelims}

 InRestr := false;
 while (SeekPos <= Length(s)) and ( not(S[SeekPos] in TokenDelims) or InRestr) do
 begin
  if InRestr then
  begin
   if S[SeekPos] = RestrBeginChar then InRestr := false;
  end else
  begin
   if S[SeekPos] in RestrAreas then begin InRestr := true; RestrBeginChar := S[SeekPos] end;
  end;

  Inc(SeekPos);
 end;

 result := Copy(s, TokStart, SeekPos-TokStart); {result := s[TokStart, ... , SeekPos-1] }
 Inc(SeekPos); { moglibysmy nie robic tu Inc(seekPos) ale wiadomo ze szukania nastepnego
                 tokenu nie warto zaczynac od SeekPos bo przeciez wiemy ze s[SeekPos] to
                 TokenDelim ! }
end;
{$WARNINGS ON}

function FindPos(const subTekst, tekst: string; startPoz, dlugosc: integer; opcje: TSearchOptions; const WordBorders: TSetOfChars): integer;
var S, SubS: string;

  function MatchingPos(i: integer): boolean;
  { sprawdz czy i jest dobra pozycja wystapienia SubS w S.
    Uwzglednij przy tym czy soWholeWord in opcje, zachowuj sie zawsze
    jakby bylo soMatchCase in opcje. }
  var realI: integer;
  begin
   result := false;
   if Copy(S, i, Length(SubS)) = SubS then
   begin
    if soWholeWord in opcje then
    begin
     realI := i+startPoz-1;
     if ( (realI = 1) or (tekst[realI-1] in wordBorders) ) and
        ( (realI+length(subS)-1 = length(tekst)) or (tekst[realI+length(subS)] in WordBorders) )
     then result := true
    end else result := true;
   end;
  end;

var i: integer;
begin
 S := copy(tekst, startPoz, dlugosc);
 SubS := SubTekst;
 if not (soMatchCase in opcje) then
 begin
  S := AnsiUpperCase(S);
  SubS := AnsiUpperCase(SubS);
 end;
 result := 0;
 if soBackwards in opcje then
 begin
  for i := dlugosc-Length(SubS)+1 downto 1 do
   if MatchingPos(i) then begin result := i; break end;
 end else
 begin
  for i := 1 to dlugosc-Length(SubS)+1 do
   if MatchingPos(i) then begin result := i; break end;
 end;
 if result > 0 then result := result+startPoz-1;
end;

function MatchingFind(const SubText, Text: string; MatchStart, MatchLength: integer; matchCase, wholeWord: boolean; const WordBorders: TSetOfChars): boolean;
var Match: string;
    start, stop: integer;
begin
 result := false;

 { ponizsze sprawdzenie nie jest konieczne ale czesto pozwoli bardzo szybko
   odrzucic nieprawidlowe matching }
 if Length(SubText) <> MatchLength then exit;

 Match := Copy(Text, MatchStart, MatchLength);
 if MatchCase then
 begin
  if not AnsiSameStr(SubText, Match) then exit;
 end else
 begin
  if not AnsiSameText(SubText, Match) then exit;
 end;

 if WholeWord then
 begin
  start := MatchStart-1;
  if (start > 0) and not (Text[start] in WordBorders) then exit;
  stop := MatchStart+MatchLength;
  if (stop <= Length(Text)) and not (Text[stop] in WordBorders) then exit;
 end;

 result := true;
end;

function MatchingFind(const SubText, Text: string; MatchStart, MatchLength: integer;
  so: TSearchOptions; const WordBorders: TSetOfChars): boolean;
begin
 result := MatchingFind(SubText, Text, MatchStart, MatchLength, soMatchCase
   in so, soWholeWord in so, WordBorders);
end;

function FindWordPos(const SubTekst, Tekst: string; const WordBorders: TSetOfChars {DefaultWordBorders}): integer;
var i: integer;
begin
 for i := 1 to Length(Tekst) - Length(subTekst) +1 do
  if (Copy(Tekst, i, Length(SubTekst)) = SubTekst) and
     ( (i = 1) or (Tekst[i-1] in WordBorders) ) and
     ( (i+Length(SubTekst)-1 = Length(Tekst)) or (Tekst[i+Length(SubTekst)] in WordBorders) )
    then begin result := i; exit end;
 result := 0;
end;

function GetWordAtPos(const tekst: string; pozycja: integer; const WordBorders: TSetOfChars): string;
var pozStart, dlug, tekstLen: integer;
begin
 pozStart := pozycja;
 dlug := 0;
 tekstLen := length(tekst);
 while (pozStart > 1) and (not (tekst[pozStart-1] in wordBorders)) do
  begin Dec(pozStart); Inc(dlug); end;
 while (pozycja < tekstLen) and (not (tekst[pozycja] in wordBorders)) do
  begin Inc(pozycja); Inc(dlug); end;
 result := copy(tekst, pozStart, dlug);
end;

function SRight(const s: string; const rpart: integer): string;
begin
 if Length(s) < rpart then
  result := s else
  result := Copy(s, Length(s)-rpart+1, rpart);
end;

function SAppendPart(const s, PartSeparator, NextPart: string): string;
begin
 if s = '' then
  result := NextPart else
  result := s+PartSeparator+NextPart;
end;

function FileToString(const fname: string): string;
var f: file;
begin
 SafeReset(f, fname, true);
 try
  SetLength(result, FileSize(f));
  BlockRead(f, PChar(result)^, Length(result));
 finally
  CloseFile(F);
 end;
end;

procedure StringToFile(const fname, contents: string);
var F: File;
begin
 SafeRewrite(F, fname);
 try
   BlockWrite(F, PChar(contents)^, Length(contents));
 finally
   CloseFile(F);
 end;
end;

procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer; IgnoreCase: boolean);
begin
 if TryDeFormat(Data, Format, args, IgnoreCase) < High(args)+1 then
  raise EDeformatError.CreateFmt(
    'Unexpected end of Data (%s) - format (%s) not fully evaluated',
    [Data, Format]);
end;

function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer; IgnoreCase: boolean): integer;
var datapos, formpos: integer;

  function ReadExtendedData: Extended;
  var dataposstart: integer;
  begin
   {pierwszy znak liczby moze byc + lub -. Potem musza byc same cyfry.}
   if not (data[datapos] in ['0'..'9', '+', '-']) then
    raise EDeformatError.CreateFmt('float not found in data ''%s'' on position %d', [data, datapos]);
   dataposstart := datapos;
   Inc(datapos);
   while (datapos <= Length(data)) and (data[datapos] in ['0'..'9','.', 'e','E', '-', '+']) do
    Inc(datapos);
   {ponizsze StrToFloat tez moze spowodowac blad jesli np.
    wyszedl nam string '-' lub '+' lub string z dwoma kropkami}
   result := StrToFloat(CopyPos(data, dataposstart, datapos-1));
  end;

  function ReadIntegerData: Integer;
  var dataposstart: integer;
  begin
   {pierwszy znak integera moze byc + lub -. Potem musza byc same cyfry.}
   if not (data[datapos] in ['0'..'9', '+', '-']) then
    raise EDeformatError.CreateFmt('integer not found in data ''%s'' on position %d', [data, datapos]);
   dataposstart := datapos;
   Inc(datapos);
   while (datapos <= Length(data)) and (data[datapos] in ['0'..'9']) do
    Inc(datapos);
   {ponizszy StrToInt tez moze spowodowac blad jesli np.
    wyszedl nam string '-' lub '+'}
   result := StrToInt(CopyPos(data, dataposstart, datapos-1));
  end;

  function ReadStringData: string;
  var dataposstart: integer;
  begin
   dataposstart := datapos;
   while (datapos <= Length(data)) and
         (not (data[datapos] in WhiteSpaces)) do Inc(datapos);
   result := CopyPos(data, dataposstart, datapos-1);
  end;

  function ReadTypeSpecifier: string;
  {odczytaj type specifier z kropka z format. Przesun formpos}
  var formposstart: integer;
  begin
   formposstart := formpos;
   repeat
    if formpos > Length(format) then
     raise EDeformatError.Create('type specifier incorrect in  format '''+format+'''');
    if format[formpos] = '.' then
     break else
     Inc(formpos);
   until false;
   result := CopyPos(format, formposstart, formpos-1);
   Inc(formpos); { omin kropke '.' w format }
  end;

  procedure CheckBlackChar(formatchar: char);
  var BlackCharsCheck: boolean;
  begin
   if IgnoreCase then
    BlackCharsCheck := SameText(Data[datapos], format[formpos]) else
    BlackCharsCheck := Data[datapos] = format[formpos];
   if not BlackCharsCheck then
    raise EDeformatError.CreateFmt('data (%s) and format (%s) don''t match', [data, format]);
  end;

begin
 {omin biale znaki na poczatku i na koncu data jesli nie ma ich w format}
 if not SCharIs(format, 1, WhiteSpaces) then data := TrimLeft(data);
 if not ((Length(format) > 0) and (format[Length(format)] in WhiteSpaces)) then
  data := TrimRight(data);

 datapos := 1;
 formpos := 1;
 result := 0; { no args done yet }
 while formpos <= Length(Format) do
 begin
  {datapos > Length(data) -> means Data has ended but Format not.
   OK, so we can exit, because we are doing only TryDeFormat.
   Real DeFormat should check our result if it wishes to check that we parsed
   whole Format.}
  if datapos > Length(data) then exit;

  {1 or more whitespace in format means 1 or more whitespaces in data}
  if format[formpos] in WhiteSpaces then
  begin
   if not (Data[datapos] in WhiteSpaces) then
    raise EDeformatError.Create('whitespace not found in data '''+data+''' as requested by format '''+format+'''');
   repeat Inc(formpos) until not (format[formpos] in WhiteSpaces);
   repeat Inc(datapos) until not (data[datapos] in WhiteSpaces);
  end else

  {%+something means "read this from data", %% means "read %"}
  if format[formpos] = '%' then
  begin
   Inc(formpos);
   if formpos > Length(format) then
    raise EDeformatError.Create('unexpected end of format : '''+format+'''');
   try
    case format[formpos] of
     '%':begin
          CheckBlackChar('%');
          Inc(formpos);
          Inc(datapos);
         end;
     's':begin
          PString(args[result])^:=ReadStringData;
          Inc(formpos);
          Inc(result);
         end;
     'd':begin
          PInteger(args[result])^:=ReadIntegerData;
          Inc(formpos);
          Inc(result);
         end;
     'f':begin
          PFloat(args[result])^:=ReadExtendedData;
          Inc(formpos);
          Inc(result);
         end;
     '.':begin
          Inc(formpos);
          case ArrayPosStr(ReadTypeSpecifier, ['single', 'double', 'extended']) of
           0: PSingle(args[result])^:=ReadExtendedData;
           1: PDouble(args[result])^:=ReadExtendedData;
           2: PExtended(args[result])^:=ReadExtendedData;
          end;
          Inc(result);
         end;
     else raise EDeformatError.Create('incorrect format specifier after "%" sign : '''+format+'''');
    end;
   except
    on E: EConvertError do raise EDeformatError.Create('convert error - '+E.Message)
   end;
  end else

  begin
   CheckBlackChar(format[formpos]);
   Inc(datapos);
   Inc(formpos);
  end;
 end;

 if datapos <= Length(data) then
  raise EDeformatError.CreateFmt(
    'data ''%s'' too long - unexpected end of format ''%s''', [Data, Format]);
end;

procedure GetFileFilterExts(const FileFilter: string; var Extensions: TDynStringArray);
var p, SeekPos: integer;
    ExtsStr, filemask: string;
begin
 Extensions.SetLength(0);
 ExtsStr := GetFileFilterExtsStr(FileFilter);
 SeekPos := 1;
 repeat
  filemask := NextToken(ExtsStr, SeekPos,[';']);
  if filemask = '' then break;
  p := CharPos('.', filemask);
  if p > 0 then
   Delete(filemask, 1, p-1) else { delete name from filemask }
   filemask := '.'+filemask; { it means there was no name and dot in filemask. So prepend dot. }
  Extensions.IncLength;
  Extensions[Extensions.High] := filemask;
 until false;
end;

function GetFileFilterName(const FileFilter: string): string;
var ffLeft, ffRight: string;
    p, len: integer;
begin
 p := CharPos('|', FileFilter);
 if p = 0 then result := Trim(FileFilter) else
 begin
  ffLeft := Trim(Copy(FileFilter, 1, p-1));
  ffRight := Trim(SEnding(FileFilter, p+1));
  if ffRight = '' then
  begin
   result := ffLeft;
   { if FileFilter = 'xxx()|' then it matches to pattern 'xxx(exts)|exts'
     so we should return 'xxx', not 'xxx()'.
     This is often really useful when FileFilter was constructed in an
     automatic way (e.g. as in mine edytorek). }
   if IsSuffix('()', Result) then
   begin
    SetLength(Result, Length(Result)-2);
    { trim once again to delete rightmost whitespace (as in 'xxx ()|') }
    Result := TrimRight(Result);
   end;
  end else
  begin
   p := FindPos(ffRight, ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then
    p := FindPos(SReplaceChars(ffRight, ';', ','), ffLeft, 1, Length(ffLeft), [soBackwards]);
   if p = 0 then result := ffLeft else
   begin
    len := Length(ffRight);
    {zwieksz len tak zeby objelo biale znaki az do ')'}
    while p+len <= Length(ffLeft) do
    begin
     if ffLeft[p+len] = ')' then
      begin Inc(len); break end else
     if ffLeft[p+len] in WhiteSpaces then
      Inc(len) else
      break;
    end;
    {zmniejsz p tak zeby objelo biale znaki az do '('}
    while p-1 >= 1 do
    begin
     if ffLeft[p-1] = '(' then
      begin Dec(p); Inc(len); break end else
     if ffLeft[p-1] in WhiteSpaces then
      begin Dec(p); Inc(len) end else
      break;
    end;
    {koniec; wypieprz p, len}
    Delete(ffLeft, p, len);
    result := Trim(ffLeft);
   end;
  end;
 end;
end;

function GetFileFilterExtsStr(const FileFilter: string): string;
var p: integer;
begin
 p := CharPos('|', FileFilter);
 if p > 0 then
  result := SEnding(FileFilter, p+1) else
  result := '';
end;

function SReplacePatterns(const s: string;
  const patterns, values: array of string; Options: TSearchOptions): string;
var i, poz, minpoz, minind, sinresult: integer;
begin
 Result := '';

 Assert(High(patterns) = High(values));
 Assert(not (soBackwards in Options));
 sinresult := 0; { ile znakow z s zostalo juz przekopiowanych do result ? (lub, w przypadku
   wystapien pattern, ominietych) }

 repeat
  {licz najwczesniejsza pozycje patterns w pozostalej czesci s}
  minind := -1;
  minpoz := 0;
  for i := 0 to High(patterns) do
  begin
   poz := FindPos(patterns[i], s, sinresult+1, Length(s), Options);
   if (poz > 0) and ((minind = -1) or (poz < minpoz)) then
   begin
    minind := i;
    minpoz := poz;
   end;
  end;
  if minind = -1 then break; { wszystkie poz sa rowne 0, a wiec wszystko zamienione }

  {skopiuj do result wszystko z s przed wystapieniem pattern}
  result := result + CopyPos(s, sinresult+1, minpoz-1);
  sinresult := minpoz-1;
  {omin pattern[] w s, dolacz value[] do result}
  sinresult := sinresult + Length(patterns[minind]);
  result := result + values[minind];
 until false;

 result := result + SEnding(s, sinresult+1);
end;

function SCharsCount(const S: string; C: char): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[i] = C then Inc(Result);
end;

function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[i] in Chars then Inc(Result);
end;

function STruncateHash(const s: string): string;
var p: integer;
begin
 p := CharPos('#', s);
 result := s;
 if p > 0 then SetLength(result, p-1);
end;

function SUnformattable(const s: string): string;
begin
 result := StringReplace(s, '%', '%%', [rfReplaceAll]);
end;

function SAnsiCompare(const s1, s2: string; IgnoreCase: boolean): Integer;
begin
 if IgnoreCase then
  result := AnsiCompareText(s1, s2) else
  result := AnsiCompareStr(s1, s2);
end;

function SAnsiSame(const s1, s2: string; IgnoreCase: boolean): boolean;
begin
 result := SAnsiCompare(s1, s2, IgnoreCase) = 0;
end;

function SPercentReplace(const InitialFormat: string; const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean; PercentChar: char; IgnoreCase: boolean): string;

  function ReplaceWithC(c: char): Integer;
  var i: Integer;
  begin
   if IgnoreCase then
   begin
    for i := 0 to High(Replaces) do
     if AnsiSameText(c, Replaces[i].c) then begin result := i; Exit end;
   end else
   begin
    for i := 0 to High(Replaces) do
     if c = Replaces[i].c then begin result := i; Exit end;
   end;
   result := -1;
  end;

  procedure UnknownPercentFormat(const WrongSequence: string);
  begin
   raise EUnknownPercentFormat.Create('Unknown format pattern in format "'
     +InitialFormat+'", wrong sequence is : ' +WrongSequence);
  end;

var p, ReplNum: Integer;
    Format: string;
begin
 { Result zawiera czesciowy wynik. Od Format bedziemy odcinac zrobione juz kawalki.
   Bedziemy caly czas doklejac kolejne wyniki do Result (bedziemy starali sie,
   dla szybkosci, doklejac mozliwie duze kawalki do Result na raz, np. nie chcemy
   przepisywac do result po jednym znaku). }
 result := '';
 Format := InitialFormat;

 while Format <> '' do
 begin
  p := Pos(PercentChar, Format);
  if p = 0 then begin result := result + Format; Exit end;

  result := result + Copy(Format, 1, p-1);
  if p+1 <= Length(Format) then
  begin
   { zwieksz result o element wynikajacy z formatu Format[p+1] }
   if Format[p+1] = PercentChar then
    result := result + PercentChar else
   begin
    ReplNum := ReplaceWithC(Format[p+1]);
    if ReplNum = -1 then
    begin
     if ErrorOnUnknownPercentFormat then
      UnknownPercentFormat('"'+PercentChar+Format[p+1]+'"');
     result := result +PercentChar +Format[p+1];
    end else
     result := result +Replaces[ReplNum].s;
   end;
   { obietnij wykonana czesc z Format }
   Delete(Format, 1, p+1);
  end else
  begin
   { mamy PercentChar na koncu stringa }
   if ErrorOnUnknownPercentFormat then
    UnknownPercentFormat(PercentChar+' at the end of the format string');
   result := result + PercentChar;
   Exit;
  end;
 end;
end;

function AnsiUpperCaseChar(C: char): char;
begin
 Result :=
   {$ifdef WIN32} Chr( TPointerUInt( Windows.CharUpper(
     Windows.LPSTR(TPointerUInt(Ord(C))) ) ) )
   {$else} AnsiUpperCase(C)[1]
   {$endif};
end;

function AnsiLowerCaseChar(C: char): char;
begin
 Result :=
   {$ifdef WIN32} Chr( TPointerUInt( Windows.CharLower(
     Windows.LPSTR(TPointerUInt(Ord(C))) ) ) )
   {$else} AnsiLowerCase(C)[1]
   {$endif};
end;

function SAnsiUpperFirstChar(const S: string): string;
begin
 Result := S;
 if Result <> '' then
  Result[1] := AnsiUpperCaseChar(Result[1]);
end;

{ convertions ------------------------------------------------------------ }

function DigitAsChar(b: byte): char;
begin Result := char(b+byte('0')) end;

function DigitAsByte(c: char): byte;
begin Result := byte(c)-byte('0') end;

function IntToStrZPad(n: integer; minLength: integer): string;
begin result := SZeroPad(IntToStr(n), minLength) end;

function IntToStrPoz(n: Int64; poz: Byte): string;

  function TablZnakow(cyfra: Byte): char;
  { result := symbol cyfry 'cyfra'. Zawsze cyfra < poz }
  begin
   if cyfra < 10 then
    result := DigitAsChar(cyfra) else
    result := Chr( cyfra-10+Ord('A') ); {'A'=10 , 'B'=11 itd.}
  end;

var minus: boolean;
begin
 {Nasze symbole to 0..9, 'A' ..'Z'. Mamy wiec 10+'Z'-'A'+1 symboli na poz cyfr. }
 Assert(poz < 10+Ord('Z')-Ord('A')+1, 'za duzy arg poz w IntToStrPoz');
 if n = 0 then result := '0' else
 begin
  minus := n < 0;
  n := Abs(n);
  result := '';
  while n <> 0 do
  begin
   result := TablZnakow(n mod poz)+result;
   n := n div poz;
  end;
  if minus then result := '-'+result;
 end;
end;

function IntToStrPoz(n: Int64; poz: Byte; minLength: Cardinal): string;
{wywoluje IntToStrPoz, dodatkowo wypelniajac zerami z lewej, jesli trzeba}
begin
 result := IntToStrPoz(n, poz);
 if n < 0 then
  result := '-'+SZeroPad(SEnding(result, 2), minLength) else
  result := SZeroPad(result, minLength);
end;

function IntToStr2(n: Int64; MinLength: Cardinal; ZeroDigit, OneDigit,
  MinusSign: char): string;
var Negative: boolean;
    i: Integer;
begin
 { Simple implementation : Result := IntToStrPoz(n, 2, minLength) }

 { Negative := n < 0, n := Abs(n) }
 Negative := n < 0;
 if Negative then n := -n;

 Result := '';

 { from 0 .. SizeOf(n)*8-1 we have SizeOf(n)*8 values,
   all possible bits positions. So we're taking SizeOf(n)*8-2,
   to avoid most significant bit, the sign bit. }
 for i := SizeOf(n)*8-2 downto 0 do
  if ((Int64(1) shl i) and n) <> 0 then
   Result := Result + OneDigit else
  if Result <> '' then
   Result := Result + ZeroDigit;

 if Result = '' then Result := ZeroDigit;

 Result := SPad(Result, MinLength, ZeroDigit);

 if Negative then Result := MinusSign + Result;
end;

function IntToStr2(n: Int64; MinLength: Cardinal): string;
begin
 Result := IntToStr2(n, MinLength, '0', '1', '-');
end;

function IntToStr16(n: Int64; minLength: Cardinal): string;
begin result := IntToStrPoz(n, 16, minLength) end;

function IntToStrThousandSep(const Value: Int64): string;

  { Inserts ThousandSeparator to Result, where Result must be a sequence
    of digits (no '-' sign allowed !) }
  procedure InsertThousandSep;
  var i, SeparatorsCount, ResultPos, SeparatorPos: Integer;
      NewResult: string;
  begin
   if ThousandSeparator <> #0 then
   begin
    SeparatorsCount := (Length(Result)-1) div 3;

    { We already know the length of NewResult, so we set it now,
      this may we avoid many ReallocMems if length of NewResult would
      be changing. }
    SetLength(NewResult, Length(Result) + SeparatorsCount);

    { calculate initial SeparatorPos }
    SeparatorPos := Length(Result) mod 3;
    if SeparatorPos = 0 then SeparatorPos := 3;
    Inc(SeparatorPos);

    { calculate initial ResultPos }
    ResultPos := SeparatorPos;

    Move(Result[1], NewResult[1], SeparatorPos-1);

    for i := 1 to SeparatorsCount do
    begin
     NewResult[SeparatorPos] := ThousandSeparator;
     Move(Result[ResultPos], NewResult[SeparatorPos+1], 3);
     SeparatorPos := SeparatorPos + 4;
     ResultPos := ResultPos + 3;
    end;

    Result := NewResult;
   end;
  end;

begin
 if Value < 0 then
 begin
  Result := IntToStr(-Value);
  InsertThousandSep;
  Result := '-' + Result;
 end else
 begin
  Result := IntToStr(Value);
  InsertThousandSep;
 end;
end;

function Str2ToInt(const s: string): integer;
  function BinInt(c: char): integer;
  begin
   case c of
    '0': result := 0;
    '1': result := 1;
    else raise EConvertError.Create('Nieprawidlowy argument dla StrBinToInt : '+s);
   end;
  end;

var NextChar: integer;
begin
 if s = '' then
  raise EConvertError.Create('Argument StrBinToInt ma zerowa dlugosc.');
 if s[1] = '-' then
 begin
  if Length(s) = 1 then
   raise EConvertError.Create('StrBinToInt cannot convert ''-'' to int.');
  result := -BinInt(s[2]);
  NextChar := 3;
 end else
 begin
  result := BinInt(s[1]);
  NextChar := 2;
 end;
 while NextChar <= Length(s) do
 begin
  result := result*2+binInt(s[NextChar]);
  Inc(NextChar);
 end;
end;

function StrHexToInt(const s: string): Int64;
var ScanStart: integer;

  procedure Scan;
  var digit: Int64;
      i: integer;
  begin
   if ScanStart > Length(s) then
    raise EConvertError.Create('Unexpected end of string : no digits');
   result := 0;
   for i := ScanStart to Length(s) do
   begin
    case s[i] of
     '0'..'9':digit := Ord(s[i])-Ord('0');
     'a'..'f':digit := Ord(s[i])-Ord('a')+10;
     'A'..'F':digit := Ord(s[i])-Ord('A')+10;
     else raise EConvertError.Create('Character "'+s[i]+
       '" is not a hexadecimal digit');
    end;
    result := result*16 + digit;
   end;
  end;

begin
 if SCharIs(s, 1, '-') then
 begin
  ScanStart := 2;
  Scan;
  Result := -Result;
 end else
 begin
  if SCharIs(s, 1, '+') then ScanStart := 2 else ScanStart := 1;
  Scan;
 end;
end;

function ToStr(const Args: array of const): string;
var i: Integer;
begin
 Result := '';
 for i := 0 to High(Args) do
  Result := Result + VarRecToStr(Args[i]);
end;

function VarRecToStr(const v: TVarRec): string;
begin
 with v do
 case VType of
   vtInteger:    Result := IntToStr(VInteger);
   vtBoolean:    Result := BoolToStr[VBoolean];
   vtChar:       Result := VChar;
   vtExtended:   Result := FloatToStr(VExtended^);
   vtString:     Result := VString^;
   vtPChar:      Result := VPChar;
   vtWideChar:   Result := WideCharToString(@vWideChar);
   vtPWideChar:  Result := WideCharToString(vPWideChar);
   vtAnsiString: Result := AnsiString(vAnsiString);
   vtCurrency:   Result := CurrToStr(VCurrency^);
   vtVariant:    Result := VarToStr(VVariant^);
   vtPointer:    Result := 'Pointer('+IntToStr(TPointerUInt(vPointer))+')';
   vtObject:     Result := 'Object('+VObject.ClassName+')';
   vtClass:      Result := 'ClassRef('+VClass.ClassName+')';
   else raise Exception.CreateFmt('Wrong argument for VarRecToStr (v.vType = %d)', [vType]);
 end;
end;

function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;
var BSet: set of byte absolute SetVariable;
    i: byte;
begin
 result := '[';
 for i := 0 to NumEnd-NumStart do
  if i in BSet then
   if result = '[' then
    result := '['+IntToStr(i+NumStart) else
    result := result+','+IntToStr(i+NumStart);
 result := result+']';
end;

function StrToFloatDef(const s: string; DefValue: Extended): Extended;
begin
 try
  result := StrToFloat(s);
 except
  on EConvertError do result := DefValue
 end;
end;

function PCharOrNil(const s: string): PChar; begin if s = '' then result := nil else result := PChar(s); end;

{ describe chars ---------------------------------------- }

function DescribeKey(c: char): string;

  function DescribeCtrlKey(c: char): string;
  begin result := 'Ctrl+'+Chr(Ord(c)-1+Ord('a')) end;

begin
 case c of
  #0: Result := '#0';
  CharBackSpace: Result := 'BackSpace';
  CharTab: Result := 'Tab'; { ('+DescribeCtrlKey(c)+')'; }
  CharEnter: Result := 'Enter'; { ('+DescribeCtrlKey(c)+')'; }
  CharEscape: Result := 'Esc';
  ' ' : Result := 'Space';
  else
   { writing it in such way ensures that CharTab will NOT be shown
     as Ctrl+I, Enter as Ctrl+M etc. }
   if c in [CtrlA..CtrlZ] then
    Result := DescribeCtrlKey(c) else
    Result := c;
 end;
end;

end.
