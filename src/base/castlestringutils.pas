{
  Copyright 2000-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ String utilities.
  Also some operations on chars and PChars.
  And various conversions strings<->numbers.

  General comments for all procedures that have parameter like IgnoreCase:
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

unit CastleStringUtils;

interface

uses SysUtils, Classes, FGL,
  CastleUtils;

type
  TDynamicStringArray = array of string;

  { List of strings. This is a slightly extended version of standard TStringList.
    The default CaseSensitive value is @true. }
  TCastleStringList = class(TStringList)
  private
    procedure SetCount(const Value: Integer);
    function GetL(const Index: Integer): string;
    procedure SetL(const Index: Integer; const S: string);
  public
    constructor Create;
    property Count: Integer read GetCount write SetCount;
    { Add strings from Source list.
      Alias for AddStrings, useful for castlescriptarrays_implement.inc
      (since it's consistent with AddList in other lists). }
    procedure AddList(const Source: TStringList);
    procedure AddArray(const A: array of string);
    procedure AssignArray(const A: array of string);
    function Equal(List: TCastleStringList): boolean; overload;
    function Equal(const A: array of string): boolean; overload;

    { Reverse the order of items on the array. }
    procedure Reverse;

    { Access strings. This is exactly equivalent to just using standard
      TStringList.Strings property, and is useful only for implementing macros
      to work for both TGenericStructList and for TCastleStringList. }
    property L[Index: Integer]: string read GetL write SetL;

    function ToArray: TDynamicStringArray;
  end;

  { String-to-string map. Note that in simple cases you can also
    use standard TStringList functionality (see it's properties Names, Values),
    but this is better if your key/values may be multiline. }
  TStringStringMap = class(specialize TFPGMap<string, string>)
  public
    { Set given key value, trying to preserve previous key value too.
      This is useful for safely setting X3D META values.

      Compared to normal PutKeyData, this behaves smarter if given Name
      is already set. If it's set with the same Content, we do nothing.
      If the Content is different, we move previous content to a
      @code(Name + '-previous') key.
      This way previous content value is preserved once (but not more,
      to not grow the X3D file indefinitely). }
    procedure PutPreserve(const Name, Content: string);

    { Create another TStringStringMap with exactly the same contents at the beginning. }
    function CreateCopy: TStringStringMap;

    { Assign contents (all keys, values) of another TStringStringMap instance. }
    procedure Assign(const Source: TStringStringMap);
  end;

type
  { }
  TSearchOptions = set of (soMatchCase, soWholeWord, soBackwards);
  { A set of chars. }
  TSetOfChars = SysUtils.TSysCharSet;

const
  AllChars = [Low(Char) .. High(Char)];
  DefaultWordBorders = AllChars - ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  WhiteSpaces = [' ', #9, #10, #13];
  SimpleAsciiCharacters = [#32 .. #126];

function RandomString: string;

{ Replace all occurrences of FromPattern string to ToPattern string,
  within another string S.

  @code(StringReplaceAllVar(s, from, to)) is actually equivalent to
  simply @code(s := StringReplace(s, from, to, [rfReplaceAll, rfIgnoreCase])).
  So StringReplaceAllVar is just a wrapper for very common use case of
  StringReplace. }
procedure StringReplaceAllVar(var S: string;
  const FromPattern, ToPattern: string;
  IgnoreCase: boolean = true); overload;

{ Insert newline characters into string S, such that each line
  has at most MaxCol chars. Newline characters inserted is @link(NL).

  It tries to insert NL at the last character in OnBreakChars but still
  before MaxCol limit, and the character in OnBreakChars is deleted in this case.
  In other words, in most typical situation it simply breaks the string
  where the whitespace is, trying to make the line as long as possible within
  MaxCol limit. If no such character in OnBreakChars is found (e.g., you
  put a long line of non-white characters), it will still break the string
  at MaxCol position (so in this exceptional case, it will cause a break
  in the middle of the word).

  While breaking the string in the middle
  of the word in not nice, this allows us a safe feeling that this
  will always break the string into MaxCol chunks.

  This intelligently recognizes already
  existing newline characters (#13, #10, #13#10 or #10#13) in the string,
  so e.g. it will not insert more newline characters when they are not
  necessary. }
function BreakLine(const s: string; MaxCol: integer;
  onbreakChars: TSetOfChars = WhiteSpaces): string; overload;

{ Returns S with all chars in ExcludedChars deleted. }
function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;

{ Replace all occurrences of characters in FromChars with
  the new string / character. There are three overloaded versions:

  @orderedList(
    @item(SReplaceChars(string, string, string) looks in S for characters within
      FromChars, and replaces them with characters on appropriate position
      in ToChars. For example, SReplaceChars(S, 'ab', 'cd') replaces
      all occurrences of 'a' into 'c' and all occurrences of 'b' into 'd'.
      It must always be Length(FromChars) <= Length(ToChars).)

    @item(SReplaceChars(string, TSetOfChars, char) replaces all occurrences
      of any character in given set with the one specified character.)

    @item(SReplaceChars(string, char, char) simply replaces all occurrences
      of one character into another.))

  @groupBegin
}
function SReplaceChars(const s, FromChars, ToChars: string): string; overload;
function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string; overload;
function SReplaceChars(const s: string; FromChar, ToChar: char): string; overload;
{ @groupEnd }

{ Pad (fill from the left with character C) string S, until length
  of resulting string is at least Len.

  For example, @code(SPad('29', 4, '0')) gives '0029' }
function SPad(const s: string; len: integer; c: char = ' '): string; overload;

{ Pad (fill from the left)  with zeros string S, until length
  of resulting string is at least Len. It's actually just a shortcut for SPad
  with padding character set to '0'. }
function SZeroPad(const s: string; len: integer): string;

{ Convert uppercase letters to lowercase. Analogous to UpCase.
  Doesn't change other characters. Just like UpCase, this doesn't
  take current locale into account, and works only on English
  A-Z -> a-z letters. }
function LoCase(c: char): char;

function CharPos(c: char; const s: string; Offset: Integer = 1): integer;

{ Find first occurrence of any character in Chars in string S.
  This is quite like FirstDelimiter but it takes parameter as TSetOfChars
  and has much more sensible name.

  BackCharsPos does the same, but from
  the end of the string (i.e. finds the last occurrence).

  CharsPosEx searches starting from Offset char.

  They all return 0 if not found.

  @groupBegin }
function CharsPos(const chars: TSetOfChars; const s: string): integer;
function CharsPosEx(const chars: TSetOfChars; const s: string;
  Offset: Integer): integer;
function BackCharsPos(const chars: TSetOfChars; const s: string): integer;
{ @groupEnd }

{ Find @bold(last) occurrence of SubString within S.
  0 if not found. Overloaded version is optimized for searching for
  single character. }
function BackPos(const SubString, S: string): Integer; overload;
function BackPos(const SubString: char; const S: string): Integer; overload;

{ Find first occurrence of character in Delimiters. Name is analogous to
  LastDelimiter. Returns 0 if not found. }
function FirstDelimiter(const Delimiters, S: string): Integer;

{ Returns suffix of S starting from position P.
  Returns '' if P > length(S).
  Yes, this is simply equivalent to Copy(S, P, MaxInt). }
function SEnding(const s: string; P: integer): string;

function IsPrefix(const Prefix, S: string;
  IgnoreCase: boolean = true): boolean; overload;
function IsSuffix(const Suffix, S: string;
  IgnoreCase: boolean = true): boolean; overload;

{ Removes the prefix, if it is present. More precisely, if
  IsPrefix(Prefix, S, IgnoreCase) then returns S with this prefix
  removed. Else returns S. }
function PrefixRemove(const Prefix, S: string; IgnoreCase: boolean): string;

{ Like PrefixRemove, but checks for and removes Suffix. }
function SuffixRemove(const Suffix, S: string; IgnoreCase: boolean): string;

{ Appends to a string S DataSize bytes from Data. }
procedure SAppendData(var s: string; const Data; DataSize: integer);

{ A pointer to S[CharNum], that is just @@S[CharNum],
  avoiding range checking. }
function SChar(const s: string; CharNum: integer): PChar;

{ Check whether S[Index] = C, also checking is Index within S length.
  Return false if S is too short, or the chatacter differs.

  @groupBegin }
function SCharIs(const s: string; index: integer; c: char): boolean; overload;
function SCharIs(const s: string; index: integer; const chars: TSetOfChars): boolean; overload;
{ @groupEnd }

{ Replace typically unreadable characters in string S with #number notation.
  Useful for printing strings with some unprintable chars for
  debugging purposes. }
function SReadableForm(const s: string): string;
function SReadableForm(const C: char): string;

{ Return S[StartPosition..EndPosition].
  This is similar to standard Copy procedure,
  but last parameter is EndPosition instead of Count, which is more comfortable
  sometimes. }
function CopyPos(const s: string; StartPosition, EndPosition: integer): string;

{ Delete from S range of characters [StartPosition..EndPosition].
  Analogous to standard Delete but with EndPosition parameter (while
  standard Delete takes Count). }
procedure DeletePos(var S: string; StartPosition, EndPosition: Integer);

(*Find next part in the string S separated by delimiters
  TokenDelims. More precisely: search S, starting from position
  SeekPos, for the first character that is @italic(not in TokenDelims).
  Then, all subsequent characters that are not in TokenDelims are
  appended to the Result, until any character @italic(is in TokenDelims)
  is found. In effect, Result contains the whole part that was in TokenDelims.

  SeekPos is advanced to the position of the next character, i.e. the character
  right after the ending character that was in TokenDelims. In other words,
  SeekPos points to the position of the next "unprocessed" character in
  string S. Often you will want to make another call to NextToken, passing
  this SeekPos, and this way you can split your string S into parts
  delimited by TokenDelims.

  Returns '' if no more tokens available (SeekPos value at the end is
  unspecified).

  Typical use scenario (iterate over all tokens in the string) :

@longCode(#
  SeekPos := 1;
  repeat
    Token := NextToken(S, SeekPos);
    if Token = '' then break;
    { ... process_next_token (Token) ... }
  until false;
#)

  The above example will split the string into parts separated by whitespace.

  Note: it's much easier to use CreateTokens instead of this procedure.
  But this procedure gives you quite more flexibility. *)
function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;

{ NextTokenOnce works just like NextToken, but doesn't advance the SeekPos
  position. This means that it's quite useless when you're interested
  in @italic(all) tokens inside some string, but it's also more comfortable
  when you're interested in only @italic(one) token inside some string.
  When SeekPos = 1, this is the first token. }
function NextTokenOnce(const s: string; SeekPos: integer = 1;
  const TokenDelims: TSetOfChars = WhiteSpaces): string;
  overload;

{ Returns TCastleStringList with tokens extracted from S.
  Token is something delimited by TokenDelims.
  TokenDelims are not contained in resulting items.
  E.g. CreateTokens('foo, bar', [' ', ',']) returns TCastleStringList
  with 2 items: 'foo' and 'bar'. }
function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars = WhiteSpaces): TCastleStringList;

function GlueStrings(const Strings: array of string; const Delimiter: char): string;
function GlueStrings(const Strings: TStrings; const Delimiter: char): string;

{ Find substring SubText within Text. Returns 0 if not found.
  Similar to a standard Pos function, with some improvements.

  @param(StartPosition Starts searching for SubText starting from this position.
    Note that the resulting position is still returned with respect
    to the string beginning. Just like standard PosEx.)

  @param(Count Looks only at Count characters from Text.
    You can say that the search is done only within Copy(Text, StartPosition, Count).)

  @param(Options Various searching options:

    @unorderedList(
      @item(soMatchCase: makes searching case-sensitive (by default,
        case is ignored, taking locale into account).)

      @item(soWholeWord: looks only for SubText occurrences surrounded
        by characters from WordBorders (or the beginning/end of Text).

        Note that, while the beginning/end of Text is always treated like a word border,
        but the mere beginning/end of the searching range (StartPosition, Count)
        is not a word border.
        For example FindPos('cat', 'foocat dog', 4, MaxInt, [soWholeWord])
        will answer 0 (not found), because the only 'cat' occurrence is not
        surrounded by default word borders.)

      @item(soBackwards: search from the end, that is return rightmost
        found occurrence.)
    )
  ) }
function FindPos(const SubText, Text: string; StartPosition, Count: integer;
  const Options: TSearchOptions;
  const WordBorders: TSetOfChars = DefaultWordBorders): integer; overload;

{ Return rightmost RPart characters from S.
  If RPart > Length(S) then returns S. }
function SRight(const s: string; const rpart: integer): string;

{ If S = '' then returns NextPart, else returns S + PartSeparator + NextPart. }
function SAppendPart(const s, PartSeparator, NextPart: string): string;

{ Read file or URL contents to a string.
  MimeType is returned, calculated just like the @link(Download) function.
  If AllowStdIn, then URL = '-' (one dash) is treated specially:
  it means to read contents from standard input (stdin, Input in Pascal). }
function FileToString(const URL: string;
  const AllowStdIn: boolean; out MimeType: string): string;
function FileToString(const URL: string;
  const AllowStdIn: boolean = false): string;

procedure StringToFile(const URL, contents: string);

type
  EDeformatError = class(Exception);

{ Parse a string according to the given format, returning the
  values corresponding to placeholders %x in format string.

  Format parameter is a sequence of white spaces, placeholders like %d or %f,
  and other characters. More precisely:

  @unorderedList(
    @item(If RelaxedWhitespaceChecking = @true (that's the default value)
      then 1 or more white spaces in Format must correspond to 1 or more
      any whitespace characters in Data. I.e., the actual number and kind
      of whitespace in Format and Data doesn't have to match --- it's
      only important that @italic(some whitespace in Format) correspond
      to @italic(some whitespace in Data).)

    @item(@code(%d) in Format means an integer value (possibly signed) in Data.
      Args should have a pointer to Integer variable on the appropriate
      position.)

    @item(@code(%f) in Format means a float value (possibly signed, possibly
      with a dot) in Data. Args should have a pointer to Float variable
      on the appropriate position.)

    @item(@code(%.single.), @code(%.double.), @code(%.extended.) are like
      @code(%f), but they
      specify appropriate variable type in Args.
      Since DeFormat can't check the type validity of your pointers,
      always be sure to pass in Args pointers to appropriate types.)

    @item(@code(%s) in Format means a string (will end on the first whitespace)
      in Data. Args should contain a pointer to an AnsiString
      on the appropriate position. Note that I mean it --- a pointer
      to an AnsiString, not just a string typecasted into a pointer.
      I.e., if S is AnsiString, Args should contain @@S, not Pointer(S).

      Note that a string may be empty in some cases, e.g. Format = '%d %s'
      and Data = '123 ' will result in the empty string as second Args.)

    @item(@code(%%) in Format means a one % sign in Data.)

    @item(All the other characters (non-white, not %x sequences above)
      should be present in Data exactly like they are specified in Format.
      IgnoreCase controls is the letter case checked. When
      RelaxedWhitespaceChecking = @false then white-space characters
      are treated just like non-white chars: they must match exactly
      between Format and Data.)
  )

  Format must always match the whole Data --- in other words, when
  we finished reading the Format, Data should be finished too.
  The exception is at the beginning and end of Data, if
  RelaxedWhitespaceChecking = @true : then at the beginning and end of Data
  any number of white-space is allowed.

  For DeFormat, the opposite must also be true: when we finished reading
  Data, Format should be finished too. However, for TryDeFormat, it's
  allowed for Data to end prematurely. TryDeFormat returns how many Args
  were initialized.

  Note that while usually you will want RelaxedWhitespaceChecking = @true,
  sometimes it can be needed to set this to @false not only to get
  strickter checking, but also to get some things matching that otherwise
  wouldn't match. For example, consider Data = 'first  second apple'
  and Format = 'first %s second %s'. With RelaxedWhitespaceChecking
  these things @italic(do not match) --- because the 1st space character
  in the Format string "consumes" the 1st and 2nd space characters
  in the Data. Then '%s' is matched to the word 'second', and the
  word 'second' is compared with 'apple' and they do not match.
  If you want such Data and Format to match, you must pass
  RelaxedWhitespaceChecking = @true. Then the first '%s' will be matched
  to '' (empty string).

  This was written because both JclSscanf and scanf units were buggy.
  (see openGL.testy/nehe10).

  @raises(EDeformatError In case of any error --- mismatch between Format
    and Data. Note that in case of error, some of Args may be initialized,
    and some not --- no guarantees here, sorry.) }
procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean = true;
  const RelaxedWhitespaceChecking: boolean = true); overload;
function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean = true;
  const RelaxedWhitespaceChecking: boolean = true): integer; overload;

{ Extract file extensions from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: expects FileFilter to be in the form of
  @code('xxxx|name1.ext1;name2.ext2'). Where "xxxx" is just about anything
  (it is ignored), and in fact whole "xxxx|" (with bar) may be omitted.
  The rest (after "|") is treated as a filename list, separated by semicolon ";".

  As Extensions contents, we set an array of all extensions extracted from these
  filenames. For example above, we would set Extensions to array
  with two items: @code(['.ext1', '.ext2']). }
procedure GetFileFilterExts(const FileFilter: string; Extensions: TStringList);

{ Extract file filter name, from a file filter usually specified
  a TOpenDialog.Filter value.

  More precisely: if we do not see bar "|" character, then this is
  the filter name. Otherwise, everything on the right of "|" is "extensions"
  and everything on the left is "filter name".

  Additionally, if filter name ends with extensions value in parenthesis,
  they are removed. In other words, for 'Pascal files (*.pas)|*.pas',
  this will return just 'Pascal files'. The '(*.pas)' was removed
  from the filter name, because we detected this just repeats the extensions
  on the right of "|". Extensions on the right of "|" must be separated by
  semicolons, extensions within parenthesis on the left of "|" may
  be separated by semicolons ";" or colons ",". }
function GetFileFilterName(const FileFilter: string): string;

{ Search in FileFilter for the bar character "|", and return everything
  after it. This is a simple basis for GetFileFilterExts.

  If no "|" found, we return an empty string (in other words,
  file filter without "|" is treated as just a filter name, without
  any extensions). }
function GetFileFilterExtsStr(const FileFilter: string): string;

{ Replace all strings in Patterns with corresponding strings in Values.
  This is similar to standard StringReplace, but this does many
  replaces at once.

  Patterns and Values arrays must have equal length.
  Patterns[0] will be replaced with Values[0], Patterns[1] with Values[0] etc.
  Patterns are scanned from left to right, that is if two pattern occurrences
  overlap --- we will detect the leftmost one. If both patterns start
  at the same place (this means that one pattern is a prefix of the other),
  we will choose the first pattern in Patterns table.

  Using this avoids a common trap at repeated search-replace operations.
  A naive implementation of doing many search-replace over the same string
  is like

@longCode(#
  Result := S;
  Result := StringReplace(Result, Patterns[0], Values[0], [rfReplaceAll]);
  Result := StringReplace(Result, Patterns[1], Values[1], [rfReplaceAll]);
  etc.
#)

  But the above fails badly when inserting some Values[] creates
  an occurrence of Pattern checked later. For example, when Values[0]
  contains inside whole Patterns[1]. More exotic situations involve
  when some Values[] glues with previous string contents to make
  a pattern detected later. This means that you could replace the same
  content many times, which is usually not what you want.

  That's why you should instead use this function for such situations.

  Options cannot contain soBackwards flag. }
function SReplacePatterns(const s: string; const patterns, values: array of string; const Options: TSearchOptions): string;
function SReplacePatterns(const s: string; const patterns, values: TStrings; const Options: TSearchOptions): string;
function SReplacePatterns(const s: string; const Parameters: TStringStringMap; const Options: TSearchOptions): string;

function SCharsCount(const s: string; c: char): Cardinal; overload;
function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal; overload;

{ Remove from the string S everything after the first hash "#" character.
  Removes also this very "#" character.

  If string doesn't contain hash character, it's simply returned.

  Useful for interpreting simple text files when you want to treat
  things after "#" like a comment. }
function STruncateHash(const s: string): string;

{ Return the value to reproduce exactly string S by Format procedure.
  Saying simply, this doubles the "%" characters inside the string.
  The intention is to make such string that
  @code(Format(SUnformattable(S), []) = S). In other words, "quote"
  any suspicious "%" characters in S for Format. }
function SUnformattable(const s: string): string;

{ Compare strings, taking into account current locale.
  This simply does AnsiCompareStr or AnsiCompareText, depending on IgnoreCase.

  Returns value < 0 when S1 < S2, returns 0 when S1 = S2 and value > 0
  when S1 > S2. }
function SAnsiCompare(const s1, s2: string; IgnoreCase: boolean): Integer;

{ Check if strings are equal, taking into account current locale.
  Shortcut for SAnsiCompare(S1, S2) = 0 }
function SAnsiSame(const s1, s2: string; IgnoreCase: boolean): boolean;

type
  TPercentReplace = record
    { @noAutoLinkHere }
    c: char;
    { @noAutoLinkHere }
    s: string;
  end;

  EUnknownPercentFormat = class(Exception);

{ Searches for %x patterns and replaces them with specified strings.
  Something like a more generalized Format routine.

  More precisely: every two-char sequence that starts with PercentChar
  and then is followed by one of Replaces[I].c characters is replaced
  with appropriate Replaces[i].s. Moreover, a pair of two PercentChar
  characters is replaced with one PercentChar character.

  @italic(For example), assume that Replaces contains two items:
  @code((c: 'B'; s: '<bold>'), (c: 'b'; s: '</bold>')).
  Then @code(SPercentReplace('100%% of cats are %Bcute%b', Replaces)) will return
  string @code('100% of cats are <bold>cute</bold>').

  EUnknownPercentFormat is raised if we will see two-char sequence
  that starts with PercentChar and then is followed by character that
  is not any Replaces[i].c and is not PercentChar. Also, a single PercentChar
  at the end of the string is an error.

  @italic(For example), assume that Replaces contains the same two items as
  previously. Following calls will result in EUnknownPercentFormat being raised:
  @code(SPercentReplace('Unknown sequence %x', Replaces)),
  @code(SPercentReplace('Unterminated sequence %', Replaces)).

  If ErrorOnUnknownPercentFormat is @false, then EUnknownPercentFormat will
  not be raised. Instead, incorrect sequence (like %x or unterminated % in
  examples above) will simply be left in the string.

  Of course, replacing is done intelligently. Which means that
  e.g. sequence of four % characters will be correctly transformed into
  two % characters.

  Note that IgnoreCase is used to match characters for Replaces[I].c.
  IgnoreCase is not used when it comes to comparing with PercentChar character,
  i.e. even when PercentChar will be set to some letter, it will always
  be compared in case-sensitive manner, regardless of IgnoreCase value.

  It is undefined (meaning: don't do it) what happens if Replaces array
  contains more than once the same character C, or if any character C
  in Replaces array is equal to PercentChar.

  ReplacementsDone, if passed, will return how many replacements were done.
  Not counting "meaningless" replacements of pair of PercentChar to one
  PercentChar (that is, we count only actual replacements from Replaces
  array).

  @raises(EUnknownPercentFormat In case of error in InitialFormat string,
    if ErrorOnUnknownPercentFormat is @true.)

  @deprecated Do not use. Use standard StrUtils.StringsReplace instead.
  This procedure has no place in a game engine...

  @groupBegin }
function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  out ReplacementsDone: Cardinal;
  ErrorOnUnknownPercentFormat: boolean = true;
  PercentChar: char ='%';
  IgnoreCase: boolean = false): string; overload;

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean = true;
  PercentChar: char ='%';
  IgnoreCase: boolean = false): string; overload;
{ @groupEnd }

{ Replace sequences @code(@@counter(<padding>)) in the NamePattern with Index.
  Any sequence @code(@@counter(<padding>)) is detected (where <padding> is any
  integer >= 0) and replaced with Index padded with zeros (to given <padding>
  length).

  If AllowOldPercentSyntax is @true then we also allow older deprecated
  syntax: replace %d in the NamePattern with Index.
  This is used only if @code(@@counter(<padding>)) was not found in NamePattern.

  @unorderedList(
    @item(%d is replaced with Index.

      You can insert a non-negative number between % and d, to pad
      the counter with zeros to desired length. For example, with Counter = 2,
      %d is replaced with just "2", %2d is replaced with "02",
      %4d is replaced with "0002".)

    @item(%% is replaced with single percent char %.)

    @item(Everything else is just copied to resulting string.
      Not recognized %-patterns are also just copied.
      The main purpose of this is to specify filenames with optional
      placeholders, so unrecognized stuff should be gracefully ignored.)
  )

  The percent syntax was deprecated as it cannot be used with URLs.
  Inside URLs, percent character must always be encodede as @code(%25).
  Sequence like @code(%4d) must mean letter "M" (ASCII 77, which is 4d in
  hexadecimal) inside URL. We could potentially allow syntax like @code(%25d)
  or @code(%254d) (4-digit counter), but that's just ugly, and compatibility
  had to be broken anyway (after Castle Game Engine 4.0.1, you have to fix
  URLs to image sequences anyway, as @code(%4d) must mean letter "M").

  See http://castle-engine.sourceforge.net/x3d_extensions.php#section_ext_movie_from_image_sequence
  for an example when this is useful.

  @groupBegin }
function FormatNameCounter(const NamePattern: string;
  const Index: Integer; const AllowOldPercentSyntax: boolean;
  out ReplacementsDone: Cardinal): string;
function FormatNameCounter(const NamePattern: string;
  const Index: Integer; const AllowOldPercentSyntax: boolean): string;
{ @groupEnd }

{ conversions ------------------------------------------------------------ }

const
  { I should restrain from adding more similiar BoolToStrXxx constants
    below, since there are *so* many possibilities here (on/off, ON/OFF,
    On/Off, yes/no, YES/NO etc.) that it's quite useless trying to
    gather them all here. }

  { Convert boolean to string, using a simple table lookup.

    I don't use BoolToStr function from SysUtils unit,
    since there are differences in FPC implementations:
    @unorderedList(
      @item(In FPC <= 2.0.4, BoolToStr takes one param
        and returns 'FALSE' or 'TRUE' string.)
      @item(In FPC > 2.0.4 (trunk (2.3.1 currently), and fixes_2_2 (2,1.3)),
        BoolToStr was changed for Delphi compat. Now when passed only 1 param
        it returns 0 or -1 (who the hell needs such BoolToStr interpretation ?).

        You have to pass 2nd param to BoolToStr as @true
        to get strings 'False' and 'True'. But this makes it non-compileable
        in FPC <= 2.0.4. So to call BoolToStr like I want to, I would have
        to use ugly $ifdefs...))

    So I decided to use my BoolToStr table throughout my units.
    When I'll switch fully to FPC > 2.0.4, I'll drop this and use
    BoolToStr function from SysUtils unit. }
  BoolToStr: array[boolean] of string=('FALSE','TRUE');
  BoolToStrYesNo: array[boolean]of string = ('No','Yes');

{ Convert digit (like number 0) to character (like '0').
  Use only for arguments within 0..9 range. }
function DigitAsChar(b: byte): char;

{ Convert digit character (like '0') to a number (like 0).
  Use only for characters in '0'...'9' range. }
function DigitAsByte(c: char): byte;

{ Convert integer to string, padding string with zeros if needed. }
function IntToStrZPad(n: integer; minLength: integer): string;

{ Convert integer to string, in base-Base (like base-16) numeral system.
  For digits above '9', we will use upper letters 'A', 'B'...  etc.
  That's also why Base cannot be larger than 'Z'-'A' + 1 + 10
  (we would not have enough digits then).

  Overloaded versions with MinLength pad result with zeros to have
  at least MinLength.

  @groupBegin }
function IntToStrBase(const n: Int64; Base: Byte): string; overload;
function IntToStrBase(      n: QWord; Base: Byte): string; overload;
function IntToStrBase(const n: Int64; Base: Byte; minLength: Cardinal): string; overload;
function IntToStrBase(const n: QWord; Base: Byte; minLength: Cardinal): string; overload;
{ @groupEnd }

{ Convert integer to binary (base-2 numeral system).
  MinLength means to left-pad result with zeros if necessary. }
function IntToStr2(n: Int64;
  const MinLength: Cardinal = 1;
  const ZeroDigit: char = '0';
  const OneDigit: char = '1';
  const MinusSign: char = '-'): string; overload;

{ Convert integer to hexadecimal (base-16 numeral system).
  @groupBegin }
function IntToStr16(const n: Int64; const minLength: Cardinal = 1): string; overload;
function IntToStr16(const n: QWord; const minLength: Cardinal = 1): string; overload;
{ @groupEnd }

{ Returns Ptr as 0xXXX... hexadecimal value. "0x" is not a Pascal standard
  for coding hex values, but it's so popular that users are more likely
  to "get" 0x notation. }
function PointerToStr(Ptr: Pointer): string;

{ Convert string representing binary number to an integer.
  String must contain only '0', '1' (digits) and start with an optional sign
  (+ or -).
  @raises EConvertError On problems with conversion. }
function Str2ToInt(const s: string): integer;

{ Convert string with hexadecimal number to an integer.
  String must contain only digits (0-9, a-z, A-Z), and with an optional
  sign (+ or -).
  @raises EConvertError On problems with conversion. }
function StrHexToInt(const s: string): Int64;

function StrToFloatDef(const s: string; DefValue: Extended): Extended;

{ Convert a set to a string representation, in somewhat hacky way.
  This assumes that given SetVariable is a set value, and the set type
  is "set of [NumStart .. NumEnd]".

  @italic(Implementation is heavily dependent on how the sets are internally
  stored.)
  For now, we depend that a set of [NumStart .. NumEnd] behaves like a set
  of Byte, shifted to the left (i.e., NumStart corresponds to a 0 in set of Byte).
  This is not necessarily true ! For example in Delphi 5 (as far as I remember
  --- I don't have this Delphi now, and I don't remember on which Delphi
  version I observed this) set of 1..16 uses first three bytes, and
  the first bit (that would correspond to 0) is simply wasted. In fact,
  SizeOf such set is still 4, which means that internally sets eat 4 bytes anyway.
  But SizeOf set 200..216 is also 4, which means that the compiler is smart
  and doesn't waste too much space to store only 17 bits.

  This all is not a rant on internal set handling by Delphi. On the contrary,
  Delphi does it for speed reasons, and that's very good. This is just
  a warning that SetToStr is not really reliable, and you may need to experiment
  a little with NumStart / NumEnd values to get sensible results.
  Although if your set is like "set of [0 ... something]", this should usually
  work OK,

  Still: @italic(this function should be used only for debug purposes.
  Don't depend on it working 100% correctly always --- it can't, because we
  can't depend on how compiler stores sets.) }
function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;

function CharSetToStr(const SetVariable: TSetOfChars): string;

{ PCharOrNil simply returns a Pointer(S), you can think of it as a NO-OP.
  If string is empty, this returns @nil, otherwise it works just like
  PChar(S): returns a Pointer(S) with appropriate type cast. }
function PCharOrNil(const s: string): PChar;

{ Replace any number of consecutive whitespace (including newlines)
  with a single whitespace. This is nice when you have a string
  (possibly multiline) supplied by user, and you want to use this
  for some UI item (like window's caption or menu item) --- this
  "sanitizes" whitespace inside such string. }
function SCompressWhiteSpace(const S: string): string;

const
  { }
  CtrlA = Chr(Ord('a') - Ord('a') + 1); { = #1 } { }
  CtrlB = Chr(Ord('b') - Ord('a') + 1); { = #2 } { }
  CtrlC = Chr(Ord('c') - Ord('a') + 1); { ... etc. } { }
  CtrlD = Chr(Ord('d') - Ord('a') + 1);
  CtrlE = Chr(Ord('e') - Ord('a') + 1);
  CtrlF = Chr(Ord('f') - Ord('a') + 1);
  CtrlG = Chr(Ord('g') - Ord('a') + 1);
  CtrlH = Chr(Ord('h') - Ord('a') + 1); { = CharBackspace } { }
  CtrlI = Chr(Ord('i') - Ord('a') + 1); { = CharTab } { }
  CtrlJ = Chr(Ord('j') - Ord('a') + 1);
  CtrlK = Chr(Ord('k') - Ord('a') + 1);
  CtrlL = Chr(Ord('l') - Ord('a') + 1);
  CtrlM = Chr(Ord('m') - Ord('a') + 1); { = CharEnter } { }
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
  CtrlZ = Chr(Ord('z') - Ord('a') + 1); { = #26 } { }

  CharBackSpace = #8;
  CharTab = #9;
  CharEnter = #13;
  CharEscape = #27;
  CharDelete = #127;

implementation

uses CastleFilesUtils, CastleClassUtils, CastleDownload, Regexpr;

{ TCastleStringList ------------------------------------------------------------- }

constructor TCastleStringList.Create;
begin
  inherited;
  CaseSensitive := true;
end;

procedure TCastleStringList.SetCount(const Value: Integer);
var
  I: Integer;
begin
  { Use local variable I, instead of comparing Value = Count for,
    to possibly speed up a little (GetCount is virtual) }
  if Value < Count then
  begin
    for I := 1 to Count - Value do Delete(Count - 1);
  end else
  if Value > Count then
  begin
    for I := 1 to Value - Count do Add('');
  end;
end;

procedure TCastleStringList.AddList(const Source: TStringList);
begin
  AddStrings(Source);
end;

procedure TCastleStringList.AddArray(const A: array of string);
var
  I: Integer;
begin
  for I := 0 to High(A) do
    Add(A[I]);
end;

procedure TCastleStringList.AssignArray(const A: array of string);
begin
  Clear;
  AddArray(A);
end;

procedure TCastleStringList.Reverse;
var
  I: Integer;
begin
  { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
    which means that loop would try invalid Exchange(0, -1). }
  if Count = 0 then Exit;
  for I := 0 to (Count - 1) div 2 do
    Exchange(I, Count - 1 - I);
end;

function TCastleStringList.Equal(List: TCastleStringList): boolean;
var
  I: Integer;
begin
  if List.Count <> Count then Exit(false);
  for I := 0 to Count - 1 do
    if DoCompareText(List[I], Strings[I]) <> 0 then
      Exit(false);
  Result := true;
end;

function TCastleStringList.Equal(const A: array of string): boolean;
var
  I: Integer;
begin
  if High(A) <> Count - 1 then Exit(false);
  for I := 0 to Count - 1 do
    if DoCompareText(A[I], Strings[I]) <> 0 then
      Exit(false);
  Result := true;
end;

function TCastleStringList.GetL(const Index: Integer): string;
begin
  Result := Strings[Index];
end;

procedure TCastleStringList.SetL(const Index: Integer; const S: string);
begin
  Strings[Index] := S;
end;

function TCastleStringList.ToArray: TDynamicStringArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

{ TStringStringMap ----------------------------------------------------------- }

procedure TStringStringMap.PutPreserve(const Name, Content: string);
var
  PreviousContent: string;
  I: Integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
  begin
    PreviousContent := Data[I];
    if PreviousContent <> Content then
    begin
      { move current content to -previous name }
      KeyData[Name + '-previous'] := PreviousContent;
      { set new content }
      Data[I] := Content;
    end;
  end else
    KeyData[Name] := Content;
end;

function TStringStringMap.CreateCopy: TStringStringMap;
begin
  Result := TStringStringMap.Create;
  try
    Result.Assign(Self);
  except FreeAndNil(Result); raise end;
end;

procedure TStringStringMap.Assign(const Source: TStringStringMap);
var
  I: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    KeyData[Source.Keys[I]] := Source.Data[I];
end;

{ routines ------------------------------------------------------------------- }

function RandomString: string;
var i: integer;
begin
  result := '';
  for i := 1 to random(10) do result := result+char(byte('A')+Random(26));
  for i := 1 to 3 do result := result+char(byte('0')+Random(10));
end;

procedure StringReplaceAllVar(var S: string;
  const FromPattern, ToPattern: string;
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
    s := StringReplace(s, FromPattern, ToPattern, [rfReplaceAll, rfIgnoreCase]) else
    s := StringReplace(s, FromPattern, ToPattern, [rfReplaceAll]);
end;

function BreakLine(const s: string; MaxCol: integer; onbreakChars: TSetOfChars): string;
var
  done: integer;
  nowcol, i, brk: integer;
  BrokenSuccess: boolean;
const
  breakingstr = nl;
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
        BrokenSuccess := false;
        for brk := i downto Done + 1 do
          if s[brk] in OnBreakChars then
          begin
            Result := Result + CopyPos(s, Done+1, Brk-1) + BreakingStr;
            Done := brk; { we left the rest : s[brk+1..i] to be done }
            Break;
            BrokenSuccess := true;;
          end;
        if not BrokenSuccess then
        begin
          { ups ! it can't be broken - no onbreakChars found ! so we break after
            done+maxcol position. }
          Result := Result + Copy(s, Done+1, MaxCol) + BreakingStr;
          Done := Done + MaxCol;
        end;
      end;
    end;

    Inc(i);
  end;

  if Done < Length(S) then
    Result := Result + SEnding(S, Done+1);
end;

function SDeleteChars(const s: string; const excludedChars: TSetOfChars): string;
var
  i, j: integer;
begin
  SetLength(result, length(s));
  j := 1;
  for i := 1 to length(s) do
    if not (s[i] in excludedChars) then
      begin result[j] := s[i]; Inc(j); end;
  SetLength(result, j-1);
end;

function SReplaceChars(const s, FromChars, ToChars: string): string;
var
  i, p: integer;
begin
  Assert(Length(FromChars) = Length(ToChars));
  result := s;
  for i := 1 to Length(result) do
  begin
    p := CharPos(result[i], FromChars);
    if p > 0 then result[i] := ToChars[p];
  end;
end;

function SReplaceChars(const s: string; FromChars: TSetOfChars; ToChar: char): string;
var
  i: integer;
begin
  result := s;
  for i := 1 to Length(result) do
    if result[i] in FromChars then result[i] := ToChar;
end;

function SReplaceChars(const s: string; FromChar, ToChar: char): string;
var
  i: Integer;
begin
  Result := S;
  for i := 1 to Length(Result) do
    if Result[i] = FromChar then Result[i] := ToChar;
end;

function SPad(const s: string; len: integer; c: char): string;
var
  lnow: integer;
begin
  lnow := length(s);
  if lnow < len then
    Result := StringOfChar(c, len-lnow) + s else
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

function CharPos(c: char; const s: string; Offset: Integer): integer;
var
  i: integer;
begin
  for i := Offset to length(s) do
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

function BackPos(const SubString, S: string): integer;
begin
  for Result := Length(S) - Length(SubString) + 1 downto 1 do
    if SubString = Copy(S, Result, Length(SubString)) then Exit;
  Result := 0;
end;

function BackPos(const SubString: char; const S: string): Integer;
begin
  for Result := Length(S) downto 1 do
    if S[Result] = SubString then Exit;
  Result := 0;
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
    Result := AnsiCompareText(Copy(S, 1, Length(Prefix)), Prefix) = 0 else
    Result := AnsiCompareStr(Copy(S, 1, Length(Prefix)), Prefix) = 0;
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

function SReadableForm(const S: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result += SReadableForm(S[I]);
end;

function SReadableForm(const C: char): string;
begin
  if (Ord(C) < Ord(' ')) or (Ord(C) >= 128) then
    Result := '#'+IntToStr(Ord(C)) else
    Result := C;
end;

function CopyPos(const s: string; StartPosition, EndPosition: integer): string;
begin
 result := Copy(s, StartPosition, EndPosition - StartPosition + 1);
end;

procedure DeletePos(var S: string; StartPosition, EndPosition: Integer);
begin
 Delete(S, StartPosition, EndPosition - StartPosition + 1);
end;

function NextToken(const S: string; var SeekPos: Integer;
  const TokenDelims: TSetOfChars): string;
var
  TokStart: Integer;
begin
  repeat
    if SeekPos > Length(s) then begin Result := ''; Exit end;
    if S[SeekPos] in TokenDelims then Inc(SeekPos) else Break;
  until false;
  TokStart := SeekPos; { TokStart := first character not in TokenDelims }

  while (SeekPos <= Length(s)) and not(S[SeekPos] in TokenDelims) do Inc(SeekPos);

  { Calculate result := s[TokStart, ... , SeekPos-1] }
  result := Copy(s, TokStart, SeekPos-TokStart);

  { We don't have to do Inc(seekPos) below. But it's obvious that searching
    for next token can skip SeekPos, since we know S[SeekPos] is TokenDelim. }
  Inc(SeekPos);
end;

function NextTokenOnce(const s: string; SeekPos: integer;
  const TokenDelims: TSetOfChars): string;
begin
 result := Nexttoken(S, SeekPos, TokenDelims);
end;

function CreateTokens(const s: string;
  const TokenDelims: TSetOfChars): TCastleStringList;
var SeekPos: Integer;
    Token: string;
begin
 Result := TCastleStringList.Create;
 try
  SeekPos := 1;
  repeat
   Token := NextToken(s, SeekPos, TokenDelims);
   if Token = '' then break;
   Result.Add(Token);
  until false;
 except Result.Free; raise end;
end;

function GlueStrings(const Strings: array of string; const Delimiter: char): string;
var
  I: Integer;
begin
  if High(Strings) = -1 then
    Exit('');
  Result := Strings[0];
  for I := 1 to High(Strings) do
    Result += Delimiter + Strings[I];
end;

function GlueStrings(const Strings: TStrings; const Delimiter: char): string;
var
  I: Integer;
begin
  if Strings.Count = 0 then
    Exit('');
  Result := Strings[0];
  for I := 1 to Strings.Count - 1 do
    Result += Delimiter + Strings[I];
end;

function FindPos(const SubText, Text: string; StartPosition, Count: integer; const Options: TSearchOptions; const WordBorders: TSetOfChars): integer;
var S, SubS: string;

  function MatchingPos(i: integer): boolean;
  { sprawdz czy i jest dobra Position wystapienia SubS w S.
    Uwzglednij przy tym czy soWholeWord in Options, zachowuj sie zawsze
    jakby bylo soMatchCase in Options. }
  var realI: integer;
  begin
   result := false;
   if Copy(S, i, Length(SubS)) = SubS then
   begin
    if soWholeWord in Options then
    begin
     realI := i+StartPosition-1;
     if ( (realI = 1) or (Text[realI-1] in wordBorders) ) and
        ( (realI+length(subS)-1 = length(Text)) or (Text[realI+length(subS)] in WordBorders) )
     then result := true
    end else result := true;
   end;
  end;

var i: integer;
begin
 S := copy(Text, StartPosition, Count);
 SubS := SubText;
 if not (soMatchCase in Options) then
 begin
  S := AnsiUpperCase(S);
  SubS := AnsiUpperCase(SubS);
 end;
 result := 0;
 if soBackwards in Options then
 begin
  for i := Count-Length(SubS)+1 downto 1 do
   if MatchingPos(i) then begin result := i; break end;
 end else
 begin
  for i := 1 to Count-Length(SubS)+1 do
   if MatchingPos(i) then begin result := i; break end;
 end;
 if result > 0 then result := result+StartPosition-1;
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

function FileToString(const URL: string;
  const AllowStdIn: boolean; out MimeType: string): string;
var
  F: TStream;
begin
  if AllowStdIn and (URL = '-') then
  begin
    Result := ReadGrowingStreamToString(StdInStream);
    MimeType := '';
  end else
  begin
    F := Download(URL, [], MimeType);
    try
      { Some streams can be optimized, just load file straight to string memory }
      if (F is TFileStream) or
         (F is TMemoryStream) then
      begin
        SetLength(Result, F.Size);
        if F.Size <> 0 then
          F.ReadBuffer(Result[1], Length(Result));
      end else
        Result := ReadGrowingStreamToString(F);
    finally FreeAndNil(F) end;
  end;
end;

function FileToString(const URL: string; const AllowStdIn: boolean): string;
var
  MimeType: string;
begin
  Result := FileToString(URL, AllowStdIn, MimeType { ignored });
end;

procedure StringToFile(const URL, Contents: string);
var
  F: TStream;
begin
  F := URLSaveStream(URL);
  try
    if Length(Contents) <> 0 then
      F.WriteBuffer(Contents[1], Length(Contents));
  finally FreeAndNil(F) end;
end;

procedure DeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean;
  const RelaxedWhitespaceChecking: boolean);
begin
 if TryDeFormat(Data, Format, args, IgnoreCase,
   RelaxedWhitespaceChecking) < High(args)+1 then
  raise EDeformatError.CreateFmt(
    'Unexpected end of Data (%s) - format (%s) not fully evaluated',
    [Data, Format]);
end;

function TryDeFormat(Data: string; const Format: string;
  const args: array of pointer;
  const IgnoreCase: boolean;
  const RelaxedWhitespaceChecking: boolean): integer;
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

  procedure CheckFormatNotEnd;
  begin
    if formpos > Length(format) then
      raise EDeformatError.Create('Unexpected end of format : "'+format+'"');
  end;

begin
 datapos := 1;
 formpos := 1;
 result := 0; { no args done yet }

 { Skip whitespace and the beginning of data }
 if RelaxedWhitespaceChecking then
   while SCharIs(Data, DataPos, WhiteSpaces) do Inc(DataPos);

 while formpos <= Length(Format) do
 begin
  {datapos > Length(data) -> means Data has ended but Format not.
   OK, so we can exit, because we are doing only TryDeFormat.
   Real DeFormat should check our result if it wishes to check that we parsed
   whole Format.}
  if datapos > Length(data) then
  begin
    { Actually, if next thing in format is %s, we can parse it too
      (string will just be '') }
    if Format[FormPos] = '%' then
    begin
      Inc(formpos);
      CheckFormatNotEnd;
      if Format[FormPos] = 's' then
      begin
        PString(args[result])^ := ReadStringData;
        Inc(formpos);
        Inc(result);
      end;
    end;
    Exit;
  end;

  {1 or more whitespace in format means 1 or more whitespaces in data}
  if RelaxedWhitespaceChecking and (format[formpos] in WhiteSpaces) then
  begin
   if not SCharIs(Data, datapos, WhiteSpaces) then
    raise EDeformatError.Create('Whitespace not found in data "' + data +
      '" as requested by format "' + format + '"');
   repeat Inc(formpos) until not SCharIs(format, formpos, WhiteSpaces);
   repeat Inc(datapos) until not SCharIs(data, datapos, WhiteSpaces);
  end else

  {%+something means "read this from data", %% means "read %"}
  if format[formpos] = '%' then
  begin
   Inc(formpos);
   CheckFormatNotEnd;
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

 if RelaxedWhitespaceChecking then
   while SCharIs(Data, DataPos, WhiteSpaces) do Inc(DataPos);

 if datapos <= Length(data) then
  raise EDeformatError.CreateFmt(
    'data ''%s'' too long - unexpected end of format ''%s''', [Data, Format]);
end;

procedure GetFileFilterExts(const FileFilter: string; Extensions: TStringList);
var p, SeekPos: integer;
    ExtsStr, filemask: string;
begin
 Extensions.Clear;
 ExtsStr := GetFileFilterExtsStr(FileFilter);
 SeekPos := 1;
 repeat
  filemask := NextToken(ExtsStr, SeekPos,[';']);
  if filemask = '' then break;
  p := CharPos('.', filemask);
  if p > 0 then
   Delete(filemask, 1, p-1) else { delete name from filemask }
   filemask := '.'+filemask; { it means there was no name and dot in filemask. So prepend dot. }
  Extensions.Add(filemask);
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

function SReplacePatterns(const S: string;
  const Patterns, Values: array of string; const Options: TSearchOptions): string;
var
  PatternsList, ValuesList: TCastleStringList;
begin
  PatternsList := nil;
  ValuesList := nil;
  try
    PatternsList := TCastleStringList.Create;
    PatternsList.AssignArray(Patterns);
    ValuesList := TCastleStringList.Create;
    ValuesList.AssignArray(Values);
    Result := SReplacePatterns(S, PatternsList, ValuesList, Options);
  finally
    FreeAndNil(PatternsList);
    FreeAndNil(ValuesList);
  end;
end;

function SReplacePatterns(const s: string; const Parameters: TStringStringMap;
  const Options: TSearchOptions): string;
var
  PatternsList, ValuesList: TCastleStringList;
  I: Integer;
begin
  PatternsList := nil;
  ValuesList := nil;
  try
    PatternsList := TCastleStringList.Create;
    ValuesList := TCastleStringList.Create;
    for I := 0 to Parameters.Count - 1 do
    begin
      PatternsList.Add(Parameters.Keys[I]);
      ValuesList.Add(Parameters.Data[I]);
    end;
    Result := SReplacePatterns(S, PatternsList, ValuesList, Options);
  finally
    FreeAndNil(PatternsList);
    FreeAndNil(ValuesList);
  end;
end;

function SReplacePatterns(const S: string;
  const Patterns, Values: TStrings; const Options: TSearchOptions): string;
var
  i, poz, minpoz, minind, Processed: integer;
begin
  Result := '';

  Assert(Patterns.Count = Values.Count);
  Assert(not (soBackwards in Options));
  Processed := 0; { how many characters from S was already processed, and are in
    Result? }

  repeat
    { calculate earliest occurence of some pattern within the remaining of S }
    minind := -1;
    minpoz := 0;
    for i := 0 to Patterns.Count - 1 do
    begin
      poz := FindPos(patterns[i], s, Processed+1, Length(s), Options);
      if (poz > 0) and ((minind = -1) or (poz < minpoz)) then
      begin
        minind := i;
        minpoz := poz;
      end;
    end;
    if minind = -1 then break; { all poz = 0, so everything is already done }

    { copy to Result everything from S before a pattern occurence }
    result := result + CopyPos(s, Processed+1, minpoz-1);
    Processed := minpoz-1;
    { omit pattern[] in S, append corresponding value[] to Result }
    Processed := Processed + Length(patterns[minind]);
    result := result + values[minind];
  until false;

  result := result + SEnding(s, Processed+1);
end;

function SCharsCount(const S: string; C: char): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[I] = C then Inc(Result);
end;

function SCharsCount(const s: string; const Chars: TSetOfChars): Cardinal;
var i: Integer;
begin
 Result := 0;
 for I := 1 to Length(s) do if S[I] in Chars then Inc(Result);
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

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  out ReplacementsDone: Cardinal;
  ErrorOnUnknownPercentFormat: boolean;
  PercentChar: char;
  IgnoreCase: boolean): string;

  function ReplaceWithC(c: char): Integer;
  var
    I: Integer;
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

var
  P, ReplNum: Integer;
  Format: string;
begin
  { Result zawiera czesciowy wynik. Od Format bedziemy odcinac zrobione juz kawalki.
    Bedziemy caly czas doklejac kolejne wyniki do Result (bedziemy starali sie,
    dla szybkosci, doklejac mozliwie duze kawalki do Result na raz, np. nie chcemy
    przepisywac do Result po jednym znaku). }
  Result := '';
  Format := InitialFormat;
  ReplacementsDone := 0;

  while Format <> '' do
  begin
    P := Pos(PercentChar, Format);
    if P = 0 then begin Result := Result + Format; Exit end;

    Result := Result + Copy(Format, 1, P - 1);
    if P + 1 <= Length(Format) then
    begin
      { zwieksz Result o element wynikajacy z formatu Format[p+1] }
      if Format[P + 1] = PercentChar then
        Result := Result + PercentChar else
      begin
        ReplNum := ReplaceWithC(Format[P + 1]);
        if ReplNum = -1 then
        begin
          if ErrorOnUnknownPercentFormat then
            UnknownPercentFormat('"'+PercentChar+Format[P + 1]+'"');
          Result := Result + PercentChar + Format[P + 1];
        end else
        begin
          Result := Result + Replaces[ReplNum].s;
          Inc(ReplacementsDone);
        end;
      end;
      { obetnij wykonana czesc z Format }
      Delete(Format, 1, P + 1);
    end else
    begin
      { mamy PercentChar na koncu stringa }
      if ErrorOnUnknownPercentFormat then
       UnknownPercentFormat(PercentChar+' at the end of the format string');
      Result := Result + PercentChar;
      Exit;
    end;
  end;
end;

function SPercentReplace(const InitialFormat: string;
  const Replaces: array of TPercentReplace;
  ErrorOnUnknownPercentFormat: boolean;
  PercentChar: char;
  IgnoreCase: boolean): string;
var
  ReplacementsDone: Cardinal;
begin
  Result := SPercentReplace(InitialFormat, Replaces, ReplacementsDone,
    ErrorOnUnknownPercentFormat, PercentChar, IgnoreCase);
  { returned ReplacementsDone will simply be ignored }
end;

function FormatIndexedName(const NamePattern: string;
  const Index: Integer; out ReplacementsDone: Cardinal): string;
const
  PercentChar = '%';
var
  StartP, P, MinLength: Integer;
  Format: string;
begin
  { Result zawiera czesciowy wynik. Od Format bedziemy odcinac zrobione juz kawalki.
    Bedziemy caly czas doklejac kolejne wyniki do Result (bedziemy starali sie,
    dla szybkosci, doklejac mozliwie duze kawalki do Result na raz, np. nie chcemy
    przepisywac do Result po jednym znaku). }
  Result := '';
  Format := NamePattern;
  ReplacementsDone := 0;

  while Format <> '' do
  begin
    P := Pos(PercentChar, Format);
    if P = 0 then begin Result := Result + Format; Exit end;

    Result := Result + Copy(Format, 1, P - 1);
    if P + 1 <= Length(Format) then
    begin
      { zwieksz Result o element wynikajacy z formatu Format[P + 1] }
      if Format[P + 1] = PercentChar then
        Result := Result + PercentChar else
      if Format[P + 1] = 'd' then
      begin
        Result := Result + IntToStr(Index);
        Inc(ReplacementsDone);
      end else
      if Format[P + 1] in ['0'..'9'] then
      begin
        Inc(P);
        StartP := P;
        while SCharIs(Format, P, ['0'..'9']) do Inc(P);
        if SCharIs(Format, P, 'd') then
        begin
          { valid % + number + d sequence, do the replace }
          MinLength := StrToInt(Copy(Format, StartP, P - StartP));
          Result := Result + IntToStrZPad(Index, MinLength);
          Inc(ReplacementsDone);
        end else
        begin
          { invalid %-pattern, just copy it (including leading PercentChar
            and following character <> 'd') }
          Result := Result + Copy(Format, StartP - 1, P - StartP + 2);
        end;
        { decrement P just so that Delete(Format, ...) below will work Ok }
        Dec(P);
      end else
      begin
        { unknown %-pattern, just copy it }
        Result := Result + PercentChar + Format[P + 1];
      end;
      { obetnij wykonana czesc z Format }
      Delete(Format, 1, P + 1);
    end else
    begin
      { mamy PercentChar na koncu stringa }
      Result := Result + PercentChar;
      Exit;
    end;
  end;
end;

type
  TRegExprCounter = class
  private
    Index: Integer;
    ReplacementsDone: Cardinal;
    function ReplaceCallback(ARegExpr : TRegExpr): string;
  end;

function TRegExprCounter.ReplaceCallback(ARegExpr : TRegExpr): string;
begin
  Result := IntToStrZPad(Index, StrToInt(ARegExpr.Match[1]));
  Inc(ReplacementsDone);
end;

function FormatNameCounter(const NamePattern: string;
  const Index: Integer; const AllowOldPercentSyntax: boolean;
  out ReplacementsDone: Cardinal): string;
var
  R: TRegExpr;
  C: TRegExprCounter;
begin
  R := TRegExpr.Create;
  try
    R.Expression := '@counter\(([\d]+)\)';
    C := TRegExprCounter.Create;
    try
      C.Index := Index;
      Result := R.Replace(NamePattern, @C.ReplaceCallback);
      ReplacementsDone := C.ReplacementsDone;
    finally FreeAndNil(C) end;
  finally FreeAndNil(R) end;

  if (ReplacementsDone = 0) and AllowOldPercentSyntax then
    Result := FormatIndexedName(NamePattern, Index, ReplacementsDone);
end;

function FormatNameCounter(const NamePattern: string;
  const Index: Integer; const AllowOldPercentSyntax: boolean): string;
var
  ReplacementsDone: Cardinal;
begin
  Result := FormatNameCounter(NamePattern, Index, AllowOldPercentSyntax,
    ReplacementsDone);
end;

{ conversions ------------------------------------------------------------ }

function DigitAsChar(b: byte): char;
begin Result := char(b+byte('0')) end;

function DigitAsByte(c: char): byte;
begin Result := byte(c)-byte('0') end;

function IntToStrZPad(n: integer; minLength: integer): string;
begin result := SZeroPad(IntToStr(n), minLength) end;

function IntToStrBase(n: QWord; Base: Byte): string;

  function TablZnakow(cyfra: Byte): char;
  { result := symbol cyfry 'cyfra'. Zawsze cyfra < Base }
  begin
   if cyfra < 10 then
    result := DigitAsChar(cyfra) else
    result := Chr( cyfra-10+Ord('A') ); {'A'=10 , 'B'=11 itd.}
  end;

begin
 {Nasze symbole to 0..9, 'A' ..'Z'. Mamy wiec 10+'Z'-'A'+1 symboli na Base cyfr. }
 Assert(Base < 10+Ord('Z')-Ord('A')+1, 'too large Base in IntToStrBase');
 if n = 0 then result := '0' else
 begin
  result := '';
  while n <> 0 do
  begin
   result := TablZnakow(n mod Base)+result;
   n := n div Base;
  end;
 end;
end;

function IntToStrBase(const n: Int64; Base: Byte): string;
begin
  if N < 0 then
    Result := '-' + IntToStrBase(QWord(Abs(N)), Base) else
    Result := IntToStrBase(QWord(N), Base);
end;

function IntToStrBase(const n: Int64; Base: Byte; minLength: Cardinal): string;
{wywoluje IntToStrBase, dodatkowo wypelniajac zerami z lewej, jesli trzeba}
begin
 result := IntToStrBase(n, Base);
 if n < 0 then
  result := '-'+SZeroPad(SEnding(result, 2), minLength) else
  result := SZeroPad(result, minLength);
end;

function IntToStrBase(const n: QWord; Base: Byte; minLength: Cardinal): string;
{wywoluje IntToStrBase, dodatkowo wypelniajac zerami z lewej, jesli trzeba}
begin
 result := IntToStrBase(n, Base);
 result := SZeroPad(result, minLength);
end;

function IntToStr2(n: Int64;
  const MinLength: Cardinal;
  const ZeroDigit: char;
  const OneDigit: char;
  const MinusSign: char): string;
var Negative: boolean;
    i: Integer;
begin
 { Simple implementation : Result := IntToStrBase(n, 2, minLength) }

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

function IntToStr16(const n: Int64; const minLength: Cardinal): string;
begin result := IntToStrBase(n, 16, minLength) end;

function IntToStr16(const n: QWord; const minLength: Cardinal): string;
begin result := IntToStrBase(n, 16, minLength) end;

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
  raise EConvertError.Create('Argument StrBinToInt ma zerowa length.');
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
    case S[I] of
     '0'..'9':digit := Ord(S[I])-Ord('0');
     'a'..'f':digit := Ord(S[I])-Ord('a')+10;
     'A'..'F':digit := Ord(S[I])-Ord('A')+10;
     else raise EConvertError.Create('Character "'+S[I]+
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

function PointerToStr(Ptr: Pointer): string;
begin
  Result := '0x' + IntToStr16(PtrUInt(Ptr),
    {$ifdef CPU32} 8 {$endif}
    {$ifdef CPU64} 16 {$endif} );
end;

function SetToStr(const SetVariable; NumStart, NumEnd: byte): string;
var
  BSet: set of byte absolute SetVariable;
  i: byte;
begin
  Result := '[';
  for i := 0 to NumEnd-NumStart do
    if i in BSet then
      if Result = '[' then
        Result := '[' + IntToStr(i + NumStart) else
        Result := Result + ',' + IntToStr(i + NumStart);
  Result := Result + ']';
end;

function CharSetToStr(const SetVariable: TSetOfChars): string;
var
  C: char;
begin
  Result := '[';
  for C := Low(C) to High(C) do
    if C in SetVariable then
      if Result = '[' then
        Result := '[' + SReadableForm(C) else
        Result := Result + ',' + SReadableForm(C);
  Result := Result + ']';
end;

function StrToFloatDef(const s: string; DefValue: Extended): Extended;
begin
 try
  result := StrToFloat(s);
 except
  on EConvertError do result := DefValue
 end;
end;

function PCharOrNil(const s: string): PChar;
begin if s = '' then result := nil else result := PChar(s); end;

function SCompressWhiteSpace(const S: string): string;
var
  ResultPos: Integer; { this is always next free result position }
  SPos: Integer; { this is always next unhandled S position }
  NextSPos: Integer;
begin
  ResultPos := 1;
  SPos := 1;
  SetLength(Result, Length(S)); { resulting string is at most as long as S }

  if SCharIs(S, 1, WhiteSpaces) then
  begin
    Result[1] := ' ';
    Inc(ResultPos);
    while SCharIs(S, SPos, WhiteSpaces) do Inc(SPos);
  end;

  while SPos <= Length(S) do
  begin
    Assert(not (S[SPos] in WhiteSpaces));

    { read next non-white-space chunk }

    NextSPos := SPos + 1;
    while (NextSPos <= Length(S)) and
          not (S[NextSPos] in WhiteSpaces) do
      Inc(NextSPos);

    Move(S[SPos], Result[ResultPos], NextSPos - SPos);

    ResultPos += NextSPos - SPos;
    SPos := NextSPos;

    { omit next white-space chunk }

    if SCharIs(S, SPos, WhiteSpaces) then
    begin
      Result[ResultPos] := ' ';
      Inc(ResultPos);
      while SCharIs(S, SPos, WhiteSpaces) do Inc(SPos);
    end;
  end;

  { assert we didn't do buffer overflow just now }
  Assert(ResultPos - 1 <= Length(Result));

  SetLength(Result, ResultPos - 1);
end;

end.
