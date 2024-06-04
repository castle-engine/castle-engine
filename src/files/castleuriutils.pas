{
  Copyright 2007-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ URI utilities. These extend standard FPC URIParser unit. }
unit CastleUriUtils;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleStringUtils;

{ Extracts #anchor from URI. On input, URI contains full URI.
  On output, Anchor is removed from URI and saved in Anchor.
  If no #anchor existed, Anchor is set to ''. }
procedure UriExtractAnchor(var Uri: String; out Anchor: string);

{ Like UriExtractAnchor, but URI remains unchanged. }
procedure UriGetAnchor(const Uri: String; out Anchor: string);

{ Calculate #anchor from an URI, and split it into a key-value map.

  This supports special CGE syntax within URL anchor to specify loading parameters for
  @url(https://castle-engine.io/spine Spine),
  @url(https://castle-engine.io/sprite_sheets sprite sheets),
  @url(https://castle-engine.io/using_images images).

  On output, the key-value pairs from anchor are saved in TStringStringMap.
  The SettingsFromAnchor is always cleared at the beginning.
  If no anchor existed, SettingsFromAnchor will be empty when this ends. }
procedure UriGetSettingsFromAnchor(const Uri: String;
  const SettingsFromAnchor: TStringStringMap);

{ Return URI with anchor (if was any) stripped. }
function UriDeleteAnchor(const Uri: String): string;

{ Replace all sequences like %xx with their actual 8-bit characters.

  The intention is that this is similar to PHP function with the same name.

  To account for badly encoded strings, invalid encoded URIs do not
  raise an error --- they are only reported to WritelnWarning.
  So you can simply ignore them, or write a warning about them for user.
  This is done because often you will use this with
  URIs provided by the user, read from some file etc., so you can't be sure
  whether they are correctly encoded, and raising error unconditionally
  is not OK. (Considering the number of bad HTML pages on WWW.)

  The cases of badly encoded strings are:

  @unorderedList(
    @item("%xx" sequence ends unexpectedly at the end of the string.
      That is, string ends with "%" or "%x". In this case we simply
      keep "%" or "%x" in resulting string.)

    @item("xx" in "%xx" sequence is not a valid hexadecimal number.
      In this case we also simply keep "%xx" in resulting string.)
  )
}
function RawUriDecode(const S: string): string;

{ Get protocol from given URI.

  This is very similar to how UriParser.ParseUri function detects the protocol,
  although not 100% compatible:

  @unorderedList(
    @item(We allow whitespace (including newline) before protocol name.

      This is useful, because some VRML/X3D files have the ECMAScript code
      inlined and there is sometimes whitespace before "ecmascript:" protocol.)

    @item(We never detect a single-letter protocol name.

      This is useful, because we do not use any single-letter protocol name,
      and it allows to detect Windows absolute filenames like
      @code(c:\blah.txt) as filenames. Otherwise, Windows absolute filenames
      could not be accepted by any of our routines that work with URLs
      (like the @link(Download) function),
      since they would be detected as URLs with unknown protocol "c".

      Our UriProtocol will answer that protocol is empty for @code(c:\blah.txt).
      Which means no protocol, so our engine will treat it as a filename.
      (In contrast with UriParser.ParseUri that would detect protocol called "c".)
      See doc/uri_filename.txt in sources for more comments about differentiating
      Uri and filenames in our engine.)

    @item(We always return lowercase protocol. This is comfortable,
      since you almost always calculate protocol to compare it,
      and protocol names are not case-sensitive,
      and you should always produce URLs with lowercase protocol names
      (see http://tools.ietf.org/html/rfc3986#section-3.1).)
  )
}
function UriProtocol(const Uri: String): string;

{ Check does URI contain given Protocol.
  This is equivalent to checking UriProtocol(S) = Protocol, ignoring case,
  although may be a little faster. Given Protocol string cannot contain
  ":" character. }
function UriProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;

{ Remove the protocol part from URI. }
function UriDeleteProtocol(const S: string): string;
  deprecated 'use ParseUri to extract Uri.Path + Uri.Document, instead of this routine that doesn''t do decoding';

{ Is the S a valid protocol scheme.

  Following https://datatracker.ietf.org/doc/html/rfc3986 ,
  protocol scheme must

  @unorderedList(
    @item(begin with an (ASCII) letter)

    @item(and be followed by any combination of (ASCII) letters, digits, plus ("+"), period ("."), or hyphen ("-").)
  )
}
function UriValidProtocol(const P: String): Boolean;

{ Return absolute URI, given base and relative URI.

  Base URI must be either an absolute (with protocol) URI, or only
  an absolute filename (in which case we'll convert it to file:// URI under
  the hood, if necessary). This is usually the URI of the containing file,
  for example an HTML file referencing the image, processed by AbsoluteURI.

  Relative URI may be a relative URI or an absolute URI.
  In the former case it is merged with Base.
  In the latter case it is simply returned.

  If you want to support relative URIs, you want to use this routine.
  It treats Relative always as an URI (so it should be percent-escaped,
  with slashes and such). Other routines in our engine,
  like AbsoluteUri and @link(Download), treat strings without protocol
  as a filename (so it's not percent-escaped, it uses PathDelim
  specific to OS --- slash or backslash etc.).
  This routine, on the other hand, treats Relative string always as an
  URI (when it doesn't include protocol, it just means it's relative to Base). }
function CombineUri(const Base, Relative: string): string;

{ Make sure that the URI is absolute (always has a protocol).
  This function treats an URI without a protocol as a simple filename
  (absolute or relative to the current directory).
  This includes treating empty string as equivalent to current directory. }
function AbsoluteUri(const Uri: String): string;

{ Does URI contain only an absolute filename.
  Useful to detect unwanted paths in data files,
  you usually do not want to have such paths in data files,
  as they make it impossible to transfer the data (move/copy files)
  to other system/location. }
function AbsoluteFileUri(const Uri: String): boolean;

{ Convert URI (or filename) to a filename.

  This is an improved UriToFilename from UriParser.
  When URI is already a filename, this does a better job than UriToFilename,
  as it handles also Windows absolute filenames (see UriProtocol).
  Returns empty string in case of problems, for example when this is not
  a file URI.

  Just like UriParser.UriToFilename, this percent-decodes the parameter.
  For example, @code(%4d) in URI will turn into letter @code(M) in result.

  It also handles our castle-data: protocol.}
function UriToFilenameSafe(const Uri: String): string;

{ Convert filename to URI.

  This is a fixed version of UriParser.FilenameToUri, that correctly
  percent-encodes the parameter, making it truly a reverse of
  UriToFilenameSafe. In FPC > 2.6.2 UriParser.FilenameToUri will also
  do this (after Michalis' patch, see
  http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=24321 ).

  It also makes sure the filename is absolute (it uses ExpandFileName,
  so if the FileName is relative --- it will be expanded, treating it
  as relative to the current directory). }
function FilenameToUriSafe(FileName: string): string;

{ Tries change URI to use castle-data: protocol.
  It's used in our editor to change absolute paths to relative to castle-data
  directory. }
function MaybeUseDataProtocol(const Url: String): String;

{ Get MIME type for content of the URI @italic(without downloading the file).
  For local and remote files (file, http, and similar protocols)
  it guesses MIME type based on file extension.

  Using this function is not adviced if you want to properly support
  MIME types returned by http server for network resources.
  For this, you have to download the file,
  and look at what MIME type the http server reports.
  The @link(Download) function returns such proper MimeType.
  This function only guesses without downloading.

  Returns empty string if MIME type is unknown.

  Overloaded version returns also Gzipped to detect whether file contents
  are gzipped.

  The recognition mechanism can be enhanced by adding your own
  mappings to the @link(UriMimeExtensions).

  @groupBegin }
function UriMimeType(const Uri: String): string; overload;
function UriMimeType(const Uri: String; out Gzipped: boolean): string; overload;
{ @groupEnd }

{ Map from an extension to a MIME type, used by @link(UriMimeType).
  The extension should be lowercase, and includes a leading dot, like @code(.png). }
function UriMimeExtensions: TStringStringMap;

{ Convert URI to a nice form for display (to show in messages and such).
  It makes sure to nicely trim URLs that would be too long/unreadable otherwise
  (like "data:" URI, or multi-line URLs with inlined ECMAScript/CastleScript/shader
  code).

  When Short = @false (default), then for most "file:" and "http:" URLs,
  it just returns them untouched.

  When Short = @true, it will try to extract the last path component from
  URLs like "file:" and "http:", if this last component is not empty.
  Similar to what ExtractFileName does for filenames.
  It will also decode the URI (convert %xx to normal charaters).
  Because of the percent-decoding,
  it is not advised to use this on filenames with Short=true.
  Usually, you want to call UriCaption that makes sure that argument is URL
  (using AbsoluteUri) and then returns UriDisplay with Short=true.

  It is safe to use this on both absolute and relative URLs.
  It does not resolve relative URLs in any way.
  It also means that it returns empty string for empty URI
  (contrary to most other routines that convert empty string
  to a current directory when resolving relative URLs). }
function UriDisplay(const Uri: String; const Short: boolean = false): string;

{ Convert URI to a nice form for a short caption.

  Returns empty string for empty URI (contrary to most other routines that
  treat empty string like a current directory).

  See UriDisplay documentation for details.
  This calls UriDisplay with Short = @true. }
function UriCaption(const Uri: String): string;

{ Change extension of the URL. }
function ChangeUriExt(const Url, Extension: string): string;

{ Delete extension of the URL. }
function DeleteUriExt(const Url: String): string;

{ Extract filename (last part after slash) from URL. }
function ExtractUriName(const Url: String): string;

{ Extract path (everything before last part), including final slash, from URL. }
function ExtractUriPath(const Url: String): string;

{ Ensure URL ends with slash.

  For an empty URL, returns empty string (so it does not turn "" into "/").
  For an URL ending with bashslash (which usually means you passed Windows
  path name), it removes the backslash before adding slash.

  This should be used instead of InclPathDelim or IncludeTrailingPathDelimiter,
  when you use URLs instead of filenames. }
function UriIncludeSlash(const Url: String): String;

{ Ensure URL does not end with slash.
  In case you passed Windows path name, it also removes the backslash.

  This should be used instead of ExclPathDelim or ExcludeTrailingPathDelimiter,
  when you use URLs instead of filenames. }
function UriExcludeSlash(const Url: String): String;

{ Does a file exist, that is: whether it makes sense to load it with
  the @link(Download) function.

  Returns @true for URLs where we cannot determine whether the file exists
  (like http / https).

  This is simply a shortcut for @code(UriExists(Url) in [ueFile, ueUnknown]). }
function UriFileExists(const Url: String): Boolean;

type
  { Result of the @link(UriExists) query. }
  TUriExists = (
    { Given path does not indicate either a file or directory. }
    ueNotExists,

    { Given path is a regular file.
      In particular, this means it can be read with the @link(Download) function.

      Note that there is no @italic(guarantee) that opening it will work.
      On a multi-process system the file can be always deleted between the call
      to UriExists and Download.
      And the file permissions may not allow reading.
      We merely say that "right now this file exists".
    }
    ueFile,

    { Given path is a directory. E.g. it can be used as path for the @link(FindFiles) function. }
    ueDirectory,

    { Detecting existence of given path is tricky, it could be time-consuming.

      This applies e.g. to URLs using http / https protocols.
      The only way to detect their existence would be to actually open them.
      But this involves a network request, so it may take some time,
      and should be done asynchronously (see @link(TCastleDownload)).

      If you really want to check the file existence, you can always
      try to open it by @link(Download):

      @longCode(#
      try
        Stream := Download(Url);
        FreeAndNil(Stream);
        ItExists := true;
      except
        on E: Exception do
        begin
          WritelnLog('Opening URL %s failed with exception %s', [
            UriCaption(Url),
            ExceptMessage(E)
          ]);
          ItExists := false;
        end;
      end;
      #)

      Depending on the circumstances, the "ueUnknown" can be sometimes interpreted
      as "it exists" and sometimes as "it doesn't exist".
      Opening it with @link(Download) may either fail or succeed, we cannot detect.
    }
    ueUnknown
  );

{ Does a file or directory exist under this URL.
  See TUriExists for possible return values. }
function UriExists(Url: String): TUriExists;

{ Current working directory of the application, expressed as URL,
  including always final slash at the end. }
function UriCurrentPath: string;

{ If the given URL uses "castle-data:..." protocol, resolve it,
  returning a URL that does not use "castle-data:..." protocol any more.
  For example may resolve "castle-data:/xxx" into
  "file:///home/michalis/my-application/data/xxx".
  See https://castle-engine.io/data
  for documentation how our "data directory" works.

  If the URL has a different protocol, it is returned unchanged. }
function ResolveCastleDataUrl(const Url: String): String;

{ If this URL indicates something inside the @url(https://castle-engine.io/data
  CGE data directory) then return URL relative to this data directory.
  E.g. for "castle-data:/foo/bar.txt" it returns "foo/bar.txt".

  It accepts any URL, relative (to the current working directory)
  or absolute (with any protocol).
  It works when the URL starts with castle-data:/ protocol,
  it works when the URL starts with other (like file:/) protocol
  that still points to a file inside data.

  If the URL does not point to a file in data, it is returned untouched. }
function RelativeToCastleDataUrl(const Url: String; out WasInsideData: Boolean): String;

{ Encode string by using percent encoding (https://en.wikipedia.org/wiki/Percent-encoding)
  @exclude }
function InternalUriEscape(const S: String): String;

{ Decode string encoded by percent encoding (https://en.wikipedia.org/wiki/Percent-encoding )
  @exclude }
function InternalUriUnescape(const S: String): String;

var
  { On systems where filesystems are usually case-sensitive
    (Unix; like Linux, FreeBSD, macOS),
    accept any @code('castle-data:/xxx') URLs, even when they have different case
    than the actual files.

    This is a quick way to run applications prepared / tested on case-insensitive
    systems (like Windows) on Unix. It causes a small additional work when we open
    data files, but in many cases it is acceptable. }
  CastleDataIgnoreCase: Boolean = false;

implementation

uses UriParser,
  CastleUtils, CastleInternalDataUri, CastleLog, CastleFilesUtils,
  CastleInternalDirectoryInformation, CastleFindFiles, CastleDownload
  {$ifdef CASTLE_NINTENDO_SWITCH}, CastleInternalNxBase {$endif}
  {$ifndef FPC}, Character{$endif};

{ Escape and Unescape --------------------------------------------------------
  Copied from UriParser and fixed for Delphi, as they are internal there.
}

function InternalUriUnescape(const S: String): String;

  function HexValue(C: Char): Integer;
  begin
    case C of
      '0'..'9': Result := Ord(C) - Ord('0');
      'A'..'F': Result := Ord(C) - (Ord('A') - 10);
      'a'..'f': Result := Ord(C) - (Ord('a') - 10);
    else
      Result := 0;
    end;
  end;

var
  I: Integer;
  {$ifdef FPC}
  RealLength: Integer;
  P: PChar;
  {$else}
  J: Integer;
  Utf8Char: RawByteString;
  Utf16Char: String;
  {$endif}
begin
  I := 1;
  {$ifdef FPC}
  SetLength(Result, Length(s));
  P := PChar(Result);  { use PChar to prevent numerous calls to UniqueString }
  RealLength := 0;
  while i <= Length(S) do
  begin
    if S[I] = '%' then
    begin
      P[RealLength] := Chr(HexValue(S[I + 1]) shl 4 or HexValue(S[I + 2]));
      Inc(I, 3);
    end else
    begin
      P[RealLength] := S[I];
      Inc(I);
    end;
    Inc(RealLength);
  end;
  SetLength(Result, RealLength);
  {$else}
  if S = '' then
    Exit('');

  while I <= Length(S) do
  begin
    if IsSurrogate(S, I) then
    begin
      { I don't think that should ever happen. If so, we simply add
        such a character. }
      Result := Result + S[I];
      if I <> Length(S) then
      begin
        Result := Result + S[I + 1];
      end;
      Inc(I);
    end else
    if S[I] = '%' then
    begin
      { We need support here a case when more than one "%" block make one UTF16
        character }
      Utf8Char := '';
      J := I;
      while J <= Length(S) do
      begin
        Utf8Char := Utf8Char + AnsiChar(HexValue(S[J + 1]) shl 4 or HexValue(S[J + 2]));
        Inc(J, 3);
        if (J > Length(S)) or (S[J] <> '%') then
          Break;
      end;
      I := J - 1; { -1 becouse incrementation on the loop end }
      Utf16Char :=  UTF8ToString(Utf8Char);
      Result := Result + Utf16Char;
    end else
      Result := Result + S[I];

    Inc(I);
  end;
  {$endif}
end;

function InternalUriEscapeCore(const S: String; const Allowed: TSysCharSet): String;
var
  i, L: Integer;
  {$ifdef FPC}
  P: PChar;
  {$else}
  J: Integer;
  Utf16Char: String; // String here becouse somtimes UTF-16 char can be two wide chars
  UTF8Char: UTF8String;
  {$endif}
begin
  L := Length(s);
  {$ifdef FPC}
  for i := 1 to Length(s) do
    if not CharInSet(s[i], Allowed) then Inc(L,2);
  if L = Length(s) then
  begin
    Result := s;
    Exit;
  end;

  SetLength(Result, L);
  P := @Result[1];
  for i := 1 to Length(s) do
  begin
    if not CharInSet(s[i], Allowed) then
    begin
      P^ := '%'; Inc(P);
      StrFmt(P, '%.2x', [ord(s[i])]); Inc(P);
    end else
      P^ := s[i];
    Inc(P);
  end;
  {$else}
  if L = 0 then
    Exit('');

  I := 1;
  while I <= L do
  begin
    if IsSurrogate(S, I) then
    begin
      { Check the string is complete (Is there the second char?) }
      if I < L then
      begin
        Utf16Char := S[I] + S[I + 1];
        UTF8Char := UTF8String(Utf16Char);
        for J := 1 to Length(UTF8Char) do
        begin
          Result := Result + '%' + Format('%.2x', [ord(UTF8Char[J])]);
        end;
      end;
      Inc(I);
    end else
    begin
      if not CharInSet(s[i], Allowed) then
      begin
        // Not surrogate but also not allowed
        UTF8Char := UTF8String(S[I]);
        for J := 1 to Length(UTF8Char) do
        begin
          Result := Result + '%' + Format('%.2x', [ord(UTF8Char[J])]);
        end;
      end else
      begin
        Result := Result + S[I];
      end;
    end;

    Inc(I);
  end;
  {$endif FPC}
end;

function InternalUriEscape(const S: String): String;
const
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];
begin
  Result := InternalUriEscapeCore(S, ValidPathChars);
end;

{ other routines ------------------------------------------------------------- }

procedure UriGetAnchor(const Uri: String; out Anchor: string);
var
  U: String;
begin
  U := Uri;
  UriExtractAnchor(U, Anchor);
end;

procedure UriExtractAnchor(var Uri: String; out Anchor: string);
var
  HashPos: Integer;
begin
  Anchor := '';

  { Avoid extracting anchor from data URI, to avoid touching things like
      data:model/x3d+vrml,#X3D V3.2 utf8
      ...
    which are used to embed classic VRML/X3D content.
    The hash in data URI is *not* an anchor. }

  if TDataUri.IsDataUri(Uri) then
    Exit;

  HashPos := BackPos('#', Uri);
  if HashPos <> 0 then
  begin
    Anchor := SEnding(Uri, HashPos + 1);
    SetLength(Uri, HashPos - 1);
  end;
end;

procedure UriGetSettingsFromAnchor(const Uri: String;
  const SettingsFromAnchor: TStringStringMap);
var
  UrlForDisplay: String;

  procedure ProcessAnchorPart(const Part: String);
  var
    Semicolon: Integer;
    PartName, PartValue: String;
  begin
    Semicolon := Pos(':', Part);

    if Semicolon = 0 then
    begin
      WritelnWarning('Empty setting (%s) in anchor of "%s"', [Part, UrlForDisplay]);
      SettingsFromAnchor.Add(Part, '');
    end else
    begin
      PartName := Copy(Part, 1, Semicolon - 1);
      PartValue := SEnding(Part, Semicolon + 1);
      SettingsFromAnchor.Add(PartName, PartValue);
    end;
  end;

var
  Anchor, AnchorPart: String;
  SeekPos: Integer;
begin
  UrlForDisplay := UriDisplay(Uri);

  { We need recognize escaped hash because GTK2 open dialog returns %23
    in # position }
  UriGetAnchor(Uri, Anchor);
  SettingsFromAnchor.Clear;

  if Anchor = '' then
    Exit;

  SeekPos := 1;
  repeat
    AnchorPart := NextToken(Anchor, SeekPos, [',']);
    if AnchorPart = '' then
      Break;
    ProcessAnchorPart(AnchorPart);
  until false;
end;

function UriDeleteAnchor(const Uri: String): string;
var
  Anchor: string;
begin
  Result := Uri;
  UriExtractAnchor(Result, Anchor);
end;

function RawUriDecode(const S: string): string;

  { Assume Position <= Length(S).
    Check is S[Positon] is a start of %xx sequence:
    - if not, exit false
    - if yes, but %xx is invalid, report WritelnWarning and exit false
    - if yes and %xx is valid, set DecodedChar and exit true }
  function ValidSequence(const S: string; Position: Integer;
    out DecodedChar: char): boolean;
  const
    ValidHexaChars = ['a'..'f', 'A'..'F', '0'..'9'];

    { Assume C is valid hex digit, return it's value (in 0..15 range). }
    function HexDigit(const C: char): Byte;
    begin
      if CharInSet(C, ['0'..'9']) then
        Result := Ord(C) - Ord('0')
      else
      if CharInSet(C, ['a'..'f']) then
        Result := 10 + Ord(C) - Ord('a')
      else
      if CharInSet(C, ['A'..'F']) then
        Result := 10 + Ord(C) - Ord('A')
      else
        raise EInternalError.Create('Invalid hex character');
    end;

  begin
    Result := S[Position] = '%';
    if Result then
    begin
      if Position + 2 > Length(S) then
      begin
        WritelnWarning('URI', Format(
          'URI "%s" incorrectly encoded, %%xx sequence ends unexpectedly', [S]));
        Exit(false);
      end;

      if (not CharInSet(S[Position + 1], ValidHexaChars)) or
         (not CharInSet(S[Position + 2], ValidHexaChars)) then
      begin
        WritelnWarning('URI', Format(
          'URI "%s" incorrectly encoded, %s if not a valid hexadecimal number',
          [S, S[Position + 1] + S[Position + 2]]));
        Exit(false);
      end;

      DecodedChar := Chr(
        (HexDigit(S[Position + 1]) shl 4) or
         HexDigit(S[Position + 2])
      );
    end;
  end;

var
  I, ResultI: Integer;
  DecodedChar: char;
begin
  { Allocate Result string at the beginning, to save time later for
    memory reallocations. We can do this, since we know that final
    Result is shorter or equal to S. }
  SetLength(Result, Length(S));

  ResultI := 1;
  I := 1;

  while I <= Length(S) do
  begin
    if ValidSequence(S, I, DecodedChar) then
    begin
      Result[ResultI] := DecodedChar;
      Inc(ResultI);
      Inc(I, 3);
    end else
    begin
      Result[ResultI] := S[I];
      Inc(ResultI);
      Inc(I);
    end;
  end;

  SetLength(Result, ResultI - 1);
end;

const
  { These constants match UriParser algorithm, which in turn follows RFC. }
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  ProtocolFirstChar = ALPHA;
  ProtocolChar = ALPHA + DIGIT + ['+', '-', '.'];

function UriValidProtocol(const P: String): Boolean;
var
  I: Integer;
begin
  Result := (P <> '') and CharInSet(P[1], ProtocolFirstChar);
  if Result then
    for I := 2 to Length(P) do
      if not CharInSet(P[I], ProtocolChar) then
        Exit(false);
end;

{ Detect protocol delimiting positions.
  If returns true, then for sure:
  - FirstCharacter < Colon
  - FirstCharacter >= 1
  - Colon > 1 }
function UriProtocolIndex(const S: string; out FirstCharacter, Colon: Integer): boolean;
var
  I: Integer;
begin
  Result := false;
  Colon := Pos(':', S);
  if Colon <> 0 then
  begin
    (* Skip beginning whitespace from protocol.
       This allows us to detect properly "ecmascript:" protocol in VRML/X3D:
      Script { url "
        ecmascript:..." }
    *)
    FirstCharacter := 1;
    while (FirstCharacter < Colon) and CharInSet(S[FirstCharacter], WhiteSpaces) do
      Inc(FirstCharacter);
    if FirstCharacter >= Colon then
      Exit;

    { Protocol name can only contain specific characters. }
    if not CharInSet(S[FirstCharacter], ProtocolFirstChar) then
      Exit;
    for I := FirstCharacter + 1 to Colon - 1 do
      if not CharInSet(S[I], ProtocolChar) then
        Exit;

    { Do not treat drive names in Windows filenames as protocol.
      To allow stable testing, do this on all platforms, even non-Windows.
      We do not use any single-letter protocol, so no harm. }
    Result := not ((FirstCharacter = 1) and (Colon = 2));
  end;
end;

function UriProtocol(const Uri: String): string;
var
  FirstCharacter, Colon: Integer;
begin
  if UriProtocolIndex(Uri, FirstCharacter, Colon) then
    Result := LowerCase(CopyPos(Uri, FirstCharacter, Colon - 1)) else
    Result := '';
end;

function UriProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;
var
  FirstCharacter, I: Integer;
begin
  Result := false;
  if UriProtocolIndex(S, FirstCharacter, Colon) and
     (Colon - FirstCharacter = Length(Protocol)) then
  begin
    for I := 1 to Length(Protocol) do
      if LoCase(Protocol[I]) <> LoCase(S[I - FirstCharacter + 1]) then
        Exit;
    Result := true;
  end;
end;

function UriDeleteProtocol(const S: string): string;
var
  FirstCharacter, Colon: Integer;
begin
  if UriProtocolIndex(S, FirstCharacter, Colon) then
    { Cut off also whitespace before FirstCharacter }
    Result := SEnding(S, Colon + 1) else
    Result := S;
end;

function CombineUri(const Base, Relative: string): string;
// var
//   RelativeProtocol: string;
begin
  { Test for some special protocols first, that may have whitespace before
    the protocol name. }
  { This is not necessary anymore -- check below for UriProtocol(Relative) <> ''
    will handle this case anyway.
  RelativeProtocol := UriProtocol(Relative);
  if (RelativeProtocol = 'ecmascript') or
     (RelativeProtocol = 'javascript') or
     (RelativeProtocol = 'castlescript') or
     (RelativeProtocol = 'kambiscript') or
     (RelativeProtocol = 'compiled') then
    Exit(Relative);
  }

  { When Base is like 'castle-data:/CastleSettings.xml'
    and Relative is like '../gfx/font.ttf',
    you need to resolve the Base to use the file:/ protocol. }
  if (UriProtocol(Base) = 'castle-data') and IsPrefix('../', Relative) then
    Exit(CombineUri(ResolveCastleDataUrl(Base), Relative));

  { Relative is already an absolute Url, no point in doing anything,
    in particular no point for doing AbsoluteUri(Base) below,
    which could fail on NX in case Base='', calling ExpandFileName
    is not allowed on NX. }
  if UriProtocol(Relative) <> '' then
    Exit(Relative);

  try
    if not ResolveRelativeUri(AbsoluteUri(Base), Relative, Result) then
    begin
      { The only case when ResolveRelativeUri may fail is when neither argument
        contains a protocol. But we just used AbsoluteUri, which makes sure
        that AbsoluteUri(Base) has some protocol. }
      raise EInternalError.CreateFmt('Failed to resolve relative URI "%s" with base "%s"',
        [Relative, Base]);
    end;
  except
    { workaround http://bugs.freepascal.org/view.php?id=28496 , see also
      https://sourceforge.net/p/castle-engine/tickets/35/ }
    on E: EConvertError do
    begin
      WritelnWarning('URL', Format('Error when parsing URL. This usually indicates an incorrect Windows "file:" URL (it should have *three* slashes, like "file:///c:/blah..."): "%s"',
        [Relative]));
      Result := Relative;
    end;
  end;
end;

function AbsoluteUri(const Uri: String): string;
begin
  if UriProtocol(Uri) = '' then
    Result := FilenameToUriSafe(Uri) else
    Result := Uri;
end;

function AbsoluteFileUri(const Uri: String): boolean;
begin
  Result := (UriProtocol(Uri) = '') and IsPathAbsoluteOnDrive(Uri);
end;

function UriToFilenameSafe(const Uri: String): string;
var
  P, CastleDataResolved: string;
begin
  { Use our UriProtocol instead of depending that UriToFilename will detect
    empty protocol case correctly. This allows to handle Windows absolute
    filenames like "c:\foo" as filenames. }
  P := UriProtocol(Uri);
  if P = '' then
    Result := Uri
  else
  if P = 'file' then
  begin
    try
      if not UriToFilename(Uri, Result) then Result := '';
    except
      { workaround http://bugs.freepascal.org/view.php?id=28496 , see also
        https://sourceforge.net/p/castle-engine/tickets/35/ }
      on E: EConvertError do
      begin
        WritelnWarning('URL', Format('Error when parsing URL. This usually indicates an incorrect Windows "file:" URL (it should have *three* slashes, like "file:///c:/blah..."): "%s"',
          [Uri]));
        Result := '';
      end;
    end;
  end else
  if P = 'castle-data' then
  begin
    CastleDataResolved := ResolveCastleDataUrl(Uri);
    if UriProtocol(CastleDataResolved) = 'castle-data' then
      raise EInternalError.CreateFmt('ResolveCastleDataUrl cannot return URL with castle-data protocol. This probably indicates that ApplicationDataOverride (%s) contains castle-data protocol, which it should not.', [
        ApplicationDataOverride
      ]);
    Result := UriToFilenameSafe(CastleDataResolved);
  end else
    Result := '';
end;

function FilenameToUriSafe(FileName: string): string;

{ Code adjusted from FPC FilenameToUri (same license as our engine,
  so it's Ok to share code). Adjusted to call Escape on FileName.
  See http://bugs.freepascal.org/view.php?id=24324 : FPC FilenameToUri
  should be fixed in the future to follow this.

  We also make sure to call ExpandFileName,
  and so we don't need checks for IsAbsFilename. }

  { Like ExpandFileName, but
    - guarantees that if input ends with directory separator,
      then output will end with it too.
      This is necessary on Delphi+Posix.
    - for S = '', outputs current dir. }
  function ExpandFileNameFixed(const S: String): String;
  begin
    if S = '' then
      Exit(InclPathDelim(GetCurrentDir));

    Result := ExpandFileName(S);
    {$ifndef FPC}
    if (S <> '') and
       CharInSet(S[Length(S)], AllowDirectorySeparators) and
       ( (Result = '') or
         (not CharInSet(Result[Length(Result)], AllowDirectorySeparators) ) ) then
        Result := InclPathDelim(Result);
    {$endif}
  end;

var
  I: Integer;
  FilenamePart: String;
begin
  FileName := ExpandFileNameFixed(FileName);

  Result := 'file:';

  Assert(Filename <> '');
  if Filename[1] <> PathDelim then
    Result := Result + '///'
  else
    Result := Result + '//';

  FilenamePart := Filename;
  { unreachable code warning is ok here }
  {$warnings off}
  if PathDelim <> '/' then
  begin
    I := Pos(PathDelim, FilenamePart);
    while I <> 0 do
    begin
      FilenamePart[I] := '/';
      I := Pos(PathDelim, FilenamePart);
    end;
  end;
  {$warnings on}
  FilenamePart := InternalUriEscape(FilenamePart);

  Result := Result + FilenamePart;
end;

var
  FUriMimeExtensions: TStringStringMap;

function UriMimeExtensions: TStringStringMap;
begin
  if FUriMimeExtensions = nil then
    FUriMimeExtensions := TStringStringMap.Create;
  Result := FUriMimeExtensions;
end;

function UriMimeType(const Uri: String; out Gzipped: boolean): string;
begin
  Result := InternalUriMimeType(Uri, Gzipped);
end;

function UriMimeType(const Uri: String): string;
var
  Gzipped: boolean;
begin
  Result := UriMimeType(Uri, Gzipped);
end;

function MaybeUseDataProtocol(const Url: String): String;
var
  DataPath: String;
begin
  { Use below ResolveCastleDataUrl, to get real location of data,
    e.g. resolved to file:// on normal desktop. }
  DataPath := ResolveCastleDataUrl('castle-data:/');
  if IsPrefix(DataPath, Url, not FileNameCaseSensitive) then
    Result := 'castle-data:/' + PrefixRemove(DataPath, Url, not FileNameCaseSensitive)
  else
    Result := Url;
end;

function UriDisplay(const Uri: String; const Short: boolean): string;
var
  DataUri: TDataUri;
  NewLinePos: Integer;
  Parsed: TUri;
begin
  Result := Trim(Uri);

  if TDataUri.IsDataUri(Uri) then
  begin
    DataUri := TDataUri.Create;
    try
      DataUri.Uri := Uri;
      if DataUri.Valid then Result := DataUri.UriPrefix + ',...';
    finally FreeAndNil(DataUri) end;
  end else

  begin
    NewLinePos := CharsPos([#10, #13], Result);
    if NewLinePos <> 0 then
    begin
      { we have done Trim(Uri) to prevent starting from newline }
      Assert(NewLinePos <> 1);
      Result := Copy(Result, 1, NewLinePos - 1) + '...';
    end else
    if Short then
    begin
      { try to extract last path component }
      try
        Parsed := ParseUri(Uri);
      except
        on E: EConvertError do
        begin
          WritelnWarning('URL', Format('Error when parsing Uri. This usually indicates an incorrect Windows "file:" URL (it should have *three* slashes, like "file:///c:/blah..."): "%s"',
            [Uri]));
          Parsed.Document := ExtractUriName(Uri);
        end;
      end;

      Parsed.Document := Trim(Parsed.Document);
      if Parsed.Document <> '' then
        Result := Parsed.Document;
    end;
  end;
end;

function UriCaption(const Uri: String): string;
begin
  if Uri = '' then
    Result := ''
  else
    Result := UriDisplay(AbsoluteUri(Uri), true);
end;

function ChangeUriExt(const Url, Extension: string): string;

  {$ifndef FPC}
  { Mask default Delphi ChangeFileExt that behaves badly for filenames
    like '.hidden'.}
  function ChangeFileExt(const FileName, NewExtension: String): String;
  var
    I: Integer;
    ExtDotPos: Integer;
  begin
    ExtDotPos := 0;
    for I := Length(FileName) downto 1 do
      if FileName[I] in AllowDirectorySeparators then
      begin
        // no extension, leave ExtDotPos = 0
        Break;
      end else
      if (FileName[I] = '.') and
         (I > 1) and
         (not (FileName[I - 1] in AllowDirectorySeparators)) then
      begin
        // dot, but not at the beginning of the name -> valid ExtDotPos
        ExtDotPos := I;
        Break;
      end;

    if ExtDotPos <> 0 then
      Result := Copy(FileName, 1, ExtDotPos - 1) + NewExtension
    else
      Result := FileName + NewExtension;
  end;
  {$endif}

var
  UrlWithoutAnchor, Anchor: String;
begin
  UrlWithoutAnchor := Url;
  UriExtractAnchor(UrlWithoutAnchor, Anchor);
  Result := ChangeFileExt(UrlWithoutAnchor, Extension);
  if Anchor <> '' then
    Result := Result + '#' + Anchor;
end;

function DeleteUriExt(const Url: String): string;
begin
  Result := ChangeUriExt(Url, '');
end;

function ExtractUriName(const Url: String): string;
var
  UrlWithoutAnchor: String;
  {$ifndef FPC} I: Integer; {$endif}
begin
  UrlWithoutAnchor := UriDeleteAnchor(Url);
  {$ifdef FPC}
  Result := ExtractFileName(UrlWithoutAnchor);
  {$else}
  { In Delphi, / separator in paths is not recognized, so we cannot use ExtractFilePath.
    TODO: our own solution should be just used for both compilers.
    Need autotests to confirm it behaves the same, on both platforms. }

  I := BackCharsPos(['/'], UrlWithoutAnchor);
  if I <> 0 then
    Result := SEnding(UrlWithoutAnchor, I + 1)
  else
    Result := UrlWithoutAnchor;
  {$endif}
end;

function ExtractUriPath(const Url: String): string;
var
  UrlWithoutAnchor: String;
  {$ifndef FPC} I: Integer; {$endif}
begin
  { While on non-Windows ExtractFilePath would work on full URL as well,
    but on Windows the ":" inside anchor (like
    "castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore")
    would cause trouble: it would be considered a drive letter separator,
    and change the result. }
  UrlWithoutAnchor := UriDeleteAnchor(Url);
  {$ifdef FPC}
  Result := ExtractFilePath(UrlWithoutAnchor);
  {$else}
  { In Delphi, / separator in paths is not recognized, so we cannot use ExtractFilePath.
    TODO: our own solution should be just used for both compilers.
    Need autotests to confirm it behaves the same, on both platforms. }

  I := BackCharsPos(['/'], UrlWithoutAnchor);
  if I <> 0 then
    Result := Copy(UrlWithoutAnchor, 1, I)
  else
    Result := UrlWithoutAnchor;
  {$endif}
end;

function UriIncludeSlash(const Url: String): String;
var
  L: Integer;
begin
  if Url = '' then
    Exit('');

  L := Length(Url);
  case Url[L] of
    '/': Result := Url; // nothing needs to be done
    '\': Result := Copy(Url, 1, L - 1) + '/';
    else Result := Url + '/';
  end;
end;

function UriExcludeSlash(const Url: String): String;
var
  L: Integer;
begin
  L := Length(Url);
  if (L <> 0) and CharInSet(Url[L], ['/', '\']) then
    Result := Copy(Url, 1, L - 1)
  else
    Result := Url;
end;

function UriFileExists(const Url: String): Boolean;
begin
  Result := UriExists(Url) in [ueFile, ueUnknown];
end;

function UriExists(Url: String): TUriExists;

  // Detect existence of castle-data:/xxx URL using DataDirectoryInformation.
  function UseDataDirectoryInformation(const Url: String): TUriExists;
  var
    U: TUri;
    UrlPath: String;
    PathEntry: TDirectoryInformation.TEntry;
  begin
    U := ParseUri(Url);
    UrlPath := PrefixRemove('/', U.Path + U.Document, false);
    PathEntry := DataDirectoryInformation.FindEntry(UrlPath);
    if PathEntry = nil then
      Exit(ueNotExists)
    else
    if PathEntry is TDirectoryInformation.TDirectory then
      Exit(ueDirectory)
    else
      Exit(ueFile);
  end;

  {$ifdef CASTLE_NINTENDO_SWITCH}
  // Detect existence of castle-nx-contents or castle-nx-save URL using NX-specific function.
  function UseNXExists(const Url: String): TUriExists;
  begin
    Result := NXFileExists(Url);
  end;
  {$endif CASTLE_NINTENDO_SWITCH}

  // Detect existence of a filename using FileExists, DirectoryExists.
  function UseFileDirectoryExists(const FileName: String): TUriExists;
  var
    F, D: Boolean;
  begin
    F := FileExists(FileName);
    D := DirectoryExists(FileName);

    { FileExists behaves inconsistently for directories.
      On non-Windows, returns true.
      On Windows, returns false.
      See http://www.freepascal.org/docs-html/rtl/sysutils/fileexists.html
      http://free-pascal-general.1045716.n5.nabble.com/FileExists-inconsistency-td2813433.html
      So check both, and if DirectoryExists then assume it's a directory
      (regardless of FileExists result). }
    if D then
      Exit(ueDirectory)
    else
    if F then
      Exit(ueFile)
    else
      Exit(ueNotExists);
  end;

var
  P: String;
begin
  { data: URI is like a file, since you can call Download() on it }
  if TDataUri.IsDataUri(Url) then
    Exit(ueFile);

  P := UriProtocol(Url);

  if (P = 'castle-data') and
     (DisableDataDirectoryInformation = 0) and
     (DataDirectoryInformation <> nil) then
    Exit(UseDataDirectoryInformation(Url));

  { Resolve castle-data:/xxx now.
    This way we can work in case we have castle-data:/xxx URL that resolves
    to something handled below (like file:/xxx) but wasn't handled above
    (e.g. because DataDirectoryInformation = nil). }
  Url := ResolveCastleDataUrl(Url);
  P := UriProtocol(Url);

  {$ifdef CASTLE_NINTENDO_SWITCH}
  if (P = 'castle-nx-contents') or
     (P = 'castle-nx-save') then
    Exit(UseNXExists(Url));
  {$endif CASTLE_NINTENDO_SWITCH}

  if (P = '') or (P = 'file') then
    Exit(UseFileDirectoryExists(UriToFilenameSafe(Url)));

  Result := ueUnknown;
end;

function UriCurrentPath: string;
begin
  Result := FilenameToUriSafe(InclPathDelim(GetCurrentDir));
end;

type
  TFixCaseHandler = class
    SearchName: String;
    FoundName: String;
  end;

procedure FixCaseCallback(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
var
  H: TFixCaseHandler;
begin
  H := TFixCaseHandler(Data);
  if AnsiSameText(FileInfo.Name, H.SearchName) then
  begin
    if H.FoundName <> '' then
      raise Exception.CreateFmt('Ambiguous files found in directory "%s": "%s" and "%s" (and CastleDataIgnoreCase was used)', [
        ExtractUriPath(FileInfo.Url),
        FileInfo.Name,
        H.FoundName
      ]);
    H.FoundName := FileInfo.Name;
  end;
end;

{ If Path ends with <platform>/<config>/,
  return @true and put in StrippedPath the Path with the last 2 path components
  removed.
  Otherwise return @false.

  Path must end with path delimiter.

  When returns @true, StrippedPath is also guaranteed to end with path delimiter.
}
function StripExePathFromPlatformConfig(const Path: String;
  out StrippedPath: String): Boolean;

  { Platform name, like 'Win32', as used by Delphi in $(Platform) subdirectory
    name, corresponding to this process OS / CPU.
    Always lowercase.

    This is made to also compile and work with FPC, for testing,
    so it doesn't use TOSVersion. }
  function DelphiPlatformName: String;
  begin
    Result :=
      {$if defined(MSWINDOWS)} 'win'
      {$elseif defined(LINUX)} 'linux'
      {$elseif defined(DARWIN)} 'macos' // TODO: not yet confirmed by testing, just guessing
      {$elseif defined(ANDROID)} 'android' // TODO: not yet confirmed by testing, just guessing
      {$elseif defined(IOS)} 'ios' // TODO: not yet confirmed by testing, just guessing
      {$else} ''
      {$endif};

    Result := Result +
      {$if defined(CPU32)} '32'
      {$elseif defined(CPU64)} '64'
      {$else} ''
      {$endif};
  end;

var
  Dir: String;
  ParentName: String;
begin
  Result := false;

  Dir := ExclPathDelim(Path);
  // LowerCase, to detect <config> case-insensitively
  ParentName := LowerCase(ExtractFileName(Dir));
  if (ParentName = 'debug') or
     (ParentName = 'release') then
  begin
    Dir := ExtractFileDir(Dir);
    // LowerCase, to detect <platform> case-insensitively
    ParentName := LowerCase(ExtractFileName(Dir));
    if ParentName = DelphiPlatformName then
    begin
      StrippedPath := ExtractFilePath(Dir);
      Result := true;
    end;
  end;
end;

var
  ApplicationDataIsCache: Boolean = false;
  ApplicationDataCache: string;

function ApplicationDataCore(const Path: string): string;

  {$ifndef ANDROID}
  function GetApplicationDataPath: string;
  {$ifdef MSWINDOWS}
  var
    ExePath, StrippedExePath: string;
  begin
    {$warnings off}
    // knowingly using deprecated; ExeName should be undeprecated but internal one day
    ExePath := ExtractFilePath(ExeName);
    {$warnings on}

    Result := ExePath + 'data' + PathDelim;
    if DirectoryExists(Result) then Exit;

    if StripExePathFromPlatformConfig(ExePath, StrippedExePath) then
    begin
      Result := StrippedExePath + 'data' + PathDelim;
      if DirectoryExists(Result) then Exit;
    end;

    Result := ExePath;
  {$endif MSWINDOWS}
  {$ifdef UNIX}
  var
    CurPath: string;
  begin
    {$ifdef DARWIN}
    if BundlePath <> '' then
    begin
      {$ifdef CASTLE_IOS}
      Result := BundlePath + 'data/';
      {$else}
      Result := BundlePath + 'Contents/Resources/data/';
      {$endif}
      if DirectoryExists(Result) then Exit;

      {$ifndef IOS}
      Result := BundlePath + '../data/';
      if DirectoryExists(Result) then
      begin
        WritelnLog('"Contents/Resources/data/" subdirectory not found inside the macOS application bundle: ' + BundlePath + NL +
          '  Using instead "data/" directory that is sibling to the application bundle.' + NL +
          '  This makes sense only for debug.' + NL +
          '  The released application version should instead include the data inside the bundle.');
        Exit;
      end;
      {$endif}
    end;
    {$endif DARWIN}

    Result := HomePath + '.local/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/local/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    Result := '/usr/share/' + ApplicationName + '/';
    if DirectoryExists(Result) then Exit;

    CurPath := InclPathDelim(GetCurrentDir);

    Result := CurPath + 'data/';
    if DirectoryExists(Result) then Exit;

    Result := CurPath;
  {$endif UNIX}
  end;
  {$endif not ANDROID}

begin
  if ApplicationDataOverride <> '' then
    Exit(ApplicationDataOverride + Path);

  if Pos('\', Path) <> 0 then
    WritelnWarning('ResolveCastleDataUrl', 'Do not use backslashes (or a PathDelim constant) in the ApplicationDataCore parameter. The ApplicationDataCore parameter should be a relative URL, with components separated by slash ("/"), regardless of the OS. Path given was: ' + Path);

  { Cache directory returned by ApplicationDataCore. This has two reasons:
    1. On Unix GetApplicationDataPath makes three DirectoryExists calls,
       so it's not too fast, avoid calling it often.
    2. It would be strange if ApplicationDataCore results
       suddenly changed in the middle of the program (e.g. because user just
       made appropriate symlink or such).
       The only case where we allow it is by ApplicationDataOverride. }

  if not ApplicationDataIsCache then
  begin
    ApplicationDataCache :=
      {$if defined(CASTLE_NINTENDO_SWITCH)}
        'castle-nx-contents:/'
      {$elseif defined(ANDROID)}
        'castle-android-assets:/'
      {$else}
        FilenameToUriSafe(GetApplicationDataPath)
      {$endif}
    ;
    WritelnLog('Path', Format('Program data path detected as "%s"', [ApplicationDataCache]));
    ApplicationDataIsCache := true;
  end;

  Result := ApplicationDataCache + Path;
end;

function ResolveCastleDataUrl(const Url: String): String;

  { Fix case for the Url relative to data. }
  function FixCase(const RelativeToData: String): String;
  var
    Parts: TCastleStringList;
    H: TFixCaseHandler;
    ParentUrl: String;
    I: Integer;
  begin
    ParentUrl := UriIncludeSlash(ApplicationDataCore(''));
    Result := '';

    H := TFixCaseHandler.Create;
    try
      Parts := SplitString(InternalUriUnescape(RelativeToData), '/');
      try
        for I := 0 to Parts.Count - 1 do
        begin
          H.SearchName := Parts[I];
          H.FoundName := '';
          FindFiles(ParentUrl, '*', { assume that even final part may be directory } true, @FixCaseCallback, H, []);
          if H.FoundName = '' then
            raise Exception.CreateFmt('Cannot find file "%s" in directory "%s" (even with case-insensitive search, because CastleDataIgnoreCase was used)', [
              H.SearchName,
              ParentUrl
            ]);
          ParentUrl := SAppendPart(ParentUrl, '/', H.FoundName);
          Result := SAppendPart(Result, '/', H.FoundName);
        end;
      finally FreeAndNil(Parts) end;
    finally FreeAndNil(H) end;

    // if RelativeToData ends with slash, also make Result end with slash
    if IsSuffix('/', RelativeToData) then
      Result := UriIncludeSlash(Result);
  end;

var
  U: TUri;
  RelativeToData: String;
begin
  if UriProtocol(Url) = 'castle-data' then
  begin
    U := ParseUri(Url);
    RelativeToData := PrefixRemove('/', U.Path + U.Document, false);
    {$warnings off} // don't warn that CastleDataIgnoreCase is experimental
    if CastleDataIgnoreCase and FileNameCaseSensitive then
    {$warnings on}
      RelativeToData := FixCase(RelativeToData);
    Result := ApplicationDataCore(RelativeToData);
  end else
    Result := Url;
end;

function RelativeToCastleDataUrl(const Url: String; out WasInsideData: Boolean): String;
var
  FullUrl, DataUrl: String;
begin
  FullUrl := ResolveCastleDataUrl(AbsoluteUri(Url));
  DataUrl := ResolveCastleDataUrl('castle-data:/');
  WasInsideData := IsPrefix(DataUrl, FullUrl, true);
  if WasInsideData then
    Result := PrefixRemove(DataUrl, FullUrl, true)
  else
    Result := Url;
end;

initialization
finalization
  FreeAndNil(FUriMimeExtensions);
end.
