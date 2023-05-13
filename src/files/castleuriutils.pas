{
  Copyright 2007-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ URI utilities. These extend standard FPC URIParser unit. }
unit CastleURIUtils;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  CastleStringUtils;

{ Extracts #anchor from URI. On input, URI contains full URI.
  On output, Anchor is removed from URI and saved in Anchor.
  If no #anchor existed, Anchor is set to ''.

  When RecognizeEvenEscapedHash, we also recognize as a delimiter
  escaped hash, %23. This is a hack and should not be used (prevents
  from using actual filename with hash, thus making the escaping process
  useless). Unless there's no other sensible way --- e.g. specify
  Spine skin name when opening Spine json file... }
procedure URIExtractAnchor(var URI: string; out Anchor: string;
  const RecognizeEvenEscapedHash: boolean = false);

{ Like URIExtractAnchor, but URI remains unchanged. }
procedure URIGetAnchor(const URI: string; out Anchor: string;
  const RecognizeEvenEscapedHash: boolean = false);

{ Calculate #anchor from an URI, and split it into a key-value map.

  This supports special CGE syntax within URL anchor to specify loading parameters for
  @url(https://castle-engine.io/spine Spine),
  @url(https://castle-engine.io/sprite_sheets sprite sheets),
  @url(https://castle-engine.io/using_images images).

  On output, the key-value pairs from anchor are saved in TStringStringMap.
  The SettingsFromAnchor is always cleared at the beginning.
  If no anchor existed, SettingsFromAnchor will be empty when this ends. }
procedure URIGetSettingsFromAnchor(const URI: string;
  const SettingsFromAnchor: TStringStringMap);

{ Return URI with anchor (if was any) stripped. }
function URIDeleteAnchor(const URI: string;
  const RecognizeEvenEscapedHash: boolean = false): string;

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
function RawURIDecode(const S: string): string;

{ Get protocol from given URI.

  This is very similar to how URIParser.ParseURI function detects the protocol,
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

      Our URIProtocol will answer that protocol is empty for @code(c:\blah.txt).
      Which means no protocol, so our engine will treat it as a filename.
      (In contrast with URIParser.ParseURI that would detect protocol called "c".)
      See doc/uri_filename.txt in sources for more comments about differentiating
      URI and filenames in our engine.)

    @item(We always return lowercase protocol. This is comfortable,
      since you almost always calculate protocol to compare it,
      and protocol names are not case-sensitive,
      and you should always produce URLs with lowercase protocol names
      (see http://tools.ietf.org/html/rfc3986#section-3.1).)
  )
}
function URIProtocol(const URI: string): string;

{ Check does URI contain given Protocol.
  This is equivalent to checking URIProtocol(S) = Protocol, ignoring case,
  although may be a little faster. Given Protocol string cannot contain
  ":" character. }
function URIProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;

{ Remove the protocol part from URI. }
function URIDeleteProtocol(const S: string): string;
  deprecated 'use ParseURI to extract URI.Path + URI.Document, instead of this routine that doesn''t do decoding';

{ Is the S a valid protocol scheme.

  Following https://datatracker.ietf.org/doc/html/rfc3986 ,
  protocol scheme must

  @unorderedList(
    @item(begin with an (ASCII) letter)

    @item(and be followed by any combination of (ASCII) letters, digits, plus ("+"), period ("."), or hyphen ("-").)
  )
}
function URIValidProtocol(const P: String): Boolean;

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
  like AbsoluteURI and @link(Download), treat strings without protocol
  as a filename (so it's not percent-escaped, it uses PathDelim
  specific to OS --- slash or backslash etc.).
  This routine, on the other hand, treats Relative string always as an
  URI (when it doesn't include protocol, it just means it's relative to Base). }
function CombineURI(const Base, Relative: string): string;

{ Make sure that the URI is absolute (always has a protocol).
  This function treats an URI without a protocol as a simple filename
  (absolute or relative to the current directory).
  This includes treating empty string as equivalent to current directory. }
function AbsoluteURI(const URI: string): string;

{ Does URI contain only an absolute filename.
  Useful to detect unwanted paths in data files,
  you usually do not want to have such paths in data files,
  as they make it impossible to transfer the data (move/copy files)
  to other system/location. }
function AbsoluteFileURI(const URI: string): boolean;

{ Convert URI (or filename) to a filename.

  This is an improved URIToFilename from URIParser.
  When URI is already a filename, this does a better job than URIToFilename,
  as it handles also Windows absolute filenames (see URIProtocol).
  Returns empty string in case of problems, for example when this is not
  a file URI.

  Just like URIParser.URIToFilename, this percent-decodes the parameter.
  For example, @code(%4d) in URI will turn into letter @code(M) in result.

  It also handles our castle-data: protocol.}
function URIToFilenameSafe(const URI: string): string;

{ Convert filename to URI.

  This is a fixed version of URIParser.FilenameToURI, that correctly
  percent-encodes the parameter, making it truly a reverse of
  URIToFilenameSafe. In FPC > 2.6.2 URIParser.FilenameToURI will also
  do this (after Michalis' patch, see
  http://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=24321 ).

  It also makes sure the filename is absolute (it uses ExpandFileName,
  so if the FileName is relative --- it will be expanded, treating it
  as relative to the current directory). }
function FilenameToURISafe(FileName: string): string;

{ Tries change URI to use castle-data: protocol.
  It's used in our editor to change absolute paths to relative to castle-data
  directory. }
function MaybeUseDataProtocol(const URL: String): String;

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
  mappings to the @link(URIMimeExtensions).

  @groupBegin }
function URIMimeType(const URI: string): string; overload;
function URIMimeType(const URI: string; out Gzipped: boolean): string; overload;
{ @groupEnd }

{ Map from an extension to a MIME type, used by @link(URIMimeType).
  The extension should be lowercase, and includes a leading dot, like @code(.png). }
function URIMimeExtensions: TStringStringMap;

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
  Usually, you want to call URICaption that makes sure that argument is URL
  (using AbsoluteURI) and then returns URIDisplay with Short=true.

  It is safe to use this on both absolute and relative URLs.
  It does not resolve relative URLs in any way.
  It also means that it returns empty string for empty URI
  (contrary to most other routines that convert empty string
  to a current directory when resolving relative URLs). }
function URIDisplay(const URI: string; const Short: boolean = false): string;

{ Convert URI to a nice form for a short caption.

  Returns empty string for empty URI (contrary to most other routines that
  treat empty string like a current directory).

  See URIDisplay documentation for details.
  This calls URIDisplay with Short = @true. }
function URICaption(const URI: string): string;

{ Change extension of the URL. }
function ChangeURIExt(const URL, Extension: string): string;

{ Delete extension of the URL. }
function DeleteURIExt(const URL: string): string;

{ Extract filename (last part after slash) from URL. }
function ExtractURIName(const URL: string): string;

{ Extract path (everything before last part), including final slash, from URL. }
function ExtractURIPath(const URL: string): string;

{ Ensure URL ends with slash.

  For an empty URL, returns empty string (so it does not turn "" into "/").
  For an URL ending with bashslash (which usually means you passed Windows
  path name), it removes the backslash before adding slash.

  This should be used instead of InclPathDelim or IncludeTrailingPathDelimiter,
  when you use URLs instead of filenames. }
function URIIncludeSlash(const URL: String): String;

{ Ensure URL does not end with slash.
  In case you passed Windows path name, it also removes the backslash.

  This should be used instead of ExclPathDelim or ExcludeTrailingPathDelimiter,
  when you use URLs instead of filenames. }
function URIExcludeSlash(const URL: String): String;

{ Does a file exist, that is: whether it makes sense to load it with
  the @link(Download) function.

  Returns @true for URLs where we cannot determine whether the file exists
  (like http / https).

  This is simply a shortcut for @code(URIExists(URL) in [ueFile, ueUnknown]). }
function URIFileExists(const URL: string): Boolean;

type
  { Result of the @link(URIExists) query. }
  TURIExists = (
    { Given path does not indicate either a file or directory. }
    ueNotExists,

    { Given path is a regular file.
      In particular, this means it can be read with the @link(Download) function.

      Note that there is no @italic(guarantee) that opening it will work.
      On a multi-process system the file can be always deleted between the call
      to URIExists and Download.
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
        Stream := Download(URL);
        FreeAndNil(Stream);
        ItExists := true;
      except
        on E: Exception do
        begin
          WritelnLog('Opening URL %s failed with exception %s', [
            URICaption(URL),
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
  See TURIExists for possible return values. }
function URIExists(URL: string): TURIExists;

{ Current working directory of the application, expressed as URL,
  including always final slash at the end. }
function URICurrentPath: string;

{ If this is castle-data:... URL, resolve it using ApplicationData. }
function ResolveCastleDataURL(const URL: String): String;

{ If this URL indicates something inside the @url(https://castle-engine.io/manual_data_directory.php
  CGE data directory) then return URL relative to this data directory.
  E.g. for "castle-data:/foo/bar.txt" it returns "foo/bar.txt".

  It accepts any URL, relative (to the current working directory)
  or absolute (with any protocol).
  It works when the URL starts with castle-data:/ protocol,
  it works when the URL starts with other (like file:/) protocol
  that still points to a file inside data.

  If the URL does not point to a file in data, it is returned untouched. }
function RelativeToCastleDataURL(const URL: String; out WasInsideData: Boolean): String;

{ Encode string by using percent encoding (https://en.wikipedia.org/wiki/Percent-encoding)
  @exclude }
function InternalUriEscape(const S: String; const Allowed: TSysCharSet): String;

{ Decode string encoded by percent encoding (https://en.wikipedia.org/wiki/Percent-encoding )
  @exclude }
function InternalUriUnescape(const S: String): String;

var
  { On systems where filesystems are usually case-sensitive (Unix),
    accept also 'castle-data:/xxx` URLs that have different case than the actual files.

    @bold(This is not advised to be used in production applications.)
    It is slow (searching for case-insensitive match at each subdirectory and filename).
    But it is a quick way to run applications prepared for case-insensitive
    systems (like Windows) on Unix. }
  CastleDataIgnoreCase: Boolean = false;

implementation

uses URIParser,
  CastleUtils, CastleInternalDataUri, CastleLog, CastleFilesUtils,
  CastleInternalDirectoryInformation, CastleFindFiles, CastleDownload
  {$ifdef CASTLE_NINTENDO_SWITCH}, CastleInternalNxBase {$endif}
  {$ifndef FPC}, Character{$endif};

{ Escape and Unescape --------------------------------------------------------
  Copied from URIParser and fixed for Delphi, as they are internal there.
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

function InternalUriEscape(const S: String; const Allowed: TSysCharSet): String;
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

{ other routines ------------------------------------------------------------- }

procedure URIGetAnchor(const URI: string; out Anchor: string;
  const RecognizeEvenEscapedHash: boolean = false);
var
  U: String;
begin
  U := URI;
  URIExtractAnchor(U, Anchor, RecognizeEvenEscapedHash);
end;

procedure URIExtractAnchor(var URI: string; out Anchor: string;
  const RecognizeEvenEscapedHash: boolean);
var
  HashPos: Integer;
begin
  Anchor := '';

  { Avoid extracting anchor from data URI, to avoid touching things like
      data:model/x3d+vrml,#X3D V3.2 utf8
      ...
    which are used to embed classic VRML/X3D content.
    The hash in data URI is *not* an anchor. }

  if TDataURI.IsDataURI(URI) then
    Exit;

  HashPos := BackPos('#', URI);
  if HashPos <> 0 then
  begin
    Anchor := SEnding(URI, HashPos + 1);
    SetLength(URI, HashPos - 1);
  end else
  if RecognizeEvenEscapedHash then
  begin
    HashPos := BackPos('%23', URI);
    if HashPos <> 0 then
    begin
      Anchor := SEnding(URI, HashPos + 3);
      SetLength(URI, HashPos - 1);
    end;
  end;
end;

procedure URIGetSettingsFromAnchor(const URI: string;
  const SettingsFromAnchor: TStringStringMap);
var
  URLForDisplay: String;

  procedure ProcessAnchorPart(const Part: String);
  var
    Semicolon: Integer;
    PartName, PartValue: String;
  begin
    Semicolon := Pos(':', Part);

    if Semicolon = 0 then
    begin
      WritelnWarning('Empty setting (%s) in anchor of "%s"', [Part, URLForDisplay]);
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
  URLForDisplay := URIDisplay(URI);

  { We need recognize escaped hash because GTK2 open dialog returns %23
    in # position }
  URIGetAnchor(URI, Anchor, true);
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

function URIDeleteAnchor(const URI: string;
  const RecognizeEvenEscapedHash: boolean): string;
var
  Anchor: string;
begin
  Result := URI;
  URIExtractAnchor(Result, Anchor, RecognizeEvenEscapedHash);
end;

function RawURIDecode(const S: string): string;

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
  { These constants match URIParser algorithm, which in turn follows RFC. }
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  ProtocolFirstChar = ALPHA;
  ProtocolChar = ALPHA + DIGIT + ['+', '-', '.'];

function URIValidProtocol(const P: String): Boolean;
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
function URIProtocolIndex(const S: string; out FirstCharacter, Colon: Integer): boolean;
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

function URIProtocol(const URI: string): string;
var
  FirstCharacter, Colon: Integer;
begin
  if URIProtocolIndex(URI, FirstCharacter, Colon) then
    Result := LowerCase(CopyPos(URI, FirstCharacter, Colon - 1)) else
    Result := '';
end;

function URIProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;
var
  FirstCharacter, I: Integer;
begin
  Result := false;
  if URIProtocolIndex(S, FirstCharacter, Colon) and
     (Colon - FirstCharacter = Length(Protocol)) then
  begin
    for I := 1 to Length(Protocol) do
      if LoCase(Protocol[I]) <> LoCase(S[I - FirstCharacter + 1]) then
        Exit;
    Result := true;
  end;
end;

function URIDeleteProtocol(const S: string): string;
var
  FirstCharacter, Colon: Integer;
begin
  if URIProtocolIndex(S, FirstCharacter, Colon) then
    { Cut off also whitespace before FirstCharacter }
    Result := SEnding(S, Colon + 1) else
    Result := S;
end;

function CombineURI(const Base, Relative: string): string;
// var
//   RelativeProtocol: string;
begin
  { Test for some special protocols first, that may have whitespace before
    the protocol name. }
  { This is not necessary anymore -- check below for URIProtocol(Relative) <> ''
    will handle this case anyway.
  RelativeProtocol := URIProtocol(Relative);
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
  if (URIProtocol(Base) = 'castle-data') and IsPrefix('../', Relative) then
    Exit(CombineURI(ResolveCastleDataURL(Base), Relative));

  { Relative is already an absolute URL, no point in doing anything,
    in particular no point for doing AbsoluteURI(Base) below,
    which could fail on NX in case Base='', calling ExpandFileName
    is not allowed on NX. }
  if URIProtocol(Relative) <> '' then
    Exit(Relative);

  try
    if not ResolveRelativeURI(AbsoluteURI(Base), Relative, Result) then
    begin
      { The only case when ResolveRelativeURI may fail is when neither argument
        contains a protocol. But we just used AbsoluteURI, which makes sure
        that AbsoluteURI(Base) has some protocol. }
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

function AbsoluteURI(const URI: string): string;
begin
  if URIProtocol(URI) = '' then
    Result := FilenameToURISafe(URI) else
    Result := URI;
end;

function AbsoluteFileURI(const URI: string): boolean;
begin
  Result := (URIProtocol(URI) = '') and IsPathAbsoluteOnDrive(URI);
end;

function URIToFilenameSafe(const URI: string): string;
var
  P, CastleDataResolved: string;
begin
  { Use our URIProtocol instead of depending that URIToFilename will detect
    empty protocol case correctly. This allows to handle Windows absolute
    filenames like "c:\foo" as filenames. }
  P := URIProtocol(URI);
  if P = '' then
    Result := URI
  else
  if P = 'file' then
  begin
    try
      if not URIToFilename(URI, Result) then Result := '';
    except
      { workaround http://bugs.freepascal.org/view.php?id=28496 , see also
        https://sourceforge.net/p/castle-engine/tickets/35/ }
      on E: EConvertError do
      begin
        WritelnWarning('URL', Format('Error when parsing URL. This usually indicates an incorrect Windows "file:" URL (it should have *three* slashes, like "file:///c:/blah..."): "%s"',
          [URI]));
        Result := '';
      end;
    end;
  end else
  if P = 'castle-data' then
  begin
    CastleDataResolved := ResolveCastleDataURL(URI);
    if URIProtocol(CastleDataResolved) = 'castle-data' then
      raise EInternalError.CreateFmt('ResolveCastleDataURL cannot return URL with castle-data protocol. This probably indicates that ApplicationDataOverride (%s) contains castle-data protocol, which it should not.', [
        ApplicationDataOverride
      ]);
    Result := URIToFilenameSafe(CastleDataResolved);
  end else
    Result := '';
end;

function FilenameToURISafe(FileName: string): string;

{ Code adjusted from FPC FilenameToURI (same license as our engine,
  so it's Ok to share code). Adjusted to call Escape on FileName.
  See http://bugs.freepascal.org/view.php?id=24324 : FPC FilenameToURI
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

const
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];
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
  FilenamePart := InternalUriEscape(FilenamePart, ValidPathChars);

  Result := Result + FilenamePart;
end;

var
  FURIMimeExtensions: TStringStringMap;

function URIMimeExtensions: TStringStringMap;
begin
  if FURIMimeExtensions = nil then
    FURIMimeExtensions := TStringStringMap.Create;
  Result := FURIMimeExtensions;
end;

function URIMimeType(const URI: string; out Gzipped: boolean): string;
begin
  Result := InternalURIMimeType(URI, Gzipped);
end;

function URIMimeType(const URI: string): string;
var
  Gzipped: boolean;
begin
  Result := URIMimeType(URI, Gzipped);
end;

function MaybeUseDataProtocol(const URL: String): String;
var
  DataPath: String;
begin
  { Use below ResolveCastleDataURL, to get real location of data,
    e.g. resolved to file:// on normal desktop. }
  DataPath := ResolveCastleDataURL('castle-data:/');
  if IsPrefix(DataPath, URL, not FileNameCaseSensitive) then
    Result := 'castle-data:/' + PrefixRemove(DataPath, URL, not FileNameCaseSensitive)
  else
    Result := URL;
end;

function URIDisplay(const URI: string; const Short: boolean): string;
var
  DataURI: TDataURI;
  NewLinePos: Integer;
  Parsed: TURI;
begin
  Result := Trim(URI);

  if TDataURI.IsDataURI(URI) then
  begin
    DataURI := TDataURI.Create;
    try
      DataURI.URI := URI;
      if DataURI.Valid then Result := DataURI.URIPrefix + ',...';
    finally FreeAndNil(DataURI) end;
  end else

  begin
    NewLinePos := CharsPos([#10, #13], Result);
    if NewLinePos <> 0 then
    begin
      { we have done Trim(URI) to prevent starting from newline }
      Assert(NewLinePos <> 1);
      Result := Copy(Result, 1, NewLinePos - 1) + '...';
    end else
    if Short then
    begin
      { try to extract last path component }
      try
        Parsed := ParseURI(URI);
      except
        on E: EConvertError do
        begin
          WritelnWarning('URL', Format('Error when parsing URI. This usually indicates an incorrect Windows "file:" URL (it should have *three* slashes, like "file:///c:/blah..."): "%s"',
            [URI]));
          Parsed.Document := ExtractURIName(URI);
        end;
      end;

      Parsed.Document := Trim(Parsed.Document);
      if Parsed.Document <> '' then
        Result := Parsed.Document;
    end;
  end;
end;

function URICaption(const URI: string): string;
begin
  if URI = '' then
    Result := '' else
    Result := URIDisplay(AbsoluteURI(URI), true);
end;

const
  { RecognizeEvenEscapedHash value for URI extracting functions below. }
  DefaultRecognizeEvenEscapedHash = true;

function ChangeURIExt(const URL, Extension: string): string;
var
  URLWithoutAnchor, Anchor: String;
begin
  URLWithoutAnchor := URL;
  URIExtractAnchor(URLWithoutAnchor, Anchor, DefaultRecognizeEvenEscapedHash);
  Result := ChangeFileExt(URLWithoutAnchor, Extension);
  if Anchor <> '' then
    Result := Result + '#' + Anchor;
end;

function DeleteURIExt(const URL: string): string;
begin
  Result := ChangeURIExt(URL, '');
end;

function ExtractURIName(const URL: string): string;
var
  URLWithoutAnchor: String;
  {$ifndef FPC} I: Integer; {$endif}
begin
  URLWithoutAnchor := URIDeleteAnchor(URL, DefaultRecognizeEvenEscapedHash);
  {$ifdef FPC}
  Result := ExtractFileName(URLWithoutAnchor);
  {$else}
  { In Delphi, / separator in paths is not recognized, so we cannot use ExtractFilePath.
    TODO: our own solution should be just used for both compilers.
    Need autotests to confirm it behaves the same, on both platforms. }

  I := BackCharsPos(['/'], URLWithoutAnchor);
  if I <> 0 then
    Result := SEnding(URLWithoutAnchor, I + 1)
  else
    Result := URLWithoutAnchor;
  {$endif}
end;

function ExtractURIPath(const URL: string): string;
var
  URLWithoutAnchor: String;
  {$ifndef FPC} I: Integer; {$endif}
begin
  { While on non-Windows ExtractFilePath would work on full URL as well,
    but on Windows the ":" inside anchor (like
    "castle-data:/starling/character_zombie_atlas.starling-xml#fps:8,anim-naming:strict-underscore")
    would cause trouble: it would be considered a drive letter separator,
    and change the result. }
  URLWithoutAnchor := URIDeleteAnchor(URL, DefaultRecognizeEvenEscapedHash);
  {$ifdef FPC}
  Result := ExtractFilePath(URLWithoutAnchor);
  {$else}
  { In Delphi, / separator in paths is not recognized, so we cannot use ExtractFilePath.
    TODO: our own solution should be just used for both compilers.
    Need autotests to confirm it behaves the same, on both platforms. }

  I := BackCharsPos(['/'], URLWithoutAnchor);
  if I <> 0 then
    Result := Copy(URLWithoutAnchor, 1, I)
  else
    Result := URLWithoutAnchor;
  {$endif}
end;

function URIIncludeSlash(const URL: String): String;
var
  L: Integer;
begin
  if URL = '' then
    Exit('');

  L := Length(URL);
  case URL[L] of
    '/': Result := URL; // nothing needs to be done
    '\': Result := Copy(URL, 1, L - 1) + '/';
    else Result := URL + '/';
  end;
end;

function URIExcludeSlash(const URL: String): String;
var
  L: Integer;
begin
  L := Length(URL);
  if (L <> 0) and CharInSet(URL[L], ['/', '\']) then
    Result := Copy(URL, 1, L - 1)
  else
    Result := URL;
end;

function URIFileExists(const URL: string): Boolean;
begin
  Result := URIExists(URL) in [ueFile, ueUnknown];
end;

function URIExists(URL: string): TURIExists;

  // Detect existence of castle-data:/xxx URL using DataDirectoryInformation.
  function UseDataDirectoryInformation(const URL: string): TURIExists;
  var
    U: TURI;
    URLPath: String;
    PathEntry: TDirectoryInformation.TEntry;
  begin
    U := ParseURI(URL);
    URLPath := PrefixRemove('/', U.Path + U.Document, false);
    PathEntry := DataDirectoryInformation.FindEntry(URLPath);
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
  function UseNXExists(const URL: string): TURIExists;
  begin
    Result := NXFileExists(URL);
  end;
  {$endif CASTLE_NINTENDO_SWITCH}

  // Detect existence of a filename using FileExists, DirectoryExists.
  function UseFileDirectoryExists(const FileName: String): TURIExists;
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
  if TDataURI.IsDataURI(URL) then
    Exit(ueFile);

  P := URIProtocol(URL);

  if (P = 'castle-data') and
     (DisableDataDirectoryInformation = 0) and
     (DataDirectoryInformation <> nil) then
    Exit(UseDataDirectoryInformation(URL));

  { Resolve castle-data:/xxx now.
    This way we can work in case we have castle-data:/xxx URL that resolves
    to something handled below (like file:/xxx) but wasn't handled above
    (e.g. because DataDirectoryInformation = nil). }
  URL := ResolveCastleDataURL(URL);
  P := URIProtocol(URL);

  {$ifdef CASTLE_NINTENDO_SWITCH}
  if (P = 'castle-nx-contents') or
     (P = 'castle-nx-save') then
    Exit(UseNXExists(URL));
  {$endif CASTLE_NINTENDO_SWITCH}

  if (P = '') or (P = 'file') then
    Exit(UseFileDirectoryExists(URIToFilenameSafe(URL)));

  Result := ueUnknown;
end;

function URICurrentPath: string;
begin
  Result := FilenameToURISafe(InclPathDelim(GetCurrentDir));
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
        ExtractURIPath(FileInfo.URL),
        FileInfo.Name,
        H.FoundName
      ]);
    H.FoundName := FileInfo.Name;
  end;
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
    ParentUrl := URIIncludeSlash(ApplicationData(''));
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
      Result := URIIncludeSlash(Result);
  end;

var
  U: TURI;
  RelativeToData: String;
begin
  if URIProtocol(Url) = 'castle-data' then
  begin
    U := ParseURI(Url);
    RelativeToData := PrefixRemove('/', U.Path + U.Document, false);
    {$warnings off} // don't warn that CastleDataIgnoreCase is experimental
    if CastleDataIgnoreCase and FileNameCaseSensitive then
    {$warnings on}
      RelativeToData := FixCase(RelativeToData);
    Result := ApplicationData(RelativeToData);
  end else
    Result := Url;
end;

function RelativeToCastleDataURL(const Url: String; out WasInsideData: Boolean): String;
var
  FullUrl, DataUrl: String;
begin
  FullUrl := ResolveCastleDataURL(AbsoluteURI(Url));
  DataUrl := ResolveCastleDataURL('castle-data:/');
  WasInsideData := IsPrefix(DataUrl, FullUrl, true);
  if WasInsideData then
    Result := PrefixRemove(DataUrl, FullUrl, true)
  else
    Result := Url;
end;

initialization
finalization
  FreeAndNil(FURIMimeExtensions);
end.
