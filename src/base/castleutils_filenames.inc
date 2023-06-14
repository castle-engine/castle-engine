{%MainUnit castleutils.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Operations on filenames (i.e. they don't TOUCH, even read, real files) }

{$ifdef read_interface}

const
  { Root dir name. Empty if not applicable to this OS. }
  RootDir = {$ifdef UNIX} '/' {$endif}
            {$ifdef MSWINDOWS} '' {$endif} ;

  ExeExtension = {$ifdef MSWINDOWS} '.exe' {$else} '' {$endif};

  {$ifndef FPC}
  AllowDirectorySeparators = {$ifdef MSWINDOWS} ['/', '\'] {$else} ['/'] {$endif};
  FileNameCaseSensitive = {$ifdef MSWINDOWS} false {$else} true {$endif};
  {$endif}

{ Remove from the FileName the last extension (including the dot).
  Note that if the FileName had a couple of extensions (e.g. @code(blah.x3d.gz))
  this will remove only the last one.
  Will remove nothing if filename has no extension.

  @italic(It is not adviced to use this function,
  better to operate on URLs and MIME types instead of filenames
  and extensions, see CastleURIUtils.) }
function DeleteFileExt(const FileName: string): string;

{ Extracts @italic(two) last extensions from the filename, if it has two extensions.
  If the filename has only one extension, returns that one extension,
  if the filename has no extension --- returns empty string,
  similar to ExtractFileExt.

  This is useful to detect file types from filenames like @code(model.x3d.gz),
  where ExtractFileExt returns only @code(.gz).
  This function will return @code(.x3d.gz).

  @italic(It is not adviced to use this function,
  better to operate on URLs and MIME types instead of filenames
  and extensions, see CastleURIUtils.) }
function ExtractFileDoubleExt(const FileName: string): string;

{ Extracts from FileName the name of file, without directory,
  without last extension and without any Windows drive letter.

  @deprecated Deprecated, since we use URLs everywhere and also
  because this has very low usage. Use DeleteURIExt(ExtractURIName(URL))
  if you really need to. }
function ExtractOnlyFilename(const FileName: string): string; deprecated;

{ Returns FileName with directory (path) part replaced
  with given NewPath. NewPath @bold(must) contain trailing PathDelim.

  @deprecated Deprecated, since we use URLs everywhere and also
  because this has very low usage. }
function ChangeFilePath(const FileName, NewPath: string): string; deprecated;

{ Include / exclude the last path delimiter, if necessary.
  These are just comfortable shorter names for
  IncludeTrailingPathDelimiter and ExcludeTrailingPathDelimiter.

  For Delphi, we also fix treatment of slash on Windows:
  it should be treated as a path delimiter, as Windows accepts
  both slashes and backslashes.

  @groupBegin }
function InclPathDelim(const s: string): string;
function ExclPathDelim(const s: string): string;
{ @groupEnd }

{ Check is the given Path absolute.

  Path may point to directory or normal file,
  it doesn't matter. Also it doesn't matter whether Path ends with PathDelim or not.

  Note for Windows: while it's obvious that @code('c:\autoexec.bat') is an
  absolute path, and @code('autoexec.bat') is not, there's a question
  whether path like @code('\autoexec.bat') is absolute? It doesn't specify
  drive letter, but it does specify full directory hierarchy on some drive.
  This function treats this as @italic(not absolute), on the reasoning that
  "not all information is contained in Path".

  @seealso IsPathAbsoluteOnDrive }
function IsPathAbsolute(const Path: string): boolean;

{ Just like IsPathAbsolute, but on Windows accepts also paths that specify
  full directory tree without drive letter.

  @seealso IsPathAbsolute }
function IsPathAbsoluteOnDrive(const Path: string): boolean;

{ Checks is the directory name special, like "." or "..".

  The precise definition of "special" is that you cannot ever create
  or even have any filenames / directories named like this. }
function SpecialDirName(const DirectoryName: string): boolean;

{ Add Suffix to the filename, right before extension.
  Returns @code(DeleteFileExt(FileName) + Suffix + ExtractFileExt(FileName)). }
function AppendToFilename(const FileName, Suffix: string): string;

{$endif read_interface}

{$ifdef read_implementation}

function DeleteFileExt(const FileName: string): string;
begin
  Result := ChangeFileExt(FileName, '');
end;

function ExtractFileDoubleExt(const FileName: string): string;
begin
  Result := ExtractFileExt(FileName);
  if Result <> '' then
    Result := ExtractFileExt(ChangeFileExt(FileName, '')) + Result;
end;

function ExtractOnlyFileName(const FileName: string): string;
begin
 result := DeleteFileExt(ExtractFileName(FileName));
end;

function ChangeFilePath(const FileName, NewPath: string): string;
begin
 result := NewPath + ExtractFileName(FileName);
end;

function InclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS) and not defined(FPC)}
  { On Windows, also accept / as final path delimiter.
    FPC does it automatically. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(S);
  {$endif}
  Result := IncludeTrailingPathDelimiter(S);
end;

function ExclPathDelim(const s: string): string;
begin
  {$if defined(MSWINDOWS) and not defined(FPC)}
  { On Windows, also accept / as final path delimiter.
    FPC does it automatically. }
  if (S <> '') and (S[Length(S)] = '/') then Exit(Copy(S, 1, Length(S) - 1));
  {$endif}
  Result := ExcludeTrailingPathDelimiter(S);
end;

{ This is also defined in CastleStringUtils publicly,
  but we don't want to use CastleStringUtils here, to keep our dependencies simple. }
function SCharIs(const s: string; index: integer; c: char): boolean; overload;
begin
  Result := (index <= Length(s)) and (s[index] = c);
end;

type
  TSetOfChars = SysUtils.TSysCharSet;

function SCharIs(const S: String; Index: Integer; const Chars: TSetOfChars): Boolean; overload;
begin
  Result := (Index <= Length(s)) and CharInSet(S[index], Chars);
end;

function IsPathAbsolute(const Path: string): boolean;
begin
  Result := {$ifdef UNIX} SCharIs(Path, 1, PathDelim) {$endif}
            {$ifdef MSWINDOWS} SCharIs(Path, 2, DriveDelim) {$endif};
end;

function IsPathAbsoluteOnDrive(const Path: string): boolean;
begin
  Result := IsPathAbsolute(Path)
    {$ifdef MSWINDOWS} or SCharIs(Path, 1, AllowDirectorySeparators) {$endif}
end;

function SpecialDirName(const DirectoryName: string): boolean;
begin
  Result := (DirectoryName = '.') or (DirectoryName = '..');
end;

function AppendToFilename(const FileName, Suffix: string): string;
begin
  { Could be optimized, as DeleteFileExt and ExtractFileExt
    do almost the same work twice. }
  Result := DeleteFileExt(FileName) + Suffix + ExtractFileExt(FileName);
end;

{$endif read_implementation}
