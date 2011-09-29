{
  Copyright 2002-2011 Michalis Kamburelis.

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

{ Remove from the FileName the last extension (including the dot).
  Note that if the FileName had a couple of extensions (e.g. @code(blah.x3d.gz))
  this will remove only the last one.
  Will remove nothing if filename has no extension. }
function DeleteFileExt(const FileName: string): string;

{ Extracts from FileName the name of file, without directory,
  without last extension and without any Windows drive letter. }
function ExtractOnlyFilename(const FileName: string): string;

{ Returns FileName with directory (path) part replaced
  with given NewPath. NewPath @bold(must) contain trailing PathDelim. }
function ChangeFilePath(const FileName, NewPath: string): string;

{ Duplicates (makes them double) the PathDelim characters in the string. }
function DuplicateSlash(const s: string): string;

{ Checks is C valid path delimiter under this platform.

  For all platforms this just compares C with PathDelim,
  and under Windows additionally accepts also C = '/'.
  (Since under Windows the Unix-like '/' is not standard PathDelim,
  but it's accepted by all WinAPI functions and programs,
  and it's comfortable to use if you're accustomed to Unix.) }
function IsPathDelim(c: char): boolean;

{ These do almost the same as standard Include/ExcludeTrailingPathDelimiter
  functions, but have shorter names. So InclPathDelim makes sure
  S has some path delimiter at the end (appends PathDelim if needed),
  and ExclPathDelim makes sure there's no path delimiter at the end
  (strips it if needed).

  The difference from Delphi Include/ExcludeTrailingPathDelimiter
  is that my functions use IsPathDelim, so they take into account
  the fact that under Windows '/' is also a valid path delimiter.
  For FPC, default Include/ExcludeTrailingPathDelimiter already
  take this into account, so my versions are not really useful,
  they only have shorter names.

  Also, my functions do not handle MBCS strings.

  @groupBegin }
function InclPathDelim(const s: string): string;
function ExclPathDelim(const s: string): string;
{ @groupEnd }

{ Forces FileName to be relative (relative from CurrentDir).

  Given FileName may be absolute (then it's directory part will be modified
  accordingly) or already relative (then it will simply be returned
  without any changes). }
function RelativeFilename(const FileName: string): string;

{ Makes FileName relative or absolute, whichever is nicer to show the user.

  Given FileName may be absolute or relative (with respect to CurrentDir).
  This function maybe will convert FileName to absolute filename and
  maybe will convert it to relative filename -- whichever will be
  nicer to show the user.

  It's not guaranteed what exactly this function considers as "nicer for
  the user". Current implementation simply assumes that if FileName
  is under current directory, then relative name is nicer,
  otherwise absolute name is nicer. In other words, it tries to
  make a relative name but without any leading "..".

  Examples:

@preformatted(
  If CurrentDir = '/usr/share/' and
     FileName = '/usr/share/some_file'
     Result will be a relative filename 'some_file'
  If CurrentDir = '/usr/share/' and
     FileName = '/lib/bla/bla'
     Result will be equal to FileName '/lib/bla/bla'
  If CurrentDir = '/usr/share/' and
     FileName = '../../lib/bla/bla'
     Result will be absolute filename '/lib/bla/bla'
) }
function NiceFileName(const FileName: string): string;

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

{ Make path absolute. Same as ExpandFileName, but knows that
  FilePath contains only a path and will always work as it should.
  (Delphi's and FPC's do not guarantee correct ExpandFileName result
  for paths ended with PathDelim.)

  FilePath may, but doesn't have to, end with PathDelim.
  Result will always end with PathDelim.

  Note: '' is treated as current dir (as it always should be, because
  ExtractFilePath('file') = '' and 'file' is in current dir). }
function ExpandFilePath(const FilePath: string): string;

{$endif read_interface}

{$ifdef read_implementation}

function DeleteFileExt(const FileName: string): string;
var
  I: Integer;
begin
  for I := Length(FileName) downto 1 do
  begin
    if FileName[I] = '.' then
    begin
      Result := Copy(FileName, 1, I - 1);
      Exit;
    end else
    if FileName[I] = PathDelim then
      Break;
  end;

  Result := FileName;
end;

function ExtractOnlyFileName(const FileName: string): string;
begin
 result := DeleteFileExt(ExtractFileName(FileName));
end;

function ChangeFilePath(const FileName, NewPath: string): string;
begin
 result := NewPath + ExtractFileName(FileName);
end;

function DuplicateSlash(const s: string): string;
var i: integer;
begin
 result := s;
 i := 1;
 while i <= length(result) do
 begin
  if result[i] = PathDelim then begin Insert(PathDelim, result, i); Inc(i) end;
  Inc(i);
 end;
end;

function IsPathDelim(c: char): boolean;
begin
 result:=(c = PathDelim) {$ifdef MSWINDOWS} or (c = '/') {$endif};
end;

function InclPathDelim(const s: string): string;
var L: Integer;
begin
 L := Length(S);
 if (L = 0) or (not IsPathDelim(S[L])) then result := S+PathDelim else result := S;
end;

function ExclPathDelim(const s: string): string;
var L: Integer;
begin
 L := Length(S);
 if (L > 0) and IsPathDelim(S[L]) then Dec(L);
 Result := Copy(S, 1, L);
end;

function RelativeFilename(const FileName: string): string;
begin
 if IsPathAbsolute(FileName) then
  Result := ExtractRelativePath( InclPathDelim(GetCurrentDir) + 'dummy_file',
    FileName) else
 { TODO: MSWINDOWS - chyba nie trzeba uwzgledniac tu
   IsPathAbsoluteOnDrive(FileName) ? }
  Result := FileName;
end;

function NiceFileName(const FileName: string): string;
var AbsoluteFileName: string;
begin
 AbsoluteFileName := ExpandFileName(FileName);
 { It's important that we end InclPathDelim(GetCurrentDir) and
   ExtractFilePath(AbsoluteFileName) always with PathDelim, it makes sure that
   IsPrefix actually checks is one file in subdirectory of another. }
 if IsPrefix( InclPathDelim(GetCurrentDir), ExtractFilePath(AbsoluteFileName) ) then
  Result := RelativeFilename(FileName) else
  Result := AbsoluteFileName;
end;

function IsPathAbsolute(const Path: string): boolean;
begin
  Result := {$ifdef UNIX} SCharIs(Path, 1, PathDelim) {$endif}
            {$ifdef MSWINDOWS} SCharIs(Path, 2, DriveDelim) {$endif};
end;

function IsPathAbsoluteOnDrive(const Path: string): boolean;
begin
  Result := IsPathAbsolute(Path)
    {$ifdef MSWINDOWS} or SCharIs(Path, 1, PathDelim) {$endif}
end;

function SpecialDirName(const DirectoryName: string): boolean;
begin
 { Microsoft swego czasu straszyl ze w systemie beda zdefiniowane katalogi ...
     i .... jako katalog-dziadek i pradziadek tak jak obecnie sa .. jako rodzic i
     . jako aktualny. Sprawdzilem to i przynajmniej pod win95/98 nie zrobili tego
     bezuzytecznego kretynstwa, ale zawsze lepiej sie zabezpieczyc - stad powyzsze
     checki '...' i '....'. One niczemu nie przeszkadzaja, bo i tak
     ... i .... to nieprawidlowe nazwy plikow pod Windows wiec ten check
     (poza zabieraniem chwilki czasu) nic nie psuje.

   Pod linuxem (i prawdopodobnie kazdym innym nie-Windowsem)
   ... i .... to zupelnie legalne nazwy plikow dlatego
   ten check nie moze istniec pod nie-Windowsem. }
 result := ArrayPosStr(DirectoryName,['.','..' {$ifdef MSWINDOWS},'...','....'{$endif}]) >= 0;
end;

function AppendToFilename(const FileName, Suffix: string): string;
begin
 { I know, this can be optimized, as DeleteFileExt and ExtractFileExt
   do almost the same work twice. }
 Result := DeleteFileExt(FileName) +Suffix +ExtractFileExt(FileName);
end;

function ExpandFilePath(const FilePath: string): string;
begin
 if FilePath = '' then
  Result := InclPathDelim(GetCurrentDir) else
  Result := ExtractFilePath( ExpandFileName(InclPathDelim(FilePath) +
    'dummy_file_name' ));

 { note : doing here something like
     ExtractFilePath( ExpandFileName(ExclPathDelim(FilePath)) )
   (i.e., pretending that FilePath points to a file, not directory)
   would not be safe, because we don't know what ExpandFileName
   will do with 'c:' }
end;

{$endif read_implementation}
