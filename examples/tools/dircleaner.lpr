{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Clean directory, recursively, from trash files (editor backup files,
  compilers temp files etc.).

  Exactly which files are considered "trash"
  is subjective, from author's (Michalis') point of view.
  Before cleaning (dircleaner DIR-NAME clean) be sure to check
  whether it really removes the files you don't want:
  - you can check "dircleaner DIR-NAME print" or
    "dircleaner DIR-NAME nothing" commands, to see which files would be removed.
  - see at the implementation of DefaultFilesToCleanInit and
    DefaultDirsToCleanInit to know default masks for removed files.

}
uses SysUtils, CastleUtils, CastleParameters, CastleFindFiles,
  CastleFilesUtils, CastleStringUtils;

{ Action ------------------------------------------------------------ }

type TAction=(aNothing, aPrint, aClean);
const ActionParStr: array[TAction]of string = ('nothing', 'print', 'clean');
var Action: TAction;

  function ActionParStrToAction(const S: string): TAction;
  begin
    for Result := Low(Result) to High(Result) do
      if ActionParStr[Result] = S then Exit;
    raise Exception.CreateFmt('Invalid action name "%s"', [S]);
  end;

{ global vars ------------------------------------------------------- }

var
  StartPath: string = '.' +PathDelim;
  CleanDirsRecursively: boolean = true;

  { note: we're keeping separate DirsToClean and DefaultDirsToClean
    (instead of just filling DirsToClean on start with default state)
    only so that --help option produce right output
    (e.g. when you call
      dircleaner -f '*.foo' -h
    you want from -h option to write proper default DefaultDirsToClean,
    not (already extended) DirsToClean.
    Same for FilesToClean and DefaultFilesToClean. }
  FilesToClean: TCastleStringList;
  DefaultFilesToClean: TCastleStringList;
  DirsToClean: TCastleStringList;
  DefaultDirsToClean: TCastleStringList;

{ funcs ------------------------------------------------------------ }

var
  FilesCount: Cardinal = 0;
  FilesSize: Cardinal = 0;

procedure CleanFiles_FileProc(const FileInfo: TFileInfo;
  Data: Pointer; var StopSearch: boolean);
begin
  Inc(FilesCount);
  FilesSize += FileInfo.Size;

  case Action of
    aNothing: ;
    aPrint: Writeln(FileInfo.AbsoluteName);
    aClean: CheckDeleteFile(FileInfo.AbsoluteName);
  end;
end;

function MaybeRecursive: TFindFilesOptions;
begin
  if CleanDirsRecursively then
    Result := [ffRecursive] else
    Result := [];
end;

procedure CleanFiles(const Pattern: string);

  function SizeToStr(const Size: Int64): string;
  begin
    { powers of 1000, not 1024.
      https://en.wikipedia.org/wiki/Gigabyte
      https://en.wikipedia.org/wiki/Gibibyte
      http://www.computerhope.com/unix/udu.htm
      https://blogs.gnome.org/cneumair/2008/09/30/1-kb-1024-bytes-no-1-kb-1000-bytes/
    }
    if Size > 1000 * 1000 * 1000 then
      Result := Format('%d.%.2d GB',
        [ Size div (1000 * 1000 * 1000),
         (Size div (  10 * 1000 * 1000)) mod 100]) else
    if Size > 1000 * 1000 then
      Result := Format('%d.%.2d MB',
        [ Size div (1000 * 1000),
         (Size div (  10 * 1000)) mod 100]) else
    if Size > 1000 then
      Result := Format('%d.%.2d KB',
        [ Size div (1000),
         (Size div (  10)) mod 100]) else
      Result := Format('%d B', [Size]);
  end;

begin
  FilesCount := 0;
  FilesSize := 0;

  FindFiles(StartPath, Pattern, false, @CleanFiles_FileProc, nil, MaybeRecursive);

  if FilesCount <> 0 then
    Writeln(FilesCount, ' files matching ',Pattern,
      ' (total size: ', SizeToStr(FilesSize), ')');
end;

var DirsCount: Cardinal = 0;

procedure CleanDirs_FileProc(const FileInfo: TFileInfo; Data: Pointer; var StopSearch: boolean);
begin
  if not FileInfo.Directory then Exit;

  if SpecialDirName(ExtractFileName(FileInfo.AbsoluteName)) then Exit;

  Inc(DirsCount);

  case Action of
    aNothing: ;
    aPrint: Writeln(FileInfo.AbsoluteName);
    aClean: RemoveNonEmptyDir(FileInfo.AbsoluteName);
  end;
end;

procedure CleanDirs(const Pattern: string);
begin
  DirsCount := 0;

  FindFiles(StartPath, Pattern, true, @CleanDirs_FileProc, nil, MaybeRecursive);

  if DirsCount <> 0 then
    Writeln(DirsCount, ' dirs matching ', Pattern);
end;

procedure DefaultFilesToCleanInit;
begin
  DefaultFilesToClean.Add('*~');     { emacs, edytorek & inne }
  DefaultFilesToClean.Add('*.~???'); { delphi (windows version) }
  DefaultFilesToClean.Add('*.o');    { gcc, gpc, fpc }
  DefaultFilesToClean.Add('*.dcu');  { delphi }

 { DefaultFilesToClean.Add('*.obj'); }
 { cpp builder, turbo c - BE CAREFUL: it is also a 3d model format ! }

  DefaultFilesToClean.Add('*.tpu');  { turbo pascal }
  DefaultFilesToClean.Add('*.tpp');  { -'- }
  DefaultFilesToClean.Add('*.bak');  { -'- }
  DefaultFilesToClean.Add('*.ppu');     { fpc }
  DefaultFilesToClean.Add('*.ow');      { -'- }
  DefaultFilesToClean.Add('*.ppw');     { -'- }
  DefaultFilesToClean.Add('*.rst');     { -'- }
  DefaultFilesToClean.Add('ppas.bat');  { -'- }
  DefaultFilesToClean.Add('ppas.sh');   { -'- }
  DefaultFilesToClean.Add('*.rsj');     { fpc >= 2.7.1 }
  DefaultFilesToClean.Add('*.compiled');   { Lazarus }
  DefaultFilesToClean.Add('*.or'); { fpc on windows }
  DefaultFilesToClean.Add('fpc-res.res'); { fpc on windows }
  DefaultFilesToClean.Add('.Bpib');    { blender }
  DefaultFilesToClean.Add('*.blend1');  { -'- }
  DefaultFilesToClean.Add('*.blend2');  { -'- }
  DefaultFilesToClean.Add('*.bphys');  { -'- }
  DefaultFilesToClean.Add('*.cmi');  { ocamlc }
  DefaultFilesToClean.Add('*.cmo');  { -'- }
  DefaultFilesToClean.Add('*.cmx');  { -'- }
  DefaultFilesToClean.Add('.DS_Store'); { macOS (Finder?) }

  // commented for now; castle-engine tool should clear them itself.
  // And clearing them here is not good for castle-engine tool sources...:)
  // DefaultFilesToClean.Add('automatic-windows-resources.rc');  { castle-engine tool }
  // DefaultFilesToClean.Add('automatic-windows.manifest'); { -'- }

  // Note: do not remove automatic-windows-resources.res,
  // it would prevent compiling without castle-engine tool
end;

procedure DefaultDirsToCleanInit;
begin
  DefaultDirsToClean.Add('.xvpics'); { gimp }
end;

{ parsing options ------------------------------------------------------ }

const
  Options: array[0..3]of TOption =
  ( (Short:'h'; Long:'help'; Argument: oaNone),
    (Short:'f'; Long:'files'; Argument: oaRequired),
    (Short:'d'; Long:'dirs'; Argument: oaRequired),
    (Short:'n'; Long:'no-recursive'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  var
    i: Integer;
  begin
    case OptionNum of
      0: begin
           Write(
    {        '0123456789012345678901234567890123456789012345678901234567890123456789012345' }
             'dircleaner: cleans given directory of garbage files and dirs,' +nl+
             '  recursively.' +nl+
             'Run as' +nl+
             '  dircleaner (works like dircleaner ./ nothing)' +nl+
             '  dircleaner START-DIR (works like dircleaner START-DIR nothing)' +nl+
             '  dircleaner START-DIR ACTION' +nl+
             '  (case ACTION:' +nl+
             '    nothing -> print files to clear counts' +nl+
             '    print -> print files to clean counts and names' +nl+
             '    clean -> print files to clean counts and REMOVE them' +nl+
             '  )' +nl+
             nl+
             'Also following options are allowed (as many times as you want):' +nl+
             '  -f / --files FILE-NAME-MASK' +nl+
             '                    Clean also files (not dirs) matching FILE-NAME-MASK' +nl+
             '  -d / --dirs DIR-NAME-MASK' +nl+
             '                    Clean also dirs matching DIR-NAME-MASK' +nl+
             '  -n / --no-recursive' +nl+
             '                    By default, dirs are cleaned recursively.'+nl+
             '                    Use this to clear dirs non-recursively.' +nl+
             nl+
             'Default files to clean:' +nl);

           for i := 0 to DefaultFilesToClean.Count-1 do
             Write('  '+DefaultFilesToClean[i]);

           Write(nl+
             'Default dirs to clean:' +nl);

           for i := 0 to DefaultDirsToClean.Count-1 do
             Write('  '+DefaultDirsToClean[i]);

           Writeln(nl+
             nl+
             SCastleEngineProgramHelpSuffix('dircleaner', '1.0.0', true));
           Halt;
         end;
      1: FilesToClean.Add(Argument);
      2: DirsToClean.Add(Argument);
      3: CleanDirsRecursively := false;
      else raise EInternalError.Create('OptionProc');
    end;
  end;

{ main ------------------------------------------------------------ }

var
  i: Integer;
begin
  try
    FilesToClean := TCastleStringList.Create;
    DefaultFilesToClean := TCastleStringList.Create;
    DirsToClean := TCastleStringList.Create;
    DefaultDirsToClean := TCastleStringList.Create;

    DefaultFilesToCleanInit;
    DefaultDirsToCleanInit;

    { parse params }
    Parameters.Parse(Options, @OptionProc, nil);

    if Parameters.High = 2 then
    begin
      Action := ActionParStrToAction(Parameters[2]);
      Parameters.Delete(2);
    end;
    if Parameters.High = 1 then
      StartPath := InclPathDelim(Parameters[1]) else
    if Parameters.High <> 0 then
      raise EinvalidParams.Create('Excessive parameter "' + Parameters[1] + '"');

    { do the job }
    FilesToClean.AddStrings(DefaultFilesToClean);
    DirsToClean.AddStrings(DefaultDirsToClean);

    for i := 0 to FilesToClean.Count-1 do CleanFiles(FilesToClean[i]);
    for i := 0 to DirsToClean.Count-1 do CleanDirs(DirsToClean[i]);
  finally
    FilesToClean.Free;
    DefaultFilesToClean.Free;
    DirsToClean.Free;
    DefaultDirsToClean.Free;
  end;
end.
