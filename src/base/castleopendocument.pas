{
  Copyright 2012 Michalis Kamburelis, and Lazarus developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on Lazarus LCL code, which has
  exactly the same license as our "Castle Game Engine":
  LGPL with static linking exception, see COPYING.txt for details.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Opening files and URLs. }
unit CastleOpenDocument;

interface

resourcestring
  SCannotOpenURL = 'WWW browser not found on your system.';

function OpenURL(AURL: String): Boolean;
function OpenDocument(APath: String): Boolean;

implementation

{ Largely copied from Lazarus LCL unit LCLIntf. The core of our engine
  cannot depend on LCL. Fortunately, Lazarus
  has the same license as our engine, so copying code is fine.

  I merely removed references to UTF8 classes and functions.
  Not that I don't like them, but they just have no place in a game engine.
  With time, hopefully FPC will have nice solution for UTF8.
  Also, with time, hopefully these functions may be moved to FPC FCL too.

  Also, some bits may use our CastleUtils, CastleStringUtils functions.

  So:
  - FilenameIsAbsolute => our IsPathAbsolute
  - FileExistsUTF8 => FileExists
  - CleanAndExpandFilename => ExpandFilename
  - AppendPathDelim => InclPathDelim
  - TrimFilename => no need to
  - GetExeExt => ExeExtension
  - GetEnvironmentVariableUTF8 => GetEnvironmentVariable
  - FindFilenameOfCmd => PathFileSearch
  - FindDefaultBrowser => simplified and folded inside OpenURL for Unix,
    LCL implementation was cross-platform but was used only on
    Unix (except Darwin) (for these OpenXxx routines).
  - SearchFileInPath => PathFileSearch (it's only used by Unix (except Darwin))
 }

uses
  {$ifdef UNIX} BaseUnix, {$endif}
  SysUtils, Classes, Process, CastleUtils, CastleFilesUtils, CastleLog;

{ lcl/lclstrconsts.pas ------------------------------------------------------- }

resourcestring
  lisProgramFileNotFound = 'program file not found %s';
  lisCanNotExecute = 'can not execute %s';

{ lcl/include/unixfileutil.inc ----------------------------------------------- }

{$ifdef UNIX}
function FileIsExecutable(const AFilename: string): boolean;
var
  Info : Stat;
begin
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info)<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
end;
{$endif}

{ lcl/utf8process.pp --------------------------------------------------------- }

// Runs a short command which should point to an executable in
// the environment PATH
// For example: ProgramFilename=ls CmdLineParameters=-l /home
// Will locate and execute the file /bin/ls
// If the command isn't found, an exception will be raised
procedure RunCmdFromPath(ProgramFilename, CmdLineParameters: string);
var
  OldProgramFilename: String;
  BrowserProcess: TProcess;
begin
  OldProgramFilename:=ProgramFilename;
  ProgramFilename:=PathFileSearch(ProgramFilename);

  if ProgramFilename='' then
    raise EFOpenError.Create(Format(lisProgramFileNotFound, [OldProgramFilename]
      ));
  if not FileIsExecutable(ProgramFilename) then
    raise EFOpenError.Create(Format(lisCanNotExecute, [ProgramFilename]));

  // run
  BrowserProcess := TProcess.Create(nil);
  try
    // Encloses the executable with "" if it's name has spaces
    if Pos(' ',ProgramFilename)>0 then
      ProgramFilename:='"'+ProgramFilename+'"';

    BrowserProcess.CommandLine := ProgramFilename;
    if CmdLineParameters<>'' then
      BrowserProcess.CommandLine := BrowserProcess.CommandLine + ' ' + CmdLineParameters;

    if Log then
      WritelnLog('Executing', BrowserProcess.CommandLine);

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

{$ifdef Windows}

{ lcl/include/sysenvapis_win.inc --------------------------------------------- }

function OpenURL(AURL: String): Boolean;
var
{$IFDEF WinCE}
  Info: SHELLEXECUTEINFO;
{$ELSE}
  ws: WideString;
  ans: AnsiString;
{$ENDIF}
begin
  Result := False;
  if AURL = '' then Exit;

  {$IFDEF WinCE}
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(Info);
  Info.fMask := SEE_MASK_FLAG_NO_UI;
  Info.lpVerb := 'open';
  Info.lpFile := PWideChar(UTF8Decode(AURL));
  Result := ShellExecuteEx(@Info);
  {$ELSE}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    ws := UTF8Decode(AURL);
    Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
  end
  else
  begin
    ans := Utf8ToAnsi(AURL); // utf8 must be converted to Windows Ansi-codepage
    Result := ShellExecute(0, 'open', PAnsiChar(ans), nil, nil, SW_SHOWNORMAL) > 32;
  end;
  {$ENDIF}
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(APath);
end;

{$endif}
{$ifdef UNIX}
  {$ifdef darwin}

{ lcl/include/sysenvapis_mac.inc --------------------------------------------- }

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  cf: CFStringRef;
  url: CFURLRef;
begin
  if AURL = '' then
    Exit(False);
  cf := CFStringCreateWithCString(kCFAllocatorDefault, @AURL[1], kCFStringEncodingUTF8);
  if not Assigned(cf) then
    Exit(False);
  url := CFURLCreateWithString(nil, cf, nil);
  Result := LSOpenCFURLRef(url, nil) = 0;
  CFRelease(url);
  CFRelease(cf);
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := True;
  RunCmdFromPath('open',APath);
end;

  {$else}

{ lcl/include/sysenvapis.inc ------------------------------------------------- }

function FindDefaultBrowser(out ABrowser: String): Boolean;

  function Find(const ShortFilename: String; out ABrowser: String): Boolean; inline;
  begin
    ABrowser := PathFileSearch(ShortFilename + ExeExtension);
    Result := ABrowser <> '';
  end;

begin
  // search in path. Prefer open source ;)
  if Find('xdg-open', ABrowser)  // Portland OSDL/FreeDesktop standard on Linux
  or Find('sensible-browser', ABrowser)  // Kambi: Debian-based systems
  or Find('htmlview', ABrowser)  // some redhat systems
  or Find('firefox', ABrowser)
  or Find('mozilla', ABrowser)
  or Find('galeon', ABrowser)
  or Find('konqueror', ABrowser)
  or Find('safari', ABrowser)
  or Find('netscape', ABrowser)
  or Find('opera', ABrowser) then ;
  Result := ABrowser <> '';
end;

{ lcl/include/sysenvapis_unix.inc -------------------------------------------- }

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  ABrowser: String;
begin
  Result := FindDefaultBrowser(ABrowser) and FileExists(ABrowser) and FileIsExecutable(ABrowser);
  if not Result then
    Exit;
  RunCmdFromPath(ABrowser, AnsiQuotedStr(AURL, '"'));
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
var
  lApp: string;
begin
  Result := True;
  if not FileExists(APath) then exit(false);

  lApp:=PathFileSearch('xdg-open'); // Portland OSDL/FreeDesktop standard on Linux
  if lApp='' then
    lApp:=PathFileSearch('kfmclient'); // KDE command
  if lApp='' then
    lApp:=PathFileSearch('gnome-open'); // GNOME command
  if lApp='' then
    Exit(False);

  if (APath<>'') and (APath[1]<>'"') then
    APath:=QuotedStr(APath);
  RunCmdFromPath(lApp,APath);
end;

  {$endif}
{$endif}

end.
