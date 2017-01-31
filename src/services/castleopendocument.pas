{
  Copyright 2012-2017 Michalis Kamburelis and Lazarus developers.

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

{$include castleconf.inc}

interface

resourcestring
  SCannotOpenURL = 'Browser not found on your system.';

{ Open URL with the suitable application.

  This detects and handles also local files (as filenames, or URLs with "file:"
  protocol).

  On Android, it can use the Android intent system to open the URL,
  supporting all URL types that are handled by some installed app.
  For example, it will support the market:// URLs.
  To use this, declare your Android project type as "integrated",
  see https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine . }
function OpenURL(AURL: String): Boolean;

{ Open a local file or directory.
  @deprecated You should instead use OpenURL,
  that automatically detects local filenames and URLs leading to local filenames. }
function OpenDocument(APath: String): Boolean;

{ Share a text/link through user-choosen application.

  This is available only on Android right now, ignored elsewhere.
  To include the necessary integration code in your Android project,
  you must declare your Android project type as "integrated".
  See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .

  @param(Title The short title of the share.)
  @param(Subject Used as an email subject, and any other app on Android
    that interprets EXTRA_SUBJECT parameter.)
  @param(Content Multi-line share text content, possibly with URL inside.)
}
procedure ShareText(const Title, Subject, Content: string);

{ Vibrate the device.

  This is available only on Android right now, ignored elsewhere.
  To include the necessary integration code in your Android project,
  declare your Android project type as "integrated" with
  the "vibrate" component inside CastleEngineManifest.xml.
  See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine . }
procedure Vibrate(const Miliseconds: Cardinal);

{ Simple on-screen notification using Android "toast" call.

  This is available only on Android right now, ignored elsewhere.
  To include the necessary integration code in your Android project,
  you must declare your Android project type as "integrated".
  See https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine . }
procedure OnScreenNotification(const Message: string);

implementation

{ Copied and adapted from Lazarus LCL unit LCLIntf. The core of our engine
  cannot depend on LCL. Fortunately, Lazarus
  has the same license as our engine, so copying code is fine.

  We did some changes to the code here:
  - Removed references to UTF8 classes and functions.
    Not that I don't like them, but they just have no place in a game engine.
    With time, hopefully FPC will have nice solution for UTF8.
    Also, with time, hopefully these functions may be moved to FPC FCL too.
  - Using TProcess.Executable, TProcess.Parameters instead of
    TProcess.CommandLine. This avoids the need for many paranoid quoting
    previously present here.
  - Some bits may use our CastleUtils, CastleFilesUtils functions.
  - On Android, we use CastleMessaging to integrate with Android activities
    through Java APIs.

  So:
  - FilenameIsAbsolute => IsPathAbsolute
  - FileExistsUTF8 => FileExists
  - CleanAndExpandFilename => ExpandFilename
  - AppendPathDelim => InclPathDelim
  - TrimFilename => no need to
  - GetExeExt => ExeExtension
  - GetEnvironmentVariableUTF8 => GetEnvironmentVariable
  - FindFilenameOfCmd => PathFileSearch or FindExe
  - FindDefaultBrowser => simplified and folded inside OpenURL for Unix,
    LCL implementation was cross-platform but was used only on
    Unix (except Darwin) (for these OpenXxx routines).
  - SearchFileInPath => PathFileSearch (it's only used by Unix) or FindExe
}

uses
  {$ifdef UNIX} BaseUnix, {$endif}
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef DARWIN} MacOSAll, {$endif} CastleURIUtils,
  SysUtils, Classes, Process,
  CastleUtils, CastleFilesUtils, CastleLog, CastleMessaging;

{ lcl/lclstrconsts.pas ------------------------------------------------------- }

resourcestring
  lisProgramFileNotFound = 'program file not found %s';
  lisCanNotExecute = 'can not execute %s';

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

{$ifdef ANDROID}
function OpenURL(AURL: String): Boolean;
begin
  Messaging.Send(['intent-view-uri', AURL]);
  Result := true;
end;

function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(FilenameToURISafe(APath));
end;
{$else}

{ lcl/include/unixfileutil.inc ----------------------------------------------- }

function FileIsExecutable(const AFilename: string): boolean;
var
  Info : Stat;
begin
  // first check AFilename is not a directory and then check if executable
  Result:= (FpStat(AFilename,info)<>-1) and FPS_ISREG(info.st_mode) and
           (BaseUnix.FpAccess(AFilename,BaseUnix.X_OK)=0);
end;

{ lcl/utf8process.pp --------------------------------------------------------- }

{ Runs a short command which should point to an executable in
  the environment PATH.

  Kambi: simplified this to use TProcess.Executable, TProcess.Parameters
  instead of TProcess.CommandLine (Lazarus TProcessUTF8 didn't have these
  improvements). This removes the need for paranoid quoting of strings
  everywhere.
  This always takes exactly 1 parameter now --- which is actually Ok
  for usage in this unit. }
procedure RunCmdFromPath(ProgramFilename, Parameter: string);
var
  OldProgramFilename: String;
  BrowserProcess: TProcess;
begin
  OldProgramFilename:=ProgramFilename;
  ProgramFilename:=PathFileSearch(ProgramFilename);

  if ProgramFilename='' then
    raise EFOpenError.Create(Format(lisProgramFileNotFound, [OldProgramFilename]));
  if not FileIsExecutable(ProgramFilename) then
    raise EFOpenError.Create(Format(lisCanNotExecute, [ProgramFilename]));

  // run
  BrowserProcess := TProcess.Create(nil);
  try
    { Only FPC >= 2.6.0 has TProcess.Parameters, TProcess.Executable }
    {$define USE_PROCESS_PARAMETERS}
    {$ifdef VER2_2} {$undef USE_PROCESS_PARAMETERS} {$endif}
    {$ifdef VER2_4} {$undef USE_PROCESS_PARAMETERS} {$endif}

    {$ifdef USE_PROCESS_PARAMETERS}
    BrowserProcess.Executable := ProgramFilename;
    BrowserProcess.Parameters.Add(Parameter);
    {$else}
    BrowserProcess.CommandLine := AnsiQuotedStr(ProgramFilename, '"') +
                            ' ' + AnsiQuotedStr(Parameter, '"');
    {$endif}

    if Log then
      WritelnLog('Executing', 'Executable: "' + ProgramFilename +
        '", Parameter: "' + Parameter + '"');

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

  {$if defined(darwin) and not defined(iOS) and not defined(CPUX86_64)}

{ lcl/include/sysenvapis_mac.inc --------------------------------------------- }

// Open a given URL with the default browser
function OpenURL(AURL: String): Boolean;
var
  cf: CFStringRef;
  url: CFURLRef;
  FileName: string;
begin
  if AURL = '' then
    Exit(False);

  { If this is a local filename, open it using OpenDocument. }
  FileName := URIToFilenameSafe(AURL);
  if FileName <> '' then
    Exit(OpenDocument(FileName));

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

  function Find(const ShortFilename: String; out ABrowser: String): Boolean; {$ifdef SUPPORTS_INLINE} inline; {$endif}
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
  ABrowser, FileName: String;
begin
  { If this is a local filename, open it using OpenDocument. }
  FileName := URIToFilenameSafe(AURL);
  if FileName <> '' then
    Exit(OpenDocument(FileName));

  Result := FindDefaultBrowser(ABrowser) and FileExists(ABrowser) and FileIsExecutable(ABrowser);
  if not Result then
    Exit;
  RunCmdFromPath(ABrowser, AURL);
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

  RunCmdFromPath(lApp,APath);
end;

    {$endif}
  {$endif}  // not Android
{$endif} // UNIX

procedure ShareText(const Title, Subject, Content: string);
begin
  Messaging.Send(['intent-send-text', Title, Subject, Content]);
end;

procedure Vibrate(const Miliseconds: Cardinal);
begin
  Messaging.Send(['vibrate', IntToStr(Miliseconds)]);
end;

procedure OnScreenNotification(const Message: string);
begin
  Messaging.Send(['on-screen-notification', Message]);
end;


end.
