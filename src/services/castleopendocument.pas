{
  Copyright 2012-2023 Michalis Kamburelis and Lazarus developers.

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

  On Android and iOS, it uses the OS functions to open the URL,
  supporting all URL types that are handled by the installed applications.
  For example, it will support the market:// URLs on Android.

  To use this on Android, declare your Android project type as "integrated",
  see https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
function OpenURL(AURL: String): Boolean;

{ Open a local file or directory.
  @deprecated You should instead use OpenURL,
  that automatically detects local filenames and URLs leading to local filenames. }
function OpenDocument(APath: String): Boolean;

{ Share a text/link through user-choosen application.

  This works only on Android and iOS right now.
  For Android, you need to declare the project type as "integrated":
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine .

  @param(Title The short title of the share.)
  @param(Subject Used as an email subject, and any other app on Android
    that interprets EXTRA_SUBJECT parameter.)
  @param(Content Multi-line share text content, possibly with URL inside.)
}
procedure ShareText(const Title, Subject, Content: string);

{ Show the application in the application store (Google Play on Android,
  AppStore on iOS). Ignored on other platforms now.

  @unorderedList(
    @itemSpacing Compact
    @item(On Android, ApplicationId should be the qualitied name of the application
      (same thing you use as qualified_name in CastleEngineManifest.xml).

      To include the necessary integration code in your Android project,
      you must declare your Android project type as "integrated".
      See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine .
    )
    @item(On iOS, ApplicationId has to be the "Apple ID" number of your application
      (you can see it e.g. in https://itunesconnect.apple.com/ page of your application).
    )
  ) }
procedure OpenApplicationStore(const ApplicationId: string);

{ Vibrate the device.

  Available on Android, iOS and Nintendo Switch now. Ignored on other platforms.

  To include the necessary integration code in your Android project,
  declare your Android project type as "integrated" with
  the "vibrate" service inside CastleEngineManifest.xml.
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
procedure Vibrate(const Miliseconds: Cardinal);

{ Simple on-screen notification using Android "toast" call.

  This is available only on Android right now, ignored elsewhere.
  To include the necessary integration code in your Android project,
  you must declare your Android project type as "integrated".
  See https://castle-engine.io/android-Project-Services-Integrated-with-Castle-Game-Engine . }
procedure OnScreenNotification(const Message: string);
  deprecated 'This is Android-specific and probably will not be ever supported on other platforms. Better use CGE UI to make cros-platform UI notifications, like TCastleNotifications or just TCastleLabel with animated color/background.';

implementation

uses
  {$if not(defined(ANDROID) or defined(CASTLE_IOS))}
    {$ifdef UNIX} BaseUnix, {$endif}
    {$ifdef MSWINDOWS} Windows, {$endif}
    {$ifdef DARWIN} MacOSAll, {$endif}
  {$endif}
  SysUtils, Classes, {$ifdef FPC} Process, {$else} ShellApi, {$endif}
  CastleUriUtils, CastleUtils, CastleFilesUtils, CastleLog, CastleMessaging;

{ Has URL any anchor at the end, like "index.html#chapter1".
  For such URLs, converting them to local filename may be possible
  but is lossy: anchor would be lost.

  Testcase: API docs to
  file:///..../doc/reference/CastleSoundEngine.TCastleSound.html#DefaultReferenceDistance
  would effectively open
  /..../doc/reference/CastleSoundEngine.TCastleSound.html
  (anchor lost). }
function UrlHasAnchor(const Url: String): Boolean;
var
  U, Anchor: String;
begin
  U := Url;
  URIExtractAnchor(U, Anchor, true);
  Result := Anchor <> '';
end;

{ Portions of OpenURL below copied and adapted from Lazarus LCL unit LCLIntf.
  The core of our engine cannot depend on LCL. Fortunately, Lazarus
  has the same license as our engine, so copying code is fine.

  We did a lot of changes to the code here:
  - Removed references to UTF8 classes and functions.
    CGE uses FPC codepage-aware strings.
  - Use TProcess.Executable, TProcess.Parameters instead of
    deprecated TProcess.CommandLine. This avoids the need for many paranoid quoting
    previously present here.
  - Some bits adjusted to use our CastleUtils, CastleFilesUtils functions.
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
  - ... and more.
}

{ lcl/lclstrconsts.pas ------------------------------------------------------- }

resourcestring
  lisProgramFileNotFound = 'program file not found %s';
  lisCanNotExecute = 'can not execute %s';

{$ifdef MSWINDOWS}

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
    {$ifdef FPC}
    ws := UTF8Decode(AURL);
    {$else}
    ws := AURL;
    {$endif}
    Result := ShellExecuteW(0, 'open', PWideChar(ws), nil, nil, SW_SHOWNORMAL) > 32;
  end
  else
  begin
    {$ifdef FPC}
    ans := Utf8ToAnsi(AURL); // utf8 must be converted to Windows Ansi-codepage
    {$else}
    ans := AnsiString(AURL);
    {$endif}
    Result := ShellExecuteA(0, 'open', PAnsiChar(ans), nil, nil, SW_SHOWNORMAL) > 32;
  end;
  {$ENDIF}
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := OpenURL(APath);
end;

{$endif MSWINDOWS}

{$ifdef UNIX}

{$if defined(ANDROID) or defined(CASTLE_IOS)}
function OpenURL(AURL: String): Boolean;
begin
  Messaging.Send(['view-url', AURL]);
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
    BrowserProcess.Executable := ProgramFilename;
    BrowserProcess.Parameters.Add(Parameter);

    WritelnLog('Executing', 'Executable: "' + ProgramFilename +
      '", Parameter: "' + Parameter + '"');

    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

  {$if defined(darwin) and not defined(CASTLE_IOS)}

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
  if not UrlHasAnchor(AURL) then
  begin
    FileName := URIToFilenameSafe(AURL);
    if FileName <> '' then
      Exit(OpenDocument(FileName));
  end;

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
  if not UrlHasAnchor(AURL) then
  begin
    FileName := URIToFilenameSafe(AURL);
    if FileName <> '' then
      Exit(OpenDocument(FileName));
  end;

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
  if not (FileExists(APath) or DirectoryExists(APath)) then
    Exit(false);

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
  {$endif}  // not Android or iOS
{$endif} // UNIX

procedure ShareText(const Title, Subject, Content: string);
begin
  Messaging.Send(['share-text', Title, Subject, Content]);
end;

procedure OpenApplicationStore(const ApplicationId: string);
begin
  {$ifdef ANDROID} OpenURL('market://details?id=' + ApplicationId); {$endif}

  {$ifdef CASTLE_IOS} Messaging.Send(['open-application-store', ApplicationId]); {$endif}
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
