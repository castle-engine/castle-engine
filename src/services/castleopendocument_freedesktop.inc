{%MainUnit castleopendocument.pas}
{
  Copyright 2012-2024 Michalis Kamburelis and Lazarus developers.

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

{ OpenDocument and OpenUrl implementations for desktops following
  XDG https://www.freedesktop.org/wiki/ , in practice:
  desktop Unix like Linux, FreeBSD. }

// Relies on RunCmdFromPath
{$I castleopendocument_process.inc}

{$ifdef read_implementation}

{ Based on Lazarus LCL:
  lcl/include/sysenvapis.inc
  lcl/include/sysenvapis_unix.inc }

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

// Open a given Url with the default browser
function OpenUrl(AUrl: String): Boolean;
var
  ABrowser, FileName: String;
begin
  { If this is a local filename, open it using OpenDocument. }
  if not UrlHasAnchor(AUrl) then
  begin
    FileName := UriToFilenameSafe(AUrl);
    if FileName <> '' then
      Exit(OpenDocument(FileName));
  end;

  Result := FindDefaultBrowser(ABrowser) and FileExists(ABrowser) and FileIsExecutable(ABrowser);
  if not Result then
    Exit;
  RunCmdFromPath(ABrowser, AUrl);
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

{$endif read_implementation}
