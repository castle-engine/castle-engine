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

{ OpenDocument and OpenUrl implementations for macOS. }

// Relies on RunCmdFromPath
{$I castleopendocument_process.inc}

{$ifdef read_unit_compiler_directives}
// Needed to compile OpenUrl, toc pass NSString around
{$modeswitch objectivec1}
{$endif read_unit_compiler_directives}

{$ifdef read_uses}
MacOSAll, CocoaAll,
{$endif read_uses}

{$ifdef read_implementation}

{ Based on Lazarus LCL:
  lcl/include/sysenvapis_mac.inc }

// Open a given URL with the default browser
function OpenUrl(AUrl: String): Boolean;
var
  url: NSURL;
  ws: NSWorkspace;
  FileName: string;
begin
  if AUrl = '' then
    Exit(False);

  { If this is a local filename, open it using OpenDocument. }
  if not UrlHasAnchor(AUrl) then
  begin
    FileName := UriToFilenameSafe(AUrl);
    if FileName <> '' then
      Exit(OpenDocument(FileName));
  end;

  { Implementation follows LCL, see lcl/include/sysenvapis_mac.inc .

    In the past it was using LSOpenCFURLRef, no longer, see:
    https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/26890
    https://gitlab.com/freepascal.org/lazarus/lazarus/-/commit/bde2979aa0fa407e116925dbbadfa53536ab4613
  }

  url := NSURL.URLWithString(NSString.stringWithUTF8String(@AUrl[1]));
  // scheme is checking for "protocol" specifier.
  // if no protocol specifier exist - do not consider it as URL and fail
  if not Assigned(url) or (url.scheme.length = 0) then
    Exit;

  ws := NSWorkspace.sharedWorkspace;
  Result := ws.openURL(url);
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := True;
  RunCmdFromPath('open',APath);
end;

{$endif read_implementation}