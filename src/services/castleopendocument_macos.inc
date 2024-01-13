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

{$ifdef read_uses}
MacOSAll,
{$endif}

{$ifdef read_implementation}

{ Based on Lazarus LCL:
  lcl/include/sysenvapis_mac.inc }

// Open a given URL with the default browser
function OpenUrl(AUrl: String): Boolean;
var
  cf: CFStringRef;
  url: CFUrlRef;
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

  cf := CFStringCreateWithCString(kCFAllocatorDefault, @AUrl[1], kCFStringEncodingUTF8);
  if not Assigned(cf) then
    Exit(False);
  url := CFUrlCreateWithString(nil, cf, nil);
  Result := LSOpenCFUrlRef(url, nil) = 0;

  CFRelease(url);
  CFRelease(cf);
end;

// Open a document with the default application associated with it in the system
function OpenDocument(APath: String): Boolean;
begin
  Result := True;
  RunCmdFromPath('open',APath);
end;

{$endif read_implementation}