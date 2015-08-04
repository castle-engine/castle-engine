{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on FPC packages/fpmkunit/src/fpmkunit.pp unit,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Windows registry stuff (for installing plugin). }
unit ToolWindowsRegistry;

interface

uses CastleUtils, CastleStringUtils;

procedure InstallWindowsPluginRegistry(const ProjectName, ProjectCaption: string);

implementation

uses SysUtils, Registry;

procedure InstallWindowsPluginRegistry(const ProjectName, ProjectCaption: string);
var
  Registry: TRegistry;

  procedure OpenCreateKey(const KeyPath: string);
  begin
    if not Registry.OpenKey(KeyPath, true) then
      raise Exception.Create('Cannot open or create Windows registry key: ' + KeyPath);
  end;

begin
  Registry := TRegistry.Create;
  try
    // TODO: do we need to bother with 32/64 registry versions?
    // see http://wiki.freepascal.org/fcl-registry
    // https://msdn.microsoft.com/en-us/library/windows/desktop/aa384129%28v=vs.85%29.aspx
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    OpenCreateKey('\SOFTWARE\MozillaPlugins\@' + QualifiedName '/' + ProjectName + ',version=' + ProjectVersion);
    Registry.WritelnString('Description', 'Plugin created with Castle Game Engine to run ' + ProjectName);
    Registry.WritelnString('Path', ProjectPath);
    Registry.WritelnString('ProductName', ProjectName);
    Registry.WritelnString('Vendor', Author);
    Registry.WritelnString('Version', Version);
    Registry.WritelnString('GeckoVersion', '1.9');

    OpenCreateKey('\SOFTWARE\MozillaPlugins\@' + QualifiedName '/' + ProjectName + ',version=' + ProjectVersion + '\MimeTypes');

    OpenCreateKey('\SOFTWARE\MozillaPlugins\@' + QualifiedName '/' + ProjectName + ',version=' + ProjectVersion + '\MimeTypes\x-' + ProjectName);
    Registry.WritelnString('Description', 'Game Data for ' + ProjectName + ' (Castle Game Engine)');

    //  Registry.RootKey := HKEY_CURRENT_USER;
    // TODO: is this needed? Do the same, underneath
    // HKEY_CURRENT_USER\Software\MozillaPlugins\...
  finally
    Registry.Free;
  end;
end;
end.
