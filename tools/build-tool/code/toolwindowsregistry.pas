{
  Copyright 2014-2017 Michalis Kamburelis.

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

procedure InstallWindowsPluginRegistry(const ProjectName, QualifiedName, ProjectPath,
  PluginCompiledFile, Version, Author: string);

implementation

uses SysUtils, Registry;

procedure InstallWindowsPluginRegistry(const ProjectName, QualifiedName, ProjectPath,
  PluginCompiledFile, Version, Author: string);
var
  Registry: TRegistry;

  procedure OpenCreateKey(const KeyPath: string);
  begin
    if not Registry.OpenKey(KeyPath, true) then
      raise Exception.Create('Cannot open or create Windows registry key: ' + KeyPath);
  end;

var
  BaseKeyPath: string;
begin
  Registry := TRegistry.Create;
  try
    // TODO: check installing win64 plugin

    // TODO: do we need to bother with 32/64 registry versions?
    // see http://wiki.freepascal.org/fcl-registry
    // https://msdn.microsoft.com/en-us/library/windows/desktop/aa384129%28v=vs.85%29.aspx
    Registry.RootKey := HKEY_CURRENT_USER;
    { For system-wide install we would use HKEY_LOCAL_MACHINE.
      Also, 'SOFTWARE' is written all in caps, although this probably doesn't matter. }

    BaseKeyPath := '\Software\MozillaPlugins\@' + QualifiedName + '/' + ProjectName + ',version=' + Version;

    OpenCreateKey(BaseKeyPath);
    Registry.WriteString('Description', 'Plugin created with Castle Game Engine to run ' + ProjectName);
    Registry.WriteString('Path', InclPathDelim(ProjectPath) + PluginCompiledFile);
    Registry.WriteString('ProductName', ProjectName);
    Registry.WriteString('Vendor', Author);
    Registry.WriteString('Version', Version);
    Registry.WriteString('GeckoVersion', '1.9');

    OpenCreateKey(BaseKeyPath + '\MimeTypes');

    OpenCreateKey(BaseKeyPath + '\MimeTypes\x-' + ProjectName);
    Registry.WriteString('Description', 'Game Data for ' + ProjectName + ' (Castle Game Engine)');

    Writeln('Installed Windows registry entries under HKEY_CURRENT_USER' + BaseKeyPath);
    Writeln('  The plugin should be visible now in Firefox and other browsers that support NPAPI plugins.');
  finally FreeAndNil(Registry) end;
end;

end.
