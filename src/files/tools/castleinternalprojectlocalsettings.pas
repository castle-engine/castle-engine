{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Local (user/machine-specific) project settings (TCastleProjectLocalSettings).

  @italic(Do not use this unit in your own applications.)
  This unit is only directly used by CGE tools (build tool, editor).
  However, do consult API documentation of this unit if you want to
  write @code(CastleProjectLocalSettings.json) file by hand. }
unit CastleInternalProjectLocalSettings;

interface

uses Classes,
  CastleClassUtils, CastleInternalArchitectures;

type
  { Local (user/machine-specific) project settings. }
  TCastleProjectLocalSettings = class(TCastleComponent)
  strict private
    FUsePlatformDefaults: Boolean;
    FDefaultTarget: TTarget;
    FDefaultOS: TOS;
    FDefaultCPU: TCPU;
  published
    { Do you have a preferred default platform to use (compile, run and so on)
      by the build tool.
      If this is @true, then @link(DefaultTarget), @link(DefaultOS),
      @link(DefaultCPU) are meaningful and they determine the default build tool
      platform (as if you used --target, --os, --cpu command-line options,
      see https://castle-engine.io/build_tool ). }
    property UsePlatformDefaults: Boolean
      read FUsePlatformDefaults write FUsePlatformDefaults default false;

    { Default target, used only if @link(UsePlatformDefaults) is @true. }
    property DefaultTarget: TTarget
      read FDefaultTarget write FDefaultTarget default targetCustom;

    { Default OS, used only if @link(UsePlatformDefaults) is @true
      and @link(DefaultTarget) is targetCustom. }
    property DefaultOS: TOS
      read FDefaultOS write FDefaultOS default osNone;

    { Default CPU, used only if @link(UsePlatformDefaults) is @true
      and @link(DefaultTarget) is targetCustom. }
    property DefaultCPU: TCPU
      read FDefaultCPU write FDefaultCPU default cpuNone;
  end;

{ Optionally adjust the given platform specification
  if project has a file called
  @code(CastleProjectLocalSettings.json)
  defining @link(TCastleProjectLocalSettings).

  Project is in path given as ProjectPath (must be absolute path ending
  with path delimiter). }
procedure ProjectOverridePlatform(const ProjectPath: String;
  var Target: TTarget; var OS: TOS; var CPU: TCPU);

{ Read (or create new) local settings for the project in ProjectPath.
  ProjectPath must be an absolute path ending with path delimiter.

  @param(CreateNew Determines what to do when the local settings file
    (CastleProjectLocalSettings.json) does not exist. If @true,
    we create new instance of TCastleProjectLocalSettings with default values,
    so in effect we never return @nil.
    If @false, we return @nil when the file does not exist.)

  @param(AOwner Owner of the created TCastleProjectLocalSettings instance.
    Also owner of any other loaded components, which may happen if you use
    TCastleComponent.NonVisualComponents to store more components
    attached to the TCastleProjectLocalSettings instance.
    Or when TCastleProjectLocalSettings will have subcomponents.) }
function ProjectGetLocalSettings(const ProjectPath: String;
  const CreateNew: Boolean; const AOwner: TComponent): TCastleProjectLocalSettings;

{ Write local settings for the project in ProjectPath.
  ProjectPath must be an absolute path ending with path delimiter. }
procedure ProjectSetLocalSettings(const ProjectPath: String;
  const LocalSettings: TCastleProjectLocalSettings);

implementation

uses SysUtils,
  CastleComponentSerialize, CastleUriUtils;

const
  SettingsFileBaseName = 'CastleProjectLocalSettings.json';

procedure ProjectOverridePlatform(const ProjectPath: String;
  var Target: TTarget; var OS: TOS; var CPU: TCPU);
var
  LocalSettings: TCastleProjectLocalSettings;
  LocalSettingsOwner: TComponent;
begin
  LocalSettingsOwner := TComponent.Create(nil);
  try
    LocalSettings := ProjectGetLocalSettings(ProjectPath, false, LocalSettingsOwner);
    if (LocalSettings <> nil) and
       LocalSettings.UsePlatformDefaults then
    begin
      Target := LocalSettings.DefaultTarget;
      if LocalSettings.DefaultOS <> osNone then
        OS := LocalSettings.DefaultOS;
      if LocalSettings.DefaultCPU <> cpuNone then
        CPU := LocalSettings.DefaultCPU;
    end;
  finally FreeAndNil(LocalSettingsOwner) end;
end;

function ProjectGetLocalSettings(const ProjectPath: String;
  const CreateNew: Boolean; const AOwner: TComponent): TCastleProjectLocalSettings;
var
  LocalSettingsUri: String;
begin
  LocalSettingsUri := FilenameToUriSafe(ProjectPath + SettingsFileBaseName);
  if UriFileExists(LocalSettingsUri) then
    Result := ComponentLoad(LocalSettingsUri, AOwner) as TCastleProjectLocalSettings
  else
  if CreateNew then
    Result := TCastleProjectLocalSettings.Create(AOwner)
  else
    Result := nil;
end;

procedure ProjectSetLocalSettings(const ProjectPath: String;
  const LocalSettings: TCastleProjectLocalSettings);
var
  LocalSettingsUri: String;
begin
  LocalSettingsUri := FilenameToUriSafe(ProjectPath + SettingsFileBaseName);
  ComponentSave(LocalSettings, LocalSettingsUri);
end;

initialization
  RegisterSerializableComponent(TCastleProjectLocalSettings, 'Local Project Settings');
end.
