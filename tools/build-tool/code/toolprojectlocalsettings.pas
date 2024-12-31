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
{ Local (user/machine-specific) project settings (TCastleProjectLocalSettings). }
unit ToolProjectLocalSettings;

interface

uses CastleClassUtils,
  ToolArchitectures;

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

implementation

uses CastleComponentSerialize;

initialization
  RegisterSerializableComponent(TCastleProjectLocalSettings, 'Local Project Settings');
end.
