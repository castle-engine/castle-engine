{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ Main program file on standalone. }
program rift;

{$apptype GUI}

uses SysUtils, CastleParameters, CastleUtils, CastleWindow,
  CastleClassUtils, CastleStringUtils, CastleProgress, CastleWindowProgress,
  CastleGLUtils, CastleLog, CastleGameNotifications, CastleUIControls,
  CastleConfig, CastleSoundEngine, CastleVectors,
  RiftWindow, RiftVideoOptions, RiftIntro, RiftMainMenu,
  RiftSound, RiftCreatures;

{ parsing parameters --------------------------------------------------------- }

var
  WasParam_NoScreenChange: boolean = false;

const
  Version = '0.1.0';
  Options: array [0..6] of TOption =
  ( (Short:'h'; Long: 'help'; Argument: oaNone),
    (Short:'v'; Long: 'version'; Argument: oaNone),
    (Short:'n'; Long: 'no-screen-change'; Argument: oaNone),
    (Short: #0; Long: 'debug-menu-designer'; Argument: oaNone),
    (Short: #0; Long: 'debug-menu-fps'; Argument: oaNone),
    (Short: #0; Long: 'debug-log'; Argument: oaNone),
    (Short: #0; Long: 'debug-no-creatures'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           '"The Rift" version ' + Version + '.' +nl+
           'WWW: http://castle-engine.sourceforge.net/' +nl+
           '     (no rift-specific webpage yet)' +nl+
           nl+
           'Options:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           SoundEngine.ParseParametersHelp +nl+
           '  -n / --no-screen-resize' +nl+
           '                        Do not try to resize the screen.' +nl+
           nl+
           Window.ParseParametersHelp([poDisplay], true) +nl+
           nl+
           'Debug (use only if you know what you''re doing):' +nl+
           '  --debug-menu-designer   Toggle menu designer by F12' +nl+
           '  --debug-menu-fps      Window caption will show intro/menus FPS' +nl+
           '  --debug-no-creatures  Do not load creatures. Only for developers,' +nl+
           '                        avoids loading creatures (may cause crashes' +nl+
           '                        if you don''t know what you''re doing).' +nl+
           '  --debug-log           Print log on StdOut. Be sure to redirect' +nl+
           '                        if running on Windows.'
           );
         Halt;
       end;
    1: begin
         WritelnStr(Version);
         Halt;
       end;
    2: WasParam_NoScreenChange := true;
    3: DebugMenuDesignerAllowed := true;
    4: DebugMenuFps := true;
    5: InitializeLog(Version);
    6: DebugNoCreatures := true;
    else raise EInternalError.Create('OptionProc');
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'rift';
end;

const
  DefaultWidth = 1024;
  DefaultHeight = 768;

{ main -------------------------------------------------------------------- }

begin
  { This is needed because
    - I sometimes display ApplicationName for user, and under Windows
      ParamStr(0) is ugly uppercased.
    - ParamStr(0) is unsure for Unixes.
    - ApplicationConfig uses this. }
  OnGetApplicationName := @MyGetApplicationName;

  //InitializeLog;

  { configure Notifications }
  Notifications.MaxMessages := 4;
  Notifications.Color := Vector4Single(0.8, 0.8, 0.8, 1.0);

  UserConfig.Load;
  SoundEngine.LoadFromConfig(UserConfig);

  { window configfuration (may be overridden by command-line parameters) }
  Window.Width := DefaultWidth;
  Window.Height := DefaultHeight;
  Window.ColorBits := ColorDepthBits;
  Window.FullScreen := true;
  Window.Container.UIReferenceWidth := DefaultWidth;
  Window.Container.UIReferenceHeight := DefaultHeight;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { parse parameters }
  SoundEngine.ParseParameters;
  Window.ParseParameters;
  Parameters.Parse(Options, @OptionProc, nil);

  if Window.FullScreen and not WasParam_NoScreenChange then
  begin
    if (Application.ScreenWidth <> DefaultWidth) or
       (Application.ScreenHeight <> DefaultHeight) or
       (VideoFrequency <> 0) or
       (ColorDepthBits <> 0) then
    begin
      Application.VideoColorBits := ColorDepthBits;
      Application.VideoFrequency := VideoFrequency;
      Application.VideoResize := true;
      Application.VideoResizeWidth := DefaultWidth;
      Application.VideoResizeHeight := DefaultHeight;

      if not Application.TryVideoChange then
      begin
        WarningWrite('Can''t change display settings to: ' +nl+
          Application.VideoSettingsDescribe +
          'Now I will just continue with default system settings. ');
        { AllowScreenChange := false; }
      end;
    end;
  end;

  { open window }
  Window.Caption := 'The Rift';
  Window.ResizeAllowed := raOnlyAtOpen;
  Window.StencilBits := 8;
  Window.Open;

  { init progress }
  Application.MainWindow := Window;
  Progress.UserInterface := WindowProgressInterface;

  { open OpenAL (after opening Window, because ALContextOpen
    wants to display progress of "Loading sounds") }
  SoundEngine.ALContextOpen;

  DoIntro;
  DoMainMenu;

  { unload all }
  CreaturesKinds.UnLoad;

  SoundEngine.SaveToConfig(UserConfig);
  UserConfig.Save;
end.
