{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file. }
unit GameInitialize;

interface

uses CastleWindow;

var
  Window1, Window2: TCastleWindow;

implementation

uses SysUtils,
  CastleLog, CastleUIControls, CastleViewport
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewMain
  {$endregion 'Castle Initialization Uses'};

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  { Adjust ViewportToChange.Items, changing also camera parent.
    See https://castle-engine.io/multiple_viewports_to_display_one_world }
  procedure ShareWorld(const TargetViewport, ViewportToChange: TCastleViewport);
  begin
    ViewportToChange.Items.Remove(ViewportToChange.Camera);
    ViewportToChange.Items := TargetViewport.Items;
    ViewportToChange.Items.Add(ViewportToChange.Camera);
  end;

begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window1.Container.LoadSettings('castle-data:/CastleSettings.xml');
  Window2.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Create views (see https://castle-engine.io/views ). }
  ViewMainInWindow1 := TViewMain.Create(Application);
  ViewMainInWindow2 := TViewMain.Create(Application);

  Window1.Container.View := ViewMainInWindow1;
  Window2.Container.View := ViewMainInWindow2;

  { Make all 4 viewports refer to one world,
    from ViewMainInWindow1.ViewportTop.Items }
  ShareWorld(ViewMainInWindow1.ViewportTop, ViewMainInWindow1.ViewportBottom);
  ShareWorld(ViewMainInWindow1.ViewportTop, ViewMainInWindow2.ViewportTop);
  ShareWorld(ViewMainInWindow1.ViewportTop, ViewMainInWindow2.ViewportBottom);
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window1 := TCastleWindow.Create(Application);
  Window2 := TCastleWindow.Create(Application);
  Application.MainWindow := Window1;

  Window1.Left := Application.ScreenWidth div 9;
  Window1.Width := Application.ScreenWidth div 3;

  Window2.Left := Application.ScreenWidth - Application.ScreenWidth div 9 - Application.ScreenWidth div 3;
  Window2.Width := Application.ScreenWidth div 3;

  { Open Window2 now. Window1 will be open by main application file.
    This way of opening many windows is nice to keep auto-generated
    main application file (DPR).
    You can also easily ifdef-out the creation of Window2
    on systems that don't support multiple windows,
    so on Android or iOS will have only Window1.

    Alternative approach would be to modify DPR to open all windows
    in a desired order, like

      Window1.Open;
      Window2.Open;
      Application.Run;

    instead of Window1.OpenAndRun.
    This would be more explicit, and it makes sense if you really
    target only desktops (where multiple windows are supported).
  }
  Window2.Open;

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Application.MainWindow.ParseParameters;
end.
