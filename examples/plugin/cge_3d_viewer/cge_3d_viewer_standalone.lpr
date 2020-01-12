program cge_3d_viewer_standalone;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses CastleWindow, GameInitialize;
begin
  { explicitly initialize MainWindow with useful NamedParameters,
    to make it similar to what happens when you run a plugin. }
  Application.MainWindow := Application.DefaultWindowClass.Create(Application);
  Application.MainWindow.NamedParameters.Values['cge_scene'] :=
    'https://raw.githubusercontent.com/castle-engine/demo-models/master/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv';
    //'http://localhost/~michalis/test_model/skeleton.json';
  Application.MainWindow.OpenAndRun;
end.
