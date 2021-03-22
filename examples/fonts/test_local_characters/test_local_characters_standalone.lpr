{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

program test_local_characters_standalone;
uses CastleWindow, GameInitialize;
begin
  Window.OpenAndRun;
end.
