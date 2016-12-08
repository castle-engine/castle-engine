{$mode objfpc}{$H+}
{$apptype GUI}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile".
  Comment this out if you don't compile using our "castle-engine" build tool. }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

program my_fantastic_game_standalone;
uses CastleWindow, Game;
begin
  Window.OpenAndRun;
end.
