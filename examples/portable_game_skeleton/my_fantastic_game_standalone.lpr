{$mode objfpc}{$H+}

{ This contains icons and version info, automatically created by "castle-engine compile".
  You can comment this out if you compile through other means. }
{$ifdef MSWINDOWS} {$R automatic-windows-resources.res} {$endif MSWINDOWS}

{$apptype GUI}
program my_fantastic_game_standalone;
uses CastleWindow, Game;
begin
  Window.OpenAndRun;
end.
