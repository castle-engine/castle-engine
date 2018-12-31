{$mode objfpc}{$H+}
{$ifdef MSWINDOWS} {$apptype GUI} {$endif}
program tiled_demo_standalone;
uses CastleWindow, GameInitialize;
begin
  Window.OpenAndRun;
end.
