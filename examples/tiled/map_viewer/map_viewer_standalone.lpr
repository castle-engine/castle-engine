{$mode objfpc}{$H+}
{$ifdef MSWINDOWS} {$apptype GUI} {$endif}
program map_viewer_standalone;
uses CastleWindow, GameInitialize;
begin
  Application.MainWindow.OpenAndRun;
end.
