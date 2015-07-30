{$mode objfpc}{$H+}
{$apptype GUI}
program alienoutpost_standalone;
uses CastleWindow, Game;
begin
  Application.MainWindow := Application.DefaultWindowClass.Create(Application);
  Application.MainWindow.OpenAndRun;
end.
