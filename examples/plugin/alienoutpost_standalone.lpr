{$mode objfpc}{$H+}
{$apptype GUI}
program alienoutpost_standalone;
{$ifdef MSWINDOWS}
  {$R automatic-windows-resources.res}
{$endif MSWINDOWS}
uses CastleWindow, Game;
begin
  Application.MainWindow := Application.DefaultWindowClass.Create(Application);
  Application.MainWindow.OpenAndRun;
end.
