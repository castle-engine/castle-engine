program model_3d_viewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainf, LazOpenGLContext, castle_engine_base,
  castle_engine_lcl, OpenGLInformation, consolef;

begin
  Application.Title := 'Castle Game Engine - Lazarus Model Viewer';
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

