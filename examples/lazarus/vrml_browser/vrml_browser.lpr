program vrml_browser;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainf, LazOpenGLContext, castle_base,
  castle_components, OpenGLInformation, vrmlconsolef;

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

