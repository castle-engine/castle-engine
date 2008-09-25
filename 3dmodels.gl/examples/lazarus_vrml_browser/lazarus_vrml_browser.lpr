program view3dscene_mini_by_lazarus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainf, LazOpenGLContext, kambi_units,
  kambi_components, OpenGLInformation;

begin
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

