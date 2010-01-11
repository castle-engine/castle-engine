program camera;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainf, LResources, kambi_components, kambi_base
  { you can add units after this };

{$IFDEF WINDOWS}{$R camera.rc}{$ENDIF}

begin
  {$I camera.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

