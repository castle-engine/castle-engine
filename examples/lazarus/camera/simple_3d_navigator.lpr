program simple_3d_navigator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainf, LResources, kambi_components, kambi_base
  { you can add units after this };

{$IFDEF WINDOWS}{$R simple_3d_navigator.rc}{$ENDIF}

begin
  {$I simple_3d_navigator.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

