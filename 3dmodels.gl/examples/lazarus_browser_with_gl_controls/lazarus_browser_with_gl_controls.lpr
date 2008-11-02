program Project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { you can add units after this }, MainF, LResources;

{$IFDEF WINDOWS}{$R lazarus_browser_with_gl_controls.rc}{$ENDIF}

begin
  {$I lazarus_browser_with_gl_controls.lrs}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

