program castle_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ChooseProjectForm, castle_components, ProjectUtils, NewProjectForm,
  EditorUtils
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TChooseProject, ChooseProject);
  Application.CreateForm(TNewProject, NewProject);
  Application.Run;
end.

