program castle_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormChooseProject, castle_components, ProjectUtils, FormNewProject,
  EditorUtils, FormProject
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TChooseProjectForm, ChooseProjectForm);
  Application.CreateForm(TNewProjectForm, NewProjectForm);
  { On Windows, Position = poDefault is all over the place,
    never where one expects to see a new application. }
  {$ifdef MSWINDOWS}
  ChooseProjectForm.Position := poScreenCenter;
  {$endif}
  Application.Run;
end.

