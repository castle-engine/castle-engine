program castle_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormChooseProject, castle_components, ProjectUtils, FormNewProject,
  EditorUtils, FormProject, FrameDesign
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TChooseProjectForm, ChooseProjectForm);
  Application.CreateForm(TNewProjectForm, NewProjectForm);
  Application.Run;
end.

