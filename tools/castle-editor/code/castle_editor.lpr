program castle_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormChooseProject, castle_components, ProjectUtils, FormNewProject,
  EditorUtils, FormProject, FrameDesign, FormAbout, FrameViewFile,
  FormPreferences, VisualizeTransform;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TChooseProjectForm, ChooseProjectForm);
  Application.CreateForm(TNewProjectForm, NewProjectForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.Run;
end.

