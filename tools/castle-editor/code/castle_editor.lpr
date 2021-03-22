program castle_editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  // packages:
  castle_components,
  // uncomment this line in custom editor template LPI:
  //castle_editor_automatic_package,
  Forms, FormChooseProject, ProjectUtils, FormNewProject,
  EditorUtils, FormProject, FrameDesign, FormAbout, FrameViewFile,
  FormPreferences, VisualizeTransform, FormSpriteSheetEditor, DataModuleIcons,
  FormImportAtlas, FormImportStarling, FormNewUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  { When this code changes (e.g. new auto-created form is added),
    remember to also update ../../build-tool/data/custom_editor_template/castle_editor.lpr }
  Application.CreateForm(TIcons, Icons);
  Application.CreateForm(TChooseProjectForm, ChooseProjectForm);
  Application.CreateForm(TNewProjectForm, NewProjectForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TImportAtlasForm, ImportAtlasForm);
  Application.CreateForm(TImportStarlingForm, ImportStarlingForm);
  Application.CreateForm(TNewUnitForm, NewUnitForm);
  Application.Run;
end.

