{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Castle Game Engine editor.

  See https://castle-engine.io/manual_editor.php about editor usage.
  See README.md about editor development.

  When this LPR code changes (e.g. new auto-created form is added),
  remember to rerun ../build-tool/data/custom_editor_template_rebuild.sh . }
program castle_editor;

{$mode objfpc}{$H+}

{ CGE applications use threads for
  - music streaming (when TCastleSound.Stream = @true)
  - asynchronous downloading (TCastleDownload with protocols like http/https)
  - and maybe more in the future. }
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  // packages:
  castle_components,
  // This line was uncommented by tools/build-tool/data/custom_editor_template_rebuild.sh
  castle_editor_automatic_package,
  Forms, anchordockpkg, FormChooseProject, ProjectUtils, FormNewProject,
  EditorUtils, FormProject, FrameDesign, FormAbout, FrameViewFile,
  FormPreferences, DesignVisualizeTransform, FormSpriteSheetEditor, DataModuleIcons,
  FormImportAtlas, FormImportStarling, FormNewUnit, EditorCodeTools,
  CastleShellCtrls, FormSystemInformation;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TIcons, Icons);
  Application.CreateForm(TChooseProjectForm, ChooseProjectForm);
  Application.CreateForm(TNewProjectForm, NewProjectForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TImportAtlasForm, ImportAtlasForm);
  Application.CreateForm(TImportStarlingForm, ImportStarlingForm);
  Application.CreateForm(TNewUnitForm, NewUnitForm);
  Application.CreateForm(TSystemInformationForm, SystemInformationForm);
  Application.Run;
end.
