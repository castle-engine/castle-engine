{
  Copyright 2018-2023 Michalis Kamburelis.

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
  FormPreferences, DesignVisualizeTransform, FormSpriteSheetEditor,
  DataModuleIcons, FormImportAtlas, FormImportStarling, FormNewUnit,
  EditorCodeTools, CastleShellCtrls, FormSystemInformation,
  CastleComponentEditorDesigner, DesignCameraPreview, DesignObjectInspector,
  DesignUndoSystem, FormRestartCustomEditor;

{ Forces using a dedicated (faster) GPU on laptops with multiple GPUs.
  See https://castle-engine.io/dedicated_gpu }
{$if (not defined(CASTLE_NO_FORCE_DEDICATED_GPU)) and (defined(cpu386) or defined(cpux64) or defined(cpuamd64)) and (defined(MSWINDOWS) or defined(Linux))}
    {$ifdef fpc}
     {$asmmode intel}
    {$endif}

    procedure NvOptimusEnablement; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    procedure AmdPowerXpressRequestHighPerformance; {$ifdef fpc}assembler; nostackframe;{$endif}
    asm
    {$ifdef cpu64}
    {$ifndef fpc}
     .NOFRAME
    {$endif}
    {$endif}
     dd 1
    end;

    exports
      NvOptimusEnablement,
      AmdPowerXpressRequestHighPerformance;
{$ifend}

{$R *.res}

{ Do not auto-create below forms that use TCastleControl, and would initialize OpenGL
  right when the CGE editor opens.

  Reason: In case someone has broken OpenGL library installation
  we don't want to have CGE editor just crash at start, it's better if it will crash
  later -- allowing us to recognize this case (e.g. because it crashes when you open any
  design or "System Information").

  If there's a single variable to hold single form instance, you can initialize it on-demand, like

    if SystemInformationForm = nil then
      SystemInformationForm := TSystemInformationForm.Create(Application);
    SystemInformationForm.Show;
}

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
  Application.CreateForm(TRestartCustomEditorForm, RestartCustomEditorForm);
  Application.Run;
end.
