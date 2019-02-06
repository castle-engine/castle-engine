## TODO

------------------------------------------------------------------------------
Before 6.6 release:

* warning when project with editor_units opened in vanilla editor

* when trying to drag to resize, we could show a hint from EditorAllowResize
  (ResizeDisabledReason) somewhere
  (at tooltip)?

* Define EditorAllowResize for
  castledialogstates_dialog.inc
  castlecontrols_progressbar.inc
  castleonscreenmenu.pas
  castleinspectorcontrol.pas
  castlenotifications.pas
  castlecontrols_touchcontrol.inc
  castlecontrols_groups.inc
  castlecontrols_crosshair.inc

* Add TCastleImageComponent, manually make all UI controls use it
  See /home/michalis/common/TODO/castle-engine/editor-castleimages_components.inc

  *All* images from theme should also be customizable at the control level,
  and naming should be consistent

* Anchors tab keeps getting deselected for some reason when moving UI control

* TCastleButton:
- we need a way to adjust various images of tcastlebutton
  See /home/michalis/common/TODO/castle-engine/editor/castleimages_components.inc
  Also special descendant for 3x3 images, with corners property (or maybe it should always have 3x3 information?)
- Simplify property names, just Color and UseColor and BackgroundImage, less usage of "Custom" prefix
- Test a way to upgrade names in design files.

* build tool integration:
    * when running, provide CGE libs on path for Windows? Should this maybe be done by build tool, actually?
    * Smartly detect CASTLE_ENGINE_PATH, and set it for subprocesses, see Michalis ~/common/TODO
    * Smartly detect castle-engine exe (look in CASTLE_ENGINE_PATH/bin etc.), see Michalis ~/common/TODO
    * Detect lack of FPC / Delphi and make a nice error message

* Show on recent list %20 as spaces, use URICaption or such ready function?

* rename TUIState -> TCastleState? (best)
  TCastleUserInterfaceState?
  TCastleForm?

* 3d gizmos to translate / rotate / scale

* MainScene cannot be changed
  (we disabled in object inspector some types, maybe we should not?)
  Is it deserialized OK? Unsure, as headlight with hlMainScene doesn't shine.
  Saving back suggests it's not deserialized OK now.

* force non-empty Name on all, to have wokring streaming?

  unless it's already forced, is it possible to set name='' without
  exception from SetName?

* show Background.Color := Vector4(0.1, 0.1, 0.1, 1); under default control

* publish and save SceneManager.NavigationType
  and last camera
  { Use initial camera settings stored in
    InitialCameraPosition,
    InitialCameraDirection,
    InitialCameraUp
    values. They are used if you create a camera using one of the
    TCastleAbstractViewport methods, like RequiredCamera or WalkCamera
    or ExamineCamera. They will not be used if you assign to @link(Camera)
    your own camera instance. }
  StoreInitialCamera: Boolean
  InitialCamera

* Make files browser at least basic: ignore castle-engine-output
    * filter out stuff in "Files" (castle-engine-output, *~, created binaries)
      (need to use custom draw for this? grep, search code)
    * "Files" showroot=false doesn't work?
    * need to fork Lazarus shell control for this, no other solution?

* TEditDirectory use at "new project"

* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
    * Some (or all?) templates should show using TUIState. This is our ultimate flexible architecture to develop “pure games” applications (where OpenGL context is your only user-interface): TCastleWindow with a number of TUIState instances using TCastleUserInterface inside.

* TCastle2DScene design cannot load if you don't use Castle2DSceneManager unit.
  Show better message for XxxLoad:

    The class "%s" cannot be loaded from the design file. You should add to the "uses" clause a unit that calls "RegisterSerializableComponent(%s,...);". For example, to allow loading TCastle2DScene class, add the unit Castle2DSceneManager.

* unpublish HeadlightFromViewport, since unsure (deprecated even, or planned to be deprecated?)

* Scene.Rendering (new Scene.Attributes) should be subcomponent and published

------------------------------------------------------------------------------
Lower priority:
OK if after nearest release:

* castle-data:/ support in castlefindfiles.pas too

* make https://github.com/castle-engine/blaise-pascal-article-examples/
  version using editor for level,
  3d_game_alternative_using_editor
  mention in README

* more colors, vectors published props:

  grep for TCastleColor*, TVector* properties and add everything.
  For now we only browsed stuff in src/ui/opengl/ , and TCastleTransform.

* F1 help to API reference, show also in menu
    for now just go to API reference main page?

* When adding new item to hierarchy,
  in general: when doing UpdateDesign,
  preserve previous state of
  - collapsed/not collapsed
  - scrollbar of scrollbar
  - maybe just traverse existing tree and only "fix", removing/adding what is necessary

* Visual inspector. designer etc. less important
    * Dragging UI: should "Snap" snap to the final value (like an invisible grid?)
      Would be more like Delphi/Lazarus, probably.
      Right now we are more like Blender, only movement amount is snapped.
    * open last design in the project,
      open the only scene in the project, if only one exists?
    * does recursive saving work when Tcastletransform is present multiple times in graph?
    * Allow editing of:
        * initial animation? along with TimePlayingSpeed, ProcessEvents

    * adding - better UI? component palette?
      after adding, keep previously selected still selected
      content menu on hierarchy, to add transform/ui depending on parent
    * removing - keep selected above?
    * UI controls improvements:
        * Maybe TCastleUserInterface should have color property too . Just make TCastleReactngleControl a descendant with white opaque color by default.

* Add components tab at the bottom, with large icon for each component?

* Allow to attach rigidbody and collision instances.

  Allow to autosynchronize them (e.g. size) with size of the model?

* allow to control auto-scaling

    when clicking on 44% (currently shows a hint about UI scaling) a dialog:

    Configure user interface scaling

    User interface scaling simulates a window of a particular size by adjusting the coordinates internally used by the UI controls. In effect, you can set hardcoded values for controls sizes, and they will take the same portion of the final window. You should still carefully set the anchors of your UI controls, as the final window may have various aspect ratios.

    - No user interface scaling
    - Window fits inside the simulated area size
    - Window encloses simulated area size (Recommended)

    (From code, you can control this using Window.Container.UIScalingXxx properties.)

* ugly button in example? new ui for internal controls?

* Make files browser with features as documented.
    Also to allow dropping scenes/images on UI design.

* build tool integration:
    * For "run", colorized CastleLog warnings
    * For "compile", colorize FPC warnings, errors
    * Shorter compile output:
        * lines "compiling..", "writing resource string table...", are displayed, but then replaced by a next AddLine. This way they serve as "progress indicator" but do not eat so much output space.
        * also do not show FPC "logo", do not repeat information about FPC version, Os/CPU 2 times, debug mode,...
        * remove the "separator" lines. The bold lines already separate them nicely?
        * "command finished with status 0" -> "Command finished successfully."
    * show count of warnings/errors if non-zero on tab header, allow to filter by them
    * Allow to choose platform
    * rerun generate-program each time? (mark them as some *DO NOT MODIFY THIS, THIS IS ONLY FOR LAZARUS* comment)
        Not really OK, in case we open program with hand-crafted program file.
        Maybe only auto-generate in castle-engine-output,
          before opening Lazarus,
          if lpr not yet present?
        Best: change to use planned CastleEngineConfig.pas unit,
          that is always auto-generated without warning.
          Do not overwrite lpr each time.
    * checkbox in menu for verbose output from the build tool
    * use machine-readble format format for communication with build tool and CastleLog when CASTLE_ENGINE_EDITOR_INSIDE=true

        Causes build tool some lines (e.g. in verbose fpc command line) to use special format, and actual program uses CastleLog that has special output (and always goes to console even on Windows)
        - Avoid xml tags here (would require quoting rest).
        - Just tags like Cge-output, bytes=xxx:
        - Cge-output,multiline,...
        - Cge-output, warning,...
        - bytes are always required and allow reliably waiting reading up to message end, without the need to quote/unquote it

    * Detect multilibe logs and show as one list item in output that can be expanded,, only category initially visible. E.g. useful for
      - "OpenGL Information" or
      - command-line of FPC in build tool verbose mode

* small GUI stuff:
    * on NewProject form AutoSize?
    * allow switching list/icon/etc. view on "Files"
    * allow configuring command output "word wrap"
    * Output ListBox has some width (and horiz scrollbar) on Windows, unrelated to anything?
    * remember ProjectForm state of maximized/not

* move CastleComponentSerialize to src/base/
  And UserInterfaceLoad, TransformLoad move to appropriate units with these classes.

* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor
* Desing also X3D nodes inside TCastleScene. This would be powerful... But not for now. I also deliberately do not want to turn CGE editor into Blender :) For creating 3D models, the recommended workflow will remain to use external editor (like Blender), and only e.g. adjust materials in CGE (override material properties using material_properties.xml). An editor for X3D nodes would be great to add stuff not possible in Blender, though (Background, clip planes, primitives...).

* For editor on Lazarus at design-time:
    * TCastleControl (or sthg else) in designer mode should set ApplicationDataOverride,
        to allow our dialogs to replace URL with castle-data:/ nicely.
    * saving TCastleColorPersistent to LFM for now doesn't work?
        maybe ignore, we will save TCastleControl to JSON?

* setting PrimitiveGeometry to pgSphere,
  clears URL,
  but it is not visible in object inspector immediately (it is not redrawn, it seems?)

* at scene loading show something "wait, loading..."

* "Camera View All" button more prominent?
  Maybe attached to scene manager in designer mode?

* allow to import file with textures, audio, inline tracked
  as alternative at warning message when opening file outside of castle-data:

* When something is anchored to top-right,
  resizing by dragging left border makes unpleasant visual effect.
  Weird resizing by dragging bottom border is OK.
  On GTK2, with LCL 1.8.0.

------------------------------------------------------------------------------
"castle-engine editor" improvements:

- Make editor warn when opening project with editor_units, but castle_editor_automatic_package not compiled in. (Known registered packages? Or just unit castleeditorcustomcomponents.pas can set some global variable?)

  """
  Warning: You are opening a project with possible custom components (editor_units attribute in CastleEngineManifest.xml), but these custom components are not compiled-in in this editor version. Use menu item "Restart editor in this project" to get the correct editor version.
  """

- Do not rebuild "castle-engine editor" when not needed. It seems we should compare generated lpi/lpk, and *do not* overwrite (to not change timestamp) when equal. This way Lazarus xxx.compiled files will work.

    Hm, risky. Lazarus will then not recompile when only editor_units changed.
