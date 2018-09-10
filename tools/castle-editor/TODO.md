## TODO

Now:

* Visual inspector. designer etc.
    * some way to disable auto-selecting,
      to allow moving parent when it is difficult to reach,
      or to allow moving e.g. camera of scene manager instead of moving sceme manager
    * update property grid when moving by mouse immediately
    * Anchors tab keeps getting deselected for some reason when moving UI control
    * Snap = 5.0; // TODO: configurable
    * why moving scenemanager with fullsize=false doesn;t work?
      changing Left works,
      but changing HorizontalAnchorDelta doesn't?
    * check scene with primitive is (de)serialized ok
    * Dragging UI:
      * "Anchors" tab
	Parent and Self anchors are the same (checkbox, if not -- separate 3x3 controls)
      * Allow clicking on control, to select and drag (move)
      * When dragging don't allow to drag outside of parent
      * Drag by multiply, like 5, configurable
      * Make Label for fps (in template) now designed in editor
    * Button at scene manager: Camera View All
    * Property editor for TCastleColorPersistent
    * F1 help to API reference, show also in menu
      for now just go to API reference main page?
    * Component wrapper will also need info about what is default, to know what to save...
      to json,
      and for lfm -- se TCastleColorPersistent.DefineProperties todo
    * open last design in the project,
      open the only scene in the project, if only one exists?
    * does recursive saving work when Tcastletransform is present multiple times in graph?
    * Allow editing of:
	* initial animation? along with TimePlayingSpeed, ProcessEvents
        * TCastleTransform position, rotation, scale (using gizmos)
        * TCastleUserInterface anchors (self, parent -- together in simple ver, as 3x3 grid) and (using gizmo) delta to anchor
    * save also vectors, colors.
      Like position, rotation, scale as TVector3 properties should be fixed --- need to expose them as published, see TODOs, probably.
      See /home/michalis/common/TODO/castle-engine/editor/castlevectors_components.inc
      and /home/michalis/common/TODO/castle-engine/editor/cge-editor-older-notes-published-vectors.txt
      started (TCastleRectangleControl.Color works), needs to be automated for other now.

      Autogenerate wrappers for vectors, colors

    * Add TCastleImageComponent, manually make all UI controls use it
      See /home/michalis/common/TODO/castle-engine/editor/castleimages_components.inc

    * moving added things in hierarchy (dragging in tree).
    * adding - better UI? component palette?
      after adding, keep previously selected still selected
      content menu on hierarchy, to add transform/ui depending on parent
    * removing - keep selected above?
    * UI controls improvements:
	* more should descend from TCastleUserInterfaceRect, e.g. TCastleButton/Label//ImageControl too.
	  At TCastleUserInterfaceRect document:

	    Some descendants support auto-sizing, which means that the control's size
	    set by these properties is ignored, and instead the calculated size
	    (CalculatedWidth, CalculatedHeight, CalculatedRect) depends on some core
	    values of the control. E.g. TCastleImageControl adjusts to image,
	    TCastleLabel adjusts to caption, TCastleButton adjusts to button and icon,
	    TCastleVerticalGroup adjusts to all children and so on.
	    Consult the documentation of each descendant for the exact
	    specification of behavior, usually a property called @code(AutoSize) controls it. }

	* virtual functions to say VerticalResizingEffective
	  (at TCastleUserInterface or only TCastleUserInterfaceRect)
    * design files UI:
        * ask before overriding saved file
	* before opening new one - ask whether to save design
        * before exiting - ask whether to save design

    * mark Width, Height as stored=false when FloatWidth, FloatHeight available
    * force non-empty Name on all, to have wokring streaming?
    * show checkerboard instead of Background.Color := Vector4(0.5, 0.5, 0.5, 1);, to make it clear it's undefind
    * MainScene cannot be changed
      (we disabled in object inspector some types, maybe we should not?)
      Is it deserialized OK? Unsure, as headlight with hlMainScene doesn't shine.
      Saving back suggests it's not deserialized OK now.
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

    * TCastleButton:
      - Pressed must be editable even when not toggle?
	or maybe not published
      - EnableParentDragging should not be published (and should be read-only?)
      - we need a way to adjust various images of tcastlebutton
        See /home/michalis/common/TODO/castle-engine/editor/castleimages_components.inc
      	Also special descendant for 3x3 images, with corners property (or maybe it should always have 3x3 information?)
      - Simplify property names, just Color and UseColor and BackgroundImage, less usage of "Custom" prefix

    * castle-data:/ finish
      - support castlefindfiles.pas too
      - document at ApplicationData
        See /home/michalis/common/TODO/castle-engine/editor/castle-data-url.txt

    * unpublish KeepInFront, since switching it at runtime is not supported
    * unpublish HeadlightFromViewport, since unsure (deprecated even, or planned to be deprecated?)
    * TLabel.Text using prop editor (multiline) setting is ignored
      (we should react to Text.Assign maybe?)

    * Scene.Rendering (new Scene.Attributes) should be subcomponent and published

    * after changing scene manager items (or scene url),
      recalculate camera box to have zoom working

------------------------------------------------------------------------------
Lower priority:
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

* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
    * Some (or all?) templates should show using TUIState. This is our ultimate flexible architecture to develop “pure games” applications (where OpenGL context is your only user-interface): TCastleWindow with a number of TUIState instances using TCastleUserInterface inside.

* build tool integration:
    * when running, provide CGE libs on path for Windows? Should this maybe be done by build tool, actually?
    * For "run", colorized CastleLog warnings
    * For "compile", colorize FPC warnings, errors
    * Shorter compile output:
        * lines "compiling..", "writing resource string table...", are displayed, but then replaced by a next AddLine. This way they serve as "progress indicator" but do not eat so much output space.
        * also do not show FPC "logo", do not repeat information about FPC version, Os/CPU 2 times, debug mode,...
        * remove the "separator" lines. The bold lines already separate them nicely?
        * "command finished with status 0" -> "Command finished successfully."
    * show count of warnings/errors if non-zero on tab header, allow to filter by them
    * Smartly detect CASTLE_ENGINE_PATH, and set it for subprocesses, see Michalis ~/common/TODO
    * Smartly detect castl-engine exe (look in CASTLE_ENGINE_PATH/bin etc.), see Michalis ~/common/TODO
    * Detect lack of FPC / Delphi and make a nice error message
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
    * Show on recent list %20 as spaces, use URICaption or such ready function?
    * on NewProject form AutoSize?
    * allow switching list/icon/etc. view on "Files"
    * allow configuring command output "word wrap"
    * filter out stuff in "Files" (castle-engine-output, *~, created binaries)
      (need to use custom draw for this? grep, search code)
    * "Files" showroot=false doesn't work?
    * TEditDirectory use at "new project"
    * Output ListBox has some width (and horiz scrollbar) on Windows, unrelated to anything?
    * remember ProjectForm state of maximized/not

------------------------------------------------------------------------------
Lowest priority (OK if not in 1st release):

* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor
* Desing also X3D nodes inside TCastleScene. This would be powerful... But not for now. I also deliberately do not want to turn CGE editor into Blender :) For creating 3D models, the recommended workflow will remain to use external editor (like Blender), and only e.g. adjust materials in CGE (override material properties using material_properties.xml). An editor for X3D nodes would be great to add stuff not possible in Blender, though (Background, clip planes, primitives...).

* For editor on Lazarus at design-time:
    * TCastleControl (or sthg else) in designer mode should set ApplicationDataOverride,
        to allow our dialogs to replace URL with castle-data:/ nicely.
    * object inspector editing vectors/colors question:
	+  // TODO: Why these are necessary to expand in castle-editor,
	+  // but in Lazarus at least TCastleVector3Persistent in test project was
	+  // expanded without this?
	+  RegisterPropertyEditor(TypeInfo(TCastleColorPersistent), nil, '',
	+    TSubPropertiesEditor);
	+  RegisterPropertyEditor(TypeInfo(TCastleColorRGBPersistent), nil, '',
	+    TSubPropertiesEditor);
	+  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
	+    TSubPropertiesEditor);
	+  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
	+    TSubPropertiesEditor);
    * saving TCastleColorPersistent to LFM for now doesn't work?

* setting PrimitiveGeometry to pgSphere,
  clears URL,
  but it is not visible in object inspector immediately (it is not redrawn, it seems?)
