## TODO

Now:
* Visual inspector. designer etc.
    * Component wrapper will also need info about what is default, to know what to save...
      to json,
      and for lfm -- se TCastleColorPersistent.DefineProperties todo
    * UI of designer needs to be improved to clearly communicate what is happening:
	* File->Open should by default show our project directory,
	  and warn when opening/saving xx.castle-xxx outside of our project
	  ("You are saving or loading a hierarchy (.castle-user-interface or .castle-transform file) outside of the project "data" subdirectory. This is possible, but for typical games not adviced.

	  Typical games should keep all the designs (.castle-user-interface and .castle-transform files) inside the "data" subdirectory to have them packaged in the application on all platforms (desktop, mobile). And you should always load them using the ApplicationData function or the castle-data:/xxx URL.
	  )
	* before exiting ask whether to save.
	* special menu "Designer" with new, open, close. Separate from "File"
	* rest of "File" rename to "Project"
	* initially nothing should be open,
	  and top part of editor should show
	  "Open or create a new
	   - user interface (.castle-user-interface file) or
	   - transformation (.castle-transform file)
	   using the "Designer" menu
	* Designer -> Close (Ctrl + W) menu
    * open last scene in the project,
      open the only scene in the project, if only one exists?
    * does recursive saving work when Tcastletransform is present multiple times in graph?
    * (in-progress) Allow editing at least most important properties:
        * (done) Name
        * (done) TCastleScene.URL
	* (but fix URL to set castle-data:)
	* initial animation? along with TimePlayingSpeed, ProcessEvents
        * TCastleTransform position, rotation, scale (using gizmos)
        * TCastleUserInterface anchors (self, parent -- together in simple ver, as 3x3 grid) and (using gizmo) delta to anchor
    * save also vectors, colors.
      Like position, rotation, scale as TVector3 properties should be fixed --- need to expose them as published, see TODOs, probably.
      And colors, like
        RectangleGroup.Color := Vector4(0.5, 0.5, 1, 0.2); // transparent light-blue

      See /home/michalis/common/TODO/castle-engine/editor/castlevectors_components.inc
      and /home/michalis/common/TODO/castle-engine/editor/cge-editor-older-notes-published-vectors.txt
      Make a minimal test of this, with Delphi and FPC/Lazarus.
      Possibly we can publish records now?

    * Store only non-default with stored=true.
      https://stackoverflow.com/questions/30352756/delphi-how-to-get-default-value-for-property-using-rtti
      http://docwiki.embarcadero.com/Libraries/Berlin/en/System.TypInfo.TPropInfo
      See how normal TWriter does it, using TypInfo.

      yes, we can copy from
      /home/michalis/installed/fpclazarus/3.0.4/fpcsrc/rtl/objpas/classes/writer.inc

      cleanup then data/project_templates/empty/files/data/main.castle-user-interface
      to not contain defaults

    * Allow adding new, deleting, moving around
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
    * remove _Children once processed
    * ask before closing project when HierarchyModified - save? cancel?
    * ask before overriding saved file
    * mark Width, Height as stored=false when FloatWidth, FloatHeight available
    * force non-empty Name on all, to have wokring streaming?
    * show checkerboard instead of Background.Color := Vector4(0.5, 0.5, 0.5, 1);, to make it clear it's undefind
    * MainScene cannot be changed
      (we disabled in object inspector some types, maybe we should not?)
      Is it deserialized OK? Unsure, as headlight with hlMainScene doesn't shine
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

    * castle-data:/ finish
      - support castlefindfiles.pas too
      - document at ApplicationData
        See /home/michalis/common/TODO/castle-engine/editor/castle-data-url.txt

    * unpublish KeepInFront, since switching it at runtime is not supported
    * unpublish HeadlightFromViewport, since unsure (deprecated even, or planned to be deprecated?)
    * TLabel.Text setting is ignored
      (we should react to Text.Assign maybe?)
    * saving TCastleColorPersistent to LFM for now doesn't work?

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

    * opening other data (like 3d models) should also default to data dir,
      and warn if opening outside.

Lower priority:
* ugly button in example? new ui for internal controls?
* Files browser as above
* templates:
    * Create other than "empty" project templates
    * Proper screenshots of all project templates
    * Templates should load by default the visually-designed world. The goal: you should be able to modify and run the game in the editor without writing code.
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

Lowest priority (OK if not in 1st release)
* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor
* Desing also X3D nodes inside TCastleScene. This would be powerful... But not for now. I also deliberately do not want to turn CGE editor into Blender :) For creating 3D models, the recommended workflow will remain to use external editor (like Blender), and only e.g. adjust materials in CGE (override material properties using material_properties.xml). An editor for X3D nodes would be great to add stuff not possible in Blender, though (Background, clip planes, primitives...).
