## TODO

------------------------------------------------------------------------------
Lower priority:
OK if after 7.0 release:

* Define EditorAllowResize for
  castlenotifications.pas
  TCastleTouchNavigation

* TCastleButton:
  - Simplify property names, just Color and UseColor and BackgroundImage, less usage of "Custom" prefix
  - Test a way to upgrade names in design files while doing above?

* *All* images from theme should also be customizable at the control level,
  and naming should be consistent.

* ? on Layout tab, new button for TCastleImageControl
  "Set Size Explicitly From Current Image"
  Unsure how much use-case.

* Visual inspector. designer etc. less important
    * Dragging UI: should "Snap" snap to the final value (like an invisible grid?)
      Would be more like Delphi/Lazarus, probably.
      Right now we are more like Blender, only movement amount is snapped.
    * ? open last design in the project,
      ? open the only scene in the project, if only one exists
      for now, list of views is good solution.
    * after adding, keep previously selected still selected
    * removing - keep selected above?

* Add components tab at the bottom, with large icon for each component

* allow to control auto-scaling

    when clicking on 44% (currently shows a hint about UI scaling) a dialog:

    Configure user interface scaling

    User interface scaling simulates a window of a particular size by adjusting the coordinates internally used by the UI controls. In effect, you can set hardcoded values for controls sizes, and they will take the same portion of the final window. You should still carefully set the anchors of your UI controls, as the final window may have various aspect ratios.

    - No user interface scaling
    - Window fits inside the simulated area size
    - Window encloses simulated area size (Recommended)

    (From code, you can control this using Window.Container.UIScalingXxx properties.)

* Make files browser with features as planned.
    * allow dropping scenes/images on UI design - done, but show preview when dropping
    * on audio files, you can open them with `examples/audio/audio_player/` (should this be moved to tools directory? probably!)

* build tool integration:
    * For "run", colorized CastleLog warnings
    * For "compile", colorize FPC warnings, errors
    * Shorter compile output:
        * lines "compiling..", "writing resource string table...", are displayed, but then replaced by a next AddLine. This way they serve as "progress indicator" but do not eat so much output space.
        * also do not show FPC "logo", do not repeat information about FPC version, Os/CPU 2 times, debug mode,...
        * remove the "separator" lines. The bold lines already separate them nicely?
        * "command finished with status 0" -> "Command finished successfully."
    * show count of warnings/errors if non-zero on tab header, allow to filter by them
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

* Project options:
    * Allow to configure project qualified name from "Project Options" in editor
    * Icon, other stuff from CastleEngineManifest.xml could be configuirable in editor

* For editor on Lazarus at design-time:
    * TCastleControl (or sthg else) in designer mode should set ApplicationDataOverride,
        to allow our dialogs to replace URL with castle-data:/ nicely.

* at scene loading show something "wait, loading..."

* allow to import file with textures, audio, inline tracked
  as alternative at warning message when opening file outside of castle-data:

* When something is anchored to top-right,
  resizing by dragging left border makes unpleasant visual effect.
  Weird resizing by dragging bottom border is OK.
  On GTK2, with LCL 1.8.0.

------------------------------------------------------------------------------
"castle-engine editor" improvements:

- Do not rebuild "castle-engine editor" when not needed. It seems we should compare generated lpi/lpk, and *do not* overwrite (to not change timestamp) when equal. This way Lazarus xxx.compiled files will work.

    Hm, risky. Lazarus will then not recompile when only editor_units changed.
