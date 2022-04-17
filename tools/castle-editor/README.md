# Castle Game Engine Editor

Create, build, edit Castle Game Engine projects.

See the [manual documenting the editor](https://castle-engine.io/manual_editor.php).

## Contributing

- Use desktop settings with 125% font scaling. Unfortunately, your personal desktop settings, at design-time, affect what is saved in LFM files, so it is best if we all use the same scaling, otherwise diffs to LFM files wildly change everything.

    You can set such scaling e.g. by GNOME 3 _"Large fonts"_ accessibilty option, or by adjusting Xorg dpi to 120 (96 * 1.25), Windows also allows to set 125% scaling.

### Contributing

When creating a new Lazarus form, remember to:

- Save form class `TFooForm` (so it will have singleton `FooForm`) in unit name `FormFoo`.
- Adjust form's `Caption`.
- Adjust `TabStop` of all the controls inside, to make it comfortable to use keyboard.
- Use `AutoSize` and anchoring on all controls, to work regardless of theme font size. Do not assume that a text will have the same size as you have designed --- people use various themes and font types. Lazarus applications have a native look, and are expected to adjust to user's theme preferences.
- Consider using `AutoSize` on the form itself too.
- Adjust `BorderStyle` from `bsSizeable` to `bsSingle` if it's a small form that doesn't need to be resized (for larger forms, it's safer to allow resizing, even if you think you know the best size -- in case user will view it on a smaller monitor).
- Adjust `Position` from "as designed" (usually "default" or "main form center" is more sensible).
- Adjust `ShowInTaskBar`, this is important for selecting form on Windows in case of multiple windows.
- Make sure closing the form with "X" (Alt + F4) works OK.
- For a form you create manually, make sure it is freed at some point (preferably, not only at the end of application, if you can free it earlier; e.g. we don't want to have 100 of TProjectForm instances in memory after using the editor for a long time).

When adding new units, like forms, make sure you synchronize the custom editor template. Just

```
cd ../build-tool/data/
./custom_editor_template_rebuild.sh
```

Units called `DesignXxx` are helper units for `FrameDesign`, i.e. for design editing.

## License

The **Castle Game Engine Editor** is provided on the GNU GPL >= 2 license terms.
*Not* on more permissive terms of LGPL (with static linking exception) >= 2
(that are used for the rest of the Castle Game Engine).
See the file ../../COPYING.md for details.
This means that:

- You can use the editor to create your own closed-source programs,
- but you cannot fork the editor into a closed-source program (and distribute it publicly).
- You are of course free to extend Castle Game Engine editor for internal purposes (if you don't distribute it publicly, the GPL license terms don't concern you), or you can extend it publicly on terms of GPL (thus, keeping the whole work open-source).

When contributing (sending pull requests etc.) to the castle-editor source code,
you agree that your contributions may be used under either GPL
or a more permissive "LGPL with static linking exception" terms,
at the discretion of _Castle Game Engine Developers_.
_Castle Game Engine Developers_ are defined as _people with write (commit) access
to the official CGE version control repository_
(referred to from https://castle-engine.io/ , currently
https://github.com/castle-engine/castle-engine/ ).
The idea is that we sometimes want to move code from castle-editor to
the engine core, for technical reasons, and we want the freedom to do so.
Still, the editor stays GPL for the general public.
