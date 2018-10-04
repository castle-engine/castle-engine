# Settings Specific to a Project

If your game customizes some per-window CGE settings,
then you want the CGE editor to know about these customizations.
This way CGE editor can show at design-time the same thing
that you will see when you load the designed file
(`xxx.castle-user-interface` or `xxx.castle-transform`) in game.

To do this, place a file called `CastleSettings.xml`
in the `data` subdirectory of your project.
In the future, this file should be editable by CGE editor GUI,
for now you just have to edit it directly.

To make the settings synchronized with the running game, it is best
to call `Window.Container.LoadSettings('castle-data:/CastleSettings.xml');`
early in your Application.OnInitialize callback.
This way whatever changes you make in `CastleSettings.xml` will be visible
by the editor, *and* used by the running game.

The `LoadSettings` call sets
- `Container.UIScaling`,
- `Container.UIReferenceWidth`,
- `Container.UIReferenceHeight`,
- `Container.DefaultFont`.

The features that can be controlled this way:

- Container.UIScaling (and related UIReferenceWidth, UIReferenceHeight):

    The default value of `TUIContainer.UIScaling` is `usNone`,
    so no "coordinate scaling" is done.
    If you don't specify `<ui_scaling>` element in `CastleSettings.xml`,
    we keep using this default.

    However, we advice all new cross-platform projects to use UIScaling.
    This is the best practical way to achieve a consistent look
    on various screen sizes and devices.
    It is useful even for desktop-only applications (since people have
    various monitor resolutions), and it is crucial for mobile applications
    (where devices have wildly different resolutions).

    The _"New Project"_ templates provided by the CGE editor
    all set up by default UI scaling to the reference sizes of 1600x900
    (the most popular aspect ratio in 2018).

- Container.DefaultFont:

    This controls the default font look (font file, font size)
    for all user-interface controls.
    Note that each control can customize it
    (using `TCastleUserInterfaceFont.CustomFont`,
    `TCastleUserInterfaceFont.FontSize`).

- Custom component classes:

    Right now, the CGE editor doesn't allow installing custom components.
    Although you could recompile the CGE editor with additional units
    (that call `RegisterSerializableComponent`)
    (but then be wary of CGE editor license in COPYING.GPL2.txt:
    if you distribute a modified CGE editor only for internal purposes,
    then you don't have to do anything;
    but if you distribute a modified CGE editor publicly,
    you have to share the code of your components too).

    There is another way of informing the CGE editor about your custom components:

    You can use `<additional_component>` element, as presented below,
    to make CGE editor fake having an additional component class.

# Example CastleSettings.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<castle_settings>
  <!--
    UI scaling settings.
    The allowed mode values are "None", "EncloseReferenceSize", "FitReferenceSize".
    See the [TUIScaling API docs](https://castle-engine.io/apidoc/html/CastleUIControls.html#TUIScaling)
    for meaning.
  -->
  <ui_scaling
    mode="EncloseReferenceSize"
    reference_width="1920"
    reference_height="1080"
  />

  <!--
    Default font.

    Properties size, size_at_load, anti_aliased are optional.
    Their default values are shown below.

    The difference between size and size_at_load:

    - `size` determines the `TCastleFont.Size`,
      it determines how large the font is on the screen.

    - `size_at_load` is the font size used to create an internal texture
      with letters. By default it is equal to `size`,
      but it can be set to something larger to improve the quality of the font.
      This is useful if in your game you will often use this font
      with other sizes.
      (E.g. your controls leave `TCastleUserInterfaceFont.CustomFont = nil`,
      but often use large `TCastleUserInterfaceFont.FontSize` or
      `TCastleUserInterfaceFont.FontScale`).
  -->
  <default_font
    url="castle-data:/MyFontFile.ttf"
    size="20"
    size_at_load="20"
    anti_aliased="true"
  />

  <!--
    Use this if your game code calls this
    (usually in the "initialization" section of some unit):

      RegisterSerializableComponent(TUnholyButton, 'Unholy Button');

    This way the castle-editor will allow you to use class TUnholyButton
    (exposing TCastleButton properties of it).
    The example assumes that TUnholyButton is a custom descendant of
    TCastleButton.
  -->
  <additional_components>
    <additional_component
      class_name="TUnholyButton"
      ancestor_class="TCastleButton"
      caption="Unholy Button"
    />
  </additional_components>
</castle_settings>
```
