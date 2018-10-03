# Editor Settings Specific to a Project

If your game customizes some global (or per-window) CGE settings,
then you want the CGE editor to know about these customizations.
This way CGE editor can show at design-time the same thing
that you will see when you load the designed file
(`xxx.castle-user-interface` or `xxx.castle-transform`) in game.

To do this, you place a file called `CastleEditorSettings.xml`
in the root directory of your project (right next to the `CastleEngineManifest.xml`).
In the future, this file should be configurable by GUI,
for now you just have to edit it directly.

The features that can be achieved this way:

- UIScaling:

    The editor by default uses `UIScaling` to 1600x900 reference sizes.

    The game code by default doesn't use `UIScaling` at all
    (that is, the default value of `TUIContainer.UIScaling` is `usNone`).
    This discrepancy is for historic reasons.
    We advice all new projects to use UIScaling.

    You should use the `<ui_scaling>` element, as presented below,
    to synchronize the `UIScaling` you set in your application
    (typically in `Application.OnInitialize` callback)
    with what CGE editor is showing.

- UIFont:

    If you customize the UIFont in your game code,
    you want to use the `<ui_font>` element, as presented below,
    to make CGE editor use this font too.

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

# Example CastleEditorSettings.xml

```xml
<?xml version="1.0" encoding="utf-8"?>
<editor_settings>
  <!--
    Use this if in your Application.OnInitialize you do this:

    Window.Container.UIScaling := usEncloseReferenceSize;
    Window.Container.UIReferenceWidth := 1920;
    Window.Container.UIReferenceHeight := 1080;

    This way the castle-editor will use the same UI scaling settings as your game.
  -->
  <ui_scaling
    type="usFitReferenceSize"
    reference_width="1920"
    reference_height="1080"
  />

  <!--
    Use this if in your Application.OnInitialize you do this:

    UIFont := TTextureFont.Create(Application);
    (UIFont as TTextureFont).Load(ApplicationData('MyFontFile.ttf'), 20, true);

    This way the castle-editor will use the same UIFont (default font) as your game.
  -->
  <ui_font
    url="MyFontFile.ttf"
    size="20"
    anti_aliased="true"
  />

  <!--
    Use this if in your Application.OnInitialize you do this:

    RegisterSerializableComponent(TUnholyButton, 'Unholy Button');
    and TUnholyButton descends from TCastleButton.

    This way the castle-editor will allow you to use class TUnholyButton
    (exposing TCastleButton properties of it).
  -->
  <additional_components>
    <additional_component
      class_name="TUnholyButton"
      ancestor_class="TCastleButton"
      caption="Unholy Button"
    />
  </additional_components>
</editor_settings>
```
