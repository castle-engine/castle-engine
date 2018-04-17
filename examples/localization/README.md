Demo how to use localization (translate your game into multiple languages)
with Castle Game Engine.

We use FPC GetText unit for this:
https://www.freepascal.org/docs-html/fcl/gettext/index.html
This translates all strings in `resourcestring` declarations.

# Creating translations

1. Place everything you may need to translate as a `resourcestring` in Pascal. You can use English text in code, or you can use internal translation identifiers (never to be seen by normal users) -- both approaches are possible.

2. Create initial `game.pot` file.

    _Note that this step is optional_. If you want, you can work without any `game.pot` file, and just create translations by manually creating files like `game.pl.po` for each language. The syntax of PO files is trivial, see `po_files` subdirectory here.

    The `game.pot` can serve a basis for translations. You can create it using strings from the source code:

    * Compile the game (`castle-engine compile`).

    * Use `rstconv` (distributed with FPC) like this: `rstconv -i castle-engine-output/compilation/x86_64-linux/game.rsj -o po_files/game.pot`

    * Note that FPC will create one `xxx.rsj` file for each unit. But this should not limit you. It's normal to put all the strings from the *complete* application into a single `xxx.pot` file. In general, the format of the `.pot` and .po` files (they are the same) is trivial, they are simple text files that can be concatenated together etc.

3. To translate to a new language:

    * For each new language, create a new file like `po_files/game.pl.po` (`pl` for a Polish translation, `de` for German, `en` for English etc.).

        * You can start by just copying `game.pl.po` from `game.pot`.

        * Or you can start by `msginit --locale=pl --input=game.pot --no-translator --output-file=game.pl.po`. This creates `game.pl.po`, with the initial translated strings having contents from `game.pot`. This makes sense if `game.pot` contains English text, and it's a good starting point for a new translation.

    * Edit the `game.pl.po` using a normal text editor. Or use a specialized editor like https://poedit.net/

    * Generate .mo file: `msgfmt po_files/game.pl.po --output-file=data/locale/game.pl.mo`. We have a trivial script here `update_translations.sh` doing that. You need to rerun it after every modification to `po_files`.

In code:

* Call in Pascal `TranslateResourceStrings(URIToFilenameSafe(ApplicationData('locale/game.pl.mo')));` to use the Polish translation. This simply updates all `resourcestring` contents to the Polish versions.

* Assign the resourcestrings to the approproate properties of appropriate objects, like `TCastleLabel.Caption`.

It's probably easiest to just call `TranslateResourceStrings` once, at the very beginning of your application (at the beginning of `Application.OnInitialize` handler). And then construct UI as usual (just use `resourcestring`s). If the user wants to change the language, it's easiest to just say _"Please restart the application in order for the language change to take effect."_.

That said, if you put some more work, you can allow to dynamically switch the language during the game. Just reassign all captions from the corresponding `resourcestring`s. The demo here shows how to do it: see the `TApplicationLogic.SwitchLanguage` method. It's trivial to write, although it may be a pain to maintain in a larger project.

# Fonts

Note that we also adjust font in this application.
The default font contains only basic ASCII (English) characters,
so we load a font with additional German, Polish, Russian and Ukrainian characters.
See https://castle-engine.io/manual_text.php about loading fonts
in Castle Game Engine.

# Lazarus references

While we don't use Lazarus LCL code in CGE, but this mechanism is consistent with how Lazarus application can be localized. So a lot of Lazarus documentation apply also to us:

* http://wiki.lazarus.freepascal.org/Step-by-step_instructions_for_creating_multi-language_applications
* http://wiki.lazarus.freepascal.org/Translations_/_i18n_/_localizations_for_programs
* http://wiki.lazarus.freepascal.org/Everything_else_about_translations

See also resources about gettext:

* https://en.wikipedia.org/wiki/Gettext

All the tools and editors for gettext files should work fine.
There are even online services that take a ready PO file for translating.
