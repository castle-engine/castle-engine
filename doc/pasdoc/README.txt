The script here generates the API documentation
for Castle Game Engine (https://castle-engine.io/)
using PasDoc (http://pasdoc.sourceforge.net/).

The online version of this documentation is already generated for you:

- On https://castle-engine.io/apidoc/html/ , for the last stable
  engine release.

- Or on http://michalis.ii.uni.wroc.pl/cge-www-preview/apidoc/html/ ,
  for the unstable (bleeding-edge) engine version from GitHub / SVN.
  Automatically regenerated after every commit.

- If you downloaded engine sources as tar.gz or zip archive, then you also
  already have an offline version of this documentation generated in doc/reference/ .

------------------------------------------------------------------------------
To generate the docs yourself:

Run "make html" here.

- You must first install "pasdoc".
  It's available in some Linux distributions packages,
  or you can get it from http://pasdoc.sourceforge.net/ .

  If you install PasDoc yourself (not from packages),
  be sure to put the "pasdoc" binary on $PATH, so that calling just "pasdoc"
  from the command-line works.

  - On Unix (Linux, macOS...) add this to your $HOME/.bashrc file:

    export PATH="$HOME/installed/pasdoc/bin:$PATH"

  - On Windows, google "how to modify PATH in windows 7" or something similar,
    to learn how to set the $PATH correctly.

- Running "make html" should work out-of-the-box on any Unix
  (Linux, macOS...) system.
  On Windows you will need to additionally install Cygwin (http://cygwin.com/)
  or MinGW (http://www.mingw.org/).

- Note that the html-parts/ subdirectory here contains some files
  (HTMLs, and some images/CSS/JS in subdirectories)
  copied (or generated from) our actual website on https://castle-engine.io/ .
  For your comfort, these files are already present inside html-parts/ here,
  committed. They will be automatically used as necessary.

  If you would like to regenerate them, do

    cd html-parts
    make clean
    make

  Be aware that you will need to have command-line PHP for this,
  and you will need our website sources from
  https://github.com/castle-engine/cge-www to be present (cloned such that
  cge-www and castle-engine repos are siblings).

------------------------------------------------------------------------------
How to write documentation for the engine:

The documentation is just comments in the unit interface.
Place a simple text in plain English that describes the intention
of your identifier (a unit, a function, a class, a property,
an enumeration value...).

The documentation style:

- Always use full, English sentences, that start with an upper letter
  and end with a dot. So instead of

    { returns true if camera view orientation is known }

  it's better to write:

    { Returns true if camera view orientation is known. }

- The first sentence (up to the first dot, see
  https://github.com/pasdoc/pasdoc/wiki/AutoAbstractOption )
  is automatically understood to be an "abstract" by PasDoc.
  So it's ultra-important. It should stand on it's own,
  and be a good summary of what the given identifier does.

  It should be concise, and not repeat anything obvious.
  So, continuing the previous example, it's even better to write:

    { Returns if camera view orientation is known. }

  or even more better

    { Is the camera view orientation known. }

  BTW, do not be tempted to end the above sentence with a question mark, "?".
  End it with ".", as it's not a question -- it's a description of the value.
  Also, Pasdoc's --auto-abstract expects the sentence to end with ".", not "?".

  In general, avoid words that do not actually enhance the meaning,
  especially in that first sentence.
  Try to keep the English simple and clean.

  - Instead of "Returns blah.", you can usually say just "Blah."
    ("Returns the game state." -> "Game state.")

  - Instead of "Controls blah.", "Determines blah.", ... you can also usually
    (but, admittedly, not always) say "Blah."

  - Do *not* start the documentation string with "This...",
    like "This procedure performs eating a fruit."
    or "This class represents a fruit."
    or "This function calculates a square root.".
    Instead just write "Eat a fruit.", "A fruit.", "Square root.".

    The reader already sees the identifier declaration, so you don't need to
    explicitly tell that "this is a class", or a "procedure", or ...

  These guidelines are somewhat inspired by the javadoc standards,
  http://www.oracle.com/technetwork/articles/java/index-137868.html .

- PasDoc supports many @-tags to format your documentation nicer.

  E.g.
  - @italic(this is italic),
  - @bold(this is bold),
  - @unorderedList( @item(One) @item(Two) ),
  - see https://github.com/pasdoc/pasdoc/wiki/SupportedTags for the list.

- We use the --auto-link feature of PasDoc. This means that identifiers
  are automatically linked, without the need to surround them in @link()
  tag... unless they are a common English word and we added them as an exception
  to auto_link_exclude.txt file here. So:

  - If you have an identifier that's a common English word, add it to
    auto_link_exclude.txt and use @link to link to it.

  - Otherwise, just write it's name anywhere in the documentation string,
    and it will be automatically picked up.
    Sometimes, it's reasonable to use an explicit @link anyway,
    to make sure that the link is correct (PasDoc will warn if @link contents
    are incorrect, e.g. you make a typo).

- An exception to the rule "put comments in the unit interface"
  are the X3D fields in "src/x3d/auto_generated_node_helpers/xxx.inc".
  They should be documented inside the "src/x3d/x3dnodes_documentation.txt" file,
  not by editing the Pascal source code.

  This makes it possible to regenerate the include files in
  "auto_generated_node_helpers" when necessary.
