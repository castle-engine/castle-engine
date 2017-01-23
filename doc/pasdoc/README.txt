The script here generates the API documentation for Castle Game Engine
using PasDoc (http://pasdoc.sourceforge.net/).

The online version of this documentation is already generated for you on
http://castle-engine.sourceforge.net/apidoc/html/ (for last stable
engine release) or
http://michalis.ii.uni.wroc.pl/cge-www-preview/apidoc/html/
(for SVN engine sources; regenerated daily).
If you downloaded engine sources as tar.gz/zip archive, you should also
already have offline version of this documentation here.

------------------------------------------------------------------------------
If you want to generate the docs yourself:

Just run "make html" here.
It should work out-of-the-box on any Unix (Linux, Mac OS X etc.)
system (if you have pasdoc installed and available on $PATH of course).
On Windows you will need to install Cygwin or MinGW
(that give you some Unix tools ported to Windows) in order to use it,
or rewrite our mk_docs.sh script to a DOS .bat file (be sure to submit
it to us then, so that we can include it in SVN for others!)

------------------------------------------------------------------------------
Some notes about writing docs for our engine:

The documentation is just a comment in the unit interface describing,
in plain English, your identifier. PasDoc can catch the documentation
for functions, classes, enumeration values, and pretty much every other
Pascal construct. So the brief summary is: just document everything
in unit's interface and that's it.

More notes:

- PasDoc supports many @-tags to format your documentation nicer.
  E.g. @italic(this is italic), @bold(this is bold),
  make lists like this: @unorderedList( @item(One) @item(Two) ).

- We use the --auto-abstract feature of PasDoc. This means that the first
  sentence of the documentation (to the first dot followed by a whitespace)
  should stand on it's own, and be a good brief summary of what given
  identifier does.

- We use the --auto-link feature of PasDoc. This means that identifiers
  are automatically linked, without the need to surround them in @link()
  tag... unless they are a common English word and we added them as an exception
  to auto_link_exclude.txt file here. So:
  - if you have an identifier that's a common English word, add it to
    auto_link_exclude.txt and use @link to link to it.
  - otherwise, just write it's name anywhere in the documentation string,
    and it will be automatically picked up.

- Try to keep English simple and clean, especially that first sentence.
  Do *not* start every documentation string with convoluted "This...",
  like "This procedure performs eating a fruit."
  or "This class represents a fruit."
  or "This function calculates a square root.".
  Instead just write "Eat a fruit.", "A fruit.", "Square root.".
  Generated documentation already shows reader the name of
  the identifier and it's declaration, so don't repeat it without any purpose
  in the comment.

See PasDoc's documentation on (http://pasdoc.sourceforge.net/)
for details. Every @-tag, and every command-line option we use,
is documented there nicely. (With the exception of @groupBegin / @groupEnd
tags, that are *not* implemented in PasDoc yet. We use them in the hope
that one day they will be implemented, which will probably end as
a task for Michalis :)
