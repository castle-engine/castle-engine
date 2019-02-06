# Generics.Collections

FreePascal Generics.Collections library (TList, TDictionary, THashMap and more...)

This directory contains the source code of the Generics.Collections
and related units. It is used only when compiling with FPC 3.0.x,
since FPC >= 3.1.1 already contains this code as part of the standard library,
and Delphi has it's own implementation of Generics.Collections.

## Original version

The original version of these units is on
https://github.com/maciej-izak/generics.collections .
Thousand thanks go Maciej Izak for implementing it!

These units are also available inside the FPC SVN repository,
and are part of FPC >= 3.1.1 standard units (rtl-generics).
See:

- https://svn.freepascal.org/cgi-bin/viewvc.cgi/trunk/packages/rtl-generics/
- https://svn.freepascal.org/svn/fpc/trunk/packages/rtl-generics/
  (You can do "svn checkout ..." on this URL to easily download whole directory.)

(Note: We do not use "git submodules" to include one repository inside another,
because git submodules are not automatically cloned, and we need this to work
for all new users that get our code from GitHub.)

## How to use

Both the original version in
https://github.com/maciej-izak/generics.collections ,
and the version in FPC SVN,
contain various examples using Generics.Collections.

You can also take a look at the short overview of
Generics.Collections by Michalis Kamburelis on
https://castle-engine.io/modern_pascal_introduction.html#generic-containers-section .

You can also see Delphi Generics.Collections documentation:
http://docwiki.embarcadero.com/Libraries/Tokyo/en/System.Generics.Collections
These units were implemented independently, but are compatible with Delphi.
