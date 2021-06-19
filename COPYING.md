"Castle Game Engine" is a free software and open source game engine.

**You can use it to develop commercial (including closed-source) games and applications.**

# Summary

- "Castle Game Engine" is covered by the "LGPL with static linking exception" license.

- It is the same license as used by [FPC (Free Pascal Compiler) run-time library](http://www.freepascal.org/faq.var#general-license) and [LCL (Lazarus Component Library)](http://www.lazarus-ide.org/).

- You can use "Castle Game Engine" to develop any applications, including closed-source.

- If you modify the "Castle Game Engine" units, you need to share your modifications. Other than that, you don't need to share your code, regardless if it is using CGE in any way. Your game code is yours.

- The CGE code is covered by our copyrights. E.g. if you distribute the "Castle Game Engine" sources yourself (modified or not), you must clearly specify our copyrights to the original code.

- All example code, and almost all example data, are covered by a much more permissive "modified BSD 3-clause" license. So you can use our examples (stuff in `examples/` subdirectory) as a basis for your applications easily, without the need to share anything you did based on these examples.

    There are some exceptions though (some examples use data on other licenses), they are documented by `AUTHORS.txt` files in examples `data` directories.

- Some tools (in particular, the `castle-editor`), are covered by a more strict GPL license. To put it simply, you *cannot* make a closed-source fork of our "Castle Game Engine" visual editor.

The above is a quick summary of our licensing terms.

Please note that the text above is not "legally binding". E.g. _"the need to share your modifications"_ mentioned above is a simplification of the license terms. The precise license requirements discuss (non-internal) distribution of the application and other details. The exact legal text of the licenses is linked below, and they are analyzed in depth in many places on the Internet.

# Engine core (LGPL + static linking exception)

The core of the "Castle Game Engine" is available on terms of the _GNU Lesser General Public License_ (version 2 or above, whichever you like). See the [doc/licenses/COPYING.LGPL2.txt](https://github.com/castle-engine/castle-engine/blob/master/doc/licenses/COPYING.LGPL2.txt) for the exact LGPL 2 license text, see the http://www.gnu.org/copyleft/lesser.html for the current version, FAQs etc.

Moreover, we add the so-called _"static linking exception"_ to the LGPL terms. The idea of this exception is to allow statically linking with the engine on the same terms as dynamically linking. Static linking is what usually happens when you compile a program using the engine units (without wrapping the engine in a dynamic library (dll, so, dylib) or Delphi runtime package).

The exact legal text of this "static linking exception" is this:

    As a special exception, the copyright holders of this library
    give you permission to link this library with independent
    modules to produce an executable, regardless of the license
    terms of these independent modules, and to copy and distribute
    the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module,
    the terms and conditions of the license of that module.
    An independent module is a module which is not derived from
    or based on this library. If you modify this library,
    you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so,
    delete this exception statement from your version.

By "engine core" we mean everything that can be found in this directory (GitHub repository https://github.com/castle-engine/castle-engine/ ) *except* the exceptions mentioned in the section below (castle-editor, examples).

# Visual editor (castle-editor), some example data, some website data (GPL)

The items listed below are covered by a more strict GNU General Public License (version 2 or above, whichever you like). See the file [doc/licenses/COPYING.GPL2.txt](https://github.com/castle-engine/castle-engine/blob/master/doc/licenses/COPYING.GPL2.txt) for the exact license text. See http://www.gnu.org/copyleft/gpl.html for more information about this license (including translations of it to various languages) and philosophy of the free software.

In short, it means that: If you use the items listed below (modified or not) to create your own application, then you need to share the code of your entire application, as the complete work must be covered also by the GPL license.

The items covered by the GPL license are:

- castle-editor in `tools/castle-editor/` directory.

- The data files of some examples (the examples are in `examples/` subdirectory, and their "data files" are in `data/` subdirectories beneath). They are clearly marked by the existence of `COPYING.GPL2.txt` and `AUTHORS.txt` files in their respective directories.

- Some files included as part of our PasDoc-generated documentation (in `doc/pasdoc/html-parts/` and `doc/reference/`) come from

    - https://github.com/castle-engine/cge-www (GNU GPL)
    - https://github.com/pasdoc/pasdoc/wiki (GNU GPL)

    The relevant files are clearly marked as having "GNU General Public License" in the header.

    Note that the documentation also includes some projects (Bootstrap, jQuery, Tipue) on more permissive license. Consult the appropriate files for details.

- Note that we also develop some tools / games related to the Castle Game Engine, available in other repositories in [GitHub CGE organization](https://github.com/castle-engine). For example:

    - view3dscene: https://github.com/castle-engine/view3dscene
    - "Darkest Before the Dawn" game: https://github.com/castle-engine/darkest-before-dawn , https://castle-engine.io/darkest_before_dawn.php
    - "Wyrd Forest" game: https://github.com/castle-engine/wyrd-forest , https://www.patreon.com/posts/wyrd-forest-demo-15811244

    These tools / games are covered by their own license terms, and in general they use more strict GPL license.

# Examples (BSD)

This section applies to all files within the `examples/` subdirectory:

The example code, and most example data (unless specified otherwise using the `AUTHORS.txt` file in example `data` subdirectory) are covered by a permissive "modified BSD (3-clause)" license. See the file [doc/licenses/COPYING.BSD-3-clause.txt](https://github.com/castle-engine/castle-engine/blob/master/doc/licenses/COPYING.BSD-3-clause.txt) for the exact license text.

In short, you only need to retain our copyright (just keep somewhere line like _"This code is based on Castle Game Engine example"_) in sources or docs. You don't need to share anything you do (you don't need to share the sources/docs). Whatever modifications you do to the example code are yours.

# CASTLE_ENGINE_LGPL symbol (relevant only for Castle Game Engine <= 6.4)

In the older Castle Game Engine versions (6.4 and earlier), if you wanted to use the more permissive LGPL license, you had to compile the engine with the symbol `CASTLE_ENGINE_LGPL`. This avoided pulling GPL-only NURBS implementation.

This requirement is removed in Castle Game Engine >= 6.5 (more precisely, in the GIT commit https://github.com/castle-engine/castle-engine/commit/82f3c7562db21d498d5000628fedeeefaf5f7550 ). All the engine units, including the (new) NURBS implementation, are now automatically covered by the more permissive LGPL license.
