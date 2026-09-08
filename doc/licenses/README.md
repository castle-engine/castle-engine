# License of Castle Game Engine

_"Castle Game Engine"_ is a free and open source game engine.

You can use it to develop commercial (including proprietary) games and applications.

See the file [../../COPYING.md](../../COPYING.md) for a short version of the license terms.

For the best, exhaustive but also friendly-to-read explanation of our licenses, see https://castle-engine.io/license .

This file, `doc/licenses/README.md`, reflects the same information as above https://castle-engine.io/license page. This way, this information is available offline and is part of the repository.

## Engine core: permissive LGPL with static linking exception

The core of the _Castle Game Engine_ is available on terms of the _GNU Lesser General Public License_ (version 2 or above, whichever you like). See the [COPYING.LGPL2.txt](COPYING.LGPL2.txt) for the exact LGPL 2 license text.

Moreover, we add the so-called _"static linking exception"_ to the LGPL terms. The idea of this exception is to allow statically linking with the engine on the same terms as dynamically linking. Static linking is what usually happens when you compile a program using the engine units (without wrapping the engine in a dynamic library (dll, so, dylib) or Delphi runtime package).

The exact legal text of this "static linking exception" is this:

> As a special exception, the copyright holders of this library give you permission to link this library with independent modules to produce an executable, regardless of the license terms of these independent modules, and to copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions of the license of that module. An independent module is a module which is not derived from or based on this library. If you modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception statement from your version.

By "engine core" we mean everything that can be found in [this repository](https://github.com/castle-engine/castle-engine/) *except* the exceptions mentioned in the section below (`tools/castle-editor`, `tools/castle-editor-portable`, `examples`).

## Engine examples: very permissive (do what you want) BSD

This section applies to all files within the `examples/` subdirectory:

The example code, and most example data (unless specified otherwise using the `AUTHORS.txt` file in example `data` subdirectory) are covered by a permissive "modified BSD (3-clause)" license. See the file [COPYING.BSD-3-clause.txt](COPYING.BSD-3-clause.txt) for the exact license text.

In short, you only need to retain our copyright (just keep somewhere line like _"This code is based on Castle Game Engine example"_) in sources or docs. You don't need to share anything you do (you don't need to share the sources/docs). Whatever modifications you do to the example code are yours.

## Visual editor (castle-editor, castle-editor-portable), some example data, some website data: GPL

The items listed below are covered by a more strict GNU General Public License (version 2 or above, whichever you like). See the file [COPYING.GPL2.txt](COPYING.GPL2.txt) for the exact license text. See http://www.gnu.org/copyleft/gpl.html for more information about this license (including translations of it to various languages) and philosophy of the free software.

In short, it means that: If you use the items listed below (modified or not) to create your own application, then you need to share the code of your entire application, as the complete work must be covered also by the GPL license.

The items covered by the GPL license are:

- Editor in `tools/castle-editor/` and `tools/castle-editor-portable/` directories.

- The data files of some examples (the examples are in `examples/` subdirectory, and their "data files" are in `data/` subdirectories beneath). They are clearly marked by the existence of `COPYING.GPL2.txt` and `AUTHORS.txt` files in their respective directories.

- Some files included as part of our PasDoc-generated documentation (in `doc/pasdoc/html-parts/` and `doc/reference/`) come from

  - https://github.com/castle-engine/cge-www (GNU GPL)
  - https://github.com/pasdoc/pasdoc/wiki (GNU GPL)

    The relevant files are clearly marked as having "GNU General Public License" in the header.

    Note that the documentation also includes some projects (Bootstrap, jQuery, Tipue) on more permissive license. Consult the appropriate files for details.

- Note that we also develop some tools / games related to the Castle Game Engine, available in other repositories in [GitHub CGE organization](https://github.com/castle-engine). For example: [Castle Model Viewer](https://github.com/castle-engine/castle-model-viewer). These tools / games are covered by their own license terms, and in general they use more strict GPL license.
