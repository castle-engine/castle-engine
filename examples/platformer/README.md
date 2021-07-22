# Platformer

A complete demo of a platformer game, with

- Level (and all UI) designed visually using the editor.

- Sprites sheets designed using CGE editor and managed in .castle-sprite-sheet format (see [sprite sheets docs](https://github.com/castle-engine/castle-engine/wiki/Sprite-sheets)).

- Full platformer gameplay. Player can move, jump, pick up a weapon, be hurt by enemies, be hurt by obstacles, collect things, die, finish the level. Extra jumps in the air are possible (check out _Advanced player_ checkbox). Enemies move following a simple pattern.

- Sound and music.

- All UI states you expect from a usual game — main menu, options (with volume configuration), pause, credits, game over and of course the actual game.

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `platformer_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
