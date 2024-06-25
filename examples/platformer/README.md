# Platformer

## Introduction

Downloads:
- [Linux, Windows, Android APK (from itch.io)](https://castle-engine.itch.io/platformer).
- [Android (from Google Play)](https://play.google.com/store/apps/details?id=io.castleengine.platformer).

Watch the movie with a playthrough on https://www.youtube.com/watch?v=yVTxr9tTHxg .

## Features

A complete demo of a platformer game, with

- Level (and all UI) designed visually using the editor.

- Sprites sheets designed using CGE editor and managed in .castle-sprite-sheet format (see [sprite sheets docs](https://castle-engine.io/sprite_sheets)).

- Full platformer gameplay. Player can move, jump, pick up a weapon, be hurt by enemies, be hurt by obstacles, collect things, die, finish the level. Extra jumps in the air are possible (check out _Advanced player_ checkbox). Enemies move following a simple pattern.

- Sound and music.

- All UI states you expect from a usual game — main menu, options (with volume configuration), pause, credits, game over and of course the actual game.

Using [Castle Game Engine](https://castle-engine.io/).

## Keys, mouse, touch input

You can use keys, mouse or touch (on mobile) input to fully control the game. On mobile, multi-touch works, so you can e.g. move left and jump at the same time.

- Move left: "A" key, or "left arrow" key, or press mouse/finger in the left-bottom screen part.

- Move right: "D" key, or "right arrow" key, or press mouse/finger in the right-bottom screen part.

- Jump: "W" key, or "up arrow" key, or press mouse/finger in the upper screen part.

- Shoot: "Space" key, or press right mouse button, or press at least 2 fingers on the touch device.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `platformer_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `platformer_standalone.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.
