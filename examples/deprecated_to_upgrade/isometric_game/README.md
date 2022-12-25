# Overview

Demo of an isometric game, using static images, using Castle Game Engine.

Everything is just drawn as 2D, using our TDrawableImage.

This could be made much more impressive by:

- Using TCastleViewport for a game world,
  and using inside TCastleScene with (possibly animated) models.
- And by providing more impressive game assets :)
- And by designing some interesting map (the current map is just a test).

This effort is already started -- see `examples/isometric_game` for a new version.
We only keep this deprecated version to show alternative rendering method.

# Keybindings

* 12346789:
  Move your character. These are comfortable to use on numpad when NumLock is ON.

* up/down/right/left:
  Move your character, if "view follows the player" mode is ON.
  Otherwise, only move the view.

* f:
  Toggle "view follows the player" mode. Initially it's ON.

* Escape:
  Exit.

* e:
  Edit base tile.

* E:
  Edit bonus tile.

* s:
  Save current map to the file. Together with "e" and "E" keys,
  this allows you to design new levels completely inside the game.

* i:
  Field info.

# Map file format

Design considerations:
- This was reused as a programming exercise (PGK exercise on ii.uni.wroc.pl,
  where Michalis was a lecturer), so I wanted some simple text format.
  Simple to read by code.
  And simple to design even when you don't have a visual editor.
- Tile filenames and counts must be specified inside the file,
  to make it flexible.

The map file looks like this:

```
Width Height
PlayerStartX PlayerStartY
BaseTilesCount BonusTilesCount
# Now BaseTilesCount lines follow, describing the tiles used on this map.
# The idea is that each tile corresponds to a different image filename.
# Each tile also has a one-char name, that will be used to indicate this
# tile later on the map. This one-char is any non-whitespace character
# besides the "_".
BaseChar1 BaseTileFileName1
...
BonusChar1 BonusTileFileName1
....
# Now Height map lines follow. Each line has exactly 2*Width
# characters. First char of each pair indicates the base tile,
# second char indicates the bonus tile (or _ if no bonus tile).
# Lines are specified from highest to lowest (so the resulting game
# screen looks roughly like your text file).
...
```

# TODO

Well, obviously, this is not a real game :)

There's nothing besides the player and the *absolutely static* level.
A whole lot of things should be done to turn this into a real game:

- Collisions. Right now you can move freely everywhere,
  pass through walls etc.
- Player's sprite should animate (making steps) when moving.
  Maybe using TSprite, maybe using TCastleScene inside TCastleViewport.
- Creatures (probably sharing most of the current TPlayer class code,
  to make them move smoothly, be shown from various directions,
  show animations of moving). Fighting and/or talking and/or trading
  with the player, at least. Talking choices and/or free talk a'la Wizardry
  and/or giving quests.
- Items (pick, drop, show inv, use,
  equip (change player's sprite), show equip, unequip).
- Changing levels (maps) --- at least things disappearing/appearing on the map
  when player does something (open door).

Michalis Kamburelis
https://castle-engine.io/
