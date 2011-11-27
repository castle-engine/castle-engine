Demo of an isometric game view, using Castle Game Engine.

Quite simple, as everything is just drawn as 2D.
We use our CastleWindow and GLImages to draw 2D graphics on the window.

----------------------------------------
Keybindings:
12346789:
  Move your character. These are comfortable to use
  on numpad when NumLock is ON.
up/down/right/left:
  Move your character, if "view follows the player" mode is ON.
  Otherwise, only move the view.
f:
  Toggle "view follows the player" mode. Initially it's ON.
Escape:
  Exit.
e:
  Edit base tile.
E:
  Edit bonus tile.
s:
  Save current map to the file. Together with "e" and "E" keys,
  this allows you to design new levels completely inside the game.
i:
  Field info.

----------------------------------------
Map file format:
(design considerations:
- This is supposed to be reused for PGK exercise on II, so it must
  be some simple format
- It's a text format, to be edited in any text editor.
  On PGK they will not have time to do any real game editor.
- Tile filenames and counts must be specified inside the file,
  to make it flexible.
)

----
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
----

----------------------------------------
TODO:

- There's no collision for now. You can move freely everywhere,
  pass through walls etc.

Obviously, this is not a real game. I'm sure you already noticed this.
There's nothing besides the player and the *absolutely static* level.
A whole lot of things should be done to turn this into a real game:

- Player's sprite should animate (making steps) when moving.
- Creatures (probably sharing most of the current TPlayer class code,
  to make them move smoothly, be shown from various directions,
  show animations of moving). Fighting and/or talking and/or trading
  with the player, at least. Talking choices and/or free talk a'la Wizardry
  and/or giving quests.
- Items (pick, drop, show inv, use,
  equip (change player's sprite), show equip, unequip).
- Changing levels --- at least things disappearing/appearing on the map
  when player does something (open door).
- Heck. A multiplayer ? An endless list of features follows here.
- And still a whole lot of other things.

----------------------------------------
Michalis Kamburelis
http://castle-engine.sourceforge.net/
