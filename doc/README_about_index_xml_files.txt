About index.xml files:

- Various index.xml files within subdirectories describe particular
  resources, like a creature, an item (item is something that can be picked
  and carried by the player) or a level.

  Each index.xml file may contain relative filenames for
  3D models and images related to this resource.
  The idea is that the XML file is kept together with the data of particular
  creature, item etc.
  So you can trivially add/remove available resources
  by simply adding/removing the appropriate subdirectory to the game data.
  No recompilation, and no modification of any central file,
  are necessary to add e.g. a new creature and a new level using that creature.

- Normally, the index.xml files are scanned and read only once when
  the game starts. For easy editing of game data (to not be forced
  to restart the game after every little tweak in configuration),
  you can also use debug menu (under ` key by default)
  to reload XML configuration of various things during the game.
  (Most, but not absolutely all, settings can be changed even while
  the game is running; exceptions are things that are "heavy" ---
  e.g. changing animation filename may require restarting the level.)

- Some attributes specify a sound name.
  For available sound names see names in sounds/index.xml.
  Empty (or not specified) sound name always means "no sound for this event".

- Paths in the XML files should be treated like URLs.
  That is, use slashes as directory separators, and watch out for case.
  The engine will make sure they are handled OK on all platforms.

------------------------------------------------------------------------------
Documentation for creature or item kind description,
in creatures/*/index.xml and items/*/index.xml:

- name: the unique object name to indicate initial position of this creature in
  the level 3D file. IOW, this determines Blender object name
  to choose this creature type. It must be unique among all resources
  (creature and items kinds). For all (current and future) uses it should
  be a valid VRML/X3D and ObjectPascal identifier, and also we reserve
  underscores and digits for some tricks.
  So stick to only (English) letters.

- type: determines the exact class (ObjectPascal implementation)
  used to instantiate this creature kind.
  It doesn't have to be unique. E.g. creature type "Missile"
  or generic item type "Item" are used by many resources.

  The type determines the behavior that is coded in ObjectPascal
  --- like creature artificial intelligence and whether item can be equipped.

- The type also determines other available attributes of this kind.
  For example, only creature type "WalkAttack" (or it's descendants,
  like "Alien") have the "attack_animation" attribute.

  For the documentation and default values of properties that you can
  set on a creature or item, see T3DResource descendants in the engine:
  TCreatureKind (and descendants) for creatures,
  TItemKind (and descendants) for items.
  They have lot's of properties, and almost all their properties
  can be set by appopriate XML attribute.

------------------------------------------------------------------------------
Documentation for level description, in levels/*/index.xml:

- name: the unique level name, used in scripts and such.
  It must be unique among all levels.

  For all (current and future) uses it should be a valid VRML/X3D
  and ObjectPascal identifier, so stick to only (English) letters,
  underscores and digits (and don't start with digit).

- title: nice, human-readable (with spaces, non-English letters etc.)
  level title that is displayed for users.

- scene: URL to the 3D file containing the level scene.

- type: (optional, default just generic "Level")
  Use specific ObjectPascal class to implement this level behavior.
  Default value is "Level", which means that the level will be
  handled with vanilla TLevel implementation.
  Many advanced tricks are possible by implementing in the game code
  (GameLevelSpecific unit) a descendant class of TLevel that does
  something special, then you should set this attribute to the id
  of that class (see bottom of GameLevelSpecific for possible names).

- default_available_for_new_game: (optional, default "false")
  Should the level be initially available (visible in "New Game" menu)
  for new players.

- loading_image (optional, default empty): filename of image file to display
  while loading the level (under the progress bar).

- loading_image_bar_y_position (optional, default 0.5):
  indicates vertical position of progress bar when loading level,
  used only if loading_image is defined.
  Between 0 and 1, default value 0.5 means "middle of the screen".
  Should be synchronized with loading_bg image, to look right.

- See TLevelAvailable properties documentation if in doubt.
