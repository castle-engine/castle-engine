About level.xml and resource.xml files:

- The data directory of the game is scanned for the special XML files named
  level.xml and resource.xml. This allows you to define new
  creatures or items (something that can be picked and carried by the player)
  or levels to the game simply by adding an additional subdirectory
  to the game data.

  What exactly is "data directory"? You give it as parameter to
  Levels.LoadFromFiles and Resources.LoadFromFiles calls,
  by default it's the result of ProgramDataPath function.

- Each level.xml / resource.xml file may contain relative filenames for
  3D models and images related to this resource.
  The idea is that the XML file is kept together with the data of particular
  creature, item etc.
  So you can trivially add/remove available resources
  by simply adding/removing the appropriate subdirectory to the game data.
  No recompilation, and no modification of any central file,
  are necessary to add e.g. a new creature and a new level using that creature.

- In normal circumstances, these xml files are scanned and read only once when
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
Specifically about resource.xml:

- Defines a creature or an item resource. The system is extensible,
  so it can actually define other 3D resources that are part of the world
  in your games.

  The root element is <resource>.

- name: the unique object name to indicate initial position of this creature in
  the level 3D file. IOW, this determines Blender object name
  to choose this creature type. It must be unique among all resources
  (creature and items resources). For all (current and future) uses it should
  be a valid VRML/X3D and ObjectPascal identifier, and also we reserve
  underscores and digits for some tricks.
  So stick to only (English) letters.

- type: determines the exact class (ObjectPascal implementation)
  used to instantiate this creature resource.
  It doesn't have to be unique. E.g. creature type "Missile"
  or generic item type "Item" are used by many resources.

  The type determines the behavior that is coded in ObjectPascal
  --- like creature artificial intelligence and whether item can be equipped.

- The type also determines other available attributes of this resource.
  For example, only creature type "WalkAttack" (or it's descendants,
  like "Alien") have the "attack_animation" attribute.

  For the documentation and default values of properties that you can
  set on a creature or item, see T3DResource descendants in the engine:
  TCreatureResource (and descendants) for creatures,
  TItemResource (and descendants) for items.
  They have lot's of properties, and almost all their properties
  can be set by appopriate XML attribute.

- radius: (default 0.0) Radius used for collision detection with this creature.
  If you don't set radius in resource.xml file (or set it to default value 0.0),
  we will calculate a sensible default radius based on the bounding box
  of the creature.

- middle_height: (default 0.5) Position of eyes of the creature,
  used for various collision detection routines.
  See T3DCustomTransform.MiddleHeight documentation.

  Game developers can use the Castle3D.RenderDebug3D variable to easily
  visualize the bounding sphere (and other things) around resources.
  The bounding sphere is centered around the point derived from "middle_height"
  setting and with given (or automatically calculated) "radius".

- flying: False or True to indicate if creature / item is affected by gravity.

  Missile creatures (resources with type="Missile",
  indicating TMissileCreatureResource implementation,
  or other type indicating your custom class descending from TMissileCreatureResource)
  are an exception here: they ignore this setting.
  Missiles have special approach to gravity (see direction_fall_speed)
  and are not affected by normal gravity settings.

- fall_speed: the speed (in units per second) of falling down because of gravity.
  Default is 10 (see CastleResources.DefaultFallSpeed constant).

  Note that the gravity direction is controlled by your level 3D model,
  see "Which way is up" section in the engine tutorial.

  Currently, falling down of creatures and items just uses this constant speed.
  In the future, we plan to add properties to control mass and air friction
  and perform more physically-correct simulation of falling down.

  This has no effect for resources (creatures or items) with flying="True".
  This also has no effect for missile creatures (their flying="Xxx" is ignored,
  as documented above).

- grow_speed: the speed (in units per second) of growing.
  Default is 5 (see CastleResources.DefaultGrowSpeed constant).

  The "growing" is used to allow non-flying creatures to climb stairs.
  The creature can move whenever a sphere (see "middle_height" and "radius"
  settings mentioned above) can move. This means that part of the bounding
  box (part of the T3DCustomTransform.PreferredHeight) may temporarily
  "sink" into the ground. The growing, controlled by this property,
  allows the creature to go up.

- direction_fall_speed: (default 0) The gravity of missiles.
  This works by gradually changing the missile direction to point downward
  (in the same direction where gravity pulls).

  (Only for missiles, that is: resources with type="Missile",
  indicating TMissileCreatureResource implementation,
  or other type indicating your custom class descending from TMissileCreatureResource.)

- <model> element describes 3D models and animations of the creature/items.
  More information about it on DRAFT.modeling_tutorial.txt.
------------------------------------------------------------------------------
Specifically about level.xml:

- Defines a level.
  The root element is <level>.

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
  handled with vanilla TLevelLogic implementation.
  Many advanced tricks are possible by implementing in the game code
  a descendant class of TLevelLogic that does something special,
  you can then register it by "LevelLogicClasses['My'] := TMyLogic;",
  and then type="My" is allowed in level.xml file.
  See castle1 GameLevelSpecific unit for examples.

- default_played: (optional, default "false")
  Should the level be initially considered "played".
  This sets TLevelInfo.DefaultPlayed property, which in turn
  (if nothing is stored in user preferences file about it) sets
  TLevelInfo.Played. How is this useful, depends on a particular game:
  some games may decide to show in the "New Game" menu levels with Played=true.
  Some games may ignore it.

- loading_image (optional, default empty): filename of image file to display
  while loading the level (under the progress bar).

- loading_image_bar_y_position (optional, default 0.5):
  indicates vertical position of progress bar when loading level,
  used only if loading_image is defined.
  Between 0 and 1, default value 0.5 means "middle of the screen".
  Should be synchronized with loading_bg image, to look right.

- placeholders: You can place placeholders in the level 3D model,
  to create various things:
  - creatures/items (commonly called "resources",
    as they refer to T3DResource) by placeholders named "CasRes...",
  - water volume by placeholder "CasWater",
  - move limit by placeholder "CasMoveLimit",
  - sectors/waypoints (to make creature AI smarter)
    by placeholders "CasSector..." and "CasWaypoint..."
  - see TGameSceneManager.LoadLevel docs for full list.
  - and possibly more, as every level type may allow additional placeholders,
    you can handle them in a descendant of TLevelLogic by overriding
    TLevelLogic.Placeholder.

  The "placeholders" attribute in level.xml determines how we derive
  "placeholder name" from a VRML/X3D shape.
  - "x3dshape" (default) means that the placeholder name comes from
    VRML 2.0/X3D Shape node name (set using "DEF" in VRML/X3D).
  - "blender" means that the placeholder name is detected following
    standard Blender VRML/X3D exporters behavior.
    This allows you to set the placeholder name easily in Blender,
    just set the Blender object name.
  - and possibly more, see CastleShape.PlaceholderNames.
    You can define and register your own functions there, to handle
    other 3D modelers, like 3DSMax or Maya or anything else
    (and you're welcome to contribute them to include them in engine code,
    of course!).

- placeholder_reference_direction (optional, default "1 0 0"):
  Some placeholders (currently, only creatures) may be used to determine
  initial direction of the resource. For example, the direction
  the creature is initially facing.
  This direction is calculated as the transformation
  of given placeholder applied to this 3D vector.

  The correct value may depend on the exporter you used to create 3D models,
  and on the exporter settings (how and if it rotates the model when exporting,
  and is this rotation recorded in placeholder transformation
  or applied directly to mesh coordinates). It may also depend on personal
  preference, as it determines how you set resources in your 3D modelling tool
  (like Blender).

  Fortunately, the default value (+X vector) is suitable for at least
  2 common situations:

  - If your exporter rotates the world to turn +Z up into +Y up.
    (This is the case of default Blender X3D exporter with default settings.)
  - If your exporter doesn't rotate the world.
    (You can configure Blender exporter to behave like this.
    You may also then configure engine to use +Z as up vector for everything,
    see "Which way is up?" notes in DRAFT.engine_tutorial.txt.)

  In Blender it's useful to enable the "Display -> Wire" option for placeholder
  objects, then Blender will show arrows inside the placeholder.
  +X of the arrow determines the default direction understood by our engine.

- See TLevelInfo properties documentation if in doubt.

- Every TLevelLogic class (you indicate it with "type", see above)
  may use additional attributes from level.xml file:

  TLevelLogic constructor gets DOMElement: TDOMElement parameter,
  which is an XML tree of level.xml file. You can read it
  however you want. We use standard FPC DOM unit and classes,
  and add a handful of simple comfortable routines in CastleXMLUtils unit,
  for example you can use

    if not DOMGetBooleanAttribute(DOMElement, 'my_attribute', MyAttribute) then
      MyAttribute := false; // default value, if not specified in level.xml

  to read a boolean attribute "my_attribute".
