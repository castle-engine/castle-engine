{
  Copyright 2003-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Key and mouse shortcuts (TInputShortcut) and global management of them.

  See https://castle-engine.io/manual_key_mouse.php for overview.

  TInputShortcut instance represents a key/mouse shortcut.
  Instances of this class are spread throughout the engine.
  We have two different ways of using TInputShortcut instance:

  @unorderedList(
    @item(
      @bold(Global)

      TInputShortcut instance with TInputShortcut.Group <> igLocal
      is called "global". Such instance is automatically (at construction)
      added to InputsAll and InputsGroup(Group) lists.

      The purpose of such global input map is to be able to detect key conflicts,
      be able to restore whole input map to default,
      load/save them to the user config file, and so on.
      For example @link(CastleViewport.Input_Interact) is global.

      Global shortcuts are owned (they will be freed by) this unit
      (more specifically, they will be freed by InputsAll).
      When creating them, pass @nil to the Owner parameter
      of constructor TInputShortcut.Create.
      This implies that InputsAll and InputsGroup(Group) lists will never shrink,
      which is useful --- once added, shortcuts will not disappear.
      Global TInputShortcut instances are always in practice also global variables.
    )

    @item(
      @bold(Local)

      TInputShortcut instance with TInputShortcut.Group = igLocal
      is called "local". It means it's a normal component,
      it's not automatically present on any global list,
      so it doesn't conflict with other global shortcuts.

      For example TCastleWalkNavigation contains a number of them like
      TCastleWalkNavigation.Input_Forward.
    )
  )

  You create new inputs by simply constructing new TInputShortcut instances.
  Make sure you add all global inputs before calling @code(Config.Load),
  as some functionality assumes that all shortcuts are already added
  at the time @code(Config.Load) is called.
  The engine units themselves never call @code(Config.Load), it is left
  to the final application. }
unit CastleInputs;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleKeysMouse, CastleUtils, CastleClassUtils,
  CastleXMLConfig, CastleUIControls;

implementation

uses SysUtils, Generics.Defaults,
  CastleStringUtils;

end.
