{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Base classes to access Castle Game Engine editor functionality from custom components.

  Right now just TCastleComponentEditorDesigner,
  our descendant of TComponentEditorDesigner that you can access using GetDesigner
  in classes that descend from TComponentEditor.
  It exposes to CGE-specific functionality to custom components.

  See https://castle-engine.io/custom_components#_add_verbs_to_the_context_menu_for_given_component
  about component editors.
}
unit CastleEditorAccess;

{$I castleconf.inc}

interface

uses Classes,
  ComponentEditors;

type
  { Access instance of this from TComponentEditor descendants, using GetDesigner.
    This class allows to act on CGE editor from within a specific component editor. }
  TCastleComponentEditorDesigner = class(TComponentEditorDesigner)
  public
    { Component editor can use this to propose opening a new design.
      For example TCastleTransformDesign can propose to open design
      in TCastleTransformDesign.URL. }
    procedure ProposeOpenDesign(const DesignUrl: String); virtual; abstract;

    { Free component C (which should be part of this designed, owned by DesignOwner)
      and all children.

      We have to delete things recursively, otherwise they would keep existing,
      taking resources and reserving names in DesignOwner,
      even though they would not be visible when disconnected from parent
      hierarchy.

      This does nothing if you try to free some internal component
      (like csTransient) or the design root (which can never be freed). }
    procedure FreeComponentRecursively(const C: TComponent); virtual; abstract;
  end;

implementation

end.
