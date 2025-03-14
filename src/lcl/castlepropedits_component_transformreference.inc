{%MainUnit castlepropedits.pas}
{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { Editor for TCastleTransformReference. }
  TCastleTransformReferenceComponentEditor = class(TCastleTransformComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TCastleTransformReferenceComponentEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 2;
end;

function TCastleTransformReferenceComponentEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  case Index - InheritedCount of
    0: Result := 'Edit (Make Independent Copy) Referenced Transform';
    // 'Replace Reference With Independent Copy' // another way to make this, with replacement
    1: Result := 'Revert To Referenced Transform';
    else Result := inherited GetVerb(Index);
  end;
end;

procedure TCastleTransformReferenceComponentEditor.ExecuteVerb(Index: Integer);

  { Make a deep copy of C.Reference, place it as C child, then clear C.Reference.
    A bit similar to Edit in TCastleTransformDesignComponentEditor.ExecuteVerb,
    but we make a deep copy of Reference here. }
  procedure Edit(const E: TCastleComponentEditorDesigner; const C: TCastleTransformReference);
  var
    NewChild: TCastleTransform;
  begin
    if C.Reference <> nil then
    begin
      NewChild := ComponentClone(C.Reference, C.Owner) as TCastleTransform;
      C.Add(NewChild);

      InternalCastleDesignInvalidate := true;

      C.RevertReference := C.Reference;
      C.Reference := nil;
      E.Modified;
    end;
  end;

  { Use C.RevertReference as C.Reference, freeing all children of C.
    Similar to Revert in TCastleTransformDesignComponentEditor.ExecuteVerb. }
  procedure Revert(const E: TCastleComponentEditorDesigner; const C: TCastleTransformReference);
  var
    I: Integer;
  begin
    if C.RevertReference <> nil then
    begin
      for I := C.Count - 1 downto 0 do
        { Just like Revert in TCastleTransformDesignComponentEditor.ExecuteVerb
          comments:

          Note: we don't free csTransient, e.g. do not free editor gizmos.
          This is now checked by FreeComponentRecursively, no need for explicit
          check here, just be aware that some FreeComponentRecursively calls
          will be ignored.

          Note: It is critical to call E.FreeComponentRecursively,
          not just C.Items[I].Free.
          Because otherwise the components inside will be "left hanging":
          they will keep taking names in DesignOwner,
          they will keep being referenced (e.g. if design contained TCastleCamera,
          it can be referenced by some TCastleViewport.Camera outside of this design),
          and they are not part of hierarchy (so they would not be serialized to JSON). }
        E.FreeComponentRecursively(C.Items[I]);

      InternalCastleDesignInvalidate := true;

      C.Reference := C.RevertReference;
      E.Modified;
    end;
  end;

var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  case Index - InheritedCount of
    0:begin
        if GetDesigner is TCastleComponentEditorDesigner then
          Edit(GetDesigner as TCastleComponentEditorDesigner, Component as TCastleTransformReference);
      end;
    1:begin
        if GetDesigner is TCastleComponentEditorDesigner then
          Revert(GetDesigner as TCastleComponentEditorDesigner, Component as TCastleTransformReference);
      end;
    else inherited ExecuteVerb(Index);
  end;
end;
