{%MainUnit castletransform.pas}
{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type
  { List of TCastleTransform instances.
    This inherits from TCastleObjectList, getting many
    features like TList notification mechanism. }
  TCastleTransformList = class(TCastleObjectList)
  private
    { Note: Using this class with FOwner <> nil is only for internal purposes. }
    FOwner: TCastleTransform;
    function GetItem(const I: Integer): TCastleTransform;
    procedure SetItem(const I: Integer; const Item: TCastleTransform);
    { TCastleTransform instance that owns this list.
      May be @nil, for example when this list is used by TRayCollision. }
    property Owner: TCastleTransform read FOwner;

    procedure SortBackToFront2D;
      deprecated 'use TCastleViewport.BlendingSort to sort objects for blending';
  public
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Items[const I: Integer]: TCastleTransform read GetItem write SetItem; default;

    function First: TCastleTransform;
    function Last: TCastleTransform;
  end;

{$endif read_interface}

{$ifdef read_implementation}

function CompareBackToFront2D(A, B: Pointer): Integer;
begin
  {$warnings off} // using deprecated in deprecated
  Result := TBox3D.CompareBackToFront2D(TCastleTransform(A).BoundingBox, TCastleTransform(B).BoundingBox);
  {$warnings on}
end;

{ TCastleTransformList ------------------------------------------------------------ }

procedure TCastleTransformList.Notify(Ptr: Pointer; Action: TListNotification);
var
  B: TCastleTransform;
begin
  inherited;

  if Owner <> nil then
  begin
    B := TCastleTransform(Ptr);

    case Action of
      lnAdded: B.AddParent(Owner);
      lnExtracted, lnDeleted: B.RemoveParent(Owner);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTransformList.Notify action?');
      {$endif}
    end;

    if (Owner.World <> nil) and Assigned(Owner.World.OnCursorChange) then
      Owner.World.OnCursorChange(Owner);
  end;
end;

function TCastleTransformList.GetItem(const I: Integer): TCastleTransform;
begin
  Result := TCastleTransform(inherited Items[I]);
end;

procedure TCastleTransformList.SetItem(const I: Integer; const Item: TCastleTransform);
begin
  inherited Items[I] := Item;
end;

function TCastleTransformList.First: TCastleTransform;
begin
  Result := (inherited First) as TCastleTransform;
end;

function TCastleTransformList.Last: TCastleTransform;
begin
  Result := (inherited Last) as TCastleTransform;
end;

procedure TCastleTransformList.SortBackToFront2D;
begin
  Sort(@CompareBackToFront2D);
end;

{$endif read_implementation}
