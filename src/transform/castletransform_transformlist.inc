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
  { List of TCastleTransform instances. }
  TCastleTransformList = class({$ifdef FPC}specialize{$endif} TObjectList<TCastleTransform>)
  private
    { Note: Using this class with FOwner <> nil is only for internal purposes. }
    FOwner: TCastleTransform;

    { TCastleTransform instance that gets notified when items
      are added or removed.
      May be @nil, for example when this list is used by TRayCollision. }
    property Owner: TCastleTransform read FOwner;
  protected
    procedure Notify({$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AValue: TCastleTransform;
      ACollectionNotification: TCollectionNotification); override;
  public
    { Delete all found instances of the given Item.
      Shifts all other pointers to the left.
      Returns how many instances were removed (that is, how much Count
      was decreased).

      This does not free the items,
      unless the list has been created with OwnsObjects = @true. }
    function RemoveAll(const Item: TCastleTransform): Cardinal;
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

procedure TCastleTransformList.Notify(
  {$ifdef GENERICS_CONSTREF}constref{$else}const{$endif} AValue: TCastleTransform;
  ACollectionNotification: TCollectionNotification);
begin
  inherited;

  if Owner <> nil then
  begin
    case ACollectionNotification of
      cnAdded:
        AValue.AddParent(Owner);
      cnExtracted, cnRemoved:
        AValue.RemoveParent(Owner);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleTransformList.Notify ACollectionNotification?');
      {$endif}
    end;
  end;
end;

function TCastleTransformList.RemoveAll(const Item: TCastleTransform): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := 0;
  while I < Count do
  begin
    if Items[I] = Item then
    begin
      Delete(I);
      Inc(Result);
    end else
      Inc(I);
  end;
end;

{$endif read_implementation}
