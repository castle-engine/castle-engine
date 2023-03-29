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

    { Sort objects back-to-front @italic(right now)
      following one of the blending sorting algorithms.
      See @link(TCastleTransform.SortBackToFront) for description. }
    procedure SortBackToFront(const BlendingSort: TBlendingSort;
      const CameraPosition: TVector3);
  public
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Items[const I: Integer]: TCastleTransform read GetItem write SetItem; default;

    function First: TCastleTransform;
    function Last: TCastleTransform;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ utilities for TCastleTransformList.SortBackToFront ------------------------- }

function CompareBackToFront2D(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront2D(TCastleTransform(A).BoundingBox, TCastleTransform(B).BoundingBox);
end;

var
  { Has to be global, since TObjectList.Sort
    requires normal function (not "of object"). }
  SortCameraPosition: TVector3;

function CompareBackToFront3DBox(A, B: Pointer): Integer;
begin
  Result := TBox3D.CompareBackToFront3D(TCastleTransform(A).BoundingBox, TCastleTransform(B).BoundingBox,
    SortCameraPosition);
end;

function CompareBackToFront3DOrigin(A, B: Pointer): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := TCastleTransform(A).Translation;
  PointB := TCastleTransform(B).Translation;
  Result := Sign(
    PointsDistanceSqr(PointB, SortCameraPosition) -
    PointsDistanceSqr(PointA, SortCameraPosition));
end;

function CompareBackToFront3DGround(A, B: Pointer): Integer;
var
  PointA, PointB: TVector3;
begin
  PointA := TCastleTransform(A).Translation;
  PointB := TCastleTransform(B).Translation;
  PointA.Y := 0;
  PointB.Y := 0;
  Result := Sign(
    PointsDistanceSqr(PointB, SortCameraPosition) -
    PointsDistanceSqr(PointA, SortCameraPosition));
end;

function CompareBackToFrontYSort(A, B: Pointer): Integer;
var
  Ya, Yb: Single;
begin
  Ya := TCastleTransform(A).OutsideTranslation.Y + TCastleTransform(A).YSortOffset;
  Yb := TCastleTransform(B).OutsideTranslation.Y + TCastleTransform(B).YSortOffset;
  Result := Sign(Yb - Ya);
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

procedure TCastleTransformList.SortBackToFront(const BlendingSort: TBlendingSort;
  const CameraPosition: TVector3);
begin
  case BlendingSort of
    bs2D: Sort(@CompareBackToFront2D);
    bsYSort: Sort(@CompareBackToFrontYSort);
    bs3D:
      begin
        SortCameraPosition := CameraPosition;
        Sort(@CompareBackToFront3DBox);
      end;
    bs3DOrigin:
      begin
        SortCameraPosition := CameraPosition;
        Sort(@CompareBackToFront3DOrigin);
      end;
    bs3DGround:
      begin
        SortCameraPosition := CameraPosition;
        Sort(@CompareBackToFront3DGround);
      end;
    else ;
  end;
end;

{$endif read_implementation}
