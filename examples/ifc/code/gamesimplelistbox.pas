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

{ List of selectable items in @link(TCastleListBox).
  This is a simple implementation of list box specialized
  for our needs in examples/ifc/ .

  It allows to prevent slowdown by not rebuilding the UI when

  - we changed IFC, but no change to what list is showing is necessary
    (e.g. we only changed translation of sthg)

  - or we only need to change list item's selected state.

  Never operate on the children of this list directly.
  Do not use ClearControls, InsertFront, InsertBack, InsertControl.
  Use only the relevant methods of this list to add and remove items,
  they make sure to keep various internal state correct (index, associated items)
  and also release memory of unused list items (so that rebuilding lists
  doesn't continue to consume some memory from removed items).

  TODO: More complete and universal list box
  is coming soon to CGE, following plan in GameListBox unit. }
unit GameSimpleListBox;

interface

uses SysUtils, Classes, Contnrs,
  CastleControls, CastleComponentSerialize, CastleUiControls, CastleUtils;

type
  TCastleListBox = class;

  TListClickEvent = procedure (const Sender: TCastleListBox;
    const ItemIndex: Integer; const ItemObject: TObject) of object;

  { List of selectable items. }
  TCastleListBox = class(TCastleVerticalGroup)
  private
    type
      TItem = record
        Owner: TComponent;
        Button: TCastleButton;
        ItemLabel: TCastleLabel;
        AssociatedObject: TObject;
      end;
      TItemList = {$ifdef FPC}specialize{$endif} TStructList<TItem>;
    var
      FItemFactory: TCastleComponentFactory;
      FTemplateLabelName, FTemplateButtonName: String;
      FItems: TItemList;
      FItemIndex: Integer;
      FOnClick: TListClickEvent;
    function GetItemsStrings(const Index: Integer): String;
    procedure SetItemsStrings(const Index: Integer; const Value: String);
    procedure ButtonClick(Sender: TObject);
    procedure SetItemIndex(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadItemTemplate(const Template: TCastleUserInterface);
    property TemplateLabelName: String read FTemplateLabelName write FTemplateLabelName;
    property TemplateButtonName: String read FTemplateButtonName write FTemplateButtonName;

    function ItemsCount: Integer;
    property ItemsStrings[const Index: Integer]: String
      read GetItemsStrings write SetItemsStrings;
    procedure AddItem(const S: String; const AssociatedObject: TObject);

    { Delete last item from list.

      Releases all the item's allocated memory, so removed items do not
      continue to consume any memory.

      This is a specialized delete, because it can be done efficiently
      and it doesn't break indexes of existing items. }
    procedure DeleteLastItem;

    { Delete all the items.
      Releases also all their memory, so removed items do not
      continue to consume any memory.

      Also sets ItemIndex to -1. }
    procedure ClearItems;

    { Currently selected item, or -1 if none. }
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;

    property OnClick: TListClickEvent read FOnClick write FOnClick;
  end;

implementation

constructor TCastleListBox.Create(AOwner: TComponent);
begin
  inherited;
  FItemFactory := TCastleComponentFactory.Create(nil);
  FItems := TItemList.Create;
  FItemIndex := -1;
end;

destructor TCastleListBox.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FItemFactory);
  if FItems <> nil then
  begin
    { More optimal version of
        while ItemsCount > 0 do DeleteLastItem; }
    for I := 0 to FItems.Count - 1 do
      FreeAndNil(FItems.L[I].Owner);
    FreeAndNil(FItems);
  end;
  inherited;
end;

procedure TCastleListBox.LoadItemTemplate(const Template: TCastleUserInterface);
begin
  FItemFactory.LoadFromComponent(Template);
end;

function TCastleListBox.ItemsCount: Integer;
begin
  Result := FItems.Count;
end;

function TCastleListBox.GetItemsStrings(const Index: Integer): String;
begin
  Result := FItems.L[Index].ItemLabel.Caption;
end;

procedure TCastleListBox.SetItemsStrings(const Index: Integer; const Value: String);
begin
  FItems.L[Index].ItemLabel.Caption := Value;
end;

procedure TCastleListBox.ClearItems;
var
  I: Integer;
begin
  { More optimal version of
      while ItemsCount > 0 do DeleteLastItem; }
  for I := 0 to FItems.Count - 1 do
    FreeAndNil(FItems.L[I].Owner);
  FItems.Clear;
  FItemIndex := -1;
end;

procedure TCastleListBox.DeleteLastItem;
var
  LastIndex: Integer;
begin
  if FItems.Count = 0 then
    raise Exception.Create('Cannot use TCastleListBox.DeleteLastItem on empty list');
  LastIndex := FItems.Count - 1;
  FreeAndNil(FItems.L[LastIndex].Owner);
  FItems.Count := LastIndex;
end;

procedure TCastleListBox.AddItem(const S: String; const AssociatedObject: TObject);
var
  NewItem: TItem;
  NewItemUi: TCastleUserInterface;
  NewIndex: Integer;
begin
  NewIndex := ItemsCount;

  NewItem.Owner := TComponent.Create(nil);
  NewItem.AssociatedObject := AssociatedObject;

  NewItemUi := FItemFactory.ComponentLoad(NewItem.Owner) as TCastleUserInterface;
  NewItemUi.SetTransient;
  InsertFront(NewItemUi);

  NewItem.Button := NewItem.Owner.FindRequiredComponent(TemplateButtonName) as TCastleButton;
  NewItem.Button.OnClick := {$ifdef FPC}@{$endif} ButtonClick;
  NewItem.Button.Tag := NewIndex;

  NewItem.ItemLabel := NewItem.Owner.FindRequiredComponent(TemplateLabelName) as TCastleLabel;
  NewItem.ItemLabel.Caption := S;

  FItems.Add(NewItem);
end;

procedure TCastleListBox.ButtonClick(Sender: TObject);
var
  SenderComponent: TComponent;
begin
  if Assigned(OnClick) then
  begin
    SenderComponent := Sender as TComponent;
    ItemIndex := SenderComponent.Tag;
    OnClick(Self, ItemIndex, FItems.L[ItemIndex].AssociatedObject);
  end;
end;

procedure TCastleListBox.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    if Between(FItemIndex, 0, FItems.Count - 1) then
      FItems.L[FItemIndex].Button.Pressed := false;
    FItemIndex := Value;
    if Between(FItemIndex, 0, FItems.Count - 1) then
      FItems.L[FItemIndex].Button.Pressed := true;
  end;
end;

end.
