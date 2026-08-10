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

{ List of selectable items: @link(TCastleListBox). }
unit GameListBox;

interface

uses CastleUiControls;

type
  { List of selectable and clickable items.

    Displays a list of @link(Items).
    Each item has a caption and can be selected.
    The currently selected item is @link(ItemIndex), -1 if none,
    and we make event @link(OnChange) when it changes due to user input
    (like clicking).

    By default, each item is displayed as a TCastleButton with style applied
    to make it look like a typical list box (e.g. similar to a TListBox known
    from LCL, VCL, FMX), so it's white, and it gets a border when it is selected.

    Set @link(TemplateUrl), @link(TemplateButtonName), @link(TemplateLabelName)
    to customize the look.

    The list automatically shows a scrollbar when there's more content than fits. }
  TCastleListBox = class(TCastleUserInterface)
  strict private
    FItems: TStrings;
    FScrollview: TCastleScrollView;
    FGroup: TCastlePackedGroup;
    function GetItemsUi(const Index: Integer): TCastleUserInterface;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { String representing each element.
      Just like with any TStrings, you can also associate an arbitrary object
      to each string. For example:

      @longCode(#
        MyList.Items.Add('my list item');
        MyList.Items.Add('my list item with MyObject', MyObject);
        ReadObject0 := MyList.Items.Objects[0]; // returns nil
        ReadObject1 := MyList.Items.Objects[0]; // returns MyObject
      #)

      TODO: changing this sucks, as we need to rebuild everything?
      Expose own
        Items[]: String,
        ItemsObjects[]: TObject,
        AddItem(String)
        AddItem(String, Object)
        ClearItems
        etc.

      TODO: own storage would allow to keep multi-line string for each item.
      Or is this possible with Tstrings too? Autotest on both FPC and Delphi,
      that we can add, read, get whole, set whole text (with speciali delimiter)
      and keep multi-line.

      TODO: If we resign from TStrings, we'll need own prop editor to edit in
      editor.
    }
    property Items: TStrings read FItems write SetItems;

    { Access the user interface instantiated for each list item.
      Always has the same count as @code(Items.Count).

      Treat it as read-only to be safe.
      In particular, do not free these objects (they must always be non-nil),
      and do not change their contents.
      The only exception is that you can change the contents
      if you loaded their layout by setting @link(TemplateUrl),
      and even then

      @unorderedList(
        @item(you cannot change some things of the button referenced
          by @link(TemplateButtonName):

          @unorderedList(
            @itemSpacing Compact
            @item @link(TCastleButton.Caption),
            @item @link(TCastleButton.OnClick),
            @item @link(TCastleButton.Pressed),
            @item @link(TCastleButton.Toggle).
          )
        )

        @item(you cannot change some things of the label referenced
          by @link(TemplateLabelName): @link(TCastleLabel.Caption).
        )
      )

      TODO: Or just hide this property, and disable modification,
      because it's inherently broken with how we cache items (to not recreate them
      on rebuilding). }
    property ItemsUi[const Index: Integer]: TCastleUserInterface read GetItemsUi;

    { Currently selected item, -1 if none.

      We call @link(OnChange) when it changes due to user input
      (like clicking), but not when you change it programmatically
      using this property. }
    property ItemIndex: Integer read FItemIndex write SetItemIndex;

    { Called when @link(ItemIndex) changes due to user input.

      Note: This is not called when @link(ItemIndex) changes because you modify
      that property programmatically. }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Customize the look by assigning a design file URL (usually with
      .castle-user-interface extension) to this property.
      Like @code(castle-data:/my_list_item_template.castle-user-interface).

      Use this to customize anything, e.g. font size, color of items'
      text, make the text honor HTML tags (using TCastleLabel with
      TCastleLabel.Html for each item), make the items transparent etc.

      Set also @link(TemplateButtonName) and maybe @link(TemplateLabelName)
      to make the template really functional. }
    property TemplateUrl: String read FTemplateUrl write SetTemplateUrl;

    { If @link(TemplateUrl) is not empty, then this determines the component
      name within the @link(TemplateUrl) design that represents a button
      (TCastleButton).

      This button is used to determine clicks (selection) on list items.
      The button is displayed using its "pressed" state
      (e.g. using @link(TCastleButton.CustomBackgroundPressed)
      if @link(TCastleButton.CustomBackground)) if the list item is selected.

      If @link(TemplateLabelName) is empty (or not found),
      this button's caption is also used to show the item text.

      If this property is empty (or not found), then list items are not clickable.
      If both @link(TemplateButtonName) and @link(TemplateLabelName) are empty
      (or not found), then the element's caption is not reflected anywhere,
      so the template isn't really functional.

      It's your responsibility to take care of the used item size.

      @unorderedList(
        @item(When the list is @link(Vertical) (default), you usually want to
          set @link(TCastleUserInterface.WidthFraction) to 1.0,
          and have constant non-zero @link(TCastleUserInterface.Height),
          (or determine height by auto-sizing to content size,
          which should be affected by the text height).

          Unless you use @link(AutoSize), then both width and height must
          be constant or determined by the content.)

        @item(When the list is horizontal, you usually want to set
          @link(TCastleUserInterface.HeightFraction) to 1.0,
          and have constant non-zero @link(TCastleUserInterface.Width),
          (or determine width by auto-sizing to content size,
          which should be affected by the text width).

          Unless you use @link(AutoSize), then both width and height must
          be constant or determined by the content.)
      )

      TODO: Is above true? We have issues with one dimension depending on parent,
      other on children.
    }
    property TemplateButtonName: String read FTemplateButtonName write SetTemplateButtonName;

    { If @link(TemplateUrl) is not empty, then this determines the component
      name within the @link(TemplateUrl) design that represents a label
      and displays the item caption.
      It can be any UI component, as we use @link(TCastleUserInterface.InternalText)
      to set the text, but in most typical case this is just @link(TCastleLabel). }
    property TemplateLabelName: String read FTemplateLabelName write SetTemplateLabelName;

    { Use a vertical (@true, default) or horizontal (@false) layout.

      TODO: No scrolling support when the list is horizontal for now.
      Just make sure the list contents fit within, or use @link(AutoSize). }
    property Vertical: Boolean read FVertical write SetVertical default true;

    { Adjust the control size to account for all the children. }
    property AutoSize: Boolean read FAutoSize write SetAutoSize;

    // TODO: MultiSelect, just like in LCL/VCL/FMX - see what ItemIndex means then, and how item selection is get/set
  end;

implementation

constructor TCastleListBox.Create(AOwner: TComponent);
begin
  inherited;
  FVertical := true;
  FItemIndex := -1;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := {$ifdef FPC}@{$endif} ItemsChange;
  RebuildGroup;
end;

destructor TCastleListBox.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TCastleListBox.GetItemsUi(const Index: Integer): TCastleUserInterface;
begin
  Result := FGroup.Controls[Index];
end;

end.
