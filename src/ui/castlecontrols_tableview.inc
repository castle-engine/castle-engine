{%MainUnit castlecontrols.pas}
{
  Copyright 2017-2023 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO:

  The API of TCastleTableViewCell and TCastleTableView requires creating
  ICastleTableViewDataSource instead of "just" adding
  stuff by calling a method like "AddRow".
  And it doesn't allow visual designing by CGE editor.

  Compare this with TCastleVerticalGroup, to which you can add stuff like this:

    Group := TCastleVerticalGroup.Create;
    Group.InsertFront(Label1);
    Group.InsertFront(Label2);

  Ideally, we should have a grid component, that allows to add stuff in an easy
  way like this:

    Grid := TCastleGrid.Create(Owner);
    Grid.Rows := 5;
    Grid.Columns := 3;
    Grid.Cells[0, 0] := Label1;
    Grid.Cells[0, 2] := Label2;

  or

    Grid := TCastleGrid.Create(Owner);
    Grid.Columns := 3;
    GridRow1 := TCastleGridRow.Create(Owner);
    GridRow1.InsertFront(Label1);
    GridRow1.InsertFront(Label2);
    GridRow1.InsertFront(Label3);
    Grid.InsertFront(GridRow1);
}

{$ifdef read_interface}

  TCastleTableView = class;

  { Decoration used in @link(TCastleTableViewCell). }
  TCastleTableViewCellAccessoryType = (tvcaNone, tvcaCheckmark, tvcaDisclosureIndicator);

  { Cell inside @link(TCastleTableView). Do not create the cells by yourself,
    use TableView datasource to fill data.

    Cell is organized as:
    @preformatted(
    ---------------------------------------------------------------------
    | ImageIcon | TextLabel ......   | AccessoryControl | AccessoryType |
    ---------------------------------------------------------------------
    )
    }
  TCastleTableViewCell = class(TCastleRectangleControl)
  strict private
    FTextLabel: TCastleLabel;
    FImageIcon: TCastleImageControl;
    FAccessoryType: TCastleTableViewCellAccessoryType;
    FAccessoryTypeImage: TCastleImageControl;   // image representing the AccessoryType
    FAccessoryControl: TCastleUserInterface;

    procedure SetAccessoryControl(AControl: TCastleUserInterface);

  private
    procedure ReflectUIControls;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MakeEmpty;

    { Use TextLabel.Caption to set the text visible on the cell. }
    property TextLabel: TCastleLabel read FTextLabel;
    { Define the (square) image shown at the left cell border. }
    property ImageIcon: TCastleImageControl read FImageIcon;
    { Decoration shown at the right cell border. }
    property AccessoryType: TCastleTableViewCellAccessoryType
                            read FAccessoryType write FAccessoryType default tvcaNone;
    { Any custom control added at the right cell border (e.g. SwitchControl, ImageControl) }
    property AccessoryControl: TCastleUserInterface
                            read FAccessoryControl write SetAccessoryControl default nil;
  end;

  TCastleTableViewCellList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleTableViewCell>;

  { Data source used in @link(TCastleTableView). Implement this interface to
    populate data into the TableView.

    @deprecated Along with TCastleTableView, this is deprecated.

    @exclude We even exclude it, to not "stand out" in "Class Hierarchy" in API docs. }
  ICastleTableViewDataSource = interface
    { Return number of cells (rows). }
    function TableViewNumberOfRows(Sender: TCastleTableView): Integer;
    { Called when TableView needs the contents for the cell. TableView manages
      its cells automatically. For example set Cell.TextLabel.Caption to
      define the text displayed in the cell. }
    procedure TableViewUpdateCell(Cell: TCastleTableViewCell; Row: Integer; Sender: TCastleTableView);
  end;

  TTableViewDidSelectCellEvent = procedure(Row: Integer; Sender: TCastleTableView) of object;

  { TableView control to show lists in GUI. The usage is similar as in iOS
    TableView or Android ListView - you have to pass a data source (implement
    @link(ICastleTableViewDataSource) interface), and TableView control takes
    care of the rest. TableView control descends from @link(TCastleScrollView),
    so it shows the scrollbar when needed. }
  TCastleTableView = class(TCastleScrollView)
  strict private
    FDataSource: ICastleTableViewDataSource;
    FCells: TCastleTableViewCellList;
    FCellHeight: Integer;

    FOnDidSelectCell: TTableViewDidSelectCellEvent;

    FPressPos: TVector2;
    FMotionIsClick: boolean;
  public
    const
      DefaultCellHeight = 50;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReloadData;
    procedure SetDataSource(ADataSource: ICastleTableViewDataSource);

    function Press(const Event: TInputPressRelease): boolean; override;
    function Release(const Event: TInputPressRelease): boolean; override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;

    { TableView needs data source set in order to show some contents. }
    property DataSource: ICastleTableViewDatasource read FDataSource write SetDataSource;
    { Event called when cell was selected by user. }
    property OnSelectCell: TTableViewDidSelectCellEvent
                           read FOnDidSelectCell write FOnDidSelectCell;
    { Height of all cells. }
    property CellHeight: Integer read FCellHeight write FCellHeight default DefaultCellHeight;
  end deprecated 'use instead TCastleVerticalGroup with children as TCastleHorizontalGroup to create a table layout'; // it is better to visually design in editor, it is more flexible (with cells and borders being any TCastleUserInterface control), it has more natual API (you can just add rows)

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTableView ---------------------------------------------------------- }

constructor TCastleTableViewCell.Create(AOwner: TComponent);
begin
  inherited;
  FTextLabel := TCastleLabel.Create(Self);
  FTextLabel.Anchor(vpMiddle);
  FTextLabel.Color := White;
  InsertFront(FTextLabel);

  FImageIcon := TCastleImageControl.Create(Self);
  FImageIcon.Anchor(vpMiddle);
  InsertFront(FImageIcon);

  FAccessoryTypeImage := nil;
  FAccessoryControl := nil;

  MakeEmpty;
end;

destructor TCastleTableViewCell.Destroy;
begin
  inherited;
end;

procedure TCastleTableViewCell.MakeEmpty;
begin
  TextLabel.Caption := '';
  ImageIcon.Image := nil;
  AccessoryType := tvcaNone;
  AccessoryControl := nil;
  Tag := 0;
  Color := Vector4(0.0, 0.0, 0.0, 0.0); // transparent
end;

procedure TCastleTableViewCell.SetAccessoryControl(AControl: TCastleUserInterface);
begin
  if AControl = FAccessoryControl then
    Exit;
  // remove old
  if FAccessoryControl <> nil then
  begin
    RemoveControl(FAccessoryControl);
    RemoveComponent(FAccessoryControl);
    FreeAndNil(FAccessoryControl);
  end;

  // set new
  if AControl <> nil then
  begin
    AControl.Anchor(vpMiddle);    // horizontal posistion is set in ReflectUIControls
    InsertFront(AControl);
    FAccessoryControl := AControl;
  end;
end;

procedure TCastleTableViewCell.ReflectUIControls;
var
  NextLeft, NextRight, Spacing: Single;
begin
  if FAccessoryType = tvcaNone then
  begin
    if FAccessoryTypeImage <> nil then
      FAccessoryTypeImage.Exists := false;
  end
  else begin
    if FAccessoryTypeImage <> nil then
      FAccessoryTypeImage.Exists := true
    else begin
      FAccessoryTypeImage := TCastleImageControl.Create(Self);
      FAccessoryTypeImage.Anchor(vpMiddle);
      InsertFront(FAccessoryTypeImage);
    end;

    if FAccessoryType = tvcaCheckmark then
    begin
      FAccessoryTypeImage.OwnsImage := false;
      FAccessoryTypeImage.Image := Theme.ImagesPersistent[tiCheckmark].Image;
      FAccessoryTypeImage.Color := FTextLabel.Color;
    end
    else if FAccessoryType = tvcaDisclosureIndicator then begin
      FAccessoryTypeImage.OwnsImage := false;
      FAccessoryTypeImage.Image := Theme.ImagesPersistent[tiDisclosure].Image;
      FAccessoryTypeImage.Color := Theme.DisabledTextColor;
    end;
  end;

  // update horizontal positions
  NextLeft := 5;
  NextRight := -5;
  Spacing := 8;

  if (FAccessoryTypeImage <> nil) and FAccessoryTypeImage.Exists then
  begin
    FAccessoryTypeImage.Anchor(hpRight, NextRight);
    NextRight := NextRight - FAccessoryTypeImage.EffectiveWidth - Spacing;
  end;

  if (FAccessoryControl <> nil) and FAccessoryControl.Exists then
  begin
    FAccessoryControl.Anchor(hpRight, NextRight);
    NextRight := NextRight - FAccessoryControl.EffectiveWidth - Spacing;
  end;

  if (FImageIcon.Image <> nil) and FImageIcon.Exists then
  begin
    FImageIcon.Anchor(hpLeft, NextLeft);
    NextLeft := NextLeft + FImageIcon.EffectiveWidth + Spacing;
  end;

  FTextLabel.Anchor(hpLeft, NextLeft);
  FTextLabel.MaxWidth := EffectiveWidth - NextLeft + NextRight;
end;

constructor TCastleTableView.Create(AOwner: TComponent);
begin
  inherited;
  FCells := TCastleTableViewCellList.Create;
  FCells.OwnsObjects := false;
  FDataSource := nil;
  FOnDidSelectCell := nil;
  FMotionIsClick := false;
  FCellHeight := DefaultCellHeight;
end;

destructor TCastleTableView.Destroy;
begin
  FreeAndNil(FCells);
  inherited;
end;

procedure TCastleTableView.ReloadData;
var
  I, ItemCount, OldCount: Integer;
  Cell: TCastleTableViewCell;
  CellWidth, SeparatorHeight: Single;
begin
  if ScrollbarVisible then
    CellWidth := Width - ScrollBarWidth
  else
    CellWidth := Width;
  SeparatorHeight := 1;

  if Assigned(FDataSource) then
    ItemCount := FDataSource.TableViewNumberOfRows(Self)
  else
    ItemCount := 3;
  OldCount := FCells.Count;
  for I := 0 to ItemCount - 1 do
  begin
    if I < OldCount then
    begin
      Cell := FCells.Items[I];
      Cell.MakeEmpty;
    end
    else begin
      Cell := TCastleTableViewCell.Create(ScrollArea);
      ScrollArea.InsertFront(Cell);
      FCells.Add(Cell);
    end;

    if Assigned(FDataSource) then
      FDataSource.TableViewUpdateCell(Cell, I, Self)
    else
      Cell.TextLabel.Caption := Format('Cell %d', [I+1]);

    Cell.Width := CellWidth;
    Cell.Height := CellHeight;
    Cell.Anchor(vpTop, -I * (CellHeight + SeparatorHeight));
    Cell.ReflectUIControls;
  end;

  // remove unused cells
  for I := OldCount - 1 downto ItemCount do
  begin
    Cell := FCells.Items[I];
    ScrollArea.RemoveControl(Cell);
    ScrollArea.RemoveComponent(Cell);
    FCells.Delete(I);
    FreeAndNil(Cell);
  end;

  ScrollArea.Height := ItemCount * (CellHeight + SeparatorHeight);
  ScrollArea.Width := EffectiveWidth;
end;

procedure TCastleTableView.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  WasScrollbar: boolean;
  Cell: TCastleTableViewCell;
  I: Integer;
  CellWidth: Single;
begin
  WasScrollbar := ScrollbarVisible;

  inherited;

  { update the cell width when scrollbar changed visibility }
  if WasScrollbar <> ScrollbarVisible then
  begin
    if ScrollbarVisible then
      CellWidth := Width - ScrollBarWidth
    else
      CellWidth := Width;
    for I := 0 to FCells.Count - 1 do
    begin
      Cell := FCells.Items[I];
      Cell.Width := CellWidth;
      Cell.ReflectUIControls;
    end;
  end;
end;

procedure TCastleTableView.SetDataSource(ADataSource: ICastleTableViewDataSource);
begin
  FDataSource := ADataSource;
  ReloadData;
end;

function TCastleTableView.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;

  if Assigned(FOnDidSelectCell) then
  begin
    FPressPos := Event.Position;
    FMotionIsClick := true;
    Result := true;   // be sure to get messages
  end;
end;

function TCastleTableView.Release(const Event: TInputPressRelease): boolean;
var
  I: Integer;
  CellRect: TFloatRectangle;
begin
  Result := inherited;
  if Assigned(FOnDidSelectCell) and FMotionIsClick then
  begin
    for I := 0 to FCells.Count - 1 do
    begin
      CellRect := FCells.Items[I].RenderRect;
      if CellRect.Contains(FPressPos) then
      begin
        FOnDidSelectCell(I, Self);
        break;
      end;
    end;
  end;
  FMotionIsClick := false;
end;

function TCastleTableView.Motion(const Event: TInputMotion): boolean;
begin
  Result := inherited;

  if Assigned(FOnDidSelectCell) and FMotionIsClick and (PointsDistance(Event.Position, FPressPos) > 4.0) then
    FMotionIsClick := false;
end;

{$endif read_implementation}
