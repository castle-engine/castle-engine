{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

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

  { List of UI controls, with a parent control and container.
    Ordered from back to front.
    Used for @link(TCastleContainer.Controls).

    User code should not create instances of this class.
    The only way to have an instance of it, is to get a container,
    by creating a @link(TCastleWindow) or @link(TCastleControl).
    Then you can access @code(MyWindow.Container.Controls).

    User code should rarely need to use methods and properties of this class.
    That's because most TCastleUserInterface controls should be added to TCastleView,
    so only TCastleView instances should be present at the top-level of the UI hierarchy
    in TInternalChildrenControls. And you should manage TCastleView instances using
    @link(TCastleContainer.View), @link(TCastleContainer.PushView) etc. that will make
    it added / removed from the TInternalChildrenControls automatically. }
  TInternalChildrenControls = class
  strict private
    FParent: TCastleUserInterface;
    FContainer: TCastleContainer;

    procedure RegisterContainer(const C: TCastleUserInterface; const AContainer: TCastleContainer);
    procedure UnregisterContainer(const C: TCastleUserInterface; const AContainer: TCastleContainer);
    procedure SetContainer(const AContainer: TCastleContainer);

    type
      TMyObjectList = class(TCastleObjectList)
        Parent: TInternalChildrenControls;
        { Pass notifications to Parent. }
        procedure Notify(Ptr: Pointer; Action: TListNotification); override;
      end;
      TCaptureFreeNotifications = class(TComponent)
      protected
        Parent: TInternalChildrenControls;
        { Pass notifications to Parent. }
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      end;
    var
      FList: TMyObjectList;
      FCaptureFreeNotifications: TCaptureFreeNotifications;

    function GetItem(const I: Integer): TCastleUserInterface;
    procedure SetItem(const I: Integer; const Item: TCastleUserInterface);
    { React to add/remove notifications. }
    procedure Notify(Ptr: Pointer; Action: TListNotification);
  private
    property Container: TCastleContainer read FContainer write SetContainer;
  public
    constructor Create(AParent: TCastleUserInterface);
    destructor Destroy; override;

    property Items[const I: Integer]: TCastleUserInterface read GetItem write SetItem; default;
    function Count: Integer;
    procedure Assign(const Source: TInternalChildrenControls);
    { Remove the Item from this list.
      Note that the given Item should always exist only once on a list
      (it is not allowed to add it multiple times), so there's no @code(RemoveAll)
      method. }
    procedure Remove(const Item: TCastleUserInterface);
    procedure Clear;
    procedure Add(const Item: TCastleUserInterface); deprecated 'use InsertFront or InsertBack';
    procedure Insert(Index: Integer; const Item: TCastleUserInterface);
    function IndexOf(const Item: TCastleUserInterface): Integer;

    { Make sure that NewItem is the only instance of given ReplaceClass
      on the list, replacing old item if necesssary.
      See TCastleObjectList.MakeSingle for precise description. }
    function MakeSingle(ReplaceClass: TCastleUserInterfaceClass; NewItem: TCastleUserInterface;
      AddFront: boolean = true): TCastleUserInterface;
      deprecated 'this is a complicated method without clear use-case now; do not use';

    { Add at the end of the list. }
    procedure InsertFront(const NewItem: TCastleUserInterface); overload;
    procedure InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertFront(const NewItems: TCastleUserInterfaceList); overload;

    { Add at the beginning of the list. }
    procedure InsertBack(const NewItem: TCastleUserInterface); overload;
    procedure InsertBackIfNotExists(const NewItem: TCastleUserInterface);
    procedure InsertBack(const NewItems: TCastleUserInterfaceList); overload;

    procedure InsertIfNotExists(const Index: Integer; const NewItem: TCastleUserInterface); deprecated 'use InsertFrontIfNotExists or InsertBackIfNotExists';
    procedure AddIfNotExists(const NewItem: TCastleUserInterface); deprecated 'use InsertFrontIfNotExists or InsertBackIfNotExists';

    { BeginDisableContextOpenClose disables sending
      TCastleUserInterface.GLContextOpen and TCastleUserInterface.GLContextClose to all the controls
      on the list. EndDisableContextOpenClose ends this.
      They work by increasing / decreasing the TCastleUserInterface.DisableContextOpenClose
      for all the items on the list.

      @groupBegin }
    procedure BeginDisableContextOpenClose;
    procedure EndDisableContextOpenClose;
    { @groupEnd }
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TInternalChildrenControls ------------------------------------------------------------- }

constructor TInternalChildrenControls.Create(AParent: TCastleUserInterface);
begin
  inherited Create;
  FParent := AParent;
  FList := TMyObjectList.Create;
  FList.Parent := Self;
  FCaptureFreeNotifications := TCaptureFreeNotifications.Create(nil);
  FCaptureFreeNotifications.Parent := Self;
end;

destructor TInternalChildrenControls.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FCaptureFreeNotifications);
  inherited;
end;

function TInternalChildrenControls.GetItem(const I: Integer): TCastleUserInterface;
begin
  Result := TCastleUserInterface(FList.Items[I]);
end;

procedure TInternalChildrenControls.SetItem(const I: Integer; const Item: TCastleUserInterface);
begin
  FList.Items[I] := Item;
end;

function TInternalChildrenControls.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TInternalChildrenControls.BeginDisableContextOpenClose;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TCastleUserInterface(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose + 1;
end;

procedure TInternalChildrenControls.EndDisableContextOpenClose;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    with TCastleUserInterface(FList.Items[I]) do
      DisableContextOpenClose := DisableContextOpenClose - 1;
end;

procedure TInternalChildrenControls.InsertFront(const NewItem: TCastleUserInterface);
begin
  Insert(Count, NewItem);
end;

procedure TInternalChildrenControls.InsertFrontIfNotExists(const NewItem: TCastleUserInterface);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertFront(NewItem);
end;

procedure TInternalChildrenControls.InsertFront(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := 0 to NewItems.Count - 1 do
    InsertFront(NewItems[I]);
end;

procedure TInternalChildrenControls.InsertBack(const NewItem: TCastleUserInterface);
begin
  FList.Insert(0, NewItem);
end;

procedure TInternalChildrenControls.InsertBackIfNotExists(const NewItem: TCastleUserInterface);
begin
  if FList.IndexOf(NewItem) = -1 then
    InsertBack(NewItem);
end;

procedure TInternalChildrenControls.Add(const Item: TCastleUserInterface);
begin
  InsertFront(Item);
end;

procedure TInternalChildrenControls.Insert(Index: Integer; const Item: TCastleUserInterface);
var
  I: Integer;
begin
  { TODO: code duplicated with TCastleUserInterfaceList.InsertWithZOrder }
  Index := Clamped(Index, 0, Count);
  if Item.KeepInFront or
     (Count = 0) or
     (Index = 0) or
     (not Items[Index - 1].KeepInFront) then
    FList.Insert(Index, Item) else
  begin
    for I := Index - 1 downto 0 do
      if not Items[I].KeepInFront then
      begin
        FList.Insert(I + 1, Item);
        Exit;
      end;
    { everything has KeepInFront = true }
    FList.Insert(0, Item);
  end;
end;

procedure TInternalChildrenControls.InsertIfNotExists(const Index: Integer; const NewItem: TCastleUserInterface);
begin
  Insert(Index, NewItem);
end;

procedure TInternalChildrenControls.AddIfNotExists(const NewItem: TCastleUserInterface);
begin
  Insert(Count, NewItem);
end;

function TInternalChildrenControls.IndexOf(const Item: TCastleUserInterface): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TInternalChildrenControls.InsertBack(const NewItems: TCastleUserInterfaceList);
var
  I: Integer;
begin
  for I := NewItems.Count - 1 downto 0 do
    InsertBack(NewItems[I]);
end;

procedure TInternalChildrenControls.Notify(Ptr: Pointer; Action: TListNotification);
var
  C: TCastleUserInterface;
begin
  { TODO: while this updating works cool,
    if the Parent or Container is destroyed
    before children --- the children will keep invalid reference. }

  C := TCastleUserInterface(Ptr);
  case Action of
    lnAdded:
      begin
        if ((C.Container <> nil) or (C.Parent <> nil)) and
           ((Container <> nil) or (FParent <> nil)) then
          WritelnWarning('UI', 'Inserting to the UI list (InsertFront, InsertBack) an item that is already a part of other UI list: ' + C.DebugName + '. The result is undefined, you cannot insert the same TCastleUserInterface instance multiple times.');
        C.FreeNotification(FCaptureFreeNotifications);
        if Container <> nil then RegisterContainer(C, FContainer);
        C.FParent := FParent;
      end;
    lnExtracted, lnDeleted:
      begin
        C.FParent := nil;
        if Container <> nil then UnregisterContainer(C, FContainer);
        C.RemoveFreeNotification(FCaptureFreeNotifications);
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TInternalChildrenControls.Notify action?');
    {$endif}
  end;

  { This notification may get called during FreeAndNil(FControls)
    in TCastleContainer.Destroy. Then FControls is already nil, and we're
    getting remove notification for all items (as FreeAndNil first sets
    object to nil). Testcase: lets_take_a_walk exit. }
  if (FContainer <> nil) and
     (FContainer.FControls <> nil) then
  begin
    if (FParent <> nil) and not (csDestroying in FParent.ComponentState) then
      FParent.VisibleChange([chChildren])
    else
    begin
      { if this is top-most control inside Container,
        we need to call ControlsVisibleChange directly. }
      Container.ControlsVisibleChange(C, [chChildren], false);
    end;
  end;
end;

procedure TInternalChildrenControls.TCaptureFreeNotifications.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  { We have to remove a reference to the object from list.
    This is crucial, various methods assume that all objects on
    the list are always valid objects (no invalid references,
    even for a short time). }

  if (Operation = opRemove) and (AComponent is TCastleUserInterface) then
    Parent.FList.Remove(AComponent);
end;

procedure TInternalChildrenControls.Assign(const Source: TInternalChildrenControls);
begin
  FList.Assign(Source.FList);
end;

procedure TInternalChildrenControls.Remove(const Item: TCastleUserInterface);
begin
  FList.Remove(Item);
end;

procedure TInternalChildrenControls.Clear;
begin
  FList.Clear;
end;

function TInternalChildrenControls.MakeSingle(ReplaceClass: TCastleUserInterfaceClass; NewItem: TCastleUserInterface;
  AddFront: boolean): TCastleUserInterface;
begin
  {$warnings off} // using deprecated in deprecated
  Result := FList.MakeSingle(ReplaceClass, NewItem, AddFront) as TCastleUserInterface;
  {$warnings on}
end;

procedure TInternalChildrenControls.RegisterContainer(
  const C: TCastleUserInterface; const AContainer: TCastleContainer);
begin
  { Register AContainer to be notified of control destruction. }
  C.FreeNotification(AContainer);

  C.InternalSetContainer(AContainer);
  C.CheckUIScaleChanged;

  { Make sure Resize is called at the nearest opportunity.
    For historic reasons, some controls may depend that Resize happens
    immediately after adding control to the container,
    they may even initialize GL resources there. }
  C.FLastSeenContainerWidth := 0;
  C.FLastSeenContainerHeight := 0;

  if AContainer.GLInitialized then
  begin
    if C.DisableContextOpenClose = 0 then
      C.GLContextOpen;
    AContainer.Invalidate;
    { Call initial Resize for control.
      If window OpenGL context is not yet initialized, defer it to later
      (we will call CheckResize before BeforeRender). }
    C.CheckResize;
  end;
end;

procedure TInternalChildrenControls.UnregisterContainer(
  const C: TCastleUserInterface; const AContainer: TCastleContainer);
begin
  if AContainer.GLInitialized and
     (C.DisableContextOpenClose = 0) then
    C.GLContextClose;

  C.RemoveFreeNotification(AContainer);
  AContainer.DetachNotification(C);

  C.InternalSetContainer(nil);
end;

procedure TInternalChildrenControls.SetContainer(const AContainer: TCastleContainer);
var
  I: Integer;
begin
  if FContainer <> AContainer then
  begin
    if FContainer <> nil then
      for I := 0 to Count - 1 do
        UnregisterContainer(Items[I], FContainer);
    FContainer := AContainer;
    if FContainer <> nil then
      for I := 0 to Count - 1 do
        RegisterContainer(Items[I], FContainer);
  end;
end;

{ TInternalChildrenControls.TMyObjectList ----------------------------------------------- }

procedure TInternalChildrenControls.TMyObjectList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  Parent.Notify(Ptr, Action);
end;

{$endif read_implementation}
