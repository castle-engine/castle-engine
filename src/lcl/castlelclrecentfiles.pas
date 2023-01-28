{
  Copyright 2006-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage a list of recently opened files, and show a menu in Lazarus menu.
  See TRecentFiles class. }
unit CastleLCLRecentFiles;

{$I castleconf.inc}

interface

uses Menus, CastleRecentFiles;

type
  { Manage a list of recently opened files, and show a menu in Lazarus. }
  TCastleRecentFiles = class(TRecentFiles)
  private
    FNextMenuItem: TMenuItem;
    FirstSeparator: TMenuItem;
    procedure SetNextMenuItem(Value: TMenuItem);
  protected
    procedure MenuCreate; override;

    { Destroy the menu. Internal (do not call this directly, it's only called
      by parent class).
      Note that it doesn't read URLs list at all, it only
      depends on FirstSeparator value. }
    procedure MenuDestroy; override;
  public
    { Determines the placement of "recent files" list inside your menu.

      If this is not @nil then each update to the recent files
      list (for example by @link(Add) method) will result in the menu
      items right before NextMenuItem to be updated.

      Also changing the value of this property will result in one-time
      update of the menu. Menu at the place of old NextMenuItem
      will be removed and menu at the place of new NextMenuItem will
      get created.

      Usually NextMenuItem should be the separator that divides
      the print and properties menu sections and the close menu sections.
      That's consistent with other GNOME programs, and this is what we usually
      try to follow. }
    property NextMenuItem: TMenuItem read FNextMenuItem write SetNextMenuItem;
  end;

implementation

uses SysUtils, Classes, CastleLCLUtils, CastleURIUtils;

{ TMenuRecentItem ------------------------------------------------------------ }

type
  TMenuRecentItem = class(TMenuItem)
  private
    FURL: string;
  public
    { Constructor. Number is 1-based number of this recent file entry. }
    constructor Create(AOwner: TComponent;
      const Number: Integer; const AURL: string); reintroduce;
  public
    OnOpenRecent: TOnOpenRecent;
    property URL: string read FURL;
    procedure Click; override;
  end;

constructor TMenuRecentItem.Create(AOwner: TComponent;
  const Number: Integer; const AURL: string);
var
  S: string;
begin
  FURL := AURL;

  if Number <= 9 then
    S := '&' + IntToStr(Number) else
  if Number = 10 then
    S := '1&0' else
    S := IntToStr(Number);
  S += '. ' + SQuoteLCLCaption(URICaption(URL));

  inherited Create(AOwner);

  Caption := S;
end;

procedure TMenuRecentItem.Click;
begin
  inherited;
  if Assigned(OnOpenRecent) then
    OnOpenRecent(URL);
end;

{ TCastleRecentFiles -------------------------------------------------------------- }

procedure TCastleRecentFiles.MenuCreate;
var
  I: Integer;
  ParentMenu: TMenuItem;
  Position: Cardinal;
  MenuRecentOpen: TMenuRecentItem;
begin
  inherited;

  { Add recent files menu }
  if (NextMenuItem <> nil) and (NextMenuItem.Parent <> nil) and
    { When URLs.Count = 0 then we don't want to add anything,
      even FirstSeparator. }
    (URLs.Count > 0) then
  begin
    ParentMenu := NextMenuItem.Parent;
    Position := NextMenuItem.MenuIndex;
    FirstSeparator := TMenuItem.Create(nil);
    FirstSeparator.Caption := '-';
    ParentMenu.Insert(Position, FirstSeparator);
    for I := 0 to URLs.Count - 1 do
    begin
      MenuRecentOpen := TMenuRecentItem.Create(nil, I+1, URLs[I]);
      MenuRecentOpen.OnOpenRecent := OnOpenRecent;
      ParentMenu.Insert(Position + Cardinal(I) + 1, MenuRecentOpen);
    end;
  end;
end;

procedure TCastleRecentFiles.MenuDestroy;
var
  ParentMenu: TMenuItem;
  Position: Cardinal;
begin
  { Remove recent files menu from old NextMenuItem }
  if (NextMenuItem <> nil) and
    { Checks for below are to safeguard against cases
      when you could assign NextMenuItem before it had
      any ParentMenu, or if you somehow rearranged your menu,
      or if URLs list was empty when you assigned NextMenuItem
      (in this case FirstSeparator is left nil, but later it should
      be created after the 1st add). }
    (NextMenuItem.Parent <> nil) and
    (FirstSeparator <> nil) and
    (NextMenuItem.Parent = FirstSeparator.Parent) then
  begin
    ParentMenu := NextMenuItem.Parent;
    Position := FirstSeparator.MenuIndex;
    Assert(ParentMenu[Position] = FirstSeparator);
    FirstSeparator := nil;
    repeat
      ParentMenu.Delete(Position);
    until (ParentMenu[Position] = NextMenuItem);
  end;

  inherited;
end;

procedure TCastleRecentFiles.SetNextMenuItem(Value: TMenuItem);
begin
  if Value <> FNextMenuItem then
  begin
    MenuDestroy;
    FNextMenuItem := Value;
    MenuCreate;
  end;
end;

end.
