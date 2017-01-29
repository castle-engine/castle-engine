{
  Copyright 2006-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage a list of recently opened files, and show a menu in TCastleWindowCustom.
  See TRecentFiles class. }
unit CastleWindowRecentFiles;

{$I castleconf.inc}

interface

uses Classes, CastleWindow, CastleRecentFiles;

type
  { Manage a list of recently opened files, and show a menu in TCastleWindowCustom. }
  TWindowRecentFiles = class(TRecentFiles)
  private
    FNextMenuItem: TMenuEntry;
    FirstSeparator: TMenuEntry;
    procedure SetNextMenuItem(Value: TMenuEntry);
  protected
    procedure MenuCreate; override;

    { Destroy the menu. Internal (do not call this directly, it's only called
      by parent class).
      Note that it doesn't read URLs list at all, it only
      depends on FirstSeparator value. }
    procedure MenuDestroy; override;
  public
    { This determines the placement of "recent files" list inside your menu.

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
    property NextMenuItem: TMenuEntry read FNextMenuItem write SetNextMenuItem;
  end;

implementation

uses SysUtils, CastleClassUtils, CastleURIUtils;

{ TMenuRecentItem ------------------------------------------------------------ }

type
  TMenuRecentItem = class(TMenuItem)
  private
    FURL: string;
  public
    { Constructor. Number is 1-based number of this recent file entry. }
    constructor Create(const Number: Integer; const AURL: string;
      AIntData: Integer);
  public
    OnOpenRecent: TOnOpenRecent;
    property URL: string read FURL;
    function DoClick: boolean; override;
  end;

constructor TMenuRecentItem.Create(
  const Number: Integer; const AURL: string; AIntData: Integer);
var
  S: string;
begin
  FURL := AURL;

  if Number <= 9 then
    S := '_' + IntToStr(Number) else
  if Number = 10 then
    S := '1_0' else
    S := IntToStr(Number);
  S += '. ' + SQuoteMenuEntryCaption(URICaption(URL));

  inherited Create(S, AIntData);
end;

function TMenuRecentItem.DoClick: boolean;
begin
  if Assigned(OnOpenRecent) then
    OnOpenRecent(URL);
  Result := true;
end;

{ TWindowRecentFiles -------------------------------------------------------------- }

procedure TWindowRecentFiles.MenuCreate;
var
  I: Integer;
  ParentMenu: TMenu;
  Position: Cardinal;
  MenuRecentOpen: TMenuRecentItem;
begin
  inherited;

  { Add recent files menu }
  if (NextMenuItem <> nil) and (NextMenuItem.ParentMenu <> nil) and
    { When URLs.Count = 0 then we don't want to add anything,
      even FirstSeparator. }
    (URLs.Count > 0) then
  begin
    ParentMenu := NextMenuItem.ParentMenu;
    Position := NextMenuItem.ParentMenuPosition;
    FirstSeparator := TMenuSeparator.Create;
    ParentMenu.Insert(Position, FirstSeparator);
    for I := 0 to URLs.Count - 1 do
    begin
      MenuRecentOpen := TMenuRecentItem.Create(I+1, URLs[I], 0);
      MenuRecentOpen.OnOpenRecent := OnOpenRecent;
      ParentMenu.Insert(Position + Cardinal(I) + 1, MenuRecentOpen);
    end;
  end;
end;

procedure TWindowRecentFiles.MenuDestroy;
var
  ParentMenu: TMenu;
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
    (NextMenuItem.ParentMenu <> nil) and
    (FirstSeparator <> nil) and
    (NextMenuItem.ParentMenu = FirstSeparator.ParentMenu) then
  begin
    ParentMenu := NextMenuItem.ParentMenu;
    Position := FirstSeparator.ParentMenuPosition;
    Assert(ParentMenu.Entries[Position] = FirstSeparator);
    FirstSeparator := nil;
    repeat
      ParentMenu.Delete(Position);
    until (ParentMenu.Entries[Position] = NextMenuItem);
  end;

  inherited;
end;

procedure TWindowRecentFiles.SetNextMenuItem(Value: TMenuEntry);
begin
  if Value <> FNextMenuItem then
  begin
    MenuDestroy;
    FNextMenuItem := Value;
    MenuCreate;
  end;
end;

end.
