{
  Copyright 2006 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Management of "recent files" menu for programs using GLWindow menu.
  See TGLRecentMenu class. }
unit GLWindowRecentMenu;

interface

uses Classes, GLWindow, KambiXMLCfg;

const
  DefaultMaxCount = 5;

type
  TOnOpenRecent = procedure (const FileName: string) of object;

  { Management of "recent files" menu for programs using GLWindow menu. }
  TGLRecentMenu = class
  private
    FileNames: TStringList;
    FNextMenuItem: TMenuEntry;
    FirstSeparator: TMenuEntry;
    FOnOpenRecent: TOnOpenRecent;
    procedure MenuInit;
    { Note that MenuDone doesn't read FileNames list at all, it only
      depends on FirstSeparator value. }
    procedure MenuDone;
    procedure SetNextMenuItem(Value: TMenuEntry);
    FMaxCount: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

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

    { Adds as the most recent file FileName.

      Note that you want to place here only absolute filenames,
      so this will always ExpandFileName to make sure FileName is absolute. }
    procedure Add(const FileName: string);

    property OnOpenRecent: TOnOpenRecent read FOnOpenRecent write FOnOpenRecent;

    property MaxCount: Cardinal read FMaxCount write FMaxCount
      default DefaultMaxCount;

    { These load and save recently opened files list to/from the TKamXMLConfig file.
      Path should not contain final "/" --- it will be added automatically. }
    procedure LoadFromConfig(ConfigFile: TKamXMLConfig; const Path: string);
    procedure SaveToConfig(ConfigFile: TKamXMLConfig; const Path: string);
  end;

implementation

uses SysUtils, KambiClassUtils;

{ TMenuRecentItem ------------------------------------------------------------ }

type
  TMenuRecentItem = class(TMenuItem)
  private
    FFileName: string;
  public
    { Constructor. Number is 1-based number of this recent file entry. }
    constructor Create(const Number: Integer; const AFileName: string;
      AIntData: Integer);
    OnOpenRecent: TOnOpenRecent;
    property FileName: string read FFileName;
    function DoCommand: boolean; override;
  end;

constructor TMenuRecentItem.Create(
  const Number: Integer; const AFileName: string; AIntData: Integer);
var
  S: string;
begin
  FFileName := AFilename;

  if Number <= 9 then
    S := '_' + IntToStr(Number) else
  if Number = 10 then
    S := '1_0' else
    S := IntToStr(Number);
  S += '. ' + SQuoteMenuEntryCaption(ExtractFileName(FileName));

  inherited Create(S, AIntData);
end;

function TMenuRecentItem.DoCommand: boolean;
begin
  if Assigned(OnOpenRecent) then
    OnOpenRecent(FileName);
  Result := true;
end;

{ TGLRecentMenu -------------------------------------------------------------- }

constructor TGLRecentMenu.Create;
begin
  inherited;
  FileNames := TStringList.Create;
  FMaxCount := DefaultMaxCount;
end;

destructor TGLRecentMenu.Destroy;
begin
  FreeAndNil(FileNames);
  inherited;
end;

procedure TGLRecentMenu.MenuInit;
var
  I: Integer;
  ParentMenu: TMenu;
  Position: Cardinal;
  MenuRecentOpen: TMenuRecentItem;
begin
  { Add recent files menu }
  if (NextMenuItem <> nil) and (NextMenuItem.ParentMenu <> nil) and
    { When FileNames.Count = 0 then we don't want to add anything,
      even FirstSeparator. }
    (FileNames.Count > 0) then
  begin
    ParentMenu := NextMenuItem.ParentMenu;
    Position := NextMenuItem.ParentMenuPosition;
    FirstSeparator := TMenuSeparator.Create;
    ParentMenu.Insert(Position, FirstSeparator);
    for I := 0 to FileNames.Count - 1 do
    begin
      MenuRecentOpen := TMenuRecentItem.Create(I+1, FileNames[I], 0);
      MenuRecentOpen.OnOpenRecent := OnOpenRecent;
      ParentMenu.Insert(Position + Cardinal(I) + 1, MenuRecentOpen);
    end;
  end;
end;

procedure TGLRecentMenu.MenuDone;
var
  ParentMenu: TMenu;
  Position: Cardinal;
begin
  { Remove recent files menu from old NextMenuItem }
  if (NextMenuItem <> nil) and
    { Checks for below are to safeguard against cases
      when you could assign NextMenuItem before it had
      any ParentMenu, or if you somehow rearranged your menu,
      or if FileNames list was empty when you assigned NextMenuItem
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
      ParentMenu.EntryDelete(Position);
    until (ParentMenu.Entries[Position] = NextMenuItem);
  end;
end;

procedure TGLRecentMenu.SetNextMenuItem(Value: TMenuEntry);
begin
  if Value <> FNextMenuItem then
  begin
    MenuDone;
    FNextMenuItem := Value;
    MenuInit;
  end;
end;

procedure TGLRecentMenu.Add(const FileName: string);
var
  F: string;
  Index: Integer;
begin
  F := ExpandFileName(FileName);

  { We calculate Index, because if user opens a file already on the "recent files"
    list then we want to just move this filename to the top. }
  Index := FileNames.IndexOf(F);

  if Index = 0 then
    Exit { user reopens last opened file, nothing to do };

  if Index <> -1 then
    { Just move Index to the beginning }
    FileNames.Exchange(Index, 0) else
  begin
    FileNames.Insert(0, F);
    Strings_Trim(FileNames, MaxCount);
  end;

  MenuDone;
  MenuInit;
end;

procedure TGLRecentMenu.LoadFromConfig(ConfigFile: TKamXMLConfig; const Path: string);
var
  I, C: Integer;
  S: string;
begin
  FileNames.Clear;
  C := ConfigFile.GetValue(Path + '/count', 0);
  for I := 0 to C - 1 do
  begin
    S := ConfigFile.GetValue(Path + '/item' + IntToStr(I) + '/filename', '');
    if S <> '' then
      FileNames.Append(S);
  end;

  MenuDone;
  MenuInit;
end;

procedure TGLRecentMenu.SaveToConfig(ConfigFile: TKamXMLConfig; const Path: string);
var
  I: Integer;
begin
  ConfigFile.SetDeleteValue(Path + '/count', FileNames.Count, 0);
  for I := 0 to FileNames.Count - 1 do
    ConfigFile.SetDeleteValue(Path + '/item' + IntToStr(I) + '/filename', FileNames[I], '');
end;

end.