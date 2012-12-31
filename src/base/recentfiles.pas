{
  Copyright 2006-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Manage a list of recently open files (TBaseRecentFiles). }
unit RecentFiles;

interface

uses Classes, CastleXMLConfig;

type
  TOnOpenRecent = procedure (const FileName: string) of object;

  { Manage a list of recently open files.

    This is designed as a base class, usable on it's own, but also
    as a parent for classes that show this list inside a menu.
    For Lazarus menu version, see TLazRecentFiles.
    For TCastleWindowBase menu version, see TCastleRecentFiles. }
  TBaseRecentFiles = class(TComponent)
  private
    FFileNames: TStringList;
    FMaxCount: Cardinal;
    FOnOpenRecent: TOnOpenRecent;

    { Load and save recently opened files list to/from the Config file. }
    procedure LoadFromConfig(const Config: TCastleConfig);
    procedure SaveToConfig(const Config: TCastleConfig);
  protected
    { Create and destroy menu (or anything else that mirrors FileNames contents).
      @groupBegin }
    procedure MenuCreate; virtual;
    procedure MenuDestroy; virtual;
    { @groupEnd }
  public
    const
      DefaultMaxCount = 5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds as the most recent file FileName.

      If MaybeStdIn, then we treat FileName = '-' specially:
      it's ignored. Use this if your program interprets '-' as "load file
      from standard input", such files should not be added to recent files menu.

      Note that we want to store only absolute filenames,
      so this method will always call ExpandFileName to make sure stored
      filename is absolute. }
    procedure Add(const FileName: string; const MaybeStdIn: boolean = true); virtual;

    { List of currently stored filenames. @italic(This is readonly.) }
    property FileNames: TStringList read FFileNames;
  published
    property OnOpenRecent: TOnOpenRecent read FOnOpenRecent write FOnOpenRecent;

    property MaxCount: Cardinal read FMaxCount write FMaxCount
      default DefaultMaxCount;
  end;

implementation

uses SysUtils, CastleClassUtils, CastleConfig;

constructor TBaseRecentFiles.Create(AOwner: TComponent);
begin
  inherited;
  FFileNames := TStringList.Create;
  FMaxCount := DefaultMaxCount;

  { one day, make this optional, to also enable many TBaseRecentFiles instances
    in a program not overwriting each others' state. }
  Config.OnLoad.Add(@LoadFromConfig);
  Config.OnSave.Add(@SaveToConfig);
end;

destructor TBaseRecentFiles.Destroy;
begin
  if Config <> nil then
  begin
    Config.OnLoad.Remove(@LoadFromConfig);
    Config.OnSave.Remove(@SaveToConfig);
  end;

  FreeAndNil(FFileNames);
  inherited;
end;

procedure TBaseRecentFiles.MenuCreate;
begin
end;

procedure TBaseRecentFiles.MenuDestroy;
begin
end;

procedure TBaseRecentFiles.Add(const FileName: string; const MaybeStdIn: boolean);
var
  F: string;
  Index: Integer;
begin
  if MaybeStdIn and (Filename = '-') then Exit;

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

  MenuDestroy;
  MenuCreate;
end;

const
  { Should not contain final "/" --- it will be added automatically. }
  Path = 'recent_files';

procedure TBaseRecentFiles.LoadFromConfig(const Config: TCastleConfig);
var
  I, C: Integer;
  S: string;
begin
  FileNames.Clear;
  C := Config.GetValue(Path + '/count', 0);
  for I := 0 to C - 1 do
  begin
    S := Config.GetValue(Path + '/item' + IntToStr(I) + '/filename', '');
    if S <> '' then
      FileNames.Append(S);
  end;

  MenuDestroy;
  MenuCreate;
end;

procedure TBaseRecentFiles.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  Config.SetDeleteValue(Path + '/count', FileNames.Count, 0);
  for I := 0 to FileNames.Count - 1 do
    Config.SetDeleteValue(Path + '/item' + IntToStr(I) + '/filename', FileNames[I], '');
end;

end.
