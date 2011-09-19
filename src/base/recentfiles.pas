{
  Copyright 2006-2011 Michalis Kamburelis.

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

uses Classes, KambiXMLConfig;

const
  DefaultMaxCount = 5;

type
  TOnOpenRecent = procedure (const FileName: string) of object;

  { Manage a list of recently open files.

    This is designed as a base class, usable on it's own, but also
    as a parent for classes that show this list inside a menu.
    For Lazarus menu version, see TKamRecentFiles.
    For TGLWindow menu version, see TGLRecentFiles. }
  TBaseRecentFiles = class(TComponent)
  private
    FFileNames: TStringList;
    FMaxCount: Cardinal;
    FOnOpenRecent: TOnOpenRecent;
  protected
    { Create and destroy menu (or anything else that mirrors FileNames contents).
      @groupBegin }
    procedure MenuCreate; virtual;
    procedure MenuDestroy; virtual;
    { @groupEnd }
  public
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

    { These load and save recently opened files list to/from the TKamXMLConfig file.
      Path should not contain final "/" --- it will be added automatically. }
    procedure LoadFromConfig(ConfigFile: TKamXMLConfig; const Path: string);
    procedure SaveToConfig(ConfigFile: TKamXMLConfig; const Path: string);

    { List of currently stored filenames. @italic(This is readonly.) }
    property FileNames: TStringList read FFileNames;
  published
    property OnOpenRecent: TOnOpenRecent read FOnOpenRecent write FOnOpenRecent;

    property MaxCount: Cardinal read FMaxCount write FMaxCount
      default DefaultMaxCount;
  end;

implementation

uses SysUtils, KambiClassUtils;

constructor TBaseRecentFiles.Create(AOwner: TComponent);
begin
  inherited;
  FFileNames := TStringList.Create;
  FMaxCount := DefaultMaxCount;
end;

destructor TBaseRecentFiles.Destroy;
begin
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

procedure TBaseRecentFiles.LoadFromConfig(ConfigFile: TKamXMLConfig; const Path: string);
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

  MenuDestroy;
  MenuCreate;
end;

procedure TBaseRecentFiles.SaveToConfig(ConfigFile: TKamXMLConfig; const Path: string);
var
  I: Integer;
begin
  ConfigFile.SetDeleteValue(Path + '/count', FileNames.Count, 0);
  for I := 0 to FileNames.Count - 1 do
    ConfigFile.SetDeleteValue(Path + '/item' + IntToStr(I) + '/filename', FileNames[I], '');
end;

end.
