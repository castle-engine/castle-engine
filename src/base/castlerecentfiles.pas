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

{ Manage a list of recently open files (TRecentFiles). }
unit CastleRecentFiles;

{$I castleconf.inc}

interface

uses Classes, CastleXMLConfig;

type
  TOnOpenRecent = procedure (const URL: string) of object;

  { Manage a list of recently open files.

    This is designed as a base class, usable on it's own, but also
    as a parent for classes that show this list inside a menu.
    For Lazarus menu version, see TLazRecentFiles.
    For TCastleWindowCustom menu version, see TCastleRecentFiles. }
  TRecentFiles = class(TComponent)
  private
    FURLs: TStringList;
    FMaxCount: Cardinal;
    FOnOpenRecent: TOnOpenRecent;
  protected
    { Create and destroy menu (or anything else that mirrors URLs contents).
      @groupBegin }
    procedure MenuCreate; virtual;
    procedure MenuDestroy; virtual;
    { @groupEnd }
  public
    const
      DefaultMaxCount = 5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds as the most recent file URL.

      If MaybeStdIn, then we treat URL = '-' specially:
      it's ignored. Use this if your program interprets '-' as "load file
      from standard input", such files should not be added to recent files menu.

      Note that we want to store only absolute URLs.
      So this method will always call AbsoluteURI (which will eventually
      call ExpandFileName on filename inside, and make sure it has appropriate
      protocol) on the given URL. }
    procedure Add(const URL: string; const MaybeStdIn: boolean = true); virtual;

    { List of currently stored URLs. @italic(This is readonly.) }
    property URLs: TStringList read FURLs;

    { Load and save recently opened files list to/from the Config file.
      @groupBegin }
    procedure LoadFromConfig(const Config: TCastleConfig);
    procedure SaveToConfig(const Config: TCastleConfig);
    { @groupEnd }
  published
    property OnOpenRecent: TOnOpenRecent read FOnOpenRecent write FOnOpenRecent;

    property MaxCount: Cardinal read FMaxCount write FMaxCount
      default DefaultMaxCount;
  end;

implementation

uses SysUtils, CastleClassUtils, CastleURIUtils;

constructor TRecentFiles.Create(AOwner: TComponent);
begin
  inherited;
  FURLs := TStringList.Create;
  FMaxCount := DefaultMaxCount;

  // automatic loading/saving is more troublesome than it's worth
  // Config.AddLoadListener(@LoadFromConfig);
  // Config.AddSaveListener(@SaveToConfig);
end;

destructor TRecentFiles.Destroy;
begin
  // automatic loading/saving is more troublesome than it's worth
  // if Config <> nil then
  // begin
  //   Config.RemoveLoadListener(@LoadFromConfig);
  //   Config.RemoveSaveListener(@SaveToConfig);
  // end;

  FreeAndNil(FURLs);
  inherited;
end;

procedure TRecentFiles.MenuCreate;
begin
end;

procedure TRecentFiles.MenuDestroy;
begin
end;

procedure TRecentFiles.Add(const URL: string; const MaybeStdIn: boolean);
var
  F: string;
  Index: Integer;
begin
  if MaybeStdIn and (URL = '-') then Exit;

  F := AbsoluteURI(URL);

  { We calculate Index, because if user opens a file already on the "recent files"
    list then we want to just move this URL to the top. }
  Index := URLs.IndexOf(F);

  if Index = 0 then
    Exit { user reopens last opened file, nothing to do };

  if Index <> -1 then
    { Just move Index to the beginning }
    URLs.Exchange(Index, 0) else
  begin
    URLs.Insert(0, F);
    Strings_Trim(URLs, MaxCount);
  end;

  MenuDestroy;
  MenuCreate;
end;

const
  { Should not contain final "/" --- it will be added automatically. }
  Path = 'recent_files';

procedure TRecentFiles.LoadFromConfig(const Config: TCastleConfig);
var
  I, C: Integer;
  S: string;
begin
  URLs.Clear;
  C := Config.GetValue(Path + '/count', 0);
  for I := 0 to C - 1 do
  begin
    S := Config.GetValue(Path + '/item' + IntToStr(I) + '/url', '');
    if S <> '' then
      URLs.Append(S);
  end;

  MenuDestroy;
  MenuCreate;
end;

procedure TRecentFiles.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  Config.SetDeleteValue(Path + '/count', URLs.Count, 0);
  for I := 0 to URLs.Count - 1 do
    Config.SetDeleteValue(Path + '/item' + IntToStr(I) + '/url', URLs[I], '');
end;

end.
