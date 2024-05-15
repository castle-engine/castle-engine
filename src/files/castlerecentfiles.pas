{
  Copyright 2006-2018 Michalis Kamburelis.

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
  TOnOpenRecent = procedure (const Url: String) of object;

  { Manage a list of recently open files.

    This is designed as a base class, usable on it's own, but also
    as a parent for classes that show this list inside a menu.
    For Lazarus menu version, see TLazRecentFiles.
    For TCastleWindow menu version, see TCastleRecentFiles. }
  TRecentFiles = class(TComponent)
  private
    FUrls: TStringList;
    FMaxCount: Cardinal;
    FOnOpenRecent: TOnOpenRecent;
  protected
    { Create and destroy menu (or anything else that mirrors Urls contents).
      @groupBegin }
    procedure MenuCreate; virtual;
    procedure MenuDestroy; virtual;
    { @groupEnd }
  public
    const
      DefaultMaxCount = 5;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Adds as the most recent file Url.

      Note that we want to store only absolute URLs.
      So this method will always call AbsoluteURI (which will eventually
      call ExpandFileName on filename inside, and make sure it has appropriate
      protocol) on the given URL. }
    procedure Add(const Url: String); overload; virtual;

    procedure Add(const Url: String; const Ignored: Boolean); overload; deprecated 'use Add(Url)';

    { Remove URL. }
    procedure Remove(const Url: String);

    { List of currently stored URLs. @italic(This is readonly.) }
    property Urls: TStringList read FUrls;

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

uses SysUtils, CastleClassUtils, CastleUriUtils;

constructor TRecentFiles.Create(AOwner: TComponent);
begin
  inherited;
  FUrls := TStringList.Create;
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

  FreeAndNil(FUrls);
  inherited;
end;

procedure TRecentFiles.MenuCreate;
begin
end;

procedure TRecentFiles.MenuDestroy;
begin
end;

procedure TRecentFiles.Add(const Url: String);
var
  F: string;
  Index: Integer;
begin
  F := AbsoluteURI(Url);

  { We calculate Index, because if user opens a file already on the "recent files"
    list then we want to just move this URL to the top. }
  Index := Urls.IndexOf(F);

  if Index = 0 then
    Exit { user reopens last opened file, nothing to do };

  if Index <> -1 then
  begin
    { Just move Index to the beginning }
    Urls.Delete(Index);
    Urls.Insert(0, F);
  end else
  begin
    Urls.Insert(0, F);
    Strings_Trim(Urls, MaxCount);
  end;

  MenuDestroy;
  MenuCreate;
end;

procedure TRecentFiles.Add(const Url: String; const Ignored: Boolean);
begin
  Add(Url);
end;

procedure TRecentFiles.Remove(const Url: String);
var
  F: string;
  Index: Integer;
begin
  F := AbsoluteURI(Url);

  { We calculate Index, because if user opens a file already on the "recent files"
    list then we want to just move this URL to the top. }
  Index := Urls.IndexOf(F);

  if Index <> -1 then
  begin
    Urls.Delete(Index);
    MenuDestroy;
    MenuCreate;
  end;
end;

const
  { Should not contain final "/" --- it will be added automatically. }
  Path = 'recent_files';

procedure TRecentFiles.LoadFromConfig(const Config: TCastleConfig);
var
  I, C: Integer;
  S: string;
begin
  Urls.Clear;
  C := Config.GetValue(Path + '/count', 0);
  for I := 0 to C - 1 do
  begin
    S := Config.GetValue(Path + '/item' + IntToStr(I) + '/url', '');
    if S <> '' then
      Urls.Append(S);
  end;

  MenuDestroy;
  MenuCreate;
end;

procedure TRecentFiles.SaveToConfig(const Config: TCastleConfig);
var
  I: Integer;
begin
  Config.SetDeleteValue(Path + '/count', Urls.Count, 0);
  for I := 0 to Urls.Count - 1 do
    Config.SetDeleteValue(Path + '/item' + IntToStr(I) + '/url', Urls[I], '');
end;

end.
