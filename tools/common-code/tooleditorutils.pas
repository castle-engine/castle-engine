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

{ Utils shared by castle-editor and castle-editor-portable. }
unit ToolEditorUtils;

interface

uses SysUtils, Classes, Generics.Collections,
  CastleFindFiles;

type
  TProjectView = class
    { Full URL of the view file. }
    Url: String;
    { Short view name, e.g. 'play'. }
    Name: String;
    { View path inside the project, e.g. 'data/gameviewplay.castle-user-interface'. }
    Path: String;
    { Description of last modification time. }
    LastModified: String;
  end;

  { Scan project for views. }
  TProjectViewList = class({$ifdef FPC}specialize{$endif} TObjectList<TProjectView>)
  private
    ViewUrls: TStringList;
    procedure AddFile(const FileInfo: TFileInfo; var StopSearch: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ScanProject(const ProjectPathUrl: String);
  end;

implementation

uses DateUtils, StrUtils,
  CastleTimeUtils, CastleUtils, CastleUriUtils, CastleStringUtils;

{ Show last file modification time as nice string. }
function FileDateTimeStr(const FileName: String): String;

  function RoundUp(const Val: Double): Int64;
  begin
    Result := Trunc(Val);
    if Frac(Val) > 0 then
      Inc(Result);
  end;

var
  FileDateTime: TDateTime;
  Secs, Mins: Int64;
begin
  if FileAge(FileName, FileDateTime) then
  begin
    Secs := RoundUp(SecondSpan(Now, FileDateTime));
    if Secs < 60 then
      FileDateTimeStr := Format('%d second%s ago', [
        Secs,
        Iff(Secs > 0, 's', '')
      ])
    else
    begin
      Mins := RoundUp(MinuteSpan(Now, FileDateTime));
      if Mins < 60 then
        FileDateTimeStr := Format('%d minute%s ago', [
          Mins,
          Iff(Mins > 0, 's', '')
        ])
      else
        FileDateTimeStr := DateTimeToAtStr(FileDateTime);
    end;
  end else
    Result := 'Unknown';
end;

{ Show last URL modification time as nice string.
  Returns empty string if URL is not a file. }
function UrlDateTimeStr(const Url: String): String;
var
  FileName: String;
begin
  FileName := UriToFilenameSafe(Url);
  if FileName = '' then
    Result := ''
  else
    Result := FileDateTimeStr(FileName);
end;

function ShortDesignName(const S: String): String;
begin
  Result := DeleteUriExt(ExtractUriName(S));
  Result := PrefixRemove('gameview', Result, true);
  Result := PrefixRemove('gamestate', Result, true);
  Result := SuffixRemove('.castle-user-interface', Result, true);
end;

function ExtractRelativeUrl(const BaseUrl, Url: String): String;
var
  BaseFileName, FileName: String;
begin
  { If both BaseUrl and Url are file URLs, then we can use ExtractRelativePath
    to get the relative path. }
  BaseFileName := UriToFilenameSafe(BaseUrl);
  FileName := UriToFilenameSafe(Url);
  if (BaseFileName <> '') and (FileName <> '') then
    Exit(ExtractRelativePath(BaseFileName, FileName));

  { Otherwise, simply use PrefixRemove.
    Don't ignore case, in case it mattered for these URLs. }
  if IsPrefix(BaseUrl, Url, false) then
    Result := PrefixRemove(BaseUrl, Url, false)
  else
    Result := Url;
end;

{ TProjectViewList ----------------------------------------------------------- }

constructor TProjectViewList.Create;
begin
  inherited;
  ViewUrls := TStringList.Create;
end;

destructor TProjectViewList.Destroy;
begin
  FreeAndNil(ViewUrls);
  inherited;
end;

procedure TProjectViewList.AddFile(const FileInfo: TFileInfo; var StopSearch: boolean);
begin
  ViewUrls.Append(FileInfo.Url);
end;

procedure TProjectViewList.ScanProject(const ProjectPathUrl: String);
var
  ViewUrl, ProjectDataUrl: String;
  View: TProjectView;
begin
  { calculate ViewUrls contents }
  ViewUrls.Clear;

  { Search in ProjectDataUrl, not ProjectPathUrl, as all designs should be part of data
    to be possible to open them at runtime.
    This also avoids finding stuff in castle-engine-output, which is possible,
    e.g. after "castle-engine package --target=android" the castle-engine-output contains
    some temporary data with copies of design files -- and we *do not* want to show them here. }
  ProjectDataUrl := CombineUri(ProjectPathUrl, 'data/');
  if UriExists(ProjectDataUrl) <> ueNotExists then
  begin
    FindFiles(ProjectDataUrl, 'gameview*.castle-user-interface', false,
      {$ifdef FPC}@{$endif} AddFile, [ffRecursive]);
    // support deprecated names
    FindFiles(ProjectDataUrl, 'gamestate*.castle-user-interface', false,
      {$ifdef FPC}@{$endif} AddFile, [ffRecursive]);
  end;

  { without sorting, the order would be ~random (as FindFiles enumarates).
    Note that we sort including the subdirectory names, which is good,
    we want files in the same subdirectory to be together. }
  ViewUrls.Sort;

  { copy ViewUrls contents -> Self contents }
  Clear;
  for ViewUrl in ViewUrls do
  begin
    View := TProjectView.Create;
    Add(View);

    View.Url := ViewUrl;
    View.Name := ShortDesignName(ViewUrl);
    View.Path := ExtractRelativeUrl(ProjectPathUrl, ViewUrl);
    View.LastModified := UrlDateTimeStr(ViewUrl);
  end;
end;

end.