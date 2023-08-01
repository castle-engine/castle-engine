{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Monitor when files change on disk,
  to enable auto-reloading them at design-time. }
unit CastleInternalFileMonitor;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleClassUtils;

type
  { Monitor when files change on disk,
    to enable auto-reloading them at design-time.
    Use only through singleton @link(FileMonitor).

    When we're not design-mode (CastleDesignMode is @false),
    watching / unwatching does nothing,
    no changes are ever reported, @link(Watch) returns always @false. }
  TCastleFileMonitor = class
  private
    type
      TFileInfo = class
      public
        { URL of the watched file.
          Always an absolute URL.
          For now only file:// URLs are supported.
          The purpose of this is to watch files on regular filesystem,
          files in castle-data:// can be resolved to regular files too on desktop.
          We use URLs (not filenames) just in case in the future more options
          will be possible (e.g. in case CGE editor will run on web some day).}
        Url: String;

        { Last seen modified time of the file.
          0 if cannot be obtained. }
        LastModified: TDateTime;

        { Always non-nil, non-empty instance
          of TNotifyEventList with non-nil and valid callbacks to call.}
        OnChanged: TNotifyEventList;

        constructor Create;
        destructor Destroy; override;

        { Update LastModified value based on the file last modified time.
          Calls OnChanged if the file was modified since last time
          and CallChanged. }
        procedure UpdateLastModified(const CallChanged: Boolean);
      end;

      { Map from URL to the list of events that should be notified when URL
        contents change.
        Key is equal to corresponding TFileInfo.Url.
        Value is never nil. }
      TFiles = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TFileInfo>;
    var
      FFiles: TFiles;

    { Convert to final URL to watch (that can be put in TFileInfo.Url),
      making URL absolute, and not using castle-data://.
      Returns '' if cannot be watched. }
    function UrlToWatch(const Url: String): String;
  public
    constructor Create;
    destructor Destroy; override;

    { Start watching given URL for changes.
      Returns @false if this URL type cannot be watched.
      If it returns @true, make sure to always call @link(Unwatch)
      when you're done with this URL. }
    function Watch(const Url: String; const Notify: TNotifyEvent): Boolean;

    { Stop watching given URL for changes.
      Makes a warning if it was not watched. }
    procedure Unwatch(const Url: String; const Notify: TNotifyEvent);

    { Scan watched files for changes to their last-modified time,
      and base on this possibly fire some notifications.
      Nice to use from CGE editor e.g. when it gets focus (when switching
      to it from other applications). }
    procedure CheckChanges;

    { Fire change notifications for given file.
      Can be called when we @italic(know) given file changed,
      e.g. because it was modified using CGE editor process
      (like using a sprite sheet editor).

      Note: This also updates saved last-modified time,
      so that the file is not detected as changed again by next
      @link(CheckChanges) call. }
    procedure Changed(const Url: String);
  end;

{ Monitor when files change on disk,
  to enable auto-reloading them at design-time. }
function FileMonitor: TCastleFileMonitor;

implementation

uses CastleUriUtils, CastleUtils, CastleLog;

{ TFileInfo -------------------------------------------------------------- }

constructor TCastleFileMonitor.TFileInfo.Create;
begin
  inherited;
  OnChanged := TNotifyEventList.Create;
end;

destructor TCastleFileMonitor.TFileInfo.Destroy;
begin
  FreeAndNil(OnChanged);
  inherited;
end;

procedure TCastleFileMonitor.TFileInfo.UpdateLastModified(const CallChanged: Boolean);
var
  NewLastModified: TDateTime;
begin
  if not FileAge(URIToFilenameSafe(Url), NewLastModified) then
  begin
    WritelnWarning('File Monitor', 'Cannot get last modified time of URL "' + Url + '", it will not be watched reliably');
    NewLastModified := 0;
  end;

  if CallChanged and
     (LastModified <> NewLastModified) then
  begin
    OnChanged.ExecuteAll(Self);
    WritelnLog('File Monitor', 'Notified about change of URL "' + Url + '" because last modification time changed');
  end;

  LastModified := NewLastModified;
end;

{ TCastleFileMonitor ------------------------------------------------------ }

constructor TCastleFileMonitor.Create;
begin
  inherited;
  FFiles := TFiles.Create([doOwnsValues]);
end;

destructor TCastleFileMonitor.Destroy;
begin
  FreeAndNil(FFiles);
  inherited;
end;

function TCastleFileMonitor.UrlToWatch(const Url: String): String;
begin
  if not CastleDesignMode then
    Exit('');

  // resolve castle-data://
  Result := ResolveCastleDataURL(AbsoluteURI(Url));

  // only watch regular files
  if URIToFilenameSafe(Result) = '' then
    Result := '';
end;

function TCastleFileMonitor.Watch(const Url: String; const Notify: TNotifyEvent): Boolean;
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  UrlWatch := UrlToWatch(Url);
  Result := UrlWatch <> '';
  if Result then
  begin
    if not FFiles.TryGetValue(UrlWatch, FileInfo) then
    begin
      FileInfo := TFileInfo.Create;
      FileInfo.Url := UrlWatch;
      FileInfo.UpdateLastModified(false);
      FFiles.Add(UrlWatch, FileInfo);
    end;
    FileInfo.OnChanged.Add(Notify);
  end;
end;

procedure TCastleFileMonitor.Unwatch(const Url: String; const Notify: TNotifyEvent);
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  UrlWatch := UrlToWatch(Url);
  if FFiles.TryGetValue(UrlWatch, FileInfo) then
  begin
    FileInfo.OnChanged.Remove(Notify);
    if FileInfo.OnChanged.Count = 0 then
      FFiles.Remove(UrlWatch);
  end else
    WritelnWarning('File Monitor', 'Cannot unwatch URL "' + Url + '", it was not watched by anything');
end;

procedure TCastleFileMonitor.CheckChanges;
var
  FileInfo: TFileInfo;
begin
  for FileInfo in FFiles.Values do
    FileInfo.UpdateLastModified(true);
end;

procedure TCastleFileMonitor.Changed(const Url: String);
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  UrlWatch := UrlToWatch(Url);
  if FFiles.TryGetValue(UrlWatch, FileInfo) then
  begin
    FileInfo.UpdateLastModified(false);
    FileInfo.OnChanged.ExecuteAll(Self);
    WritelnLog('File Monitor', 'Notified about change of URL "' + Url + '" because changed explicitly by the editor');
  end;
end;

{ global -------------------------------------------------------------------- }

var
  FFileMonitor: TCastleFileMonitor;

function FileMonitor: TCastleFileMonitor;
begin
  if FFileMonitor = nil then
    FFileMonitor := TCastleFileMonitor.Create;
  Result := FFileMonitor;
end;

end.
