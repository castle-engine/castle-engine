{
  Copyright 2023-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Monitor when files change on disk,
  to enable auto-reloading them. }
unit CastleInternalFileMonitor;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Generics.Collections,
  CastleUtils, CastleClassUtils;

type
  { Monitor when files change on disk,
    to enable auto-reloading them.
    Use only through singleton @link(FileMonitor).

    To make the file monitor actually do something, you have to set both
    PossiblyEnabled and Enabled to @true.

    - Before first URL calls Watch, call FileMonitor.MakePossiblyEnabled.

      Right now this is automatically done when DEBUG is defined when
      compiling CGE. Also CGE editor does it.

      This allows to completely avoid any overhead of FileMonitor in release
      version, for users, when data is never observed, because it is not supposed
      to change.

    - Set Enabled to @true anytime you want.
      It can be toggled between @false and @true freely.

      Only when both PossiblyEnabled and Enabled are @true,
      the monitor actually does its work fully.

      When PossiblyEnabled = @true but Enabled = @false,
      the monitor does minimal work (collecting the files to watch)
      to allow to later resume in case you toggle Enabled to @true.
      But otherwise monitor doesn't check file times,
      doesn't generate OnChanged events. }
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
          of TSimpleNotifyEventList with non-nil and valid callbacks to call. }
        OnChanged: TSimpleNotifyEventList;

        { When this is > 0, we are in the middle of iteration over
          OnChanged callbacks. In this case outside code should not modify
          OnChanged count (it can only set some items to @nil)
          or free this instance.

          This prevents undefined behavior in case some OnChanged
          callback unwatches the URL, changing the OnChanged order
          or even freeing TFileInfo class while we're in the middle of iteration
          over OnChanged.

          Testcase: just change image used by TCastleImageControl, once.
          It will then exactly do this: TCastleImagePersistent.ReloadUrl
          temporatily unwatches the URL, freeing TFileInfo, only to watch it again.
          The incorrect log
            Notified about change of URL "" because last modification time...
          with empty URL (which shall not be possible, TFileInfo never has URL='')
          appears, without the protection given by this field.
        }
        InsideExecuteAll: Cardinal;

        constructor Create;
        destructor Destroy; override;

        { Update LastModified value based on the file last modified time.
          Calls OnChanged if the file was modified since last time
          and CallChanged. }
        procedure UpdateLastModified(const CallChanged: Boolean);

        { Execute all OnChanged callbacks, safely (manages InsideExecuteAll
          and later cleanup of @nil OnChanged callbacks). }
        procedure ExecuteAllChanged;
      end;

      { Map from URL to the list of events that should be notified when URL
        contents change.
        Key is equal to corresponding TFileInfo.Url.
        Value is never nil. }
      TFiles = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TFileInfo>;
    var
      FFiles: TFiles;
      FPossiblyEnabledFinalized: Boolean;
      FPossiblyEnabled: Boolean;
      FEnabled: Boolean;

    { Convert to final URL to watch (that can be put in TFileInfo.Url),
      making URL absolute, and not using castle-data://.
      Returns '' if cannot be watched. }
    function UrlToWatch(const Url: String): String;

    { Stop watching given URL for changes.
      Makes a warning if it was not watched.
      This is private -- external code should use
      "class procedure Unwatch". }
    procedure UnwatchCore(const Url: String; const Notify: TSimpleNotifyEvent);

    procedure SetEnabled(const Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    { Start watching given URL for changes.
      Returns @false if this URL type cannot be watched.
      If it returns @true, make sure to always call @link(Unwatch)
      when you're done with this URL. }
    function Watch(const Url: String; const Notify: TSimpleNotifyEvent): Boolean;

    { Stop watching given URL for changes, for singleton @link(FileMonitor).
      Makes a warning if it was not watched.

      This is a class method, and it is hardcoded that it works
      on singleton @link(FileMonitor) in this unit.
      Though this is inconsistent with other methods in this class,
      but it is safer: This way external code is secured in case
      it will call "unwatch" after finalization of this unit is done,
      at which point the @link(FileMonitor) has been freed and shall not be
      recreated.
      This class method handles this internally and gracefully. }
    class procedure Unwatch(const Url: String; const Notify: TSimpleNotifyEvent);

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

    { Set PossiblyEnabled to @true.
      Only possible if no file initialized watching yet. }
    procedure MakePossiblyEnabled;

    { Was MakePossiblyEnabled called. }
    property PossiblyEnabled: Boolean read FPossiblyEnabled;

    { Is monitoring enabled. Both PossiblyEnabled and Enabled must be @true
      to actually monitor files. }
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

type
  { Helper to use TCastleFileMonitor in most typical fashion:
    you have a URL variable, String, and you have a parameter-less method
    that reloads the contents based on this.

    The record has been designed such that when it is filled with all zeroes
    (which happens when class is created that has a field of this type),
    this is also a valid state.
    And calling @link(Finish) on this state is OK. }
  TUrlMonitoring = record
  strict private
    FWatching: Boolean;
    FReloadUrl: TSimpleNotifyEvent;
  public
    procedure Init(const ReloadUrl: TSimpleNotifyEvent);
    procedure Finish(const Url: String);
    procedure ChangeUrl(var Url: String; const NewUrl: String);
  end;

{ Monitor when files change on disk,
  to enable auto-reloading them at design-time. }
function FileMonitor: TCastleFileMonitor;

implementation

uses CastleUriUtils, CastleLog, CastleApplicationProperties;

var
  FFinalizationDone: Boolean;

{ TFileInfo -------------------------------------------------------------- }

constructor TCastleFileMonitor.TFileInfo.Create;
begin
  inherited;
  OnChanged := TSimpleNotifyEventList.Create;
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
  if not FileAge(UriToFilenameSafe(Url), NewLastModified) then
  begin
    WritelnWarning('File Monitor', 'Cannot get last modified time of URL "' + Url + '", it will not be watched reliably');
    NewLastModified := 0;
  end;

  if CallChanged and
     (LastModified <> NewLastModified) then
  begin
    ExecuteAllChanged;
    WritelnLog('File Monitor', 'Notified about change of URL "' + Url + '" because last modification time changed');
  end;

  LastModified := NewLastModified;
end;

procedure TCastleFileMonitor.TFileInfo.ExecuteAllChanged;
begin
  Inc(InsideExecuteAll);
  try
    OnChanged.ExecuteAll();
  finally Dec(InsideExecuteAll) end;

  if InsideExecuteAll = 0 then
    OnChanged.Pack;

  { If OnChanged.Count = 0 we could actually destroy ourselves,
    doing Self.Destroy. That's what happens if someone unwatches the file
    for the last time when InsideExecuteAll = 0.

    But doing so would cause more trouble than benefits:

    - All code calling ExecuteAllChanged would have to be prepared that
      TFileInfo later possibly doesn't exist. E.g. our own UpdateLastModified
      would have to exit after ExecuteAllChanged, not do anything more.

    - And actually, leaving TFileInfo with empty OnChanged means less work.
      Because almost always, when someone unwatches URL during
      OnChanged.ExecuteAll, it means the URL was unwatched only temporarily
      by ReloadUrl.
      It is watched immediately right after.
      So destroying TFileInfo instance only to recreate it later is pointless
      work.

    - And it is harmless. TFileInfo with empty OnChanged works OK.
  }
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
  // resolve castle-data://
  Result := ResolveCastleDataURL(AbsoluteURI(Url));

  // only watch regular files
  if UriToFilenameSafe(Result) = '' then
    Result := '';
end;

function TCastleFileMonitor.Watch(const Url: String; const Notify: TSimpleNotifyEvent): Boolean;
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  FPossiblyEnabledFinalized := true; // cannot change FPossiblyEnabled anymore
  if not FPossiblyEnabled then
    Exit(false); // early exit, we will never watch anything

  UrlWatch := UrlToWatch(Url);
  Result := UrlWatch <> '';
  if Result then
  begin
    if not FFiles.TryGetValue(UrlWatch, FileInfo) then
    begin
      FileInfo := TFileInfo.Create;
      FileInfo.Url := UrlWatch;
      { Read initial LastModified regardless of Enabled, to later not think
        that "everything changed" when setting Enabled = true. }
      FileInfo.UpdateLastModified(false);
      FFiles.Add(UrlWatch, FileInfo);
    end;
    FileInfo.OnChanged.Add(Notify);
  end;
end;

procedure TCastleFileMonitor.UnwatchCore(const Url: String; const Notify: TSimpleNotifyEvent);
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  if not FPossiblyEnabled then
    Exit; // early exit, we will never watch anything

  UrlWatch := UrlToWatch(Url);
  if FFiles.TryGetValue(UrlWatch, FileInfo) then
  begin
    if FileInfo.InsideExecuteAll <> 0 then
    begin
      FileInfo.OnChanged.Unassign(Notify);
      //WritelnLog('Unwatching URL when we iterate inside its own OnChanged');
    end else
    begin
      FileInfo.OnChanged.Remove(Notify);
      if FileInfo.OnChanged.Count = 0 then
        FFiles.Remove(UrlWatch);
    end;
  end else
    WritelnWarning('File Monitor', 'Cannot unwatch URL "' + Url + '", it was not watched by anything');
end;

procedure TCastleFileMonitor.CheckChanges;
var
  FileInfo: TFileInfo;
begin
  if not (PossiblyEnabled and Enabled) then
    Exit;

  for FileInfo in FFiles.Values do
    FileInfo.UpdateLastModified(true);
end;

procedure TCastleFileMonitor.Changed(const Url: String);
var
  UrlWatch: String;
  FileInfo: TFileInfo;
begin
  if not (PossiblyEnabled and Enabled) then
    Exit;

  UrlWatch := UrlToWatch(Url);
  if FFiles.TryGetValue(UrlWatch, FileInfo) then
  begin
    FileInfo.UpdateLastModified(false);
    FileInfo.ExecuteAllChanged;
    WritelnLog('File Monitor', 'Notified about change of URL "' + Url + '" because changed explicitly by the editor');
  end;
end;

class procedure TCastleFileMonitor.Unwatch(
  const Url: String; const Notify: TSimpleNotifyEvent);
begin
  if FFinalizationDone then
  begin
    { Note that we haven't actually experienced this situation in the real code
      of editor (though at one point we thought we do, but it was a different problem).
      But this code and log remains, in case it will appear one day. }
    WritelnLog('File Monitor', 'Unwatching URL "' + Url + '" after finalization of this unit is done, this is normal -> ignoring');
  end else
  begin
    FileMonitor.UnwatchCore(Url, Notify);
  end;
end;

{ TUrlMonitoring ---------------------------------------------------------- }

procedure TUrlMonitoring.Init(const ReloadUrl: TSimpleNotifyEvent);
begin
  FWatching := false;
  FReloadUrl := ReloadUrl;
end;

procedure TUrlMonitoring.Finish(const Url: String);
begin
  if FWatching then
  begin
    TCastleFileMonitor.Unwatch(Url, FReloadUrl);
    FWatching := false;
  end;
end;

procedure TUrlMonitoring.ChangeUrl(var Url: String; const NewUrl: String);
begin
  if Url <> NewUrl then
  begin
    Assert(Assigned(FReloadUrl), 'FReloadUrl must be assigned when ChangeUrl is called; this means you have to use TUrlMonitoring.Init before ChangeUrl');

    { Unwatch previous URL, if any was watched }
    Finish(Url);

    Url := NewUrl;

    { Watch new URL.
      Ignore Url = '', it conventionally means "load nothing" in CGE components. }
    if Url <> '' then
      FWatching := FileMonitor.Watch(Url, FReloadUrl);
  end;
end;

procedure TCastleFileMonitor.MakePossiblyEnabled;
begin
  if FPossiblyEnabledFinalized and (not FPossiblyEnabled) then
    raise Exception.Create('Too late to call FileMonitor.MakePossiblyEnabled, some file already called Watch (and we optimized it out, assuming FileMonitor.PossiblyEnabled = false).');
  FPossiblyEnabled := true;
end;

procedure TCastleFileMonitor.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
      { We didn't call UpdateLastModified when Eaabled = false.
        So now we can update them, updating LastModified, and calling OnChanged
        if anything changed. }
      CheckChanges;
  end;
end;

{ global -------------------------------------------------------------------- }

var
  FFileMonitor: TCastleFileMonitor;

function FileMonitor: TCastleFileMonitor;
begin
  if FFinalizationDone then
  begin
    WritelnWarning('Accessing FileMonitor singleton after finalization of this unit is done.' +
      ' We create a new instance, empty, that will leak.' +
      ' You should avoid this situation, following the pattern of class method "TCastleFileMonitor.Unwatch".');
  end;
  if FFileMonitor = nil then
    FFileMonitor := TCastleFileMonitor.Create;
  Result := FFileMonitor;
end;

procedure InitializeFileMonitorDebug;
begin
  FileMonitor.MakePossiblyEnabled;
end;

initialization
  ApplicationProperties.AddInitializeDebugListener(
    {$ifdef FPC}@{$endif} InitializeFileMonitorDebug);
  { TODO: The InitializeFileMonitorDebug is called from here only
    temporarily (in case user didn't update the project to have new
    CastleAutoGenerated that calls ApplicationProperties.InitializeDebug,
    but expects to have file monitor working). }
  {$ifdef DEBUG}
  InitializeFileMonitorDebug;
  {$endif}
finalization
  FreeAndNil(FFileMonitor);
  FFinalizationDone := true;
end.
