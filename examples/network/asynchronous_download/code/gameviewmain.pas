{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleViewport, CastleUIControls,
  CastleDownload;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonStartDownloads, ButtonAbortDownloads: TCastleButton;
    LabelStatus: TCastleLabel;
  strict private
    const
      DownloadsCount = 3;
    var
      LabelDownload: array [1..DownloadsCount] of TCastleLabel;
      ProgressDownload: array [1..DownloadsCount] of TCastleRectangleControl;
      Download: array [1..DownloadsCount] of TCastleDownload;
    procedure ClickStartDownloads(Sender: TObject);
    procedure ClickAbortDownloads(Sender: TObject);
    procedure DownloadFinish(const Sender: TCastleDownload; var FreeSender: Boolean);
    procedure UpdateDownloadState;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  {$ifdef FPC} {$ifndef VER3_0} OpenSSLSockets, {$endif} {$endif} // https support
  CastleComponentSerialize, CastleUtils, CastleStringUtils, CastleLog,
  CastleURIUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelDownload[1] := DesignedComponent('LabelDownload1') as TCastleLabel;
  LabelDownload[2] := DesignedComponent('LabelDownload2') as TCastleLabel;
  LabelDownload[3] := DesignedComponent('LabelDownload3') as TCastleLabel;
  ProgressDownload[1] := DesignedComponent('ProgressDownload1') as TCastleRectangleControl;
  ProgressDownload[2] := DesignedComponent('ProgressDownload2') as TCastleRectangleControl;
  ProgressDownload[3] := DesignedComponent('ProgressDownload3') as TCastleRectangleControl;

  ButtonStartDownloads.OnClick := {$ifdef FPC}@{$endif} ClickStartDownloads;
  ButtonAbortDownloads.OnClick := {$ifdef FPC}@{$endif} ClickAbortDownloads;

  UpdateDownloadState;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  LabelStatus.Caption := 'FPS: ' + Container.Fps.ToString;
  UpdateDownloadState;
end;

procedure TViewMain.ClickStartDownloads(Sender: TObject);
const
  Urls: array [1..DownloadsCount] of String = (
    'https://castle-engine.io/latest.zip',
    'https://castle-engine.io/modern_pascal_introduction.html',
    'https://en.wikipedia.org/wiki/Main_Page'
    // 'file:///home/michalis/sources/castle-engine/castle-engine/examples/network/asynchronous_download/data/gears.blend',
    // 'castle-data:/gears.gltf',
    // 'https://deliberately-invalid-server.org/deliberately-invalid-url'
    // 'https://castle-engine.io/deliberately-invalid-url',
    // 'http://example.org/',
    // 'https://github.com/castle-engine/castle-engine/'
  );
var
  I: Integer;
begin
  for I := 1 to DownloadsCount do
  begin
    FreeAndNil(Download[I]);
    Download[I] := TCastleDownload.Create(Self);
    Download[I].Url := Urls[I];
    Download[I].OnFinish := {$ifdef FPC}@{$endif} DownloadFinish;

    { Without soForceMemoryStream, returns as soon as possible with
      any stream class. This may give you e.g.:

      - TFileStream for normal files. Which is usually fine for normal reading,
        but note that underlying TFileStream may prevent from keeping
        the same file open multiple times in multiple TFileStream instances.

      - If you would use soGzip, then it may give you TGZFileStream
        which is not "seekable", i.e. you cannot freely move within the stream.

      Using soForceMemoryStream guarantees you get TMemoryStream which is easy
      to handle, always seekable etc.
    }
    //Download[I].Options := [soForceMemoryStream];

    Download[I].Start;
  end;
end;

procedure TViewMain.DownloadFinish(const Sender: TCastleDownload; var FreeSender: Boolean);
var
  HttpResponseHeaders: String;
begin
  { Gracefully handle the case when Sender.HttpResponseHeaders = nil,
    which will happen if you try to download non-HTTP/HTTPS URL,
    like 'castle-data:/gears.gltf' . }
  if Sender.HttpResponseHeaders <> nil then
    HttpResponseHeaders := Sender.HttpResponseHeaders.Text
  else
    HttpResponseHeaders := '';

  if Sender.Status = dsError then
    WritelnLog('Downloading "%s" failed: %s.' + NL +
      'HTTP response code: %d' + NL +
      'HTTP response headers: %s' + NL +
      'Final URL: %s', [
      URIDisplay(Sender.Url),
      Sender.ErrorMessage,
      Sender.HttpResponseCode,
      HttpResponseHeaders,
      URIDisplay(Sender.FinalUrl)
    ])
  else
    WritelnLog('Downloading "%s" successful.' + NL +
      'HTTP response code: %d' + NL +
      'HTTP response headers: %s' + NL +
      'Final URL: %s', [
      URIDisplay(Sender.Url),
      Sender.HttpResponseCode,
      HttpResponseHeaders,
      URIDisplay(Sender.FinalUrl)
    ]);
end;

procedure TViewMain.ClickAbortDownloads(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to DownloadsCount do
    FreeAndNil(Download[I]);
end;

procedure TViewMain.UpdateDownloadState;
const
  StatusToStr: array [TDownloadStatus] of String = (
    { The "Not Started" from here will never be visible, as we call TCastleDownload.Start
      right after creating it. }
    'Not Started',
    'Downloading',
    'Error',
    'Success'
  );
var
  I: Integer;
begin
  for I := 1 to DownloadsCount do
  begin
    if Download[I] = nil then
    begin
      LabelDownload[I].Caption := 'Not started (or aborted)';
      ProgressDownload[I].Exists := false;
    end else
    begin
      LabelDownload[I].Text.Clear;
      LabelDownload[I].Text.Add('Downloading: ' + Download[I].Url);
      LabelDownload[I].Text.Add('Status: ' + StatusToStr[Download[I].Status]);
      if Download[I].Status = dsError then
        LabelDownload[I].Text.AddMultiLine('Error message: ' + Download[I].ErrorMessage)
      else
        // ErrorMessage should only be set if Status is dsError
        Assert(Download[I].ErrorMessage = '');
      LabelDownload[I].Text.Add(Format('Downloaded size: %s / %s (bytes: %d / %d)', [
        SizeToStr(Download[I].DownloadedBytes),
        SizeToStr(Download[I].TotalBytes),
        Download[I].DownloadedBytes,
        Download[I].TotalBytes
      ]));
      LabelDownload[I].Text.Add('MIME type: ' + Download[I].MimeType);

      ProgressDownload[I].Exists := true;
      if Download[I].Status in [dsDownloading, dsSuccess] then
      begin
        ProgressDownload[I].Color := HexToColor('399100E6');

        if Download[I].Status = dsSuccess then
          { Regardless of TotalBytes (which may remain -1 if server never reported them)
            report progress as finished when dsSuccess. }
          ProgressDownload[I].WidthFraction := 1
        else
        if Download[I].TotalBytes > 0 then
          { Note that when WidthFraction = 0, then WidthFraction is ignored,
            and the Width property determines size.
            But ProgressDownload[I].Width is always 0 (set in editor).
            So WidthFraction = 0 reliably hides the bar. }
          ProgressDownload[I].WidthFraction := Download[I].DownloadedBytes / Download[I].TotalBytes
        else
          ProgressDownload[I].WidthFraction := 0;
      end else
      begin
        ProgressDownload[I].Color := HexToColor('FF0000E6');
        ProgressDownload[I].WidthFraction := 1;
      end;
    end;
  end;
end;

end.
