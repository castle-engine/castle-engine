{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses CastleUIState, CastleScene, CastleControls,
  CastleKeysMouse, CastleColors, CastleViewport, CastleUIControls,
  CastleDownload;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  strict private
    const
      DownloadsCount = 3;
    var
      { Components designed using CGE editor, loaded from state_main.castle-user-interface. }
      LabelDownload: array [1..DownloadsCount] of TCastleLabel;
      ProgressDownload: array [1..DownloadsCount] of TCastleUserInterface;
      ButtonStartDownloads, ButtonAbortDownloads: TCastleButton;
      LabelStatus: TCastleLabel;

      Download: array [1..DownloadsCount] of TCastleDownload;
    procedure ClickStartDownloads(Sender: TObject);
    procedure ClickAbortDownloads(Sender: TObject);
    procedure UpdateLabels;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils, Classes, Math,
  {$ifndef VER3_0} OpenSSLSockets, {$endif} // https support
  CastleComponentSerialize, CastleUtils;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find components, by name, that we need to access from code }
  ButtonStartDownloads := UiOwner.FindRequiredComponent('ButtonStartDownloads') as TCastleButton;
  ButtonAbortDownloads := UiOwner.FindRequiredComponent('ButtonAbortDownloads') as TCastleButton;
  LabelDownload[1] := UiOwner.FindRequiredComponent('LabelDownload1') as TCastleLabel;
  LabelDownload[2] := UiOwner.FindRequiredComponent('LabelDownload2') as TCastleLabel;
  LabelDownload[3] := UiOwner.FindRequiredComponent('LabelDownload3') as TCastleLabel;
  ProgressDownload[1] := UiOwner.FindRequiredComponent('ProgressDownload1') as TCastleUserInterface;
  ProgressDownload[2] := UiOwner.FindRequiredComponent('ProgressDownload2') as TCastleUserInterface;
  ProgressDownload[3] := UiOwner.FindRequiredComponent('ProgressDownload3') as TCastleUserInterface;
  LabelStatus := UiOwner.FindRequiredComponent('LabelStatus') as TCastleLabel;

  ButtonStartDownloads.OnClick := @ClickStartDownloads;
  ButtonAbortDownloads.OnClick := @ClickAbortDownloads;

  EnableNetwork := true;

  UpdateLabels;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  LabelStatus.Caption := 'FPS: ' + Container.Fps.ToString;
  UpdateLabels;
end;

procedure TStateMain.ClickStartDownloads(Sender: TObject);
const
  Urls: array [1..DownloadsCount] of String = (
    'https://castle-engine.io/latest.zip',
    'https://castle-engine.io/modern_pascal_introduction.html',
    'https://en.wikipedia.org/wiki/Main_Page'
//    'https://github.com/castle-engine/castle-engine/'
  );
var
  I: Integer;
begin
  for I := 1 to DownloadsCount do
  begin
    FreeAndNil(Download[I]);
    Download[I] := TCastleDownload.Create(Self);
    Download[I].Url := Urls[I];
    Download[I].Start;
  end;
end;

procedure TStateMain.ClickAbortDownloads(Sender: TObject);
var
  I: Integer;
begin
  for I := 1 to DownloadsCount do
    FreeAndNil(Download[I]);
end;

procedure TStateMain.UpdateLabels;
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
      LabelDownload[I].Caption := Format(
        'Downloading: %s' + NL +
        'Status: %s' + NL +
        'Error: %s' + NL +
        'Bytes: %d / %d' + NL +
        'MIME type: %s', [
        Download[I].Url,
        StatusToStr[Download[I].Status],
        Download[I].ErrorMessage,
        Download[I].DownloadedBytes,
        Download[I].TotalBytes,
        Download[I].MimeType
      ]);
      ProgressDownload[I].Exists := true;
      { Note that WidthFraction = 0 is ignored, we use Width then.
        But ProgressDownload[I].Width is always 0, so it reliably hides the bar. }
      ProgressDownload[I].WidthFraction := Download[I].DownloadedBytes / Download[I].TotalBytes;
    end;
  end;
end;

end.
