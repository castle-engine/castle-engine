{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main form. }
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, CastleSoundEngine, CastleDialogs, CastleTimeUtils;

type
  TMainForm = class(TForm)
    ButtonSoundEngineInformation: TButton;
    ButtonPlay: TButton;
    ButtonOpen: TButton;
    ButtonStop: TButton;
    CheckBoxLoop: TCheckBox;
    LabelOffset: TLabel;
    LabelOffset1: TLabel;
    LabelVolume: TLabel;
    OpenDialogSound: TCastleOpenDialog;
    LabelCaption: TLabel;
    LabelSoundInfo: TLabel;
    Timer1: TTimer;
    TrackOffset: TTrackBar;
    TrackVolume: TTrackBar;
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonSoundEngineInformationClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure CheckBoxLoopChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackOffsetChange(Sender: TObject);
    procedure TrackVolumeChange(Sender: TObject);
  private
    PlayingSound: TCastlePlayingSound;
  public

  end;

var
  MainForm: TMainForm;

implementation

uses CastleUtils, CastleVectors, CastleLCLUtils, CastleURIUtils,
  CastleApplicationProperties,
  FormSoundEngineInfo;

{$R *.lfm}

procedure TMainForm.ButtonSoundEngineInformationClick(Sender: TObject);
begin
  SoundEngineInfoForm.Show;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  PlayingSound.Stop;
end;

procedure TMainForm.CheckBoxLoopChange(Sender: TObject);
begin
  PlayingSound.Loop := CheckBoxLoop.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PlayingSound := TCastlePlayingSound.Create(Self);
  PlayingSound.Sound := TCastleSound.Create(Self);
  FileFiltersToDialog(LoadSound_FileFilters, OpenDialogSound);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  { When application doesn't use any TCastleWindow or TCastleControl,
    this must be called manually to update some CGE state,
    e.g. detect unused sounds. }
  ApplicationProperties._Update;

  if PlayingSound.Playing then
  begin
    LabelOffset.Caption := FormatDot('Offset within played sound: %fs', [PlayingSound.Offset]);

    { Show PlayingSound.Offset in UI.

      Temporary unassign the TrackOffset.OnChange handler, otherwise
      we would change sound offset in reaction to this, causing slight "popping"
      in the sound.

      Note: This is OK even when not playing,
      PlayingSound.Offset is guaranteed to be zero then. }
    TrackOffset.OnChange := nil;
    TrackOffset.Position := Round(PlayingSound.Offset * 1000);
    TrackOffset.OnChange := @TrackOffsetChange;
  end else
  begin
    LabelOffset.Caption := 'Offset within played sound: Not Playing';

    { Do not update TrackOffset.Position in this case,
      let user manipulate TrackOffset to set InitialOffset. }
  end;
end;

procedure TMainForm.TrackOffsetChange(Sender: TObject);
begin
  { Note: This is OK even when not playing,
    setting PlayingSound.Offset is guaranteed to be ignored then. }
  PlayingSound.Offset := TrackOffset.Position / 1000;
end;

procedure TMainForm.TrackVolumeChange(Sender: TObject);
begin
  PlayingSound.Sound.Volume := TrackVolume.Position / 1000;
end;

procedure TMainForm.ButtonOpenClick(Sender: TObject);
begin
  OpenDialogSound.URL := PlayingSound.Sound.URL;
  if OpenDialogSound.Execute then
  begin
    PlayingSound.Stop;
    PlayingSound.Sound.URL := OpenDialogSound.URL;
    LabelSoundInfo.Caption := Format('Sound File:' + NL +
      '%s' + NL +
      'Duration: %f', [
      URIDisplay(PlayingSound.Sound.URL),
      PlayingSound.Sound.Duration
    ]);
    TrackOffset.Min := 0;
    TrackOffset.Max := Round(PlayingSound.Sound.Duration * 1000);
    TrackOffset.Position := 0;
  end;
end;

procedure TMainForm.ButtonPlayClick(Sender: TObject);
begin
  PlayingSound.Stop;
  { start playback from InitialOffset }
  PlayingSound.InitialOffset := TrackOffset.Position / 1000;
  SoundEngine.Play(PlayingSound);
end;

end.
