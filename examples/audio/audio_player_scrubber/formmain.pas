{
  Copyright 2017-2018 Michalis Kamburelis.

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
    SoundBuffer: TSoundBuffer;
    Sound: TInternalPlayingSound;
    SoundURL: string;
    SoundDuration: TFloatTime;
    procedure SoundRelease(Sender: TInternalPlayingSound);
  public

  end;

var
  MainForm: TMainForm;

implementation

uses CastleUtils, CastleVectors, CastleLCLUtils,
  FormSoundEngineInfo;

{$R *.lfm}

procedure TMainForm.ButtonSoundEngineInformationClick(Sender: TObject);
begin
  SoundEngineInfoForm.Show;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  if Sound <> nil then
    Sound.Release;
end;

procedure TMainForm.CheckBoxLoopChange(Sender: TObject);
begin
  if Sound <> nil then
    Sound.Looping := CheckBoxLoop.Checked;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FileFiltersToDialog(LoadSound_FileFilters, OpenDialogSound);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if Sound <> nil then
  begin
    LabelOffset.Caption := Format('Offset within played sound: %fs', [Sound.Offset]);

    { temporary unassign the TrackOffset.OnChange handler, otherwise
      we would change sound offset in reaction to this, causing slight "popping"
      in the sound. }
    TrackOffset.OnChange := nil;
    TrackOffset.Position := Round(Sound.Offset * 1000);
    TrackOffset.OnChange := @TrackOffsetChange;
  end else
  begin
    LabelOffset.Caption := 'Offset within played sound: Not Playing';
    TrackOffset.Position := 0;
  end;
end;

procedure TMainForm.TrackOffsetChange(Sender: TObject);
begin
  if Sound <> nil then
    Sound.Offset := TrackOffset.Position / 1000;
end;

procedure TMainForm.TrackVolumeChange(Sender: TObject);
begin
  // if the sound is currently playing, adjust the volume
  if Sound <> nil then
    Sound.Gain := TrackVolume.Position / 1000;
end;

procedure TMainForm.SoundRelease(Sender: TInternalPlayingSound);
begin
  // nil the Sound when it's stopped, this is assigned to Sound.OnRelease
  Sound := nil;
end;

procedure TMainForm.ButtonOpenClick(Sender: TObject);
var
  NewDuration: TFloatTime;
begin
  OpenDialogSound.URL := SoundURL;
  if OpenDialogSound.Execute then
  begin
    SoundBuffer := SoundEngine.LoadBuffer(OpenDialogSound.URL, NewDuration);
    { only when LoadBuffer suceeded without exception, change our fields
      to point to the new sound }
    SoundURL := OpenDialogSound.URL;
    SoundDuration := NewDuration;
    LabelSoundInfo.Caption := Format('Sound File:' + NL +
      '%s' + NL +
      'Duration: %f', [SoundURL, SoundDuration]);
    if Sound <> nil then
      Sound.Release;
    Assert(Sound = nil); // SoundRelease should free it above
    TrackOffset.Min := 0;
    TrackOffset.Max := Round(SoundDuration * 1000);
    TrackOffset.Position := 0;
  end;
end;

procedure TMainForm.ButtonPlayClick(Sender: TObject);
var
  InitialVolume: Single;
  Parameters: TPlaySoundParameters;
begin
  if Sound <> nil then
    Sound.Release;
  InitialVolume := TrackVolume.Position / 1000;
  Parameters := TPlaySoundParameters.Create;
  try
    Parameters.Buffer := SoundBuffer;
    Parameters.Looping := CheckBoxLoop.Checked;
    Parameters.Gain := InitialVolume;
    Parameters.Offset := TrackOffset.Position;
    Sound := SoundEngine.PlaySound(Parameters);
  finally FreeAndNil(Parameters) end;
  if Sound <> nil then
    Sound.OnRelease := @SoundRelease;
end;

end.
