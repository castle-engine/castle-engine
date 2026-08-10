{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View showing current song, and allowing to play it (@link(TViewPlaySong)). }
unit GameViewPlaySong;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleSoundEngine,
  GameSongs;

type
  { View showing current song, and allowing to play it. }
  TViewPlaySong = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    SongArt: TCastleImageControl;
    ButtonPlayPause: TCastleButton;
    ImagePlay: TCastleImageControl;
    ImagePause: TCastleImageControl;
    LabelOffset: TCastleLabel;
    SliderOffset: TCastleFloatSlider;
    LabelArtist, LabelTitle: TCastleLabel;
    SoundSong: TCastleSound;
    ButtonBack: TCastleButton;
    GroupNeedsSafeBottom: TCastleUserInterface;
    ButtonLoop: TCastleButton;
  private
    const
      DefaultLoop = false;
    var
      SoundPlaying: TCastlePlayingSound;
      { We maintain offset ourselves, to keep even even for stopped sounds. }
      Offset: Single;
      SongDurationKnown: Boolean;
    procedure SoundPlayingStopped(Sender: TObject);
    procedure ClickPlayPause(Sender: TObject);
    procedure ClickBack(Sender: TObject);
    procedure SliderOffsetChange(Sender: TObject);
    procedure ClickLoop(Sender: TObject);
    procedure SetButtonPlayPauseIcon(const PlayingNow: Boolean);
  public
    { Assign before starting. }
    Song: TSong;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewPlaySong: TViewPlaySong;

implementation

uses SysUtils,
  CastleLog,
  GameViewChooseSong;

constructor TViewPlaySong.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplaysong.castle-user-interface';
end;

procedure TViewPlaySong.Start;
begin
  inherited;
  Assert(Song <> nil, 'TViewPlaySong.Start: Song must be assigned before starting the view');

  // initialize basic UI
  SongArt.Url := Song.UrlArt;
  LabelArtist.Caption := Song.Artist;
  LabelTitle.Caption := Song.Title;
  SoundSong.Url := Song.UrlMusic;
  ButtonLoop.Pressed := DefaultLoop;

  { Offset related UI.

    Note about SongDurationKnown:
    - when using WebAudio (on web platform), the Duration only becomes known
      after sound is asynchronously loaded.
    - on other backends, Duration is known immediately after assigning SoundSong.Url. }
  SongDurationKnown := SoundSong.Duration > 0;
  if SongDurationKnown then
    SliderOffset.Max := SoundSong.Duration;
  Offset := 0;
  SliderOffset.Value := Offset;
  SliderOffset.OnChange := {$ifdef FPC}@{$endif} SliderOffsetChange;

  // buttons
  ButtonBack.OnClick := {$ifdef FPC}@{$endif} ClickBack;
  ButtonPlayPause.OnClick := {$ifdef FPC}@{$endif} ClickPlayPause;
  ButtonLoop.OnClick := {$ifdef FPC}@{$endif} ClickLoop;

  // start playing
  SoundPlaying := TCastlePlayingSound.Create(FreeAtStop);
  SoundPlaying.Sound := SoundSong;
  SoundPlaying.OnStop := {$ifdef FPC}@{$endif} SoundPlayingStopped;
  SoundPlaying.Loop := ButtonLoop.Pressed;
  SoundEngine.Play(SoundPlaying);

  // show we're playing now
  SetButtonPlayPauseIcon(true);
end;

procedure TViewPlaySong.Stop;
begin
  { All designed components (like SoundSong and UI like ImagePlay)
    and SoundPlaying (owned by FreeAtStop) are going to be freed soon.
    So do not let SoundPlaying.OnStop update any UI, in case UI would be freed
    earlier.
    One idea was to unassign SoundPlaying.OnStop.
    But we cannot do this while playing... so actually we just need to stop
    *now* to be safe.
    Afterwards, everything (SoundSong, all UI) can be safely freed in any order. }
  if SoundPlaying <> nil then
  begin
    SoundPlaying.Stop;
    SoundPlaying.OnStop := nil;
  end;
  SongDurationKnown := false;
  inherited;
end;

procedure TViewPlaySong.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { On WebAudio, SoundSong.Duration becomes known with a small delay
    after assigning SoundSong.Url. }
  if not SongDurationKnown and (SoundSong.Duration > 0) then
  begin
    SongDurationKnown := true;
    SliderOffset.Max := SoundSong.Duration;
  end;
  if SoundPlaying.Playing then
  begin
    { keep saving Offset from SoundPlaying, to be able to resume playback
      from that point after pausing. }
    Offset := SoundPlaying.Offset;
    { also show current offset in UI. }
    SliderOffset.Value := Offset;
    LabelOffset.Caption := Format('%d:%.2d / %d:%.2d', [
      Trunc(Offset) div 60,
      Trunc(Offset) mod 60,
      Trunc(SoundSong.Duration) div 60,
      Trunc(SoundSong.Duration) mod 60
    ]);
  end;
  // make sure that buttons are not overlapping with unsafe bottom area
  GroupNeedsSafeBottom.Border.Assign(Container.SafeBorder);
end;

procedure TViewPlaySong.SoundPlayingStopped(Sender: TObject);
begin
  { Note: Do not update "Offset := SoundPlaying.Offset" now,
    as offset is not valid at this point (always 0, as sound is stopped).
    We have to count on updating Offset in Update. }

  SetButtonPlayPauseIcon(false);
end;

procedure TViewPlaySong.ClickPlayPause(Sender: TObject);
begin
  if SoundPlaying.Playing then
  begin
    SoundPlaying.Stop;
    { The above line will call SoundPlaying.OnStop, which is SoundPlayingStopped,
      and this will update the play/pause icon. }
  end else
  begin
    { If sound stopped because it reached the end, then start from
      the beginning. }
    if Abs(Offset - SoundSong.Duration) < 0.01 then
      Offset := 0;
    SoundPlaying.InitialOffset := Offset;

    SoundEngine.Play(SoundPlaying);
    SetButtonPlayPauseIcon(true);
  end;
end;

procedure TViewPlaySong.SliderOffsetChange(Sender: TObject);
begin
  Offset := SliderOffset.Value;
  if SoundPlaying.Playing then
    SoundPlaying.Offset := Offset;
end;

procedure TViewPlaySong.ClickBack(Sender: TObject);
begin
  Container.View := ViewChooseSong;
end;

procedure TViewPlaySong.ClickLoop(Sender: TObject);
begin
  ButtonLoop.Pressed := not ButtonLoop.Pressed;
  SoundPlaying.Loop := ButtonLoop.Pressed;
end;

procedure TViewPlaySong.SetButtonPlayPauseIcon(const PlayingNow: Boolean);
begin
  ImagePause.Exists := PlayingNow;
  ImagePlay.Exists := not PlayingNow;
end;

end.
