{
  Copyright 2019-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main user interface class.
  This implements the majority of this application functionality. }
unit GameStateMain;

interface

uses Classes, Generics.Collections,
  CastleUIState, CastleComponentSerialize, CastleControls, CastleSoundEngine;

type
  { Main user interface class.
    This implements the majority of this application functionality. }
  TStateMain = class(TUIState)
  private
    type
      TButtonSound = class(TCastleButton)
      public
        Sound: TCastleSound;
        constructor Create(const AOwner: TComponent; const SoundFileURL: String); reintroduce;
      end;

      TPlayingSoundUiOwner = class(TComponent)
      strict private
        SliderSoundVolume, SliderSoundPitch: TCastleFloatSlider;
        CheckboxLoop: TCastleCheckbox;
        procedure ClickStop(Sender: TObject);
        procedure ChangeSliderSoundVolume(Sender: TObject);
        procedure ChangeSliderSoundPitch(Sender: TObject);
        procedure ChangeCheckboxLoop(Sender: TObject);
      public
        PlayingSound: TCastlePlayingSound;
        constructor Create(const AOwner: TComponent; const APlayingSound: TCastlePlayingSound;
          const UiTemplate: TSerializedComponent;
          const GroupPlayingSounds: TCastleVerticalGroup); reintroduce;
      end;

      TPlayingSoundUiOwnerList = specialize TObjectList<TPlayingSoundUiOwner>;

    var
      PlayingSoundUiTemplate: TSerializedComponent;
      LabelPlayingSounds: TCastleLabel;
      GroupSoundBuffers, GroupPlayingSounds: TCastleVerticalGroup;
      ButtonExit: TCastleButton;
      PlayingSoundUiOwners: TPlayingSoundUiOwnerList;
    procedure ClickExit(Sender: TObject);
    procedure ClickPlayBuffer(Sender: TObject);
    procedure PlayingSoundStop(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleLog, CastleUIControls, CastleWindow, CastleURIUtils, CastleTimeUtils,
  CastleSoundBase, CastleViewport;

{ TButtonSound --------------------------------------------------------- }

constructor TStateMain.TButtonSound.Create(const AOwner: TComponent;
  const SoundFileURL: String);
begin
  inherited Create(AOwner);
  Sound := TCastleSound.Create(Self);
  { Uncomment this to allow streaming loading.
    This means that sound file will be loaded partially, on-demand,
    instead of being loaded to memory all at once.

    The upside is much faster initialization (loading of a sound file
    with slStreaming is almost instant, even for large files).

    The downside is a possible additional work at run-time
    (but it's done in a thread and should not matter in normal use-cases).
  }
  // Sound.Stream := true;
  Sound.URL := SoundFileURL;

  Caption := Format('%s (%f)', [
    // extract last URL component, i.e. just the filename
    URIDisplay(SoundFileURL, true),
    Sound.Duration
  ]);
end;

{ TPlayingSoundUiOwner ---------------------------------------------------------- }

constructor TStateMain.TPlayingSoundUiOwner.Create(const AOwner: TComponent;
  const APlayingSound: TCastlePlayingSound;
  const UiTemplate: TSerializedComponent;
  const GroupPlayingSounds: TCastleVerticalGroup);
var
  Ui: TCastleUserInterface;
  LabelSoundName: TCastleLabel;
  ButtonStop: TCastleButton;
begin
  inherited Create(AOwner);
  PlayingSound := APlayingSound;

  // use Self as Owner of Ui, so below we just call Self.FindRequiredComponent
  Ui := UiTemplate.UserInterfaceLoad(Self);
  GroupPlayingSounds.InsertFront(Ui);

  LabelSoundName := FindRequiredComponent('LabelSoundName') as TCastleLabel;
  LabelSoundName.Caption := URIDisplay(PlayingSound.Sound.URL, true);

  ButtonStop := FindRequiredComponent('ButtonStop') as TCastleButton;
  ButtonStop.OnClick := @ClickStop;

  SliderSoundVolume := FindRequiredComponent('SliderSoundVolume') as TCastleFloatSlider;
  SliderSoundVolume.Value := PlayingSound.Volume;
  SliderSoundVolume.OnChange := @ChangeSliderSoundVolume;

  SliderSoundPitch := FindRequiredComponent('SliderSoundPitch') as TCastleFloatSlider;
  SliderSoundPitch.Value := PlayingSound.Pitch;
  SliderSoundPitch.OnChange := @ChangeSliderSoundPitch;

  CheckboxLoop := FindRequiredComponent('CheckboxLoop') as TCastleCheckbox;
  CheckboxLoop.Checked := PlayingSound.Loop;
  CheckboxLoop.OnChange := @ChangeCheckboxLoop;
end;

procedure TStateMain.TPlayingSoundUiOwner.ClickStop(Sender: TObject);
begin
  PlayingSound.Stop; // this will also call PlayingSoundStop
end;

procedure TStateMain.TPlayingSoundUiOwner.ChangeSliderSoundVolume(Sender: TObject);
begin
  PlayingSound.Volume := SliderSoundVolume.Value;
end;

procedure TStateMain.TPlayingSoundUiOwner.ChangeSliderSoundPitch(Sender: TObject);
begin
  PlayingSound.Pitch := SliderSoundPitch.Value;
end;

procedure TStateMain.TPlayingSoundUiOwner.ChangeCheckboxLoop(Sender: TObject);
begin
  PlayingSound.Loop := CheckboxLoop.Checked;
end;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  procedure AddSoundBufferButton(const SoundFileURL: String);
  var
    Button: TButtonSound;
  begin
    try
      Button := TButtonSound.Create(FreeAtStop, SoundFileURL);
    except
      on E: Exception do
      begin
        WritelnWarning('Loading of sound file "%s" failed: %s',
          [SoundFileURL, E.Message]);
        Exit;
      end;
    end;
    Button.OnClick := @ClickPlayBuffer;
    GroupSoundBuffers.InsertFront(Button);
  end;

begin
  inherited;

  PlayingSoundUiOwners := TPlayingSoundUiOwnerList.Create(false);

  { Find useful components by name }
  LabelPlayingSounds := DesignedComponent('LabelPlayingSounds') as TCastleLabel;
  GroupSoundBuffers := DesignedComponent('GroupSoundBuffers') as TCastleVerticalGroup;
  GroupPlayingSounds := DesignedComponent('GroupPlayingSounds') as TCastleVerticalGroup;
  ButtonExit := DesignedComponent('ButtonExit') as TCastleButton;

  LabelPlayingSounds.Caption := Format('Currently playing sounds (max %d):',
    [SoundEngine.MaxAllocatedSources]);
  ButtonExit.OnClick := @ClickExit;

  { List the sound files to load.
    Hint: We could also use FindFiles from CastleFindFiles unit to automatically
    scan the directory for files. }
  AddSoundBufferButton('castle-data:/sounds/beating_that_thing-22000Hz-16bit-stereo.ogg');
  AddSoundBufferButton('castle-data:/sounds/beating_that_thing-44100Hz-16bit-stereo.ogg');
  AddSoundBufferButton('castle-data:/sounds/misc_sound-22000Hz-8bit-mono.wav');
  AddSoundBufferButton('castle-data:/sounds/misc_sound-44100Hz-8bit-mono.wav');
  AddSoundBufferButton('castle-data:/sounds/negative-44100Hz-8bit-stereo.wav');
  AddSoundBufferButton('castle-data:/sounds/positive-44100Hz-16bit-mono.wav');
  AddSoundBufferButton('castle-data:/sounds/save-44100Hz-16bit-stereo.wav');
  AddSoundBufferButton('castle-data:/sounds/stereo_test.wav');
  AddSoundBufferButton('castle-data:/sounds/temple_adam_goh-44000Hz-16bit-mono.ogg');

  PlayingSoundUiTemplate := TSerializedComponent.Create('castle-data:/part_playing_sound.castle-user-interface');
end;

procedure TStateMain.Stop;
begin
  FreeAndNil(PlayingSoundUiOwners);
  FreeAndNil(PlayingSoundUiTemplate);
  inherited;
end;

procedure TStateMain.ClickExit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TStateMain.ClickPlayBuffer(Sender: TObject);
var
  SenderButton: TButtonSound;
  PlayingSound: TCastlePlayingSound;
  PlayingSoundUiOwner: TPlayingSoundUiOwner;
begin
  inherited;
  SenderButton := Sender as TButtonSound;

  { Note: by freeing TCastlePlayingSound at state stop (using FreeAtStop)
    we make sure sound stops at state Stop too. }
  PlayingSound := TCastlePlayingSound.Create(FreeAtStop);
  PlayingSound.FreeOnStop := true;
  PlayingSound.Sound := SenderButton.Sound;
  { It's better to make PlayingSoundStop a method of TStateMain,
    not TPlayingSoundUiOwner, because when it occurs the whole instance
    of TPlayingSoundUiOwner (along with the UI) should be destroyed. }
  PlayingSound.OnStop := @PlayingSoundStop;
  SoundEngine.Play(PlayingSound);

  PlayingSoundUiOwner := TPlayingSoundUiOwner.Create(FreeAtStop, PlayingSound,
    PlayingSoundUiTemplate, GroupPlayingSounds);
  PlayingSoundUiOwners.Add(PlayingSoundUiOwner);
end;

procedure TStateMain.PlayingSoundStop(Sender: TObject);
var
  PlayingSoundUiOwner: TPlayingSoundUiOwner;
begin
  { This may happen when TCastlePlayingSound is freed by FreeAtStop,
    when our PlayingSoundUiOwners is freed. So secure from it.
    Testcase: start some longer sound, and then just close application by Alt+F4. }
  if PlayingSoundUiOwners = nil then
    Exit;

  for PlayingSoundUiOwner in PlayingSoundUiOwners do
    if PlayingSoundUiOwner.PlayingSound = Sender then
    begin
      PlayingSoundUiOwners.Remove(PlayingSoundUiOwner);
      // This frees TPlayingSoundUiOwner, along with UI
      PlayingSoundUiOwner.Free;
      Break;
    end;
end;

end.
