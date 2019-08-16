{
  Copyright 2019-2019 Michalis Kamburelis.

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
      TButtonSoundBuffer = class(TCastleButton)
      public
        Buffer: TSoundBuffer;
        constructor Create(const AOwner: TComponent; const SoundFileURL: String); reintroduce;
      end;

      TSoundSourceUiOwner = class(TComponent)
      strict private
        SliderSoundVolume, SliderSoundPitch: TCastleFloatSlider;
        CheckboxLoop: TCastleCheckbox;
        procedure ClickStop(Sender: TObject);
        procedure ChangeSliderSoundVolume(Sender: TObject);
        procedure ChangeSliderSoundPitch(Sender: TObject);
        procedure ChangeCheckboxLoop(Sender: TObject);
      public
        SoundSource: TSound;
        constructor Create(const AOwner: TComponent; const ASoundSource: TSound;
          const UiTemplate: TSerializedComponent;
          const GroupSoundSources: TCastleVerticalGroup); reintroduce;
      end;

      TSoundSourceUiOwnerList = specialize TObjectList<TSoundSourceUiOwner>;

    var
      SoundSourceUiTemplate: TSerializedComponent;
      LabelSoundSources: TCastleLabel;
      GroupSoundBuffers, GroupSoundSources: TCastleVerticalGroup;
      ButtonExit: TCastleButton;
      SoundSourceUiOwners: TSoundSourceUiOwnerList;
    procedure ClickExit(Sender: TObject);
    procedure ClickPlayBuffer(Sender: TObject);
    procedure SoundSourceRelease(Sender: TSound);
  public
    procedure Start; override;
    procedure Stop; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleLog, CastleUIControls, CastleWindow, CastleURIUtils, CastleTimeUtils, CastleSoundBase;

{ TButtonSoundBuffer --------------------------------------------------------- }

constructor TStateMain.TButtonSoundBuffer.Create(const AOwner: TComponent;
  const SoundFileURL: String);
begin
  inherited Create(AOwner);
  Buffer := SoundEngine.LoadBuffer(SoundFileURL
    { Uncomment this (pass slStreaming argument) to allow streaming loading.
      This means that sound file will be loaded partially, on-demand,
      instead of being loaded to memory all at once.

      The upside is much faster initialization (loading of a sound file
      with slStreaming is almost instant, even for large files).

      The downside is a possible additional work at run-time
      (but it's done in a thread and should not matter in normal use-cases).
      Make sure to also uncomment thread support in CastleEngineManifest.xml,
      see the example in CastleEngineManifest.xml in this directory.
    }
    // , slStreaming
  );
  Caption := Format('%s (%f)', [
    // extract last URL component, i.e. just the filename
    URIDisplay(SoundFileURL, true),
    Buffer.Duration
  ]);
  { Note: We could also free the buffer in destructor, by SoundEngine.FreeBuffer.
    In this simple example, there's no need for it, as SoundEngine will free
    all the buffers anyway at application exit. }
end;

{ TSoundSourceUiOwner ---------------------------------------------------------- }

constructor TStateMain.TSoundSourceUiOwner.Create(const AOwner: TComponent;
  const ASoundSource: TSound;
  const UiTemplate: TSerializedComponent;
  const GroupSoundSources: TCastleVerticalGroup);
var
  Ui: TCastleUserInterface;
  LabelSoundName: TCastleLabel;
  ButtonStop: TCastleButton;
begin
  inherited Create(AOwner);
  SoundSource := ASoundSource;

  // use Self as Owner of Ui, so below we just call Self.FindRequiredComponent
  Ui := UiTemplate.UserInterfaceLoad(Self);
  GroupSoundSources.InsertFront(Ui);

  LabelSoundName := FindRequiredComponent('LabelSoundName') as TCastleLabel;
  LabelSoundName.Caption := URIDisplay(SoundSource.Buffer.URL, true);

  ButtonStop := FindRequiredComponent('ButtonStop') as TCastleButton;
  ButtonStop.OnClick := @ClickStop;

  SliderSoundVolume := FindRequiredComponent('SliderSoundVolume') as TCastleFloatSlider;
  SliderSoundVolume.Value := SoundSource.Gain;
  SliderSoundVolume.OnChange := @ChangeSliderSoundVolume;

  SliderSoundPitch := FindRequiredComponent('SliderSoundPitch') as TCastleFloatSlider;
  SliderSoundPitch.Value := SoundSource.Pitch;
  SliderSoundPitch.OnChange := @ChangeSliderSoundPitch;

  CheckboxLoop := FindRequiredComponent('CheckboxLoop') as TCastleCheckbox;
  CheckboxLoop.Checked := SoundSource.Looping;
  CheckboxLoop.OnChange := @ChangeCheckboxLoop;
end;

procedure TStateMain.TSoundSourceUiOwner.ClickStop(Sender: TObject);
begin
  SoundSource.Release; // this will also call SoundSourceRelease
end;

procedure TStateMain.TSoundSourceUiOwner.ChangeSliderSoundVolume(Sender: TObject);
begin
  SoundSource.Gain := SliderSoundVolume.Value;
end;

procedure TStateMain.TSoundSourceUiOwner.ChangeSliderSoundPitch(Sender: TObject);
begin
  SoundSource.Pitch := SliderSoundPitch.Value;
end;

procedure TStateMain.TSoundSourceUiOwner.ChangeCheckboxLoop(Sender: TObject);
begin
  SoundSource.Looping := CheckboxLoop.Checked;
end;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;

  procedure AddSoundBufferButton(const SoundFileURL: String);
  var
    Button: TButtonSoundBuffer;
  begin
    try
      Button := TButtonSoundBuffer.Create(FreeAtStop, SoundFileURL);
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

var
  UiOwner: TComponent;
begin
  inherited;

  SoundSourceUiOwners := TSoundSourceUiOwnerList.Create(false);

  { Load designed user interface }
  InsertUserInterface('castle-data:/state_main.castle-user-interface', FreeAtStop, UiOwner);

  { Find useful components by name }
  LabelSoundSources := UiOwner.FindRequiredComponent('LabelSoundSources') as TCastleLabel;
  GroupSoundBuffers := UiOwner.FindRequiredComponent('GroupSoundBuffers') as TCastleVerticalGroup;
  GroupSoundSources := UiOwner.FindRequiredComponent('GroupSoundSources') as TCastleVerticalGroup;
  ButtonExit := UiOwner.FindRequiredComponent('ButtonExit') as TCastleButton;

  LabelSoundSources.Caption := Format('Currently playing sound sources (max %d):',
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

  SoundSourceUiTemplate := TSerializedComponent.Create('castle-data:/part_sound_source.castle-user-interface');
end;

procedure TStateMain.Stop;

  procedure StopAllSounds;
  begin
    while SoundSourceUiOwners.Count <> 0 do
      // this calls SoundSourceRelease that will remove this item from list
      SoundSourceUiOwners[0].SoundSource.Release;
  end;

begin
  StopAllSounds;
  FreeAndNil(SoundSourceUiOwners);
  FreeAndNil(SoundSourceUiTemplate);
  inherited;
end;

procedure TStateMain.ClickExit(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TStateMain.ClickPlayBuffer(Sender: TObject);
var
  SenderButton: TButtonSoundBuffer;
  SoundSource: TSound;
  SoundSourceUiOwner: TSoundSourceUiOwner;
begin
  inherited;
  SenderButton := Sender as TButtonSoundBuffer;
  SoundSource := SoundEngine.PlaySound(SenderButton.Buffer);

  if SoundSource <> nil then // SoundSource may be nil if we cannot play yet another sound
  begin
    SoundSourceUiOwner := TSoundSourceUiOwner.Create(FreeAtStop, SoundSource,
      SoundSourceUiTemplate, GroupSoundSources);
    SoundSourceUiOwners.Add(SoundSourceUiOwner);
    { It's better to make SoundSourceRelease a method of TStateMain,
      not TSoundSourceUiOwner, because when it occurs the whole instance
      of TSoundSourceUiOwner (along with the UI) should be destroyed. }
    SoundSourceUiOwner.SoundSource.OnRelease := @SoundSourceRelease;
  end;
end;

procedure TStateMain.SoundSourceRelease(Sender: TSound);
var
  SoundSourceUiOwner: TSoundSourceUiOwner;
begin
  for SoundSourceUiOwner in SoundSourceUiOwners do
    if SoundSourceUiOwner.SoundSource = Sender then
    begin
      SoundSourceUiOwners.Remove(SoundSourceUiOwner);
      // This frees TSoundSourceUiOwner, along with UI
      SoundSourceUiOwner.Free;
      Break;
    end;
end;

end.
