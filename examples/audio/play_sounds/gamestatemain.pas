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

uses Classes,
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
        procedure DoClick; override;
      end;
    var
      SoundSourceUiTemplate: TSerializedComponent;
      LabelSoundSources: TCastleLabel;
      GroupSoundBuffers, GroupSoundSources: TCastleVerticalGroup;
      ButtonExit: TCastleButton;
    procedure ClickExit(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleLog, CastleUIControls, CastleWindow, CastleURIUtils;

{ TButtonSoundBuffer --------------------------------------------------------- }

constructor TStateMain.TButtonSoundBuffer.Create(const AOwner: TComponent; const SoundFileURL: String);
begin
  inherited Create(AOwner);
  Buffer := SoundEngine.LoadBuffer(SoundFileURL);
  // extract last component, i.e. just the filename
  Caption := URIDisplay(SoundFileURL, true);
  { Note: We could also free the buffer in destructor, by SoundEngine.FreeBuffer.
    In this simple example, there's no need for it, as SoundEngine will free
    all the buffers anyway at application exit. }
end;

procedure TStateMain.TButtonSoundBuffer.DoClick;
begin
  inherited;
  SoundEngine.PlaySound(Buffer);
end;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.Start;
var
  UiOwner: TComponent;
begin
  inherited;

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
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/beating_that_thing-22000Hz-16bit-stereo.ogg'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/beating_that_thing-44100Hz-16bit-stereo.ogg'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/misc_sound-22000Hz-8bit-mono.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/misc_sound-44100Hz-8bit-mono.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/negative-44100Hz-8bit-stereo.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/positive-44100Hz-16bit-mono.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/save-44100Hz-16bit-stereo.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/stereo_test.wav'));
  GroupSoundBuffers.InsertFront(TButtonSoundBuffer.Create(FreeAtStop, 'castle-data:/sounds/temple_adam_goh-44000Hz-16bit-mono.ogg'));

  SoundSourceUiTemplate := TSerializedComponent.Create('castle-data:/part_sound_source.castle-user-interface');
end;

procedure TStateMain.ClickExit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
