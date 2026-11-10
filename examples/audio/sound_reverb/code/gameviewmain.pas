{
  Copyright 2026 Popa Marius Adrian.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reverb: the echoes of a room, hall or cave added to spatial sounds,
  using SoundEngine.Reverb (TCastleSoundReverb).

  The design (data/gameviewmain.castle-user-interface) has a ball with
  a TCastleSoundSource. We play a short sound from it every second,
  so that the reverb after it is easy to hear.
  The buttons set SoundEngine.Reverb to a few environments.
  The same sound played as non-spatial (button "Play non-spatial")
  is not affected by the reverb. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport,
  CastleBehaviors, CastleSoundEngine;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelInfo, LabelFps: TCastleLabel;
    SoundSource1: TCastleSoundSource;
    SoundBlip: TCastleSound;
    ButtonOff, ButtonSmallRoom, ButtonBathroom, ButtonHall, ButtonCave,
      ButtonHangar, ButtonNonSpatial: TCastleButton;
  private
    EnvironmentName: String;
    TimeToBlip: Single;
    procedure ClickOff(Sender: TObject);
    procedure ClickSmallRoom(Sender: TObject);
    procedure ClickBathroom(Sender: TObject);
    procedure ClickHall(Sender: TObject);
    procedure ClickCave(Sender: TObject);
    procedure ClickHangar(Sender: TObject);
    procedure ClickNonSpatial(Sender: TObject);
    procedure UpdateInfo;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonOff.OnClick := {$ifdef FPC}@{$endif} ClickOff;
  ButtonSmallRoom.OnClick := {$ifdef FPC}@{$endif} ClickSmallRoom;
  ButtonBathroom.OnClick := {$ifdef FPC}@{$endif} ClickBathroom;
  ButtonHall.OnClick := {$ifdef FPC}@{$endif} ClickHall;
  ButtonCave.OnClick := {$ifdef FPC}@{$endif} ClickCave;
  ButtonHangar.OnClick := {$ifdef FPC}@{$endif} ClickHangar;
  ButtonNonSpatial.OnClick := {$ifdef FPC}@{$endif} ClickNonSpatial;

  { Initialize the sound backend now, to know if it supports reverb. }
  SoundEngine.ContextOpen;
  ClickHall(nil);
end;

{ The environments below are rough approximations, made by ear.
  Each starts from the defaults (TCastleSoundReverb.SetDefaults)
  and changes the most important parameters:
  the decay time (how long the reverb lasts), the volume and delay
  of the early reflections and of the late reverberation,
  and how fast high frequencies decay (DecayHFRatio). }

procedure TViewMain.ClickOff(Sender: TObject);
begin
  SoundEngine.Reverb.Enabled := false;
  EnvironmentName := 'No reverb';
  UpdateInfo;
end;

procedure TViewMain.ClickSmallRoom(Sender: TObject);
var
  R: TCastleSoundReverb;
begin
  R := SoundEngine.Reverb;
  R.SetDefaults;
  R.Density := 0.4;
  R.DecayTime := 0.4;
  R.DecayHFRatio := 0.8;
  R.ReflectionsGain := 0.15;
  R.ReflectionsDelay := 0.002;
  R.LateReverbGain := 1.0;
  R.LateReverbDelay := 0.003;
  R.Enabled := true;
  EnvironmentName := 'Small room';
  UpdateInfo;
end;

procedure TViewMain.ClickBathroom(Sender: TObject);
var
  R: TCastleSoundReverb;
begin
  R := SoundEngine.Reverb;
  R.SetDefaults;
  { hard tiles: strong reflections, slowly decaying high frequencies }
  R.Density := 0.2;
  R.DecayTime := 1.5;
  R.DecayHFRatio := 0.9;
  R.ReflectionsGain := 0.6;
  R.ReflectionsDelay := 0.007;
  R.LateReverbGain := 2.0;
  R.LateReverbDelay := 0.011;
  R.Enabled := true;
  EnvironmentName := 'Bathroom';
  UpdateInfo;
end;

procedure TViewMain.ClickHall(Sender: TObject);
var
  R: TCastleSoundReverb;
begin
  R := SoundEngine.Reverb;
  R.SetDefaults;
  R.DecayTime := 3.0;
  R.DecayHFRatio := 0.6;
  R.ReflectionsGain := 0.25;
  R.ReflectionsDelay := 0.02;
  R.LateReverbGain := 1.2;
  R.LateReverbDelay := 0.03;
  R.Enabled := true;
  EnvironmentName := 'Concert hall';
  UpdateInfo;
end;

procedure TViewMain.ClickCave(Sender: TObject);
var
  R: TCastleSoundReverb;
begin
  R := SoundEngine.Reverb;
  R.SetDefaults;
  { hard rock: high frequencies last as long as the rest }
  R.GainHF := 1.0;
  R.DecayTime := 3.5;
  R.DecayHFRatio := 1.3;
  R.DecayHFLimit := false;
  R.ReflectionsGain := 0.5;
  R.ReflectionsDelay := 0.015;
  R.LateReverbGain := 0.7;
  R.LateReverbDelay := 0.022;
  R.Enabled := true;
  EnvironmentName := 'Cave';
  UpdateInfo;
end;

procedure TViewMain.ClickHangar(Sender: TObject);
var
  R: TCastleSoundReverb;
begin
  R := SoundEngine.Reverb;
  R.SetDefaults;
  R.DecayTime := 10.0;
  R.DecayHFRatio := 0.23;
  R.ReflectionsGain := 0.5;
  R.ReflectionsDelay := 0.02;
  R.LateReverbGain := 1.25;
  R.LateReverbDelay := 0.03;
  R.Enabled := true;
  EnvironmentName := 'Hangar';
  UpdateInfo;
end;

procedure TViewMain.ClickNonSpatial(Sender: TObject);
begin
  { Non-spatial sounds (like UI sounds and music) are not affected by the reverb. }
  SoundEngine.Play(SoundBlip);
end;

procedure TViewMain.UpdateInfo;
var
  R: TCastleSoundReverb;
  Supported: String;
begin
  if not SoundEngine.IsContextOpenSuccess then
    Supported := 'Sound backend not initialized: ' + SoundEngine.InformationSummary
  else
  if SoundEngine.ReverbSupported then
    Supported := 'Reverb supported by the sound backend'
  else
    Supported := 'Reverb NOT supported by this sound backend (e.g. OpenAL without the EFX extension)';
  R := SoundEngine.Reverb;
  LabelInfo.Caption := Format(
    '%s.' + NL +
    'Environment: %s.' + NL +
    'Decay time %f s, reflections gain %f (delay %f s), late reverb gain %f (delay %f s).' + NL +
    'The green ball plays a sound every second (a spatial sound, with reverb).' + NL +
    'Press + / - to change the decay time. Drag with mouse to rotate the view.', [
    Supported,
    EnvironmentName,
    R.DecayTime,
    R.ReflectionsGain,
    R.ReflectionsDelay,
    R.LateReverbGain,
    R.LateReverbDelay
  ]);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  TimeToBlip := TimeToBlip - SecondsPassed;
  if TimeToBlip <= 0 then
  begin
    TimeToBlip := 1.2;
    SoundSource1.Play(SoundBlip);
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyPlus) or Event.IsKey(keyNumpadPlus) then
  begin
    SoundEngine.Reverb.DecayTime := Min(20, SoundEngine.Reverb.DecayTime * 1.5);
    UpdateInfo;
    Exit(true);
  end;

  if Event.IsKey(keyMinus) or Event.IsKey(keyNumpadMinus) then
  begin
    SoundEngine.Reverb.DecayTime := Max(0.1, SoundEngine.Reverb.DecayTime / 1.5);
    UpdateInfo;
    Exit(true);
  end;
end;

end.
