{%MainUnit castlesoundengine.pas}
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

{$ifdef read_interface}

  { Reverb (reverberation): the echoes of the environment, like a room,
    a hall or a cave, added to the spatial sounds
    (@link(TCastleSoundSource) with @link(TCastleSoundSource.Spatial) = @true).
    Non-spatial sounds (like music and user interface sounds) are not affected.

    Use it through @link(TSoundEngine.Reverb), and set @link(Enabled) to @true
    to hear it. Check @link(TSoundEngine.ReverbSupported) to know
    whether the current sound backend can do it.

    The parameters are the ones of the OpenAL EFX "EAX reverb" effect
    (with the same defaults and ranges),
    see the "Effects Extension Guide" from the OpenAL SDK
    and https://openal-soft.org/ .
    When the OpenAL implementation only has the standard EFX reverb,
    the parameters that only the EAX reverb has are ignored
    (this is noted in their descriptions).
    Values outside of the allowed ranges are clamped. }
  TCastleSoundReverb = class(TPersistent)
  strict private
    FParameters: TSoundReverbParameters;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetDensity(const Value: Single);
    procedure SetDiffusion(const Value: Single);
    procedure SetGain(const Value: Single);
    procedure SetGainHF(const Value: Single);
    procedure SetGainLF(const Value: Single);
    procedure SetDecayTime(const Value: Single);
    procedure SetDecayHFRatio(const Value: Single);
    procedure SetDecayLFRatio(const Value: Single);
    procedure SetReflectionsGain(const Value: Single);
    procedure SetReflectionsDelay(const Value: Single);
    procedure SetLateReverbGain(const Value: Single);
    procedure SetLateReverbDelay(const Value: Single);
    procedure SetEchoTime(const Value: Single);
    procedure SetEchoDepth(const Value: Single);
    procedure SetModulationTime(const Value: Single);
    procedure SetModulationDepth(const Value: Single);
    procedure SetAirAbsorptionGainHF(const Value: Single);
    procedure SetHFReference(const Value: Single);
    procedure SetLFReference(const Value: Single);
    procedure SetRoomRolloffFactor(const Value: Single);
    procedure SetDecayHFLimit(const Value: Boolean);
    procedure SetReflectionsPan(const Value: TVector3);
    procedure SetLateReverbPan(const Value: TVector3);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;

    { Set all parameters to the defaults (the EFX reverb defaults,
      a generic medium room). Doesn't change @link(Enabled). }
    procedure SetDefaults;

    { All parameters, for sound backends. }
    property Parameters: TSoundReverbParameters read FParameters;

    { Called when any parameter changes. Used by @link(TSoundEngine). }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Direction (in the listener's coordinates) the early reflections come
    from, its length (0 to 1) is how much they come from that direction. Zero
    (the default) means from all around. EAX reverb only. }
    property ReflectionsPan: TVector3 read FParameters.ReflectionsPan write SetReflectionsPan;

    { Direction (in the listener's coordinates) the late reverberation comes
    from, like ReflectionsPan. EAX reverb only. }
    property LateReverbPan: TVector3 read FParameters.LateReverbPan write SetLateReverbPan;

  published
    { Add the reverb to spatial sounds. @false by default. }
    property Enabled: Boolean read FParameters.Enabled write SetEnabled default false;

    { Modal density of the late reverberation: lower values make it sound more
    "grainy" (with fewer, more distinct reflections), like a small room. From
    0 to 1. Default 1. }
    property Density: Single read FParameters.Density write SetDensity
      {$ifdef FPC}default 1.0{$endif};

    { Echo density of the late reverberation: lower values make it sound more
    like separate echoes. From 0 to 1. Default 1. }
    property Diffusion: Single read FParameters.Diffusion write SetDiffusion
      {$ifdef FPC}default 1.0{$endif};

    { Master volume of the reverb (both early reflections and late
    reverberation). From 0 (no reverb) to 1. Default 0.32. }
    property Gain: Single read FParameters.Gain write SetGain
      {$ifdef FPC}default 0.32{$endif};

    { Attenuation of the reverb at high frequencies (above HFReference): lower
    values make the room sound more "muffled". From 0 to 1. Default 0.89. }
    property GainHF: Single read FParameters.GainHF write SetGainHF
      {$ifdef FPC}default 0.89{$endif};

    { Attenuation of the reverb at low frequencies (below LFReference). From 0
    to 1. Default 1. EAX reverb only. }
    property GainLF: Single read FParameters.GainLF write SetGainLF
      {$ifdef FPC}default 1.0{$endif};

    { Reverberation decay time, in seconds: how long the reverb lasts. Small
    rooms have short times (below 1), large halls and caves long ones. From
    0.1 to 20. Default 1.49. }
    property DecayTime: Single read FParameters.DecayTime write SetDecayTime
      {$ifdef FPC}default 1.49{$endif};

    { Ratio of the decay time at high frequencies to the decay time at middle
    frequencies: below 1 makes high frequencies decay faster (like soft,
    absorbing surfaces). From 0.1 to 2. Default 0.83. }
    property DecayHFRatio: Single read FParameters.DecayHFRatio write SetDecayHFRatio
      {$ifdef FPC}default 0.83{$endif};

    { Ratio of the decay time at low frequencies to the decay time at middle
    frequencies. From 0.1 to 2. Default 1. EAX reverb only. }
    property DecayLFRatio: Single read FParameters.DecayLFRatio write SetDecayLFRatio
      {$ifdef FPC}default 1.0{$endif};

    { Volume of the early reflections (the first, distinct echoes from nearby
    surfaces). From 0 to 3.16. Default 0.05. }
    property ReflectionsGain: Single read FParameters.ReflectionsGain write SetReflectionsGain
      {$ifdef FPC}default 0.05{$endif};

    { Delay from the sound to its first reflection, in seconds. Larger in
    larger rooms. From 0 to 0.3. Default 0.007. }
    property ReflectionsDelay: Single read FParameters.ReflectionsDelay write SetReflectionsDelay
      {$ifdef FPC}default 0.007{$endif};

    { Volume of the late reverberation (the dense tail of the reverb). From 0
    to 10. Default 1.26. }
    property LateReverbGain: Single read FParameters.LateReverbGain write SetLateReverbGain
      {$ifdef FPC}default 1.26{$endif};

    { Delay from the first reflection to the late reverberation, in seconds.
    From 0 to 0.1. Default 0.011. }
    property LateReverbDelay: Single read FParameters.LateReverbDelay write SetLateReverbDelay
      {$ifdef FPC}default 0.011{$endif};

    { Time between the cyclic echoes of the reverb, in seconds. From 0.075 to
    0.25. Default 0.25. EAX reverb only. }
    property EchoTime: Single read FParameters.EchoTime write SetEchoTime
      {$ifdef FPC}default 0.25{$endif};

    { Strength of the cyclic echoes. From 0 (none) to 1. Default 0. EAX reverb
    only. }
    property EchoDepth: Single read FParameters.EchoDepth write SetEchoDepth
      {$ifdef FPC}default 0.0{$endif};

    { Speed of the pitch modulation of the reverb, in seconds. From 0.04 to 4.
    Default 0.25. EAX reverb only. }
    property ModulationTime: Single read FParameters.ModulationTime write SetModulationTime
      {$ifdef FPC}default 0.25{$endif};

    { Amount of the pitch modulation of the reverb (like in a room with moving
    air). From 0 (none) to 1. Default 0. EAX reverb only. }
    property ModulationDepth: Single read FParameters.ModulationDepth write SetModulationDepth
      {$ifdef FPC}default 0.0{$endif};

    { Attenuation of high frequencies by the air, per meter. From 0.892 to 1
    (no absorption). Default 0.994. }
    property AirAbsorptionGainHF: Single read FParameters.AirAbsorptionGainHF write SetAirAbsorptionGainHF
      {$ifdef FPC}default 0.994{$endif};

    { Frequency (in Hz) above which GainHF applies. From 1000 to 20000.
    Default 5000. EAX reverb only. }
    property HFReference: Single read FParameters.HFReference write SetHFReference
      {$ifdef FPC}default 5000.0{$endif};

    { Frequency (in Hz) below which GainLF applies. From 20 to 1000. Default
    250. EAX reverb only. }
    property LFReference: Single read FParameters.LFReference write SetLFReference
      {$ifdef FPC}default 250.0{$endif};

    { How much the reverb gets quieter with the distance of the sound, like
    the distance attenuation of the sound itself (0: the reverb does not
    depend on the distance). From 0 to 10. Default 0. }
    property RoomRolloffFactor: Single read FParameters.RoomRolloffFactor write SetRoomRolloffFactor
      {$ifdef FPC}default 0.0{$endif};

    { Limit the high frequency decay time by the air absorption
    (AirAbsorptionGainHF), as happens in real rooms. Default @true. }
    property DecayHFLimit: Boolean read FParameters.DecayHFLimit write SetDecayHFLimit default true;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSoundReverb ---------------------------------------------------------- }

constructor TCastleSoundReverb.Create;
begin
  inherited;
  FParameters.SetDefaults;
end;

procedure TCastleSoundReverb.Assign(Source: TPersistent);
begin
  if Source is TCastleSoundReverb then
  begin
    FParameters := TCastleSoundReverb(Source).FParameters;
    Changed;
  end else
    inherited;
end;

procedure TCastleSoundReverb.SetDefaults;
var
  WasEnabled: Boolean;
begin
  WasEnabled := FParameters.Enabled;
  FParameters.SetDefaults;
  FParameters.Enabled := WasEnabled;
  Changed;
end;

procedure TCastleSoundReverb.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCastleSoundReverb.SetEnabled(const Value: Boolean);
begin
  if FParameters.Enabled <> Value then
  begin
    FParameters.Enabled := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDensity(const Value: Single);
begin
  if FParameters.Density <> Value then
  begin
    FParameters.Density := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDiffusion(const Value: Single);
begin
  if FParameters.Diffusion <> Value then
  begin
    FParameters.Diffusion := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetGain(const Value: Single);
begin
  if FParameters.Gain <> Value then
  begin
    FParameters.Gain := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetGainHF(const Value: Single);
begin
  if FParameters.GainHF <> Value then
  begin
    FParameters.GainHF := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetGainLF(const Value: Single);
begin
  if FParameters.GainLF <> Value then
  begin
    FParameters.GainLF := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDecayTime(const Value: Single);
begin
  if FParameters.DecayTime <> Value then
  begin
    FParameters.DecayTime := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDecayHFRatio(const Value: Single);
begin
  if FParameters.DecayHFRatio <> Value then
  begin
    FParameters.DecayHFRatio := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDecayLFRatio(const Value: Single);
begin
  if FParameters.DecayLFRatio <> Value then
  begin
    FParameters.DecayLFRatio := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetReflectionsGain(const Value: Single);
begin
  if FParameters.ReflectionsGain <> Value then
  begin
    FParameters.ReflectionsGain := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetReflectionsDelay(const Value: Single);
begin
  if FParameters.ReflectionsDelay <> Value then
  begin
    FParameters.ReflectionsDelay := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetLateReverbGain(const Value: Single);
begin
  if FParameters.LateReverbGain <> Value then
  begin
    FParameters.LateReverbGain := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetLateReverbDelay(const Value: Single);
begin
  if FParameters.LateReverbDelay <> Value then
  begin
    FParameters.LateReverbDelay := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetEchoTime(const Value: Single);
begin
  if FParameters.EchoTime <> Value then
  begin
    FParameters.EchoTime := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetEchoDepth(const Value: Single);
begin
  if FParameters.EchoDepth <> Value then
  begin
    FParameters.EchoDepth := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetModulationTime(const Value: Single);
begin
  if FParameters.ModulationTime <> Value then
  begin
    FParameters.ModulationTime := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetModulationDepth(const Value: Single);
begin
  if FParameters.ModulationDepth <> Value then
  begin
    FParameters.ModulationDepth := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetAirAbsorptionGainHF(const Value: Single);
begin
  if FParameters.AirAbsorptionGainHF <> Value then
  begin
    FParameters.AirAbsorptionGainHF := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetHFReference(const Value: Single);
begin
  if FParameters.HFReference <> Value then
  begin
    FParameters.HFReference := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetLFReference(const Value: Single);
begin
  if FParameters.LFReference <> Value then
  begin
    FParameters.LFReference := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetRoomRolloffFactor(const Value: Single);
begin
  if FParameters.RoomRolloffFactor <> Value then
  begin
    FParameters.RoomRolloffFactor := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetDecayHFLimit(const Value: Boolean);
begin
  if FParameters.DecayHFLimit <> Value then
  begin
    FParameters.DecayHFLimit := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetReflectionsPan(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FParameters.ReflectionsPan, Value) then
  begin
    FParameters.ReflectionsPan := Value;
    Changed;
  end;
end;

procedure TCastleSoundReverb.SetLateReverbPan(const Value: TVector3);
begin
  if not TVector3.PerfectlyEquals(FParameters.LateReverbPan, Value) then
  begin
    FParameters.LateReverbPan := Value;
    Changed;
  end;
end;

{$endif read_implementation}
