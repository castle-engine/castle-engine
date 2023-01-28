{%MainUnit castlesoundengine.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type // allow type declaration to continue

  { Parameters to use when playing sound, see @link(TSoundEngine.PlaySound).

    @exclude Deprecated now, as @link(TSoundEngine.PlaySound) is deprecated.
    Playback properties are now in @link(TCastleSound),
    and some more in @link(TCastlePlayingSound). }
  TPlaySoundParameters = class
  strict private
    FPriority: Single;
    function GetImportance: Cardinal;
    procedure SetImportance(const Value: Cardinal);
    procedure SetPriority(const Value: Single);
  public
    Buffer: TInternalSoundBuffer;
    Spatial, Loop: Boolean;

    { How loud is the sound. By default 1.0. }
    Volume: Single;

    { MinGain and MaxGain determine how volume can change because of spatialization
      (where the sound may get quieter / louder as you get further / closer to it).
      By default MinGain is 0.0, MaxGain is 1.0. }
    MinGain: Single platform { this feature is only available with OpenAL sound backend };
    MaxGain: Single platform { this feature is only available with OpenAL sound backend };

    { The position of sound in 3D space.
      Used only if @link(Spatial) = @true. }
    Position: TVector3;
    { Pitch allows to play the sound faster. By default it is 1.0. }
    Pitch: Single;
    { See @link(TCastleSound.ReferenceDistance), by default 1. }
    ReferenceDistance: Single;
    { See @link(TCastleSound.MaxDistance), by default 10000. }
    MaxDistance: Single;
    { Offset is a position in time of the sound. }
    Offset: Single;

    {$ifdef FPC}
    property Gain: Single read Volume write Volume; deprecated 'use Volume';
    property Looping: Boolean read Loop write Loop; deprecated 'use Loop';
    {$endif}

    { See TCastleSound.Priority for range and meaning. }
    property Priority: Single read FPriority write SetPriority {$ifdef FPC}default 0.5{$endif};

    {$ifdef FPC}
    property Importance: Cardinal read GetImportance write SetImportance; deprecated 'use Priority (with priorities as float, in 0..1 range)';
    {$endif}
    constructor Create;
  end;

  TSoundParameters = TPlaySoundParameters deprecated 'use TPlaySoundParameters';

{$endif read_interface}

{$ifdef read_implementation}

{ TPlaySoundParameters ----------------------------------------------------------- }

constructor TPlaySoundParameters.Create;
begin
  inherited;
  FPriority := 0.5;
  Volume := 1;
  {$warnings off} // using unportable symbols knowingly
  MaxGain := 1;
  {$warnings on}
  Pitch := 1;
  ReferenceDistance := TCastleSound.DefaultReferenceDistance;
  MaxDistance       := TCastleSound.DefaultMaxDistance;
end;

function TPlaySoundParameters.GetImportance: Cardinal;
begin
  Result := PriorityToImportance(Priority);
end;

procedure TPlaySoundParameters.SetImportance(const Value: Cardinal);
begin
  Priority := ImportanceToPriority(Value);
end;

procedure TPlaySoundParameters.SetPriority(const Value: Single);
begin
  if not Between(Value, 0.0, 1.0) then
  begin
    WritelnWarning('Sound priority must be within 0..1 range, but is %f', [Value]);
    FPriority := Clamped(Value, 0.0, 1.0);
  end else
    FPriority := Value;
end;

{$endif read_implementation}
