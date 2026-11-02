{%MainUnit castleinternalnodesunsupported.pas}

{ Enumeration types (and their string values) used by X3D nodes in SoundX3D4.txt }

{$ifdef read_interface}

  TChannelCountMode = (
    ccmMax,
    ccmClampedMax,
    ccmExplicit
  );

  TChannelInterpretation = (
    ciSpeakers,
    ciDiscrete
  );

  TDistanceModel = (
    dmLinear,
    dmInverse,
    dmExponential
  );

  TPeriodicWaveType = (
    pwtSine,
    pwtSquare,
    pwtSawtooth,
    pwtTriangle,
    pwtCustom
  );

  TBiquadFilterType = (
    bftLowpass,
    bftHighpass,
    bftBandpass,
    bftLowshelf,
    bftHighshelf,
    bftPeaking,
    bftNotch,
    bftAllpass
  );

  TOversample = (
    osNone,
    os2x,
    os4x
  );

{$endif read_interface}

{$ifdef read_implementation}

const
  ChannelCountModeNames: array [TChannelCountMode] of String =
  ( 'MAX', 'CLAMPED_MAX', 'EXPLICIT' );

  ChannelInterpretationNames: array [TChannelInterpretation] of String =
  ( 'SPEAKERS', 'DISCRETE' );

  DistanceModelNames: array [TDistanceModel] of String =
  ( 'LINEAR', 'INVERSE', 'EXPONENTIAL' );

  PeriodicWaveTypeNames: array [TPeriodicWaveType] of String =
  ( 'SINE', 'SQUARE', 'SAWTOOTH', 'TRIANGLE', 'CUSTOM' );

  BiquadFilterTypeNames: array [TBiquadFilterType] of String =
  ( 'LOWPASS', 'HIGHPASS', 'BANDPASS', 'LOWSHELF', 'HIGHSHELF', 'PEAKING', 'NOTCH', 'ALLPASS' );

  OversampleNames: array [TOversample] of String =
  ( 'NONE', '2X', '4X' );

{$endif read_implementation}
