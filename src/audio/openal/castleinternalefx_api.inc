{%MainUnit castleinternalefx.pas}

{ This contains translation of C header eax.h. }

const
  { }
  ALC_EXT_EFX_NAME = 'ALC_EXT_EFX';

{*
 * Context definitions to be used with alcCreateContext.
 * These values must be unique and not conflict with other
 * al context values.
 } { }
  ALC_EFX_MAJOR_VERSION                               = $20001;
  ALC_EFX_MINOR_VERSION                               = $20002;
  ALC_MAX_AUXILIARY_SENDS                             = $20003;

{*
 * Listener definitions to be used with alListener functions.
 * These values must be unique and not conflict with other
 * al listener values.
 } { }
  AL_METERS_PER_UNIT                                  = $20004;

{*
 * Source definitions to be used with alSource functions.
 * These values must be unique and not conflict with other
 * al source values.
 } { }
  AL_DIRECT_FILTER                                    = $20005;
  AL_AUXILIARY_SEND_FILTER                            = $20006;
  AL_AIR_ABSORPTION_FACTOR                            = $20007;
  AL_ROOM_ROLLOFF_FACTOR                              = $20008;
  AL_CONE_OUTER_GAINHF                                = $20009;
  AL_DIRECT_FILTER_GAINHF_AUTO                        = $2000A;
  AL_AUXILIARY_SEND_FILTER_GAIN_AUTO                  = $2000B;
  AL_AUXILIARY_SEND_FILTER_GAINHF_AUTO                = $2000C;

{*
 * Effect object definitions to be used with alEffect functions.
 *
 * Effect parameter value definitions, ranges, and defaults
 * appear farther down in this file.
 } { }

{ Reverb Parameters } { }
  AL_REVERB_DENSITY                                   = $0001;
  AL_REVERB_DIFFUSION                                 = $0002;
  AL_REVERB_GAIN                                      = $0003;
  AL_REVERB_GAINHF                                    = $0004;
  AL_REVERB_DECAY_TIME                                = $0005;
  AL_REVERB_DECAY_HFRATIO                             = $0006;
  AL_REVERB_REFLECTIONS_GAIN                          = $0007;
  AL_REVERB_REFLECTIONS_DELAY                         = $0008;
  AL_REVERB_LATE_REVERB_GAIN                          = $0009;
  AL_REVERB_LATE_REVERB_DELAY                         = $000A;
  AL_REVERB_AIR_ABSORPTION_GAINHF                     = $000B;
  AL_REVERB_ROOM_ROLLOFF_FACTOR                       = $000C;
  AL_REVERB_DECAY_HFLIMIT                             = $000D;

{ Chorus Parameters } { }
  AL_CHORUS_WAVEFORM                                  = $0001;
  AL_CHORUS_PHASE                                     = $0002;
  AL_CHORUS_RATE                                      = $0003;
  AL_CHORUS_DEPTH                                     = $0004;
  AL_CHORUS_FEEDBACK                                  = $0005;
  AL_CHORUS_DELAY                                     = $0006;

{ Distortion Parameters } { }
  AL_DISTORTION_EDGE                                  = $0001;
  AL_DISTORTION_GAIN                                  = $0002;
  AL_DISTORTION_LOWPASS_CUTOFF                        = $0003;
  AL_DISTORTION_EQCENTER                              = $0004;
  AL_DISTORTION_EQBANDWIDTH                           = $0005;

{ Echo Parameters } { }
  AL_ECHO_DELAY                                       = $0001;
  AL_ECHO_LRDELAY                                     = $0002;
  AL_ECHO_DAMPING                                     = $0003;
  AL_ECHO_FEEDBACK                                    = $0004;
  AL_ECHO_SPREAD                                      = $0005;

{ Flanger Parameters } { }
  AL_FLANGER_WAVEFORM                                 = $0001;
  AL_FLANGER_PHASE                                    = $0002;
  AL_FLANGER_RATE                                     = $0003;
  AL_FLANGER_DEPTH                                    = $0004;
  AL_FLANGER_FEEDBACK                                 = $0005;
  AL_FLANGER_DELAY                                    = $0006;

{ Frequencyshifter Parameters } { }
  AL_FREQUENCY_SHIFTER_FREQUENCY                      = $0001;
  AL_FREQUENCY_SHIFTER_LEFT_DIRECTION                 = $0002;
  AL_FREQUENCY_SHIFTER_RIGHT_DIRECTION                = $0003;

{ Vocalmorpher Parameters } { }
  AL_VOCAL_MORPHER_PHONEMEA                           = $0001;
  AL_VOCAL_MORPHER_PHONEMEA_COARSE_TUNING             = $0002;
  AL_VOCAL_MORPHER_PHONEMEB                           = $0003;
  AL_VOCAL_MORPHER_PHONEMEB_COARSE_TUNING             = $0004;
  AL_VOCAL_MORPHER_WAVEFORM                           = $0005;
  AL_VOCAL_MORPHER_RATE                               = $0006;

{ Pitchshifter Parameters } { }
  AL_PITCH_SHIFTER_COARSE_TUNE                        = $0001;
  AL_PITCH_SHIFTER_FINE_TUNE                          = $0002;

{ Ringmodulator Parameters } { }
  AL_RING_MODULATOR_FREQUENCY                         = $0001;
  AL_RING_MODULATOR_HIGHPASS_CUTOFF                   = $0002;
  AL_RING_MODULATOR_WAVEFORM                          = $0003;

{ Autowah Parameters } { }
  AL_AUTOWAH_ATTACK_TIME                              = $0001;
  AL_AUTOWAH_RELEASE_TIME                             = $0002;
  AL_AUTOWAH_RESONANCE                                = $0003;
  AL_AUTOWAH_PEAK_GAIN                                = $0004;

{ Compressor Parameters } { }
  AL_COMPRESSOR_ONOFF                                 = $0001;

{ Equalizer Parameters } { }
  AL_EQUALIZER_LOW_GAIN                               = $0001;
  AL_EQUALIZER_LOW_CUTOFF                             = $0002;
  AL_EQUALIZER_MID1_GAIN                              = $0003;
  AL_EQUALIZER_MID1_CENTER                            = $0004;
  AL_EQUALIZER_MID1_WIDTH                             = $0005;
  AL_EQUALIZER_MID2_GAIN                              = $0006;
  AL_EQUALIZER_MID2_CENTER                            = $0007;
  AL_EQUALIZER_MID2_WIDTH                             = $0008;
  AL_EQUALIZER_HIGH_GAIN                              = $0009;
  AL_EQUALIZER_HIGH_CUTOFF                            = $000A;

{ Effect type } { }
  AL_EFFECT_FIRST_PARAMETER                           = $0000;
  AL_EFFECT_LAST_PARAMETER                            = $8000;
  AL_EFFECT_TYPE                                      = $8001;

{ Effect type definitions to be used with AL_EFFECT_TYPE. } { }
  AL_EFFECT_NULL                                      = $0000  { Can also be used as an Effect Object ID } { };
  AL_EFFECT_REVERB                                    = $0001;
  AL_EFFECT_CHORUS                                    = $0002;
  AL_EFFECT_DISTORTION                                = $0003;
  AL_EFFECT_ECHO                                      = $0004;
  AL_EFFECT_FLANGER                                   = $0005;
  AL_EFFECT_FREQUENCY_SHIFTER                         = $0006;
  AL_EFFECT_VOCAL_MORPHER                             = $0007;
  AL_EFFECT_PITCH_SHIFTER                             = $0008;
  AL_EFFECT_RING_MODULATOR                            = $0009;
  AL_EFFECT_AUTOWAH                                   = $000A;
  AL_EFFECT_COMPRESSOR                                = $000B;
  AL_EFFECT_EQUALIZER                                 = $000C;

{*
 * Auxiliary Slot object definitions to be used with alAuxiliaryEffectSlot functions.
 } { }
  AL_EFFECTSLOT_EFFECT                                = $0001;
  AL_EFFECTSLOT_GAIN                                  = $0002;
  AL_EFFECTSLOT_AUXILIARY_SEND_AUTO                   = $0003;

{*
 * Value to be used as an Auxiliary Slot ID to disable a source send..
 } { }
  AL_EFFECTSLOT_NULL                                  = $0000;


{*
 * Filter object definitions to be used with alFilter functions.
 } { }

{ Lowpass parameters. } { }
  AL_LOWPASS_GAIN                                     = $0001;
  AL_LOWPASS_GAINHF                                   = $0002;

{ Highpass Parameters } { }
  AL_HIGHPASS_GAIN                                    = $0001;
  AL_HIGHPASS_GAINLF                                  = $0002;

{ Bandpass Parameters } { }
  AL_BANDPASS_GAIN                                    = $0001;
  AL_BANDPASS_GAINLF                                  = $0002;
  AL_BANDPASS_GAINHF                                  = $0003;

{ Filter type } { }
  AL_FILTER_FIRST_PARAMETER                           = $0000;
  AL_FILTER_LAST_PARAMETER                            = $8000;
  AL_FILTER_TYPE                                      = $8001;

{ Filter type definitions to be used with AL_FILTER_TYPE. } { }
  AL_FILTER_NULL                                      = $0000  { Can also be used as a Filter Object ID } { };
  AL_FILTER_LOWPASS                                   = $0001;
  AL_FILTER_HIGHPASS                                  = $0002;
  AL_FILTER_BANDPASS                                  = $0003;

var

{*
 * Effect object functions.
 } { }

{ Create Effect objects. } { }
  alGenEffects: procedure ( n: TALsizei; effects: PALuint ); cdecl;

{ Delete Effect objects. } { }
  alDeleteEffects: procedure ( n: TALsizei; effects: PALuint ); cdecl;

{ Verify a handle is a valid Effect. } { }
  alIsEffect: function ( eid: TALuint ): TALboolean; cdecl;

{ Set an integer parameter for an Effect object. } { }
  alEffecti: procedure ( eid: TALuint; param: TALenum; value: TALint); cdecl;
  alEffectiv: procedure ( eid: TALuint; param: TALenum; values: PALint ); cdecl;

{ Set a floating point parameter for an Effect object. } { }
  alEffectf: procedure ( eid: TALuint; param: TALenum; value: TALfloat); cdecl;
  alEffectfv: procedure ( eid: TALuint; param: TALenum; values: PALfloat ); cdecl;

{ Get an integer parameter for an Effect object. } { }
  alGetEffecti: procedure ( eid: TALuint; pname: TALenum; value: PALint ); cdecl;
  alGetEffectiv: procedure ( eid: TALuint; pname: TALenum; values: PALint ); cdecl;

{ Get a floating point parameter for an Effect object. } { }
  alGetEffectf: procedure ( eid: TALuint; pname: TALenum; value: PALfloat ); cdecl;
  alGetEffectfv: procedure ( eid: TALuint; pname: TALenum; values: PALfloat ); cdecl;


{*
 * Filter object functions
 } { }

{ Create Filter objects. } { }
  alGenFilters: procedure ( n: TALsizei; filters: PALuint ); cdecl;

{ Delete Filter objects. } { }
  alDeleteFilters: procedure ( n: TALsizei; filters: PALuint ); cdecl;

{ Verify a handle is a valid Filter. } { }
  alIsFilter: function ( fid: TALuint ): TALboolean; cdecl;

{ Set an integer parameter for a Filter object. } { }
  alFilteri: procedure ( fid: TALuint; param: TALenum; value: TALint ); cdecl;
  alFilteriv: procedure ( fid: TALuint; param: TALenum; values: PALint ); cdecl;

{ Set a floating point parameter for an Filter object. } { }
  alFilterf: procedure ( fid: TALuint; param: TALenum; value: TALfloat); cdecl;
  alFilterfv: procedure ( fid: TALuint; param: TALenum; values: PALfloat ); cdecl;

{ Get an integer parameter for a Filter object. } { }
  alGetFilteri: procedure ( fid: TALuint; pname: TALenum; value: PALint ); cdecl;
  alGetFilteriv: procedure ( fid: TALuint; pname: TALenum; values: PALint ); cdecl;

{ Get a floating point parameter for a Filter object. } { }
  alGetFilterf: procedure ( fid: TALuint; pname: TALenum; value: PALfloat ); cdecl;
  alGetFilterfv: procedure ( fid: TALuint; pname: TALenum; values: PALfloat ); cdecl;


{*
 * Auxiliary Slot object functions
 } { }

{ Create Auxiliary Slot objects. } { }
  alGenAuxiliaryEffectSlots: procedure ( n: TALsizei; slots: PALuint ); cdecl;

{ Delete Auxiliary Slot objects. } { }
  alDeleteAuxiliaryEffectSlots: procedure ( n: TALsizei; slots: PALuint ); cdecl;

{ Verify a handle is a valid Auxiliary Slot. } { }
  alIsAuxiliaryEffectSlot: function ( slot: TALuint ): TALboolean; cdecl;

{ Set an integer parameter for a Auxiliary Slot object. } { }
  alAuxiliaryEffectSloti: procedure ( asid: TALuint; param: TALenum; value: TALint ); cdecl;
  alAuxiliaryEffectSlotiv: procedure ( asid: TALuint; param: TALenum; values: PALint ); cdecl;

{ Set a floating point parameter for an Auxiliary Slot object. } { }
  alAuxiliaryEffectSlotf: procedure ( asid: TALuint; param: TALenum; value: TALfloat ); cdecl;
  alAuxiliaryEffectSlotfv: procedure ( asid: TALuint; param: TALenum; values: PALfloat ); cdecl;

{ Get an integer parameter for a Auxiliary Slot object. } { }
  alGetAuxiliaryEffectSloti: procedure ( asid: TALuint; pname: TALenum; value: PALint ); cdecl;
  alGetAuxiliaryEffectSlotiv: procedure ( asid: TALuint; pname: TALenum; values: PALint ); cdecl;

{ Get a floating point parameter for a Auxiliary Slot object. } { }
  alGetAuxiliaryEffectSlotf: procedure ( asid: TALuint; pname: TALenum; value: PALfloat ); cdecl;
  alGetAuxiliaryEffectSlotfv: procedure ( asid: TALuint; pname: TALenum; values: PALfloat ); cdecl;

{*********************************************************
 * Filter ranges and defaults.
 } { }

const

{*
 * Lowpass filter
 } { }

  LOWPASS_MIN_GAIN                                   = 0.0;
  LOWPASS_MAX_GAIN                                   = 1.0;
  LOWPASS_DEFAULT_GAIN                               = 1.0;

  LOWPASS_MIN_GAINHF                                 = 0.0;
  LOWPASS_MAX_GAINHF                                 = 1.0;
  LOWPASS_DEFAULT_GAINHF                             = 1.0;

{*
 * Highpass filter
 } { }

  HIGHPASS_MIN_GAIN                                  = 0.0;
  HIGHPASS_MAX_GAIN                                  = 1.0;
  HIGHPASS_DEFAULT_GAIN                              = 1.0;

  HIGHPASS_MIN_GAINLF                                = 0.0;
  HIGHPASS_MAX_GAINLF                                = 1.0;
  HIGHPASS_DEFAULT_GAINLF                            = 1.0;

{*
 * Bandpass filter
 } { }

  BANDPASS_MIN_GAIN                                  = 0.0;
  BANDPASS_MAX_GAIN                                  = 1.0;
  BANDPASS_DEFAULT_GAIN                              = 1.0;

  BANDPASS_MIN_GAINHF                                = 0.0;
  BANDPASS_MAX_GAINHF                                = 1.0;
  BANDPASS_DEFAULT_GAINHF                            = 1.0;

  BANDPASS_MIN_GAINLF                                = 0.0;
  BANDPASS_MAX_GAINLF                                = 1.0;
  BANDPASS_DEFAULT_GAINLF                            = 1.0;




 {*********************************************************
 * Effect parameter structures, value definitions, ranges and defaults.
 } { }

{*
 * AL reverb effect parameter ranges and defaults
 } { }
  AL_REVERB_MIN_DENSITY                              = 0.0;
  AL_REVERB_MAX_DENSITY                              = 1.0;
  AL_REVERB_DEFAULT_DENSITY                          = 1.0;

  AL_REVERB_MIN_DIFFUSION                            = 0.0;
  AL_REVERB_MAX_DIFFUSION                            = 1.0;
  AL_REVERB_DEFAULT_DIFFUSION                        = 1.0;

  AL_REVERB_MIN_GAIN                                 = 0.0;
  AL_REVERB_MAX_GAIN                                 = 1.0;
  AL_REVERB_DEFAULT_GAIN                             = 0.32;

  AL_REVERB_MIN_GAINHF                               = 0.0;
  AL_REVERB_MAX_GAINHF                               = 1.0;
  AL_REVERB_DEFAULT_GAINHF                           = 0.89;

  AL_REVERB_MIN_DECAY_TIME                           = 0.1;
  AL_REVERB_MAX_DECAY_TIME                           = 20.0;
  AL_REVERB_DEFAULT_DECAY_TIME                       = 1.49;

  AL_REVERB_MIN_DECAY_HFRATIO                        = 0.1;
  AL_REVERB_MAX_DECAY_HFRATIO                        = 2.0;
  AL_REVERB_DEFAULT_DECAY_HFRATIO                    = 0.83;

  AL_REVERB_MIN_REFLECTIONS_GAIN                     = 0.0;
  AL_REVERB_MAX_REFLECTIONS_GAIN                     = 3.16;
  AL_REVERB_DEFAULT_REFLECTIONS_GAIN                 = 0.05;

  AL_REVERB_MIN_REFLECTIONS_DELAY                    = 0.0;
  AL_REVERB_MAX_REFLECTIONS_DELAY                    = 0.3;
  AL_REVERB_DEFAULT_REFLECTIONS_DELAY                = 0.007;

  AL_REVERB_MIN_LATE_REVERB_GAIN                     = 0.0;
  AL_REVERB_MAX_LATE_REVERB_GAIN                     = 10.0;
  AL_REVERB_DEFAULT_LATE_REVERB_GAIN                 = 1.26;

  AL_REVERB_MIN_LATE_REVERB_DELAY                    = 0.0;
  AL_REVERB_MAX_LATE_REVERB_DELAY                    = 0.1;
  AL_REVERB_DEFAULT_LATE_REVERB_DELAY                = 0.011;

  AL_REVERB_MIN_AIR_ABSORPTION_GAINHF                = 0.892;
  AL_REVERB_MAX_AIR_ABSORPTION_GAINHF                = 1.0;
  AL_REVERB_DEFAULT_AIR_ABSORPTION_GAINHF            = 0.994;

  AL_REVERB_MIN_ROOM_ROLLOFF_FACTOR                  = 0.0;
  AL_REVERB_MAX_ROOM_ROLLOFF_FACTOR                  = 10.0;
  AL_REVERB_DEFAULT_ROOM_ROLLOFF_FACTOR              = 0.0;

  AL_REVERB_MIN_DECAY_HFLIMIT                        = AL_FALSE;
  AL_REVERB_MAX_DECAY_HFLIMIT                        = AL_TRUE;
  AL_REVERB_DEFAULT_DECAY_HFLIMIT                    = AL_TRUE;

{*
 * AL chorus effect parameter ranges and defaults
 } { }
  AL_CHORUS_MIN_WAVEFORM                             = 0;
  AL_CHORUS_MAX_WAVEFORM                             = 1;
  AL_CHORUS_DEFAULT_WAVEFORM                         = 1;

  AL_CHORUS_WAVEFORM_SINUSOID                        = 0;
  AL_CHORUS_WAVEFORM_TRIANGLE                        = 1;

  AL_CHORUS_MIN_PHASE                                = -180;
  AL_CHORUS_MAX_PHASE                                = 180;
  AL_CHORUS_DEFAULT_PHASE                            = 90;

  AL_CHORUS_MIN_RATE                                 = 0.0;
  AL_CHORUS_MAX_RATE                                 = 10.0;
  AL_CHORUS_DEFAULT_RATE                             = 1.1;

  AL_CHORUS_MIN_DEPTH                                = 0.0;
  AL_CHORUS_MAX_DEPTH                                = 1.0;
  AL_CHORUS_DEFAULT_DEPTH                            = 0.1;

  AL_CHORUS_MIN_FEEDBACK                             = -1.0;
  AL_CHORUS_MAX_FEEDBACK                             = 1.0;
  AL_CHORUS_DEFAULT_FEEDBACK                         = 0.25;

  AL_CHORUS_MIN_DELAY                                = 0.0;
  AL_CHORUS_MAX_DELAY                                = 0.016;
  AL_CHORUS_DEFAULT_DELAY                            = 0.016;

{*
 * AL distortion effect parameter ranges and defaults
 } { }
  AL_DISTORTION_MIN_EDGE                             = 0.0;
  AL_DISTORTION_MAX_EDGE                             = 1.0;
  AL_DISTORTION_DEFAULT_EDGE                         = 0.2;

  AL_DISTORTION_MIN_GAIN                             = 0.01;
  AL_DISTORTION_MAX_GAIN                             = 1.0;
  AL_DISTORTION_DEFAULT_GAIN                         = 0.05;

  AL_DISTORTION_MIN_LOWPASS_CUTOFF                   = 80.0;
  AL_DISTORTION_MAX_LOWPASS_CUTOFF                   = 24000.0;
  AL_DISTORTION_DEFAULT_LOWPASS_CUTOFF               = 8000.0;

  AL_DISTORTION_MIN_EQCENTER                         = 80.0;
  AL_DISTORTION_MAX_EQCENTER                         = 24000.0;
  AL_DISTORTION_DEFAULT_EQCENTER                     = 3600.0;

  AL_DISTORTION_MIN_EQBANDWIDTH                      = 80.0;
  AL_DISTORTION_MAX_EQBANDWIDTH                      = 24000.0;
  AL_DISTORTION_DEFAULT_EQBANDWIDTH                  = 3600.0;

{*
 * AL echo effect parameter ranges and defaults
 } { }
  AL_ECHO_MIN_DELAY                                  = 0.0;
  AL_ECHO_MAX_DELAY                                  = 0.207;
  AL_ECHO_DEFAULT_DELAY                              = 0.1;

  AL_ECHO_MIN_LRDELAY                                = 0.0;
  AL_ECHO_MAX_LRDELAY                                = 0.404;
  AL_ECHO_DEFAULT_LRDELAY                            = 0.1;

  AL_ECHO_MIN_DAMPING                                = 0.0;
  AL_ECHO_MAX_DAMPING                                = 0.99;
  AL_ECHO_DEFAULT_DAMPING                            = 0.5;

  AL_ECHO_MIN_FEEDBACK                               = 0.0;
  AL_ECHO_MAX_FEEDBACK                               = 1.0;
  AL_ECHO_DEFAULT_FEEDBACK                           = 0.5;

  AL_ECHO_MIN_SPREAD                                 = -1.0;
  AL_ECHO_MAX_SPREAD                                 = 1.0;
  AL_ECHO_DEFAULT_SPREAD                             = -1.0;

{*
 * AL flanger effect parameter ranges and defaults
 } { }
  AL_FLANGER_MIN_WAVEFORM                            = 0;
  AL_FLANGER_MAX_WAVEFORM                            = 1;
  AL_FLANGER_DEFAULT_WAVEFORM                        = 1;

  AL_FLANGER_WAVEFORM_SINUSOID                       = 0;
  AL_FLANGER_WAVEFORM_TRIANGLE                       = 1;

  AL_FLANGER_MIN_PHASE                               = -180;
  AL_FLANGER_MAX_PHASE                               = 180;
  AL_FLANGER_DEFAULT_PHASE                           = 0;

  AL_FLANGER_MIN_RATE                                = 0.0;
  AL_FLANGER_MAX_RATE                                = 10.0;
  AL_FLANGER_DEFAULT_RATE                            = 0.27;

  AL_FLANGER_MIN_DEPTH                               = 0.0;
  AL_FLANGER_MAX_DEPTH                               = 1.0;
  AL_FLANGER_DEFAULT_DEPTH                           = 1.0;

  AL_FLANGER_MIN_FEEDBACK                            = -1.0;
  AL_FLANGER_MAX_FEEDBACK                            = 1.0;
  AL_FLANGER_DEFAULT_FEEDBACK                        = -0.5;

  AL_FLANGER_MIN_DELAY                               = 0.0;
  AL_FLANGER_MAX_DELAY                               = 0.004;
  AL_FLANGER_DEFAULT_DELAY                           = 0.002;

{*
 * AL frequency shifter effect parameter ranges and defaults
 } { }
  AL_FREQUENCY_SHIFTER_MIN_FREQUENCY                 = 0.0;
  AL_FREQUENCY_SHIFTER_MAX_FREQUENCY                 = 24000.0;
  AL_FREQUENCY_SHIFTER_DEFAULT_FREQUENCY             = 0.0;

  AL_FREQUENCY_SHIFTER_MIN_LEFT_DIRECTION            = 0;
  AL_FREQUENCY_SHIFTER_MAX_LEFT_DIRECTION            = 2;
  AL_FREQUENCY_SHIFTER_DEFAULT_LEFT_DIRECTION        = 0;

  AL_FREQUENCY_SHIFTER_MIN_RIGHT_DIRECTION           = 0;
  AL_FREQUENCY_SHIFTER_MAX_RIGHT_DIRECTION           = 2;
  AL_FREQUENCY_SHIFTER_DEFAULT_RIGHT_DIRECTION       = 0;

  AL_FREQUENCY_SHIFTER_DIRECTION_DOWN                = 0;
  AL_FREQUENCY_SHIFTER_DIRECTION_UP                  = 1;
  AL_FREQUENCY_SHIFTER_DIRECTION_OFF                 = 2;

{*
 * AL vocal morpher effect parameter ranges and defaults
 } { }
  AL_VOCAL_MORPHER_MIN_PHONEMEA                      = 0;
  AL_VOCAL_MORPHER_MAX_PHONEMEA                      = 29;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA                  = 0;

  AL_VOCAL_MORPHER_MIN_PHONEMEA_COARSE_TUNING            = -24;
  AL_VOCAL_MORPHER_MAX_PHONEMEA_COARSE_TUNING            = 24;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEA_COARSE_TUNING    = 0;

  AL_VOCAL_MORPHER_MIN_PHONEMEB                      = 0;
  AL_VOCAL_MORPHER_MAX_PHONEMEB                      = 29;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB                  = 10;

  AL_VOCAL_MORPHER_PHONEME_A                         = 0;
  AL_VOCAL_MORPHER_PHONEME_E                         = 1;
  AL_VOCAL_MORPHER_PHONEME_I                         = 2;
  AL_VOCAL_MORPHER_PHONEME_O                         = 3;
  AL_VOCAL_MORPHER_PHONEME_U                         = 4;
  AL_VOCAL_MORPHER_PHONEME_AA                        = 5;
  AL_VOCAL_MORPHER_PHONEME_AE                        = 6;
  AL_VOCAL_MORPHER_PHONEME_AH                        = 7;
  AL_VOCAL_MORPHER_PHONEME_AO                        = 8;
  AL_VOCAL_MORPHER_PHONEME_EH                        = 9;
  AL_VOCAL_MORPHER_PHONEME_ER                        = 10;
  AL_VOCAL_MORPHER_PHONEME_IH                        = 11;
  AL_VOCAL_MORPHER_PHONEME_IY                        = 12;
  AL_VOCAL_MORPHER_PHONEME_UH                        = 13;
  AL_VOCAL_MORPHER_PHONEME_UW                        = 14;
  AL_VOCAL_MORPHER_PHONEME_B                         = 15;
  AL_VOCAL_MORPHER_PHONEME_D                         = 16;
  AL_VOCAL_MORPHER_PHONEME_F                         = 17;
  AL_VOCAL_MORPHER_PHONEME_G                         = 18;
  AL_VOCAL_MORPHER_PHONEME_J                         = 19;
  AL_VOCAL_MORPHER_PHONEME_K                         = 20;
  AL_VOCAL_MORPHER_PHONEME_L                         = 21;
  AL_VOCAL_MORPHER_PHONEME_M                         = 22;
  AL_VOCAL_MORPHER_PHONEME_N                         = 23;
  AL_VOCAL_MORPHER_PHONEME_P                         = 24;
  AL_VOCAL_MORPHER_PHONEME_R                         = 25;
  AL_VOCAL_MORPHER_PHONEME_S                         = 26;
  AL_VOCAL_MORPHER_PHONEME_T                         = 27;
  AL_VOCAL_MORPHER_PHONEME_V                         = 28;
  AL_VOCAL_MORPHER_PHONEME_Z                         = 29;

  AL_VOCAL_MORPHER_MIN_PHONEMEB_COARSE_TUNING        = -24;
  AL_VOCAL_MORPHER_MAX_PHONEMEB_COARSE_TUNING        = 24;
  AL_VOCAL_MORPHER_DEFAULT_PHONEMEB_COARSE_TUNING    = 0;

  AL_VOCAL_MORPHER_MIN_WAVEFORM                      = 0;
  AL_VOCAL_MORPHER_MAX_WAVEFORM                      = 2;
  AL_VOCAL_MORPHER_DEFAULT_WAVEFORM                  = 0;

  AL_VOCAL_MORPHER_WAVEFORM_SINUSOID                 = 0;
  AL_VOCAL_MORPHER_WAVEFORM_TRIANGLE                 = 1;
  AL_VOCAL_MORPHER_WAVEFORM_SAWTOOTH                 = 2;

  AL_VOCAL_MORPHER_MIN_RATE                          = 0.0;
  AL_VOCAL_MORPHER_MAX_RATE                          = 10.0;
  AL_VOCAL_MORPHER_DEFAULT_RATE                      = 1.41;

{*
 * AL pitch shifter effect parameter ranges and defaults
 } { }
  AL_PITCH_SHIFTER_MIN_COARSE_TUNE                   = -12;
  AL_PITCH_SHIFTER_MAX_COARSE_TUNE                   = 12;
  AL_PITCH_SHIFTER_DEFAULT_COARSE_TUNE               = 12;

  AL_PITCH_SHIFTER_MIN_FINE_TUNE                     = -50;
  AL_PITCH_SHIFTER_MAX_FINE_TUNE                     = 50;
  AL_PITCH_SHIFTER_DEFAULT_FINE_TUNE                 = 0;

{*
 * AL ring modulator effect parameter ranges and defaults
 } { }
  AL_RING_MODULATOR_MIN_FREQUENCY                    = 0.0;
  AL_RING_MODULATOR_MAX_FREQUENCY                    = 8000.0;
  AL_RING_MODULATOR_DEFAULT_FREQUENCY                = 440.0;

  AL_RING_MODULATOR_MIN_HIGHPASS_CUTOFF              = 0.0;
  AL_RING_MODULATOR_MAX_HIGHPASS_CUTOFF              = 24000.0;
  AL_RING_MODULATOR_DEFAULT_HIGHPASS_CUTOFF          = 800.0;

  AL_RING_MODULATOR_MIN_WAVEFORM                     = 0;
  AL_RING_MODULATOR_MAX_WAVEFORM                     = 2;
  AL_RING_MODULATOR_DEFAULT_WAVEFORM                 = 0;

  AL_RING_MODULATOR_SINUSOID                         = 0;
  AL_RING_MODULATOR_SAWTOOTH                         = 1;
  AL_RING_MODULATOR_SQUARE                           = 2;

{*
 * AL autowah effect parameter ranges and defaults
 } { }
  AL_AUTOWAH_MIN_ATTACK_TIME                         = 0.0001;
  AL_AUTOWAH_MAX_ATTACK_TIME                         = 1.0;
  AL_AUTOWAH_DEFAULT_ATTACK_TIME                     = 0.06;

  AL_AUTOWAH_MIN_RELEASE_TIME                        = 0.0001;
  AL_AUTOWAH_MAX_RELEASE_TIME                        = 1.0;
  AL_AUTOWAH_DEFAULT_RELEASE_TIME                    = 0.06;

  AL_AUTOWAH_MIN_RESONANCE                           = 2.0;
  AL_AUTOWAH_MAX_RESONANCE                           = 1000.0;
  AL_AUTOWAH_DEFAULT_RESONANCE                       = 1000.0;

  AL_AUTOWAH_MIN_PEAK_GAIN                           = 0.00003;
  AL_AUTOWAH_MAX_PEAK_GAIN                           = 31621.0;
  AL_AUTOWAH_DEFAULT_PEAK_GAIN                       = 11.22;

{*
 * AL compressor effect parameter ranges and defaults
 } { }
  AL_COMPRESSOR_MIN_ONOFF                            = 0;
  AL_COMPRESSOR_MAX_ONOFF                            = 1;
  AL_COMPRESSOR_DEFAULT_ONOFF                        = 1;

{*
 * AL equalizer effect parameter ranges and defaults
 } { }
  AL_EQUALIZER_MIN_LOW_GAIN                          = 0.126;
  AL_EQUALIZER_MAX_LOW_GAIN                          = 7.943;
  AL_EQUALIZER_DEFAULT_LOW_GAIN                      = 1.0;

  AL_EQUALIZER_MIN_LOW_CUTOFF                        = 50.0;
  AL_EQUALIZER_MAX_LOW_CUTOFF                        = 800.0;
  AL_EQUALIZER_DEFAULT_LOW_CUTOFF                    = 200.0;

  AL_EQUALIZER_MIN_MID1_GAIN                         = 0.126;
  AL_EQUALIZER_MAX_MID1_GAIN                         = 7.943;
  AL_EQUALIZER_DEFAULT_MID1_GAIN                     = 1.0;

  AL_EQUALIZER_MIN_MID1_CENTER                       = 200.0;
  AL_EQUALIZER_MAX_MID1_CENTER                       = 3000.0;
  AL_EQUALIZER_DEFAULT_MID1_CENTER                   = 500.0;

  AL_EQUALIZER_MIN_MID1_WIDTH                        = 0.01;
  AL_EQUALIZER_MAX_MID1_WIDTH                        = 1.0;
  AL_EQUALIZER_DEFAULT_MID1_WIDTH                    = 1.0;

  AL_EQUALIZER_MIN_MID2_GAIN                         = 0.126;
  AL_EQUALIZER_MAX_MID2_GAIN                         = 7.943;
  AL_EQUALIZER_DEFAULT_MID2_GAIN                     = 1.0;

  AL_EQUALIZER_MIN_MID2_CENTER                       = 1000.0;
  AL_EQUALIZER_MAX_MID2_CENTER                       = 8000.0;
  AL_EQUALIZER_DEFAULT_MID2_CENTER                   = 3000.0;

  AL_EQUALIZER_MIN_MID2_WIDTH                        = 0.01;
  AL_EQUALIZER_MAX_MID2_WIDTH                        = 1.0;
  AL_EQUALIZER_DEFAULT_MID2_WIDTH                    = 1.0;

  AL_EQUALIZER_MIN_HIGH_GAIN                         = 0.126;
  AL_EQUALIZER_MAX_HIGH_GAIN                         = 7.943;
  AL_EQUALIZER_DEFAULT_HIGH_GAIN                     = 1.0;

  AL_EQUALIZER_MIN_HIGH_CUTOFF                       = 4000.0;
  AL_EQUALIZER_MAX_HIGH_CUTOFF                       = 16000.0;
  AL_EQUALIZER_DEFAULT_HIGH_CUTOFF                   = 6000.0;




{*********************************************************
 * Source parameter value definitions, ranges and defaults.
 } { }
  AL_MIN_AIR_ABSORPTION_FACTOR                       = 0.0;
  AL_MAX_AIR_ABSORPTION_FACTOR                       = 10.0;
  AL_DEFAULT_AIR_ABSORPTION_FACTOR                   = 0.0;

  AL_MIN_ROOM_ROLLOFF_FACTOR                         = 0.0;
  AL_MAX_ROOM_ROLLOFF_FACTOR                         = 10.0;
  AL_DEFAULT_ROOM_ROLLOFF_FACTOR                     = 0.0;

  AL_MIN_CONE_OUTER_GAINHF                           = 0.0;
  AL_MAX_CONE_OUTER_GAINHF                           = 1.0;
  AL_DEFAULT_CONE_OUTER_GAINHF                       = 1.0;

  AL_MIN_DIRECT_FILTER_GAINHF_AUTO                   = AL_FALSE;
  AL_MAX_DIRECT_FILTER_GAINHF_AUTO                   = AL_TRUE;
  AL_DEFAULT_DIRECT_FILTER_GAINHF_AUTO               = AL_TRUE;

  AL_MIN_AUXILIARY_SEND_FILTER_GAIN_AUTO             = AL_FALSE;
  AL_MAX_AUXILIARY_SEND_FILTER_GAIN_AUTO             = AL_TRUE;
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAIN_AUTO         = AL_TRUE;

  AL_MIN_AUXILIARY_SEND_FILTER_GAINHF_AUTO           = AL_FALSE;
  AL_MAX_AUXILIARY_SEND_FILTER_GAINHF_AUTO           = AL_TRUE;
  AL_DEFAULT_AUXILIARY_SEND_FILTER_GAINHF_AUTO       = AL_TRUE;




{*********************************************************
 * Listener parameter value definitions, ranges and defaults.
 } { }
  AL_MIN_METERS_PER_UNIT                             = {FLT_MIN}MinSingle;
  AL_MAX_METERS_PER_UNIT                             = {FLT_MAX}MaxSingle;
  AL_DEFAULT_METERS_PER_UNIT                         = 1.0;
