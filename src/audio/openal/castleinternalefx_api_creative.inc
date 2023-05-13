{%MainUnit castleinternalefx.pas}

{ This contains translation of C header eax-creative.h. }

{*
 *  efx-creative.h - Environmental Audio Extensions
 *            for OpenAL Effects Extension.
 *
 }

const

{*
 * Effect object definitions to be used with alEffect functions.
 *
 * Effect parameter value definitions, ranges, and defaults
 * appear farther down in this file.
 }

{ AL EAXReverb effect parameters. } { }
  AL_EAXREVERB_DENSITY                                = $0001;
  AL_EAXREVERB_DIFFUSION                              = $0002;
  AL_EAXREVERB_GAIN                                   = $0003;
  AL_EAXREVERB_GAINHF                                 = $0004;
  AL_EAXREVERB_GAINLF                                 = $0005;
  AL_EAXREVERB_DECAY_TIME                             = $0006;
  AL_EAXREVERB_DECAY_HFRATIO                          = $0007;
  AL_EAXREVERB_DECAY_LFRATIO                          = $0008;
  AL_EAXREVERB_REFLECTIONS_GAIN                       = $0009;
  AL_EAXREVERB_REFLECTIONS_DELAY                      = $000A;
  AL_EAXREVERB_REFLECTIONS_PAN                        = $000B;
  AL_EAXREVERB_LATE_REVERB_GAIN                       = $000C;
  AL_EAXREVERB_LATE_REVERB_DELAY                      = $000D;
  AL_EAXREVERB_LATE_REVERB_PAN                        = $000E;
  AL_EAXREVERB_ECHO_TIME                              = $000F;
  AL_EAXREVERB_ECHO_DEPTH                             = $0010;
  AL_EAXREVERB_MODULATION_TIME                        = $0011;
  AL_EAXREVERB_MODULATION_DEPTH                       = $0012;
  AL_EAXREVERB_AIR_ABSORPTION_GAINHF                  = $0013;
  AL_EAXREVERB_HFREFERENCE                            = $0014;
  AL_EAXREVERB_LFREFERENCE                            = $0015;
  AL_EAXREVERB_ROOM_ROLLOFF_FACTOR                    = $0016;
  AL_EAXREVERB_DECAY_HFLIMIT                          = $0017;

{ Effect type definitions to be used with AL_EFFECT_TYPE. } { }
  AL_EFFECT_EAXREVERB                                 = $8000;



 {*********************************************************
 * Effect parameter structures, value definitions, ranges and defaults.
 }

{*
 * AL reverb effect parameter ranges and defaults
 } { }
  AL_EAXREVERB_MIN_DENSITY                           = 0.0;
  AL_EAXREVERB_MAX_DENSITY                           = 1.0;
  AL_EAXREVERB_DEFAULT_DENSITY                       = 1.0;

  AL_EAXREVERB_MIN_DIFFUSION                         = 0.0;
  AL_EAXREVERB_MAX_DIFFUSION                         = 1.0;
  AL_EAXREVERB_DEFAULT_DIFFUSION                     = 1.0;

  AL_EAXREVERB_MIN_GAIN                              = 0.0;
  AL_EAXREVERB_MAX_GAIN                              = 1.0;
  AL_EAXREVERB_DEFAULT_GAIN                          = 0.32;

  AL_EAXREVERB_MIN_GAINHF                            = 0.0;
  AL_EAXREVERB_MAX_GAINHF                            = 1.0;
  AL_EAXREVERB_DEFAULT_GAINHF                        = 0.89;

  AL_EAXREVERB_MIN_GAINLF                            = 0.0;
  AL_EAXREVERB_MAX_GAINLF                            = 1.0;
  AL_EAXREVERB_DEFAULT_GAINLF                        = 1.0;

  AL_EAXREVERB_MIN_DECAY_TIME                        = 0.1;
  AL_EAXREVERB_MAX_DECAY_TIME                        = 20.0;
  AL_EAXREVERB_DEFAULT_DECAY_TIME                    = 1.49;

  AL_EAXREVERB_MIN_DECAY_HFRATIO                     = 0.1;
  AL_EAXREVERB_MAX_DECAY_HFRATIO                     = 2.0;
  AL_EAXREVERB_DEFAULT_DECAY_HFRATIO                 = 0.83;

  AL_EAXREVERB_MIN_DECAY_LFRATIO                     = 0.1;
  AL_EAXREVERB_MAX_DECAY_LFRATIO                     = 2.0;
  AL_EAXREVERB_DEFAULT_DECAY_LFRATIO                 = 1.0;

  AL_EAXREVERB_MIN_REFLECTIONS_GAIN                  = 0.0;
  AL_EAXREVERB_MAX_REFLECTIONS_GAIN                  = 3.16;
  AL_EAXREVERB_DEFAULT_REFLECTIONS_GAIN              = 0.05;

  AL_EAXREVERB_MIN_REFLECTIONS_DELAY                 = 0.0;
  AL_EAXREVERB_MAX_REFLECTIONS_DELAY                 = 0.3;
  AL_EAXREVERB_DEFAULT_REFLECTIONS_DELAY             = 0.007;

  AL_EAXREVERB_DEFAULT_REFLECTIONS_PAN: TVector3 = (X: 0.0; Y: 0.0; Z: 0.0);

  AL_EAXREVERB_MIN_LATE_REVERB_GAIN                  = 0.0;
  AL_EAXREVERB_MAX_LATE_REVERB_GAIN                  = 10.0;
  AL_EAXREVERB_DEFAULT_LATE_REVERB_GAIN              = 1.26;

  AL_EAXREVERB_MIN_LATE_REVERB_DELAY                 = 0.0;
  AL_EAXREVERB_MAX_LATE_REVERB_DELAY                 = 0.1;
  AL_EAXREVERB_DEFAULT_LATE_REVERB_DELAY             = 0.011;

  AL_EAXREVERB_DEFAULT_LATE_REVERB_PAN: TVector3 = (X: 0.0; Y: 0.0; Z: 0.0);

  AL_EAXREVERB_MIN_ECHO_TIME                         = 0.075;
  AL_EAXREVERB_MAX_ECHO_TIME                         = 0.25;
  AL_EAXREVERB_DEFAULT_ECHO_TIME                     = 0.25;

  AL_EAXREVERB_MIN_ECHO_DEPTH                        = 0.0;
  AL_EAXREVERB_MAX_ECHO_DEPTH                        = 1.0;
  AL_EAXREVERB_DEFAULT_ECHO_DEPTH                    = 0.0;

  AL_EAXREVERB_MIN_MODULATION_TIME                   = 0.04;
  AL_EAXREVERB_MAX_MODULATION_TIME                   = 4.0;
  AL_EAXREVERB_DEFAULT_MODULATION_TIME               = 0.25;

  AL_EAXREVERB_MIN_MODULATION_DEPTH                  = 0.0;
  AL_EAXREVERB_MAX_MODULATION_DEPTH                  = 1.0;
  AL_EAXREVERB_DEFAULT_MODULATION_DEPTH              = 0.0;

  AL_EAXREVERB_MIN_AIR_ABSORPTION_GAINHF             = 0.892;
  AL_EAXREVERB_MAX_AIR_ABSORPTION_GAINHF             = 1.0;
  AL_EAXREVERB_DEFAULT_AIR_ABSORPTION_GAINHF         = 0.994;

  AL_EAXREVERB_MIN_HFREFERENCE                       = 1000.0;
  AL_EAXREVERB_MAX_HFREFERENCE                       = 20000.0;
  AL_EAXREVERB_DEFAULT_HFREFERENCE                   = 5000.0;

  AL_EAXREVERB_MIN_LFREFERENCE                       = 20.0;
  AL_EAXREVERB_MAX_LFREFERENCE                       = 1000.0;
  AL_EAXREVERB_DEFAULT_LFREFERENCE                   = 250.0;

  AL_EAXREVERB_MIN_ROOM_ROLLOFF_FACTOR               = 0.0;
  AL_EAXREVERB_MAX_ROOM_ROLLOFF_FACTOR               = 10.0;
  AL_EAXREVERB_DEFAULT_ROOM_ROLLOFF_FACTOR           = 0.0;

  AL_EAXREVERB_MIN_DECAY_HFLIMIT                     = AL_FALSE;
  AL_EAXREVERB_MAX_DECAY_HFLIMIT                     = AL_TRUE;
  AL_EAXREVERB_DEFAULT_DECAY_HFLIMIT                 = AL_TRUE;


