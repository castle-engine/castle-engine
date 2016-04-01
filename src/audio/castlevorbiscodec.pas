{ Minimal translation of C /usr/include/vorbis/codec.h header.
  @exclude (This is only a C header translation --- no nice PasDoc docs.) }
unit CastleVorbisCodec;

{$I castleconf.inc}
{$packrecords C}

interface

uses CTypes, CastleOgg;

type
  Tvorbis_info = record
    version: CInt;
    channels: CInt;
    rate: CLong;

    {* The below bitrate declarations are *hints*.
       Combinations of the three values carry the following implications:

       all three set to the same value:
         implies a fixed rate bitstream
       only nominal set:
         implies a VBR stream that averages the nominal bitrate.  No hard
         upper/lower limit
       upper and or lower set:
         implies a VBR bitstream that obeys the bitrate limits. nominal
         may also be set to give a nominal rate.
       none set:
         the coder does not care to speculate.
    *}

    bitrate_upper: CLong;
    bitrate_nominal: CLong;
    bitrate_lower: CLong;
    bitrate_window: CLong;

    codec_setup: Pointer;
  end;
  Pvorbis_info = ^Tvorbis_info;

  {* vorbis_dsp_state buffers the current vorbis audio
     analysis/synthesis state.  The DSP state belongs to a specific
     logical bitstream ****************************************************}
  Tvorbis_dsp_state = record
    analysisp: Cint;
    vi: Pvorbis_info;

    pcm: Pointer;
    pcmret: Pointer;
    pcm_storage: CInt;
    pcm_current: Cint;
    pcm_returned: Cint;

    preextrapolate: Cint;
    eofflag: Cint;

    lW: Clong;
    W: Clong;
    nW: Clong;
    centerW: Clong;

    granulepos: Int64;
    sequence: Int64;

    glue_bits: Int64;
    time_bits: Int64;
    floor_bits: Int64;
    res_bits: Int64;

    backend_state: Pointer;
  end;
  Pvorbis_dsp_state = ^Tvorbis_dsp_state;

  Tvorbis_block = record
    {* necessary stream state for linking to the framing abstraction *}
    pcm: Pointer;       {* this is a pointer into local storage *}
    opb: Toggpack_buffer;

    lW: Clong;
    W: Clong;
    nW: Clong;
    pcmend: Cint;
    mode: Cint;

    eofflag: Cint;
    granulepos: Int64;
    sequence: Int64;
    vd: Pvorbis_dsp_state; {* For read-only access of configuration *}

    {* local storage to avoid remallocing; it's up to the mapping to
       structure it *}
    localstore: Pointer;
    localtop: Clong;
    localalloc: Clong;
    totaluse: Clong;
    reap: Pointer;

    {* bitmetrics for the frame *}
    glue_bits: Clong;
    time_bits: Clong;
    floor_bits: Clong;
    res_bits: Clong;

    internal: Pointer;
  end;

  {* vorbis_info contains all the setup information specific to the
     specific compression/decompression mode in progress (eg,
     psychoacoustic settings, channel setup, options, codebook
     etc). vorbis_info and substructures are in backends.h.
  *********************************************************************/

  /* the comments are not part of vorbis_info so that vorbis_info can be
     static storage *}
  Tvorbis_comment = record
    {* unlimited user comment fields.  libvorbis writes 'libvorbis'
       whatever vendor is set to in encode *}
    user_comments: Pointer;
    comment_lengths: PCInt;
    comments: CInt;
    vendor: PChar;
  end;
  Pvorbis_comment = ^Tvorbis_comment;

const
  {* Vorbis ERRORS and return codes ***********************************}
  OV_FALSE      = -1;
  OV_EOF        = -2;
  OV_HOLE       = -3;

  OV_EREAD      = -128;
  OV_EFAULT     = -129;
  OV_EIMPL      = -130;
  OV_EINVAL     = -131;
  OV_ENOTVORBIS = -132;
  OV_EBADHEADER = -133;
  OV_EVERSION   = -134;
  OV_ENOTAUDIO  = -135;
  OV_EBADPACKET = -136;
  OV_EBADLINK   = -137;
  OV_ENOSEEK    = -138;

implementation

end.
