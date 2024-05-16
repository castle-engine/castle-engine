{ API of vorbisfile / tremolo library.
  Usually called libvorbisfile.so under Unixes or vorbisfile.dll under Windows.
  On ARM devices (Android, iOS) this uses the Tremolo,
  http://wss.co.uk/pinknoise/tremolo/ , which is like vorbisfile but better for mobile devices.
  This started as a translation of /usr/include/vorbis/vorbisfile.h header.

  @exclude (This unit is only a C header translation (it has no nice PasDoc docs),
  and also this is internal.) }
unit CastleInternalVorbisFile;

{ Define this to link statically to Tremolo.
  This is automatically defined by the build tool
  when building a game for iOS using OggVorbis. }
{.$define CASTLE_TREMOLO_STATIC}

{$ifdef FPC}
  {$packrecords C}
{$else}
  {$ALIGN 4}
{$endif}

{$i castleconf.inc}

interface

uses CTypes, CastleInternalVorbisCodec, CastleInternalOgg;

const
  NOTOPEN   = 0;
  PARTOPEN  = 1;
  OPENED    = 2;
  STREAMSET = 3;
  INITSET   = 4;

type
  TSizeT = CUInt32;

  TVorbisFileReadFunc = function (ptr: Pointer; Size: TSizeT; nmemb: TSizeT; DataSource: Pointer): TSizeT; cdecl;
  TVorbisFileSeekFunc = function (DataSource: Pointer; offset: Int64; whence: CInt): CInt; cdecl;
  TVorbisFileCloseFunc = function (DataSource: Pointer): CInt; cdecl;
  TVorbisFileTellFunc = function (DataSource: Pointer): CLong; cdecl;

  Tov_callbacks = record
    read_func: TVorbisFileReadFunc;
    seek_func: TVorbisFileSeekFunc;
    close_func: TVorbisFileCloseFunc;
    tell_func: TVorbisFileTellFunc;
  end;
  Pov_callbacks = ^Tov_callbacks;

  TOggVorbis_File = record
    datasource: Pointer; {* Pointer to a FILE *, etc. *}
    seekable: Cint;
    offset: Int64;
    _end: Int64;
    oy: Togg_sync_state;

    links: Cint;
    offsets: PInt64;
    dataoffsets: PInt64;
    serialnos: PCLong;
    pcmlengths: PInt64; {* overloaded to maintain binary
                                    compatability; x2 size, stores both
                                    beginning and end values *}
    vi: Pvorbis_info;
    vc: Pvorbis_comment;

    {* Decoding working state local storage *}
    pcm_offset: Int64;
    ready_state: CInt;
    current_serialno: Clong;
    current_link: CInt;

    bittrack: double;
    samptrack: double;

    os: Togg_stream_state; {* take physical pages, weld into a logical
                            stream of packets *}
    vd: Tvorbis_dsp_state; {* central working state for the packet->PCM decoder *}
    vb: Tvorbis_block; {* local working space for packet->PCM decode *}

    callbacks: Tov_callbacks;
  end;
  POggVorbis_File = ^TOggVorbis_File;

{$ifdef CASTLE_TREMOLO_STATIC}

// link statically to Tremolo

function ov_clear(Vf: POggVorbis_File): CInt; cdecl; external;
function ov_open_callbacks(DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; cdecl; external;

function ov_test_callbacks(DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; cdecl; external;
function ov_test_open(Vf: POggVorbis_File): CInt; cdecl; external;

function ov_bitrate(Vf: POggVorbis_File; i: CInt): CLong; cdecl; external;
function ov_bitrate_instant(Vf: POggVorbis_File): CLong; cdecl; external;
function ov_streams(Vf: POggVorbis_File): CLong; cdecl; external;
function ov_seekable(Vf: POggVorbis_File): CLong; cdecl; external;
function ov_serialnumber(Vf: POggVorbis_File; i: CInt): CLong; cdecl; external;

function ov_raw_total(Vf: POggVorbis_File; i: CInt): Int64; cdecl; external;
function ov_pcm_total(Vf: POggVorbis_File; i: CInt): Int64; cdecl; external;
{ Returns Int64 on Tremolo, Double on VorbisFile.
  Compare
  https://xiph.org/vorbis/doc/vorbisfile/ov_time_total.html
  https://svn.xiph.org/trunk/Tremor/doc/ov_time_total.html . }
function ov_time_total(Vf: POggVorbis_File; i: CInt): {$ifdef CASTLE_TREMOLO} Int64 {$else} Double {$endif}; cdecl; external;

function ov_raw_seek(Vf: POggVorbis_File; pos: Int64): CInt; cdecl; external;
function ov_pcm_seek(Vf: POggVorbis_File; pos: Int64): CInt; cdecl; external;
function ov_pcm_seek_page(Vf: POggVorbis_File; pos: Int64): CInt; cdecl; external;
function ov_time_seek(Vf: POggVorbis_File; pos: Double): CInt; cdecl; external;
function ov_time_seek_page(Vf: POggVorbis_File; pos: Double): CInt; cdecl; external;

function ov_raw_tell(Vf: POggVorbis_File): Int64; cdecl; external;
function ov_pcm_tell(Vf: POggVorbis_File): Int64; cdecl; external;
function ov_time_tell(Vf: POggVorbis_File): Double; cdecl; external;

function ov_info(Vf: POggVorbis_File; link: CInt): Pvorbis_info; cdecl; external;

function ov_read(Vf: POggVorbis_File; var buffer; length, bigendianp, word, sgned: CInt; bitstream: PCInt): CLong; cdecl; external;

{$else}

var
  ov_clear: function (Vf: POggVorbis_File): CInt; cdecl;
  { Not translatable, we don't know C stdio FILE type:
    extern int ov_open(FILE *f,Vf: POggVorbis_File,Initial: PChar,ibytes: CLong); cdecl;}
  ov_open_callbacks: function (DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; cdecl;

  { Not translatable, we don't know C stdio FILE type:
  extern int ov_test(FILE *f,Vf: POggVorbis_File,Initial: PChar,ibytes: CLong); cdecl;}
  ov_test_callbacks: function (DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; cdecl;
  ov_test_open: function (Vf: POggVorbis_File): CInt; cdecl;

  ov_bitrate: function (Vf: POggVorbis_File; i: CInt): CLong; cdecl;
  ov_bitrate_instant: function (Vf: POggVorbis_File): CLong; cdecl;
  ov_streams: function (Vf: POggVorbis_File): CLong; cdecl;
  ov_seekable: function (Vf: POggVorbis_File): CLong; cdecl;
  ov_serialnumber: function (Vf: POggVorbis_File; i: CInt): CLong; cdecl;

  ov_raw_total: function (Vf: POggVorbis_File; i: CInt): Int64; cdecl;
  ov_pcm_total: function (Vf: POggVorbis_File; i: CInt): Int64; cdecl;
  ov_time_total: function (Vf: POggVorbis_File; i: CInt): {$ifdef CASTLE_TREMOLO} Int64 {$else} Double {$endif}; cdecl;

  ov_raw_seek: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl;
  ov_pcm_seek: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl;
  ov_pcm_seek_page: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl;
  ov_time_seek: function (Vf: POggVorbis_File; pos: Double): CInt; cdecl;
  ov_time_seek_page: function (Vf: POggVorbis_File; pos: Double): CInt; cdecl;

  // ov_raw_seek_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl; // not available in libtremolo
  // ov_pcm_seek_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl; // not available in libtremolo
  // ov_pcm_seek_page_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; cdecl; // not available in libtremolo
  // ov_time_seek_lap: function (Vf: POggVorbis_File; pos: Double): CInt; cdecl; // not available in libtremolo
  // ov_time_seek_page_lap: function (Vf: POggVorbis_File; pos: Double): CInt; cdecl; // not available in libtremolo

  ov_raw_tell: function (Vf: POggVorbis_File): Int64; cdecl;
  ov_pcm_tell: function (Vf: POggVorbis_File): Int64; cdecl;
  ov_time_tell: function (Vf: POggVorbis_File): Double; cdecl;

  ov_info: function (Vf: POggVorbis_File; link: CInt): Pvorbis_info; cdecl;
  { Not translated yet }
  //extern vorbis_comment *ov_comment(Vf: POggVorbis_File,int link); cdecl;

  //ov_read_float: function (Vf: POggVorbis_File,float ***pcm_channels,int samples, int *bitstream): CLong; cdecl;
  ov_read: function (Vf: POggVorbis_File; var buffer; length, bigendianp, word, sgned: CInt; bitstream: PCInt): CLong; cdecl;
  //ov_crosslap: function (Vf: POggVorbis_File1,Vf: POggVorbis_File2): CInt; cdecl;

  //ov_halfrate: function (Vf: POggVorbis_File,int flag): CInt; cdecl;
  //ov_halfrate_p: function (Vf: POggVorbis_File): CInt; cdecl;

{$endif CASTLE_TREMOLO_STATIC}

{ Is the vorbisfile shared library (with all necessary symbols) available. }
function VorbisFileInitialized: boolean;

procedure VorbisFileInitialization;

implementation

{$ifdef CASTLE_TREMOLO_STATIC}

function VorbisFileInitialized: boolean;
begin
  Result := true;
end;

procedure VorbisFileInitialization;
begin
end;

end.

{$else CASTLE_TREMOLO_STATIC}

uses SysUtils, CastleUtils, CastleDynLib, CastleFilesUtils;

var
  VorbisFileLibrary: TDynLib;

function VorbisFileInitialized: boolean;
begin
  Result := VorbisFileLibrary <> nil;
end;

procedure VorbisFileInitialization;
begin
  FreeAndNil(VorbisFileLibrary);

  VorbisFileLibrary :=

    {$ifdef UNIX}
    {$ifdef DARWIN}
    TDynLib.Load('libvorbisfile.3.dylib', false);
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libvorbisfile.dylib', false);
    if (VorbisFileLibrary = nil) and (BundlePath <> '') then
      VorbisFileLibrary := TDynLib.Load(BundlePath + 'Contents/MacOS/libvorbisfile.3.dylib', false);
    if (VorbisFileLibrary = nil) and (BundlePath <> '') then
      VorbisFileLibrary := TDynLib.Load(BundlePath + 'Contents/MacOS/libvorbisfile.dylib', false);

    {$else}
    TDynLib.Load('libvorbisfile.so.3', false);
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libvorbisfile.so', false);
    // fallback on Tremolo, necessary for Android, may also work on other OSes
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libtremolo.so', false);
    if VorbisFileLibrary = nil then
      VorbisFileLibrary := TDynLib.Load('libtremolo-low-precision.so', false);
    {$endif}
    {$endif}

    {$ifdef MSWINDOWS}
    TDynLib.Load('vorbisfile.dll', false);
    {$endif}

  if VorbisFileLibrary <> nil then
  begin
    Pointer({$ifndef FPC}@{$endif} ov_clear) := VorbisFileLibrary.Symbol('ov_clear');
    //Pointer({$ifndef FPC}@{$endif} ov_open) := VorbisFileLibrary.Symbol('ov_open');
    Pointer({$ifndef FPC}@{$endif} ov_open_callbacks) := VorbisFileLibrary.Symbol('ov_open_callbacks');

    //Pointer({$ifndef FPC}@{$endif} ov_test) := VorbisFileLibrary.Symbol('ov_test');
    Pointer({$ifndef FPC}@{$endif} ov_test_callbacks) := VorbisFileLibrary.Symbol('ov_test_callbacks');
    Pointer({$ifndef FPC}@{$endif} ov_test_open) := VorbisFileLibrary.Symbol('ov_test_open');

    Pointer({$ifndef FPC}@{$endif} ov_bitrate) := VorbisFileLibrary.Symbol('ov_bitrate');
    Pointer({$ifndef FPC}@{$endif} ov_bitrate_instant) := VorbisFileLibrary.Symbol('ov_bitrate_instant');
    Pointer({$ifndef FPC}@{$endif} ov_streams) := VorbisFileLibrary.Symbol('ov_streams');
    Pointer({$ifndef FPC}@{$endif} ov_seekable) := VorbisFileLibrary.Symbol('ov_seekable');
    Pointer({$ifndef FPC}@{$endif} ov_serialnumber) := VorbisFileLibrary.Symbol('ov_serialnumber');

    Pointer({$ifndef FPC}@{$endif} ov_raw_total) := VorbisFileLibrary.Symbol('ov_raw_total');
    Pointer({$ifndef FPC}@{$endif} ov_pcm_total) := VorbisFileLibrary.Symbol('ov_pcm_total');
    Pointer({$ifndef FPC}@{$endif} ov_time_total) := VorbisFileLibrary.Symbol('ov_time_total');

    Pointer({$ifndef FPC}@{$endif} ov_raw_seek) := VorbisFileLibrary.Symbol('ov_raw_seek');
    Pointer({$ifndef FPC}@{$endif} ov_pcm_seek) := VorbisFileLibrary.Symbol('ov_pcm_seek');
    Pointer({$ifndef FPC}@{$endif} ov_pcm_seek_page) := VorbisFileLibrary.Symbol('ov_pcm_seek_page');
    Pointer({$ifndef FPC}@{$endif} ov_time_seek) := VorbisFileLibrary.Symbol('ov_time_seek');
    Pointer({$ifndef FPC}@{$endif} ov_time_seek_page) := VorbisFileLibrary.Symbol('ov_time_seek_page');

    // Pointer({$ifndef FPC}@{$endif} ov_raw_seek_lap) := VorbisFileLibrary.Symbol('ov_raw_seek_lap'); // not available in libtremolo
    // Pointer({$ifndef FPC}@{$endif} ov_pcm_seek_lap) := VorbisFileLibrary.Symbol('ov_pcm_seek_lap'); // not available in libtremolo
    // Pointer({$ifndef FPC}@{$endif} ov_pcm_seek_page_lap) := VorbisFileLibrary.Symbol('ov_pcm_seek_page_lap'); // not available in libtremolo
    // Pointer({$ifndef FPC}@{$endif} ov_time_seek_lap) := VorbisFileLibrary.Symbol('ov_time_seek_lap'); // not available in libtremolo
    // Pointer({$ifndef FPC}@{$endif} ov_time_seek_page_lap) := VorbisFileLibrary.Symbol('ov_time_seek_page_lap'); // not available in libtremolo

    Pointer({$ifndef FPC}@{$endif} ov_raw_tell) := VorbisFileLibrary.Symbol('ov_raw_tell');
    Pointer({$ifndef FPC}@{$endif} ov_pcm_tell) := VorbisFileLibrary.Symbol('ov_pcm_tell');
    Pointer({$ifndef FPC}@{$endif} ov_time_tell) := VorbisFileLibrary.Symbol('ov_time_tell');

    Pointer({$ifndef FPC}@{$endif} ov_info) := VorbisFileLibrary.Symbol('ov_info');
    //Pointer({$ifndef FPC}@{$endif} ov_comment) := VorbisFileLibrary.Symbol('ov_comment');

    //Pointer({$ifndef FPC}@{$endif} ov_read_float) := VorbisFileLibrary.Symbol('ov_read_float');
    Pointer({$ifndef FPC}@{$endif} ov_read) := VorbisFileLibrary.Symbol('ov_read');
    //Pointer({$ifndef FPC}@{$endif} ov_crosslap) := VorbisFileLibrary.Symbol('ov_crosslap');

    //Pointer({$ifndef FPC}@{$endif} ov_halfrate) := VorbisFileLibrary.Symbol('ov_halfrate');
    //Pointer({$ifndef FPC}@{$endif} ov_halfrate_p) := VorbisFileLibrary.Symbol('ov_halfrate_p');
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  VorbisFileInitialization;
  {$endif}
finalization
  FreeAndNil(VorbisFileLibrary);
end.

{$endif CASTLE_TREMOLO_STATIC}
