{ API of vorbisfile library. Usually libvorbisfile.so under Unixes
  or vorbisfile.dll under Windows. This is just a quick translation
  of /usr/include/vorbis/vorbisfile.h header.
  @exclude (This is only a C header translation --- no nice PasDoc docs.) }
unit CastleVorbisFile;

{$packrecords C}

{$i castleconf.inc}
{$I vorbisfile_conf.inc}

interface

uses CTypes, CastleVorbisCodec, CastleOgg;

const
  NOTOPEN   = 0;
  PARTOPEN  = 1;
  OPENED    = 2;
  STREAMSET = 3;
  INITSET   = 4;

type
  TSizeT = LongWord;

  TVorbisFileReadFunc = function (ptr: Pointer; Size: TSizeT; nmemb: TSizeT; DataSource: Pointer): TSizeT; libvorbisfile_decl;
  TVorbisFileSeekFunc = function (DataSource: Pointer; offset: Int64; whence: CInt): CInt; libvorbisfile_decl;
  TVorbisFileCloseFunc = function (DataSource: Pointer): CInt; libvorbisfile_decl;
  TVorbisFileTellFunc = function (DataSource: Pointer): CLong; libvorbisfile_decl;

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

var
  ov_clear: function (Vf: POggVorbis_File): CInt; libvorbisfile_decl;
  { Not translatable, we don't know C stdio FILE type:
    extern int ov_open(FILE *f,Vf: POggVorbis_File,Initial: PChar,ibytes: CLong); libvorbisfile_decl;}
  ov_open_callbacks: function (DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; libvorbisfile_decl;

  { Not translatable, we don't know C stdio FILE type:
  extern int ov_test(FILE *f,Vf: POggVorbis_File,Initial: PChar,ibytes: CLong); libvorbisfile_decl;}
  ov_test_callbacks: function (DataSource: Pointer; Vf: POggVorbis_File; Initial: PChar; ibytes: CLong; callbacks: Tov_callbacks): CInt; libvorbisfile_decl;
  ov_test_open: function (Vf: POggVorbis_File): CInt; libvorbisfile_decl;

  ov_bitrate: function (Vf: POggVorbis_File; i: CInt): CLong; libvorbisfile_decl;
  ov_bitrate_instant: function (Vf: POggVorbis_File): CLong; libvorbisfile_decl;
  ov_streams: function (Vf: POggVorbis_File): CLong; libvorbisfile_decl;
  ov_seekable: function (Vf: POggVorbis_File): CLong; libvorbisfile_decl;
  ov_serialnumber: function (Vf: POggVorbis_File; i: CInt): CLong; libvorbisfile_decl;

  ov_raw_total: function (Vf: POggVorbis_File; i: CInt): Int64; libvorbisfile_decl;
  ov_pcm_total: function (Vf: POggVorbis_File; i: CInt): Int64; libvorbisfile_decl;
  ov_time_total: function (Vf: POggVorbis_File; i: CInt): Double; libvorbisfile_decl;

  ov_raw_seek: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl;
  ov_pcm_seek: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl;
  ov_pcm_seek_page: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl;
  ov_time_seek: function (Vf: POggVorbis_File; pos: Double): CInt; libvorbisfile_decl;
  ov_time_seek_page: function (Vf: POggVorbis_File; pos: Double): CInt; libvorbisfile_decl;

  // ov_raw_seek_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl; // not available in libtremolo
  // ov_pcm_seek_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl; // not available in libtremolo
  // ov_pcm_seek_page_lap: function (Vf: POggVorbis_File; pos: Int64): CInt; libvorbisfile_decl; // not available in libtremolo
  // ov_time_seek_lap: function (Vf: POggVorbis_File; pos: Double): CInt; libvorbisfile_decl; // not available in libtremolo
  // ov_time_seek_page_lap: function (Vf: POggVorbis_File; pos: Double): CInt; libvorbisfile_decl; // not available in libtremolo

  ov_raw_tell: function (Vf: POggVorbis_File): Int64; libvorbisfile_decl;
  ov_pcm_tell: function (Vf: POggVorbis_File): Int64; libvorbisfile_decl;
  ov_time_tell: function (Vf: POggVorbis_File): Double; libvorbisfile_decl;

  ov_info: function (Vf: POggVorbis_File; link: CInt): Pvorbis_info; libvorbisfile_decl;
  { Not translated yet }
  //extern vorbis_comment *ov_comment(Vf: POggVorbis_File,int link); libvorbisfile_decl;

  //ov_read_float: function (Vf: POggVorbis_File,float ***pcm_channels,int samples, int *bitstream): CLong; libvorbisfile_decl;
  ov_read: function (Vf: POggVorbis_File; var buffer; length, bigendianp, word, sgned: CInt; bitstream: PCInt): CLong; libvorbisfile_decl;
  //ov_crosslap: function (Vf: POggVorbis_File1,Vf: POggVorbis_File2): CInt; libvorbisfile_decl;

  //ov_halfrate: function (Vf: POggVorbis_File,int flag): CInt; libvorbisfile_decl;
  //ov_halfrate_p: function (Vf: POggVorbis_File): CInt; libvorbisfile_decl;


{ Is the vorbisfile shared library (with all necessary symbols) available. }
function VorbisFileInited: boolean;

procedure VorbisFileInitialization;

implementation

uses SysUtils, CastleUtils, CastleDynLib, CastleFilesUtils;

var
  VorbisFileLibrary: TDynLib;

function VorbisFileInited: boolean;
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
    Pointer(ov_clear) := VorbisFileLibrary.Symbol('ov_clear');
    //Pointer(ov_open) := VorbisFileLibrary.Symbol('ov_open');
    Pointer(ov_open_callbacks) := VorbisFileLibrary.Symbol('ov_open_callbacks');

    //Pointer(ov_test) := VorbisFileLibrary.Symbol('ov_test');
    Pointer(ov_test_callbacks) := VorbisFileLibrary.Symbol('ov_test_callbacks');
    Pointer(ov_test_open) := VorbisFileLibrary.Symbol('ov_test_open');

    Pointer(ov_bitrate) := VorbisFileLibrary.Symbol('ov_bitrate');
    Pointer(ov_bitrate_instant) := VorbisFileLibrary.Symbol('ov_bitrate_instant');
    Pointer(ov_streams) := VorbisFileLibrary.Symbol('ov_streams');
    Pointer(ov_seekable) := VorbisFileLibrary.Symbol('ov_seekable');
    Pointer(ov_serialnumber) := VorbisFileLibrary.Symbol('ov_serialnumber');

    Pointer(ov_raw_total) := VorbisFileLibrary.Symbol('ov_raw_total');
    Pointer(ov_pcm_total) := VorbisFileLibrary.Symbol('ov_pcm_total');
    Pointer(ov_time_total) := VorbisFileLibrary.Symbol('ov_time_total');

    Pointer(ov_raw_seek) := VorbisFileLibrary.Symbol('ov_raw_seek');
    Pointer(ov_pcm_seek) := VorbisFileLibrary.Symbol('ov_pcm_seek');
    Pointer(ov_pcm_seek_page) := VorbisFileLibrary.Symbol('ov_pcm_seek_page');
    Pointer(ov_time_seek) := VorbisFileLibrary.Symbol('ov_time_seek');
    Pointer(ov_time_seek_page) := VorbisFileLibrary.Symbol('ov_time_seek_page');

    // Pointer(ov_raw_seek_lap) := VorbisFileLibrary.Symbol('ov_raw_seek_lap'); // not available in libtremolo
    // Pointer(ov_pcm_seek_lap) := VorbisFileLibrary.Symbol('ov_pcm_seek_lap'); // not available in libtremolo
    // Pointer(ov_pcm_seek_page_lap) := VorbisFileLibrary.Symbol('ov_pcm_seek_page_lap'); // not available in libtremolo
    // Pointer(ov_time_seek_lap) := VorbisFileLibrary.Symbol('ov_time_seek_lap'); // not available in libtremolo
    // Pointer(ov_time_seek_page_lap) := VorbisFileLibrary.Symbol('ov_time_seek_page_lap'); // not available in libtremolo

    Pointer(ov_raw_tell) := VorbisFileLibrary.Symbol('ov_raw_tell');
    Pointer(ov_pcm_tell) := VorbisFileLibrary.Symbol('ov_pcm_tell');
    Pointer(ov_time_tell) := VorbisFileLibrary.Symbol('ov_time_tell');

    Pointer(ov_info) := VorbisFileLibrary.Symbol('ov_info');
    //Pointer(ov_comment) := VorbisFileLibrary.Symbol('ov_comment');

    //Pointer(ov_read_float) := VorbisFileLibrary.Symbol('ov_read_float');
    Pointer(ov_read) := VorbisFileLibrary.Symbol('ov_read');
    //Pointer(ov_crosslap) := VorbisFileLibrary.Symbol('ov_crosslap');

    //Pointer(ov_halfrate) := VorbisFileLibrary.Symbol('ov_halfrate');
    //Pointer(ov_halfrate_p) := VorbisFileLibrary.Symbol('ov_halfrate_p');
  end;
end;

initialization
  {$ifdef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
  VorbisFileInitialization;
  {$endif}
finalization
  FreeAndNil(VorbisFileLibrary);
end.
