unit imjdmarker;

{ This file contains routines to decode JPEG datastream markers.
  Most of the complexity arises from our desire to support input
  suspension: if not all of the data for a marker is available;
  we must exit back to the application. On resumption; we reprocess
  the marker. }

{ Original: jdmarker.c;  Copyright (C) 1991-1998; Thomas G. Lane. }
{ History
   9.7.96                   Conversion to pascal started      jnn
   22.3.98                  updated to 6b                     jnn }


interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjcomapi,
  imjpeglib;

const	                { JPEG marker codes }
  M_SOF0  = $c0;
  M_SOF1  = $c1;
  M_SOF2  = $c2;
  M_SOF3  = $c3;
  
  M_SOF5  = $c5;
  M_SOF6  = $c6;
  M_SOF7  = $c7;
  
  M_JPG   = $c8;
  M_SOF9  = $c9;
  M_SOF10 = $ca;
  M_SOF11 = $cb;

  M_SOF13 = $cd;
  M_SOF14 = $ce;
  M_SOF15 = $cf;
  
  M_DHT   = $c4;
  
  M_DAC   = $cc;
  
  M_RST0  = $d0;
  M_RST1  = $d1;
  M_RST2  = $d2;
  M_RST3  = $d3;
  M_RST4  = $d4;
  M_RST5  = $d5;
  M_RST6  = $d6;
  M_RST7  = $d7;
  
  M_SOI   = $d8;
  M_EOI   = $d9;
  M_SOS   = $da;
  M_DQT   = $db;
  M_DNL   = $dc;
  M_DRI   = $dd;
  M_DHP   = $de;
  M_EXP   = $df;
  
  M_APP0  = $e0;
  M_APP1  = $e1;
  M_APP2  = $e2;
  M_APP3  = $e3;
  M_APP4  = $e4;
  M_APP5  = $e5;
  M_APP6  = $e6;
  M_APP7  = $e7;
  M_APP8  = $e8;
  M_APP9  = $e9;
  M_APP10 = $ea;
  M_APP11 = $eb;
  M_APP12 = $ec;
  M_APP13 = $ed;
  M_APP14 = $ee;
  M_APP15 = $ef;
  
  M_JPG0  = $f0;
  M_JPG13 = $fd;
  M_COM   = $fe;

  M_TEM   = $01;

  M_ERROR = $100;

type
  JPEG_MARKER = uint;        { JPEG marker codes }

{ Private state }

type
  my_marker_ptr = ^my_marker_reader;
  my_marker_reader = record
    pub : jpeg_marker_reader; { public fields }

    { Application-overridable marker processing methods }
    process_COM : jpeg_marker_parser_method;
    process_APPn : array[0..16-1] of jpeg_marker_parser_method;

    { Limit on marker data length to save for each marker type }
    length_limit_COM : uint;
    length_limit_APPn : array[0..16-1] of uint;

    { Status of COM/APPn marker saving }
    cur_marker : jpeg_saved_marker_ptr;	{ NIL if not processing a marker }
    bytes_read : uint;		{ data bytes read so far in marker }
    { Note: cur_marker is not linked into marker_list until it's all read. }
  end;

{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);

{$ifdef SAVE_MARKERS_SUPPORTED}

{GLOBAL}
procedure jpeg_save_markers (cinfo : j_decompress_ptr;
                             marker_code : int;
		             length_limit : uint);
{$ENDIF}

{GLOBAL}
procedure jpeg_set_marker_processor (cinfo : j_decompress_ptr;
                                     marker_code : int;
	                             routine : jpeg_marker_parser_method);

implementation

uses
  imjutils;

{ At all times, cinfo1.src.next_input_byte and .bytes_in_buffer reflect
  the current restart point; we update them only when we have reached a
  suitable place to restart if a suspension occurs. }


{ Routines to process JPEG markers.

  Entry condition: JPEG marker itself has been read and its code saved
    in cinfo^.unread_marker; input restart point is just after the marker.

  Exit: if return TRUE, have read and processed any parameters, and have
    updated the restart point to point after the parameters.
    If return FALSE, was forced to suspend before reaching end of
    marker parameters; restart point has not been moved.  Same routine
    will be called again after application supplies more input data.

  This approach to suspension assumes that all of a marker's parameters
  can fit into a single input bufferload.  This should hold for "normal"
  markers.  Some COM/APPn markers might have large parameter segments
  that might not fit.  If we are simply dropping such a marker, we use
  skip_input_data to get past it, and thereby put the problem on the
  source manager's shoulders.  If we are saving the marker's contents
  into memory, we use a slightly different convention: when forced to
  suspend, the marker processor updates the restart point to the end of
  what it's consumed (ie, the end of the buffer) before returning FALSE.
  On resumption, cinfo->unread_marker still contains the marker code,
  but the data source will point to the next chunk of marker data.
  The marker processor must retain internal state to deal with this.

  Note that we don't bother to avoid duplicate trace messages if a
  suspension occurs within marker parameters.  Other side effects
  require more care. }

{LOCAL}
function get_soi (cinfo : j_decompress_ptr) : boolean;
{ Process an SOI marker }
var
  i : int;
begin
  {$IFDEF DEBUG}
  TRACEMS(j_common_ptr(cinfo), 1, JTRC_SOI);
  {$ENDIF}

  if (cinfo^.marker^.saw_SOI) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOI_DUPLICATE);

  { Reset all parameters that are defined to be reset by SOI }

  for i := 0 to Pred(NUM_ARITH_TBLS) do
  with cinfo^ do
  begin
    arith_dc_L[i] := 0;
    arith_dc_U[i] := 1;
    arith_ac_K[i] := 5;
  end;
  cinfo^.restart_interval := 0;

  { Set initial assumptions for colorspace etc }

  with cinfo^ do
  begin
    jpeg_color_space := JCS_UNKNOWN;
    CCIR601_sampling := FALSE; { Assume non-CCIR sampling??? }

    saw_JFIF_marker := FALSE;
    JFIF_major_version := 1; { set default JFIF APP0 values }
    JFIF_minor_version := 1;
    density_unit := 0;
    X_density := 1;
    Y_density := 1;
    saw_Adobe_marker := FALSE;
    Adobe_transform := 0;

    marker^.saw_SOI := TRUE;
  end;
  get_soi := TRUE;
end; { get_soi }


{LOCAL}
function get_sof(cinfo : j_decompress_ptr;
                 is_prog : boolean;
                 is_arith : boolean) : boolean;
{ Process a SOFn marker }
var
  length : INT32;
  c, ci : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;
{}
  cinfo^.progressive_mode := is_prog;
  cinfo^.arith_code := is_arith;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable cinfo^.data_precision.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.data_precision := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_height should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_height := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_height, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

{ Read two bytes interpreted as an unsigned 16-bit integer.
  cinfo^.image_width should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.image_width := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( cinfo^.image_width, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  { Read a byte into variable cinfo^.num_components.
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sof := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  cinfo^.num_components := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  Dec(length, 8);

  {$IFDEF DEBUG}
  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF, cinfo^.unread_marker,
	   int(cinfo^.image_width), int(cinfo^.image_height),
	   cinfo^.num_components);
  {$ENDIF}

  if (cinfo^.marker^.saw_SOF) then
    ERREXIT(j_common_ptr(cinfo), JERR_SOF_DUPLICATE);

  { We don't support files in which the image height is initially specified }
  { as 0 and is later redefined by DNL.  As long as we have to check that,  }
  { might as well have a general sanity check. }
  if (cinfo^.image_height <= 0) or (cinfo^.image_width <= 0)
      or (cinfo^.num_components <= 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_EMPTY_IMAGE);

  if (length <> (cinfo^.num_components * 3)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  if (cinfo^.comp_info = NIL) then { do only once, even if suspend }
    cinfo^.comp_info := jpeg_component_info_list_ptr(
     cinfo^.mem^.alloc_small(j_common_ptr(cinfo), JPOOL_IMAGE,
                  cinfo^.num_components * SIZEOF(jpeg_component_info)));

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    compptr^.component_index := ci;

    { Read a byte into variable compptr^.component_id.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.component_id := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr^.h_samp_factor := (c shr 4) and 15;
    compptr^.v_samp_factor := (c      ) and 15;

    { Read a byte into variable compptr^.quant_tbl_no.
      If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sof := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    compptr^.quant_tbl_no := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    {$IFDEF DEBUG}
    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOF_COMPONENT,
	     compptr^.component_id, compptr^.h_samp_factor,
	     compptr^.v_samp_factor, compptr^.quant_tbl_no);
    {$ENDIF}

    Inc(compptr);
  end;

  cinfo^.marker^.saw_SOF := TRUE;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sof := TRUE;
end;  { get_sof }


{LOCAL}
function get_sos (cinfo : j_decompress_ptr) : boolean;
{ Process a SOS marker }
label
  id_found;
var
  length : INT32;
  i, ci, n, c, cc : int;
  compptr : jpeg_component_info_ptr;
{ Declare and initialize local copies of input pointer/count }
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{}

  if not cinfo^.marker^.saw_SOF then
    ERREXIT(j_common_ptr(cinfo), JERR_SOS_NO_SOF);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );


  { Read a byte into variable n (Number of components).
    If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  n := GETJOCTET(next_input_byte^);  { Number of components }
  Inc(next_input_byte);

  {$IFDEF DEBUG}
  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_SOS, n);
  {$ENDIF}

  if ((length <> (n * 2 + 6)) or (n < 1) or (n > MAX_COMPS_IN_SCAN)) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  cinfo^.comps_in_scan := n;

  { Collect the component-spec parameters }

  for i := 0 to Pred(n) do
  begin
    { Read a byte into variable cc. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    cc := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_sos := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to Pred(cinfo^.num_components) do
    begin
      if (cc = compptr^.component_id) then
	goto id_found;
      Inc(compptr);
    end;

    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_COMPONENT_ID, cc);

  id_found:

    cinfo^.cur_comp_info[i] := compptr;
    compptr^.dc_tbl_no := (c shr 4) and 15;
    compptr^.ac_tbl_no := (c      ) and 15;

    {$IFDEF DEBUG}
    TRACEMS3(j_common_ptr(cinfo), 1, JTRC_SOS_COMPONENT, cc,
	     compptr^.dc_tbl_no, compptr^.ac_tbl_no);
    {$ENDIF}
  end;

  { Collect the additional scan parameters Ss, Se, Ah/Al. }
  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ss := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Se := c;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_sos := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  cinfo^.Ah := (c shr 4) and 15;
  cinfo^.Al := (c     ) and 15;

  {$IFDEF DEBUG}
  TRACEMS4(j_common_ptr(cinfo), 1, JTRC_SOS_PARAMS, cinfo^.Ss, cinfo^.Se,
	   cinfo^.Ah, cinfo^.Al);
  {$ENDIF}

  { Prepare to scan data & restart markers }
  cinfo^.marker^.next_restart_num := 0;

  { Count another SOS marker }
  Inc( cinfo^.input_scan_number );

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_sos := TRUE;
end;  { get_sos }


{METHODDEF}
function skip_variable (cinfo : j_decompress_ptr) : boolean;  
{ Skip over an unknown or uninteresting variable-length marker }
var
  length : INT32;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;    { Array[] of JOCTET; }
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      skip_variable := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := uint(GETJOCTET(next_input_byte^)) shl 8;
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      skip_variable := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  Inc( length, GETJOCTET(next_input_byte^));
  Inc( next_input_byte );

  Dec(length, 2);

  {$IFDEF DEBUG}
  TRACEMS2(j_common_ptr(cinfo), 1, JTRC_MISC_MARKER,
    cinfo^.unread_marker, int(length));
  {$ENDIF}

  { Unload the local copies --- do this only at a restart boundary }
  { do before skip_input_data }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data(cinfo, long(length));

  skip_variable := TRUE;
end;  { skip_variable }


{$IFDEF D_ARITH_CODING_SUPPORTED}

{LOCAL}
function get_dac (cinfo : j_decompress_ptr) : boolean;
{ Process a DAC marker }
var
  length : INT32;
  index, val : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dac := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 0) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    { Read a byte into variable val. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dac := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    val := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    Dec( length, 2);

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DAC, index, val);
    {$ENDIF}

    if (index < 0) or (index >= (2*NUM_ARITH_TBLS)) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_INDEX, index);

    if (index >= NUM_ARITH_TBLS) then
    begin { define AC table }
      cinfo^.arith_ac_K[index-NUM_ARITH_TBLS] := UINT8(val);
    end
    else
    begin { define DC table }
      cinfo^.arith_dc_L[index] := UINT8(val and $0F);
      cinfo^.arith_dc_U[index] := UINT8(val shr 4);
      if (cinfo^.arith_dc_L[index] > cinfo^.arith_dc_U[index]) then
	ERREXIT1(j_common_ptr(cinfo) , JERR_DAC_VALUE, val);
    end;
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dac := TRUE;
end;  { get_dac }

{$ELSE}

{LOCAL}
function get_dac (cinfo : j_decompress_ptr) : boolean;
begin
  get_dac := skip_variable(cinfo);
end;

{$ENDIF}

{LOCAL}
function get_dht (cinfo : j_decompress_ptr) : boolean;
{ Process a DHT marker }
var
  length : INT32;
  bits : Array[0..17-1] of UINT8;
  huffval : Array[0..256-1] of UINT8;
  i, index, count : int;
  htblptr : ^JHUFF_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dht := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec(length,  2);

  while (length > 16) do
  begin
    { Read a byte into variable index. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dht := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    index := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DHT, index);
    {$ENDIF}

    bits[0] := 0;
    count := 0;
    for i := 1 to 16 do
    begin
      { Read a byte into variable bits[i]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      bits[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

      Inc( count, bits[i] );
    end;

    Dec( length, (1 + 16) );

    {$IFDEF DEBUG}
    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[1], bits[2], bits[3], bits[4],
	     bits[5], bits[6], bits[7], bits[8]);
    TRACEMS8(j_common_ptr(cinfo), 2, JTRC_HUFFBITS,
	     bits[9], bits[10], bits[11], bits[12],
	     bits[13], bits[14], bits[15], bits[16]);
    {$ENDIF}

    { Here we just do minimal validation of the counts to avoid walking
      off the end of our table space.  jdhuff.c will check more carefully. }

    if (count > 256) or (INT32(count) > length) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_HUFF_TABLE);

    for i := 0 to Pred(count) do
    begin
    { Read a byte into variable huffval[i]. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_dht := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      huffval[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;

    Dec( length, count );

    if (index and $10)<>0 then
    begin  { AC table definition }
      Dec( index, $10 );
      htblptr := @cinfo^.ac_huff_tbl_ptrs[index];
    end
    else
    begin { DC table definition }
      htblptr := @cinfo^.dc_huff_tbl_ptrs[index];
    end;

    if (index < 0) or (index >= NUM_HUFF_TBLS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_DHT_INDEX, index);

    if (htblptr^ = NIL) then
      htblptr^ := jpeg_alloc_huff_table(j_common_ptr(cinfo));

    MEMCOPY(@(htblptr^)^.bits, @bits, SIZEOF((htblptr^)^.bits));
    MEMCOPY(@(htblptr^)^.huffval, @huffval, SIZEOF((htblptr^)^.huffval));
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dht := TRUE;
end;  { get_dht }


{LOCAL}
function get_dqt (cinfo : j_decompress_ptr) : boolean;
{ Process a DQT marker }
var
  length : INT32;
  n, i, prec : int;
  tmp : uint;
  quant_ptr : JQUANT_TBL_PTR;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dqt := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  Dec( length, 2 );

  while (length > 0) do
  begin
    { Read a byte into variable n. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dqt := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    n := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

    prec := n shr 4;
    n := n and $0F;

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_DQT, n, prec);
    {$ENDIF}

    if (n >= NUM_QUANT_TBLS) then
      ERREXIT1(j_common_ptr(cinfo) , JERR_DQT_INDEX, n);

    if (cinfo^.quant_tbl_ptrs[n] = NIL) then
      cinfo^.quant_tbl_ptrs[n] := jpeg_alloc_quant_table(j_common_ptr(cinfo));
    quant_ptr := cinfo^.quant_tbl_ptrs[n];

    for i := 0 to Pred(DCTSIZE2) do
    begin
      if (prec <> 0) then
      begin
      { Read two bytes interpreted as an unsigned 16-bit integer.
        tmp should be declared unsigned int or perhaps INT32. }

      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
        Inc( next_input_byte );
        { make a byte available.
          Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
          but we must reload the local copies after a successful fill. }
          if (bytes_in_buffer = 0) then
          begin
            if (not datasrc^.fill_input_buffer(cinfo)) then
            begin
              get_dqt := FALSE;
              exit;
            end;
            { Reload the local copies }
            next_input_byte := datasrc^.next_input_byte;
            bytes_in_buffer := datasrc^.bytes_in_buffer;
          end;
          Dec( bytes_in_buffer );

        Inc( tmp, GETJOCTET( next_input_byte^));
        Inc( next_input_byte );

      end
      else
      begin
      { Read a byte into variable tmp. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
        if (bytes_in_buffer = 0) then
        begin
          if (not datasrc^.fill_input_buffer(cinfo)) then
          begin
            get_dqt := FALSE;
            exit;
          end;
          { Reload the local copies }
          next_input_byte := datasrc^.next_input_byte;
          bytes_in_buffer := datasrc^.bytes_in_buffer;
        end;
        Dec( bytes_in_buffer );

        tmp := GETJOCTET(next_input_byte^);
        Inc(next_input_byte);
      end;

      { We convert the zigzag-order table to natural array order. }
      quant_ptr^.quantval[jpeg_natural_order[i]] := UINT16(tmp);
    end;

    if (cinfo^.err^.trace_level >= 2) then
    begin
      i := 0;
      while i < Pred(DCTSIZE2) do
      begin
        {$IFDEF DEBUG}
	TRACEMS8(j_common_ptr(cinfo), 2, JTRC_QUANTVALS,
		 quant_ptr^.quantval[i],   quant_ptr^.quantval[i+1],
		 quant_ptr^.quantval[i+2], quant_ptr^.quantval[i+3],
		 quant_ptr^.quantval[i+4], quant_ptr^.quantval[i+5],
		 quant_ptr^.quantval[i+6], quant_ptr^.quantval[i+7]);
        {$ENDIF}
        Inc(i, 8);
      end;
    end;

    Dec( length, DCTSIZE2+1 );
    if (prec <> 0) then
      Dec( length, DCTSIZE2 );
  end;

  if (length <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dqt := TRUE;
end;  { get_dqt }


{LOCAL}
function get_dri (cinfo : j_decompress_ptr) : boolean;
{ Process a DRI marker }
var
  length : INT32;
  tmp : uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( length, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  if (length <> 4) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_LENGTH);

{ Read two bytes interpreted as an unsigned 16-bit integer.
  tmp should be declared unsigned int or perhaps INT32. }

{ make a byte available.
  Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
  but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_dri := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  tmp := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        get_dri := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

  Inc( tmp, GETJOCTET( next_input_byte^));
  Inc( next_input_byte );

  {$IFDEF DEBUG}
  TRACEMS1(j_common_ptr(cinfo), 1, JTRC_DRI, tmp);
  {$ENDIF}

  cinfo^.restart_interval := tmp;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  get_dri := TRUE;
end;  { get_dri }


{ Routines for processing APPn and COM markers.
  These are either saved in memory or discarded, per application request.
  APP0 and APP14 are specially checked to see if they are
  JFIF and Adobe markers, respectively. }

const
  APP0_DATA_LEN	= 14;   { Length of interesting data in APP0 }
  APP14_DATA_LEN = 12;  { Length of interesting data in APP14 }
  APPN_DATA_LEN = 14;   { Must be the largest of the above!! }


{LOCAL}
procedure examine_app0 (cinfo : j_decompress_ptr;
                        var data : array of JOCTET;
                        datalen : uint;
                        remaining : INT32);

{ Examine first few bytes from an APP0.
  Take appropriate action if it is a JFIF marker.
  datalen is # of bytes at data[], remaining is length of rest of marker data.
}
{$IFDEF DEBUG}
var
  totallen : INT32;
{$ENDIF}  
begin
  {$IFDEF DEBUG}
  totallen := INT32(datalen) + remaining;
  {$ENDIF}
  if (datalen >= APP0_DATA_LEN) and
     (GETJOCTET(data[0]) = $4A) and
     (GETJOCTET(data[1]) = $46) and
     (GETJOCTET(data[2]) = $49) and
     (GETJOCTET(data[3]) = $46) and
     (GETJOCTET(data[4]) = 0) then
  begin
    { Found JFIF APP0 marker: save info }
    cinfo^.saw_JFIF_marker := TRUE;
    cinfo^.JFIF_major_version := GETJOCTET(data[5]);
    cinfo^.JFIF_minor_version := GETJOCTET(data[6]);
    cinfo^.density_unit := GETJOCTET(data[7]);
    cinfo^.X_density := (GETJOCTET(data[8]) shl 8) + GETJOCTET(data[9]);
    cinfo^.Y_density := (GETJOCTET(data[10]) shl 8) + GETJOCTET(data[11]);
    { Check version.
      Major version must be 1, anything else signals an incompatible change.
      (We used to treat this as an error, but now it's a nonfatal warning,
      because some bozo at Hijaak couldn't read the spec.)
      Minor version should be 0..2, but process anyway if newer. }

    if (cinfo^.JFIF_major_version <> 1) then
      WARNMS2(j_common_ptr(cinfo), JWRN_JFIF_MAJOR,
	      cinfo^.JFIF_major_version, cinfo^.JFIF_minor_version);
    { Generate trace messages }
    {$IFDEF DEBUG}
    TRACEMS5(j_common_ptr(cinfo), 1, JTRC_JFIF,
	     cinfo^.JFIF_major_version, cinfo^.JFIF_minor_version,
	     cinfo^.X_density, cinfo^.Y_density, cinfo^.density_unit);
    { Validate thumbnail dimensions and issue appropriate messages }
    if (GETJOCTET(data[12]) or GETJOCTET(data[13])) <> 0 then
      TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_THUMBNAIL,
	       GETJOCTET(data[12]), GETJOCTET(data[13]));
    Dec(totallen, APP0_DATA_LEN);
    if (totallen <>
	( INT32(GETJOCTET(data[12])) * INT32(GETJOCTET(data[13])) * INT32(3) )) then
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_JFIF_BADTHUMBNAILSIZE, int(totallen));
    {$ENDIF}
  end
  else
    if (datalen >= 6) and
      (GETJOCTET(data[0]) = $4A) and
      (GETJOCTET(data[1]) = $46) and
      (GETJOCTET(data[2]) = $58) and
      (GETJOCTET(data[3]) = $58) and
      (GETJOCTET(data[4]) = 0) then
    begin
    { Found JFIF "JFXX" extension APP0 marker }
    { The library doesn't actually do anything with these,
      but we try to produce a helpful trace message. }
      {$IFDEF DEBUG}
      case (GETJOCTET(data[5])) of
        $10:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_JPEG, int(totallen));
        $11:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_PALETTE, int(totallen));
        $13:
          TRACEMS1(j_common_ptr(cinfo), 1, JTRC_THUMB_RGB, int(totallen));
        else
          TRACEMS2(j_common_ptr(cinfo), 1, JTRC_JFIF_EXTENSION,
	           GETJOCTET(data[5]), int(totallen));
      end;
      {$ENDIF}
    end
    else
    begin
      { Start of APP0 does not match "JFIF" or "JFXX", or too short }
      {$IFDEF DEBUG}
      TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP0, int(totallen));
      {$ENDIF}
    end;
end;


{LOCAL}
procedure examine_app14 (cinfo : j_decompress_ptr;
                         var data : array of JOCTET;
	                 datalen : uint;
                         remaining : INT32);
{ Examine first few bytes from an APP14.
  Take appropriate action if it is an Adobe marker.
  datalen is # of bytes at data[], remaining is length of rest of marker data.
 }
var
  {$IFDEF DEBUG}
  version, flags0, flags1,
  {$ENDIF}
  transform : uint;
begin
  if (datalen >= APP14_DATA_LEN) and
     (GETJOCTET(data[0]) = $41) and
     (GETJOCTET(data[1]) = $64) and
     (GETJOCTET(data[2]) = $6F) and
     (GETJOCTET(data[3]) = $62) and
     (GETJOCTET(data[4]) = $65) then
  begin
    { Found Adobe APP14 marker }
    {$IFDEF DEBUG}
    version := (GETJOCTET(data[5]) shl 8) + GETJOCTET(data[6]);
    flags0 := (GETJOCTET(data[7]) shl 8) + GETJOCTET(data[8]);
    flags1 := (GETJOCTET(data[9]) shl 8) + GETJOCTET(data[10]);
    {$ENDIF}
    transform := GETJOCTET(data[11]);
    {$IFDEF DEBUG}
    TRACEMS4(j_common_ptr(cinfo), 1, JTRC_ADOBE, version, flags0, flags1, transform);
    {$ENDIF}
    cinfo^.saw_Adobe_marker := TRUE;
    cinfo^.Adobe_transform := UINT8 (transform);
  end
  else
  begin
    { Start of APP14 does not match "Adobe", or too short }
    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 1, JTRC_APP14, int (datalen + remaining));
    {$ENDIF}
  end;
end;


{METHODDEF}
function get_interesting_appn (cinfo : j_decompress_ptr) : boolean;
{ Process an APP0 or APP14 marker without saving it }
var
  length : INT32;
  b : array[0..APPN_DATA_LEN-1] of JOCTET;
  i, numtoread: uint;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

{ Read two bytes interpreted as an unsigned 16-bit integer.
  length should be declared unsigned int or perhaps INT32. }

  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_interesting_appn := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  length := (uint( GETJOCTET(next_input_byte^)) shl 8);
  Inc( next_input_byte );

  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      get_interesting_appn := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  Inc( length, GETJOCTET(next_input_byte^));
  Inc( next_input_byte );

  Dec(length, 2);

  { get the interesting part of the marker data }
  if (length >= APPN_DATA_LEN) then
    numtoread := APPN_DATA_LEN
  else
    if (length > 0) then
      numtoread := uint(length)
    else
      numtoread := 0;

  if numtoread > 0 then
  begin
    for i := 0 to numtoread-1 do
    begin
    { Read a byte into b[i]. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          get_interesting_appn := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      b[i] := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    end;
  end;

  Dec(length, numtoread);

  { process it }
  case (cinfo^.unread_marker) of
  M_APP0:
    examine_app0(cinfo, b, numtoread, length);
  M_APP14:
    examine_app14(cinfo, b, numtoread, length);
  else
    { can't get here unless jpeg_save_markers chooses wrong processor }
    ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, cinfo^.unread_marker);
  end;

  { skip any remaining data -- could be lots }

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data(cinfo, long(length));

  get_interesting_appn := TRUE;
end;

{$ifdef SAVE_MARKERS_SUPPORTED}

{METHODDEF}
function save_marker (cinfo : j_decompress_ptr) : boolean;  
{ Save an APPn or COM marker into the marker list }
var
  marker : my_marker_ptr;
  cur_marker : jpeg_saved_marker_ptr;
  bytes_read, data_length : uint;
  data : JOCTET_FIELD_PTR;
  length : INT32;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
var
  limit : uint;
var
  prev : jpeg_saved_marker_ptr;
begin
  { local copies of input pointer/count }
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  marker := my_marker_ptr(cinfo^.marker);
  cur_marker := marker^.cur_marker;
  length := 0;

  if (cur_marker = NIL) then
  begin
    { begin reading a marker }
    { Read two bytes interpreted as an unsigned 16-bit integer. }

    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        save_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    length := (uint( GETJOCTET(next_input_byte^)) shl 8);
    Inc( next_input_byte );

    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        save_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    Inc( length, GETJOCTET(next_input_byte^));
    Inc( next_input_byte );

    Dec(length, 2);
    if (length >= 0) then
    begin		{ watch out for bogus length word }
      { figure out how much we want to save }

      if (cinfo^.unread_marker = int(M_COM)) then
	limit := marker^.length_limit_COM
      else
	limit := marker^.length_limit_APPn[cinfo^.unread_marker - int(M_APP0)];
      if (uint(length) < limit) then
	limit := uint(length);
      { allocate and initialize the marker item }
      cur_marker := jpeg_saved_marker_ptr(
	cinfo^.mem^.alloc_large (j_common_ptr(cinfo), JPOOL_IMAGE,
                               SIZEOF(jpeg_marker_struct) + limit) );
      cur_marker^.next := NIL;
      cur_marker^.marker := UINT8 (cinfo^.unread_marker);
      cur_marker^.original_length := uint(length);
      cur_marker^.data_length := limit;
      { data area is just beyond the jpeg_marker_struct }
      cur_marker^.data := JOCTET_FIELD_PTR(cur_marker);
      Inc(jpeg_saved_marker_ptr(cur_marker^.data));
      data := cur_marker^.data;

      marker^.cur_marker := cur_marker;
      marker^.bytes_read := 0;
      bytes_read := 0;
      data_length := limit;
    end
    else
    begin
      { deal with bogus length word }
      data_length := 0;
      bytes_read := 0;
      data := NIL;
    end
  end
  else
  begin
    { resume reading a marker }
    bytes_read := marker^.bytes_read;
    data_length := cur_marker^.data_length;
    data := cur_marker^.data;
    Inc(data, bytes_read);
  end;

  while (bytes_read < data_length) do
  begin
    { move the restart point to here }
    datasrc^.next_input_byte := next_input_byte;
    datasrc^.bytes_in_buffer := bytes_in_buffer;

    marker^.bytes_read := bytes_read;
    { If there's not at least one byte in buffer, suspend }
    if (bytes_in_buffer = 0) then
    begin
      if not datasrc^.fill_input_buffer (cinfo) then
      begin
        save_marker := FALSE;
        exit;
      end;
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;

    { Copy bytes with reasonable rapidity }
    while (bytes_read < data_length) and (bytes_in_buffer > 0) do
    begin
      JOCTETPTR(data)^ := next_input_byte^;
      Inc(JOCTETPTR(data));
      Inc(next_input_byte);
      Dec(bytes_in_buffer);
      Inc(bytes_read);
    end;
  end;

  { Done reading what we want to read }
  if (cur_marker <> NIL) then
  begin	{ will be NIL if bogus length word }
    { Add new marker to end of list }
    if (cinfo^.marker_list = NIL) then
    begin
      cinfo^.marker_list := cur_marker
    end
    else
    begin
      prev := cinfo^.marker_list;
      while (prev^.next <> NIL) do
	prev := prev^.next;
      prev^.next := cur_marker;
    end;
    { Reset pointer & calc remaining data length }
    data := cur_marker^.data;
    length := cur_marker^.original_length - data_length;
  end;
  { Reset to initial state for next marker }
  marker^.cur_marker := NIL;

  { Process the marker if interesting; else just make a generic trace msg }
  case (cinfo^.unread_marker) of
  M_APP0:
    examine_app0(cinfo, data^, data_length, length);
  M_APP14:
    examine_app14(cinfo, data^, data_length, length);
  else
    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 1, JTRC_MISC_MARKER, cinfo^.unread_marker,
	     int(data_length + length));
    {$ENDIF}
  end;

  { skip any remaining data -- could be lots }
  { do before skip_input_data }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  if (length > 0) then
    cinfo^.src^.skip_input_data (cinfo, long(length) );

  save_marker := TRUE;
end;

{$endif} { SAVE_MARKERS_SUPPORTED }


{ Find the next JPEG marker, save it in cinfo^.unread_marker.
  Returns FALSE if had to suspend before reaching a marker;
  in that case cinfo^.unread_marker is unchanged.

  Note that the result might not be a valid marker code,
  but it will never be 0 or FF. }

{LOCAL}
function next_marker (cinfo : j_decompress_ptr) : boolean;
var
  c : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  {while TRUE do}
  repeat
    { Read a byte into variable c. If must suspend, return FALSE. }
    { make a byte available.
      Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
      but we must reload the local copies after a successful fill. }
    if (bytes_in_buffer = 0) then
    begin
      if (not datasrc^.fill_input_buffer(cinfo)) then
      begin
        next_marker := FALSE;
        exit;
      end;
      { Reload the local copies }
      next_input_byte := datasrc^.next_input_byte;
      bytes_in_buffer := datasrc^.bytes_in_buffer;
    end;
    Dec( bytes_in_buffer );

    c := GETJOCTET(next_input_byte^);
    Inc(next_input_byte);

   { Skip any non-FF bytes.
     This may look a bit inefficient, but it will not occur in a valid file.
     We sync after each discarded byte so that a suspending data source
     can discard the byte from its buffer. }

    while (c <> $FF) do
    begin
      Inc(cinfo^.marker^.discarded_bytes);
      { Unload the local copies --- do this only at a restart boundary }
      datasrc^.next_input_byte := next_input_byte;
      datasrc^.bytes_in_buffer := bytes_in_buffer;

      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);

    end;
    { This loop swallows any duplicate FF bytes.  Extra FFs are legal as
      pad bytes, so don't count them in discarded_bytes.  We assume there
      will not be so many consecutive FF bytes as to overflow a suspending
      data source's input buffer. }

    repeat
      { Read a byte into variable c. If must suspend, return FALSE. }
      { make a byte available.
        Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
        but we must reload the local copies after a successful fill. }
      if (bytes_in_buffer = 0) then
      begin
        if (not datasrc^.fill_input_buffer(cinfo)) then
        begin
          next_marker := FALSE;
          exit;
        end;
        { Reload the local copies }
        next_input_byte := datasrc^.next_input_byte;
        bytes_in_buffer := datasrc^.bytes_in_buffer;
      end;
      Dec( bytes_in_buffer );

      c := GETJOCTET(next_input_byte^);
      Inc(next_input_byte);
    Until (c <> $FF);
    if (c <> 0) then
      break;			{ found a valid marker, exit loop }
    { Reach here if we found a stuffed-zero data sequence (FF/00).
      Discard it and loop back to try again. }

    Inc(cinfo^.marker^.discarded_bytes, 2);
    { Unload the local copies --- do this only at a restart boundary }
    datasrc^.next_input_byte := next_input_byte;
    datasrc^.bytes_in_buffer := bytes_in_buffer;
  Until False;

  if (cinfo^.marker^.discarded_bytes <> 0) then
  begin
    WARNMS2(j_common_ptr(cinfo), JWRN_EXTRANEOUS_DATA,
            cinfo^.marker^.discarded_bytes, c);
    cinfo^.marker^.discarded_bytes := 0;
  end;

  cinfo^.unread_marker := c;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  next_marker := TRUE;
end;  { next_marker }


{LOCAL}
function first_marker (cinfo : j_decompress_ptr) : boolean;
{ Like next_marker, but used to obtain the initial SOI marker. }
{ For this marker, we do not allow preceding garbage or fill; otherwise,
  we might well scan an entire input file before realizing it ain't JPEG.
  If an application wants to process non-JFIF files, it must seek to the
  SOI before calling the JPEG library. }
var
  c, c2 : int;
var
  datasrc : jpeg_source_mgr_ptr;
  next_input_byte : JOCTETptr;
  bytes_in_buffer : size_t;
begin
  datasrc := cinfo^.src;
  next_input_byte := datasrc^.next_input_byte;
  bytes_in_buffer := datasrc^.bytes_in_buffer;

  { Read a byte into variable c. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  { Read a byte into variable c2. If must suspend, return FALSE. }
  { make a byte available.
    Note we do *not* do INPUT_SYNC before calling fill_input_buffer,
    but we must reload the local copies after a successful fill. }
  if (bytes_in_buffer = 0) then
  begin
    if (not datasrc^.fill_input_buffer(cinfo)) then
    begin
      first_marker := FALSE;
      exit;
    end;
    { Reload the local copies }
    next_input_byte := datasrc^.next_input_byte;
    bytes_in_buffer := datasrc^.bytes_in_buffer;
  end;
  Dec( bytes_in_buffer );

  c2 := GETJOCTET(next_input_byte^);
  Inc(next_input_byte);

  if (c <> $FF) or (c2 <> int(M_SOI)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_NO_SOI, c, c2);

  cinfo^.unread_marker := c2;

  { Unload the local copies --- do this only at a restart boundary }
  datasrc^.next_input_byte := next_input_byte;
  datasrc^.bytes_in_buffer := bytes_in_buffer;

  first_marker := TRUE;
end;  { first_marker }


{ Read markers until SOS or EOI.

  Returns same codes as are defined for jpeg_consume_input:
  JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.   }

{METHODDEF}
function read_markers (cinfo : j_decompress_ptr) : int;  
begin
  { Outer loop repeats once for each marker. }
  repeat
    { Collect the marker proper, unless we already did. }
    { NB: first_marker() enforces the requirement that SOI appear first. }
    if (cinfo^.unread_marker = 0) then
    begin
      if not cinfo^.marker^.saw_SOI then
      begin
        if not first_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end
      else
      begin
        if not next_marker(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      end;
    end;
    { At this point cinfo^.unread_marker contains the marker code and the
      input point is just past the marker proper, but before any parameters.
      A suspension will cause us to return with this state still true. }

    case (cinfo^.unread_marker) of
      M_SOI:
        if not get_soi(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF0,             { Baseline }
      M_SOF1:             { Extended sequential, Huffman }
        if not get_sof(cinfo, FALSE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;
      M_SOF2:                     { Progressive, Huffman }
        if not get_sof(cinfo, TRUE, FALSE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF9:                     { Extended sequential, arithmetic }
        if not get_sof(cinfo, FALSE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_SOF10:                    { Progressive, arithmetic }
        if not get_sof(cinfo, TRUE, TRUE) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      { Currently unsupported SOFn types }
      M_SOF3,                     { Lossless, Huffman }
      M_SOF5,                     { Differential sequential, Huffman }
      M_SOF6,                     { Differential progressive, Huffman }
      M_SOF7,                     { Differential lossless, Huffman }
      M_JPG,                      { Reserved for JPEG extensions }
      M_SOF11,                    { Lossless, arithmetic }
      M_SOF13,                    { Differential sequential, arithmetic }
      M_SOF14,                    { Differential progressive, arithmetic }
      M_SOF15:                    { Differential lossless, arithmetic }
        ERREXIT1(j_common_ptr(cinfo), JERR_SOF_UNSUPPORTED, cinfo^.unread_marker);

      M_SOS:
        begin
          if not get_sos(cinfo) then
          begin
            read_markers := JPEG_SUSPENDED;
            exit;
          end;
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_SOS;
          exit;
        end;

      M_EOI:
        begin
          {$IFDEF DEBUG}
          TRACEMS(j_common_ptr(cinfo), 1, JTRC_EOI);
          {$ENDIF}
          cinfo^.unread_marker := 0;       { processed the marker }
          read_markers := JPEG_REACHED_EOI;
          exit;
        end;

      M_DAC:
        if not get_dac(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DHT:
        if not get_dht(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DQT:
        if not get_dqt(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_DRI:
        if not get_dri(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_APP0,
      M_APP1,
      M_APP2,
      M_APP3,
      M_APP4,
      M_APP5,
      M_APP6,
      M_APP7,
      M_APP8,
      M_APP9,
      M_APP10,
      M_APP11,
      M_APP12,
      M_APP13,
      M_APP14,
      M_APP15:
        if not my_marker_ptr(cinfo^.marker)^.
                process_APPn[cinfo^.unread_marker - int(M_APP0)](cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_COM:
        if not my_marker_ptr(cinfo^.marker)^.process_COM (cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      M_RST0,		{ these are all parameterless }
      M_RST1,
      M_RST2,
      M_RST3,
      M_RST4,
      M_RST5,
      M_RST6,
      M_RST7,
      M_TEM:
        {$IFDEF DEBUG}
        TRACEMS1(j_common_ptr(cinfo), 1, JTRC_PARMLESS_MARKER,
          cinfo^.unread_marker)
        {$ENDIF}
        ;

      M_DNL:		{ Ignore DNL ... perhaps the wrong thing }
        if not skip_variable(cinfo) then
        begin
          read_markers := JPEG_SUSPENDED;
          exit;
        end;

      else			{ must be DHP, EXP, JPGn, or RESn }
        { For now, we treat the reserved markers as fatal errors since they are
          likely to be used to signal incompatible JPEG Part 3 extensions.
          Once the JPEG 3 version-number marker is well defined, this code
          ought to change! }
        ERREXIT1(j_common_ptr(cinfo) , JERR_UNKNOWN_MARKER,
          cinfo^.unread_marker);
    end; { end of case }
    { Successfully processed marker, so reset state variable }
    cinfo^.unread_marker := 0;
  Until false;
end;  { read_markers }


{ Read a restart marker, which is expected to appear next in the datastream;
  if the marker is not there, take appropriate recovery action.
  Returns FALSE if suspension is required.

  This is called by the entropy decoder after it has read an appropriate
  number of MCUs.  cinfo^.unread_marker may be nonzero if the entropy decoder
  has already read a marker from the data source.  Under normal conditions
  cinfo^.unread_marker will be reset to 0 before returning; if not reset,
  it holds a marker which the decoder will be unable to read past. }

{METHODDEF}
function read_restart_marker (cinfo : j_decompress_ptr) :boolean;  
begin
  { Obtain a marker unless we already did. }
  { Note that next_marker will complain if it skips any data. }
  if (cinfo^.unread_marker = 0) then
  begin
    if not next_marker(cinfo) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  if (cinfo^.unread_marker = (int(M_RST0) + cinfo^.marker^.next_restart_num)) then
  begin
    { Normal case --- swallow the marker and let entropy decoder continue }
    {$IFDEF DEBUG}
    TRACEMS1(j_common_ptr(cinfo), 3, JTRC_RST,
      cinfo^.marker^.next_restart_num);
    {$ENDIF}
    cinfo^.unread_marker := 0;
  end
  else
  begin
    { Uh-oh, the restart markers have been messed up. }
    { Let the data source manager determine how to resync. }
    if not cinfo^.src^.resync_to_restart(cinfo,
              cinfo^.marker^.next_restart_num) then
    begin
      read_restart_marker := FALSE;
      exit;
    end;
  end;

  { Update next-restart state }
  with cinfo^.marker^ do
    next_restart_num := (next_restart_num + 1) and 7;

  read_restart_marker := TRUE;
end; { read_restart_marker }


{ This is the default resync_to_restart method for data source managers
  to use if they don't have any better approach.  Some data source managers
  may be able to back up, or may have additional knowledge about the data
  which permits a more intelligent recovery strategy; such managers would
  presumably supply their own resync method.

  read_restart_marker calls resync_to_restart if it finds a marker other than
  the restart marker it was expecting.  (This code is *not* used unless
  a nonzero restart interval has been declared.)  cinfo^.unread_marker is
  the marker code actually found (might be anything, except 0 or FF).
  The desired restart marker number (0..7) is passed as a parameter.
  This routine is supposed to apply whatever error recovery strategy seems
  appropriate in order to position the input stream to the next data segment.
  Note that cinfo^.unread_marker is treated as a marker appearing before
  the current data-source input point; usually it should be reset to zero
  before returning.
  Returns FALSE if suspension is required.

  This implementation is substantially constrained by wanting to treat the
  input as a data stream; this means we can't back up.  Therefore, we have
  only the following actions to work with:
    1. Simply discard the marker and let the entropy decoder resume at next
       byte of file.
    2. Read forward until we find another marker, discarding intervening
       data.  (In theory we could look ahead within the current bufferload,
       without having to discard data if we don't find the desired marker.
       This idea is not implemented here, in part because it makes behavior
       dependent on buffer size and chance buffer-boundary positions.)
    3. Leave the marker unread (by failing to zero cinfo^.unread_marker).
       This will cause the entropy decoder to process an empty data segment,
       inserting dummy zeroes, and then we will reprocess the marker.

  #2 is appropriate if we think the desired marker lies ahead, while #3 is
  appropriate if the found marker is a future restart marker (indicating
  that we have missed the desired restart marker, probably because it got
  corrupted).
  We apply #2 or #3 if the found marker is a restart marker no more than
  two counts behind or ahead of the expected one.  We also apply #2 if the
  found marker is not a legal JPEG marker code (it's certainly bogus data).
  If the found marker is a restart marker more than 2 counts away, we do #1
  (too much risk that the marker is erroneous; with luck we will be able to
  resync at some future point).
  For any valid non-restart JPEG marker, we apply #3.  This keeps us from
  overrunning the end of a scan.  An implementation limited to single-scan
  files might find it better to apply #2 for markers other than EOI, since
  any other marker would have to be bogus data in that case. }


{GLOBAL}
function jpeg_resync_to_restart(cinfo : j_decompress_ptr;
                                desired : int) : boolean;
var
  marker : int;
  action : int;
begin
  marker := cinfo^.unread_marker;
  //action := 1;     { never used }
  { Always put up a warning. }
  WARNMS2(j_common_ptr(cinfo), JWRN_MUST_RESYNC, marker, desired);

  { Outer loop handles repeated decision after scanning forward. }
  repeat
    if (marker < int(M_SOF0)) then
      action := 2                { invalid marker }
    else
      if (marker < int(M_RST0)) or (marker > int(M_RST7)) then
        action := 3                { valid non-restart marker }
      else
      begin
        if (marker = (int(M_RST0) + ((desired+1) and 7))) or
           (marker = (int(M_RST0) + ((desired+2) and 7))) then
          action := 3              { one of the next two expected restarts }
        else
          if (marker = (int(M_RST0) + ((desired-1) and 7))) or
             (marker = (int(M_RST0) + ((desired-2) and 7))) then
            action := 2            { a prior restart, so advance }
          else
            action := 1;           { desired restart or too far away }
      end;

    {$IFDEF DEBUG}
    TRACEMS2(j_common_ptr(cinfo), 4, JTRC_RECOVERY_ACTION, marker, action);
    {$ENDIF}
    case action of
    1:
      { Discard marker and let entropy decoder resume processing. }
      begin
        cinfo^.unread_marker := 0;
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    2:
      { Scan to the next marker, and repeat the decision loop. }
      begin
        if not next_marker(cinfo) then
        begin
          jpeg_resync_to_restart := FALSE;
          exit;
        end;
        marker := cinfo^.unread_marker;
      end;
    3:
      { Return without advancing past this marker. }
      { Entropy decoder will be forced to process an empty segment. }
      begin
        jpeg_resync_to_restart := TRUE;
        exit;
      end;
    end; { case }
  Until false; { end loop }
end;  { jpeg_resync_to_restart }


{ Reset marker processing state to begin a fresh datastream. }

{METHODDEF}
procedure reset_marker_reader (cinfo : j_decompress_ptr);  
var
  marker : my_marker_ptr;
begin
  marker := my_marker_ptr (cinfo^.marker);
  with cinfo^ do
  begin
    comp_info := NIL;            { until allocated by get_sof }
    input_scan_number := 0;      { no SOS seen yet }
    unread_marker := 0;          { no pending marker }
  end;
  marker^.pub.saw_SOI := FALSE;    { set internal state too }
  marker^.pub.saw_SOF := FALSE;
  marker^.pub.discarded_bytes := 0;
  marker^.cur_marker := NIL;
end; { reset_marker_reader }


{ Initialize the marker reader module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_marker_reader (cinfo : j_decompress_ptr);
var
  marker : my_marker_ptr;
  i : int;
begin
  { Create subobject in permanent pool }
  marker := my_marker_ptr(
     cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                             SIZEOF(my_marker_reader))
                                    );
  cinfo^.marker := jpeg_marker_reader_ptr(marker);
  { Initialize method pointers }
  marker^.pub.reset_marker_reader := reset_marker_reader;
  marker^.pub.read_markers := read_markers;
  marker^.pub.read_restart_marker := read_restart_marker;
  { Initialize COM/APPn processing.
    By default, we examine and then discard APP0 and APP14,
    but simply discard COM and all other APPn. }

  marker^.process_COM := skip_variable;
  marker^.length_limit_COM := 0;
  for i := 0 to 16-1 do
  begin
    marker^.process_APPn[i] := skip_variable;
    marker^.length_limit_APPn[i] := 0;
  end;
  marker^.process_APPn[0] := get_interesting_appn;
  marker^.process_APPn[14] := get_interesting_appn;
  { Reset marker processing state }
  reset_marker_reader(cinfo);
end; { jinit_marker_reader }


{ Control saving of COM and APPn markers into marker_list. }


{$ifdef SAVE_MARKERS_SUPPORTED}

{GLOBAL}
procedure jpeg_save_markers (cinfo : j_decompress_ptr;
                             marker_code : int;
		             length_limit : uint);
var
  marker : my_marker_ptr;
  maxlength : long;
  processor : jpeg_marker_parser_method;
begin
  marker := my_marker_ptr (cinfo^.marker);

  { Length limit mustn't be larger than what we can allocate
    (should only be a concern in a 16-bit environment). }

  maxlength := cinfo^.mem^.max_alloc_chunk - SIZEOF(jpeg_marker_struct);
  if (long(length_limit) > maxlength) then
    length_limit := uint(maxlength);

  { Choose processor routine to use.
    APP0/APP14 have special requirements. }

  if (length_limit <> 0) then
  begin
    processor := save_marker;
    { If saving APP0/APP14, save at least enough for our internal use. }
    if (marker_code = int(M_APP0)) and (length_limit < APP0_DATA_LEN) then
      length_limit := APP0_DATA_LEN
    else
      if (marker_code = int(M_APP14)) and (length_limit < APP14_DATA_LEN) then
        length_limit := APP14_DATA_LEN;
  end
  else
  begin
    processor := skip_variable;
    { If discarding APP0/APP14, use our regular on-the-fly processor. }
    if (marker_code = int(M_APP0)) or (marker_code = int(M_APP14)) then
      processor := get_interesting_appn;
  end;

  if (marker_code = int(M_COM)) then
  begin
    marker^.process_COM := processor;
    marker^.length_limit_COM := length_limit;
  end
  else
    if (marker_code >= int(M_APP0)) and (marker_code <= int(M_APP15)) then
    begin
      marker^.process_APPn[marker_code - int(M_APP0)] := processor;
      marker^.length_limit_APPn[marker_code - int(M_APP0)] := length_limit;
    end
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, marker_code);
end;

{$endif} { SAVE_MARKERS_SUPPORTED }

{ Install a special processing method for COM or APPn markers. }

{GLOBAL}

procedure jpeg_set_marker_processor (cinfo : j_decompress_ptr;
                                     marker_code : int;
			             routine : jpeg_marker_parser_method);
var
  marker : my_marker_ptr;
begin
  marker := my_marker_ptr (cinfo^.marker);
  if (marker_code = int(M_COM)) then
    marker^.process_COM := routine
  else
    if (marker_code >= int(M_APP0)) and (marker_code <= int(M_APP15)) then
      marker^.process_APPn[marker_code - int(M_APP0)] := routine
    else
      ERREXIT1(j_common_ptr(cinfo), JERR_UNKNOWN_MARKER, marker_code);
end;

end.
