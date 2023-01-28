unit imjdapimin;

{ This file contains application interface code for the decompression half
  of the JPEG library.  These are the "minimum" API routines that may be
  needed in either the normal full-decompression case or the
  transcoding-only case.

  Most of the routines intended to be called directly by an application
  are in this file or in jdapistd.c.  But also see jcomapi.c for routines
  shared by compression and decompression, and jdtrans.c for the transcoding
  case. }

{ Original : jdapimin.c ;  Copyright (C) 1994-1998, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjpeglib,
  imjmemmgr, imjdmarker, imjdinput, imjcomapi;

{ Nomssi }
procedure jpeg_create_decompress(cinfo : j_decompress_ptr);

{ Initialization of a JPEG decompression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateDecompress (cinfo : j_decompress_ptr;
                                 version : int;
                                 structsize : size_t);

{ Destruction of a JPEG decompression object }

{GLOBAL}
procedure jpeg_destroy_decompress (cinfo : j_decompress_ptr);


{ Decompression startup: read start of JPEG datastream to see what's there.
  Need only initialize JPEG object and supply a data source before calling.

  This routine will read as far as the first SOS marker (ie, actual start of
  compressed data), and will save all tables and parameters in the JPEG
  object.  It will also initialize the decompression parameters to default
  values, and finally return JPEG_HEADER_OK.  On return, the application may
  adjust the decompression parameters and then call jpeg_start_decompress.
  (Or, if the application only wanted to determine the image parameters,
  the data need not be decompressed.  In that case, call jpeg_abort or
  jpeg_destroy to release any temporary space.)
  If an abbreviated (tables only) datastream is presented, the routine will
  return JPEG_HEADER_TABLES_ONLY upon reaching EOI.  The application may then
  re-use the JPEG object to read the abbreviated image datastream(s).
  It is unnecessary (but OK) to call jpeg_abort in this case.
  The JPEG_SUSPENDED return code only occurs if the data source module
  requests suspension of the decompressor.  In this case the application
  should load more source data and then re-call jpeg_read_header to resume
  processing.
  If a non-suspending data source is used and require_image is TRUE, then the
  return code need not be inspected since only JPEG_HEADER_OK is possible.

  This routine is now just a front end to jpeg_consume_input, with some
  extra error checking. }

{GLOBAL}
function jpeg_read_header (cinfo : j_decompress_ptr;
                           require_image : boolean) : int;

{ Consume data in advance of what the decompressor requires.
  This can be called at any time once the decompressor object has
  been created and a data source has been set up.

  This routine is essentially a state machine that handles a couple
  of critical state-transition actions, namely initial setup and
  transition from header scanning to ready-for-start_decompress.
  All the actual input is done via the input controller's consume_input
  method. }

{GLOBAL}
function jpeg_consume_input (cinfo : j_decompress_ptr) : int;

{ Have we finished reading the input file? }

{GLOBAL}
function jpeg_input_complete (cinfo : j_decompress_ptr) : boolean;

{ Is there more than one scan? }

{GLOBAL}
function jpeg_has_multiple_scans (cinfo : j_decompress_ptr) : boolean;


{ Finish JPEG decompression.

  This will normally just verify the file trailer and release temp storage.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_decompress (cinfo : j_decompress_ptr) : boolean;

implementation

procedure jpeg_create_decompress(cinfo : j_decompress_ptr);
begin
  jpeg_CreateDecompress(cinfo, JPEG_LIB_VERSION,
    size_t(sizeof(jpeg_decompress_struct)));
end;

{ Initialization of a JPEG decompression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateDecompress (cinfo : j_decompress_ptr;
                                 version : int;
                                 structsize : size_t);
var
  i : int;
var
  err : jpeg_error_mgr_ptr;
  client_data : voidp;
begin
  { Guard against version mismatches between library and caller. }
  cinfo^.mem := NIL;		{ so jpeg_destroy knows mem mgr not called }
  if (version <> JPEG_LIB_VERSION) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_LIB_VERSION, JPEG_LIB_VERSION, version);
  if (structsize <> SIZEOF(jpeg_decompress_struct)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_STRUCT_SIZE,
	     int(SIZEOF(jpeg_decompress_struct)), int(structsize));

  { For debugging purposes, we zero the whole master structure.
    But the application has already set the err pointer, and may have set
    client_data, so we have to save and restore those fields.
    Note: if application hasn't set client_data, tools like Purify may
    complain here. }
  begin
    err := cinfo^.err;
    client_data := cinfo^.client_data; { ignore Purify complaint here }
    MEMZERO(j_common_ptr(cinfo), SIZEOF(jpeg_decompress_struct));
    cinfo^.err := err;
    cinfo^.client_data := client_data;
  end;
  cinfo^.is_decompressor := TRUE;

  { Initialize a memory manager instance for this object }
  jinit_memory_mgr(j_common_ptr(cinfo));

  { Zero out pointers to permanent structures. }
  cinfo^.progress := NIL;
  cinfo^.src := NIL;

  for i := 0 to pred(NUM_QUANT_TBLS) do
    cinfo^.quant_tbl_ptrs[i] := NIL;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    cinfo^.dc_huff_tbl_ptrs[i] := NIL;
    cinfo^.ac_huff_tbl_ptrs[i] := NIL;
  end;

  { Initialize marker processor so application can override methods
    for COM, APPn markers before calling jpeg_read_header.  }
  cinfo^.marker_list := NIL;
  jinit_marker_reader(cinfo);

  { And initialize the overall input controller. }
  jinit_input_controller(cinfo);

  { OK, I'm ready }
  cinfo^.global_state := DSTATE_START;
end;


{ Destruction of a JPEG decompression object }

{GLOBAL}
procedure jpeg_destroy_decompress (cinfo : j_decompress_ptr);
begin
  jpeg_destroy(j_common_ptr(cinfo)); { use common routine }
end;


{ Abort processing of a JPEG decompression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_decompress (cinfo : j_decompress_ptr);
begin
  jpeg_abort(j_common_ptr(cinfo)); { use common routine }
end;


{ Set default decompression parameters. }

{LOCAL}
procedure default_decompress_parms (cinfo : j_decompress_ptr);
var
  cid0 : int;
  cid1 : int;
  cid2 : int;
begin
  { Guess the input colorspace, and set output colorspace accordingly. }
  { (Wish JPEG committee had provided a real way to specify this...) }
  { Note application may override our guesses. }
  case (cinfo^.num_components) of
  1: begin
       cinfo^.jpeg_color_space := JCS_GRAYSCALE;
       cinfo^.out_color_space := JCS_GRAYSCALE;
     end;

  3: begin
       if (cinfo^.saw_JFIF_marker) then
       begin
         cinfo^.jpeg_color_space := JCS_YCbCr; { JFIF implies YCbCr }
       end
       else
         if (cinfo^.saw_Adobe_marker) then
         begin
           case (cinfo^.Adobe_transform) of
           0: cinfo^.jpeg_color_space := JCS_RGB;
           1: cinfo^.jpeg_color_space := JCS_YCbCr;
           else
             begin
	       WARNMS1(j_common_ptr(cinfo), JWRN_ADOBE_XFORM, cinfo^.Adobe_transform);
               cinfo^.jpeg_color_space := JCS_YCbCr; { assume it's YCbCr }
             end;
           end;
         end
         else
         begin
           { Saw no special markers, try to guess from the component IDs }
           cid0 := cinfo^.comp_info^[0].component_id;
           cid1 := cinfo^.comp_info^[1].component_id;
           cid2 := cinfo^.comp_info^[2].component_id;

           if (cid0 = 1) and (cid1 = 2) and (cid2 = 3) then
	     cinfo^.jpeg_color_space := JCS_YCbCr { assume JFIF w/out marker }
           else
             if (cid0 = 82) and (cid1 = 71) and (cid2 = 66) then
               cinfo^.jpeg_color_space := JCS_RGB { ASCII 'R', 'G', 'B' }
             else
             begin
               {$IFDEF DEBUG}
	       TRACEMS3(j_common_ptr(cinfo), 1, JTRC_UNKNOWN_IDS, cid0, cid1, cid2);
               {$ENDIF}
               cinfo^.jpeg_color_space := JCS_YCbCr; { assume it's YCbCr }
             end;
         end;
       { Always guess RGB is proper output colorspace. }
       cinfo^.out_color_space := JCS_RGB;
     end;

  4: begin
       if (cinfo^.saw_Adobe_marker) then
       begin
         case (cinfo^.Adobe_transform) of
         0: cinfo^.jpeg_color_space := JCS_CMYK;
         2: cinfo^.jpeg_color_space := JCS_YCCK;
         else
           begin
             WARNMS1(j_common_ptr(cinfo), JWRN_ADOBE_XFORM, cinfo^.Adobe_transform);
             cinfo^.jpeg_color_space := JCS_YCCK; { assume it's YCCK }
           end;
         end;
       end
       else
       begin
         { No special markers, assume straight CMYK. }
         cinfo^.jpeg_color_space := JCS_CMYK;
       end;
       cinfo^.out_color_space := JCS_CMYK;
     end;

  else
    begin
      cinfo^.jpeg_color_space := JCS_UNKNOWN;
      cinfo^.out_color_space := JCS_UNKNOWN;
    end;
  end;

  { Set defaults for other decompression parameters. }
  cinfo^.scale_num := 1;		{ 1:1 scaling }
  cinfo^.scale_denom := 1;
  cinfo^.output_gamma := 1.0;
  cinfo^.buffered_image := FALSE;
  cinfo^.raw_data_out := FALSE;
  cinfo^.dct_method := JDCT_DEFAULT;
  cinfo^.do_fancy_upsampling := TRUE;
  cinfo^.do_block_smoothing := TRUE;
  cinfo^.quantize_colors := FALSE;
  { We set these in case application only sets quantize_colors. }
  cinfo^.dither_mode := JDITHER_FS;
{$ifdef QUANT_2PASS_SUPPORTED}
  cinfo^.two_pass_quantize := TRUE;
{$else}
  cinfo^.two_pass_quantize := FALSE;
{$endif}
  cinfo^.desired_number_of_colors := 256;
  cinfo^.colormap := NIL;
  { Initialize for no mode change in buffered-image mode. }
  cinfo^.enable_1pass_quant := FALSE;
  cinfo^.enable_external_quant := FALSE;
  cinfo^.enable_2pass_quant := FALSE;
end;


{ Decompression startup: read start of JPEG datastream to see what's there.
  Need only initialize JPEG object and supply a data source before calling.

  This routine will read as far as the first SOS marker (ie, actual start of
  compressed data), and will save all tables and parameters in the JPEG
  object.  It will also initialize the decompression parameters to default
  values, and finally return JPEG_HEADER_OK.  On return, the application may
  adjust the decompression parameters and then call jpeg_start_decompress.
  (Or, if the application only wanted to determine the image parameters,
  the data need not be decompressed.  In that case, call jpeg_abort or
  jpeg_destroy to release any temporary space.)
  If an abbreviated (tables only) datastream is presented, the routine will
  return JPEG_HEADER_TABLES_ONLY upon reaching EOI.  The application may then
  re-use the JPEG object to read the abbreviated image datastream(s).
  It is unnecessary (but OK) to call jpeg_abort in this case.
  The JPEG_SUSPENDED return code only occurs if the data source module
  requests suspension of the decompressor.  In this case the application
  should load more source data and then re-call jpeg_read_header to resume
  processing.
  If a non-suspending data source is used and require_image is TRUE, then the
  return code need not be inspected since only JPEG_HEADER_OK is possible.

  This routine is now just a front end to jpeg_consume_input, with some
  extra error checking. }

{GLOBAL}
function jpeg_read_header (cinfo : j_decompress_ptr;
                           require_image : boolean) : int;
var
  retcode : int;
begin
  if (cinfo^.global_state <> DSTATE_START) and
     (cinfo^.global_state <> DSTATE_INHEADER) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  retcode := jpeg_consume_input(cinfo);

  case (retcode) of
  JPEG_REACHED_SOS:
    retcode := JPEG_HEADER_OK;
  JPEG_REACHED_EOI:
    begin
      if (require_image) then   { Complain if application wanted an image }
        ERREXIT(j_common_ptr(cinfo), JERR_NO_IMAGE);
      { Reset to start state; it would be safer to require the application to
        call jpeg_abort, but we can't change it now for compatibility reasons.
        A side effect is to free any temporary memory (there shouldn't be any). }

      jpeg_abort(j_common_ptr(cinfo)); { sets state := DSTATE_START }
      retcode := JPEG_HEADER_TABLES_ONLY;
    end;
  JPEG_SUSPENDED: ;    { no work }
  end;

  jpeg_read_header := retcode;
end;


{ Consume data in advance of what the decompressor requires.
  This can be called at any time once the decompressor object has
  been created and a data source has been set up.

  This routine is essentially a state machine that handles a couple
  of critical state-transition actions, namely initial setup and
  transition from header scanning to ready-for-start_decompress.
  All the actual input is done via the input controller's consume_input
  method. }

{GLOBAL}
function jpeg_consume_input (cinfo : j_decompress_ptr) : int;
var
  retcode : int;
begin
  retcode := JPEG_SUSPENDED;

  { NB: every possible DSTATE value should be listed in this switch }

  if (cinfo^.global_state) = DSTATE_START then
  begin {work around the FALLTHROUGH}
    { Start-of-datastream actions: reset appropriate modules }
    cinfo^.inputctl^.reset_input_controller (cinfo);
    { Initialize application's data source module }
    cinfo^.src^.init_source (cinfo);
    cinfo^.global_state := DSTATE_INHEADER;
  end;

  case (cinfo^.global_state) of
  DSTATE_START,
  DSTATE_INHEADER:
    begin
      retcode := cinfo^.inputctl^.consume_input (cinfo);
      if (retcode = JPEG_REACHED_SOS) then
      begin { Found SOS, prepare to decompress }
        { Set up default parameters based on header data }
        default_decompress_parms(cinfo);
        { Set global state: ready for start_decompress }
        cinfo^.global_state := DSTATE_READY;
      end;
    end;
  DSTATE_READY:
    { Can't advance past first SOS until start_decompress is called }
    retcode := JPEG_REACHED_SOS;

  DSTATE_PRELOAD,
  DSTATE_PRESCAN,
  DSTATE_SCANNING,
  DSTATE_RAW_OK,
  DSTATE_BUFIMAGE,
  DSTATE_BUFPOST,
  DSTATE_STOPPING:
    retcode := cinfo^.inputctl^.consume_input (cinfo);
  else
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  end;
  jpeg_consume_input := retcode;
end;


{ Have we finished reading the input file? }

{GLOBAL}
function jpeg_input_complete (cinfo : j_decompress_ptr) : boolean;
begin
  { Check for valid jpeg object }
  if (cinfo^.global_state < DSTATE_START) or
     (cinfo^.global_state > DSTATE_STOPPING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  jpeg_input_complete := cinfo^.inputctl^.eoi_reached;
end;


{ Is there more than one scan? }

{GLOBAL}
function jpeg_has_multiple_scans (cinfo : j_decompress_ptr) : boolean;
begin
  { Only valid after jpeg_read_header completes }
  if (cinfo^.global_state < DSTATE_READY) or
     (cinfo^.global_state > DSTATE_STOPPING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  jpeg_has_multiple_scans := cinfo^.inputctl^.has_multiple_scans;
end;


{ Finish JPEG decompression.

  This will normally just verify the file trailer and release temp storage.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_decompress (cinfo : j_decompress_ptr) : boolean;
begin
  if ((cinfo^.global_state = DSTATE_SCANNING) or
      (cinfo^.global_state = DSTATE_RAW_OK) and (not cinfo^.buffered_image)) then
  begin
    { Terminate final pass of non-buffered mode }
    if (cinfo^.output_scanline < cinfo^.output_height) then
      ERREXIT(j_common_ptr(cinfo), JERR_TOO_LITTLE_DATA);
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.global_state := DSTATE_STOPPING;
  end
  else
    if (cinfo^.global_state = DSTATE_BUFIMAGE) then
    begin
      { Finishing after a buffered-image operation }
      cinfo^.global_state := DSTATE_STOPPING;
    end
    else
      if (cinfo^.global_state <> DSTATE_STOPPING) then
      begin
        { STOPPING := repeat call after a suspension, anything else is error }
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
      end;
  { Read until EOI }
  while (not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.inputctl^.consume_input (cinfo) = JPEG_SUSPENDED) then
    begin
      jpeg_finish_decompress := FALSE;	{ Suspend, come back later }
      exit;
    end;
  end;
  { Do final cleanup }
  cinfo^.src^.term_source (cinfo);
  { We can use jpeg_abort to release memory and reset global_state }
  jpeg_abort(j_common_ptr(cinfo));
  jpeg_finish_decompress := TRUE;
end;

end.
