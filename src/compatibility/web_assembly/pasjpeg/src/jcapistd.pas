Unit JcAPIstd;

{ Original : jcapistd.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains application interface code for the compression half
  of the JPEG library.  These are the "standard" API routines that are
  used in the normal full-compression case.  They are not used by a
  transcoding-only application.  Note that if an application links in
  jpeg_start_compress, it will end up linking in the entire compressor.
  We thus must separate this file from jcapimin.c to avoid linking the
  whole compression library into a transcoder. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib,
  jcapimin, jcinit;



{ Compression initialization.
  Before calling this, all parameters and a data destination must be set up.

  We require a write_all_tables parameter as a failsafe check when writing
  multiple datastreams from the same compression object.  Since prior runs
  will have left all the tables marked sent_table=TRUE, a subsequent run
  would emit an abbreviated stream (no tables) by default.  This may be what
  is wanted, but for safety's sake it should not be the default behavior:
  programmers should have to make a deliberate choice to emit abbreviated
  images.  Therefore the documentation and examples should encourage people
  to pass write_all_tables=TRUE; then it will take active thought to do the
  wrong thing. }

{GLOBAL}
procedure jpeg_start_compress (cinfo : j_compress_ptr;
                               write_all_tables : boolean);


{ Write some scanlines of data to the JPEG compressor.

  The return value will be the number of lines actually written.
  This should be less than the supplied num_lines only in case that
  the data destination module has requested suspension of the compressor,
  or if more than image_height scanlines are passed in.

  Note: we warn about excess calls to jpeg_write_scanlines() since
  this likely signals an application programmer error.  However,
  excess scanlines passed in the last valid call are *silently* ignored,
  so that the application need not adjust num_lines for end-of-image
  when using a multiple-scanline buffer. }

{GLOBAL}
function jpeg_write_scanlines (cinfo : j_compress_ptr;
                              scanlines : JSAMPARRAY;
                              num_lines : JDIMENSION) : JDIMENSION;

{ Alternate entry point to write raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_write_raw_data (cinfo : j_compress_ptr;
                              data : JSAMPIMAGE;
                              num_lines : JDIMENSION) : JDIMENSION;

implementation

{ Compression initialization.
  Before calling this, all parameters and a data destination must be set up.

  We require a write_all_tables parameter as a failsafe check when writing
  multiple datastreams from the same compression object.  Since prior runs
  will have left all the tables marked sent_table=TRUE, a subsequent run
  would emit an abbreviated stream (no tables) by default.  This may be what
  is wanted, but for safety's sake it should not be the default behavior:
  programmers should have to make a deliberate choice to emit abbreviated
  images.  Therefore the documentation and examples should encourage people
  to pass write_all_tables=TRUE; then it will take active thought to do the
  wrong thing. }

{GLOBAL}
procedure jpeg_start_compress (cinfo : j_compress_ptr;
                               write_all_tables : boolean);
begin
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (write_all_tables) then
    jpeg_suppress_tables(cinfo, FALSE); { mark all tables to be written }

  { (Re)initialize error mgr and destination modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.dest^.init_destination (cinfo);
  { Perform master selection of active modules }
  jinit_compress_master(cinfo);
  { Set up for the first pass }
  cinfo^.master^.prepare_for_pass (cinfo);
  { Ready for application to drive first pass through jpeg_write_scanlines
    or jpeg_write_raw_data. }

  cinfo^.next_scanline := 0;
  if cinfo^.raw_data_in then
    cinfo^.global_state := CSTATE_RAW_OK
  else
    cinfo^.global_state := CSTATE_SCANNING;
end;


{ Write some scanlines of data to the JPEG compressor.

  The return value will be the number of lines actually written.
  This should be less than the supplied num_lines only in case that
  the data destination module has requested suspension of the compressor,
  or if more than image_height scanlines are passed in.

  Note: we warn about excess calls to jpeg_write_scanlines() since
  this likely signals an application programmer error.  However,
  excess scanlines passed in the last valid call are *silently* ignored,
  so that the application need not adjust num_lines for end-of-image
  when using a multiple-scanline buffer. }

{GLOBAL}
function jpeg_write_scanlines (cinfo : j_compress_ptr;
                              scanlines : JSAMPARRAY;
                              num_lines : JDIMENSION) : JDIMENSION;
var
  row_ctr, rows_left : JDIMENSION;
begin
  if (cinfo^.global_state <> CSTATE_SCANNING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.next_scanline >= cinfo^.image_height) then
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.next_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.image_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Give master control module another chance if this is first call to
    jpeg_write_scanlines.  This lets output of the frame/scan headers be
    delayed so that application can write COM, etc, markers between
    jpeg_start_compress and jpeg_write_scanlines. }
  if (cinfo^.master^.call_pass_startup) then
    cinfo^.master^.pass_startup (cinfo);

  { Ignore any extra scanlines at bottom of image. }
  rows_left := cinfo^.image_height - cinfo^.next_scanline;
  if (num_lines > rows_left) then
    num_lines := rows_left;

  row_ctr := 0;
  cinfo^.main^.process_data (cinfo, scanlines, {var}row_ctr, num_lines);
  Inc(cinfo^.next_scanline, row_ctr);
  jpeg_write_scanlines := row_ctr;
end;


{ Alternate entry point to write raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_write_raw_data (cinfo : j_compress_ptr;
                              data : JSAMPIMAGE;
                              num_lines : JDIMENSION) : JDIMENSION;
var
  lines_per_iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state <> CSTATE_RAW_OK) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.next_scanline >= cinfo^.image_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_write_raw_data := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long(cinfo^.next_scanline);
    cinfo^.progress^.pass_limit := long(cinfo^.image_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Give master control module another chance if this is first call to
    jpeg_write_raw_data.  This lets output of the frame/scan headers be
    delayed so that application can write COM, etc, markers between
    jpeg_start_compress and jpeg_write_raw_data. }

  if (cinfo^.master^.call_pass_startup) then
    cinfo^.master^.pass_startup (cinfo);

  { Verify that at least one iMCU row has been passed. }
  lines_per_iMCU_row := cinfo^.max_v_samp_factor * DCTSIZE;
  if (num_lines < lines_per_iMCU_row) then
    ERREXIT(j_common_ptr(cinfo), JERR_BUFFER_SIZE);

  { Directly compress the row. }
  if (not cinfo^.coef^.compress_data (cinfo, data)) then
  begin
    { If compressor did not consume the whole row, suspend processing. }
    jpeg_write_raw_data := 0;
    exit;
  end;

  { OK, we processed one iMCU row. }
  Inc(cinfo^.next_scanline, lines_per_iMCU_row);
  jpeg_write_raw_data := lines_per_iMCU_row;
end;

end.
