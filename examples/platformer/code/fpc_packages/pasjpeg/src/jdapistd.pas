Unit JdAPIstd;

{ Original : jdapistd.c ;  Copyright (C) 1994-1996, Thomas G. Lane. }

{  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains application interface code for the decompression half
  of the JPEG library.  These are the "standard" API routines that are
  used in the normal full-decompression case.  They are not used by a
  transcoding-only application.  Note that if an application links in
  jpeg_start_decompress, it will end up linking in the entire decompressor.
  We thus must separate this file from jdapimin.c to avoid linking the
  whole decompression library into a transcoder. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib,
  jdmaster;

{ Read some scanlines of data from the JPEG decompressor.

  The return value will be the number of lines actually read.
  This may be less than the number requested in several cases,
  including bottom of image, data source suspension, and operating
  modes that emit multiple scanlines at a time.

  Note: we warn about excess calls to jpeg_read_scanlines() since
  this likely signals an application programmer error.  However,
  an oversize buffer (max_lines > scanlines remaining) is not an error. }

{GLOBAL}
function jpeg_read_scanlines (cinfo : j_decompress_ptr;
                              scanlines : JSAMPARRAY;
                              max_lines : JDIMENSION) : JDIMENSION;


{ Alternate entry point to read raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_read_raw_data (cinfo : j_decompress_ptr;
                             data : JSAMPIMAGE;
                             max_lines : JDIMENSION) : JDIMENSION;

{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Initialize for an output pass in buffered-image mode. }

{GLOBAL}
function jpeg_start_output (cinfo : j_decompress_ptr;
                            scan_number : int) : boolean;

{ Finish up after an output pass in buffered-image mode.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_output (cinfo : j_decompress_ptr) : boolean;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }

{ Decompression initialization.
  jpeg_read_header must be completed before calling this.

  If a multipass operating mode was selected, this will do all but the
  last pass, and thus may take a great deal of time.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_start_decompress (cinfo : j_decompress_ptr) : boolean;


implementation

{ Forward declarations }
{LOCAL}
function output_pass_setup (cinfo : j_decompress_ptr) : boolean; forward;

{ Decompression initialization.
  jpeg_read_header must be completed before calling this.

  If a multipass operating mode was selected, this will do all but the
  last pass, and thus may take a great deal of time.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_start_decompress (cinfo : j_decompress_ptr) : boolean;
var
  retcode : int;
begin
  if (cinfo^.global_state = DSTATE_READY) then
  begin
    { First call: initialize master control, select active modules }
    jinit_master_decompress(cinfo);
    if (cinfo^.buffered_image) then
    begin
      { No more work here; expecting jpeg_start_output next }
      cinfo^.global_state := DSTATE_BUFIMAGE;
      jpeg_start_decompress := TRUE;
      exit;
    end;
    cinfo^.global_state := DSTATE_PRELOAD;
  end;
  if (cinfo^.global_state = DSTATE_PRELOAD) then
  begin
    { If file has multiple scans, absorb them all into the coef buffer }
    if (cinfo^.inputctl^.has_multiple_scans) then
    begin
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
      while TRUE do
      begin

        { Call progress monitor hook if present }
        if (cinfo^.progress <> NIL) then
          cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
        { Absorb some more input }
        retcode := cinfo^.inputctl^.consume_input (cinfo);
        if (retcode = JPEG_SUSPENDED) then
        begin
          jpeg_start_decompress := FALSE;
          exit;
        end;
        if (retcode = JPEG_REACHED_EOI) then
          break;
        { Advance progress counter if appropriate }
        if (cinfo^.progress <> NIL) and
           ((retcode = JPEG_ROW_COMPLETED) or (retcode = JPEG_REACHED_SOS)) then
        begin
          Inc(cinfo^.progress^.pass_counter);
          if (cinfo^.progress^.pass_counter >= cinfo^.progress^.pass_limit) then
          begin
            { jdmaster underestimated number of scans; ratchet up one scan }
            Inc(cinfo^.progress^.pass_limit, long(cinfo^.total_iMCU_rows));
          end;
        end;
      end;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { D_MULTISCAN_FILES_SUPPORTED }
    end;
    cinfo^.output_scan_number := cinfo^.input_scan_number;
  end
  else
    if (cinfo^.global_state <> DSTATE_PRESCAN) then
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Perform any dummy output passes, and set up for the final pass }
  jpeg_start_decompress := output_pass_setup(cinfo);
end;


{ Set up for an output pass, and perform any dummy pass(es) needed.
  Common subroutine for jpeg_start_decompress and jpeg_start_output.
  Entry: global_state := DSTATE_PRESCAN only if previously suspended.
  Exit: If done, returns TRUE and sets global_state for proper output mode.
        If suspended, returns FALSE and sets global_state := DSTATE_PRESCAN. }

{LOCAL}
function output_pass_setup (cinfo : j_decompress_ptr) : boolean;
var
  last_scanline : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_PRESCAN) then
  begin
    { First call: do pass setup }
    cinfo^.master^.prepare_for_output_pass (cinfo);
    cinfo^.output_scanline := 0;
    cinfo^.global_state := DSTATE_PRESCAN;
  end;
  { Loop over any required dummy passes }
  while (cinfo^.master^.is_dummy_pass) do
  begin
{$ifdef QUANT_2PASS_SUPPORTED}
    { Crank through the dummy pass }
    while (cinfo^.output_scanline < cinfo^.output_height) do
    begin
      { Call progress monitor hook if present }
      if (cinfo^.progress <> NIL) then
      begin
        cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
        cinfo^.progress^.pass_limit := long (cinfo^.output_height);
        cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
      end;
      { Process some data }
      last_scanline := cinfo^.output_scanline;
      cinfo^.main^.process_data (cinfo, JSAMPARRAY(NIL),
                                 cinfo^.output_scanline, {var}
                                 JDIMENSION(0));
      if (cinfo^.output_scanline = last_scanline) then
      begin
        output_pass_setup := FALSE;     { No progress made, must suspend }
        exit;
      end;
    end;
    { Finish up dummy pass, and set up for another one }
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.master^.prepare_for_output_pass (cinfo);
    cinfo^.output_scanline := 0;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { QUANT_2PASS_SUPPORTED }
  end;
  { Ready for application to drive output pass through
    jpeg_read_scanlines or jpeg_read_raw_data. }
  if cinfo^.raw_data_out then
    cinfo^.global_state := DSTATE_RAW_OK
   else
     cinfo^.global_state := DSTATE_SCANNING;
  output_pass_setup := TRUE;
end;


{ Read some scanlines of data from the JPEG decompressor.

  The return value will be the number of lines actually read.
  This may be less than the number requested in several cases,
  including bottom of image, data source suspension, and operating
  modes that emit multiple scanlines at a time.

  Note: we warn about excess calls to jpeg_read_scanlines() since
  this likely signals an application programmer error.  However,
  an oversize buffer (max_lines > scanlines remaining) is not an error. }

{GLOBAL}
function jpeg_read_scanlines (cinfo : j_decompress_ptr;
                              scanlines : JSAMPARRAY;
                              max_lines : JDIMENSION) : JDIMENSION;
var
  row_ctr : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_SCANNING) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.output_scanline >= cinfo^.output_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_read_scanlines := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.output_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Process some data }
  row_ctr := 0;
  cinfo^.main^.process_data (cinfo, scanlines, {var}row_ctr, max_lines);
  Inc(cinfo^.output_scanline, row_ctr);
  jpeg_read_scanlines := row_ctr;
end;


{ Alternate entry point to read raw data.
  Processes exactly one iMCU row per call, unless suspended. }

{GLOBAL}
function jpeg_read_raw_data (cinfo : j_decompress_ptr;
                             data : JSAMPIMAGE;
                             max_lines : JDIMENSION) : JDIMENSION;
var
  lines_per_iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state <> DSTATE_RAW_OK) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  if (cinfo^.output_scanline >= cinfo^.output_height) then
  begin
    WARNMS(j_common_ptr(cinfo), JWRN_TOO_MUCH_DATA);
    jpeg_read_raw_data := 0;
    exit;
  end;

  { Call progress monitor hook if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.pass_counter := long (cinfo^.output_scanline);
    cinfo^.progress^.pass_limit := long (cinfo^.output_height);
    cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
  end;

  { Verify that at least one iMCU row can be returned. }
  lines_per_iMCU_row := cinfo^.max_v_samp_factor * cinfo^.min_DCT_scaled_size;
  if (max_lines < lines_per_iMCU_row) then
    ERREXIT(j_common_ptr(cinfo), JERR_BUFFER_SIZE);

  { Decompress directly into user's buffer. }
  if (cinfo^.coef^.decompress_data (cinfo, data) = 0) then
  begin
    jpeg_read_raw_data := 0;                    { suspension forced, can do nothing more }
    exit;
  end;

  { OK, we processed one iMCU row. }
  Inc(cinfo^.output_scanline, lines_per_iMCU_row);
  jpeg_read_raw_data := lines_per_iMCU_row;
end;


{ Additional entry points for buffered-image mode. }

{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Initialize for an output pass in buffered-image mode. }

{GLOBAL}
function jpeg_start_output (cinfo : j_decompress_ptr;
                            scan_number : int) : boolean;
begin
  if (cinfo^.global_state <> DSTATE_BUFIMAGE) and
     (cinfo^.global_state <> DSTATE_PRESCAN) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Limit scan number to valid range }
  if (scan_number <= 0) then
    scan_number := 1;
  if (cinfo^.inputctl^.eoi_reached) and
     (scan_number > cinfo^.input_scan_number) then
    scan_number := cinfo^.input_scan_number;
  cinfo^.output_scan_number := scan_number;
  { Perform any dummy output passes, and set up for the real pass }
  jpeg_start_output := output_pass_setup(cinfo);
end;


{ Finish up after an output pass in buffered-image mode.

  Returns FALSE if suspended.  The return value need be inspected only if
  a suspending data source is used. }

{GLOBAL}
function jpeg_finish_output (cinfo : j_decompress_ptr) : boolean;
begin
  if ((cinfo^.global_state = DSTATE_SCANNING) or
      (cinfo^.global_state = DSTATE_RAW_OK) and cinfo^.buffered_image) then
  begin
    { Terminate this pass. }
    { We do not require the whole pass to have been completed. }
    cinfo^.master^.finish_output_pass (cinfo);
    cinfo^.global_state := DSTATE_BUFPOST;
  end
  else
    if (cinfo^.global_state <> DSTATE_BUFPOST) then
    begin
      { BUFPOST := repeat call after a suspension, anything else is error }
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
    end;
  { Read markers looking for SOS or EOI }
  while (cinfo^.input_scan_number <= cinfo^.output_scan_number) and
        (not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.inputctl^.consume_input (cinfo) = JPEG_SUSPENDED) then
    begin
      jpeg_finish_output := FALSE;      { Suspend, come back later }
      exit;
    end;
  end;
  cinfo^.global_state := DSTATE_BUFIMAGE;
  jpeg_finish_output := TRUE;
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }

end.
