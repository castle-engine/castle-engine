Unit JdInput;

{ Original: jdinput.c ; Copyright (C) 1991-1997, Thomas G. Lane. }

{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains input control logic for the JPEG decompressor.
  These routines are concerned with controlling the decompressor's input
  processing (marker reading and coefficient decoding).  The actual input
  reading is done in jdmarker.c, jdhuff.c, and jdphuff.c. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jpeglib,
  jdeferr,
  jerror,
  jinclude, jutils;

{ Initialize the input controller module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_input_controller (cinfo : j_decompress_ptr);

implementation

{ Private state }

type
  my_inputctl_ptr = ^my_input_controller;
  my_input_controller = record
    pub : jpeg_input_controller; { public fields }

    inheaders : boolean;                { TRUE until first SOS is reached }
  end; {my_input_controller;}



{ Forward declarations }
{METHODDEF}
function consume_markers (cinfo : j_decompress_ptr) : int; far; forward;


{ Routines to calculate various quantities related to the size of the image. }

{LOCAL}
procedure initial_setup (cinfo : j_decompress_ptr);
{ Called once, when first SOS marker is reached }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  { Make sure image isn't bigger than I can handle }
  if (long(cinfo^.image_height) > long (JPEG_MAX_DIMENSION)) or
     (long(cinfo^.image_width) > long(JPEG_MAX_DIMENSION)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG, uInt(JPEG_MAX_DIMENSION));

  { For now, precision must match compiled-in value... }
  if (cinfo^.data_precision <> BITS_IN_JSAMPLE) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PRECISION, cinfo^.data_precision);

  { Check that number of components won't exceed internal array sizes }
  if (cinfo^.num_components > MAX_COMPONENTS) then
    ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.num_components,
             MAX_COMPONENTS);

  { Compute maximum sampling factors; check factor validity }
  cinfo^.max_h_samp_factor := 1;
  cinfo^.max_v_samp_factor := 1;
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    if (compptr^.h_samp_factor<=0) or (compptr^.h_samp_factor>MAX_SAMP_FACTOR) or
       (compptr^.v_samp_factor<=0) or (compptr^.v_samp_factor>MAX_SAMP_FACTOR) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_SAMPLING);
    {cinfo^.max_h_samp_factor := MAX(cinfo^.max_h_samp_factor,
                                   compptr^.h_samp_factor);
    cinfo^.max_v_samp_factor := MAX(cinfo^.max_v_samp_factor,
                                   compptr^.v_samp_factor);}
    if cinfo^.max_h_samp_factor < compptr^.h_samp_factor then
      cinfo^.max_h_samp_factor := compptr^.h_samp_factor;
    if cinfo^.max_v_samp_factor < compptr^.v_samp_factor then
      cinfo^.max_v_samp_factor := compptr^.v_samp_factor;
    Inc(compptr);
  end;

  { We initialize DCT_scaled_size and min_DCT_scaled_size to DCTSIZE.
    In the full decompressor, this will be overridden by jdmaster.c;
    but in the transcoder, jdmaster.c is not used, so we must do it here. }

  cinfo^.min_DCT_scaled_size := DCTSIZE;

  { Compute dimensions of components }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    compptr^.DCT_scaled_size := DCTSIZE;
    { Size in DCT blocks }
    compptr^.width_in_blocks := JDIMENSION(
      jdiv_round_up( long(cinfo^.image_width) * long(compptr^.h_samp_factor),
                     long(cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.height_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
                    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    { downsampled_width and downsampled_height will also be overridden by
      jdmaster.c if we are doing full decompression.  The transcoder library
      doesn't use these values, but the calling application might. }

    { Size in samples }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) * long(compptr^.h_samp_factor),
                    long (cinfo^.max_h_samp_factor)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
                    long (cinfo^.max_v_samp_factor)) );
    { Mark component needed, until color conversion says otherwise }
    compptr^.component_needed := TRUE;
    { Mark no quantization table yet saved for component }
    compptr^.quant_table := NIL;
    Inc(compptr);
  end;

  { Compute number of fully interleaved MCU rows. }
  cinfo^.total_iMCU_rows := JDIMENSION(
    jdiv_round_up(long(cinfo^.image_height),
                  long(cinfo^.max_v_samp_factor*DCTSIZE)) );

  { Decide whether file contains multiple scans }
  if (cinfo^.comps_in_scan < cinfo^.num_components) or
     (cinfo^.progressive_mode) then
    cinfo^.inputctl^.has_multiple_scans := TRUE
  else
    cinfo^.inputctl^.has_multiple_scans := FALSE;
end;


{LOCAL}
procedure per_scan_setup (cinfo : j_decompress_ptr);
{ Do computations that are needed before processing a JPEG scan }
{ cinfo^.comps_in_scan and cinfo^.cur_comp_info[] were set from SOS marker }
var
  ci, mcublks, tmp : int;
  compptr : jpeg_component_info_ptr;
begin
  if (cinfo^.comps_in_scan = 1) then
  begin
    { Noninterleaved (single-component) scan }
    compptr := cinfo^.cur_comp_info[0];

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := compptr^.width_in_blocks;
    cinfo^.MCU_rows_in_scan := compptr^.height_in_blocks;

    { For noninterleaved scan, always one block per MCU }
    compptr^.MCU_width := 1;
    compptr^.MCU_height := 1;
    compptr^.MCU_blocks := 1;
    compptr^.MCU_sample_width := compptr^.DCT_scaled_size;
    compptr^.last_col_width := 1;
    { For noninterleaved scans, it is convenient to define last_row_height
      as the number of block rows present in the last iMCU row. }

    tmp := int (compptr^.height_in_blocks mod compptr^.v_samp_factor);
    if (tmp = 0) then
      tmp := compptr^.v_samp_factor;
    compptr^.last_row_height := tmp;

    { Prepare array describing MCU composition }
    cinfo^.blocks_in_MCU := 1;
    cinfo^.MCU_membership[0] := 0;

  end
  else
  begin

    { Interleaved (multi-component) scan }
    if (cinfo^.comps_in_scan <= 0) or (cinfo^.comps_in_scan > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.comps_in_scan,
               MAX_COMPS_IN_SCAN);

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width),
                    long (cinfo^.max_h_samp_factor*DCTSIZE)) );
    cinfo^.MCU_rows_in_scan := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height),
                    long (cinfo^.max_v_samp_factor*DCTSIZE)) );

    cinfo^.blocks_in_MCU := 0;

    for ci := 0 to pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[ci];
      { Sampling factors give # of blocks of component in each MCU }
      compptr^.MCU_width := compptr^.h_samp_factor;
      compptr^.MCU_height := compptr^.v_samp_factor;
      compptr^.MCU_blocks := compptr^.MCU_width * compptr^.MCU_height;
      compptr^.MCU_sample_width := compptr^.MCU_width * compptr^.DCT_scaled_size;
      { Figure number of non-dummy blocks in last MCU column & row }
      tmp := int (compptr^.width_in_blocks mod compptr^.MCU_width);
      if (tmp = 0) then
        tmp := compptr^.MCU_width;
      compptr^.last_col_width := tmp;
      tmp := int (compptr^.height_in_blocks mod compptr^.MCU_height);
      if (tmp = 0) then
        tmp := compptr^.MCU_height;
      compptr^.last_row_height := tmp;
      { Prepare array describing MCU composition }
      mcublks := compptr^.MCU_blocks;
      if (cinfo^.blocks_in_MCU + mcublks > D_MAX_BLOCKS_IN_MCU) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_MCU_SIZE);
      while (mcublks > 0) do
      begin
        Dec(mcublks);
        cinfo^.MCU_membership[cinfo^.blocks_in_MCU] := ci;
        Inc(cinfo^.blocks_in_MCU);
      end;
    end;

  end;
end;


{ Save away a copy of the Q-table referenced by each component present
  in the current scan, unless already saved during a prior scan.

  In a multiple-scan JPEG file, the encoder could assign different components
  the same Q-table slot number, but change table definitions between scans
  so that each component uses a different Q-table.  (The IJG encoder is not
  currently capable of doing this, but other encoders might.)  Since we want
  to be able to dequantize all the components at the end of the file, this
  means that we have to save away the table actually used for each component.
  We do this by copying the table at the start of the first scan containing
  the component.
  The JPEG spec prohibits the encoder from changing the contents of a Q-table
  slot between scans of a component using that slot.  If the encoder does so
  anyway, this decoder will simply use the Q-table values that were current
  at the start of the first scan for the component.

  The decompressor output side looks only at the saved quant tables,
  not at the current Q-table slots. }

{LOCAL}
procedure latch_quant_tables (cinfo : j_decompress_ptr);
var
  ci, qtblno : int;
  compptr : jpeg_component_info_ptr;
  qtbl : JQUANT_TBL_PTR;
begin
  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    { No work if we already saved Q-table for this component }
    if (compptr^.quant_table <> NIL) then
      continue;
    { Make sure specified quantization table is present }
    qtblno := compptr^.quant_tbl_no;
    if (qtblno < 0) or (qtblno >= NUM_QUANT_TBLS) or
       (cinfo^.quant_tbl_ptrs[qtblno] = NIL) then
      ERREXIT1(j_common_ptr(cinfo), JERR_NO_QUANT_TABLE, qtblno);
    { OK, save away the quantization table }
    qtbl := JQUANT_TBL_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(JQUANT_TBL)) );
    MEMCOPY(qtbl, cinfo^.quant_tbl_ptrs[qtblno], SIZEOF(JQUANT_TBL));
    compptr^.quant_table := qtbl;
  end;
end;


{ Initialize the input modules to read a scan of compressed data.
  The first call to this is done by jdmaster.c after initializing
  the entire decompressor (during jpeg_start_decompress).
  Subsequent calls come from consume_markers, below. }

{METHODDEF}
procedure start_input_pass (cinfo : j_decompress_ptr); far;
begin
  per_scan_setup(cinfo);
  latch_quant_tables(cinfo);
  cinfo^.entropy^.start_pass (cinfo);
  cinfo^.coef^.start_input_pass (cinfo);
  cinfo^.inputctl^.consume_input := cinfo^.coef^.consume_data;
end;


{ Finish up after inputting a compressed-data scan.
  This is called by the coefficient controller after it's read all
  the expected data of the scan. }

{METHODDEF}
procedure finish_input_pass (cinfo : j_decompress_ptr); far;
begin
  cinfo^.inputctl^.consume_input := consume_markers;
end;


{ Read JPEG markers before, between, or after compressed-data scans.
  Change state as necessary when a new scan is reached.
  Return value is JPEG_SUSPENDED, JPEG_REACHED_SOS, or JPEG_REACHED_EOI.

  The consume_input method pointer points either here or to the
  coefficient controller's consume_data routine, depending on whether
  we are reading a compressed data segment or inter-segment markers. }

{METHODDEF}
function consume_markers (cinfo : j_decompress_ptr) : int;
var
  val : int;
  inputctl : my_inputctl_ptr;
begin
  inputctl := my_inputctl_ptr (cinfo^.inputctl);

  if (inputctl^.pub.eoi_reached) then { After hitting EOI, read no further }
  begin
    consume_markers := JPEG_REACHED_EOI;
    exit;
  end;

  val := cinfo^.marker^.read_markers (cinfo);

  case (val) of
  JPEG_REACHED_SOS:     { Found SOS }
    begin
      if (inputctl^.inheaders) then
      begin     { 1st SOS }
        initial_setup(cinfo);
        inputctl^.inheaders := FALSE;
        { Note: start_input_pass must be called by jdmaster.c
          before any more input can be consumed.  jdapimin.c is
          responsible for enforcing this sequencing. }
      end
      else
      begin                     { 2nd or later SOS marker }
        if (not inputctl^.pub.has_multiple_scans) then
          ERREXIT(j_common_ptr(cinfo), JERR_EOI_EXPECTED); { Oops, I wasn't expecting this! }
        start_input_pass(cinfo);
      end;
    end;
  JPEG_REACHED_EOI:     { Found EOI }
    begin
      inputctl^.pub.eoi_reached := TRUE;
      if (inputctl^.inheaders) then
      begin     { Tables-only datastream, apparently }
        if (cinfo^.marker^.saw_SOF) then
          ERREXIT(j_common_ptr(cinfo), JERR_SOF_NO_SOS);
      end
      else
      begin
        { Prevent infinite loop in coef ctlr's decompress_data routine
          if user set output_scan_number larger than number of scans. }

        if (cinfo^.output_scan_number > cinfo^.input_scan_number) then
          cinfo^.output_scan_number := cinfo^.input_scan_number;
      end;
    end;
  JPEG_SUSPENDED:;
  end;

  consume_markers := val;
end;


{ Reset state to begin a fresh datastream. }

{METHODDEF}
procedure reset_input_controller (cinfo : j_decompress_ptr); far;
var
  inputctl : my_inputctl_ptr;
begin
  inputctl := my_inputctl_ptr (cinfo^.inputctl);

  inputctl^.pub.consume_input := consume_markers;
  inputctl^.pub.has_multiple_scans := FALSE; { "unknown" would be better }
  inputctl^.pub.eoi_reached := FALSE;
  inputctl^.inheaders := TRUE;
  { Reset other modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.marker^.reset_marker_reader (cinfo);
  { Reset progression state -- would be cleaner if entropy decoder did this }
  cinfo^.coef_bits := NIL;
end;


{ Initialize the input controller module.
  This is called only once, when the decompression object is created. }

{GLOBAL}
procedure jinit_input_controller (cinfo : j_decompress_ptr);
var
  inputctl : my_inputctl_ptr;
begin
  { Create subobject in permanent pool }
  inputctl := my_inputctl_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                SIZEOF(my_input_controller)) );
  cinfo^.inputctl := jpeg_input_controller_ptr(inputctl);
  { Initialize method pointers }
  inputctl^.pub.consume_input := consume_markers;
  inputctl^.pub.reset_input_controller := reset_input_controller;
  inputctl^.pub.start_input_pass := start_input_pass;
  inputctl^.pub.finish_input_pass := finish_input_pass;
  { Initialize state: can't use reset_input_controller since we don't
    want to try to reset other modules yet. }

  inputctl^.pub.has_multiple_scans := FALSE; { "unknown" would be better }
  inputctl^.pub.eoi_reached := FALSE;
  inputctl^.inheaders := TRUE;
end;

end.
