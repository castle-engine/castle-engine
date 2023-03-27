Unit JcMaster;

{ This file contains master control logic for the JPEG compressor.
  These routines are concerned with parameter validation, initial setup,
  and inter-pass control (determining the number of passes and the work
  to be done in each pass). }

{ Original: jcmaster.c ; Copyright (C) 1991-1997, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jutils,
  jpeglib;


{ Initialize master compression control. }

{GLOBAL}
procedure jinit_c_master_control (cinfo : j_compress_ptr;
                                  transcode_only : boolean);

implementation

{ Private state }

type
  c_pass_type = (
        main_pass,              { input data, also do first output step }
        huff_opt_pass,          { Huffman code optimization pass }
        output_pass             { data output pass }
                );

type
  my_master_ptr = ^my_comp_master;
  my_comp_master = record
    pub : jpeg_comp_master;     { public fields }

    pass_type : c_pass_type;    { the type of the current pass }

    pass_number : int;          { # of passes completed }
    total_passes : int;         { total # of passes needed }

    scan_number : int;          { current index in scan_info[] }
  end;


{ Support routines that do various essential calculations. }

{LOCAL}
procedure initial_setup (cinfo : j_compress_ptr);
{ Do computations that are needed before master selection phase }
var
  ci : int;
  compptr : jpeg_component_info_ptr;
  samplesperrow : long;
  jd_samplesperrow : JDIMENSION;
begin

  { Sanity check on image dimensions }
  if (cinfo^.image_height <= 0) or (cinfo^.image_width <= 0) or
     (cinfo^.num_components <= 0) or (cinfo^.input_components <= 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_EMPTY_IMAGE);

  { Make sure image isn't bigger than I can handle }
  if ( long(cinfo^.image_height) > long(JPEG_MAX_DIMENSION)) or
      ( long(cinfo^.image_width) > long(JPEG_MAX_DIMENSION)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_IMAGE_TOO_BIG,
                                  uInt(JPEG_MAX_DIMENSION));

  { Width of an input scanline must be representable as JDIMENSION. }
  samplesperrow := long (cinfo^.image_width) * long (cinfo^.input_components);
  jd_samplesperrow := JDIMENSION (samplesperrow);
  if ( long(jd_samplesperrow) <> samplesperrow) then
    ERREXIT(j_common_ptr(cinfo), JERR_WIDTH_OVERFLOW);

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
    if (compptr^.h_samp_factor<=0) or (compptr^.h_samp_factor>MAX_SAMP_FACTOR)
    or (compptr^.v_samp_factor<=0) or (compptr^.v_samp_factor>MAX_SAMP_FACTOR) then
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_SAMPLING);
    { MAX }
    if cinfo^.max_h_samp_factor > compptr^.h_samp_factor then
      cinfo^.max_h_samp_factor := cinfo^.max_h_samp_factor
    else
      cinfo^.max_h_samp_factor := compptr^.h_samp_factor;
    { MAX }
    if cinfo^.max_v_samp_factor > compptr^.v_samp_factor then
      cinfo^.max_v_samp_factor := cinfo^.max_v_samp_factor
    else
      cinfo^.max_v_samp_factor := compptr^.v_samp_factor;
    Inc(compptr);
  end;

  { Compute dimensions of components }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Fill in the correct component_index value; don't rely on application }
    compptr^.component_index := ci;
    { For compression, we never do DCT scaling. }
    compptr^.DCT_scaled_size := DCTSIZE;
    { Size in DCT blocks }
    compptr^.width_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) * long (compptr^.h_samp_factor),
                    long (cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.height_in_blocks := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long (compptr^.v_samp_factor),
                    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    { Size in samples }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long(cinfo^.image_width) * long(compptr^.h_samp_factor),
                    long(cinfo^.max_h_samp_factor)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) * long(compptr^.v_samp_factor),
                    long (cinfo^.max_v_samp_factor)) );
    { Mark component needed (this flag isn't actually used for compression) }
    compptr^.component_needed := TRUE;
    Inc(compptr);
  end;

  { Compute number of fully interleaved MCU rows (number of times that
    main controller will call coefficient controller). }

  cinfo^.total_iMCU_rows := JDIMENSION (
    jdiv_round_up(long (cinfo^.image_height),
                  long (cinfo^.max_v_samp_factor*DCTSIZE)) );
end;


{$ifdef C_MULTISCAN_FILES_SUPPORTED}

{LOCAL}
procedure validate_script (cinfo : j_compress_ptr);
{ Verify that the scan script in cinfo^.scan_info[] is valid; also
  determine whether it uses progressive JPEG, and set cinfo^.progressive_mode. }
type
  IntRow = array[0..DCTSIZE2-1] of int;
  introw_ptr = ^IntRow;
var
  {const}scanptr : jpeg_scan_info_ptr;
  scanno, ncomps, ci, coefi, thisi : int;
  Ss, Se, Ah, Al : int;
  component_sent : array[0..MAX_COMPONENTS-1] of boolean;
{$ifdef C_PROGRESSIVE_SUPPORTED}
  last_bitpos_int_ptr : int_ptr;
  last_bitpos_ptr : introw_ptr;
  last_bitpos : array[0..MAX_COMPONENTS-1] of IntRow;
  { -1 until that coefficient has been seen; then last Al for it }
  { The JPEG spec simply gives the ranges 0..13 for Ah and Al, but that
    seems wrong: the upper bound ought to depend on data precision.
    Perhaps they really meant 0..N+1 for N-bit precision.
    Here we allow 0..10 for 8-bit data; Al larger than 10 results in
    out-of-range reconstructed DC values during the first DC scan,
    which might cause problems for some decoders. }
{$ifdef BITS_IN_JSAMPLE_IS_8}
const
  MAX_AH_AL = 10;
{$else}
const
  MAX_AH_AL = 13;
{$endif}
{$endif}
begin

  if (cinfo^.num_scans <= 0) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, 0);

  { For sequential JPEG, all scans must have Ss=0, Se=DCTSIZE2-1;
    for progressive JPEG, no scan can have this. }

  scanptr := cinfo^.scan_info;
  if (scanptr^.Ss <> 0) or (scanptr^.Se <> DCTSIZE2-1) then
  begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
    cinfo^.progressive_mode := TRUE;
    last_bitpos_int_ptr := @(last_bitpos[0][0]);
    for ci := 0 to pred(cinfo^.num_components) do
      for coefi := 0 to pred(DCTSIZE2) do
      begin
        last_bitpos_int_ptr^ := -1;
        Inc(last_bitpos_int_ptr);
      end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    cinfo^.progressive_mode := FALSE;
    for ci := 0 to pred(cinfo^.num_components) do
      component_sent[ci] := FALSE;
  end;

  for scanno := 1 to cinfo^.num_scans do
  begin
    { Validate component indexes }
    ncomps := scanptr^.comps_in_scan;
    if (ncomps <= 0) or (ncomps > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, ncomps, MAX_COMPS_IN_SCAN);
    for ci := 0 to pred(ncomps) do
    begin
      thisi := scanptr^.component_index[ci];
      if (thisi < 0) or (thisi >= cinfo^.num_components) then
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
      { Components must appear in SOF order within each scan }
      if (ci > 0) and (thisi <= scanptr^.component_index[ci-1]) then
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
    end;
    { Validate progression parameters }
    Ss := scanptr^.Ss;
    Se := scanptr^.Se;
    Ah := scanptr^.Ah;
    Al := scanptr^.Al;
    if (cinfo^.progressive_mode) then
    begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
      if (Ss < 0) or (Ss >= DCTSIZE2) or (Se < Ss) or (Se >= DCTSIZE2) or
         (Ah < 0) or (Ah > MAX_AH_AL) or (Al < 0) or (Al > MAX_AH_AL) then
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);

      if (Ss < 0) or (Ss >= DCTSIZE2) or (Se < Ss) or (Se >= DCTSIZE2)
       or (Ah < 0) or (Ah > MAX_AH_AL) or (Al < 0) or (Al > MAX_AH_AL) then
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      if (Ss = 0) then
      begin
        if (Se <> 0) then       { DC and AC together not OK }
          ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      end
      else
      begin
        if (ncomps <> 1) then  { AC scans must be for only one component }
          ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      end;
      for ci := 0 to pred(ncomps) do
      begin
        last_bitpos_ptr := @( last_bitpos[scanptr^.component_index[ci]]);
        if (Ss <> 0) and (last_bitpos_ptr^[0] < 0) then { AC without prior DC scan }
          ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
        for coefi := Ss to Se do
        begin
          if (last_bitpos_ptr^[coefi] < 0) then
          begin
            { first scan of this coefficient }
            if (Ah <> 0) then
              ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
          end
          else
          begin
            { not first scan }
            if (Ah <> last_bitpos_ptr^[coefi]) or (Al <> Ah-1) then
              ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
          end;
          last_bitpos_ptr^[coefi] := Al;
        end;
      end;
{$endif}
    end
    else
    begin
      { For sequential JPEG, all progression parameters must be these: }
      if (Ss <> 0) or (Se <> DCTSIZE2-1) or (Ah <> 0) or (Al <> 0) then
        ERREXIT1(j_common_ptr(cinfo), JERR_BAD_PROG_SCRIPT, scanno);
      { Make sure components are not sent twice }
      for ci := 0 to pred(ncomps) do
      begin
        thisi := scanptr^.component_index[ci];
        if (component_sent[thisi]) then
          ERREXIT1(j_common_ptr(cinfo), JERR_BAD_SCAN_SCRIPT, scanno);
        component_sent[thisi] := TRUE;
      end;
    end;
    Inc(scanptr);
  end;

  { Now verify that everything got sent. }
  if (cinfo^.progressive_mode) then
  begin
{$ifdef C_PROGRESSIVE_SUPPORTED
    { For progressive mode, we only check that at least some DC data
      got sent for each component; the spec does not require that all bits
      of all coefficients be transmitted.  Would it be wiser to enforce
      transmission of all coefficient bits?? }

    for ci := 0 to pred(cinfo^.num_components) do
    begin
      if (last_bitpos[ci][0] < 0) then
        ERREXIT(j_common_ptr(cinfo), JERR_MISSING_DATA);
    end;
{$endif}
  end
  else
  begin
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      if (not component_sent[ci]) then
        ERREXIT(j_common_ptr(cinfo), JERR_MISSING_DATA);
    end;
  end;
end;

{$endif} { C_MULTISCAN_FILES_SUPPORTED }


{LOCAL}
procedure select_scan_parameters (cinfo : j_compress_ptr);
{ Set up the scan parameters for the current scan }
var
  master : my_master_ptr;
  {const} scanptr : jpeg_scan_info_ptr;
  ci : int;
var
  comp_infos : jpeg_component_info_list_ptr;
begin
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
  if (cinfo^.scan_info <> NIL) then
  begin
    { Prepare for current scan --- the script is already validated }
    master := my_master_ptr (cinfo^.master);
    scanptr := cinfo^.scan_info;
    Inc(scanptr, master^.scan_number);

    cinfo^.comps_in_scan := scanptr^.comps_in_scan;
    comp_infos := cinfo^.comp_info;
    for ci := 0 to pred(scanptr^.comps_in_scan) do
    begin
      cinfo^.cur_comp_info[ci] :=
        @(comp_infos^[scanptr^.component_index[ci]]);
    end;
    cinfo^.Ss := scanptr^.Ss;
    cinfo^.Se := scanptr^.Se;
    cinfo^.Ah := scanptr^.Ah;
    cinfo^.Al := scanptr^.Al;
  end
  else
{$endif}
  begin
    { Prepare for single sequential-JPEG scan containing all components }
    if (cinfo^.num_components > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT, cinfo^.num_components,
               MAX_COMPS_IN_SCAN);
    cinfo^.comps_in_scan := cinfo^.num_components;
    comp_infos := cinfo^.comp_info;
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      cinfo^.cur_comp_info[ci] := @(comp_infos^[ci]);
    end;
    cinfo^.Ss := 0;
    cinfo^.Se := DCTSIZE2-1;
    cinfo^.Ah := 0;
    cinfo^.Al := 0;
  end;
end;


{LOCAL}
procedure per_scan_setup (cinfo : j_compress_ptr);
{ Do computations that are needed before processing a JPEG scan }
{ cinfo^.comps_in_scan and cinfo^.cur_comp_info[] are already set }
var
  ci, mcublks, tmp : int;
  compptr : jpeg_component_info_ptr;
  nominal : long;
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
    compptr^.MCU_sample_width := DCTSIZE;
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
    if (cinfo^.comps_in_scan <= 0) or
       (cinfo^.comps_in_scan > MAX_COMPS_IN_SCAN) then
      ERREXIT2(j_common_ptr(cinfo), JERR_COMPONENT_COUNT,
        cinfo^.comps_in_scan,  MAX_COMPS_IN_SCAN);

    { Overall image size in MCUs }
    cinfo^.MCUs_per_row := JDIMENSION (
      jdiv_round_up( long (cinfo^.image_width),
                     long (cinfo^.max_h_samp_factor*DCTSIZE)) );
    cinfo^.MCU_rows_in_scan := JDIMENSION (
      jdiv_round_up( long (cinfo^.image_height),
                     long (cinfo^.max_v_samp_factor*DCTSIZE)) );

    cinfo^.blocks_in_MCU := 0;

    for ci := 0 to pred(cinfo^.comps_in_scan) do
    begin
      compptr := cinfo^.cur_comp_info[ci];
      { Sampling factors give # of blocks of component in each MCU }
      compptr^.MCU_width := compptr^.h_samp_factor;
      compptr^.MCU_height := compptr^.v_samp_factor;
      compptr^.MCU_blocks := compptr^.MCU_width * compptr^.MCU_height;
      compptr^.MCU_sample_width := compptr^.MCU_width * DCTSIZE;
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
      if (cinfo^.blocks_in_MCU + mcublks > C_MAX_BLOCKS_IN_MCU) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_MCU_SIZE);
      while (mcublks > 0) do
      begin
        Dec(mcublks);
        cinfo^.MCU_membership[cinfo^.blocks_in_MCU] := ci;
        Inc(cinfo^.blocks_in_MCU);
      end;
    end;

  end;

  { Convert restart specified in rows to actual MCU count. }
  { Note that count must fit in 16 bits, so we provide limiting. }
  if (cinfo^.restart_in_rows > 0) then
  begin
    nominal := long(cinfo^.restart_in_rows) * long(cinfo^.MCUs_per_row);
    if nominal < long(65535) then
      cinfo^.restart_interval := uInt (nominal)
    else
      cinfo^.restart_interval := long(65535);
  end;
end;


{ Per-pass setup.
  This is called at the beginning of each pass.  We determine which modules
  will be active during this pass and give them appropriate start_pass calls.
  We also set is_last_pass to indicate whether any more passes will be
  required. }

{METHODDEF}
procedure prepare_for_pass (cinfo : j_compress_ptr); far;
var
  master : my_master_ptr;
var
  fallthrough : boolean;
begin
  master := my_master_ptr (cinfo^.master);
  fallthrough := true;

  case (master^.pass_type) of
  main_pass:
    begin
      { Initial pass: will collect input data, and do either Huffman
        optimization or data output for the first scan. }
      select_scan_parameters(cinfo);
      per_scan_setup(cinfo);
      if (not cinfo^.raw_data_in) then
      begin
        cinfo^.cconvert^.start_pass (cinfo);
        cinfo^.downsample^.start_pass (cinfo);
        cinfo^.prep^.start_pass (cinfo, JBUF_PASS_THRU);
      end;
      cinfo^.fdct^.start_pass (cinfo);
      cinfo^.entropy^.start_pass (cinfo, cinfo^.optimize_coding);
      if master^.total_passes > 1 then
        cinfo^.coef^.start_pass (cinfo, JBUF_SAVE_AND_PASS)
      else
        cinfo^.coef^.start_pass (cinfo, JBUF_PASS_THRU);
      cinfo^.main^.start_pass (cinfo, JBUF_PASS_THRU);
      if (cinfo^.optimize_coding) then
      begin
        { No immediate data output; postpone writing frame/scan headers }
        master^.pub.call_pass_startup := FALSE;
      end
      else
      begin
        { Will write frame/scan headers at first jpeg_write_scanlines call }
        master^.pub.call_pass_startup := TRUE;
      end;
    end;
{$ifdef ENTROPY_OPT_SUPPORTED}
  huff_opt_pass,
  output_pass:
    begin
      if (master^.pass_type = huff_opt_pass) then
      begin
        { Do Huffman optimization for a scan after the first one. }
        select_scan_parameters(cinfo);
        per_scan_setup(cinfo);
        if (cinfo^.Ss <> 0) or (cinfo^.Ah = 0) or (cinfo^.arith_code) then
        begin
          cinfo^.entropy^.start_pass (cinfo, TRUE);
          cinfo^.coef^.start_pass (cinfo, JBUF_CRANK_DEST);
          master^.pub.call_pass_startup := FALSE;
          fallthrough := false;
        end;
        { Special case: Huffman DC refinement scans need no Huffman table
          and therefore we can skip the optimization pass for them. }
        if fallthrough then
        begin
          master^.pass_type := output_pass;
          Inc(master^.pass_number);
          {FALLTHROUGH}
        end;
      end;
{$else}
  output_pass:
    begin
{$endif}
      if fallthrough then
      begin
        { Do a data-output pass. }
        { We need not repeat per-scan setup if prior optimization pass did it. }
        if (not cinfo^.optimize_coding) then
        begin
          select_scan_parameters(cinfo);
          per_scan_setup(cinfo);
        end;
        cinfo^.entropy^.start_pass (cinfo, FALSE);
        cinfo^.coef^.start_pass (cinfo, JBUF_CRANK_DEST);
        { We emit frame/scan headers now }
        if (master^.scan_number = 0) then
          cinfo^.marker^.write_frame_header (cinfo);
        cinfo^.marker^.write_scan_header (cinfo);
        master^.pub.call_pass_startup := FALSE;
      end;
    end;
  else
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
  end;

  master^.pub.is_last_pass := (master^.pass_number = master^.total_passes-1);

  { Set up progress monitor's pass info if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.completed_passes := master^.pass_number;
    cinfo^.progress^.total_passes := master^.total_passes;
  end;
end;


{ Special start-of-pass hook.
  This is called by jpeg_write_scanlines if call_pass_startup is TRUE.
  In single-pass processing, we need this hook because we don't want to
  write frame/scan headers during jpeg_start_compress; we want to let the
  application write COM markers etc. between jpeg_start_compress and the
  jpeg_write_scanlines loop.
  In multi-pass processing, this routine is not used. }

{METHODDEF}
procedure pass_startup (cinfo : j_compress_ptr); far;
begin
  cinfo^.master^.call_pass_startup := FALSE; { reset flag so call only once }

  cinfo^.marker^.write_frame_header (cinfo);
  cinfo^.marker^.write_scan_header (cinfo);
end;


{ Finish up at end of pass. }

{METHODDEF}
procedure finish_pass_master (cinfo : j_compress_ptr); far;
var
  master : my_master_ptr;
begin
  master := my_master_ptr (cinfo^.master);

  { The entropy coder always needs an end-of-pass call,
    either to analyze statistics or to flush its output buffer. }
  cinfo^.entropy^.finish_pass (cinfo);

  { Update state for next pass }
  case (master^.pass_type) of
  main_pass:
    begin
      { next pass is either output of scan 0 (after optimization)
        or output of scan 1 (if no optimization). }

      master^.pass_type := output_pass;
      if (not cinfo^.optimize_coding) then
        Inc(master^.scan_number);
    end;
  huff_opt_pass:
    { next pass is always output of current scan }
    master^.pass_type := output_pass;
  output_pass:
    begin
      { next pass is either optimization or output of next scan }
      if (cinfo^.optimize_coding) then
        master^.pass_type := huff_opt_pass;
      Inc(master^.scan_number);
    end;
  end;

  Inc(master^.pass_number);
end;


{ Initialize master compression control. }

{GLOBAL}
procedure jinit_c_master_control (cinfo : j_compress_ptr;
                                  transcode_only : boolean);
var
  master : my_master_ptr;
begin
  master := my_master_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  SIZEOF(my_comp_master)) );
  cinfo^.master := jpeg_comp_master_ptr(master);
  master^.pub.prepare_for_pass := prepare_for_pass;
  master^.pub.pass_startup := pass_startup;
  master^.pub.finish_pass := finish_pass_master;
  master^.pub.is_last_pass := FALSE;

  { Validate parameters, determine derived values }
  initial_setup(cinfo);

  if (cinfo^.scan_info <> NIL) then
  begin
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
    validate_script(cinfo);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    cinfo^.progressive_mode := FALSE;
    cinfo^.num_scans := 1;
  end;

  if (cinfo^.progressive_mode) then  {  TEMPORARY HACK ??? }
    cinfo^.optimize_coding := TRUE;  { assume default tables no good for progressive mode }

  { Initialize my private state }
  if (transcode_only) then
  begin
    { no main pass in transcoding }
    if (cinfo^.optimize_coding) then
      master^.pass_type := huff_opt_pass
    else
      master^.pass_type := output_pass;
  end
  else
  begin
    { for normal compression, first pass is always this type: }
    master^.pass_type := main_pass;
  end;
  master^.scan_number := 0;
  master^.pass_number := 0;
  if (cinfo^.optimize_coding) then
    master^.total_passes := cinfo^.num_scans * 2
  else
    master^.total_passes := cinfo^.num_scans;
end;

end.
