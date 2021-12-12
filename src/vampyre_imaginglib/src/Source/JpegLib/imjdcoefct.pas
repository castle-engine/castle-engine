unit imjdcoefct;

{ This file contains the coefficient buffer controller for decompression.
  This controller is the top level of the JPEG decompressor proper.
  The coefficient buffer lies between entropy decoding and inverse-DCT steps.

  In buffered-image mode, this controller is the interface between
  input-oriented processing and output-oriented processing.
  Also, the input side (only) is used when reading a file for transcoding. }

{ Original: jdcoefct.c ; Copyright (C) 1994-1997, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjutils,
  imjpeglib;

{GLOBAL}
procedure jinit_d_coef_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);


implementation


{ Block smoothing is only applicable for progressive JPEG, so: }
{$ifndef D_PROGRESSIVE_SUPPORTED}
{$undef BLOCK_SMOOTHING_SUPPORTED}
{$endif}

{ Private buffer controller object }

{$ifdef BLOCK_SMOOTHING_SUPPORTED}
const
  SAVED_COEFS = 6;              { we save coef_bits[0..5] }
type
  Latch = array[0..SAVED_COEFS-1] of int;
  Latch_ptr = ^Latch;
{$endif}

type
  my_coef_ptr = ^my_coef_controller;
  my_coef_controller = record
    pub : jpeg_d_coef_controller; { public fields }

    { These variables keep track of the current location of the input side. }
    { cinfo^.input_iMCU_row is also used for this. }
    MCU_ctr : JDIMENSION;               { counts MCUs processed in current row }
    MCU_vert_offset : int;              { counts MCU rows within iMCU row }
    MCU_rows_per_iMCU_row : int;        { number of such rows needed }

    { The output side's location is represented by cinfo^.output_iMCU_row. }

    { In single-pass modes, it's sufficient to buffer just one MCU.
      We allocate a workspace of D_MAX_BLOCKS_IN_MCU coefficient blocks,
      and let the entropy decoder write into that workspace each time.
      (On 80x86, the workspace is FAR even though it's not really very big;
      this is to keep the module interfaces unchanged when a large coefficient
      buffer is necessary.)
      In multi-pass modes, this array points to the current MCU's blocks
      within the virtual arrays; it is used only by the input side. }

    MCU_buffer : array[0..D_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;

  {$ifdef D_MULTISCAN_FILES_SUPPORTED}
    { In multi-pass modes, we need a virtual block array for each component. }
    whole_image : jvirt_barray_tbl;
  {$endif}

  {$ifdef BLOCK_SMOOTHING_SUPPORTED}
    { When doing block smoothing, we latch coefficient Al values here }
    coef_bits_latch : Latch_Ptr;
  {$endif}
  end;

{ Forward declarations }
{METHODDEF}
function decompress_onepass (cinfo : j_decompress_ptr;
                             output_buf : JSAMPIMAGE) : int; forward;
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
{METHODDEF}
function decompress_data (cinfo : j_decompress_ptr;
                          output_buf : JSAMPIMAGE) : int; forward;
{$endif}
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
{LOCAL}
function smoothing_ok (cinfo : j_decompress_ptr) : boolean; forward;

{METHODDEF}
function decompress_smooth_data	(cinfo : j_decompress_ptr;
                                 output_buf : JSAMPIMAGE) : int; forward;
{$endif}


{LOCAL}
procedure start_iMCU_row (cinfo : j_decompress_ptr);
{ Reset within-iMCU-row counters for a new row (input side) }
var
  coef : my_coef_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);

  { In an interleaved scan, an MCU row is the same as an iMCU row.
    In a noninterleaved scan, an iMCU row has v_samp_factor MCU rows.
    But at the bottom of the image, process only what's left. }

  if (cinfo^.comps_in_scan > 1) then
  begin
    coef^.MCU_rows_per_iMCU_row := 1;
  end
  else
  begin
    if (cinfo^.input_iMCU_row < (cinfo^.total_iMCU_rows-1)) then
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.v_samp_factor
    else
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.last_row_height;
  end;

  coef^.MCU_ctr := 0;
  coef^.MCU_vert_offset := 0;
end;


{ Initialize for an input processing pass. }

{METHODDEF}
procedure start_input_pass (cinfo : j_decompress_ptr);  
begin
  cinfo^.input_iMCU_row := 0;
  start_iMCU_row(cinfo);
end;


{ Initialize for an output processing pass. }

{METHODDEF}
procedure start_output_pass (cinfo : j_decompress_ptr);  
var
  coef : my_coef_ptr;
begin
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
  coef := my_coef_ptr (cinfo^.coef);

  { If multipass, check to see whether to use block smoothing on this pass }
  if (coef^.pub.coef_arrays <> NIL) then
  begin
    if (cinfo^.do_block_smoothing) and smoothing_ok(cinfo) then
      coef^.pub.decompress_data := decompress_smooth_data
    else
      coef^.pub.decompress_data := decompress_data;
  end;
{$endif}
  cinfo^.output_iMCU_row := 0;
end;


{ Decompress and return some data in the single-pass case.
  Always attempts to emit one fully interleaved MCU row ("iMCU" row).
  Input and output must run in lockstep since we have only a one-MCU buffer.
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.

  NB: output_buf contains a plane for each component in image,
  which we index according to the component's SOF position.}

{METHODDEF}
function decompress_onepass (cinfo : j_decompress_ptr;
                             output_buf : JSAMPIMAGE) : int;
var
  coef : my_coef_ptr;
  MCU_col_num : JDIMENSION;     { index of current MCU within row }
  last_MCU_col : JDIMENSION;
  last_iMCU_row : JDIMENSION;
  blkn, ci, xindex, yindex, yoffset, useful_width : int;
  output_ptr : JSAMPARRAY;
  start_col, output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_MCU_col := cinfo^.MCUs_per_row - 1;
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Loop to process as much as one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.MCU_ctr to last_MCU_col do
    begin
      { Try to fetch an MCU.  Entropy decoder expects buffer to be zeroed. }
      jzero_far( coef^.MCU_buffer[0],
		size_t (cinfo^.blocks_in_MCU * SIZEOF(JBLOCK)));
      if (not cinfo^.entropy^.decode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.MCU_ctr := MCU_col_num;
	decompress_onepass := JPEG_SUSPENDED;
        exit;
      end;
      { Determine where data should go in output_buf and do the IDCT thing.
        We skip dummy blocks at the right and bottom edges (but blkn gets
        incremented past them!).  Note the inner loop relies on having
        allocated the MCU_buffer[] blocks sequentially. }

      blkn := 0;			{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	{ Don't bother to IDCT an uninteresting component. }
	if (not compptr^.component_needed) then
        begin
	  Inc(blkn, compptr^.MCU_blocks);
	  continue;
	end;
	inverse_DCT := cinfo^.idct^.inverse_DCT[compptr^.component_index];
        if (MCU_col_num < last_MCU_col) then
          useful_width := compptr^.MCU_width
        else
          useful_width := compptr^.last_col_width;

	output_ptr := JSAMPARRAY(@ output_buf^[compptr^.component_index]^
                                   [yoffset * compptr^.DCT_scaled_size]);
	start_col := LongInt(MCU_col_num) * compptr^.MCU_sample_width;
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  if (cinfo^.input_iMCU_row < last_iMCU_row) or
             (yoffset+yindex < compptr^.last_row_height) then
          begin
	    output_col := start_col;
	    for xindex := 0 to pred(useful_width) do
            begin
	      inverse_DCT (cinfo, compptr,
			   JCOEFPTR(coef^.MCU_buffer[blkn+xindex]),
			   output_ptr, output_col);
	      Inc(output_col, compptr^.DCT_scaled_size);
	    end;
	  end;
	  Inc(blkn, compptr^.MCU_width);
	  Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
	end;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.MCU_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(cinfo^.output_iMCU_row);

  Inc(cinfo^.input_iMCU_row);
  if (cinfo^.input_iMCU_row < cinfo^.total_iMCU_rows) then
  begin
    start_iMCU_row(cinfo);
    decompress_onepass := JPEG_ROW_COMPLETED;
    exit;
  end;
  { Completed the scan }
  cinfo^.inputctl^.finish_input_pass (cinfo);
  decompress_onepass := JPEG_SCAN_COMPLETED;
end;

{ Dummy consume-input routine for single-pass operation. }

{METHODDEF}
function dummy_consume_data (cinfo : j_decompress_ptr) : int;  
begin
  dummy_consume_data := JPEG_SUSPENDED;	{ Always indicate nothing was done }
end;


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Consume input data and store it in the full-image coefficient buffer.
  We read as much as one fully interleaved MCU row ("iMCU" row) per call,
  ie, v_samp_factor block rows for each component in the scan.
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.}

{METHODDEF}
function consume_data (cinfo : j_decompress_ptr) : int;  
var
  coef : my_coef_ptr;
  MCU_col_num : JDIMENSION;     { index of current MCU within row }
  blkn, ci, xindex, yindex, yoffset : int;
  start_col : JDIMENSION;
  buffer : array[0..MAX_COMPS_IN_SCAN-1] of JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  compptr : jpeg_component_info_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);

  { Align the virtual buffers for the components used in this scan. }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    buffer[ci] := cinfo^.mem^.access_virt_barray
      (j_common_ptr (cinfo), coef^.whole_image[compptr^.component_index],
       LongInt(cinfo^.input_iMCU_row) * compptr^.v_samp_factor,
       JDIMENSION (compptr^.v_samp_factor), TRUE);
    { Note: entropy decoder expects buffer to be zeroed,
      but this is handled automatically by the memory manager
      because we requested a pre-zeroed array. }

  end;

  { Loop to process one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.MCU_ctr to pred(cinfo^.MCUs_per_row) do
    begin
      { Construct list of pointers to DCT blocks belonging to this MCU }
      blkn := 0;		{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	start_col := LongInt(MCU_col_num) * compptr^.MCU_width;
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  buffer_ptr := JBLOCKROW(@ buffer[ci]^[yindex+yoffset]^[start_col]);
	  for xindex := 0 to pred(compptr^.MCU_width) do
          begin
	    coef^.MCU_buffer[blkn] := buffer_ptr;
            Inc(blkn);
            Inc(JBLOCK_PTR(buffer_ptr));
	  end;
	end;
      end;
      { Try to fetch the MCU. }
      if (not cinfo^.entropy^.decode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.MCU_ctr := MCU_col_num;
	consume_data := JPEG_SUSPENDED;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.MCU_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(cinfo^.input_iMCU_row);
  if (cinfo^.input_iMCU_row < cinfo^.total_iMCU_rows) then
  begin
    start_iMCU_row(cinfo);
    consume_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  { Completed the scan }
  cinfo^.inputctl^.finish_input_pass (cinfo);
  consume_data := JPEG_SCAN_COMPLETED;
end;


{ Decompress and return some data in the multi-pass case.
  Always attempts to emit one fully interleaved MCU row ("iMCU" row).
  Return value is JPEG_ROW_COMPLETED, JPEG_SCAN_COMPLETED, or JPEG_SUSPENDED.

  NB: output_buf contains a plane for each component in image. }

{METHODDEF}
function decompress_data (cinfo : j_decompress_ptr;
                          output_buf : JSAMPIMAGE) : int;
var
  coef : my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  block_num : JDIMENSION;
  ci, block_row, block_rows : int;
  buffer : JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  output_ptr : JSAMPARRAY;
  output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Force some input to be done if we are getting ahead of the input. }
  while (cinfo^.input_scan_number < cinfo^.output_scan_number) or
	 ((cinfo^.input_scan_number = cinfo^.output_scan_number) and
	  (LongInt(cinfo^.input_iMCU_row) <= cinfo^.output_iMCU_row)) do
  begin
    if (cinfo^.inputctl^.consume_input(cinfo) = JPEG_SUSPENDED) then
    begin
      decompress_data := JPEG_SUSPENDED;
      exit;
    end;
  end;

  { OK, output from the virtual arrays. }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Don't bother to IDCT an uninteresting component. }
    if (not compptr^.component_needed) then
      continue;
    { Align the virtual buffer for this component. }
    buffer := cinfo^.mem^.access_virt_barray
      (j_common_ptr (cinfo), coef^.whole_image[ci],
       cinfo^.output_iMCU_row * compptr^.v_samp_factor,
       JDIMENSION (compptr^.v_samp_factor), FALSE);
    { Count non-dummy DCT block rows in this iMCU row. }
    if (cinfo^.output_iMCU_row < LongInt(last_iMCU_row)) then
      block_rows := compptr^.v_samp_factor
    else
    begin
      { NB: can't use last_row_height here; it is input-side-dependent! }
      block_rows := int(LongInt(compptr^.height_in_blocks) mod compptr^.v_samp_factor);
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
    end;
    inverse_DCT := cinfo^.idct^.inverse_DCT[ci];
    output_ptr := output_buf^[ci];
    { Loop over all DCT blocks to be processed. }
    for block_row := 0 to pred(block_rows) do
    begin
      buffer_ptr := buffer^[block_row];
      output_col := 0;
      for block_num := 0 to pred(compptr^.width_in_blocks) do
      begin
	inverse_DCT (cinfo, compptr, JCOEFPTR (buffer_ptr),
			output_ptr, output_col);
	Inc(JBLOCK_PTR(buffer_ptr));
	Inc(output_col, compptr^.DCT_scaled_size);
      end;
      Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
    end;
    Inc(compptr);
  end;

  Inc(cinfo^.output_iMCU_row);
  if (cinfo^.output_iMCU_row < LongInt(cinfo^.total_iMCU_rows)) then
  begin
    decompress_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  decompress_data := JPEG_SCAN_COMPLETED;
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }


{$ifdef BLOCK_SMOOTHING_SUPPORTED}

{ This code applies interblock smoothing as described by section K.8
  of the JPEG standard: the first 5 AC coefficients are estimated from
  the DC values of a DCT block and its 8 neighboring blocks.
  We apply smoothing only for progressive JPEG decoding, and only if
  the coefficients it can estimate are not yet known to full precision. }

{ Natural-order array positions of the first 5 zigzag-order coefficients }
const
  Q01_POS = 1;
  Q10_POS = 8;
  Q20_POS = 16;
  Q11_POS = 9;
  Q02_POS = 2;

{ Determine whether block smoothing is applicable and safe.
  We also latch the current states of the coef_bits[] entries for the
  AC coefficients; otherwise, if the input side of the decompressor
  advances into a new scan, we might think the coefficients are known
  more accurately than they really are. }

{LOCAL}
function smoothing_ok (cinfo : j_decompress_ptr) : boolean;
var
  coef : my_coef_ptr;
  smoothing_useful : boolean;
  ci, coefi : int;
  compptr : jpeg_component_info_ptr;
  qtable : JQUANT_TBL_PTR;
  coef_bits : coef_bits_ptr;
  coef_bits_latch : Latch_Ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);
  smoothing_useful := FALSE;

  if (not cinfo^.progressive_mode) or (cinfo^.coef_bits = NIL) then
  begin
    smoothing_ok := FALSE;
    exit;
  end;

  { Allocate latch area if not already done }
  if (coef^.coef_bits_latch = NIL) then
    coef^.coef_bits_latch := Latch_Ptr(
      cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
                               cinfo^.num_components *
                               (SAVED_COEFS * SIZEOF(int))) );
  coef_bits_latch := (coef^.coef_bits_latch);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { All components' quantization values must already be latched. }
    qtable := compptr^.quant_table;
    if (qtable = NIL) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { Verify DC & first 5 AC quantizers are nonzero to avoid zero-divide. }
    if (qtable^.quantval[0] = 0) or
       (qtable^.quantval[Q01_POS] = 0) or
       (qtable^.quantval[Q10_POS] = 0) or
       (qtable^.quantval[Q20_POS] = 0) or
       (qtable^.quantval[Q11_POS] = 0) or
       (qtable^.quantval[Q02_POS] = 0) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { DC values must be at least partly known for all components. }
    coef_bits := @cinfo^.coef_bits^[ci];  { Nomssi }
    if (coef_bits^[0] < 0) then
    begin
      smoothing_ok := FALSE;
      exit;
    end;
    { Block smoothing is helpful if some AC coefficients remain inaccurate. }
    for coefi := 1 to 5 do
    begin
      coef_bits_latch^[coefi] := coef_bits^[coefi];
      if (coef_bits^[coefi] <> 0) then
	smoothing_useful := TRUE;
    end;
    Inc(coef_bits_latch {SAVED_COEFS});
    Inc(compptr);
  end;

  smoothing_ok := smoothing_useful;
end;


{ Variant of decompress_data for use when doing block smoothing. }

{METHODDEF}
function decompress_smooth_data (cinfo : j_decompress_ptr;
                        output_buf : JSAMPIMAGE) : int;
var
  coef : my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  block_num, last_block_column : JDIMENSION;
  ci, block_row, block_rows, access_rows : int;
  buffer : JBLOCKARRAY;
  buffer_ptr, prev_block_row, next_block_row : JBLOCKROW;
  output_ptr : JSAMPARRAY;
  output_col : JDIMENSION;
  compptr : jpeg_component_info_ptr;
  inverse_DCT : inverse_DCT_method_ptr;
  first_row, last_row : boolean;
  workspace : JBLOCK;
  coef_bits : Latch_Ptr; { coef_bits_ptr;  }
  quanttbl : JQUANT_TBL_PTR;
  Q00,Q01,Q02,Q10,Q11,Q20, num : INT32;
  DC1,DC2,DC3,DC4,DC5,DC6,DC7,DC8,DC9 : int;
  Al, pred : int;
var
  delta : JDIMENSION;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Force some input to be done if we are getting ahead of the input. }
  while (cinfo^.input_scan_number <= cinfo^.output_scan_number) and
        (not cinfo^.inputctl^.eoi_reached) do
  begin
    if (cinfo^.input_scan_number = cinfo^.output_scan_number) then
    begin
      { If input is working on current scan, we ordinarily want it to
        have completed the current row.  But if input scan is DC,
        we want it to keep one row ahead so that next block row's DC
        values are up to date. }

      if (cinfo^.Ss = 0) then
        delta := 1
      else
        delta := 0;
      if (LongInt(cinfo^.input_iMCU_row) > cinfo^.output_iMCU_row+LongInt(delta)) then
	break;
    end;
    if (cinfo^.inputctl^.consume_input(cinfo) = JPEG_SUSPENDED) then
    begin
      decompress_smooth_data := JPEG_SUSPENDED;
      exit;
    end;
  end;

  { OK, output from the virtual arrays. }
  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to (cinfo^.num_components-1) do
  begin
    { Don't bother to IDCT an uninteresting component. }
    if (not compptr^.component_needed) then
      continue;
    { Count non-dummy DCT block rows in this iMCU row. }
    if (cinfo^.output_iMCU_row < LongInt(last_iMCU_row)) then
    begin
      block_rows := compptr^.v_samp_factor;
      access_rows := block_rows * 2; { this and next iMCU row }
      last_row := FALSE;
    end
    else
    begin
      { NB: can't use last_row_height here; it is input-side-dependent! }
      block_rows := int (compptr^.height_in_blocks) mod compptr^.v_samp_factor;
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
      access_rows := block_rows; { this iMCU row only }
      last_row := TRUE;
    end;
    { Align the virtual buffer for this component. }
    if (cinfo^.output_iMCU_row > 0) then
    begin
      Inc(access_rows, compptr^.v_samp_factor); { prior iMCU row too }
      buffer := cinfo^.mem^.access_virt_barray
	(j_common_ptr (cinfo), coef^.whole_image[ci],
	 (cinfo^.output_iMCU_row - 1) * compptr^.v_samp_factor,
	 JDIMENSION (access_rows), FALSE);
      Inc(JBLOCKROW_PTR(buffer), compptr^.v_samp_factor); { point to current iMCU row }
      first_row := FALSE;
    end
    else
    begin
      buffer := cinfo^.mem^.access_virt_barray
	(j_common_ptr (cinfo), coef^.whole_image[ci],
	 JDIMENSION (0), JDIMENSION (access_rows), FALSE);
      first_row := TRUE;
    end;
    { Fetch component-dependent info }
    coef_bits := coef^.coef_bits_latch;
    Inc(coef_bits,  ci);                        { ci * SAVED_COEFS}
    quanttbl := compptr^.quant_table;
    Q00 := quanttbl^.quantval[0];
    Q01 := quanttbl^.quantval[Q01_POS];
    Q10 := quanttbl^.quantval[Q10_POS];
    Q20 := quanttbl^.quantval[Q20_POS];
    Q11 := quanttbl^.quantval[Q11_POS];
    Q02 := quanttbl^.quantval[Q02_POS];
    inverse_DCT := cinfo^.idct^.inverse_DCT[ci];
    output_ptr := output_buf^[ci];
    { Loop over all DCT blocks to be processed. }
    for block_row := 0 to (block_rows-1) do
    begin
      buffer_ptr := buffer^[block_row];
      if (first_row) and (block_row = 0) then
	prev_block_row := buffer_ptr
      else
	prev_block_row := buffer^[block_row-1];
      if (last_row) and (block_row = block_rows-1) then
	next_block_row := buffer_ptr
      else
	next_block_row := buffer^[block_row+1];
      { We fetch the surrounding DC values using a sliding-register approach.
        Initialize all nine here so as to do the right thing on narrow pics.}

      DC3 := int(prev_block_row^[0][0]);
      DC2 := DC3;
      DC1 := DC2;
      DC6 := int(buffer_ptr^[0][0]);
      DC5 := DC6;
      DC4 := DC5;
      DC9 := int(next_block_row^[0][0]);
      DC8 := DC9;
      DC7 := DC8 ;
      output_col := 0;
      last_block_column := compptr^.width_in_blocks - 1;
      for block_num := 0 to last_block_column do
      begin
	{ Fetch current DCT block into workspace so we can modify it. }
	jcopy_block_row(buffer_ptr, JBLOCKROW (@workspace), JDIMENSION(1));
	{ Update DC values }
	if (block_num < last_block_column) then
        begin
	  DC3 := int (prev_block_row^[1][0]);
	  DC6 := int (buffer_ptr^[1][0]);
	  DC9 := int (next_block_row^[1][0]);
	end;
	{ Compute coefficient estimates per K.8.
	  An estimate is applied only if coefficient is still zero,
	  and is not known to be fully accurate. }

	{ AC01 }
	Al := coef_bits^[1];
	if (Al <> 0) and (workspace[1] = 0) then
        begin
	  num := 36 * Q00 * (DC4 - DC6);
	  if (num >= 0) then
          begin
	    pred := int (((Q01 shl 7) + num) div (Q01 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q01 shl 7) - num) div (Q01 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[1] := JCOEF (pred);
	end;
	{ AC10 }
	Al := coef_bits^[2];
	if (Al <> 0) and (workspace[8] = 0) then
        begin
	  num := 36 * Q00 * (DC2 - DC8);
	  if (num >= 0) then
          begin
	    pred := int (((Q10 shl 7) + num) div (Q10 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q10 shl 7) - num) div (Q10 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[8] := JCOEF (pred);
	end;
	{ AC20 }
	Al := coef_bits^[3];
	if (Al <> 0) and (workspace[16] = 0) then
        begin
	  num := 9 * Q00 * (DC2 + DC8 - 2*DC5);
	  if (num >= 0) then
          begin
	    pred := int (((Q20 shl 7) + num) div (Q20 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q20 shl 7) - num) div (Q20 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[16] := JCOEF (pred);
	end;
	{ AC11 }
	Al := coef_bits^[4];
	if (Al <> 0) and (workspace[9] = 0) then
        begin
	  num := 5 * Q00 * (DC1 - DC3 - DC7 + DC9);
	  if (num >= 0) then
          begin
	    pred := int (((Q11 shl 7) + num) div (Q11 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q11 shl 7) - num) div (Q11 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[9] := JCOEF (pred);
	end;
	{ AC02 }
	Al := coef_bits^[5];
	if (Al <> 0) and (workspace[2] = 0) then
        begin
	  num := 9 * Q00 * (DC4 + DC6 - 2*DC5);
	  if (num >= 0) then
          begin
	    pred := int (((Q02 shl 7) + num) div (Q02 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	  end
          else
          begin
	    pred := int (((Q02 shl 7) - num) div (Q02 shl 8));
	    if (Al > 0) and (pred >= (1 shl Al)) then
	      pred := (1 shl Al)-1;
	    pred := -pred;
	  end;
	  workspace[2] := JCOEF (pred);
	end;
	{ OK, do the IDCT }
	inverse_DCT (cinfo, compptr, JCOEFPTR (@workspace),
			output_ptr, output_col);
	{ Advance for next column }
	DC1 := DC2; DC2 := DC3;
	DC4 := DC5; DC5 := DC6;
	DC7 := DC8; DC8 := DC9;
	Inc(JBLOCK_PTR(buffer_ptr));
        Inc(JBLOCK_PTR(prev_block_row));
        Inc(JBLOCK_PTR(next_block_row));
	Inc(output_col, compptr^.DCT_scaled_size);
      end;
      Inc(JSAMPROW_PTR(output_ptr), compptr^.DCT_scaled_size);
    end;
    Inc(compptr);
  end;

  Inc(cinfo^.output_iMCU_row);
  if (cinfo^.output_iMCU_row < LongInt(cinfo^.total_iMCU_rows)) then
  begin
    decompress_smooth_data := JPEG_ROW_COMPLETED;
    exit;
  end;
  decompress_smooth_data := JPEG_SCAN_COMPLETED;
end;

{$endif} { BLOCK_SMOOTHING_SUPPORTED }


{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_d_coef_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  coef : my_coef_ptr;
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
var
  ci, access_rows : int;
  compptr : jpeg_component_info_ptr;
{$endif}
var
  buffer : JBLOCK_PTR;
  i : int;
begin
  coef := my_coef_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr (cinfo), JPOOL_IMAGE,
				SIZEOF(my_coef_controller)) );
  cinfo^.coef := jpeg_d_coef_controller_ptr(coef);
  coef^.pub.start_input_pass := start_input_pass;
  coef^.pub.start_output_pass := start_output_pass;
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
  coef^.coef_bits_latch := NIL;
{$endif}

  { Create the coefficient buffer. }
  if (need_full_buffer) then
  begin
{$ifdef D_MULTISCAN_FILES_SUPPORTED}
    { Allocate a full-image virtual array for each component, }
    { padded to a multiple of samp_factor DCT blocks in each direction. }
    { Note we ask for a pre-zeroed array. }

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      access_rows := compptr^.v_samp_factor;
{$ifdef BLOCK_SMOOTHING_SUPPORTED}
      { If block smoothing could be used, need a bigger window }
      if (cinfo^.progressive_mode) then
	access_rows := access_rows * 3;
{$endif}
      coef^.whole_image[ci] := cinfo^.mem^.request_virt_barray
	(j_common_ptr (cinfo), JPOOL_IMAGE, TRUE,
	 JDIMENSION (jround_up( long(compptr^.width_in_blocks),
                                long(compptr^.h_samp_factor) )),
	 JDIMENSION (jround_up( long(compptr^.height_in_blocks),
				long(compptr^.v_samp_factor) )),
	 JDIMENSION (access_rows));
      Inc(compptr);
    end;
    coef^.pub.consume_data := consume_data;
    coef^.pub.decompress_data := decompress_data;
    coef^.pub.coef_arrays := @(coef^.whole_image);
                          { link to virtual arrays }
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
  end
  else
  begin
    { We only need a single-MCU buffer. }
    buffer := JBLOCK_PTR (
      cinfo^.mem^.alloc_large (j_common_ptr (cinfo), JPOOL_IMAGE,
				  D_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK)) );
    for i := 0 to pred(D_MAX_BLOCKS_IN_MCU) do
    begin
      coef^.MCU_buffer[i] := JBLOCKROW(buffer);
      Inc(buffer);
    end;
    coef^.pub.consume_data := dummy_consume_data;
    coef^.pub.decompress_data := decompress_onepass;
    coef^.pub.coef_arrays := NIL; { flag for no virtual arrays }
  end;
end;

end.
