unit imjccoefct;

{ This file contains the coefficient buffer controller for compression.
  This controller is the top level of the JPEG compressor proper.
  The coefficient buffer lies between forward-DCT and entropy encoding steps.}

{ Original: jccoefct.c; Copyright (C) 1994-1997, Thomas G. Lane. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjerror,
  imjdeferr,
  imjutils,
  imjpeglib;

{ We use a full-image coefficient buffer when doing Huffman optimization,
  and also for writing multiple-scan JPEG files.  In all cases, the DCT
  step is run during the first pass, and subsequent passes need only read
  the buffered coefficients. }
{$ifdef ENTROPY_OPT_SUPPORTED}
  {$define FULL_COEF_BUFFER_SUPPORTED}
{$else}
  {$ifdef C_MULTISCAN_FILES_SUPPORTED}
    {$define FULL_COEF_BUFFER_SUPPORTED}
  {$endif}
{$endif}

{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_c_coef_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

implementation

{ Private buffer controller object }

type
  my_coef_ptr = ^my_coef_controller;
  my_coef_controller = record
    pub : jpeg_c_coef_controller; { public fields }

    iMCU_row_num : JDIMENSION;	{ iMCU row # within image }
    mcu_ctr : JDIMENSION;	{ counts MCUs processed in current row }
    MCU_vert_offset : int;	{ counts MCU rows within iMCU row }
    MCU_rows_per_iMCU_row : int;  { number of such rows needed }

    { For single-pass compression, it's sufficient to buffer just one MCU
      (although this may prove a bit slow in practice).  We allocate a
      workspace of C_MAX_BLOCKS_IN_MCU coefficient blocks, and reuse it for each
      MCU constructed and sent.  (On 80x86, the workspace is FAR even though
      it's not really very big; this is to keep the module interfaces unchanged
      when a large coefficient buffer is necessary.)
      In multi-pass modes, this array points to the current MCU's blocks
      within the virtual arrays. }

    MCU_buffer : array[0..C_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;

    { In multi-pass modes, we need a virtual block array for each component. }
    whole_image : array[0..MAX_COMPONENTS-1] of jvirt_barray_ptr;
  end;


{ Forward declarations }
{METHODDEF}
function compress_data(cinfo : j_compress_ptr;
                       input_buf : JSAMPIMAGE) : boolean; forward;
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
{METHODDEF}
function compress_first_pass(cinfo : j_compress_ptr;
                             input_buf : JSAMPIMAGE) : boolean;  forward;
{METHODDEF}
function compress_output(cinfo : j_compress_ptr;
                         input_buf : JSAMPIMAGE) : boolean;  forward;
{$endif}


{LOCAL}
procedure start_iMCU_row (cinfo : j_compress_ptr);
{ Reset within-iMCU-row counters for a new row }
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
    if (coef^.iMCU_row_num < (cinfo^.total_iMCU_rows-1)) then
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.v_samp_factor
    else
      coef^.MCU_rows_per_iMCU_row := cinfo^.cur_comp_info[0]^.last_row_height;
  end;

  coef^.mcu_ctr := 0;
  coef^.MCU_vert_offset := 0;
end;


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_coef (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE);  
var
  coef : my_coef_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);

  coef^.iMCU_row_num := 0;
  start_iMCU_row(cinfo);

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
      if (coef^.whole_image[0] <> NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_data;
    end;
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
  JBUF_SAVE_AND_PASS:
    begin
      if (coef^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_first_pass;
    end;
  JBUF_CRANK_DEST:
    begin
      if (coef^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      coef^.pub.compress_data := compress_output;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data in the single-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the image.
  Returns TRUE if the iMCU row is completed, FALSE if suspended.

  NB: input_buf contains a plane for each component in image,
  which we index according to the component's SOF position. }


{METHODDEF}
function compress_data (cinfo : j_compress_ptr;
                        input_buf : JSAMPIMAGE) : boolean;
var
  coef : my_coef_ptr;
  MCU_col_num : JDIMENSION;	{ index of current MCU within row }
  last_MCU_col : JDIMENSION;
  last_iMCU_row : JDIMENSION;
  blkn, bi, ci, yindex, yoffset, blockcnt : int;
  ypos, xpos : JDIMENSION;
  compptr : jpeg_component_info_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_MCU_col := cinfo^.MCUs_per_row - 1;
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Loop to write as much as one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.mcu_ctr to last_MCU_col do
    begin
      { Determine where data comes from in input_buf and do the DCT thing.
        Each call on forward_DCT processes a horizontal row of DCT blocks
        as wide as an MCU; we rely on having allocated the MCU_buffer[] blocks
        sequentially.  Dummy blocks at the right or bottom edge are filled in
        specially.  The data in them does not matter for image reconstruction,
        so we fill them with values that will encode to the smallest amount of
        data, viz: all zeroes in the AC entries, DC entries equal to previous
        block's DC value.  (Thanks to Thomas Kinsman for this idea.) }

      blkn := 0;
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
        if (MCU_col_num < last_MCU_col)  then
          blockcnt := compptr^.MCU_width
        else
          blockcnt := compptr^.last_col_width;
	xpos := MCU_col_num * JDIMENSION(compptr^.MCU_sample_width);
	ypos := yoffset * DCTSIZE;      { ypos = (yoffset+yindex) * DCTSIZE }
	for yindex := 0 to pred(compptr^.MCU_height) do
        begin
	  if (coef^.iMCU_row_num < last_iMCU_row) or
	     (yoffset+yindex < compptr^.last_row_height) then
          begin
	    cinfo^.fdct^.forward_DCT (cinfo, compptr,
				      input_buf^[compptr^.component_index],
				      coef^.MCU_buffer[blkn],
				      ypos, xpos, JDIMENSION (blockcnt));

	    if (blockcnt < compptr^.MCU_width) then
            begin
	      { Create some dummy blocks at the right edge of the image. }
	      jzero_far({FAR}pointer(coef^.MCU_buffer[blkn + blockcnt]),
			(compptr^.MCU_width - blockcnt) * SIZEOF(JBLOCK));
	      for bi := blockcnt to pred(compptr^.MCU_width) do
              begin
		coef^.MCU_buffer[blkn+bi]^[0][0] := coef^.MCU_buffer[blkn+bi-1]^[0][0];
	      end;
	    end;
	  end
          else
          begin
	    { Create a row of dummy blocks at the bottom of the image. }
	    jzero_far({FAR}pointer(coef^.MCU_buffer[blkn]),
		      compptr^.MCU_width * SIZEOF(JBLOCK));
	    for bi := 0 to pred(compptr^.MCU_width) do
            begin
	      coef^.MCU_buffer[blkn+bi]^[0][0] := coef^.MCU_buffer[blkn-1]^[0][0];
	    end;
	  end;
	  Inc(blkn, compptr^.MCU_width);
	  Inc(ypos, DCTSIZE);
	end;
      end;
      { Try to write the MCU.  In event of a suspension failure, we will
        re-DCT the MCU on restart (a bit inefficient, could be fixed...) }

      if (not cinfo^.entropy^.encode_mcu (cinfo, JBLOCKARRAY(@coef^.MCU_buffer)^)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.mcu_ctr := MCU_col_num;
	compress_data := FALSE;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.mcu_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(coef^.iMCU_row_num);
  start_iMCU_row(cinfo);
  compress_data := TRUE;
end;


{$ifdef FULL_COEF_BUFFER_SUPPORTED}

{ Process some data in the first pass of a multi-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the image.
  This amount of data is read from the source buffer, DCT'd and quantized,
  and saved into the virtual arrays.  We also generate suitable dummy blocks
  as needed at the right and lower edges.  (The dummy blocks are constructed
  in the virtual arrays, which have been padded appropriately.)  This makes
  it possible for subsequent passes not to worry about real vs. dummy blocks.

  We must also emit the data to the entropy encoder.  This is conveniently
  done by calling compress_output() after we've loaded the current strip
  of the virtual arrays.

  NB: input_buf contains a plane for each component in image.  All
  components are DCT'd and loaded into the virtual arrays in this pass.
  However, it may be that only a subset of the components are emitted to
  the entropy encoder during this first pass; be careful about looking
  at the scan-dependent variables (MCU dimensions, etc). }

{METHODDEF}
function compress_first_pass (cinfo : j_compress_ptr;
                              input_buf : JSAMPIMAGE) : boolean;
var
  coef : my_coef_ptr;
  last_iMCU_row : JDIMENSION;
  blocks_across, MCUs_across, MCUindex : JDIMENSION;
  bi, ci, h_samp_factor, block_row, block_rows, ndummy : int;
  lastDC : JCOEF;
  compptr : jpeg_component_info_ptr;
  buffer : JBLOCKARRAY;
  thisblockrow, lastblockrow : JBLOCKROW;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Align the virtual buffer for this component. }
    buffer := cinfo^.mem^.access_virt_barray
      (j_common_ptr(cinfo), coef^.whole_image[ci],
       coef^.iMCU_row_num * JDIMENSION(compptr^.v_samp_factor),
       JDIMENSION (compptr^.v_samp_factor), TRUE);
    { Count non-dummy DCT block rows in this iMCU row. }
    if (coef^.iMCU_row_num < last_iMCU_row) then
      block_rows := compptr^.v_samp_factor
    else
    begin
      { NB: can't use last_row_height here, since may not be set! }
      block_rows := int (compptr^.height_in_blocks) mod compptr^.v_samp_factor;
      if (block_rows = 0) then
        block_rows := compptr^.v_samp_factor;
    end;
    blocks_across := compptr^.width_in_blocks;
    h_samp_factor := compptr^.h_samp_factor;
    { Count number of dummy blocks to be added at the right margin. }
    ndummy := int (blocks_across) mod h_samp_factor;
    if (ndummy > 0) then
      ndummy := h_samp_factor - ndummy;
    { Perform DCT for all non-dummy blocks in this iMCU row.  Each call
      on forward_DCT processes a complete horizontal row of DCT blocks. }

    for block_row := 0 to pred(block_rows) do
    begin
      thisblockrow := buffer^[block_row];
      cinfo^.fdct^.forward_DCT (cinfo, compptr,
	                        input_buf^[ci],
                                thisblockrow,
				JDIMENSION (block_row * DCTSIZE),
				JDIMENSION (0),
                                blocks_across);
      if (ndummy > 0) then
      begin
	{ Create dummy blocks at the right edge of the image. }
	Inc(JBLOCK_PTR(thisblockrow), blocks_across); { => first dummy block }
	jzero_far({FAR}pointer(thisblockrow), ndummy * SIZEOF(JBLOCK));
	{lastDC := thisblockrow^[-1][0];}
        { work around Range Checking }
        Dec(JBLOCK_PTR(thisblockrow));
        lastDC := thisblockrow^[0][0];
        Inc(JBLOCK_PTR(thisblockrow));

	for bi := 0 to pred(ndummy) do
        begin
	  thisblockrow^[bi][0] := lastDC;
	end;
      end;
    end;
    { If at end of image, create dummy block rows as needed.
      The tricky part here is that within each MCU, we want the DC values
      of the dummy blocks to match the last real block's DC value.
      This squeezes a few more bytes out of the resulting file... }

    if (coef^.iMCU_row_num = last_iMCU_row) then
    begin
      Inc(blocks_across, ndummy);       { include lower right corner }
      MCUs_across := blocks_across div JDIMENSION(h_samp_factor);
      for block_row := block_rows to pred(compptr^.v_samp_factor) do
      begin
	thisblockrow := buffer^[block_row];
	lastblockrow := buffer^[block_row-1];
	jzero_far({FAR} pointer(thisblockrow),
		  size_t(blocks_across * SIZEOF(JBLOCK)));
	for MCUindex := 0 to pred(MCUs_across) do
        begin
	  lastDC := lastblockrow^[h_samp_factor-1][0];
	  for bi := 0 to pred(h_samp_factor) do
          begin
	    thisblockrow^[bi][0] := lastDC;
	  end;
	  Inc(JBLOCK_PTR(thisblockrow), h_samp_factor); { advance to next MCU in row }
	  Inc(JBLOCK_PTR(lastblockrow), h_samp_factor);
	end;
      end;
    end;
    Inc(compptr);
  end;
  { NB: compress_output will increment iMCU_row_num if successful.
    A suspension return will result in redoing all the work above next time.}


  { Emit data to the entropy encoder, sharing code with subsequent passes }
  compress_first_pass := compress_output(cinfo, input_buf);
end;


{ Process some data in subsequent passes of a multi-pass case.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the scan.
  The data is obtained from the virtual arrays and fed to the entropy coder.
  Returns TRUE if the iMCU row is completed, FALSE if suspended.

  NB: input_buf is ignored; it is likely to be a NIL pointer. }

{METHODDEF}
function compress_output (cinfo : j_compress_ptr;
                          input_buf : JSAMPIMAGE) : boolean;
var
  coef : my_coef_ptr;
  MCU_col_num : JDIMENSION;	{ index of current MCU within row }
  blkn, ci, xindex, yindex, yoffset : int;
  start_col : JDIMENSION;
  buffer : array[0..MAX_COMPS_IN_SCAN-1] of JBLOCKARRAY;
  buffer_ptr : JBLOCKROW;
  compptr : jpeg_component_info_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);

  { Align the virtual buffers for the components used in this scan.
    NB: during first pass, this is safe only because the buffers will
    already be aligned properly, so jmemmgr.c won't need to do any I/O. }

  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    buffer[ci] := cinfo^.mem^.access_virt_barray (
       j_common_ptr(cinfo), coef^.whole_image[compptr^.component_index],
       coef^.iMCU_row_num * JDIMENSION(compptr^.v_samp_factor),
       JDIMENSION (compptr^.v_samp_factor), FALSE);
  end;

  { Loop to process one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.mcu_ctr to pred(cinfo^.MCUs_per_row) do
    begin
      { Construct list of pointers to DCT blocks belonging to this MCU }
      blkn := 0;			{ index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
	compptr := cinfo^.cur_comp_info[ci];
	start_col := MCU_col_num * JDIMENSION(compptr^.MCU_width);
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
      { Try to write the MCU. }
      if (not cinfo^.entropy^.encode_mcu (cinfo, coef^.MCU_buffer)) then
      begin
	{ Suspension forced; update state counters and exit }
	coef^.MCU_vert_offset := yoffset;
	coef^.mcu_ctr := MCU_col_num;
	compress_output := FALSE;
        exit;
      end;
    end;
    { Completed an MCU row, but perhaps not an iMCU row }
    coef^.mcu_ctr := 0;
  end;
  { Completed the iMCU row, advance counters for next one }
  Inc(coef^.iMCU_row_num);
  start_iMCU_row(cinfo);
  compress_output := TRUE;
end;

{$endif} { FULL_COEF_BUFFER_SUPPORTED }


{ Initialize coefficient buffer controller. }

{GLOBAL}
procedure jinit_c_coef_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  coef : my_coef_ptr;
var
  buffer : JBLOCKROW;
  i : int;
var
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  coef := my_coef_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_coef_controller)) );
  cinfo^.coef := jpeg_c_coef_controller_ptr(coef);
  coef^.pub.start_pass := start_pass_coef;

  { Create the coefficient buffer. }
  if (need_full_buffer) then
  begin
{$ifdef FULL_COEF_BUFFER_SUPPORTED}
    { Allocate a full-image virtual array for each component, }
    { padded to a multiple of samp_factor DCT blocks in each direction. }

    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      coef^.whole_image[ci] := cinfo^.mem^.request_virt_barray
	(j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
	 JDIMENSION (jround_up( long (compptr^.width_in_blocks),
				long (compptr^.h_samp_factor) )),
	 JDIMENSION (jround_up(long (compptr^.height_in_blocks),
				long (compptr^.v_samp_factor))),
	 JDIMENSION (compptr^.v_samp_factor));
      Inc(compptr);
    end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
  end
  else
  begin
    { We only need a single-MCU buffer. }
    buffer := JBLOCKROW (
      cinfo^.mem^.alloc_large (j_common_ptr(cinfo), JPOOL_IMAGE,
				  C_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK)) );
    for i := 0 to pred(C_MAX_BLOCKS_IN_MCU) do
    begin
      coef^.MCU_buffer[i] := JBLOCKROW(@ buffer^[i]);
    end;
    coef^.whole_image[0] := NIL; { flag for no virtual arrays }
  end;
end;

end.
