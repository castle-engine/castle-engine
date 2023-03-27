Unit JcTrans;

{ This file contains library routines for transcoding compression,
  that is, writing raw DCT coefficient arrays to an output JPEG file.
  The routines in jcapimin.c will also be needed by a transcoder. }

{ Original : jctrans.c - Copyright (C) 1995-1998, Thomas G. Lane. }

interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jutils,
  jpeglib,
  jcapimin, jcparam, jcomapi, jcmaster, jchuff, jcphuff, jcmarker;

{ Compression initialization for writing raw-coefficient data.
  Before calling this, all parameters and a data destination must be set up.
  Call jpeg_finish_compress() to actually write the data.

  The number of passed virtual arrays must match cinfo^.num_components.
  Note that the virtual arrays need not be filled or even realized at
  the time write_coefficients is called; indeed, if the virtual arrays
  were requested from this compression object's memory manager, they
  typically will be realized during this routine and filled afterwards. }

{GLOBAL}
procedure jpeg_write_coefficients (cinfo : j_compress_ptr;
                                   coef_arrays : jvirt_barray_tbl_ptr);

{ Initialize the compression object with default parameters,
  then copy from the source object all parameters needed for lossless
  transcoding.  Parameters that can be varied without loss (such as
  scan script and Huffman optimization) are left in their default states. }

{GLOBAL}
procedure jpeg_copy_critical_parameters (srcinfo : j_decompress_ptr;
                                         dstinfo : j_compress_ptr);


implementation

{ Forward declarations }
{LOCAL}
procedure  transencode_master_selection(cinfo : j_compress_ptr;
                                        coef_arrays : jvirt_barray_tbl_ptr);
                                        forward;
{LOCAL}
procedure  transencode_coef_controller(cinfo : j_compress_ptr;
                                       coef_arrays : jvirt_barray_tbl_ptr);
                                       forward;


{ Compression initialization for writing raw-coefficient data.
  Before calling this, all parameters and a data destination must be set up.
  Call jpeg_finish_compress() to actually write the data.

  The number of passed virtual arrays must match cinfo^.num_components.
  Note that the virtual arrays need not be filled or even realized at
  the time write_coefficients is called; indeed, if the virtual arrays
  were requested from this compression object's memory manager, they
  typically will be realized during this routine and filled afterwards. }

{GLOBAL}
procedure jpeg_write_coefficients (cinfo : j_compress_ptr;
                                   coef_arrays : jvirt_barray_tbl_ptr);
begin
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Mark all tables to be written }
  jpeg_suppress_tables(cinfo, FALSE);
  { (Re)initialize error mgr and destination modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.dest^.init_destination (cinfo);
  { Perform master selection of active modules }
  transencode_master_selection(cinfo, coef_arrays);
  { Wait for jpeg_finish_compress() call }
  cinfo^.next_scanline := 0;    { so jpeg_write_marker works }
  cinfo^.global_state := CSTATE_WRCOEFS;
end;


{ Initialize the compression object with default parameters,
  then copy from the source object all parameters needed for lossless
  transcoding.  Parameters that can be varied without loss (such as
  scan script and Huffman optimization) are left in their default states. }

{GLOBAL}
procedure jpeg_copy_critical_parameters (srcinfo : j_decompress_ptr;
                                         dstinfo : j_compress_ptr);
var
  qtblptr : ^JQUANT_TBL_PTR;
  incomp, outcomp : jpeg_component_info_ptr;
  c_quant, slot_quant : JQUANT_TBL_PTR;
  tblno, ci, coefi : int;
begin

  { Safety check to ensure start_compress not called yet. }
  if (dstinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(dstinfo), JERR_BAD_STATE, dstinfo^.global_state);
  { Copy fundamental image dimensions }
  dstinfo^.image_width := srcinfo^.image_width;
  dstinfo^.image_height := srcinfo^.image_height;
  dstinfo^.input_components := srcinfo^.num_components;
  dstinfo^.in_color_space := srcinfo^.jpeg_color_space;
  { Initialize all parameters to default values }
  jpeg_set_defaults(dstinfo);
  { jpeg_set_defaults may choose wrong colorspace, eg YCbCr if input is RGB.
    Fix it to get the right header markers for the image colorspace. }

  jpeg_set_colorspace(dstinfo, srcinfo^.jpeg_color_space);
  dstinfo^.data_precision := srcinfo^.data_precision;
  dstinfo^.CCIR601_sampling := srcinfo^.CCIR601_sampling;
  { Copy the source's quantization tables. }
  for tblno := 0 to pred(NUM_QUANT_TBLS) do
  begin
    if (srcinfo^.quant_tbl_ptrs[tblno] <> NIL) then
    begin
      qtblptr := @dstinfo^.quant_tbl_ptrs[tblno];
      if (qtblptr^ = NIL) then
        qtblptr^ := jpeg_alloc_quant_table(j_common_ptr(dstinfo));
      MEMCOPY(@(qtblptr^)^.quantval,
              @srcinfo^.quant_tbl_ptrs[tblno]^.quantval,
              SIZEOF((qtblptr^)^.quantval));
      (qtblptr^)^.sent_table := FALSE;
    end;
  end;
  { Copy the source's per-component info.
    Note we assume jpeg_set_defaults has allocated the dest comp_info array. }

  dstinfo^.num_components := srcinfo^.num_components;
  if (dstinfo^.num_components < 1) or
     (dstinfo^.num_components > MAX_COMPONENTS) then
    ERREXIT2(j_common_ptr(dstinfo), JERR_COMPONENT_COUNT,
        dstinfo^.num_components,   MAX_COMPONENTS);
  incomp := jpeg_component_info_ptr(srcinfo^.comp_info);
  outcomp := jpeg_component_info_ptr(dstinfo^.comp_info);
  for ci := 0 to pred(dstinfo^.num_components) do
  begin

    outcomp^.component_id := incomp^.component_id;
    outcomp^.h_samp_factor := incomp^.h_samp_factor;
    outcomp^.v_samp_factor := incomp^.v_samp_factor;
    outcomp^.quant_tbl_no := incomp^.quant_tbl_no;
    { Make sure saved quantization table for component matches the qtable
      slot.  If not, the input file re-used this qtable slot.
      IJG encoder currently cannot duplicate this. }

    tblno := outcomp^.quant_tbl_no;
    if (tblno < 0) or (tblno >= NUM_QUANT_TBLS) or
       (srcinfo^.quant_tbl_ptrs[tblno] = NIL) then
      ERREXIT1(j_common_ptr(dstinfo), JERR_NO_QUANT_TABLE, tblno);
    slot_quant := srcinfo^.quant_tbl_ptrs[tblno];
    c_quant := incomp^.quant_table;
    if (c_quant <> NIL) then
    begin
      for coefi := 0 to pred(DCTSIZE2) do
      begin
        if (c_quant^.quantval[coefi] <> slot_quant^.quantval[coefi]) then
          ERREXIT1(j_common_ptr(dstinfo), JERR_MISMATCHED_QUANT_TABLE, tblno);
      end;
    end;
    { Note: we do not copy the source's Huffman table assignments;
      instead we rely on jpeg_set_colorspace to have made a suitable choice. }
    Inc(incomp);
    Inc(outcomp);
  end;
  { Also copy JFIF version and resolution information, if available.
    Strictly speaking this isn't "critical" info, but it's nearly
    always appropriate to copy it if available.  In particular,
    if the application chooses to copy JFIF 1.02 extension markers from
    the source file, we need to copy the version to make sure we don't
    emit a file that has 1.02 extensions but a claimed version of 1.01.
    We will *not*, however, copy version info from mislabeled "2.01" files. }

  if (srcinfo^.saw_JFIF_marker) then
  begin
    if (srcinfo^.JFIF_major_version = 1) then
    begin
      dstinfo^.JFIF_major_version := srcinfo^.JFIF_major_version;
      dstinfo^.JFIF_minor_version := srcinfo^.JFIF_minor_version;
    end;
    dstinfo^.density_unit := srcinfo^.density_unit;
    dstinfo^.X_density := srcinfo^.X_density;
    dstinfo^.Y_density := srcinfo^.Y_density;
  end;
end;


{ Master selection of compression modules for transcoding.
  This substitutes for jcinit.c's initialization of the full compressor. }

{LOCAL}
procedure transencode_master_selection (cinfo : j_compress_ptr;
                                        coef_arrays : jvirt_barray_tbl_ptr);
begin
  { Although we don't actually use input_components for transcoding,
    jcmaster.c's initial_setup will complain if input_components is 0. }

  cinfo^.input_components := 1;
  { Initialize master control (includes parameter checking/processing) }
  jinit_c_master_control(cinfo, TRUE { transcode only });

  { Entropy encoding: either Huffman or arithmetic coding. }
  if (cinfo^.arith_code) then
  begin
    ERREXIT(j_common_ptr(cinfo), JERR_ARITH_NOTIMPL);
  end
  else
  begin
    if (cinfo^.progressive_mode) then
    begin
{$ifdef C_PROGRESSIVE_SUPPORTED}
      jinit_phuff_encoder(cinfo);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
      jinit_huff_encoder(cinfo);
  end;

  { We need a special coefficient buffer controller. }
  transencode_coef_controller(cinfo, coef_arrays);

  jinit_marker_writer(cinfo);

  { We can now tell the memory manager to allocate virtual arrays. }
  cinfo^.mem^.realize_virt_arrays (j_common_ptr(cinfo));

  { Write the datastream header (SOI, JFIF) immediately.
    Frame and scan headers are postponed till later.
    This lets application insert special markers after the SOI. }

  cinfo^.marker^.write_file_header (cinfo);
end;


{ The rest of this file is a special implementation of the coefficient
  buffer controller.  This is similar to jccoefct.c, but it handles only
  output from presupplied virtual arrays.  Furthermore, we generate any
  dummy padding blocks on-the-fly rather than expecting them to be present
  in the arrays. }

{ Private buffer controller object }

type
  my_coef_ptr = ^my_coef_controller;
  my_coef_controller = record
    pub : jpeg_c_coef_controller; { public fields }

    iMCU_row_num : JDIMENSION;    { iMCU row # within image }
    mcu_ctr : JDIMENSION;         { counts MCUs processed in current row }
    MCU_vert_offset : int;        { counts MCU rows within iMCU row }
    MCU_rows_per_iMCU_row : int;  { number of such rows needed }

    { Virtual block array for each component. }
    whole_image : jvirt_barray_tbl_ptr;

    { Workspace for constructing dummy blocks at right/bottom edges. }
    dummy_buffer : array[0..C_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;
  end; {my_coef_controller;}


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
                           pass_mode :  J_BUF_MODE); far;
var
  coef : my_coef_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);

  if (pass_mode <> JBUF_CRANK_DEST) then
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  coef^.iMCU_row_num := 0;
  start_iMCU_row(cinfo);
end;


{ Process some data.
  We process the equivalent of one fully interleaved MCU row ("iMCU" row)
  per call, ie, v_samp_factor block rows for each component in the scan.
  The data is obtained from the virtual arrays and fed to the entropy coder.
  Returns TRUE if the iMCU row is completed, FALSE if suspended.

  NB: input_buf is ignored; it is likely to be a NIL pointer. }

{METHODDEF}
function compress_output (cinfo : j_compress_ptr;
                          input_buf : JSAMPIMAGE) : boolean; far;
var
  coef : my_coef_ptr;
  MCU_col_num : JDIMENSION;     { index of current MCU within row }
  last_MCU_col : JDIMENSION;
  last_iMCU_row : JDIMENSION;
  blkn, ci, xindex, yindex, yoffset, blockcnt : int;
  start_col : JDIMENSION;
  buffer : array[0..MAX_COMPS_IN_SCAN-1] of JBLOCKARRAY;
  MCU_buffer : array[0..C_MAX_BLOCKS_IN_MCU-1] of JBLOCKROW;
  buffer_ptr : JBLOCKROW;
  compptr : jpeg_component_info_ptr;
begin
  coef := my_coef_ptr (cinfo^.coef);
  last_MCU_col := cinfo^.MCUs_per_row - 1;
  last_iMCU_row := cinfo^.total_iMCU_rows - 1;

  { Align the virtual buffers for the components used in this scan. }
  for ci := 0 to pred(cinfo^.comps_in_scan) do
  begin
    compptr := cinfo^.cur_comp_info[ci];
    buffer[ci] := cinfo^.mem^.access_virt_barray
      (j_common_ptr(cinfo), coef^.whole_image^[compptr^.component_index],
       coef^.iMCU_row_num * compptr^.v_samp_factor,
       JDIMENSION(compptr^.v_samp_factor), FALSE);
  end;

  { Loop to process one whole iMCU row }
  for yoffset := coef^.MCU_vert_offset to pred(coef^.MCU_rows_per_iMCU_row) do
  begin
    for MCU_col_num := coef^.mcu_ctr to pred(cinfo^.MCUs_per_row) do
    begin
      { Construct list of pointers to DCT blocks belonging to this MCU }
      blkn := 0;                        { index of current DCT block within MCU }
      for ci := 0 to pred(cinfo^.comps_in_scan) do
      begin
        compptr := cinfo^.cur_comp_info[ci];
        start_col := MCU_col_num * compptr^.MCU_width;
        if (MCU_col_num < last_MCU_col) then
          blockcnt := compptr^.MCU_width
        else
          blockcnt := compptr^.last_col_width;
        for yindex := 0 to pred(compptr^.MCU_height) do
        begin
          if (coef^.iMCU_row_num < last_iMCU_row) or
             (yindex+yoffset < compptr^.last_row_height) then
          begin
            { Fill in pointers to real blocks in this row }
            buffer_ptr := JBLOCKROW(@ buffer[ci]^[yindex+yoffset]^[start_col]);
            for xindex := 0 to pred(blockcnt) do
            begin
              MCU_buffer[blkn] := buffer_ptr;
              Inc(blkn);
              Inc(JBLOCK_PTR(buffer_ptr));
            end;
            xindex := blockcnt;
          end
          else
          begin
            { At bottom of image, need a whole row of dummy blocks }
            xindex := 0;
          end;
          { Fill in any dummy blocks needed in this row.
            Dummy blocks are filled in the same way as in jccoefct.c:
            all zeroes in the AC entries, DC entries equal to previous
            block's DC value.  The init routine has already zeroed the
            AC entries, so we need only set the DC entries correctly. }

          while (xindex < compptr^.MCU_width) do
          begin
            MCU_buffer[blkn] := coef^.dummy_buffer[blkn];
            MCU_buffer[blkn]^[0][0] := MCU_buffer[blkn-1]^[0][0];
            Inc(xindex);
            Inc(blkn);
          end;
        end;
      end;
      { Try to write the MCU. }
      if (not cinfo^.entropy^.encode_mcu (cinfo, MCU_buffer)) then
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


{ Initialize coefficient buffer controller.

  Each passed coefficient array must be the right size for that
  coefficient: width_in_blocks wide and height_in_blocks high,
  with unitheight at least v_samp_factor. }

{LOCAL}
procedure transencode_coef_controller (cinfo : j_compress_ptr;
                                       coef_arrays : jvirt_barray_tbl_ptr);
var
  coef : my_coef_ptr;
  buffer : JBLOCKROW;
  i : int;
begin
  coef := my_coef_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_coef_controller)));
  cinfo^.coef := jpeg_c_coef_controller_ptr (coef);
  coef^.pub.start_pass := start_pass_coef;
  coef^.pub.compress_data := compress_output;

  { Save pointer to virtual arrays }
  coef^.whole_image := coef_arrays;

  { Allocate and pre-zero space for dummy DCT blocks. }
  buffer := JBLOCKROW(
    cinfo^.mem^.alloc_large (j_common_ptr(cinfo), JPOOL_IMAGE,
                                C_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK)) );
  jzero_far({FAR} voidp(buffer), C_MAX_BLOCKS_IN_MCU * SIZEOF(JBLOCK));
  for i := 0 to pred(C_MAX_BLOCKS_IN_MCU) do
  begin
    coef^.dummy_buffer[i] := JBLOCKROW(@ buffer^[i]);
  end;
end;

end.
