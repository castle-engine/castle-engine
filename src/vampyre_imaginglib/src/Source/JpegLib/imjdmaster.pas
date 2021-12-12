unit imjdmaster;

{ This file contains master control logic for the JPEG decompressor.
  These routines are concerned with selecting the modules to be executed
  and with determining the number of passes and the work to be done in each
  pass. }

{ Original: jdmaster.c ; Copyright (C) 1991-1998, Thomas G. Lane.  }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjutils,
  imjerror,
  imjdeferr,
  imjdcolor, imjdsample, imjdpostct, imjddctmgr, imjdphuff,
  imjdhuff, imjdcoefct, imjdmainct,
{$ifdef QUANT_1PASS_SUPPORTED}
  imjquant1,
{$endif}
{$ifdef QUANT_2PASS_SUPPORTED}
  imjquant2,
{$endif}
{$ifdef UPSAMPLE_MERGING_SUPPORTED}
  imjdmerge,
{$endif}
  imjpeglib;


{ Compute output image dimensions and related values.
  NOTE: this is exported for possible use by application.
  Hence it mustn't do anything that can't be done twice.
  Also note that it may be called before the master module is initialized! }

{GLOBAL}
procedure jpeg_calc_output_dimensions (cinfo : j_decompress_ptr);
{ Do computations that are needed before master selection phase }


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{GLOBAL}
procedure jpeg_new_colormap (cinfo : j_decompress_ptr);

{$endif}

{ Initialize master decompression control and select active modules.
  This is performed at the start of jpeg_start_decompress. }

{GLOBAL}
procedure jinit_master_decompress (cinfo : j_decompress_ptr);

implementation

{ Private state }

type
  my_master_ptr = ^my_decomp_master;
  my_decomp_master = record
    pub : jpeg_decomp_master; { public fields }

    pass_number : int;		{ # of passes completed }

    using_merged_upsample : boolean; { TRUE if using merged upsample/cconvert }

    { Saved references to initialized quantizer modules,
      in case we need to switch modes. }

    quantizer_1pass : jpeg_color_quantizer_ptr;
    quantizer_2pass : jpeg_color_quantizer_ptr;
  end;

{ Determine whether merged upsample/color conversion should be used.
  CRUCIAL: this must match the actual capabilities of jdmerge.c! }

{LOCAL}
function use_merged_upsample (cinfo : j_decompress_ptr) : boolean;
var
  compptr : jpeg_component_info_list_ptr;
begin
  compptr := cinfo^.comp_info;

{$ifdef UPSAMPLE_MERGING_SUPPORTED}
  { Merging is the equivalent of plain box-filter upsampling }
  if (cinfo^.do_fancy_upsampling) or (cinfo^.CCIR601_sampling) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { jdmerge.c only supports YCC=>RGB color conversion }
  if (cinfo^.jpeg_color_space <> JCS_YCbCr) or (cinfo^.num_components <> 3)
  or (cinfo^.out_color_space <> JCS_RGB)
  or (cinfo^.out_color_components <> RGB_PIXELSIZE) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;

  { and it only handles 2h1v or 2h2v sampling ratios }
  if (compptr^[0].h_samp_factor <> 2) or
     (compptr^[1].h_samp_factor <> 1) or
     (compptr^[2].h_samp_factor <> 1) or
     (compptr^[0].v_samp_factor >  2) or
     (compptr^[1].v_samp_factor <> 1) or
     (compptr^[2].v_samp_factor <> 1) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { furthermore, it doesn't work if we've scaled the IDCTs differently }
  if (compptr^[0].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) or
     (compptr^[1].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) or
     (compptr^[2].DCT_scaled_size <> cinfo^.min_DCT_scaled_size) then
  begin
    use_merged_upsample := FALSE;
    exit;
  end;
  { ??? also need to test for upsample-time rescaling, when & if supported }
  use_merged_upsample := TRUE;			{ by golly, it'll work... }
{$else}
  use_merged_upsample := FALSE;
{$endif}
end;


{ Compute output image dimensions and related values.
  NOTE: this is exported for possible use by application.
  Hence it mustn't do anything that can't be done twice.
  Also note that it may be called before the master module is initialized! }

{GLOBAL}
procedure jpeg_calc_output_dimensions (cinfo : j_decompress_ptr);
{ Do computations that are needed before master selection phase }
{$ifdef IDCT_SCALING_SUPPORTED}
var
  ci : int;
  compptr : jpeg_component_info_ptr;
{$endif}
var
  ssize : int;
begin
  { Prevent application from calling me at wrong times }
  if (cinfo^.global_state <> DSTATE_READY) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

{$ifdef IDCT_SCALING_SUPPORTED}

  { Compute actual output image dimensions and DCT scaling choices. }
  if (cinfo^.scale_num * 8 <= cinfo^.scale_denom) then
  begin
    { Provide 1/8 scaling }
    cinfo^.output_width := JDIMENSION (
      jdiv_round_up( long(cinfo^.image_width), long(8)) );
    cinfo^.output_height := JDIMENSION (
      jdiv_round_up( long(cinfo^.image_height), long(8)) );
    cinfo^.min_DCT_scaled_size := 1;
  end
  else
    if (cinfo^.scale_num * 4 <= cinfo^.scale_denom) then
    begin
      { Provide 1/4 scaling }
      cinfo^.output_width := JDIMENSION (
        jdiv_round_up( long (cinfo^.image_width), long(4)) );
      cinfo^.output_height := JDIMENSION (
        jdiv_round_up( long (cinfo^.image_height), long(4)) );
      cinfo^.min_DCT_scaled_size := 2;
    end
    else
      if (cinfo^.scale_num * 2 <= cinfo^.scale_denom) then
      begin
        { Provide 1/2 scaling }
        cinfo^.output_width := JDIMENSION (
          jdiv_round_up( long(cinfo^.image_width), long(2)) );
        cinfo^.output_height := JDIMENSION (
          jdiv_round_up( long(cinfo^.image_height), long(2)) );
        cinfo^.min_DCT_scaled_size := 4;
      end
      else
      begin
        { Provide 1/1 scaling }
        cinfo^.output_width := cinfo^.image_width;
        cinfo^.output_height := cinfo^.image_height;
        cinfo^.min_DCT_scaled_size := DCTSIZE;
      end;
  { In selecting the actual DCT scaling for each component, we try to
    scale up the chroma components via IDCT scaling rather than upsampling.
    This saves time if the upsampler gets to use 1:1 scaling.
    Note this code assumes that the supported DCT scalings are powers of 2. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    ssize := cinfo^.min_DCT_scaled_size;
    while (ssize < DCTSIZE) and
	  ((compptr^.h_samp_factor * ssize * 2 <=
	    cinfo^.max_h_samp_factor * cinfo^.min_DCT_scaled_size) and
	   (compptr^.v_samp_factor * ssize * 2 <=
	    cinfo^.max_v_samp_factor * cinfo^.min_DCT_scaled_size)) do
    begin
      ssize := ssize * 2;
    end;
    compptr^.DCT_scaled_size := ssize;
    Inc(compptr);
  end;

  { Recompute downsampled dimensions of components;
    application needs to know these if using raw downsampled data. }

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Size in samples, after IDCT scaling }
    compptr^.downsampled_width := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_width) *
		    long (compptr^.h_samp_factor * compptr^.DCT_scaled_size),
		    long (cinfo^.max_h_samp_factor * DCTSIZE)) );
    compptr^.downsampled_height := JDIMENSION (
      jdiv_round_up(long (cinfo^.image_height) *
		    long (compptr^.v_samp_factor * compptr^.DCT_scaled_size),
		    long (cinfo^.max_v_samp_factor * DCTSIZE)) );
    Inc(compptr);
  end;

{$else} { !IDCT_SCALING_SUPPORTED }

  { Hardwire it to "no scaling" }
  cinfo^.output_width := cinfo^.image_width;
  cinfo^.output_height := cinfo^.image_height;
  { jdinput.c has already initialized DCT_scaled_size to DCTSIZE,
    and has computed unscaled downsampled_width and downsampled_height. }

{$endif} { IDCT_SCALING_SUPPORTED }

  { Report number of components in selected colorspace. }
  { Probably this should be in the color conversion module... }
  case (cinfo^.out_color_space) of
  JCS_GRAYSCALE:
    cinfo^.out_color_components := 1;
{$ifndef RGB_PIXELSIZE_IS_3}
  JCS_RGB:
    cinfo^.out_color_components := RGB_PIXELSIZE;
{$else}
  JCS_RGB,
{$endif} { else share code with YCbCr }
  JCS_YCbCr:
    cinfo^.out_color_components := 3;
  JCS_CMYK,
  JCS_YCCK:
    cinfo^.out_color_components := 4;
  else			{ else must be same colorspace as in file }
    cinfo^.out_color_components := cinfo^.num_components;
  end;
  if (cinfo^.quantize_colors) then
    cinfo^.output_components := 1
  else
    cinfo^.output_components := cinfo^.out_color_components;

  { See if upsampler will want to emit more than one row at a time }
  if (use_merged_upsample(cinfo)) then
    cinfo^.rec_outbuf_height := cinfo^.max_v_samp_factor
  else
    cinfo^.rec_outbuf_height := 1;
end;


{ Several decompression processes need to range-limit values to the range
  0..MAXJSAMPLE; the input value may fall somewhat outside this range
  due to noise introduced by quantization, roundoff error, etc.  These
  processes are inner loops and need to be as fast as possible.  On most
  machines, particularly CPUs with pipelines or instruction prefetch,
  a (subscript-check-less) C table lookup
 		x := sample_range_limit[x];
  is faster than explicit tests
 		if (x < 0)  x := 0;
 		else if (x > MAXJSAMPLE)  x := MAXJSAMPLE;
  These processes all use a common table prepared by the routine below.

  For most steps we can mathematically guarantee that the initial value
  of x is within MAXJSAMPLE+1 of the legal range, so a table running from
  -(MAXJSAMPLE+1) to 2*MAXJSAMPLE+1 is sufficient.  But for the initial
  limiting step (just after the IDCT), a wildly out-of-range value is
  possible if the input data is corrupt.  To avoid any chance of indexing
  off the end of memory and getting a bad-pointer trap, we perform the
  post-IDCT limiting thus:
 		x := range_limit[x & MASK];
  where MASK is 2 bits wider than legal sample data, ie 10 bits for 8-bit
  samples.  Under normal circumstances this is more than enough range and
  a correct output will be generated; with bogus input data the mask will
  cause wraparound, and we will safely generate a bogus-but-in-range output.
  For the post-IDCT step, we want to convert the data from signed to unsigned
  representation by adding CENTERJSAMPLE at the same time that we limit it.
  So the post-IDCT limiting table ends up looking like this:
    CENTERJSAMPLE,CENTERJSAMPLE+1,...,MAXJSAMPLE,
    MAXJSAMPLE (repeat 2*(MAXJSAMPLE+1)-CENTERJSAMPLE times),
    0          (repeat 2*(MAXJSAMPLE+1)-CENTERJSAMPLE times),
    0,1,...,CENTERJSAMPLE-1
  Negative inputs select values from the upper half of the table after
  masking.

  We can save some space by overlapping the start of the post-IDCT table
  with the simpler range limiting table.  The post-IDCT table begins at
  sample_range_limit + CENTERJSAMPLE.

  Note that the table is allocated in near data space on PCs; it's small
  enough and used often enough to justify this. }

{LOCAL}
procedure prepare_range_limit_table (cinfo : j_decompress_ptr);
{ Allocate and fill in the sample_range_limit table }
var
  table : range_limit_table_ptr;
  idct_table : JSAMPROW;
  i : int;
begin
  table := range_limit_table_ptr (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
		(5 * (MAXJSAMPLE+1) + CENTERJSAMPLE) * SIZEOF(JSAMPLE)) );

  { First segment of "simple" table: limit[x] := 0 for x < 0 }
  MEMZERO(table, (MAXJSAMPLE+1) * SIZEOF(JSAMPLE));

  cinfo^.sample_range_limit := (table);
  { allow negative subscripts of simple table }
  { is noop, handled via type definition (Nomssi) }
  { Main part of "simple" table: limit[x] := x }
  for i := 0 to MAXJSAMPLE do
    table^[i] := JSAMPLE (i);
  idct_table := JSAMPROW(@ table^[CENTERJSAMPLE]);
                        { Point to where post-IDCT table starts }
  { End of simple table, rest of first half of post-IDCT table }
  for i := CENTERJSAMPLE to pred(2*(MAXJSAMPLE+1)) do
    idct_table^[i] := MAXJSAMPLE;
  { Second half of post-IDCT table }
  MEMZERO(@(idct_table^[2 * (MAXJSAMPLE+1)]),
	  (2 * (MAXJSAMPLE+1) - CENTERJSAMPLE) * SIZEOF(JSAMPLE));
  MEMCOPY(@(idct_table^[(4 * (MAXJSAMPLE+1) - CENTERJSAMPLE)]),
	  @cinfo^.sample_range_limit^[0], CENTERJSAMPLE * SIZEOF(JSAMPLE));

end;


{ Master selection of decompression modules.
  This is done once at jpeg_start_decompress time.  We determine
  which modules will be used and give them appropriate initialization calls.
  We also initialize the decompressor input side to begin consuming data.

  Since jpeg_read_header has finished, we know what is in the SOF
  and (first) SOS markers.  We also have all the application parameter
  settings. }

{LOCAL}
procedure master_selection (cinfo : j_decompress_ptr);
var
  master : my_master_ptr;
  use_c_buffer : boolean;
  samplesperrow : long;
  jd_samplesperrow : JDIMENSION;
var
  nscans : int;
begin
  master := my_master_ptr (cinfo^.master);

  { Initialize dimensions and other stuff }
  jpeg_calc_output_dimensions(cinfo);
  prepare_range_limit_table(cinfo);

  { Width of an output scanline must be representable as JDIMENSION. }
  samplesperrow := long(cinfo^.output_width) * long (cinfo^.out_color_components);
  jd_samplesperrow := JDIMENSION (samplesperrow);
  if (long(jd_samplesperrow) <> samplesperrow) then
    ERREXIT(j_common_ptr(cinfo), JERR_WIDTH_OVERFLOW);

  { Initialize my private state }
  master^.pass_number := 0;
  master^.using_merged_upsample := use_merged_upsample(cinfo);

  { Color quantizer selection }
  master^.quantizer_1pass := NIL;
  master^.quantizer_2pass := NIL;
  { No mode changes if not using buffered-image mode. }
  if (not cinfo^.quantize_colors) or (not cinfo^.buffered_image) then
  begin
    cinfo^.enable_1pass_quant := FALSE;
    cinfo^.enable_external_quant := FALSE;
    cinfo^.enable_2pass_quant := FALSE;
  end;
  if (cinfo^.quantize_colors) then
  begin
    if (cinfo^.raw_data_out) then
      ERREXIT(j_common_ptr(cinfo), JERR_NOTIMPL);
    { 2-pass quantizer only works in 3-component color space. }
    if (cinfo^.out_color_components <> 3) then
    begin
      cinfo^.enable_1pass_quant := TRUE;
      cinfo^.enable_external_quant := FALSE;
      cinfo^.enable_2pass_quant := FALSE;
      cinfo^.colormap := NIL;
    end
    else
      if (cinfo^.colormap <> NIL) then
      begin
        cinfo^.enable_external_quant := TRUE;
      end
      else
        if (cinfo^.two_pass_quantize) then
        begin
          cinfo^.enable_2pass_quant := TRUE;
        end
        else
        begin
          cinfo^.enable_1pass_quant := TRUE;
        end;

    if (cinfo^.enable_1pass_quant) then
    begin
{$ifdef QUANT_1PASS_SUPPORTED}
      jinit_1pass_quantizer(cinfo);
      master^.quantizer_1pass := cinfo^.cquantize;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end;

    { We use the 2-pass code to map to external colormaps. }
    if (cinfo^.enable_2pass_quant) or (cinfo^.enable_external_quant) then
    begin
{$ifdef QUANT_2PASS_SUPPORTED}
      jinit_2pass_quantizer(cinfo);
      master^.quantizer_2pass := cinfo^.cquantize;
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end;
    { If both quantizers are initialized, the 2-pass one is left active;
      this is necessary for starting with quantization to an external map. }
  end;

  { Post-processing: in particular, color conversion first }
  if (not cinfo^.raw_data_out) then
  begin
    if (master^.using_merged_upsample) then
    begin
{$ifdef UPSAMPLE_MERGING_SUPPORTED}
      jinit_merged_upsampler(cinfo); { does color conversion too }
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
    begin
      jinit_color_deconverter(cinfo);
      jinit_upsampler(cinfo);
    end;
    jinit_d_post_controller(cinfo, cinfo^.enable_2pass_quant);
  end;
  { Inverse DCT }
  jinit_inverse_dct(cinfo);
  { Entropy decoding: either Huffman or arithmetic coding. }
  if (cinfo^.arith_code) then
  begin
    ERREXIT(j_common_ptr(cinfo), JERR_ARITH_NOTIMPL);
  end
  else
  begin
    if (cinfo^.progressive_mode) then
    begin
{$ifdef D_PROGRESSIVE_SUPPORTED}
      jinit_phuff_decoder(cinfo);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif}
    end
    else
      jinit_huff_decoder(cinfo);
  end;

  { Initialize principal buffer controllers. }
  use_c_buffer := cinfo^.inputctl^.has_multiple_scans or cinfo^.buffered_image;
  jinit_d_coef_controller(cinfo, use_c_buffer);

  if (not cinfo^.raw_data_out) then
    jinit_d_main_controller(cinfo, FALSE { never need full buffer here });

  { We can now tell the memory manager to allocate virtual arrays. }
  cinfo^.mem^.realize_virt_arrays (j_common_ptr(cinfo));

  { Initialize input side of decompressor to consume first scan. }
  cinfo^.inputctl^.start_input_pass (cinfo);

{$ifdef D_MULTISCAN_FILES_SUPPORTED}
  { If jpeg_start_decompress will read the whole file, initialize
    progress monitoring appropriately.  The input step is counted
    as one pass. }

  if (cinfo^.progress <> NIL) and (not cinfo^.buffered_image) and
     (cinfo^.inputctl^.has_multiple_scans) then
  begin

    { Estimate number of scans to set pass_limit. }
    if (cinfo^.progressive_mode) then
    begin
      { Arbitrarily estimate 2 interleaved DC scans + 3 AC scans/component. }
      nscans := 2 + 3 * cinfo^.num_components;
    end
    else
    begin
      { For a nonprogressive multiscan file, estimate 1 scan per component. }
      nscans := cinfo^.num_components;
    end;
    cinfo^.progress^.pass_counter := Long(0);
    cinfo^.progress^.pass_limit := long (cinfo^.total_iMCU_rows) * nscans;
    cinfo^.progress^.completed_passes := 0;
    if cinfo^.enable_2pass_quant then
      cinfo^.progress^.total_passes := 3
    else
      cinfo^.progress^.total_passes := 2;
    { Count the input pass as done }
    Inc(master^.pass_number);
  end;
{$endif} { D_MULTISCAN_FILES_SUPPORTED }
end;


{ Per-pass setup.
  This is called at the beginning of each output pass.  We determine which
  modules will be active during this pass and give them appropriate
  start_pass calls.  We also set is_dummy_pass to indicate whether this
  is a "real" output pass or a dummy pass for color quantization.
  (In the latter case, jdapistd.c will crank the pass to completion.) }

{METHODDEF}
procedure prepare_for_output_pass (cinfo : j_decompress_ptr);  
var
  master : my_master_ptr;
begin
  master := my_master_ptr (cinfo^.master);

  if (master^.pub.is_dummy_pass) then
  begin
{$ifdef QUANT_2PASS_SUPPORTED}
    { Final pass of 2-pass quantization }
    master^.pub.is_dummy_pass := FALSE;
    cinfo^.cquantize^.start_pass (cinfo, FALSE);
    cinfo^.post^.start_pass (cinfo, JBUF_CRANK_DEST);
    cinfo^.main^.start_pass (cinfo, JBUF_CRANK_DEST);
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_NOT_COMPILED);
{$endif} { QUANT_2PASS_SUPPORTED }
  end
  else
  begin
    if (cinfo^.quantize_colors) and (cinfo^.colormap = NIL) then
    begin
      { Select new quantization method }
      if (cinfo^.two_pass_quantize) and (cinfo^.enable_2pass_quant) then
      begin
	cinfo^.cquantize := master^.quantizer_2pass;
	master^.pub.is_dummy_pass := TRUE;
      end
      else
        if (cinfo^.enable_1pass_quant) then
        begin
	  cinfo^.cquantize := master^.quantizer_1pass;
        end
        else
        begin
	  ERREXIT(j_common_ptr(cinfo), JERR_MODE_CHANGE);
        end;
    end;
    cinfo^.idct^.start_pass (cinfo);
    cinfo^.coef^.start_output_pass (cinfo);
    if (not cinfo^.raw_data_out) then
    begin
      if (not master^.using_merged_upsample) then
	cinfo^.cconvert^.start_pass (cinfo);
      cinfo^.upsample^.start_pass (cinfo);
      if (cinfo^.quantize_colors) then
	cinfo^.cquantize^.start_pass (cinfo, master^.pub.is_dummy_pass);
      if master^.pub.is_dummy_pass  then
        cinfo^.post^.start_pass (cinfo, JBUF_SAVE_AND_PASS)
      else
        cinfo^.post^.start_pass (cinfo, JBUF_PASS_THRU);
      cinfo^.main^.start_pass (cinfo, JBUF_PASS_THRU);
    end;
  end;

  { Set up progress monitor's pass info if present }
  if (cinfo^.progress <> NIL) then
  begin
    cinfo^.progress^.completed_passes := master^.pass_number;
    if master^.pub.is_dummy_pass then
      cinfo^.progress^.total_passes := master^.pass_number + 2
    else
      cinfo^.progress^.total_passes := master^.pass_number + 1;
    { In buffered-image mode, we assume one more output pass if EOI not
      yet reached, but no more passes if EOI has been reached. }

    if (cinfo^.buffered_image) and (not cinfo^.inputctl^.eoi_reached) then
    begin
      if cinfo^.enable_2pass_quant then
        Inc(cinfo^.progress^.total_passes, 2)
      else
        Inc(cinfo^.progress^.total_passes, 1);
    end;
  end;
end;


{ Finish up at end of an output pass. }

{METHODDEF}
procedure finish_output_pass (cinfo : j_decompress_ptr);  
var
  master : my_master_ptr;
begin
  master := my_master_ptr (cinfo^.master);

  if (cinfo^.quantize_colors) then
    cinfo^.cquantize^.finish_pass (cinfo);
  Inc(master^.pass_number);
end;


{$ifdef D_MULTISCAN_FILES_SUPPORTED}

{ Switch to a new external colormap between output passes. }

{GLOBAL}
procedure jpeg_new_colormap (cinfo : j_decompress_ptr);
var
  master : my_master_ptr;
begin
  master := my_master_ptr (cinfo^.master);

  { Prevent application from calling me at wrong times }
  if (cinfo^.global_state <> DSTATE_BUFIMAGE) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  if (cinfo^.quantize_colors) and (cinfo^.enable_external_quant) and
     (cinfo^.colormap <> NIL) then
  begin
    { Select 2-pass quantizer for external colormap use }
    cinfo^.cquantize := master^.quantizer_2pass;
    { Notify quantizer of colormap change }
    cinfo^.cquantize^.new_color_map (cinfo);
    master^.pub.is_dummy_pass := FALSE; { just in case }
  end
  else
    ERREXIT(j_common_ptr(cinfo), JERR_MODE_CHANGE);
end;

{$endif} { D_MULTISCAN_FILES_SUPPORTED }


{ Initialize master decompression control and select active modules.
  This is performed at the start of jpeg_start_decompress. }

{GLOBAL}
procedure jinit_master_decompress (cinfo : j_decompress_ptr);
var
  master : my_master_ptr;
begin
  master := my_master_ptr (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				  SIZEOF(my_decomp_master)) );
  cinfo^.master := jpeg_decomp_master_ptr(master);
  master^.pub.prepare_for_output_pass := prepare_for_output_pass;
  master^.pub.finish_output_pass := finish_output_pass;

  master^.pub.is_dummy_pass := FALSE;

  master_selection(cinfo);
end;

end.
