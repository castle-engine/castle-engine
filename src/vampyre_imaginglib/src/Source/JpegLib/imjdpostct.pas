unit imjdpostct;

{ Original: jdpostct.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

{ This file contains the decompression postprocessing controller.
  This controller manages the upsampling, color conversion, and color
  quantization/reduction steps; specifically, it controls the buffering
  between upsample/color conversion and color quantization/reduction.

  If no color quantization/reduction is required, then this module has no
  work to do, and it just hands off to the upsample/color conversion code.
  An integrated upsample/convert/quantize process would replace this module
  entirely. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
  imjdeferr,
  imjerror,
  imjutils,
  imjpeglib;

{ Initialize postprocessing controller. }

{GLOBAL}
procedure jinit_d_post_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
implementation


{ Private buffer controller object }

type
  my_post_ptr = ^my_post_controller;
  my_post_controller = record
    pub : jpeg_d_post_controller; { public fields }

    { Color quantization source buffer: this holds output data from
      the upsample/color conversion step to be passed to the quantizer.
      For two-pass color quantization, we need a full-image buffer;
      for one-pass operation, a strip buffer is sufficient. }

    whole_image : jvirt_sarray_ptr;   { virtual array, or NIL if one-pass }
    buffer : JSAMPARRAY;		{ strip buffer, or current strip of virtual }
    strip_height : JDIMENSION;	{ buffer size in rows }
    { for two-pass mode only: }
    starting_row : JDIMENSION;	{ row # of first row in current strip }
    next_row : JDIMENSION;		{ index of next row to fill/empty in strip }
  end;

{ Forward declarations }
{METHODDEF}
procedure post_process_1pass(cinfo : j_decompress_ptr;
		             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
		             in_row_groups_avail : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
		             out_rows_avail : JDIMENSION); forward;
{$ifdef QUANT_2PASS_SUPPORTED}
{METHODDEF}
procedure post_process_prepass(cinfo : j_decompress_ptr;
		               input_buf : JSAMPIMAGE;
                               var in_row_group_ctr : JDIMENSION;
		               in_row_groups_avail : JDIMENSION;
		               output_buf : JSAMPARRAY;
                               var out_row_ctr : JDIMENSION;
		               out_rows_avail : JDIMENSION);  forward;
{METHODDEF}
procedure post_process_2pass(cinfo : j_decompress_ptr;
 		             input_buf : JSAMPIMAGE;
                             var in_row_group_ctr : JDIMENSION;
		             in_row_groups_avail : JDIMENSION;
		             output_buf : JSAMPARRAY;
                             var out_row_ctr : JDIMENSION;
		             out_rows_avail : JDIMENSION);  forward;
{$endif}


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_dpost (cinfo : j_decompress_ptr;
                            pass_mode : J_BUF_MODE);  
var
  post : my_post_ptr;
begin
  post := my_post_ptr(cinfo^.post);

  case (pass_mode) of
  JBUF_PASS_THRU:
    if (cinfo^.quantize_colors) then
    begin
      { Single-pass processing with color quantization. }
      post^.pub.post_process_data := post_process_1pass;
      { We could be doing buffered-image output before starting a 2-pass
        color quantization; in that case, jinit_d_post_controller did not
        allocate a strip buffer.  Use the virtual-array buffer as workspace. }
      if (post^.buffer = NIL) then
      begin
	post^.buffer := cinfo^.mem^.access_virt_sarray
	  (j_common_ptr(cinfo), post^.whole_image,
	   JDIMENSION(0), post^.strip_height, TRUE);
      end;
    end
    else
    begin
      { For single-pass processing without color quantization,
        I have no work to do; just call the upsampler directly. }

      post^.pub.post_process_data := cinfo^.upsample^.upsample;
    end;

{$ifdef QUANT_2PASS_SUPPORTED}
  JBUF_SAVE_AND_PASS:
    begin
      { First pass of 2-pass quantization }
      if (post^.whole_image = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      post^.pub.post_process_data := post_process_prepass;
    end;
  JBUF_CRANK_DEST:
    begin
      { Second pass of 2-pass quantization }
      if (post^.whole_image = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      post^.pub.post_process_data := post_process_2pass;
    end;
{$endif} { QUANT_2PASS_SUPPORTED }
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
  post^.next_row := 0;
  post^.starting_row := 0;
end;


{ Process some data in the one-pass (strip buffer) case.
  This is used for color precision reduction as well as one-pass quantization. }

{METHODDEF}
procedure post_process_1pass (cinfo : j_decompress_ptr;
		              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
		              in_row_groups_avail : JDIMENSION;
		              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
		              out_rows_avail : JDIMENSION);
var
  post : my_post_ptr;
  num_rows, max_rows : JDIMENSION;
begin
  post := my_post_ptr (cinfo^.post);

  { Fill the buffer, but not more than what we can dump out in one go. }
  { Note we rely on the upsampler to detect bottom of image. }
  max_rows := out_rows_avail - out_row_ctr;
  if (max_rows > post^.strip_height) then
    max_rows := post^.strip_height;
  num_rows := 0;
  cinfo^.upsample^.upsample (cinfo,
		             input_buf,
                             in_row_group_ctr,
                             in_row_groups_avail,
		             post^.buffer,
                             num_rows,  { var }
                             max_rows);
  { Quantize and emit data. }

  cinfo^.cquantize^.color_quantize (cinfo,
		post^.buffer,
                JSAMPARRAY(@ output_buf^[out_row_ctr]),
                int(num_rows));

  Inc(out_row_ctr, num_rows);
end;


{$ifdef QUANT_2PASS_SUPPORTED}

{ Process some data in the first pass of 2-pass quantization. }

{METHODDEF}
procedure post_process_prepass (cinfo : j_decompress_ptr;
                               	input_buf : JSAMPIMAGE;
                                var in_row_group_ctr : JDIMENSION;
		                in_row_groups_avail : JDIMENSION;
		                output_buf : JSAMPARRAY;
                                var out_row_ctr : JDIMENSION;
		                out_rows_avail:JDIMENSION);
var
  post : my_post_ptr;
  old_next_row, num_rows : JDIMENSION;
begin
  post := my_post_ptr(cinfo^.post);

  { Reposition virtual buffer if at start of strip. }
  if (post^.next_row = 0) then
  begin
    post^.buffer := cinfo^.mem^.access_virt_sarray
	(j_common_ptr(cinfo), post^.whole_image,
	 post^.starting_row, post^.strip_height, TRUE);
  end;

  { Upsample some data (up to a strip height's worth). }
  old_next_row := post^.next_row;
  cinfo^.upsample^.upsample (cinfo,
		input_buf, in_row_group_ctr, in_row_groups_avail,
		post^.buffer, post^.next_row, post^.strip_height);

  { Allow quantizer to scan new data.  No data is emitted, }
  { but we advance out_row_ctr so outer loop can tell when we're done. }
  if (post^.next_row > old_next_row) then
  begin
    num_rows := post^.next_row - old_next_row;


    cinfo^.cquantize^.color_quantize (cinfo,
                      JSAMPARRAY(@ post^.buffer^[old_next_row]),
			JSAMPARRAY(NIL),
                        int(num_rows));
    Inc(out_row_ctr, num_rows);
  end;

  { Advance if we filled the strip. }
  if (post^.next_row >= post^.strip_height) then
  begin
    Inc(post^.starting_row, post^.strip_height);
    post^.next_row := 0;
  end;
end;


{ Process some data in the second pass of 2-pass quantization. }

{METHODDEF}
procedure post_process_2pass (cinfo : j_decompress_ptr;
		              input_buf : JSAMPIMAGE;
                              var in_row_group_ctr : JDIMENSION;
		              in_row_groups_avail : JDIMENSION;
		              output_buf : JSAMPARRAY;
                              var out_row_ctr : JDIMENSION;
		              out_rows_avail : JDIMENSION);
var
  post : my_post_ptr;
  num_rows, max_rows : JDIMENSION;
begin
  post := my_post_ptr(cinfo^.post);

  { Reposition virtual buffer if at start of strip. }
  if (post^.next_row = 0) then
  begin
    post^.buffer := cinfo^.mem^.access_virt_sarray
	(j_common_ptr(cinfo), post^.whole_image,
	 post^.starting_row, post^.strip_height, FALSE);
  end;

  { Determine number of rows to emit. }
  num_rows := post^.strip_height - post^.next_row; { available in strip }
  max_rows := out_rows_avail - out_row_ctr; { available in output area }
  if (num_rows > max_rows) then
    num_rows := max_rows;
  { We have to check bottom of image here, can't depend on upsampler. }
  max_rows := cinfo^.output_height - post^.starting_row;
  if (num_rows > max_rows) then
    num_rows := max_rows;

  { Quantize and emit data. }
  cinfo^.cquantize^.color_quantize (cinfo,
                JSAMPARRAY(@ post^.buffer^[post^.next_row]),
                JSAMPARRAY(@ output_buf^[out_row_ctr]),
		int(num_rows));
  Inc(out_row_ctr, num_rows);

  { Advance if we filled the strip. }
  Inc(post^.next_row, num_rows);
  if (post^.next_row >= post^.strip_height) then
  begin
    Inc(post^.starting_row, post^.strip_height);
    post^.next_row := 0;
  end;
end;

{$endif} { QUANT_2PASS_SUPPORTED }


{ Initialize postprocessing controller. }

{GLOBAL}
procedure jinit_d_post_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  post : my_post_ptr;
begin
  post := my_post_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_post_controller)) );
  cinfo^.post := jpeg_d_post_controller_ptr (post);
  post^.pub.start_pass := start_pass_dpost;
  post^.whole_image := NIL;	{ flag for no virtual arrays }
  post^.buffer := NIL;		{ flag for no strip buffer }

  { Create the quantization buffer, if needed }
  if (cinfo^.quantize_colors) then
  begin
    { The buffer strip height is max_v_samp_factor, which is typically
      an efficient number of rows for upsampling to return.
      (In the presence of output rescaling, we might want to be smarter?) }

    post^.strip_height := JDIMENSION (cinfo^.max_v_samp_factor);
    if (need_full_buffer) then
    begin
      { Two-pass color quantization: need full-image storage. }
      { We round up the number of rows to a multiple of the strip height. }
{$ifdef QUANT_2PASS_SUPPORTED}
      post^.whole_image := cinfo^.mem^.request_virt_sarray
	(j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
	 LongInt(cinfo^.output_width) * cinfo^.out_color_components,
	 JDIMENSION (jround_up( long(cinfo^.output_height),
				long(post^.strip_height)) ),
	 post^.strip_height);
{$else}
      ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif} { QUANT_2PASS_SUPPORTED }
    end
    else
    begin
      { One-pass color quantization: just make a strip buffer. }
      post^.buffer := cinfo^.mem^.alloc_sarray
	(j_common_ptr (cinfo), JPOOL_IMAGE,
	 LongInt(cinfo^.output_width) * cinfo^.out_color_components,
	 post^.strip_height);
    end;
  end;
end;

end.
