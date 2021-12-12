unit imjdmainct;


{ This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains the main buffer controller for decompression.
  The main buffer lies between the JPEG decompressor proper and the
  post-processor; it holds downsampled data in the JPEG colorspace.

  Note that this code is bypassed in raw-data mode, since the application
  supplies the equivalent of the main buffer in that case. }

{ Original: jdmainct.c ; Copyright (C) 1994-1996, Thomas G. Lane.  }


{ In the current system design, the main buffer need never be a full-image
  buffer; any full-height buffers will be found inside the coefficient or
  postprocessing controllers.  Nonetheless, the main controller is not
  trivial.  Its responsibility is to provide context rows for upsampling/
  rescaling, and doing this in an efficient fashion is a bit tricky.

  Postprocessor input data is counted in "row groups".  A row group
  is defined to be (v_samp_factor * DCT_scaled_size / min_DCT_scaled_size)
  sample rows of each component.  (We require DCT_scaled_size values to be
  chosen such that these numbers are integers.  In practice DCT_scaled_size
  values will likely be powers of two, so we actually have the stronger
  condition that DCT_scaled_size / min_DCT_scaled_size is an integer.)
  Upsampling will typically produce max_v_samp_factor pixel rows from each
  row group (times any additional scale factor that the upsampler is
  applying).

  The coefficient controller will deliver data to us one iMCU row at a time;
  each iMCU row contains v_samp_factor * DCT_scaled_size sample rows, or
  exactly min_DCT_scaled_size row groups.  (This amount of data corresponds
  to one row of MCUs when the image is fully interleaved.)  Note that the
  number of sample rows varies across components, but the number of row
  groups does not.  Some garbage sample rows may be included in the last iMCU
  row at the bottom of the image.

  Depending on the vertical scaling algorithm used, the upsampler may need
  access to the sample row(s) above and below its current input row group.
  The upsampler is required to set need_context_rows TRUE at global
  selection
  time if so.  When need_context_rows is FALSE, this controller can simply
  obtain one iMCU row at a time from the coefficient controller and dole it
  out as row groups to the postprocessor.

  When need_context_rows is TRUE, this controller guarantees that the buffer
  passed to postprocessing contains at least one row group's worth of samples
  above and below the row group(s) being processed.  Note that the context
  rows "above" the first passed row group appear at negative row offsets in
  the passed buffer.  At the top and bottom of the image, the required
  context rows are manufactured by duplicating the first or last real sample
  row; this avoids having special cases in the upsampling inner loops.

  The amount of context is fixed at one row group just because that's a
  convenient number for this controller to work with.  The existing
  upsamplers really only need one sample row of context.  An upsampler
  supporting arbitrary output rescaling might wish for more than one row
  group of context when shrinking the image; tough, we don't handle that.
  (This is justified by the assumption that downsizing will be handled mostly
  by adjusting the DCT_scaled_size values, so that the actual scale factor at
  the upsample step needn't be much less than one.)

  To provide the desired context, we have to retain the last two row groups
  of one iMCU row while reading in the next iMCU row.  (The last row group
  can't be processed until we have another row group for its below-context,
  and so we have to save the next-to-last group too for its above-context.)
  We could do this most simply by copying data around in our buffer, but
  that'd be very slow.  We can avoid copying any data by creating a rather
  strange pointer structure.  Here's how it works.  We allocate a workspace
  consisting of M+2 row groups (where M = min_DCT_scaled_size is the number
  of row groups per iMCU row).  We create two sets of redundant pointers to
  the workspace.  Labeling the physical row groups 0 to M+1, the synthesized
  pointer lists look like this:
                    M+1                          M-1
  master pointer --> 0         master pointer --> 0
                     1                            1
                    ...                          ...
                    M-3                          M-3
                    M-2                           M
                    M-1                          M+1
                     M                           M-2
                    M+1                          M-1
                     0                            0
  We read alternate iMCU rows using each master pointer; thus the last two
  row groups of the previous iMCU row remain un-overwritten in the workspace.
  The pointer lists are set up so that the required context rows appear to
  be adjacent to the proper places when we pass the pointer lists to the
  upsampler.

  The above pictures describe the normal state of the pointer lists.
  At top and bottom of the image, we diddle the pointer lists to duplicate
  the first or last sample row as necessary (this is cheaper than copying
  sample rows around).

  This scheme breaks down if M < 2, ie, min_DCT_scaled_size is 1.  In that
  situation each iMCU row provides only one row group so the buffering logic
  must be different (eg, we must read two iMCU rows before we can emit the
  first row group).  For now, we simply do not support providing context
  rows when min_DCT_scaled_size is 1.  That combination seems unlikely to
  be worth providing --- if someone wants a 1/8th-size preview, they probably
  want it quick and dirty, so a context-free upsampler is sufficient. }

interface

{$I imjconfig.inc}

uses
  imjmorecfg,
  imjinclude,
{$ifdef QUANT_2PASS_SUPPORTED}
  imjquant2,
{$endif}
  imjdeferr,
  imjerror,
  imjpeglib;


{GLOBAL}
procedure jinit_d_main_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);


implementation

{ Private buffer controller object }

type
  my_main_ptr = ^my_main_controller;
  my_main_controller = record
    pub : jpeg_d_main_controller; { public fields }

    { Pointer to allocated workspace (M or M+2 row groups). }
    buffer : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

    buffer_full : boolean;	{ Have we gotten an iMCU row from decoder? }
    rowgroup_ctr : JDIMENSION ;	{ counts row groups output to postprocessor }

    { Remaining fields are only used in the context case. }

    { These are the master pointers to the funny-order pointer lists. }
    xbuffer : array[0..2-1] of JSAMPIMAGE;	{ pointers to weird pointer lists }

    whichptr : int;			{ indicates which pointer set is now in use }
    context_state : int;		{ process_data state machine status }
    rowgroups_avail : JDIMENSION;	{ row groups available to postprocessor }
    iMCU_row_ctr : JDIMENSION;	{ counts iMCU rows to detect image top/bot }
  end; { my_main_controller; }


{ context_state values: }
const
  CTX_PREPARE_FOR_IMCU  = 0;	{ need to prepare for MCU row }
  CTX_PROCESS_IMCU      = 1;	{ feeding iMCU to postprocessor }
  CTX_POSTPONED_ROW     = 2;	{ feeding postponed row group }


{ Forward declarations }
{METHODDEF}
procedure process_data_simple_main(cinfo : j_decompress_ptr;
                                   output_buf : JSAMPARRAY;
	                           var out_row_ctr : JDIMENSION;
                                   out_rows_avail : JDIMENSION); forward;
{METHODDEF}
procedure process_data_context_main (cinfo : j_decompress_ptr;
                                     output_buf : JSAMPARRAY;
	                             var out_row_ctr : JDIMENSION;
                                     out_rows_avail : JDIMENSION); forward;

{$ifdef QUANT_2PASS_SUPPORTED}
{METHODDEF}
procedure process_data_crank_post (cinfo : j_decompress_ptr;
                                     output_buf : JSAMPARRAY;
	                             var out_row_ctr : JDIMENSION;
                                     out_rows_avail : JDIMENSION); forward;
{$endif}


{LOCAL}
procedure alloc_funny_pointers (cinfo : j_decompress_ptr);
{ Allocate space for the funny pointer lists.
  This is done only once, not once per pass. }
var
  main : my_main_ptr;
  ci, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  xbuf : JSAMPARRAY;
begin
  main := my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  { Get top-level space for component array pointers.
    We alloc both arrays with one call to save a few cycles. }

  main^.xbuffer[0] := JSAMPIMAGE (
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                      cinfo^.num_components * 2 * SIZEOF(JSAMPARRAY)) );
  main^.xbuffer[1] := JSAMPIMAGE(@( main^.xbuffer[0]^[cinfo^.num_components] ));

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    { Get space for pointer lists --- M+4 row groups in each list.
      We alloc both pointer lists with one call to save a few cycles. }

    xbuf := JSAMPARRAY (
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				 2 * (rgroup * (M + 4)) * SIZEOF(JSAMPROW)) );
    Inc(JSAMPROW_PTR(xbuf), rgroup); { want one row group at negative offsets }
    main^.xbuffer[0]^[ci] := xbuf;
    Inc(JSAMPROW_PTR(xbuf), rgroup * (M + 4));
    main^.xbuffer[1]^[ci] := xbuf;
    Inc(compptr);
  end;
end;

{LOCAL}
procedure make_funny_pointers (cinfo : j_decompress_ptr);
{ Create the funny pointer lists discussed in the comments above.
  The actual workspace is already allocated (in main^.buffer),
  and the space for the pointer lists is allocated too.
  This routine just fills in the curiously ordered lists.
  This will be repeated at the beginning of each pass. }
var
  main : my_main_ptr;
  ci, i, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  buf, xbuf0, xbuf1 : JSAMPARRAY;
var
  help_xbuf0 : JSAMPARRAY;       { work around negative offsets }
begin
  main := my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    xbuf0 := main^.xbuffer[0]^[ci];
    xbuf1 := main^.xbuffer[1]^[ci];
    { First copy the workspace pointers as-is }
    buf := main^.buffer[ci];
    for i := 0 to pred(rgroup * (M + 2)) do
    begin
      xbuf0^[i] := buf^[i];
      xbuf1^[i] := buf^[i];
    end;
    { In the second list, put the last four row groups in swapped order }
    for i := 0 to pred(rgroup * 2) do
    begin
      xbuf1^[rgroup*(M-2) + i] := buf^[rgroup*M + i];
      xbuf1^[rgroup*M + i] := buf^[rgroup*(M-2) + i];
    end;
    { The wraparound pointers at top and bottom will be filled later
      (see set_wraparound_pointers, below).  Initially we want the "above"
      pointers to duplicate the first actual data line.  This only needs
      to happen in xbuffer[0]. }

    help_xbuf0 := xbuf0;
    Dec(JSAMPROW_PTR(help_xbuf0), rgroup);

    for i := 0 to pred(rgroup) do
    begin
      {xbuf0^[i - rgroup] := xbuf0^[0];}
      help_xbuf0^[i] := xbuf0^[0];
    end;
    Inc(compptr);
  end;
end;


{LOCAL}
procedure set_wraparound_pointers (cinfo : j_decompress_ptr);
{ Set up the "wraparound" pointers at top and bottom of the pointer lists.
  This changes the pointer list state from top-of-image to the normal state. }
var
  main : my_main_ptr;
  ci, i, rgroup : int;
  M : int;
  compptr : jpeg_component_info_ptr;
  xbuf0, xbuf1 : JSAMPARRAY;
var
  help_xbuf0,
  help_xbuf1 : JSAMPARRAY;       { work around negative offsets }
begin
  main := my_main_ptr (cinfo^.main);
  M := cinfo^.min_DCT_scaled_size;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    xbuf0 := main^.xbuffer[0]^[ci];
    xbuf1 := main^.xbuffer[1]^[ci];

    help_xbuf0 := xbuf0;
    Dec(JSAMPROW_PTR(help_xbuf0), rgroup);
    help_xbuf1 := xbuf1;
    Dec(JSAMPROW_PTR(help_xbuf1), rgroup);

    for i := 0 to pred(rgroup) do
    begin
      {xbuf0^[i - rgroup] := xbuf0^[rgroup*(M+1) + i];
      xbuf1^[i - rgroup] := xbuf1^[rgroup*(M+1) + i];}

      help_xbuf0^[i] := xbuf0^[rgroup*(M+1) + i];
      help_xbuf1^[i] := xbuf1^[rgroup*(M+1) + i];

      xbuf0^[rgroup*(M+2) + i] := xbuf0^[i];
      xbuf1^[rgroup*(M+2) + i] := xbuf1^[i];
    end;
    Inc(compptr);
  end;
end;


{LOCAL}
procedure set_bottom_pointers (cinfo : j_decompress_ptr);
{ Change the pointer lists to duplicate the last sample row at the bottom
  of the image.  whichptr indicates which xbuffer holds the final iMCU row.
  Also sets rowgroups_avail to indicate number of nondummy row groups in row. }
var
  main : my_main_ptr;
  ci, i, rgroup, iMCUheight, rows_left : int;
  compptr : jpeg_component_info_ptr;
  xbuf : JSAMPARRAY;
begin
  main := my_main_ptr (cinfo^.main);

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    { Count sample rows in one iMCU row and in one row group }
    iMCUheight := compptr^.v_samp_factor * compptr^.DCT_scaled_size;
    rgroup := iMCUheight div cinfo^.min_DCT_scaled_size;
    { Count nondummy sample rows remaining for this component }
    rows_left := int (compptr^.downsampled_height mod JDIMENSION (iMCUheight));
    if (rows_left = 0) then
      rows_left := iMCUheight;
    { Count nondummy row groups.  Should get same answer for each component,
      so we need only do it once. }
    if (ci = 0) then
    begin
      main^.rowgroups_avail := JDIMENSION ((rows_left-1) div rgroup + 1);
    end;
    { Duplicate the last real sample row rgroup*2 times; this pads out the
      last partial rowgroup and ensures at least one full rowgroup of context. }

    xbuf := main^.xbuffer[main^.whichptr]^[ci];
    for i := 0 to pred(rgroup * 2) do
    begin
      xbuf^[rows_left + i] := xbuf^[rows_left-1];
    end;
    Inc(compptr);
  end;
end;


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_main (cinfo : j_decompress_ptr;
                           pass_mode : J_BUF_MODE);  
var
  main : my_main_ptr;
begin
  main := my_main_ptr (cinfo^.main);

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
      if (cinfo^.upsample^.need_context_rows) then
      begin
        main^.pub.process_data := process_data_context_main;
        make_funny_pointers(cinfo); { Create the xbuffer[] lists }
        main^.whichptr := 0;	{ Read first iMCU row into xbuffer[0] }
        main^.context_state := CTX_PREPARE_FOR_IMCU;
        main^.iMCU_row_ctr := 0;
      end
      else
      begin
        { Simple case with no context needed }
        main^.pub.process_data := process_data_simple_main;
      end;
      main^.buffer_full := FALSE;	{ Mark buffer empty }
      main^.rowgroup_ctr := 0;
    end;
{$ifdef QUANT_2PASS_SUPPORTED}
  JBUF_CRANK_DEST:
    { For last pass of 2-pass quantization, just crank the postprocessor }
    main^.pub.process_data := process_data_crank_post;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data.
  This handles the simple case where no context is required. }

{METHODDEF}
procedure process_data_simple_main (cinfo : j_decompress_ptr;
			            output_buf : JSAMPARRAY;
                                    var out_row_ctr : JDIMENSION;
			            out_rows_avail : JDIMENSION);
var
  main : my_main_ptr;
  rowgroups_avail : JDIMENSION;
var
  main_buffer_ptr : JSAMPIMAGE;
begin
  main := my_main_ptr (cinfo^.main);
  main_buffer_ptr := JSAMPIMAGE(@(main^.buffer));

  { Read input data if we haven't filled the main buffer yet }
  if (not main^.buffer_full) then
  begin
    if (cinfo^.coef^.decompress_data (cinfo, main_buffer_ptr)=0) then
      exit;			{ suspension forced, can do nothing more }
    main^.buffer_full := TRUE;	{ OK, we have an iMCU row to work with }
  end;

  { There are always min_DCT_scaled_size row groups in an iMCU row. }
  rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size);
  { Note: at the bottom of the image, we may pass extra garbage row groups
    to the postprocessor.  The postprocessor has to check for bottom
    of image anyway (at row resolution), so no point in us doing it too. }

  { Feed the postprocessor }
  cinfo^.post^.post_process_data (cinfo, main_buffer_ptr,
                                  main^.rowgroup_ctr, rowgroups_avail,
				  output_buf, out_row_ctr, out_rows_avail);

  { Has postprocessor consumed all the data yet? If so, mark buffer empty }
  if (main^.rowgroup_ctr >= rowgroups_avail) then
  begin
    main^.buffer_full := FALSE;
    main^.rowgroup_ctr := 0;
  end;
end;


{ Process some data.
  This handles the case where context rows must be provided. }

{METHODDEF}
procedure process_data_context_main (cinfo : j_decompress_ptr;
			             output_buf : JSAMPARRAY;
                                     var out_row_ctr : JDIMENSION;
			             out_rows_avail : JDIMENSION);
var
  main : my_main_ptr;
begin
  main := my_main_ptr (cinfo^.main);

  { Read input data if we haven't filled the main buffer yet }
  if (not main^.buffer_full) then
  begin
    if (cinfo^.coef^.decompress_data (cinfo,
			  main^.xbuffer[main^.whichptr])=0) then
      exit;			{ suspension forced, can do nothing more }
    main^.buffer_full := TRUE;	{ OK, we have an iMCU row to work with }
    Inc(main^.iMCU_row_ctr);	{ count rows received }
  end;

  { Postprocessor typically will not swallow all the input data it is handed
    in one call (due to filling the output buffer first).  Must be prepared
    to exit and restart.  This switch lets us keep track of how far we got.
    Note that each case falls through to the next on successful completion. }

  case (main^.context_state) of
  CTX_POSTPONED_ROW:
    begin
      { Call postprocessor using previously set pointers for postponed row }
      cinfo^.post^.post_process_data (cinfo, main^.xbuffer[main^.whichptr],
			  main^.rowgroup_ctr, main^.rowgroups_avail,
			  output_buf, out_row_ctr, out_rows_avail);
      if (main^.rowgroup_ctr < main^.rowgroups_avail) then
        exit;			{ Need to suspend }
      main^.context_state := CTX_PREPARE_FOR_IMCU;
      if (out_row_ctr >= out_rows_avail) then
        exit;			{ Postprocessor exactly filled output buf }
    end;
  end;
  case (main^.context_state) of
  CTX_POSTPONED_ROW,
  CTX_PREPARE_FOR_IMCU:  {FALLTHROUGH}
    begin
      { Prepare to process first M-1 row groups of this iMCU row }
      main^.rowgroup_ctr := 0;
      main^.rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size - 1);
      { Check for bottom of image: if so, tweak pointers to "duplicate"
        the last sample row, and adjust rowgroups_avail to ignore padding rows. }

      if (main^.iMCU_row_ctr = cinfo^.total_iMCU_rows) then
        set_bottom_pointers(cinfo);
      main^.context_state := CTX_PROCESS_IMCU;

    end;
  end;
  case (main^.context_state) of
  CTX_POSTPONED_ROW,
  CTX_PREPARE_FOR_IMCU,  {FALLTHROUGH}
  CTX_PROCESS_IMCU:
    begin
      { Call postprocessor using previously set pointers }
      cinfo^.post^.post_process_data (cinfo, main^.xbuffer[main^.whichptr],
			  main^.rowgroup_ctr, main^.rowgroups_avail,
			  output_buf, out_row_ctr, out_rows_avail);
      if (main^.rowgroup_ctr < main^.rowgroups_avail) then
        exit;			{ Need to suspend }
      { After the first iMCU, change wraparound pointers to normal state }
      if (main^.iMCU_row_ctr = 1) then
        set_wraparound_pointers(cinfo);
      { Prepare to load new iMCU row using other xbuffer list }
      main^.whichptr := main^.whichptr xor 1;	{ 0=>1 or 1=>0 }
      main^.buffer_full := FALSE;
      { Still need to process last row group of this iMCU row, }
      { which is saved at index M+1 of the other xbuffer }
      main^.rowgroup_ctr := JDIMENSION (cinfo^.min_DCT_scaled_size + 1);
      main^.rowgroups_avail := JDIMENSION (cinfo^.min_DCT_scaled_size + 2);
      main^.context_state := CTX_POSTPONED_ROW;
    end;
  end;
end;


{ Process some data.
  Final pass of two-pass quantization: just call the postprocessor.
  Source data will be the postprocessor controller's internal buffer. }

{$ifdef QUANT_2PASS_SUPPORTED}

{METHODDEF}
procedure process_data_crank_post (cinfo : j_decompress_ptr;
			           output_buf : JSAMPARRAY;
                                   var out_row_ctr : JDIMENSION;
			           out_rows_avail : JDIMENSION);
var
  in_row_group_ctr : JDIMENSION;
begin
  in_row_group_ctr := 0;
  cinfo^.post^.post_process_data (cinfo, JSAMPIMAGE (NIL),
				     in_row_group_ctr,
                                     JDIMENSION(0),
				     output_buf,
                                     out_row_ctr,
                                     out_rows_avail);
end;

{$endif} { QUANT_2PASS_SUPPORTED }


{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_d_main_controller (cinfo : j_decompress_ptr;
                                   need_full_buffer : boolean);
var
  main : my_main_ptr;
  ci, rgroup, ngroups : int;
  compptr : jpeg_component_info_ptr;
begin
  main := my_main_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
				SIZEOF(my_main_controller)) );
  cinfo^.main := jpeg_d_main_controller_ptr(main);
  main^.pub.start_pass := start_pass_main;

  if (need_full_buffer)	then	{ shouldn't happen }
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);

  { Allocate the workspace.
    ngroups is the number of row groups we need.}

  if (cinfo^.upsample^.need_context_rows) then
  begin
    if (cinfo^.min_DCT_scaled_size < 2) then { unsupported, see comments above }
      ERREXIT(j_common_ptr(cinfo), JERR_NOTIMPL);
    alloc_funny_pointers(cinfo); { Alloc space for xbuffer[] lists }
    ngroups := cinfo^.min_DCT_scaled_size + 2;
  end
  else
  begin
    ngroups := cinfo^.min_DCT_scaled_size;
  end;

  compptr := jpeg_component_info_ptr(cinfo^.comp_info);
  for ci := 0 to pred(cinfo^.num_components) do
  begin
    rgroup := (compptr^.v_samp_factor * compptr^.DCT_scaled_size) div
      cinfo^.min_DCT_scaled_size; { height of a row group of component }
    main^.buffer[ci] := cinfo^.mem^.alloc_sarray
			(j_common_ptr(cinfo), JPOOL_IMAGE,
			 compptr^.width_in_blocks * uInt(compptr^.DCT_scaled_size),
			 JDIMENSION (rgroup * ngroups));
    Inc(compptr);
  end;
end;

end.
