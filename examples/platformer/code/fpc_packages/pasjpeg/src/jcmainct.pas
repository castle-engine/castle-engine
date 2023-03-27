Unit JcMainCt;

{ This file contains the main buffer controller for compression.
  The main buffer lies between the pre-processor and the JPEG
  compressor proper; it holds downsampled data in the JPEG colorspace. }

{ Original : jcmainct.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I jconfig.inc}

{ Note: currently, there is no operating mode in which a full-image buffer
  is needed at this step.  If there were, that mode could not be used with
  "raw data" input, since this module is bypassed in that case.  However,
  we've left the code here for possible use in special applications. }

{$undef FULL_MAIN_BUFFER_SUPPORTED}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
  jutils,
{$endif}
  jpeglib;

{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_c_main_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);

implementation


{ Private buffer controller object }

type
  my_main_ptr = ^my_main_controller;
  my_main_controller = record
    pub : jpeg_c_main_controller; { public fields }

    cur_iMCU_row : JDIMENSION;  { number of current iMCU row }
    rowgroup_ctr : JDIMENSION;  { counts row groups received in iMCU row }
    suspended : boolean;                { remember if we suspended output }
    pass_mode : J_BUF_MODE;             { current operating mode }

    { If using just a strip buffer, this points to the entire set of buffers
      (we allocate one for each component).  In the full-image case, this
      points to the currently accessible strips of the virtual arrays. }

    buffer : array[0..MAX_COMPONENTS-1] of JSAMPARRAY;

  {$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    { If using full-image storage, this array holds pointers to virtual-array
      control blocks for each component.  Unused if not full-image storage. }

    whole_image : array[0..MAX_COMPONENTS-1] of jvirt_sarray_ptr;
  {$endif}
  end; {my_main_controller}


{ Forward declarations }
{METHODDEF}
procedure process_data_simple_main(cinfo : j_compress_ptr;
                                   input_buf : JSAMPARRAY;
                                   var in_row_ctr: JDIMENSION;
                                   in_rows_avail : JDIMENSION); far; forward;

{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
{METHODDEF}
procedure process_data_buffer_main(cinfo : j_compress_ptr;
                                   input_buf : JSAMPARRAY;
                                   var in_row_ctr : JDIMENSION;
                                   in_rows_avail : JDIMENSION); far; forward;
{$endif}


{ Initialize for a processing pass. }

{METHODDEF}
procedure start_pass_main (cinfo : j_compress_ptr;
                           pass_mode : J_BUF_MODE); far;
var
  main : my_main_ptr;
begin
  main := my_main_ptr (cinfo^.main);

  { Do nothing in raw-data mode. }
  if (cinfo^.raw_data_in) then
    exit;

  main^.cur_iMCU_row := 0;      { initialize counters }
  main^.rowgroup_ctr := 0;
  main^.suspended := FALSE;
  main^.pass_mode := pass_mode; { save mode for use by process_data }

  case (pass_mode) of
  JBUF_PASS_THRU:
    begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
      if (main^.whole_image[0] <> NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
      main^.pub.process_data := process_data_simple_main;
    end;
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
  JBUF_SAVE_SOURCE,
  JBUF_CRANK_DEST,
  JBUF_SAVE_AND_PASS:
    begin
      if (main^.whole_image[0] = NIL) then
        ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
      main^.pub.process_data := process_data_buffer_main;
    end;
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
  end;
end;


{ Process some data.
  This routine handles the simple pass-through mode,
  where we have only a strip buffer. }

{METHODDEF}
procedure process_data_simple_main (cinfo : j_compress_ptr;
                                    input_buf : JSAMPARRAY;
                                    var in_row_ctr : JDIMENSION;
                                    in_rows_avail : JDIMENSION);
var
  main : my_main_ptr;
begin
  main := my_main_ptr (cinfo^.main);

  while (main^.cur_iMCU_row < cinfo^.total_iMCU_rows) do
  begin
    { Read input data if we haven't filled the main buffer yet }
    if (main^.rowgroup_ctr < DCTSIZE) then
      cinfo^.prep^.pre_process_data (cinfo,
                                     input_buf,
                                     in_row_ctr,
                                     in_rows_avail,
                                     JSAMPIMAGE(@main^.buffer),
                                     main^.rowgroup_ctr,
                                     JDIMENSION(DCTSIZE));

    { If we don't have a full iMCU row buffered, return to application for
      more data.  Note that preprocessor will always pad to fill the iMCU row
      at the bottom of the image. }
    if (main^.rowgroup_ctr <> DCTSIZE) then
      exit;

    { Send the completed row to the compressor }
    if (not cinfo^.coef^.compress_data (cinfo, JSAMPIMAGE(@main^.buffer))) then
    begin
      { If compressor did not consume the whole row, then we must need to
        suspend processing and return to the application.  In this situation
        we pretend we didn't yet consume the last input row; otherwise, if
        it happened to be the last row of the image, the application would
        think we were done. }

      if (not main^.suspended) then
      begin
        Dec(in_row_ctr);
        main^.suspended := TRUE;
      end;
      exit;
    end;
    { We did finish the row.  Undo our little suspension hack if a previous
      call suspended; then mark the main buffer empty. }

    if (main^.suspended) then
    begin
      Inc(in_row_ctr);
      main^.suspended := FALSE;
    end;
    main^.rowgroup_ctr := 0;
    Inc(main^.cur_iMCU_row);
  end;
end;


{$ifdef FULL_MAIN_BUFFER_SUPPORTED}

{ Process some data.
  This routine handles all of the modes that use a full-size buffer. }

{METHODDEF}
procedure process_data_buffer_main (cinfo : j_compress_ptr;
                                    input_buf : JSAMPARRAY;
                                    var in_row_ctr : JDIMENSION;
                                    in_rows_avail : JDIMENSION);
var
  main : my_main_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
  writing : boolean;
begin
  main := my_main_ptr (cinfo^.main);
  writing := (main^.pass_mode <> JBUF_CRANK_DEST);

  while (main^.cur_iMCU_row < cinfo^.total_iMCU_rows) do
  begin
    { Realign the virtual buffers if at the start of an iMCU row. }
    if (main^.rowgroup_ctr = 0) then
    begin
      compptr := cinfo^.comp_info;
      for ci := 0 to pred(cinfo^.num_components) do
      begin
        main^.buffer[ci] := cinfo^.mem^.access_virt_sarray
          (j_common_ptr (cinfo), main^.whole_image[ci],
           main^.cur_iMCU_row * (compptr^.v_samp_factor * DCTSIZE),
           JDIMENSION (compptr^.v_samp_factor * DCTSIZE), writing);
        Inc(compptr);
      end;
      { In a read pass, pretend we just read some source data. }
      if (not writing) then
      begin
        Inc(in_row_ctr, cinfo^.max_v_samp_factor * DCTSIZE);
        main^.rowgroup_ctr := DCTSIZE;
      end;
    end;

    { If a write pass, read input data until the current iMCU row is full. }
    { Note: preprocessor will pad if necessary to fill the last iMCU row. }
    if (writing) then
    begin
      cinfo^.prep^.pre_process_data (cinfo,
                                     input_buf, in_row_ctr, in_rows_avail,
                                     JSAMPIMAGE(@main^.buffer),
                                     main^.rowgroup_ctr,
                                     JDIMENSION (DCTSIZE));

      { Return to application if we need more data to fill the iMCU row. }
      if (main^.rowgroup_ctr < DCTSIZE) then
        exit;
    end;

    { Emit data, unless this is a sink-only pass. }
    if (main^.pass_mode <> JBUF_SAVE_SOURCE) then
    begin
      if (not cinfo^.coef^.compress_data (cinfo,
                                          JSAMPIMAGE(@main^.buffer))) then
      begin
        { If compressor did not consume the whole row, then we must need to
          suspend processing and return to the application.  In this situation
          we pretend we didn't yet consume the last input row; otherwise, if
          it happened to be the last row of the image, the application would
          think we were done. }

        if (not main^.suspended) then
        begin
          Dec(in_row_ctr);
          main^.suspended := TRUE;
        end;
        exit;
      end;
      { We did finish the row.  Undo our little suspension hack if a previous
        call suspended; then mark the main buffer empty. }

      if (main^.suspended) then
      begin
        Inc(in_row_ctr);
        main^.suspended := FALSE;
      end;
    end;

    { If get here, we are done with this iMCU row.  Mark buffer empty. }
    main^.rowgroup_ctr := 0;
    Inc(main^.cur_iMCU_row);
  end;
end;

{$endif} { FULL_MAIN_BUFFER_SUPPORTED }


{ Initialize main buffer controller. }

{GLOBAL}
procedure jinit_c_main_controller (cinfo : j_compress_ptr;
                                   need_full_buffer : boolean);
var
  main : my_main_ptr;
  ci : int;
  compptr : jpeg_component_info_ptr;
begin
  main := my_main_ptr(
    cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                SIZEOF(my_main_controller)) );
  cinfo^.main := jpeg_c_main_controller_ptr(main);
  main^.pub.start_pass := start_pass_main;

  { We don't need to create a buffer in raw-data mode. }
  if (cinfo^.raw_data_in) then
    exit;

  { Create the buffer.  It holds downsampled data, so each component
    may be of a different size. }

  if (need_full_buffer) then
  begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    { Allocate a full-image virtual array for each component }
    { Note we pad the bottom to a multiple of the iMCU height }
    compptr := cinfo^.comp_info;
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      main^.whole_image[ci] := cinfo^.mem^.request_virt_sarray
        (j_common_ptr(cinfo), JPOOL_IMAGE, FALSE,
         compptr^.width_in_blocks * DCTSIZE,
         JDIMENSION (jround_up( long (compptr^.height_in_blocks),
                                long (compptr^.v_samp_factor)) * DCTSIZE),
         JDIMENSION (compptr^.v_samp_factor * DCTSIZE));
      Inc(compptr);
    end;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_BAD_BUFFER_MODE);
{$endif}
  end
  else
  begin
{$ifdef FULL_MAIN_BUFFER_SUPPORTED}
    main^.whole_image[0] := NIL; { flag for no virtual arrays }
{$endif}
    { Allocate a strip buffer for each component }
    compptr := jpeg_component_info_ptr(cinfo^.comp_info);
    for ci := 0 to pred(cinfo^.num_components) do
    begin
      main^.buffer[ci] := cinfo^.mem^.alloc_sarray
        (j_common_ptr(cinfo), JPOOL_IMAGE,
         compptr^.width_in_blocks * DCTSIZE,
         JDIMENSION (compptr^.v_samp_factor * DCTSIZE));
      Inc(compptr);
    end;
  end;
end;

end.
