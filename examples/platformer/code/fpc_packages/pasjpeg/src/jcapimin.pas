Unit JcAPImin;
{$N+}
{  This file contains application interface code for the compression half
  of the JPEG library.  These are the "minimum" API routines that may be
  needed in either the normal full-compression case or the transcoding-only
  case.

  Most of the routines intended to be called directly by an application
  are in this file or in jcapistd.c.  But also see jcparam.c for
  parameter-setup helper routines, jcomapi.c for routines shared by
  compression and decompression, and jctrans.c for the transcoding case. }

{ jcapimin.c ;  Copyright (C) 1994-1998, Thomas G. Lane. }


interface

{$I jconfig.inc}

uses
  jmorecfg,
  jinclude,
  jdeferr,
  jerror,
  jpeglib,
  jcomapi,
  jmemmgr,
  jcmarker;

{ Initialization of JPEG compression objects.
  Nomssi: This is a macro in the original code.

  jpeg_create_compress() and jpeg_create_decompress() are the exported
  names that applications should call.  These expand to calls on
  jpeg_CreateCompress and jpeg_CreateDecompress with additional information
  passed for version mismatch checking.
  NB: you must set up the error-manager BEFORE calling jpeg_create_xxx. }

procedure jpeg_create_compress(cinfo : j_compress_ptr);


{ Initialization of a JPEG compression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateCompress (cinfo : j_compress_ptr;
                               version : int;
                               structsize : size_t);

{ Destruction of a JPEG compression object }

{GLOBAL}
procedure jpeg_destroy_compress (cinfo : j_compress_ptr);


{ Abort processing of a JPEG compression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_compress (cinfo : j_compress_ptr);


{ Forcibly suppress or un-suppress all quantization and Huffman tables.
  Marks all currently defined tables as already written (if suppress)
  or not written (if !suppress).  This will control whether they get emitted
  by a subsequent jpeg_start_compress call.

  This routine is exported for use by applications that want to produce
  abbreviated JPEG datastreams.  It logically belongs in jcparam.c, but
  since it is called by jpeg_start_compress, we put it here --- otherwise
  jcparam.o would be linked whether the application used it or not. }

{GLOBAL}
procedure jpeg_suppress_tables (cinfo : j_compress_ptr;
                                suppress : boolean);


{ Finish JPEG compression.

  If a multipass operating mode was selected, this may do a great deal of
  work including most of the actual output. }

{GLOBAL}
procedure jpeg_finish_compress (cinfo : j_compress_ptr);

{ Write a special marker.
  This is only recommended for writing COM or APPn markers.
  Must be called after jpeg_start_compress() and before
  first call to jpeg_write_scanlines() or jpeg_write_raw_data(). }

{GLOBAL}
procedure jpeg_write_marker (cinfo : j_compress_ptr;
                             marker : int;
                             dataptr : JOCTETptr;
                             datalen : uInt);

{GLOBAL}
procedure jpeg_write_m_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint);
{GLOBAL}
procedure jpeg_write_m_byte (cinfo : j_compress_ptr; val : int);

{ Alternate compression function: just write an abbreviated table file.
  Before calling this, all parameters and a data destination must be set up.

  To produce a pair of files containing abbreviated tables and abbreviated
  image data, one would proceed as follows:

                initialize JPEG object
                set JPEG parameters
                set destination to table file
                jpeg_write_tables(cinfo);
                set destination to image file
                jpeg_start_compress(cinfo, FALSE);
                write data...
                jpeg_finish_compress(cinfo);

  jpeg_write_tables has the side effect of marking all tables written
  (same as jpeg_suppress_tables(..., TRUE)).  Thus a subsequent start_compress
  will not re-emit the tables unless it is passed write_all_tables=TRUE. }



{GLOBAL}
procedure jpeg_write_tables (cinfo : j_compress_ptr);

implementation

procedure jpeg_create_compress(cinfo : j_compress_ptr);
begin
  jpeg_CreateCompress(cinfo, JPEG_LIB_VERSION,
                      size_t(sizeof(jpeg_compress_struct)));
end;

{ Initialization of a JPEG compression object.
  The error manager must already be set up (in case memory manager fails). }

{GLOBAL}
procedure jpeg_CreateCompress (cinfo : j_compress_ptr;
                               version : int;
                               structsize : size_t);
var
  i : int;
var
  err : jpeg_error_mgr_ptr;
  client_data : voidp;
begin

  { Guard against version mismatches between library and caller. }
  cinfo^.mem := NIL;            { so jpeg_destroy knows mem mgr not called }
  if (version <> JPEG_LIB_VERSION) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_LIB_VERSION, JPEG_LIB_VERSION, version);
  if (structsize <> SIZEOF(jpeg_compress_struct)) then
    ERREXIT2(j_common_ptr(cinfo), JERR_BAD_STRUCT_SIZE,
             int(SIZEOF(jpeg_compress_struct)), int(structsize));

  { For debugging purposes, we zero the whole master structure.
    But the application has already set the err pointer, and may have set
    client_data, so we have to save and restore those fields.
    Note: if application hasn't set client_data, tools like Purify may
    complain here. }

  err := cinfo^.err;
  client_data := cinfo^.client_data; { ignore Purify complaint here }
  MEMZERO(cinfo, SIZEOF(jpeg_compress_struct));
  cinfo^.err := err;
  cinfo^.is_decompressor := FALSE;

  { Initialize a memory manager instance for this object }
  jinit_memory_mgr(j_common_ptr(cinfo));

  { Zero out pointers to permanent structures. }
  cinfo^.progress := NIL;
  cinfo^.dest := NIL;

  cinfo^.comp_info := NIL;

  for i := 0 to pred(NUM_QUANT_TBLS) do
    cinfo^.quant_tbl_ptrs[i] := NIL;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    cinfo^.dc_huff_tbl_ptrs[i] := NIL;
    cinfo^.ac_huff_tbl_ptrs[i] := NIL;
  end;

  cinfo^.script_space := NIL;

  cinfo^.input_gamma := 1.0;    { in case application forgets }

  { OK, I'm ready }
  cinfo^.global_state := CSTATE_START;
end;


{ Destruction of a JPEG compression object }

{GLOBAL}
procedure jpeg_destroy_compress (cinfo : j_compress_ptr);
begin
  jpeg_destroy(j_common_ptr(cinfo)); { use common routine }
end;


{ Abort processing of a JPEG compression operation,
  but don't destroy the object itself. }

{GLOBAL}
procedure jpeg_abort_compress (cinfo : j_compress_ptr);
begin
  jpeg_abort(j_common_ptr(cinfo)); { use common routine }
end;


{ Forcibly suppress or un-suppress all quantization and Huffman tables.
  Marks all currently defined tables as already written (if suppress)
  or not written (if !suppress).  This will control whether they get emitted
  by a subsequent jpeg_start_compress call.

  This routine is exported for use by applications that want to produce
  abbreviated JPEG datastreams.  It logically belongs in jcparam.c, but
  since it is called by jpeg_start_compress, we put it here --- otherwise
  jcparam.o would be linked whether the application used it or not. }

{GLOBAL}
procedure jpeg_suppress_tables (cinfo : j_compress_ptr;
                                suppress : boolean);
var
  i : int;
  qtbl : JQUANT_TBL_PTR;
  htbl : JHUFF_TBL_PTR;
begin
  for i := 0 to pred(NUM_QUANT_TBLS) do
  begin
    qtbl := cinfo^.quant_tbl_ptrs[i];
    if (qtbl <> NIL) then
      qtbl^.sent_table := suppress;
  end;

  for i := 0 to pred(NUM_HUFF_TBLS) do
  begin
    htbl := cinfo^.dc_huff_tbl_ptrs[i];
    if (htbl <> NIL) then
      htbl^.sent_table := suppress;
    htbl := cinfo^.ac_huff_tbl_ptrs[i];
    if (htbl <> NIL) then
      htbl^.sent_table := suppress;
  end;
end;


{ Finish JPEG compression.

  If a multipass operating mode was selected, this may do a great deal of
  work including most of the actual output. }

{GLOBAL}
procedure jpeg_finish_compress (cinfo : j_compress_ptr);
var
  iMCU_row : JDIMENSION;
begin
  if (cinfo^.global_state = CSTATE_SCANNING) or
     (cinfo^.global_state = CSTATE_RAW_OK) then
  begin
    { Terminate first pass }
    if (cinfo^.next_scanline < cinfo^.image_height) then
      ERREXIT(j_common_ptr(cinfo), JERR_TOO_LITTLE_DATA);
    cinfo^.master^.finish_pass (cinfo);
  end
  else
    if (cinfo^.global_state <> CSTATE_WRCOEFS) then
      ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);
  { Perform any remaining passes }
  while (not cinfo^.master^.is_last_pass) do
  begin
    cinfo^.master^.prepare_for_pass (cinfo);
    for iMCU_row := 0 to pred(cinfo^.total_iMCU_rows) do
    begin
      if (cinfo^.progress <> NIL) then
      begin
        cinfo^.progress^.pass_counter := long (iMCU_row);
        cinfo^.progress^.pass_limit := long (cinfo^.total_iMCU_rows);
        cinfo^.progress^.progress_monitor (j_common_ptr(cinfo));
      end;
      { We bypass the main controller and invoke coef controller directly;
        all work is being done from the coefficient buffer. }

      if (not cinfo^.coef^.compress_data (cinfo, JSAMPIMAGE(NIL))) then
        ERREXIT(j_common_ptr(cinfo), JERR_CANT_SUSPEND);
    end;
    cinfo^.master^.finish_pass (cinfo);
  end;
  { Write EOI, do final cleanup }
  cinfo^.marker^.write_file_trailer (cinfo);
  cinfo^.dest^.term_destination (cinfo);
  { We can use jpeg_abort to release memory and reset global_state }
  jpeg_abort(j_common_ptr(cinfo));
end;


{ Write a special marker.
  This is only recommended for writing COM or APPn markers.
  Must be called after jpeg_start_compress() and before
  first call to jpeg_write_scanlines() or jpeg_write_raw_data(). }

{GLOBAL}
procedure jpeg_write_marker (cinfo : j_compress_ptr;
                             marker : int;
                             dataptr : JOCTETptr;
                             datalen : uInt);
var
  write_marker_byte : procedure(info : j_compress_ptr; val : int);
begin
  if (cinfo^.next_scanline <> 0) or
     ((cinfo^.global_state <> CSTATE_SCANNING) and
      (cinfo^.global_state <> CSTATE_RAW_OK) and
      (cinfo^.global_state <> CSTATE_WRCOEFS)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  cinfo^.marker^.write_marker_header (cinfo, marker, datalen);
  write_marker_byte := cinfo^.marker^.write_marker_byte; { copy for speed }
  while (datalen <> 0) do
  begin
    Dec(datalen);
    write_marker_byte (cinfo, dataptr^);
    Inc(dataptr);
  end;
end;

{ Same, but piecemeal. }

{GLOBAL}
procedure jpeg_write_m_header (cinfo : j_compress_ptr;
                               marker : int;
                               datalen : uint);
begin
  if (cinfo^.next_scanline <> 0) or
     ((cinfo^.global_state <> CSTATE_SCANNING) and
      (cinfo^.global_state <> CSTATE_RAW_OK) and
      (cinfo^.global_state <> CSTATE_WRCOEFS)) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  cinfo^.marker^.write_marker_header (cinfo, marker, datalen);
end;

{GLOBAL}
procedure jpeg_write_m_byte (cinfo : j_compress_ptr; val : int);
begin
  cinfo^.marker^.write_marker_byte (cinfo, val);
end;


{ Alternate compression function: just write an abbreviated table file.
  Before calling this, all parameters and a data destination must be set up.

  To produce a pair of files containing abbreviated tables and abbreviated
  image data, one would proceed as follows:

                initialize JPEG object
                set JPEG parameters
                set destination to table file
                jpeg_write_tables(cinfo);
                set destination to image file
                jpeg_start_compress(cinfo, FALSE);
                write data...
                jpeg_finish_compress(cinfo);

  jpeg_write_tables has the side effect of marking all tables written
  (same as jpeg_suppress_tables(..., TRUE)).  Thus a subsequent start_compress
  will not re-emit the tables unless it is passed write_all_tables=TRUE. }

{GLOBAL}
procedure jpeg_write_tables (cinfo : j_compress_ptr);
begin
  if (cinfo^.global_state <> CSTATE_START) then
    ERREXIT1(j_common_ptr(cinfo), JERR_BAD_STATE, cinfo^.global_state);

  { (Re)initialize error mgr and destination modules }
  cinfo^.err^.reset_error_mgr (j_common_ptr(cinfo));
  cinfo^.dest^.init_destination (cinfo);
  { Initialize the marker writer ... bit of a crock to do it here. }
  jinit_marker_writer(cinfo);
  { Write them tables! }
  cinfo^.marker^.write_tables_only (cinfo);
  { And clean up. }
  cinfo^.dest^.term_destination (cinfo);

  { In library releases up through v6a, we called jpeg_abort() here to free
    any working memory allocated by the destination manager and marker
    writer.  Some applications had a problem with that: they allocated space
    of their own from the library memory manager, and didn't want it to go
    away during write_tables.  So now we do nothing.  This will cause a
    memory leak if an app calls write_tables repeatedly without doing a full
    compression cycle or otherwise resetting the JPEG object.  However, that
    seems less bad than unexpectedly freeing memory in the normal case.
    An app that prefers the old behavior can call jpeg_abort for itself after
    each call to jpeg_write_tables(). }
end;

end.
