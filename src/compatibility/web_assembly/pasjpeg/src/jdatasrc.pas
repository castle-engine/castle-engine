Unit JDataSrc;

{ This file contains decompression data source routines for the case of
  reading JPEG data from a file (or any stdio stream).  While these routines
  are sufficient for most applications, some will want to use a different
  source manager.
  IMPORTANT: we assume that fread() will correctly transcribe an array of
  JOCTETs from 8-bit-wide elements on external storage.  If char is wider
  than 8 bits on your machine, you may need to do some tweaking. }

{ jdatasrc.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I jconfig.inc}

{ this is not a core library module, so it doesn't define JPEG_INTERNALS }
uses
  jmorecfg,
  jinclude,
  jpeglib,
  jdmarker,
  jdeferr,
  jerror;


{ Prepare for input from a stdio stream.
  The caller must have already opened the stream, and is responsible
  for closing it after finishing decompression. }

{GLOBAL}
procedure jpeg_stdio_src (cinfo : j_decompress_ptr; infile : FILEptr);

implementation

{ Expanded data source object for stdio input }

type
  my_src_ptr = ^my_source_mgr;
  my_source_mgr = record
    pub : jpeg_source_mgr;      { public fields }

    infile : FILEPTR;           { source stream }
    buffer : JOCTET_FIELD_PTR;  { start of buffer }
    start_of_file : boolean;    { have we gotten any data yet? }
  end; {my_source_mgr;}

const
  INPUT_BUF_SIZE = 4096;        { choose an efficiently fread'able size }


{ Initialize source --- called by jpeg_read_header
  before any data is actually read. }

{METHODDEF}
procedure init_source (cinfo : j_decompress_ptr); far;
var
  src : my_src_ptr;
begin
  src := my_src_ptr (cinfo^.src);

  { We reset the empty-input-file flag for each image,
    but we don't clear the input buffer.
    This is correct behavior for reading a series of images from one source. }
  src^.start_of_file := TRUE;
end;


{ Fill the input buffer --- called whenever buffer is emptied.

  In typical applications, this should read fresh data into the buffer
  (ignoring the current state of next_input_byte & bytes_in_buffer),
  reset the pointer & count to the start of the buffer, and return TRUE
  indicating that the buffer has been reloaded.  It is not necessary to
  fill the buffer entirely, only to obtain at least one more byte.

  There is no such thing as an EOF return.  If the end of the file has been
  reached, the routine has a choice of ERREXIT() or inserting fake data into
  the buffer.  In most cases, generating a warning message and inserting a
  fake EOI marker is the best course of action --- this will allow the
  decompressor to output however much of the image is there.  However,
  the resulting error message is misleading if the real problem is an empty
  input file, so we handle that case specially.

  In applications that need to be able to suspend compression due to input
  not being available yet, a FALSE return indicates that no more data can be
  obtained right now, but more may be forthcoming later.  In this situation,
  the decompressor will return to its caller (with an indication of the
  number of scanlines it has read, if any).  The application should resume
  decompression after it has loaded more data into the input buffer.  Note
  that there are substantial restrictions on the use of suspension --- see
  the documentation.

  When suspending, the decompressor will back up to a convenient restart point
  (typically the start of the current MCU). next_input_byte & bytes_in_buffer
  indicate where the restart point will be if the current call returns FALSE.
  Data beyond this point must be rescanned after resumption, so move it to
  the front of the buffer rather than discarding it. }

{METHODDEF}
function fill_input_buffer (cinfo : j_decompress_ptr) : boolean; far;
var
  src : my_src_ptr;
  nbytes : size_t;
begin
  src := my_src_ptr(cinfo^.src);
  nbytes := JFREAD(src^.infile, src^.buffer, INPUT_BUF_SIZE);

  if (nbytes <= 0) then
  begin
    if (src^.start_of_file) then   { Treat empty input file as fatal error }
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);
    { Insert a fake EOI marker }
    src^.buffer^[0] := JOCTET ($FF);
    src^.buffer^[1] := JOCTET (JPEG_EOI);
    nbytes := 2;
  end;

  src^.pub.next_input_byte := JOCTETptr(src^.buffer);
  src^.pub.bytes_in_buffer := nbytes;
  src^.start_of_file := FALSE;

  fill_input_buffer := TRUE;
end;


{ Skip data --- used to skip over a potentially large amount of
  uninteresting data (such as an APPn marker).

  Writers of suspendable-input applications must note that skip_input_data
  is not granted the right to give a suspension return.  If the skip extends
  beyond the data currently in the buffer, the buffer can be marked empty so
  that the next read will cause a fill_input_buffer call that can suspend.
  Arranging for additional bytes to be discarded before reloading the input
  buffer is the application writer's problem. }

{METHODDEF}
procedure skip_input_data (cinfo : j_decompress_ptr;
                           num_bytes : long); far;
var
  src : my_src_ptr;
begin
  src := my_src_ptr (cinfo^.src);

  { Just a dumb implementation for now.  Could use fseek() except
    it doesn't work on pipes.  Not clear that being smart is worth
    any trouble anyway --- large skips are infrequent. }

  if (num_bytes > 0) then
  begin
    while (num_bytes > long(src^.pub.bytes_in_buffer)) do
    begin
      Dec(num_bytes, long(src^.pub.bytes_in_buffer));
      {void} fill_input_buffer(cinfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    end;
    Inc( src^.pub.next_input_byte, size_t(num_bytes) );
    Dec( src^.pub.bytes_in_buffer, size_t(num_bytes) );
  end;
end;


{ An additional method that can be provided by data source modules is the
  resync_to_restart method for error recovery in the presence of RST markers.
  For the moment, this source module just uses the default resync method
  provided by the JPEG library.  That method assumes that no backtracking
  is possible. }


{ Terminate source --- called by jpeg_finish_decompress
  after all data has been read.  Often a no-op.

  NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
  application must deal with any cleanup that should happen even
  for error exit. }

{METHODDEF}
procedure term_source (cinfo : j_decompress_ptr); far;
begin
  { no work necessary here }
end;


{ Prepare for input from a stdio stream.
  The caller must have already opened the stream, and is responsible
  for closing it after finishing decompression. }

{GLOBAL}
procedure jpeg_stdio_src (cinfo : j_decompress_ptr; infile : FILEptr);
var
  src : my_src_ptr;
begin
  { The source object and input buffer are made permanent so that a series
    of JPEG images can be read from the same file by calling jpeg_stdio_src
    only before the first one.  (If we discarded the buffer at the end of
    one image, we'd likely lose the start of the next one.)
    This makes it unsafe to use this manager and a different source
    manager serially with the same JPEG object.  Caveat programmer. }

  if (cinfo^.src = NIL) then
  begin { first time for this JPEG object? }
    cinfo^.src := jpeg_source_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(my_source_mgr)) );
    src := my_src_ptr (cinfo^.src);
    src^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  INPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  end;

  src := my_src_ptr (cinfo^.src);
  src^.pub.init_source := init_source;
  src^.pub.fill_input_buffer := fill_input_buffer;
  src^.pub.skip_input_data := skip_input_data;
  src^.pub.resync_to_restart := jpeg_resync_to_restart; { use default method }
  src^.pub.term_source := term_source;
  src^.infile := infile;
  src^.pub.bytes_in_buffer := 0; { forces fill_input_buffer on first read }
  src^.pub.next_input_byte := NIL; { until buffer loaded }
end;

end.
