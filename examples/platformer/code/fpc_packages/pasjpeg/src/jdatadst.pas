Unit JDataDst;

{ This file contains compression data destination routines for the case of
  emitting JPEG data to a file (or any stdio stream).  While these routines
  are sufficient for most applications, some will want to use a different
  destination manager.
  IMPORTANT: we assume that fwrite() will correctly transcribe an array of
  JOCTETs into 8-bit-wide elements on external storage.  If char is wider
  than 8 bits on your machine, you may need to do some tweaking. }

{ Original : jdatadst.c ; Copyright (C) 1994-1996, Thomas G. Lane. }

interface

{$I jconfig.inc}

{ this is not a core library module, so it doesn't define JPEG_INTERNALS }
uses
  jmorecfg,
  jpeglib,
  jinclude,
  jdeferr,
  jerror;

{ Prepare for output to a stdio stream.
  The caller must have already opened the stream, and is responsible
  for closing it after finishing compression. }

{GLOBAL}
procedure jpeg_stdio_dest (cinfo : j_compress_ptr; outfile : FILEptr);

implementation

{ Expanded data destination object for stdio output }

type
  my_dest_ptr = ^my_destination_mgr;
  my_destination_mgr = record
    pub : jpeg_destination_mgr; { public fields }

    outfile : FILEPTR;          { target stream }
    buffer : JOCTET_FIELD_PTR;  { start of buffer }
  end; {my_destination_mgr;}


const
  OUTPUT_BUF_SIZE = 4096;       { choose an efficiently fwrite'able size }


{ Initialize destination --- called by jpeg_start_compress
  before any data is actually written. }

{METHODDEF}
procedure init_destination (cinfo : j_compress_ptr); far;
var
  dest : my_dest_ptr;
begin
  dest := my_dest_ptr(cinfo^.dest);

  { Allocate the output buffer --- it will be released when done with image }
  dest^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  OUTPUT_BUF_SIZE * SIZEOF(JOCTET)) );

  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;
end;


{ Empty the output buffer --- called whenever buffer fills up.

  In typical applications, this should write the entire output buffer
  (ignoring the current state of next_output_byte & free_in_buffer),
  reset the pointer & count to the start of the buffer, and return TRUE
  indicating that the buffer has been dumped.

  In applications that need to be able to suspend compression due to output
  overrun, a FALSE return indicates that the buffer cannot be emptied now.
  In this situation, the compressor will return to its caller (possibly with
  an indication that it has not accepted all the supplied scanlines).  The
  application should resume compression after it has made more room in the
  output buffer.  Note that there are substantial restrictions on the use of
  suspension --- see the documentation.

  When suspending, the compressor will back up to a convenient restart point
  (typically the start of the current MCU). next_output_byte & free_in_buffer
  indicate where the restart point will be if the current call returns FALSE.
  Data beyond this point will be regenerated after resumption, so do not
  write it out when emptying the buffer externally. }

{METHODDEF}
function empty_output_buffer (cinfo : j_compress_ptr) : boolean; far;
var
  dest : my_dest_ptr;
begin
  dest := my_dest_ptr(cinfo^.dest);

  if (JFWRITE(dest^.outfile, dest^.buffer, OUTPUT_BUF_SIZE) <>
      size_t(OUTPUT_BUF_SIZE)) then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);

  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;

  empty_output_buffer := TRUE;
end;


{ Terminate destination --- called by jpeg_finish_compress
  after all data has been written.  Usually needs to flush buffer.

  NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
  application must deal with any cleanup that should happen even
  for error exit. }

{METHODDEF}
procedure term_destination (cinfo : j_compress_ptr); far;
var
  dest : my_dest_ptr;
  datacount : size_t;
begin
  dest := my_dest_ptr (cinfo^.dest);
  datacount := OUTPUT_BUF_SIZE - dest^.pub.free_in_buffer;

  { Write any data remaining in the buffer }
  if (datacount > 0) then
  begin
    if (JFWRITE(dest^.outfile, dest^.buffer, datacount) <> datacount) then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  end;
  {fflush(dest^.outfile^);}

  { Make sure we wrote the output file OK }
  {if (ferror(dest^.outfile))
    ERREXIT(cinfo, JERR_FILE_WRITE);}
  if IOresult <> 0 then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
end;


{ Prepare for output to a stdio stream.
  The caller must have already opened the stream, and is responsible
  for closing it after finishing compression. }

{GLOBAL}
procedure jpeg_stdio_dest (cinfo : j_compress_ptr; outfile : FILEptr);
var
  dest : my_dest_ptr;
begin
  { The destination object is made permanent so that multiple JPEG images
    can be written to the same file without re-executing jpeg_stdio_dest.
    This makes it dangerous to use this manager and a different destination
    manager serially with the same JPEG object, because their private object
    sizes may be different.  Caveat programmer. }

  if (cinfo^.dest = NIL) then
  begin { first time for this JPEG object? }
    cinfo^.dest := jpeg_destination_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(my_destination_mgr)) );
  end;

  dest := my_dest_ptr (cinfo^.dest);
  dest^.pub.init_destination := init_destination;
  dest^.pub.empty_output_buffer := empty_output_buffer;
  dest^.pub.term_destination := term_destination;
  dest^.outfile := outfile;
end;

end.
