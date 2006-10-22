unit KambiPasJpeg_stream_mgrs;

{$mode delphi}

{$I kambiconf.inc}

interface

uses
  {jpeg units} jmorecfg, jpeglib, jerror, jdeferr, jdmarker,
  Classes;

type
  { }
  passtream_source_mgr = record
    pub    : jpeg_source_mgr;   {< public fields}
    infile : TStream;           {< source stream}
    buffer : JOCTET_FIELD_PTR;  {< start of buffer}
    start_of_file : boolean;    {< have we gotten any data yet?}
  end;
  passtream_source_ptr = ^passtream_source_mgr;

  passtream_dest_mgr = record
    pub     : jpeg_destination_mgr;  {< public fields}
    outfile : TStream;               {< target stream}
    buffer  : JOCTET_FIELD_PTR;      {< start of buffer}
  end;
  passtream_dest_ptr = ^passtream_dest_mgr;

procedure jpeg_stream_source(cinfo : j_decompress_ptr; const infile: TStream);
procedure jpeg_stream_dest(cinfo : j_compress_ptr; const outfile: TStream);

implementation

{ ---------------------------------------------------------------------- }
{   source manager to read compressed data                               }
{   for reference: JDATASRC.PAS in PASJPG10 library                      }
{
  Kambi notes : this code was originally in pasjpeg.pas written by Nomssi.
  I only changed name of manager from my_source_mgr to
  passtream_source_mgr since this is the purpose of this manager :
  read source data from a pascal stream - that is, TStream class.
  Almost whole Nomsii code for this looks good, I did only minimal changes
  not worth noticing !
}
{ ---------------------------------------------------------------------- }

const
  INPUT_BUF_SIZE = 4096;

procedure init_source(cinfo : j_decompress_ptr); 
var
  src : passtream_source_ptr;
begin
  src := passtream_source_ptr(cinfo^.src);
  src^.start_of_file := TRUE;
end;

function fill_input_buffer(cinfo : j_decompress_ptr) : boolean; 
var
  src : passtream_source_ptr;
  nbytes : size_t;
begin
  src := passtream_source_ptr(cinfo^.src);
  nbytes := src^.infile.Read(src^.buffer^, INPUT_BUF_SIZE);
  if (nbytes <= 0) then begin
    if (src^.start_of_file) then   {Treat empty input file as fatal error}
      ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);
    WARNMS(j_common_ptr(cinfo), JWRN_JPEG_EOF);
    {Insert a fake EOI marker}
    src^.buffer^[0] := JOCTET ($FF);
    src^.buffer^[1] := JOCTET (JPEG_EOI);
    nbytes := 2;
  end;
  src^.pub.next_input_byte := JOCTETptr(src^.buffer);
  src^.pub.bytes_in_buffer := nbytes;
  src^.start_of_file := FALSE;
  fill_input_buffer := TRUE;
end;

procedure skip_input_data(cinfo : j_decompress_ptr;
                      num_bytes : long); 
var
  src : passtream_source_ptr;
begin
  src := passtream_source_ptr (cinfo^.src);
  if (num_bytes > 0) then begin
    while (num_bytes > long(src^.pub.bytes_in_buffer)) do begin
      Dec(num_bytes, long(src^.pub.bytes_in_buffer));
      fill_input_buffer(cinfo);
      { note we assume that fill_input_buffer will never return FALSE,
        so suspension need not be handled. }
    end;
    Inc( src^.pub.next_input_byte, size_t(num_bytes) );
    Dec( src^.pub.bytes_in_buffer, size_t(num_bytes) );
  end;
end;

procedure term_source(cinfo : j_decompress_ptr); 
begin
  { no work necessary here }
end;

procedure jpeg_stream_source(cinfo : j_decompress_ptr; const infile: TStream);
var
  src : passtream_source_ptr;
begin
  if (cinfo^.src = nil) then begin {first time for this JPEG object?}
    cinfo^.src := jpeg_source_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(passtream_source_mgr)) );
    src := passtream_source_ptr (cinfo^.src);
    src^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  INPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  end;
  src := passtream_source_ptr (cinfo^.src);
  {override pub's method pointers}
  src^.pub.init_source := {$ifdef FPC_OBJFPC} @ {$endif} init_source;
  src^.pub.fill_input_buffer := {$ifdef FPC_OBJFPC} @ {$endif} fill_input_buffer;
  src^.pub.skip_input_data := {$ifdef FPC_OBJFPC} @ {$endif} skip_input_data;
  src^.pub.resync_to_restart := {$ifdef FPC_OBJFPC} @ {$endif} jpeg_resync_to_restart; {use default method}
  src^.pub.term_source := {$ifdef FPC_OBJFPC} @ {$endif} term_source;
  {define our fields}
  src^.infile := infile;
  src^.pub.bytes_in_buffer := 0;   {forces fill_input_buffer on first read}
  src^.pub.next_input_byte := nil; {until buffer loaded}
end;

{ ---------------------------------------------------------------------- }
{   destination manager to write compressed data                         }
{   for reference: JDATADST.PAS in PASJPG10 library                      }
{
  Kambi notes : this code was originally in pasjpeg.pas written by Nomssi.
  I only changed name of manager from my_dest_mgr to
  passtream_dest_mgr since this is the purpose of this manager :
  write destination data to a pascal stream - that is, TStream class.
  Almost whole Nomsii code for this looks good, I did only minimal changes
  not worth noticing !
}
{ ---------------------------------------------------------------------- }

const
  OUTPUT_BUF_SIZE = 4096;

procedure init_destination(cinfo : j_compress_ptr); 
var
  dest : passtream_dest_ptr;
begin
  dest := passtream_dest_ptr(cinfo^.dest);
  dest^.buffer := JOCTET_FIELD_PTR(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_IMAGE,
                                  OUTPUT_BUF_SIZE * SIZEOF(JOCTET)) );
  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;
end;

function empty_output_buffer(cinfo : j_compress_ptr) : boolean; 
var
  dest : passtream_dest_ptr;
begin
  dest := passtream_dest_ptr(cinfo^.dest);
  if (dest^.outfile.Write(dest^.buffer^, OUTPUT_BUF_SIZE)
        <> size_t(OUTPUT_BUF_SIZE))
  then
    ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
  dest^.pub.next_output_byte := JOCTETptr(dest^.buffer);
  dest^.pub.free_in_buffer := OUTPUT_BUF_SIZE;
  empty_output_buffer := TRUE;
end;

procedure term_destination(cinfo : j_compress_ptr); 
var
  dest : passtream_dest_ptr;
  datacount : size_t;
begin
  dest := passtream_dest_ptr (cinfo^.dest);
  datacount := OUTPUT_BUF_SIZE - dest^.pub.free_in_buffer;
  {write any data remaining in the buffer}
  if (datacount > 0) then
    if dest^.outfile.Write(dest^.buffer^, datacount) <> datacount then
      ERREXIT(j_common_ptr(cinfo), JERR_FILE_WRITE);
end;

procedure jpeg_stream_dest(cinfo : j_compress_ptr; const outfile: TStream);
var
  dest : passtream_dest_ptr;
begin
  if (cinfo^.dest = nil) then begin {first time for this JPEG object?}
    cinfo^.dest := jpeg_destination_mgr_ptr(
      cinfo^.mem^.alloc_small (j_common_ptr(cinfo), JPOOL_PERMANENT,
                                  SIZEOF(passtream_dest_mgr)) );
  end;
  dest := passtream_dest_ptr (cinfo^.dest);
  {override pub's method pointers}
  dest^.pub.init_destination := {$ifdef FPC_OBJFPC} @ {$endif} init_destination;
  dest^.pub.empty_output_buffer := {$ifdef FPC_OBJFPC} @ {$endif} empty_output_buffer;
  dest^.pub.term_destination := {$ifdef FPC_OBJFPC} @ {$endif} term_destination;
  {define our fields}
  dest^.outfile := outfile;
end;

end.
