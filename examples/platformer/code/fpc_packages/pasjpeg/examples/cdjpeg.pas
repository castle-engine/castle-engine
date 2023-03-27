Unit CdJpeg;

{ OriginaL : cdjpeg.h+cdjpeg.c ;  Copyright (C) 1994-1996, Thomas G. Lane.

  This file contains common support routines used by the IJG application
  programs (cjpeg, djpeg, jpegtran).

  This file contains common declarations for the sample applications
  cjpeg and djpeg.  It is NOT used by the core JPEG library. }

{$define JPEG_CJPEG_DJPEG}      { define proper options in jconfig.h }
{$define JPEG_INTERNAL_OPTIONS} { cjpeg.c,djpeg.c need to see xxx_SUPPORTED }

interface

{$I jconfig.inc}

uses
  jmorecfg, jinclude, jpeglib,
  jdeferr,
  {cderror,}          { get application-specific error codes }
  jerror;           { get library error codes too }


const
  EXIT_SUCCESS = 0;
  EXIT_FAILURE = 1;
  EXIT_WARNING = 2;

type                            { Nomssi }
  BGRptr = ^BGRtype;
  BGRtype = packed record
    b,g,r : byte;
  end;
type                            { Nomssi }
  RGBptr = ^RGBtype;
  RGBtype = packed record
    r,g,b : JSAMPLE;
  end;

{ Object interface for cjpeg's source file decoding modules }

type
  cjpeg_source_ptr = ^cjpeg_source_struct;
  cjpeg_source_struct = record
    start_input : procedure (cinfo : j_compress_ptr;
                           sinfo : cjpeg_source_ptr);
    get_pixel_rows : function (cinfo : j_compress_ptr;
                             sinfo : cjpeg_source_ptr) : JDIMENSION;
    finish_input : procedure (cinfo : j_compress_ptr;
                            sinfo : cjpeg_source_ptr);
    input_file : FILEptr;

    buffer : JSAMPARRAY;
    buffer_height : JDIMENSION;
  end;


{ Object interface for djpeg's output file encoding modules }

type
  djpeg_dest_ptr = ^djpeg_dest_struct;
  djpeg_dest_struct = record
    { start_output is called after jpeg_start_decompress finishes.
      The color map will be ready at this time, if one is needed. }

    start_output : procedure (cinfo : j_decompress_ptr;
                              dinfo : djpeg_dest_ptr);
    { Emit the specified number of pixel rows from the buffer. }
    put_pixel_rows : procedure (cinfo : j_decompress_ptr;
                                dinfo : djpeg_dest_ptr;
                                rows_supplied : JDIMENSION);
    { Finish up at the end of the image. }
    finish_output : procedure (cinfo : j_decompress_ptr;
                               dinfo : djpeg_dest_ptr);

    { Target file spec; filled in by djpeg.c after object is created. }
    output_file : FILEptr;

    { Output pixel-row buffer.  Created by module init or start_output.
      Width is cinfo^.output_width * cinfo^.output_components;
      height is buffer_height. }

    buffer : JSAMPARRAY;
    buffer_height : JDIMENSION;
  end;


{ cjpeg/djpeg may need to perform extra passes to convert to or from
  the source/destination file format.  The JPEG library does not know
  about these passes, but we'd like them to be counted by the progress
  monitor.  We use an expanded progress monitor object to hold the
  additional pass count. }

type
  cd_progress_ptr = ^cdjpeg_progress_mgr;
  cdjpeg_progress_mgr = record
    pub : jpeg_progress_mgr;    { fields known to JPEG library }
    completed_extra_passes : int;       { extra passes completed }
    total_extra_passes : int;   { total extra }
    { last printed percentage stored here to avoid multiple printouts }
    percent_done : int;
  end;

{GLOBAL}
procedure enable_signal_catcher (cinfo : j_common_ptr);

{ Case-insensitive matching of possibly-abbreviated keyword switches.
  keyword is the constant keyword (must be lower case already),
  minchars is length of minimum legal abbreviation. }

{GLOBAL}
function keymatch (arg : string;
                   const keyword : string;
                   minchars : int) : boolean;

{$ifdef PROGRESS_REPORT}

{GLOBAL}
procedure start_progress_monitor (cinfo : j_common_ptr;
                                  progress : cd_progress_ptr);

{GLOBAL}
procedure end_progress_monitor (cinfo : j_common_ptr);

{$endif}

implementation


{GLOBAL}
procedure enable_signal_catcher (cinfo : j_common_ptr);
begin
  RunError(255);  { not translated - Jacques Nomssi }
end;

{ Optional progress monitor: display a percent-done figure on stderr. }


{$ifdef PROGRESS_REPORT}

{METHODDEF}
procedure progress_monitor (cinfo : j_common_ptr); far;
var
  prog : cd_progress_ptr;
  total_passes : int;
  percent_done : int;
begin
  prog := cd_progress_ptr (cinfo^.progress);
  total_passes := prog^.pub.total_passes + prog^.total_extra_passes;
  percent_done := int (prog^.pub.pass_counter*Long(100) div prog^.pub.pass_limit);
  if (percent_done <> prog^.percent_done) then
  begin
    prog^.percent_done := percent_done;
    if (total_passes > 1) then
      Write(output, #13'Pass ',
              prog^.pub.completed_passes + prog^.completed_extra_passes + 1,
              '/',total_passes,': ',percent_done:3,'% ')
    else
      Write(#13' ', percent_done,'% ');
    {fflush(stderr);}
  end;
end;


{GLOBAL}
procedure start_progress_monitor (cinfo : j_common_ptr;
                                  progress : cd_progress_ptr);
begin
  { Enable progress display, unless trace output is on }
  if (cinfo^.err^.trace_level = 0) then
  begin
    progress^.pub.progress_monitor := progress_monitor;
    progress^.completed_extra_passes := 0;
    progress^.total_extra_passes := 0;
    progress^.percent_done := -1;
    cinfo^.progress := @progress^.pub;
  end;
end;


{GLOBAL}
procedure end_progress_monitor (cinfo : j_common_ptr);
begin
  { Clear away progress display }
  if (cinfo^.err^.trace_level = 0) then
  begin
    WriteLn(#13'                '#13);
    {fflush(stderr);}
  end;
end;

{$endif}


{ Case-insensitive matching of possibly-abbreviated keyword switches.
  keyword is the constant keyword (must be lower case already),
  minchars is length of minimum legal abbreviation. }

{GLOBAL}
function keymatch (arg : string;
                   const keyword : string;
                   minchars : int) : boolean;
var
  {register} i : int;
  ca, ck : char;
  {register} nmatched : int;
begin
  nmatched := 0;

  i := 1;
  if length(arg) > length(keyword) then
  begin
    keymatch := FALSE; { arg longer than keyword, no good }
    exit;
  end;
  while (i <= length(arg)) do
  begin
    ca := UpCase(arg[i]);
    ck := UpCase(keyword[i]);
    Inc(i);
    if (ca <> ck) then
    begin
      keymatch := FALSE;                { no good }
      exit;
    end;
    Inc(nmatched);              { count matched characters }
  end;
  { reached end of argument; fail if it's too short for unique abbrev }
  keymatch := (nmatched >= minchars);
end;

{$IFDEF std I/O}

{ Routines to establish binary I/O mode for stdin and stdout.
  Non-Unix systems often require some hacking to get out of text mode. }

{GLOBAL}
function read_stdin : FILEptr;
var
  input_file : FILEptr;
begin
  input_file := @input;

{$ifdef USE_SETMODE}            { need to hack file mode? }
  setmode(fileno(stdin), O_BINARY);
{$endif}
{$ifdef USE_FDOPEN}             { need to re-open in binary mode? }
  if ((input_file = fdopen(fileno(stdin), READ_BINARY)) = NIL) then
  begin
    WriteLn(stderr, 'Cannot reopen stdin');
    Halt(EXIT_FAILURE);
  end;
{$endif}
  read_stdin := input_file;
end;


{GLOBAL}
function write_stdout : FILEptr;
var
  output_file : FILEptr;
begin
  output_file := @output;
{$ifdef USE_SETMODE}     { need to hack file mode? }
  setmode(fileno(stdout), O_BINARY);
{$endif}
{$ifdef USE_FDOPEN}             { need to re-open in binary mode? }
  if ((output_file = fdopen(fileno(stdout), WRITE_BINARY)) = NIL) then
  begin
    WriteLn(stderr, 'Cannot reopen stdout');
    Halt(EXIT_FAILURE);
  end;
{$endif}
  write_stdout := output_file;
end;
{$ENDIF}

end.
