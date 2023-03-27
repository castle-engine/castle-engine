Program rdjpgcom;

{ This file contains a very simple stand-alone application that displays
  the text in COM (comment) markers in a JFIF file.
  This may be useful as an example of the minimum logic needed to parse
  JPEG markers. }

{ Original: Copyright (C) 1994-1995, Thomas G. Lane. }
uses
  Objects;

const
  EXIT_FAILURE = 1;
  EXIT_SUCCESS = 0;
type
  int = integer;
  uInt = word;

{ These macros are used to read the input file.
  To reuse this code in another application, you might need to change these. }

var
  infile : TBufStream;  { input JPEG file }

{ Return next input byte, or EOF if no more }

function NEXTBYTE : byte;
var
  c : Byte;
begin
  infile.Read(c, 1);
  NEXTBYTE := c;
end;

{ Error exit handler }
procedure ERREXIT(msg : string);
begin
  WriteLn(output, msg);
  Halt(EXIT_FAILURE);
end;


{ Read one byte, testing for EOF }
function read_1_byte : int;
var
  c : int;
begin
  c := NEXTBYTE;
  if (infile.Status <> stOK) then
    ERREXIT('Premature EOF in JPEG file');
  read_1_byte := c;
end;

{ Read 2 bytes, convert to unsigned int }
{ All 2-byte quantities in JPEG markers are MSB first }
function read_2_bytes : uInt;
var
  c : word;
begin
  infile.Read(c, 2);
  c := Swap(c);
  if (infile.Status <> stOK) then
    ERREXIT('Premature EOF in JPEG file');
  read_2_bytes := c;
end;


{ JPEG markers consist of one or more 0xFF bytes, followed by a marker
  code byte (which is not an FF).  Here are the marker codes of interest
  in this program.  (See jdmarker.c for a more complete list.) }

const
  M_SOF0  = $C0;        { Start Of Frame N }
  M_SOF1  = $C1;        { N indicates which compression process }
  M_SOF2  = $C2;        { Only SOF0-SOF2 are now in common use }
  M_SOF3  = $C3;
  M_SOF5  = $C5;        { NB: codes C4 and CC are NOT SOF markers }
  M_SOF6  = $C6;
  M_SOF7  = $C7;
  M_SOF9  = $C9;
  M_SOF10 = $CA;
  M_SOF11 = $CB;
  M_SOF13 = $CD;
  M_SOF14 = $CE;
  M_SOF15 = $CF;
  M_SOI   = $D8;        { Start Of Image (beginning of datastream) }
  M_EOI   = $D9;        { End Of Image (end of datastream) }
  M_SOS   = $DA;        { Start Of Scan (begins compressed data) }
  M_COM   = $FE;        { COMment }


{ Find the next JPEG marker and return its marker code.
  We expect at least one FF byte, possibly more if the compressor used FFs
  to pad the file.
  There could also be non-FF garbage between markers.  The treatment of such
  garbage is unspecified; we choose to skip over it but emit a warning msg.
  NB: this routine must not be used after seeing SOS marker, since it will
  not deal correctly with FF/00 sequences in the compressed image data... }

function next_marker : int;
var
  c : int;
  discarded_bytes : int;
begin
  discarded_bytes := 0;

  { Find 0xFF byte; count and skip any non-FFs. }
  c := read_1_byte;
  while (c <> $FF) do
  begin
    Inc(discarded_bytes);
    c := read_1_byte;
  end;
  { Get marker code byte, swallowing any duplicate FF bytes.  Extra FFs
    are legal as pad bytes, so don't count them in discarded_bytes. }

  repeat
    c := read_1_byte;
  until (c <> $FF);

  if (discarded_bytes <> 0) then
  begin
    WriteLn(output, 'Warning: garbage data found in JPEG file');
  end;

  next_marker := c;
end;


{ Read the initial marker, which should be SOI.
  For a JFIF file, the first two bytes of the file should be literally
  $FF M_SOI.  To be more general, we could use next_marker, but if the
  input file weren't actually JPEG at all, next_marker might read the whole
  file and then return a misleading error message... }

function first_marker : int;
var
  c1, c2 : int;
begin
  c1 := NEXTBYTE;
  c2 := NEXTBYTE;
  if (c1 <> $FF) or (c2 <> M_SOI) then
    ERREXIT('Not a JPEG file');
  first_marker := c2;
end;


{ Most types of marker are followed by a variable-length parameter segment.
  This routine skips over the parameters for any marker we don't otherwise
  want to process.
  Note that we MUST skip the parameter segment explicitly in order not to
  be fooled by $FF bytes that might appear within the parameter segment;
  such bytes do NOT introduce new markers. }

procedure skip_variable;
{ Skip over an unknown or uninteresting variable-length marker }
var
  length : uInt;
begin
  { Get the marker parameter length count }
  length := read_2_bytes;
  { Length includes itself, so must be at least 2 }
  if (length < 2) then
    ERREXIT('Erroneous JPEG marker length');
  Dec(length, 2);
  { Skip over the remaining bytes }
  while (length > 0) do
  begin
    read_1_byte;
    Dec(length);
  end;
end;


{ Process a COM marker.
  We want to print out the marker contents as legible text;
  we must guard against random junk and varying newline representations. }

procedure process_COM;
const
  CR = 13;
  LF = 10;
var
  length : uInt;
  comment : string;
  lastch : byte;
begin
  comment := '';
  { Get the marker parameter length count }
  length := read_2_bytes;
  { Length includes itself, so must be at least 2 }
  if (length < 2) then
    ERREXIT('Erroneous JPEG marker length');
  Dec(length, 2);

  comment := '';
  while (length > 0) do
  begin
    comment := comment + char(read_1_byte);
    Dec(length);
  end;
  WriteLn(comment);
end;


{ Process a SOFn marker.
  This code is only needed if you want to know the image dimensions... }

procedure process_SOFn (marker : int);
var
  length : uInt;
  image_height, image_width : uInt;
  data_precision, num_components : int;
  process : string;
  ci: int;
begin
  length := read_2_bytes;       { usual parameter length count }

  data_precision := read_1_byte;
  image_height := read_2_bytes;
  image_width := read_2_bytes;
  num_components := read_1_byte;

  case marker of
  M_SOF0:  process := 'Baseline';
  M_SOF1:  process := 'Extended sequential';
  M_SOF2:  process := 'Progressive';
  M_SOF3:  process := 'Lossless';
  M_SOF5:  process := 'Differential sequential';
  M_SOF6:  process := 'Differential progressive';
  M_SOF7:  process := 'Differential lossless';
  M_SOF9:  process := 'Extended sequential, arithmetic coding';
  M_SOF10: process := 'Progressive, arithmetic coding';
  M_SOF11: process := 'Lossless, arithmetic coding';
  M_SOF13: process := 'Differential sequential, arithmetic coding';
  M_SOF14: process := 'Differential progressive, arithmetic coding';
  M_SOF15: process := 'Differential lossless, arithmetic coding';
  else
   process := 'Unknown';
  end;

  WriteLn('JPEG image is ',image_width,'w * ',image_height,'h, ',
          num_components, ' color components, ',data_precision,
          ' bits per sample');
  WriteLn('JPEG process: ', process);

  if (length <> uInt(8 + num_components * 3)) then
    ERREXIT('Bogus SOF marker length');

  for ci := 0 to pred(num_components) do
  begin
    read_1_byte;        { Component ID code }
    read_1_byte;        { H, V sampling factors }
    read_1_byte;        { Quantization table number }
  end;
end;


{ Parse the marker stream until SOS or EOI is seen;
  display any COM markers.
  While the companion program wrjpgcom will always insert COM markers before
  SOFn, other implementations might not, so we scan to SOS before stopping.
  If we were only interested in the image dimensions, we would stop at SOFn.
  (Conversely, if we only cared about COM markers, there would be no need
  for special code to handle SOFn; we could treat it like other markers.) }

function scan_JPEG_header (verbose : boolean) : int;
var
  marker : int;
begin
  { Expect SOI at start of file }
  if (first_marker <> M_SOI) then
    ERREXIT('Expected SOI marker first');

  { Scan miscellaneous markers until we reach SOS. }
  repeat
    marker := next_marker;
    case marker of
    M_SOF0,             { Baseline }
    M_SOF1,             { Extended sequential, Huffman }
    M_SOF2,             { Progressive, Huffman }
    M_SOF3,             { Lossless, Huffman }
    M_SOF5,             { Differential sequential, Huffman }
    M_SOF6,             { Differential progressive, Huffman }
    M_SOF7,             { Differential lossless, Huffman }
    M_SOF9,             { Extended sequential, arithmetic }
    M_SOF10,            { Progressive, arithmetic }
    M_SOF11,            { Lossless, arithmetic }
    M_SOF13,            { Differential sequential, arithmetic }
    M_SOF14,            { Differential progressive, arithmetic }
    M_SOF15:            { Differential lossless, arithmetic }
      if (verbose) then
        process_SOFn(marker)
      else
        skip_variable;

    M_SOS:                      { stop before hitting compressed data }
    begin
      scan_JPEG_header := marker;
      exit;
    end;

    M_EOI:                      { in case it's a tables-only JPEG stream }
    begin
      scan_JPEG_header := marker;
      exit;
    end;

    M_COM:
      process_COM;

    else                        { Anything else just gets skipped }
      skip_variable;            { we assume it has a parameter count... }
    end;
  until false; { end loop }
end;


{ Command line parsing code }

var
  progname : string[79]; { program name for error messages }


procedure usage;
{ complain about bad command line }
begin
  WriteLn(output, 'rdjpgcom displays any textual comments in a JPEG file.');

  WriteLn(output, 'Usage: ',progname,' [switches] [inputfile]');

  WriteLn(output, 'Switches (names may be abbreviated):');
  WriteLn(output, '  -verbose    Also display dimensions of JPEG image');

  Halt(EXIT_FAILURE);
end;


{ The main program. }

var
  verbose : boolean;
  argn : int;
  arg : string;
begin
  verbose := FALSE;

  progname := ParamStr(0);
  if (progname = '')  then
    progname := 'rdjpgcom';     { in case C library doesn't provide it }

  { Parse switches, if any }
  for argn := 1 to Pred(ParamCount) do
  begin
    arg := ParamStr(argn);
    if arg[1] = '-' then
      if (Pos(arg, '-verbose') > 0) then
      begin
        verbose := TRUE;
      end
      else
        usage;
  end;

  { Open the input file. }
  arg := ParamStr(ParamCount);

  if not infile.Init(arg, stOpenRead, 4096) then
  begin
    WriteLn(output, 'can''t open ', arg);
    Halt(EXIT_FAILURE);
  end;

  { Scan the JPEG headers. }
  scan_JPEG_header(verbose);

  infile.done;

  { All done. }
  Halt(EXIT_SUCCESS);
end.
