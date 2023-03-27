Program WrJpgCom;

{ wrjpgcom.c

  Copyright (C) 1994-1997, Thomas G. Lane.
  This file is part of the Independent JPEG Group's software.
  For conditions of distribution and use, see the accompanying README file.

  This file contains a very simple stand-alone application that inserts
  user-supplied text as a COM (comment) marker in a JFIF file.
  This may be useful as an example of the minimum logic needed to parse
  JPEG markers. }

uses
  jmorecfg,
  jinclude,
  cdjpeg,
  strings,
  fcache;

const
  EXIT_FAILURE = 1;             { define Halt() codes if not provided }
  EXIT_SUCCESS = 0;

{ Reduce this value if your malloc() can't allocate blocks up to 64K.
  On DOS, compiling in large model is usually a better solution. }

const
  MAX_COM_LENGTH = Long(32000);  { must be <= 65533 in any case }


{ These macros are used to read the input file and write the output file.
  To reuse this code in another application, you might need to change these. }

var
  infile : file;                { input JPEG file }

{ Return next input byte, or EOF if no more }

var
  outfile : file;               { output JPEG file }

{ Emit an output byte }
function NEXTBYTE : byte;

var
  B : Byte;

begin
  BlockRead(Infile,B,1);
  NEXTBYTE :=b;
  // fc_getc(var fc : Cache);
{ Read a byte at the current buffer read-index, increment the buffer
  read-index }
end;

procedure PUTBYTE(c : int);
begin
  BlockWrite(outfile,c, 1);
end;

{ Error exit handler }
procedure ERREXIT(msg : string);
begin
  WriteLn(msg);
  Halt(EXIT_FAILURE);
end;


{ Read one byte, testing for EOF }
function read_1_byte : int;
var
  c : byte;
begin
  c := NEXTBYTE;
  if (c = int(EOF)) then
    ERREXIT('Premature EOF in JPEG file');
  read_1_byte := c;
end;

{ Read 2 bytes, convert to uint }
{ All 2-byte quantities in JPEG markers are MSB first }
function read_2_bytes : uint;
var
  c1, c2 : int;
begin
  c1 := NEXTBYTE;
  if (c1 = int(EOF)) then
    ERREXIT('Premature EOF in JPEG file');
  c2 := NEXTBYTE;
  if (c2 = int(EOF)) then
    ERREXIT('Premature EOF in JPEG file');
  read_2_bytes := ((uint(c1)) shl 8) + (uint(c2));
end;


{ Routines to write data to output file }

procedure write_1_byte (c : int);
begin
  PUTBYTE(c);
end;

procedure write_2_bytes (val : uint);
begin
  PUTBYTE((val shr 8) and $FF);
  PUTBYTE(val and $FF);
end;

procedure write_marker (marker : int);
begin
  PUTBYTE($FF);
  PUTBYTE(marker);
end;

procedure copy_rest_of_file;
var
  c : int;
begin
  repeat
    c := NEXTBYTE;
    if (c <> int(EOF)) then
      PUTBYTE(c);
  until (c = int(EOF));
end;


{ JPEG markers consist of one or more $FF bytes, followed by a marker
  code byte (which is not an FF).  Here are the marker codes of interest
  in this program.  (See jdmarker.c for a more complete list.) }
const
  M_SOF0   = $C0;               { Start Of Frame N }
  M_SOF1   = $C1;               { N indicates which compression process }
  M_SOF2   = $C2;               { Only SOF0-SOF2 are now in common use }
  M_SOF3   = $C3;
  M_SOF5   = $C5;               { NB: codes C4 and CC are NOT SOF markers }
  M_SOF6   = $C6;
  M_SOF7   = $C7;
  M_SOF9   = $C9;
  M_SOF10  = $CA;
  M_SOF11  = $CB;
  M_SOF13  = $CD;
  M_SOF14  = $CE;
  M_SOF15  = $CF;
  M_SOI    = $D8;               { Start Of Image (beginning of datastream) }
  M_EOI    = $D9;               { End Of Image (end of datastream) }
  M_SOS    = $DA;               { Start Of Scan (begins compressed data) }
  M_COM    = $FE;               { COMment }


{ Find the next JPEG marker and return its marker code.
  We expect at least one FF byte, possibly more if the compressor used FFs
  to pad the file.  (Padding FFs will NOT be replicated in the output file.)
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

  { Find $FF byte; count and skip any non-FFs. }
  c := read_1_byte;
  while (c <> $FF) do
  begin
    Inc(discarded_bytes);
    c := read_1_byte;
  end;
  { Get marker code byte, swallowing any duplicate FF bytes.  Extra FFs
    are legal as pad bytes, so don't count them in discarded_bytes.  }
  repeat
    c := read_1_byte;
  until (c <> $FF);

  if (discarded_bytes <> 0) then
  begin
    WriteLn('Warning: garbage data found in JPEG file');
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

procedure copy_variable;
{ Copy an unknown or uninteresting variable-length marker }
var
  length : uint;
begin
  { Get the marker parameter length count }
  length := read_2_bytes;
  write_2_bytes(length);
  { Length includes itself, so must be at least 2 }
  if (length < 2) then
    ERREXIT('Erroneous JPEG marker length');
  Dec(length, 2);
  { Skip over the remaining bytes }
  while (length > 0) do
  begin
    write_1_byte(read_1_byte);
    Dec(length);
  end;
end;

procedure skip_variable;
{ Skip over an unknown or uninteresting variable-length marker }
var
  length : uint;
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


{ Parse the marker stream until SOFn or EOI is seen;
  copy data to output, but discard COM markers unless keep_COM is true. }

function scan_JPEG_header (keep_COM : boolean) : int;
var
  marker : int;
begin
  { Expect SOI at start of file }
  if (first_marker <> M_SOI) then
    ERREXIT('Expected SOI marker first');
  write_marker(M_SOI);

  { Scan miscellaneous markers until we reach SOFn. }
  while TRUE do
  begin
    marker := next_marker;
    case marker of
      { Note that marker codes $C4, $C8, $CC are not, and must not be,
        treated as SOFn.  C4 in particular is actually DHT. }
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
      begin
        scan_JPEG_header := marker;
        exit;
      end;

    M_SOS:                      { should not see compressed data before SOF }
      ERREXIT('SOS without prior SOFn');

    M_EOI:                      { in case it's a tables-only JPEG stream }
      begin
        scan_JPEG_header := marker;
        exit;
      end;

    M_COM:                      { Existing COM: conditionally discard }
      if (keep_COM) then
      begin
        write_marker(marker);
        copy_variable;
      end
      else
      begin
        skip_variable;
      end;

    else                { Anything else just gets copied }
      write_marker(marker);
      copy_variable;            { we assume it has a parameter count... }
    end;
  end; { end loop }
end;


{ Command line parsing code }

var
  progname : string;    { program name for error messages }


procedure usage;
{ complain about bad command line }
begin
  WriteLn('wrjpgcom inserts a textual comment in a JPEG file.');
  WriteLn('You can add to or replace any existing comment(s).');

  Write('Usage: ',progname,' [switches] ');
{$ifdef TWO_FILE_COMMANDLINE
  WriteLn('inputfile outputfile');
{$else}
  WriteLn('[inputfile]');
{$endif}

  WriteLn('Switches (names may be abbreviated):');
  WriteLn('  -replace         Delete any existing comments');
  WriteLn('  -comment "text"  Insert comment with given text');
  WriteLn('  -cfile name      Read comment from named file');
  WriteLn('Notice that you must put quotes around the comment text');
  WriteLn('when you use -comment.');
  WriteLn('If you do not give either -comment or -cfile on the command line,');
  WriteLn('then the comment text is read from standard input.');
  WriteLn('It can be multiple lines, up to ',
           uint(MAX_COM_LENGTH),' characters total.');
{$ifndef TWO_FILE_COMMANDLINE}
  WriteLn('You must specify an input JPEG file name when supplying');
  WriteLn('comment text from standard input.');
{$endif}

  Halt(EXIT_FAILURE);
end;


function keymatch (const arg : string;
                   const keyword : string;
                   minchars : int) : boolean;
{ Case-insensitive matching of (possibly abbreviated) keyword switches. }
{ keyword is the constant keyword (must be lower case already), }
{ minchars is length of minimum legal abbreviation. }
var
  {register} ca, ck : char;
  {register} nmatched : int;
  i, len : int;
begin
  nmatched := 0;
  keymatch := FALSE;
  len := Length(keyword);
  if len >= Length(arg) then
    len := Length(arg)
  else
    exit;
  for i := 1 to len do
  begin
    if (UpCase(arg[i]) <> UpCase(keyword[i])) then
      exit;
    Inc(nmatched);                      { count matched characters }
  end;
  { reached end of argument; fail if it's too short for unique abbrev }
  if (nmatched >= minchars) then
    keymatch := TRUE;                   { A-OK }
end;

{ The main program. }
var
  argc,
  argn : int;
  arg : string;
  keep_COM : boolean;
  comment_arg : string;
  comment_arg_0 : PChar;
  comment_file : TBufStream;
  comment_length : uint;
  marker : int;
var
  src_file : PBufStream;
  c : int;
begin
  keep_COM := TRUE;
  comment_arg := '';
  comment_length := 0;

  { On Mac, fetch a command line. }
  argc := ParamCount;

  progname := ParamStr(0);

  { Parse switches, if any }
  argn := 1;
  while (argn < argc) do
  begin
    arg := ParamStr(argn);
    if (arg[1] <> '-') then
      break;                    { not switch, must be file name }
    if (keymatch(arg, '-replace', 2)) then
    begin
      keep_COM := FALSE;
    end
    else
    if (keymatch(arg, '-cfile', 3)) then
    begin
      Inc(argn);
      if (argn >= argc) then
        usage;
      if not comment_file.Init(ParamStr(argn), stOpenRead, 2048) then
      begin
        WriteLn(progname, 'can''t open ', ParamStr(argn));
        Halt(EXIT_FAILURE);
      end;
    end
    else
    {$ifdef comment}
    if (keymatch(arg, '-comment', 2)) then
    begin
      Inc(argn);
      if (argn >= argc) then
        usage;
      comment_arg := ParamStr(argn);
      { If the comment text starts with '"', then we are probably running
        under MS-DOG and must parse out the quoted string ourselves.  Sigh. }
      if (comment_arg[1] = '"') then
      begin
        GetMem(comment_arg_0, size_t(MAX_COM_LENGTH) );
        if (comment_arg_0 = NIL) then
          ERREXIT('Insufficient memory');
        strcopy(comment_arg_0, ParamStr(argn)+1);
        while TRUE do
        begin
          comment_length := uint( strlen(comment_arg) );
          if (comment_length > 0) and
             (comment_arg[comment_length-1] = '"') then
          begin
            comment_arg[comment_length-1] := #0; { zap terminating quote }
            break;
          end;
          Inc(argn);
          if (argn >= argc) then
            ERREXIT('Missing ending quote mark');
          strcat(comment_arg, ' ');
          strcat(comment_arg, argv[argn]);
        end;
      end;
      comment_length := uint(strlen(comment_arg));
    end
    else
      usage;
    {$endif}
    Inc(argn);
  end;

  { Cannot use both -comment and -cfile. }
  if (comment_arg <> '') and (comment_file.status <> stOK) then
    usage;
  { If there is neither -comment nor -cfile, we will read the comment text
    from stdin; in this case there MUST be an input JPEG file name. }
  if (comment_arg = '') and (comment_file.status <> stOK) and (argn >= argc) then
    usage;

  { Open the input file. }
  if (argn < argc) then
  begin
    infile.Init(ParamStr(argn), stOpenRead, 2048);
    if (infile.Status <> stOK) then
    begin
      WriteLn(progname, ': can''t open ', ParamStr(argn));
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    { default input file is stdin }
{$ifdef USE_FDOPEN}      { need to re-open in binary mode? }
    infile := TBufStream.Init('', stOpenRead, 2048);
    if (infile.Status <> stOK) then
    begin
      WriteLn(progname, ': can''t open stdin');
      Halt(EXIT_FAILURE);
    end;
{$else}
    {infile := input;}
    RunError(255);
{$endif}
  end;

  { Open the output file. }
{$ifdef TWO_FILE_COMMANDLINE}
  { Must have explicit output file name }
  if (argn <> argc-2) then
  begin
    WriteLn(progname, ': must name one input and one output file');
    usage;
  end;
  outfile := TBufStream.Init(ParamStr(argn+1), stOpenWrite, 2048);
  if (outfile.Status <> stOK) then
  begin
    WriteLn(progname, ': can't open ', ParamStr(argn+1));
    Halt(EXIT_FAILURE);
  end;
{$else}
  { Unix style: expect zero or one file name }
  if (argn < argc-1) then
  begin
    WriteLn(progname, ': only one input file');
    usage;
  end;
  { default output file is stdout }
{$ifdef USE_FDOPEN}             { need to re-open in binary mode? }
  outfile := TBufStream.Init('', stOpenWrite, 2048);
  if (outfile.Status <> stOK) then
  begin
    WriteLn(progname, ': can''t open stdout');
    Halt(EXIT_FAILURE);
  end;
{$else}
  RunError(255);
  {outfile := stdout;}
{$endif}
{$endif} { TWO_FILE_COMMANDLINE }

  { Collect comment text from comment_file or stdin, if necessary }
  if (comment_arg_0 = NIL) then
  begin

    GetMem(comment_arg_0, size_t(MAX_COM_LENGTH) );
    if (comment_arg_0 = NIL) then
      ERREXIT('Insufficient memory');
    comment_length := 0;
    if comment_file.status = stOK then
      src_file := @comment_file
    else
      src_file := NIL;
    repeat
      c := getc(src_file);
      if (c <> EOF) do
      begin
        if (comment_length >= uint(MAX_COM_LENGTH)) then
        begin
          WriteLn('Comment text may not exceed ',
                   uint(MAX_COM_LENGTH)),' bytes);
          Halt(EXIT_FAILURE);
        end;
        comment_arg[comment_length] := char(c);
        Inc(comment_length);
      end;
    until (c = EOF);
    if (comment_file <> '') then
      fclose(comment_file);
  end;

  { Copy JPEG headers until SOFn marker;
    we will insert the new comment marker just before SOFn.
    This (a) causes the new comment to appear after, rather than before,
    existing comments; and (b) ensures that comments come after any JFIF
    or JFXX markers, as required by the JFIF specification. }
  marker := scan_JPEG_header(keep_COM);
  { Insert the new COM marker, but only if nonempty text has been supplied }
  if (comment_length > 0) then
  begin
    write_marker(M_COM);
    write_2_bytes(comment_length + 2);
    while (comment_length > 0) do
    begin
      write_1_byte(comment_arg^);
      Inc(comment_arg);
      Dec(comment_length);
    end;
  end;
  { Duplicate the remainder of the source file.
    Note that any COM markers occuring after SOF will not be touched. }
  write_marker(marker);
  copy_rest_of_file();

  { All done. }
  Halt(EXIT_SUCCESS);
end.
