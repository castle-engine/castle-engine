Program cjpeg;

{ Original: cjpeg.c ; Copyright (C) 1991-1996, Thomas G. Lane. }

{ This file contains a command-line user interface for the JPEG compressor. }

{ Two different command line styles are permitted, depending on the
  compile-time switch TWO_FILE_COMMANDLINE:
        cjpeg [options]  inputfile outputfile
        cjpeg [options]  [inputfile]
  In the second style, output is always to standard output, which you'd
  normally redirect to a file or pipe to some other program.  Input is
  either from a named file or from standard input (typically redirected).
  The second style is convenient on Unix but is unhelpful on systems that
  don't support pipes.  Also, you MUST use the first style if your system
  doesn't do binary I/O to stdin/stdout.
  To simplify script writing, the "-outfile" switch is provided.  The syntax
        cjpeg [options]  -outfile outputfile  inputfile
  works regardless of which command line style is used. }

{$I jconfig.inc}
{$undef PPM_SUPPORTED}

uses
  jmorecfg,
  cdjpeg,               { Common decls for cjpeg/djpeg applications }
  {jversion,}           { for version message }
  jpeglib,

  jerror,
  jinclude, JDataDst,
  JcAPImin, JcAPIstd, JcParam,
{$ifdef TARGA_SUPPORTED}  rdtarga, {$endif}
{$ifdef BMP_SUPPORTED}  rdbmp, {$endif}
{$ifdef EXT_SWITCH}  rdswitch, {$endif}
  {cderror,}
  jdeferr;


{ This routine determines what format the input file is,
  and selects the appropriate input-reading module.

  To determine which family of input formats the file belongs to,
  we may look only at the first byte of the file, since C does not
  guarantee that more than one character can be pushed back with ungetc.
  Looking at additional bytes would require one of these approaches:
      1) assume we can fseek() the input file (fails for piped input);
      2) assume we can push back more than one character (works in
         some C implementations, but unportable);
      3) provide our own buffering (breaks input readers that want to use
         stdio directly, such as the RLE library);
  or  4) don't put back the data, and modify the input_init methods to assume
         they start reading after the start of file (also breaks RLE library).
  #1 is attractive for MS-DOS but is untenable on Unix.

  The most portable solution for file types that can't be identified by their
  first byte is to make the user tell us what they are.  This is also the
  only approach for "raw" file types that contain only arbitrary values.
  We presently apply this method for Targa files.  Most of the time Targa
  files start with $00, so we recognize that case.  Potentially, however,
  a Targa file could start with any byte value (byte 0 is the length of the
  seldom-used ID field), so we provide a switch to force Targa input mode. }


var
 is_targa : boolean;    { records user -targa switch }

function GetFirstChar(cinfo : j_compress_ptr;
                      fptr : fileptr) : char;
var
  c : char;
begin
  if JFREAD(fptr, @c, 1) <> 1 then
    ERREXIT(j_common_ptr(cinfo), JERR_INPUT_EMPTY);

{$ifndef delphi_stream}
  Seek(fptr^, 0);        { Nomssi: probably not portable }
{$else}
  Fptr^.Seek(0,0);
{$endif}
  if (IOresult <> 0) then
    ERREXIT(j_common_ptr(cinfo), JERR_UNGETC_FAILED);
  GetFirstChar := c;
end;


{LOCAL}
function select_file_type (cinfo : j_compress_ptr;
                           var infile : FILE) : cjpeg_source_ptr;
var
  c : char;
begin
  if (is_targa) then
  begin
{$ifdef TARGA_SUPPORTED}
    select_file_type := jinit_read_targa(cinfo);
    exit;
{$else}
    ERREXIT(j_common_ptr(cinfo), JERR_TGA_NOTCOMP);
{$endif}
  end;

  c := GetFirstChar(cinfo, @infile);

  select_file_type := NIL;      { suppress compiler warnings }
  case c of
{$ifdef BMP_SUPPORTED}
  'B': select_file_type := jinit_read_bmp(cinfo);
{$endif}
{$ifdef GIF_SUPPORTED}
  'G': select_file_type := jinit_read_gif(cinfo);
{$endif}
{$ifdef PPM_SUPPORTED}
  'P': select_file_type := jinit_read_ppm(cinfo);
{$endif}
{$ifdef RLE_SUPPORTED}
  'R': select_file_type := jinit_read_rle(cinfo);
{$endif}
{$ifdef TARGA_SUPPORTED}
  char($00): select_file_type := jinit_read_targa(cinfo);
{$endif}
  else
    ERREXIT(j_common_ptr(cinfo), JERR_UNKNOWN_FORMAT);
  end;
end;


{ Argument-parsing code.
  The switch parser is designed to be useful with DOS-style command line
  syntax, ie, intermixed switches and file names, where only the switches
  to the left of a given file name affect processing of that file.
  The main program in this file doesn't actually use this capability... }


var
  progname,     { program name for error messages }
  outfilename : string[79];     { for -outfile switch }


{LOCAL}
procedure usage;
{ complain about bad command line }
begin
  Write(output, 'usage: ', progname, ' [switches] ');
{$ifdef TWO_FILE_COMMANDLINE}
  WriteLn(output, 'inputfile outputfile');
{$else}
  WriteLn(output, '[inputfile]');
{$endif}

  WriteLn(output, 'Switches (names may be abbreviated):');
  WriteLn(output, '  -quality N     Compression quality (0..100; 5-95 is useful range)');
  WriteLn(output, '  -grayscale     Create monochrome JPEG file');
{$ifdef ENTROPY_OPT_SUPPORTED}
  WriteLn(output, '  -optimize      Optimize Huffman table (smaller file, but slow compression)');
{$endif}
{$ifdef C_PROGRESSIVE_SUPPORTED}
  WriteLn(output, '  -progressive   Create progressive JPEG file');
{$endif}
{$ifdef TARGA_SUPPORTED}
  WriteLn(output, '  -targa         Input file is Targa format (usually not needed)');
{$endif}
  WriteLn(output, 'Switches for advanced users:');
{$ifdef DCT_ISLOW_SUPPORTED}
  if (JDCT_DEFAULT = JDCT_ISLOW) then
    WriteLn(output, '  -dct int       Use integer DCT method (default)')
  else
    WriteLn(output, '  -dct int       Use integer DCT method');
{$endif}
{$ifdef DCT_IFAST_SUPPORTED}
  if (JDCT_DEFAULT = JDCT_IFAST) then
    WriteLn(output, '  -dct fast      Use fast integer DCT (less accurate) (default)')
  else
    WriteLn(output, '  -dct fast      Use fast integer DCT (less accurate)');
{$endif}
{$ifdef DCT_FLOAT_SUPPORTED}
  if (JDCT_DEFAULT = JDCT_FLOAT) then
    WriteLn(output, '  -dct float     Use floating-point DCT method (default)')
  else
    WriteLn(output, '  -dct float     Use floating-point DCT method');
{$endif}
  WriteLn(output, '  -restart N     Set restart interval in rows, or in blocks with B');
{$ifdef INPUT_SMOOTHING_SUPPORTED}
  WriteLn(output, '  -smooth N      Smooth dithered input (N=1..100 is strength)');
{$endif}
  WriteLn(output, '  -maxmemory N   Maximum memory to use (in kbytes)');
  WriteLn(output, '  -outfile name  Specify name for output file');
  WriteLn(output, '  -verbose  or  -debug   Emit debug output');
{$IFDEF EXT_SWITCH}
  WriteLn(output, 'Switches for wizards:');
{$ifdef C_ARITH_CODING_SUPPORTED}
  WriteLn(output, '  -arithmetic    Use arithmetic coding');
{$endif}
  WriteLn(output, '  -baseline      Force baseline output');
  WriteLn(output, '  -qtables file  Use quantization tables given in file');
  WriteLn(output, '  -qslots N[,...]    Set component quantization tables');
  WriteLn(output, '  -sample HxV[,...]  Set component sampling factors');
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
  WriteLn(output, '  -scans file    Create multi-scan JPEG per script file');
{$endif}
{$ENDIF}
  Halt(EXIT_FAILURE);
end;


{LOCAL}
function parse_switches (cinfo : j_compress_ptr;
                         last_file_arg_seen : int;
                         for_real : boolean) : int;
{ Parse optional switches.
  Returns argv[] index of first file-name argument (== argc if none).
  Any file names with indexes <= last_file_arg_seen are ignored;
  they have presumably been processed in a previous iteration.
  (Pass 0 for last_file_arg_seen on the first or only iteration.)
  for_real is FALSE on the first (dummy) pass; we may skip any expensive
  processing. }

var
  argn,
  argc : int;
  arg : string;
var
  value : int;
  code : integer;
var
  quality : int;                { -quality parameter }
  q_scale_factor : int;         { scaling percentage for -qtables }
  force_baseline : boolean;
  simple_progressive : boolean;
  qtablefile,                  { saves -qtables filename if any }
  qslotsarg,                   { saves -qslots parm if any }
  samplearg,                   { saves -sample parm if any }
  scansarg : string;           { saves -scans parm if any }
var
  lval : long;
  ch : char;

const
  printed_version : boolean = FALSE;
begin
  qtablefile := '';
  qslotsarg := '';
  samplearg := '';
  scansarg := '';

  { Set up default JPEG parameters. }
  { Note that default -quality level need not, and does not,
    match the default scaling for an explicit -qtables argument. }

  quality := 75;                        { default -quality value }
  q_scale_factor := 100;                { default to no scaling for -qtables }
  force_baseline := FALSE;      { by default, allow 16-bit quantizers }
  simple_progressive := FALSE;
  is_targa := FALSE;
  outfilename := '';
  cinfo^.err^.trace_level := 0;

  { Scan command line options, adjust parameters }

  argn := 0;
  argc := ParamCount;

  while argn < argc do
  begin
    Inc(argn);
    arg := ParamStr(argn);
    if (arg[1] <> '-') then
    begin
      { Not a switch, must be a file name argument }
      if (argn <= last_file_arg_seen) then
      begin
        outfilename := '';      { -outfile applies to just one input file }
        continue;               { ignore this name if previously processed }
      end;
      break;                    { else done parsing switches }
    end;
    {Inc(arg);                  - advance past switch marker character }

    if (keymatch(arg, '-arithmetic', 2)) then
    begin
      { Use arithmetic coding. }
{$ifdef C_ARITH_CODING_SUPPORTED}
      cinfo^.arith_code := TRUE;
{$else}
      WriteLn(output, progname, ': sorry, arithmetic coding not supported');
      Halt(EXIT_FAILURE);
{$endif}

    end
    else
      if (keymatch(arg, '-baseline', 2)) then
      begin
      { Force baseline output (8-bit quantizer values). }
      force_baseline := TRUE;

    end
    else
    if (keymatch(arg, '-dct', 3)) then
    begin
      { Select DCT algorithm. }
      Inc(argn);
      if (argn >= argc) then    { advance to next argument }
        usage;
      if (keymatch(ParamStr(argn), 'int', 1)) then
      begin
        cinfo^.dct_method := JDCT_ISLOW;
      end
      else
        if (keymatch(ParamStr(argn), 'fast', 2)) then
        begin
          cinfo^.dct_method := JDCT_IFAST;
        end
        else
          if (keymatch(ParamStr(argn), 'float', 2)) then
          begin
            cinfo^.dct_method := JDCT_FLOAT;
          end
          else
            usage;

    end
    else
      if keymatch(arg, '-debug', 2) or keymatch(arg, '-verbose', 2) then
      begin
        { Enable debug printouts. }
        { On first -d, print version identification }

        if (not printed_version) then
        begin
          WriteLn(output, 'Independent JPEG Group''s CJPEG, version ', JVERSION);
          WriteLn(output, JCOPYRIGHT);
          WriteLn(output, JNOTICE);
          printed_version := TRUE;
        end;
        Inc(cinfo^.err^.trace_level);

      end
      else
      if (keymatch(arg, '-grayscale', 3)) or (keymatch(arg, '-greyscale',3)) then
      begin
        { Force a monochrome JPEG file to be generated. }
        jpeg_set_colorspace(cinfo, JCS_GRAYSCALE);

      end
      else
        if (keymatch(arg, '-maxmemory', 4)) then
        begin
          ch := 'x';

          Inc(argn);
          if (argn >= argc) then        { advance to next argument }
            usage;

          arg := ParamStr(argn);
          if (length(arg) > 1) and (arg[length(arg)] in ['m','M']) then
          begin
            ch := arg[length(arg)];
            arg := Copy(arg, 1, Length(arg)-1);
          end;
          Val(arg, lval, code);
          if (code <> 1) then
            usage;
          if (ch = 'm') or (ch = 'M') then
            lval := lval * long(1000);
          cinfo^.mem^.max_memory_to_use := lval * long(1000);

        end
        else
          if (keymatch(arg, '-optimize', 2)) or (keymatch(arg, '-optimise', 2)) then
          begin
            { Enable entropy parm optimization. }
      {$ifdef ENTROPY_OPT_SUPPORTED}
            cinfo^.optimize_coding := TRUE;
      {$else}
            WriteLn(output, progname, ': sorry, entropy optimization was not compiled');
            exit(EXIT_FAILURE);
      {$endif}

          end
          else
            if (keymatch(arg, '-outfile', 5)) then
            begin
              { Set output file name. }
              Inc(argn);
              if (argn >= argc) then    { advance to next argument }
                usage;
              outfilename := ParamStr(argn);    { save it away for later use }

            end
            else
              if (keymatch(arg, '-progressive', 2)) then
              begin
                { Select simple progressive mode. }
          {$ifdef C_PROGRESSIVE_SUPPORTED}
                simple_progressive := TRUE;
                { We must postpone execution until num_components is known. }
          {$else}
                WriteLn(output, progname, ': sorry, progressive output was not compiled');
                Halt(EXIT_FAILURE);
          {$endif}

              end
              else
                if (keymatch(arg, '-quality', 2)) then
                begin
                  { Quality factor (quantization table scaling factor). }
                  Inc(argn);
                  if (argn >= argc) then        { advance to next argument }
                    usage;
                  Val(ParamStr(argn), quality, code);
                  if code <> 0 then
                    usage;

                  { Change scale factor in case -qtables is present. }
                  q_scale_factor := jpeg_quality_scaling(quality);

                end
                else
                  if (keymatch(arg, '-qslots', 3)) then
                  begin
                    { Quantization table slot numbers. }
                    Inc(argn);
                    if (argn >= argc) then      { advance to next argument }
                      usage;
                    qslotsarg := ParamStr(argn);
                    { Must delay setting qslots until after we have processed any
                      colorspace-determining switches, since jpeg_set_colorspace sets
                      default quant table numbers. }

                  end
                  else
                    if (keymatch(arg, '-qtables', 3)) then
                    begin
                      { Quantization tables fetched from file. }
                      Inc(argn);
                      if (argn >= argc) then    { advance to next argument }
                        usage;
                      qtablefile := ParamStr(argn);
                      { We postpone actually reading the file in case -quality comes later. }

                    end
                    else
                      if (keymatch(arg, '-restart', 2)) then
                      begin
                        { Restart interval in MCU rows (or in MCUs with 'b'). }
                        ch := 'x';

                        Inc(argn);
                        if (argn >= argc) then  { advance to next argument }
                          usage;

                        arg := ParamStr(argn);
                        if (length(arg) > 1) and (arg[length(arg)] in ['b','B']) then
                        begin
                          ch := arg[length(arg)];
                          arg := Copy(arg, 1, Length(arg)-1);
                        end;

                        Val(arg, lval, Code);
                        if (code <> 1) then
                          usage;
                        if (lval < 0) or (lval > long(65535)) then
                          usage;
                        if (ch = 'b') or (ch = 'B') then
                        begin
                          cinfo^.restart_interval := uInt (lval);
                          cinfo^.restart_in_rows := 0; { else prior '-restart n' overrides me }
                        end
                        else
                        begin
                          cinfo^.restart_in_rows := int (lval);
                          { restart_interval will be computed during startup }
                        end;
                      end
                      else
                        if (keymatch(arg, '-sample', 3)) then
                        begin
                          { Set sampling factors. }
                          Inc(argn);
                          if (argn >= argc) then        { advance to next argument }
                            usage;
                          samplearg := ParamStr(argn);
                          { Must delay setting sample factors until after we have processed any
                            colorspace-determining switches, since jpeg_set_colorspace sets
                            default sampling factors. }

                        end
                        else
                          if (keymatch(arg, '-scans', 3)) then
                          begin
                            { Set scan script. }
                      {$ifdef C_MULTISCAN_FILES_SUPPORTED}
                            Inc(argn);
                            if (argn >= argc) then      { advance to next argument }
                              usage;
                            scansarg := ParamStr(argn);
                            { We must postpone reading the file in case -progressive appears. }
                      {$else}
                            WriteLn(output, progname, ': sorry, multi-scan output was not compiled');
                            Halt(EXIT_FAILURE);
                      {$endif}

                          end
                          else
                            if (keymatch(arg, '-smooth', 3)) then
                            begin
                              { Set input smoothing factor. }

                              Inc(argn);
                              if (argn >= argc) then    { advance to next argument }
                                usage;
                              Val(ParamStr(argn), value, code);
                              if (value < 0) or (value > 100)
                                 or (code <> 0) then
                                usage;
                              cinfo^.smoothing_factor := value;

                            end
                            else
                              if (keymatch(arg, '-targa', 2)) then
                              begin
                                { Input file is Targa format. }
                                is_targa := TRUE;

                              end
                              else
                              begin
                                usage;                  { bogus switch }
                              end;
  end;

  { Post-switch-scanning cleanup }

  if (for_real) then
  begin

    { Set quantization tables for selected quality. }
    { Some or all may be overridden if -qtables is present. }
    jpeg_set_quality(cinfo, quality, force_baseline);

{$IFDEF EXT_SWITCH}
    if (qtablefile <> '') then  { process -qtables if it was present }
      if (not read_quant_tables(cinfo, qtablefile,
                              q_scale_factor, force_baseline)) then
        usage;

    if (qslotsarg <> '') then   { process -qslots if it was present }
      if (not set_quant_slots(cinfo, qslotsarg)) then
        usage;

    if (samplearg <> '') then   { process -sample if it was present }
      if (not set_sample_factors(cinfo, samplearg)) then
        usage;
{$ENDIF}

{$ifdef C_PROGRESSIVE_SUPPORTED}
    if (simple_progressive) then        { process -progressive; -scans can override }
      jpeg_simple_progression(cinfo);
{$endif}

{$IFDEF EXT_SWITCH}
{$ifdef C_MULTISCAN_FILES_SUPPORTED}
    if (scansarg <> '') then    { process -scans if it was present }
      if (not read_scan_script(cinfo, scansarg)) then
        usage;
{$endif}
{$ENDIF}
  end;

  parse_switches := argn;       { return index of next arg (file name) }
end;


{ The main program. }

var
  cinfo : jpeg_compress_struct;
  jerr : jpeg_error_mgr;
{$ifdef PROGRESS_REPORT}
  progress : cdjpeg_progress_mgr;
{$endif}
  file_index : int;
  src_mgr : cjpeg_source_ptr;
  input_file : FILE;
  output_file : FILE;
  num_scanlines : JDIMENSION;
var
  argc : int;
begin
  argc := ParamCount;

  progname := ParamStr(0);

  { Initialize the JPEG compression object with default error handling. }
  cinfo.err := jpeg_std_error(jerr);
  jpeg_create_compress(@cinfo);
  { Add some application-specific error messages (from cderror.h) }
  {jerr.addon_message_table := cdjpeg_message_table;}
  jerr.first_addon_message := JMSG_FIRSTADDONCODE;
  jerr.last_addon_message := JMSG_LASTADDONCODE;

  { Now safe to enable signal catcher. }
{$ifdef NEED_SIGNAL_CATCHER}
  enable_signal_catcher(j_common_ptr ( @cinfo);
{$endif}

  { Initialize JPEG parameters.
    Much of this may be overridden later.
    In particular, we don't yet know the input file's color space,
    but we need to provide some value for jpeg_set_defaults() to work. }


  cinfo.in_color_space := JCS_RGB; { arbitrary guess }
  jpeg_set_defaults(@cinfo);

  { Scan command line to find file names.
    It is convenient to use just one switch-parsing routine, but the switch
    values read here are ignored; we will rescan the switches after opening
    the input file. }


  file_index := parse_switches(@cinfo, 0, FALSE);

{$ifdef TWO_FILE_COMMANDLINE}
  { Must have either -outfile switch or explicit output file name }
  if (outfilename = '') then
  begin
    if (file_index <> argc-2+1) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
    outfilename := ParamStr(file_index+1);
  end
  else
  begin
    if (file_index <> argc-1) then
    begin
      WriteLn(output, progname, ': must name one input and one output file');
      usage;
    end;
  end;
{$else}
  { Unix style: expect zero or one file name }
  if (file_index < argc-1) then
  begin
    WriteLn(output, progname, ': only one input file');
    usage;
  end;
{$endif} { TWO_FILE_COMMANDLINE }

  { Open the input file. }
  if (file_index < argc) then
  begin
    Assign(input_file, ParamStr(file_index));
    {$push}{$I-}
    Reset(input_file, 1);
    {$pop}
    if (IOresult <> 0) then
    begin
      WriteLn(output, progname, ': can''t open ', ParamStr(file_index));
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    WriteLn(output, progname, ': no input file');
    Halt(EXIT_FAILURE);
  end;

  { Open the output file. }
  if (outfilename <> '') then
  begin
    Assign(output_file, outfilename);
{$push}{$I-}
    Reset(output_file, 1);
{$pop}
    if (IOresult = 0) then
    begin
      WriteLn(output, outfilename, ':  already exists.');
      close(output_file);
      Halt(EXIT_FAILURE);
    end;

{$push}{$I-}
    ReWrite(output_file, 1);
{$pop}
    if (IOresult <> 0) then
    begin
      WriteLn(output, progname, ': can''t create ', outfilename);
      Halt(EXIT_FAILURE);
    end;
  end
  else
  begin
    WriteLn(output, progname, ': no output file');
    Halt(EXIT_FAILURE);
  end;

{$ifdef PROGRESS_REPORT}
  start_progress_monitor(j_common_ptr (@cinfo), @progress);
{$endif}

  { Figure out the input file format, and set up to read it. }
  src_mgr := select_file_type(@cinfo, input_file);
  src_mgr^.input_file := @input_file;

  { Read the input file header to obtain file size & colorspace. }
  src_mgr^.start_input (@cinfo, src_mgr);

  { Now that we know input colorspace, fix colorspace-dependent defaults }
  jpeg_default_colorspace(@cinfo);

  { Adjust default compression parameters by re-parsing the options }
  file_index := parse_switches(@cinfo, 0, TRUE);

  { Specify data destination for compression }
  jpeg_stdio_dest(@cinfo, @output_file);

  { Start compressor }
  jpeg_start_compress(@cinfo, TRUE);

  { Process data }
  while (cinfo.next_scanline < cinfo.image_height) do
  begin
    num_scanlines := src_mgr^.get_pixel_rows (@cinfo, src_mgr);
    {void} jpeg_write_scanlines(@cinfo, src_mgr^.buffer, num_scanlines);
  end;

  { Finish compression and release memory }
  src_mgr^.finish_input (@cinfo, src_mgr);
  jpeg_finish_compress(@cinfo);
  jpeg_destroy_compress(@cinfo);

  { Close files, if we opened them }
  close(input_file);
  close(output_file);

{$ifdef PROGRESS_REPORT}
  end_progress_monitor(j_common_ptr (@cinfo));
{$endif}

  { All done. }
  if jerr.num_warnings <> 0 then
    Halt(EXIT_WARNING)
  else
    Halt(EXIT_SUCCESS);
end.
